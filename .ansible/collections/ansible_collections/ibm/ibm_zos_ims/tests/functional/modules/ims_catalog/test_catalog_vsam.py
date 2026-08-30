
from __future__ import (absolute_import, division, print_function)
import pytest
from pprint import pprint
from ibm_zos_ims.tests.functional.module_utils.ims_test_catalog_utils import CatalogInputParameters as cp  # pylint: disable=import-error
from ibm_zos_ims.tests.functional.module_utils.ims_test_catalog_utils import load_catalog, purge_catalog  # pylint: disable=import-error
from ibm_zos_ims.tests.functional.module_utils.ims_test_gen_utils import PSBInputParameters as ps  # pylint: disable=import-error
from ibm_zos_ims.tests.functional.module_utils.ims_test_gen_utils import DBDInputParameters as db  # pylint: disable=import-error
from ibm_zos_ims.tests.functional.module_utils.ims_test_gen_utils import ACBInputParameters as ac  # pylint: disable=import-error

__metaclass__ = type


def test_gen_vsam_acb_stage_import(ansible_zos_module):
    hosts = ansible_zos_module

    # Load the catalog
    load_catalog(hosts,
                 psb_lib=cp.PSBLIB,
                 dbd_lib=cp.DBDLIB,
                 acb_lib=cp.ACBLIB,
                 steplib=cp.STEPLIB,
                 reslib=cp.RESLIB,
                 proclib=cp.PROCLIB,
                 primary_log_dataset=cp.PRIMARYLOG,
                 buffer_pool_param_dataset=cp.BUFFERPOOL,
                 mode=cp.LOADMODE,
                 validation_msg="DFS4434I",
                 control_statements={
                     'managed_acbs': {
                         'setup': True
                     }
                 })

    # Generate vsam DBD
    response = hosts.all.ims_dbd_gen(
        src=cp.DBDSOURCE, location="DATA_SET", replace=True, member_list=['DGSAM1'],
        dbd_name=None, dest=cp.DBDDEST, sys_lib=["IMSBLD.I15RTSMM.SDFSMAC", "SYS1.MACLIB"])
    for result in response.contacted.values():
        assert result['changed'] is True
        assert result['rc'] == 0
        # Check for success message (if we remove return codes)
        assert result['msg'] == 'DBDGEN execution was successful.'

    # Generate vsam PSB
    response = hosts.all.ims_psb_gen(
        src=cp.PSBSOURCE, location="DATA_SET", replace=True, member_list=['PGSAM1'],
        psb_name=None, dest=cp.PSBDEST, sys_lib=["IMSBLD.I15RTSMM.SDFSMAC", "SYS1.MACLIB"])
    for result in response.contacted.values():
        assert result['changed'] is True
        assert result['rc'] == 0
        # Check for success message (if we remove return codes)
        assert result['msg'] == 'PSBGEN execution was successful.'

    # Add to ACBLIB
    validate_acbgen(
        hosts, command_input=ac.COMMAND_INPUT_BUILD, psb_name=cp.PSB_NAME,
        psb_lib=cp.PSBLIB, dbd_lib=cp.DBDLIB, acb_lib=cp.ACBDEST, steplib=cp.STEPLIB, reslib=cp.RESLIB)

    # Add to the catalog staging directory
    load_catalog(hosts,
                 psb_lib=cp.PSBLIB,
                 dbd_lib=cp.DBDLIB,
                 acb_lib=cp.ACBLIB,
                 steplib=cp.STEPLIB,
                 reslib=cp.RESLIB,
                 proclib=cp.PROCLIB,
                 primary_log_dataset=cp.PRIMARYLOG,
                 buffer_pool_param_dataset=cp.BUFFERPOOL,
                 mode=cp.UPDATEMODE,
                 validation_msg="DFS4536I",
                 control_statements={
                     'managed_acbs': {
                         'stage': {
                             'save_acb': "LATEST"
                         }
                     }
                 })
    # Update catalog directory datasets
    load_catalog(hosts,
                 psb_lib=cp.PSBLIB,
                 dbd_lib=cp.DBDLIB,
                 acb_lib=cp.ACBLIB,
                 steplib=cp.STEPLIB,
                 reslib=cp.RESLIB,
                 proclib=cp.PROCLIB,
                 primary_log_dataset=cp.PRIMARYLOG,
                 buffer_pool_param_dataset=cp.BUFFERPOOL,
                 mode=cp.UPDATEMODE,
                 validation_msg="DFS4534I",
                 control_statements={
                     'managed_acbs': {
                         'update': {
                             'replace_acb': "LATEST"
                         }
                     }
                 })

    # Purge catalog and directory
    purge_catalog(hosts,
                  psb_lib=cp.PSBLIB,
                  dbd_lib=cp.DBDLIB,
                  steplib=cp.STEPLIB,
                  reslib=cp.RESLIB,
                  proclib=cp.PROCLIB,
                  primary_log_dataset=cp.PRIMARYLOG,
                  buffer_pool_param_dataset=cp.BUFFERPOOL,
                  mode=cp.PURGEMODE,
                  validation_msg="",
                  delete=cp.DELETES,
                  managed_acbs=True)

    # response = hosts.all.ims_command(command='IMPORT DEFN SOURCE(CATALOG) NAME(PGSAM1, DGSAM1)', plex='PLEX1', route='IMS1')
    # for result in response.contacted.values():
    #  assert result['ims_output'][0]['command_return']['ctl.rc'] == '00000000'


def validate_acbgen(
    hosts, psb_name=None, dbd_name=None, psb_lib=None,
        dbd_lib=None, acb_lib=None, steplib=None, reslib=None, compression=None, build_psb=None, command_input=None):

    arguments = {}
    if psb_name:
        arguments["psb_name"] = psb_name
    if dbd_name:
        arguments["dbd_name"] = dbd_name
    if psb_lib:
        arguments["psb_lib"] = psb_lib
    if dbd_lib:
        arguments["dbd_lib"] = dbd_lib
    if acb_lib:
        arguments["acb_lib"] = acb_lib
    if steplib:
        arguments["steplib"] = steplib
    if reslib:
        arguments["reslib"] = reslib
    if compression:
        arguments["compression"] = compression
    if build_psb:
        arguments["build_psb"] = build_psb
    if command_input:
        arguments["command_input"] = command_input

    response = hosts.all.ims_acb_gen(**arguments)
    print("Result:", response)
    for result in response.contacted.values():
        pprint(result)
        print("Changed:", result.get('changed'))
        print("Return code:", result.get('rc'))
        assert result.get('changed') is True
        assert result.get('rc') <= 4
