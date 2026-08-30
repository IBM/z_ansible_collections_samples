# -*- coding: utf-8 -*-
from __future__ import (absolute_import, division, print_function)
from pprint import pprint
import pytest
from ibm_zos_ims.tests.functional.module_utils.ims_test_gen_utils import ACBInputParameters as ip
from ibm_zos_ims.tests.functional.module_utils.ims_test_gen_utils import DBDInputParameters as dbd
from ibm_zos_ims.tests.functional.module_utils.ims_test_gen_utils import PSBInputParameters as psb

__metaclass__ = type

COMMAND_INPUT_BUILD = ip.COMMAND_INPUT_BUILD
COMMAND_INPUT_DELETE = ip.COMMAND_INPUT_DELETE
PSBLIB = ip.PSBLIB
DBDLIB = ip.DBDLIB
ACBLIB = ip.ACBLIB
STEPLIB = ip.STEPLIB
RESLIB = ip.RESLIB
PSB_NAME_ALL = ip.PSB_NAME_ALL
PSB_NAME = ip.PSB_NAME
PSB_NAMES = ip.PSB_NAMES
DBD_NAME = ip.DBD_NAME
DBD_NAMES = ip.DBD_NAMES
DBD_NAMES_LIST = ip.DBD_NAMES_LIST
COMP_PRE = ip.COMP_PRE
COMP_POST = ip.COMP_POST
COMP = ip.COMP

# vars for prereqs
DBD_SRC = dbd.SOURCE
PSB_SRC = psb.SOURCE
SYSLIB = dbd.SYSLIB


"""
Work flow for Combination functional tests goes as follows:
1. BUILD PSB=ALL as string
2. DELETE PSB=PSB_NAME as list
3. BUILD PSB=PSB_NAME
4. DELETE DBD=DBD_NAME
5. BUILD DBD=DBD_NAME,BLDPSB=NO
6. BUILD DBD=DBD_NAME,BLDPSB=YES
7. BUILD PSB=PSB_NAME
8. DELETE PSB=PSB_NAME, DELETE DBD=DBD_NAMES
9. BUILD PSB=PSB_NAME
10.BUILD PSB=PSB_NAME, BUILD DBD=DBD_NAMES,BLDPSB=NO
11.BUILD PSB=PSB_NAME, BUILD DBD=DBD_NAMES,BLDPSB=YES
12.BUILD PSB=PSB_NAME, BUILD DBD=DBD_NAME,BLDPSB=YES, COMP='PRECOMP'
13.BUILD PSB=PSB_NAME, BUILD DBD=DBDNAME,BLDPSB=YES, COMP='POSTCOMP'
14.BUILD PSB=PSB_NAME, BUILD DBD=DBDNAME,BLDPSB=YES, COMP='PRECOMP,POSTCOMP'
15.Multi line PSB and DBD - BUILD with BLDPSB=YES
16.Multi line PSB list over 6 PSB's in the list
17.Multi line DBD list over 6 DBD's in the list
18.STEPLIB=STEPLIB, RESLIB=None
19.RESLIB=None, STEPLIB=None(STEPLIB should be retrieved from environment_vars)
20.STEPLIB=None(STEPLIB should be retrieved from environment_vars), RESLIB=RESLIB
"""


def validate_acbgen(hosts, psb_name=None, dbd_name=None, psb_lib=None,
                    dbd_lib=None, acb_lib=None, steplib=None, reslib=None,
                    compression=None, build_psb=None, command_input=None):
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
        assert result.get('changed')
        assert result.get('rc') <= 4


def test_dbd_gen_dataset_prereq(ansible_zos_module):
    hosts = ansible_zos_module
    # validate_single_src(hosts, DESTINATION, SYSLIB, src=SOURCE, location='DATA_SET', member_list=['DEDBJN21'], replace=True)
    response = hosts.all.ims_dbd_gen(
        dest=DBDLIB[0],
        sys_lib=SYSLIB, src=DBD_SRC, location="DATA_SET",
        replace=True,
        member_list=[
            "DH41SK01", "DBFSAMD1", "DH41SK01", "DBFSAMD2", "DBFSAMD3", "HOSPVARD", "DSVNTZ30", "DX41SK01",
            "DX41SK03", "DX41SK05", "DX41SK06", "DX41SK07", "DX41SK08", "DX41SK09", "DX41SK02", "DX41SK04", "WAREDB",
            "ORDDB", "DISTDB", "ARTDB", "CUSTDB", "NORDDB", "ORDRDB", "ORDLDB", "ITEMDB", "ITEMDBP", "STCKDB"])
    for result in response.contacted.values():
        print(result)
        print("Changed:", result['changed'])
        assert result['changed']
        assert result['rc'] == 0
        # Check for success message (if we remove return codes)
        # assert result['msg'] == GEN_SUCCESS_MSG


def test_psb_gen_dataset_prereq(ansible_zos_module):
    hosts = ansible_zos_module
    response = hosts.all.ims_psb_gen(
        dest=PSBLIB[0], sys_lib=SYSLIB, src=PSB_SRC, location="DATA_SET",
        replace=True, member_list=["PSBGENL"])
    for result in response.contacted.values():
        pprint(result)
        print("Changed:", result['changed'])
        assert result['changed']
        assert result['rc'] == 0
        # Check for success message (if we remove return codes)
        # assert result['msg'] == GEN_SUCCESS_MSG


# 1. BUILD PSB=ALL as string
def test_acb_gen_build_psbName_all(ansible_zos_module):
    hosts = ansible_zos_module
    validate_acbgen(hosts, command_input=COMMAND_INPUT_BUILD, psb_name=PSB_NAME_ALL, psb_lib=PSBLIB,
                    dbd_lib=DBDLIB, acb_lib=ACBLIB, steplib=STEPLIB, reslib=RESLIB, build_psb=True)


# 2. DELETE PSB=PSB_NAME as list
def test_acb_gen_delete_psbName(ansible_zos_module):
    hosts = ansible_zos_module
    validate_acbgen(hosts, command_input=COMMAND_INPUT_DELETE, psb_name=PSB_NAME, psb_lib=PSBLIB,
                    dbd_lib=DBDLIB, acb_lib=ACBLIB, steplib=STEPLIB, reslib=RESLIB)


# 3. BUILD PSB=PSB_NAME
def test_acb_gen_build_psbName(ansible_zos_module):
    hosts = ansible_zos_module
    validate_acbgen(hosts, command_input=COMMAND_INPUT_BUILD, psb_name=PSB_NAME, psb_lib=PSBLIB,
                    dbd_lib=DBDLIB, acb_lib=ACBLIB, steplib=STEPLIB, reslib=RESLIB, build_psb=True)


# 4. DELETE DBD=(DBFSAMD3)
def test_acb_gen_delete_dbdName(ansible_zos_module):
    hosts = ansible_zos_module
    validate_acbgen(hosts, command_input=COMMAND_INPUT_DELETE, dbd_name=DBD_NAME, psb_lib=PSBLIB,
                    dbd_lib=DBDLIB, acb_lib=ACBLIB, steplib=STEPLIB, reslib=RESLIB)


# 5. BUILD DBD=(HOSPVARD),BLDPSB=NO
def test_acb_gen_build_dbdName_bldPsb_no(ansible_zos_module):
    hosts = ansible_zos_module
    validate_acbgen(hosts, command_input=COMMAND_INPUT_BUILD, dbd_name="HOSPVARD", psb_lib=PSBLIB,
                    dbd_lib=DBDLIB, acb_lib=ACBLIB, steplib=STEPLIB, reslib=RESLIB, build_psb=False)


# 6. BUILD DBD=(HOSPVARD),BLDPSB=YES
def test_acb_gen_build_dbdName_bldPsb_yes(ansible_zos_module):
    hosts = ansible_zos_module
    validate_acbgen(hosts, command_input=COMMAND_INPUT_BUILD, dbd_name="HOSPVARD", psb_lib=PSBLIB,
                    dbd_lib=DBDLIB, acb_lib=ACBLIB, steplib=STEPLIB, reslib=RESLIB, build_psb=True)


# 7. BUILD PSB=PSB_NAME
def test_acb_gen_build_psbName2(ansible_zos_module):
    hosts = ansible_zos_module
    validate_acbgen(hosts, command_input=COMMAND_INPUT_BUILD, psb_name=PSB_NAME, psb_lib=PSBLIB,
                    dbd_lib=DBDLIB, acb_lib=ACBLIB, steplib=STEPLIB, reslib=RESLIB, build_psb=True)


# 8. DELETE PSB=PSB_NAME, DELETE DBD=DBD_NAMES
# DELETE PSB=(PSBGENL)\n'
# DELETE DBD=("DH41SK01", "HOSPVARD")\n'
# rc: '0'``
#   ret_code:
#     code: 0
#     msg: CC 0000
#     msg_code: '0000'
#     msg_txt: '' ->
# check in acblib if this member got deleted
def test_acb_gen_delete_psbName_dbdName(ansible_zos_module):
    hosts = ansible_zos_module
    validate_acbgen(hosts, command_input=COMMAND_INPUT_DELETE, psb_name=PSB_NAME, dbd_name=DBD_NAMES,
                    psb_lib=PSBLIB, dbd_lib=DBDLIB, acb_lib=ACBLIB, steplib=STEPLIB, reslib=RESLIB)


# 9. BUILD PSB=PSB_NAME
def test_acb_gen_build_psbName3(ansible_zos_module):
    hosts = ansible_zos_module
    validate_acbgen(hosts, command_input=COMMAND_INPUT_BUILD, psb_name=PSB_NAME, psb_lib=PSBLIB,
                    dbd_lib=DBDLIB, acb_lib=ACBLIB, steplib=STEPLIB, reslib=RESLIB, build_psb=True)


# 10.BUILD PSB=PSB_NAME, BUILD DBD=DBD_NAMES,BLDPSB=NO
def test_acb_gen_build_psbName_dbdNames_bldPsb_no(ansible_zos_module):
    hosts = ansible_zos_module
    validate_acbgen(hosts, command_input=COMMAND_INPUT_BUILD, psb_name=PSB_NAME, dbd_name=DBD_NAMES,
                    psb_lib=PSBLIB, dbd_lib=DBDLIB, acb_lib=ACBLIB, steplib=STEPLIB, reslib=RESLIB, build_psb=False)


# 11.BUILD PSB=PSB_NAME, BUILD DBD=DBD_NAMES,BLDPSB=YES
def test_acb_gen_build_psbName_dbdNames_bldPsb_yes(ansible_zos_module):
    hosts = ansible_zos_module
    validate_acbgen(hosts, command_input=COMMAND_INPUT_BUILD, psb_name=PSB_NAME, dbd_name=DBD_NAMES,
                    psb_lib=PSBLIB, dbd_lib=DBDLIB, acb_lib=ACBLIB, steplib=STEPLIB, reslib=RESLIB, build_psb=True)


# 12.BUILD PSB=PSB_NAME, BUILD DBD=DBD_NAME,BLDPSB=YES, COMP='PRECOMP'
def test_acb_gen_build_psbName_dbdName_bldPsb_yes_comp_precomp(ansible_zos_module):
    hosts = ansible_zos_module
    validate_acbgen(hosts, command_input=COMMAND_INPUT_BUILD, psb_name=PSB_NAME, psb_lib=PSBLIB,
                    dbd_lib=DBDLIB, acb_lib=ACBLIB, steplib=STEPLIB, reslib=RESLIB, compression=COMP_PRE, build_psb=True)


# 13.BUILD PSB=PSB_NAME, BUILD DBD=DBDNAME,BLDPSB=YES, COMP='POSTCOMP'
def test_acb_gen_build_psbName_dbdName_bldPsb_yes_comp_postcomp(ansible_zos_module):
    hosts = ansible_zos_module
    validate_acbgen(hosts, command_input=COMMAND_INPUT_BUILD, psb_name=PSB_NAME, psb_lib=PSBLIB,
                    dbd_lib=DBDLIB, acb_lib=ACBLIB, steplib=STEPLIB, reslib=RESLIB, compression=COMP_POST, build_psb=True)


# 14.BUILD PSB=PSB_NAME, BUILD DBD=DBDNAME,BLDPSB=YES, COMP='PRECOMP,POSTCOMP'
def test_acb_gen_build_psbName_dbdName_bldPsb_yes_comp_precomp_postcomp(ansible_zos_module):
    hosts = ansible_zos_module
    validate_acbgen(hosts, command_input=COMMAND_INPUT_BUILD, psb_name=PSB_NAME, psb_lib=PSBLIB,
                    dbd_lib=DBDLIB, acb_lib=ACBLIB, steplib=STEPLIB, reslib=RESLIB, compression=COMP, build_psb=True)


# 15.Multi line PSB and DBD - BUILD with BLDPSB=NO
def test_acb_gen_build_psbNames_dbdNames_list(ansible_zos_module):
    hosts = ansible_zos_module
    validate_acbgen(hosts, command_input=COMMAND_INPUT_BUILD, psb_name=PSB_NAME, dbd_name=DBD_NAMES_LIST,
                    psb_lib=PSBLIB, dbd_lib=DBDLIB, acb_lib=ACBLIB, steplib=STEPLIB, reslib=RESLIB, build_psb=False)


# 16.Multi line PSB list over 6 PSB's in the list
def test_acb_gen_delete_psbNames_list_comp_postcomp(ansible_zos_module):
    hosts = ansible_zos_module
    validate_acbgen(hosts, command_input=COMMAND_INPUT_DELETE, psb_name=PSB_NAMES, psb_lib=PSBLIB,
                    dbd_lib=DBDLIB, acb_lib=ACBLIB, steplib=STEPLIB, reslib=RESLIB, compression=COMP_POST)


# 17.Multi line DBD list over 6 DBD's in the list
def test_acb_gen_delete_dbdNames_list(ansible_zos_module):
    hosts = ansible_zos_module
    validate_acbgen(hosts, command_input=COMMAND_INPUT_DELETE, dbd_name=DBD_NAMES, psb_lib=PSBLIB,
                    dbd_lib=DBDLIB, acb_lib=ACBLIB, steplib=STEPLIB, reslib=RESLIB)


# 18.STEPLIB=STEPLIB, RESLIB=None
def test_acb_gen_build_psbName_no_reslib(ansible_zos_module):
    hosts = ansible_zos_module
    validate_acbgen(hosts, command_input=COMMAND_INPUT_BUILD, psb_name=PSB_NAME, psb_lib=PSBLIB,
                    dbd_lib=DBDLIB, acb_lib=ACBLIB, steplib=STEPLIB, build_psb=False)


# 19.RESLIB=STEPLIB=None(STEPLIB should be retrieved from environment_vars)
def test_acb_gen_delete_dbdName_no_reslib_env_steplib(ansible_zos_module):
    hosts = ansible_zos_module
    validate_acbgen(hosts, command_input=COMMAND_INPUT_DELETE, dbd_name=DBD_NAME, psb_lib=PSBLIB,
                    dbd_lib=DBDLIB, acb_lib=ACBLIB)


# 20.STEPLIB=None(STEPLIB should be retrieved from environment_vars), RESLIB=RESLIB
def test_acb_gen_build_dbdName_only_reslib(ansible_zos_module):
    hosts = ansible_zos_module
    validate_acbgen(hosts, command_input=COMMAND_INPUT_BUILD, psb_name=PSB_NAME, psb_lib=PSBLIB,
                    dbd_lib=DBDLIB, acb_lib=ACBLIB, reslib=RESLIB, build_psb=False)
