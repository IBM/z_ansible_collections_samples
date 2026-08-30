# -*- coding: utf-8 -*-
from __future__ import (absolute_import, division, print_function)
from pprint import pprint
import pytest
from ibm_zos_ims.tests.functional.module_utils.ims_test_gen_utils import ACBInputParameters as ip
from ibm_zos_ims.tests.functional.module_utils.ims_test_gen_utils import DBDInputParameters as db
from ibm_zos_ims.tests.functional.module_utils.ims_test_gen_utils import PSBInputParameters as psb

__metaclass__ = type

COMMAND_INPUT_BUILD = ip.COMMAND_INPUT_BUILD
COMMAND_INPUT_DELETE = ip.COMMAND_INPUT_DELETE
PSBLIB = ip.PSBLIB
DBDLIB = ip.DBDLIB
ACBLIB = ip.ACBLIB
STEPLIB = ip.STEPLIB
RESLIB = ip.RESLIB
PSB_NAME = ip.PSB_NAME
DBD_NAME = ip.DBD_NAME
DBD_NAMES = ip.DBD_NAMES
COMP = ip.COMP
INVALID_PSBLIB = ip.INVALID_PSBLIB
INVALID_DBDLIB = ip.INVALID_DBDLIB
INVALID_ACBLIB = ip.INVALID_ACBLIB
INVALID_STEPLIB = ip.INVALID_STEPLIB
INVALID_RESLIB = ip.INVALID_RESLIB
INVALID_PSB = ip.INVALID_PSB
INVALID_PSBS = ip.INVALID_PSBS
INVALID_DBD = ip.INVALID_DBD
INVALID_DBDS = ip.INVALID_DBDS
INVALID_COMP = ip.INVALID_COMP
INVALID_RECFOR = ip.INVALID_RECFOR
EMPTY_PSBLIB = ip.EMPTY_PSBLIB
EMPTY_DBDLIB = ip.EMPTY_DBDLIB
DBD_DESTINATION = db.DESTINATION
PSB_DESTINATION = psb.DESTINATION
SYSLIB = db.SYSLIB
DBD_SOURCE = db.SOURCE
PSB_SOURCE = psb.SOURCE
GEN_SUCCESS_MSG_DBD = 'DBDGEN execution was successful.'
GEN_SUCCESS_MSG_PSB = 'PSBGEN execution was successful'


# vars for prereqs -- disabled since it's unncessary for the current tests
# DBD_SRC = dbd.SOURCE
# PSB_SRC = psb.SOURCE
# SYSLIB = dbd.SYSLIB


"""
Work flow for Combination functional tests goes as follows:
1. PSB name doesn't exists as string, BUILD PSB=PSB_NAME
2. Invalid PSBs as list, BUILD PSB=PSB_NAME
3. Invalid DBD, BUILD DBD=DBD_NAME
4. Invalid DBDs, BUILD DBD=DBD_NAME
5. Invalid PSB and invalid DBD, BUILD PSB=PSB_NAME and BUILD DBD=DBD_NAME,BLDPSB=NO
6. Valid PSB and invalid DBD, BUILD PSB=PSB_NAME and BUILD DBD=DBD_NAME,BLDPSB=YES
7. Invalid PSB and valid DBD, BUILD PSB=PSB_NAME and BUILD DBD=DBD_NAME,BLDPSB=NO
8. Invalid PSB, DELETE PSB=PSB_NAME
9. Invalid DBD, DELETE DBD=DBD_NAME
10. Invalid PSB and invalid DBD, DELETE PSB=PSB_NAME and DELETE DBD=DBD_NAME
11. Valid PSB and invalid DBD, DELETE PSB=PSB_NAME and DELETE DBD=DBD_NAME
12. Invalid PSB and valid DBD, DELETE PSB=PSB_NAME and DELETE DBD=DBD_NAME
13. Invalid PSBLIB - LIB not exists
14. Invalid DBDLIB - LIB not exists
15. Invalid STEPLIB - LIB authorized and exists
16. Invalid RESLIB - LIB authorized and exists
17. Invalid COMP
18. PSBLIB with no psbs and DBDLIB populated, BUILD PSB=ALL
19. DBDLIB with no dbds and PSBLIB populated, BUILD PSB=ALL
20. Invalid ACBLIB
"""


# def test_dbd_gen_dataset_prereq(ansible_zos_module):
#     hosts = ansible_zos_module
#     # validate_single_src(hosts, DESTINATION, SYSLIB, src=SOURCE, location='DATA_SET', member_list=['DEDBJN21'], replace=True)
#     response = hosts.all.ims_dbd_gen(dest=DBD_DESTINATION, sys_lib=SYSLIB, src=DBD_SOURCE, location="DATA_SET", replace=True,
#                 member_list=["DH41SK01", "DBFSAMD1", "DH41SK01", "DBFSAMD2","DBFSAMD3", "HOSPVARD", "DSVNTZ30", "DX41SK01",
#                             "DX41SK03", "DX41SK05", "DX41SK06", "DX41SK07", "DX41SK08", "DX41SK09", "DX41SK02", "DX41SK04"])
#     for result in response.contacted.values():
#         print(result)
#         print("Changed:", result['changed'])
#         assert result['changed'] == True
#         assert result['rc'] == 0
#         # Check for success message (if we remove return codes)
#         assert result['msg'] == GEN_SUCCESS_MSG_DBD


# def test_psb_gen_dataset_prereq(ansible_zos_module):
#     hosts = ansible_zos_module
#     response = hosts.all.ims_psb_gen(dest=PSB_DESTINATION, sys_lib=SYSLIB, src=PSB_SOURCE, location="DATA_SET",
#                 replace=True, member_list=["PSBGENL"])
#     for result in response.contacted.values():
#         pprint(result)
#         print("Changed:", result['changed'])
#         assert result['changed'] == True
#         assert result['rc'] == 0
#         # Check for success message (if we remove return codes)
#         assert result['msg'] == GEN_SUCCESS_MSG_PSB

# dbdgen and psbgen prereqs -- disabled since it's unncessary for the current tests
# def test_dbd_gen_dataset_prereq(ansible_zos_module):
#     hosts = ansible_zos_module
#     # validate_single_src(hosts, DESTINATION, SYSLIB, src=SOURCE, location='DATA_SET', member_list=['DEDBJN21'], replace=True)
#     response = hosts.all.ims_dbd_gen(
#         dest=DBDLIB[0],
#         sys_lib=SYSLIB, src=DBD_SRC, location="DATA_SET",
#         replace=True,
#         member_list=[
#             "DH41SK01", "DBFSAMD1", "DH41SK01", "DBFSAMD2", "DBFSAMD3", "HOSPVARD", "DSVNTZ30", "DX41SK01",
#             "DX41SK03", "DX41SK05", "DX41SK06", "DX41SK07", "DX41SK08", "DX41SK09", "DX41SK02", "DX41SK04", "WAREDB",
#             "ORDDB", "DISTDB", "ARTDB", "CUSTDB", "NORDDB", "ORDRDB", "ORDLDB", "ITEMDB", "ITEMDBP", "STCKDB"])
#     for result in response.contacted.values():
#         print(result)
#         print("Changed:", result['changed'])
#         assert result['changed']
#         assert result['rc'] == 0
#         # Check for success message (if we remove return codes)
#         # assert result['msg'] == GEN_SUCCESS_MSG

# def test_psb_gen_dataset_prereq(ansible_zos_module):
#     hosts = ansible_zos_module
#     response = hosts.all.ims_psb_gen(
#         dest=PSBLIB[0], sys_lib=SYSLIB, src=PSB_SRC, location="DATA_SET",
#         replace=True, member_list=["PSBGENL"])
#     for result in response.contacted.values():
#         pprint(result)
#         print("Changed:", result['changed'])
#         assert result['changed']
#         assert result['rc'] == 0
#         # Check for success message (if we remove return codes)
#         # assert result['msg'] == GEN_SUCCESS_MSG

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
        assert result.get('changed') is False
        if result.get('rc'):
            assert result.get('rc') != 0
        elif result.get('msg'):
            assert "value of compression must be one of: precomp, postcomp, precomp,postcomp," in result.get('msg')


# 1. PSB name doesn't exists as string, BUILD PSB=PSB_NAME
def test_acb_gen_build_invalid_psbName_str(ansible_zos_module):
    hosts = ansible_zos_module
    validate_acbgen(hosts, command_input=COMMAND_INPUT_BUILD, psb_name=INVALID_PSB, psb_lib=PSBLIB, dbd_lib=DBDLIB,
                    acb_lib=ACBLIB, steplib=STEPLIB, reslib=RESLIB, build_psb=True)


# 2. Invalid PSBs as list, BUILD PSB=PSB_NAME
def test_acb_gen_build_invalid_psbNames(ansible_zos_module):
    hosts = ansible_zos_module
    validate_acbgen(hosts, command_input=COMMAND_INPUT_BUILD, psb_name=INVALID_PSBS, psb_lib=PSBLIB, dbd_lib=DBDLIB,
                    acb_lib=ACBLIB, steplib=STEPLIB, reslib=RESLIB, build_psb=True)


# 3. Invalid DBD, BUILD DBD=DBD_NAME
def test_acb_gen_build_invalid_dbdName(ansible_zos_module):
    hosts = ansible_zos_module
    validate_acbgen(hosts, command_input=COMMAND_INPUT_BUILD, dbd_name=INVALID_DBD, psb_lib=PSBLIB, dbd_lib=DBDLIB,
                    acb_lib=ACBLIB, steplib=STEPLIB, reslib=RESLIB, build_psb=True)


# 4. Invalid DBDs, BUILD DBD=DBD_NAME
def test_acb_gen_build_invalid_dbdNames(ansible_zos_module):
    hosts = ansible_zos_module
    validate_acbgen(hosts, command_input=COMMAND_INPUT_BUILD, dbd_name=INVALID_DBDS, psb_lib=PSBLIB, dbd_lib=DBDLIB,
                    acb_lib=ACBLIB, steplib=STEPLIB, reslib=RESLIB, build_psb=True)


# 5. Invalid PSB and invalid DBD, BUILD PSB=PSB_NAME and BUILD DBD=DBD_NAME,BLDPSB=NO
def test_acb_gen_build_invalid_psbName_list(ansible_zos_module):
    hosts = ansible_zos_module
    validate_acbgen(hosts, command_input=COMMAND_INPUT_BUILD, psb_name=INVALID_PSB, psb_lib=PSBLIB, dbd_lib=DBDLIB,
                    acb_lib=ACBLIB, steplib=STEPLIB, reslib=RESLIB, build_psb=True)


def test_acb_gen_build_invalid_psbName_invalid_dbdName(ansible_zos_module):
    hosts = ansible_zos_module
    validate_acbgen(hosts, command_input=COMMAND_INPUT_BUILD, psb_name=INVALID_PSB, dbd_name=INVALID_DBD, psb_lib=PSBLIB,
                    dbd_lib=DBDLIB, acb_lib=ACBLIB, steplib=STEPLIB, reslib=RESLIB, build_psb=False)


# 6. Valid PSB and invalid DBD, BUILD PSB=PSB_NAME and BUILD DBD=DBD_NAME,BLDPSB=YES
def test_acb_gen_build_valid_psbName_invalid_dbdName(ansible_zos_module):
    hosts = ansible_zos_module
    validate_acbgen(hosts, command_input=COMMAND_INPUT_BUILD, psb_name=PSB_NAME, dbd_name=INVALID_DBD, psb_lib=PSBLIB,
                    dbd_lib=DBDLIB, acb_lib=ACBLIB, steplib=STEPLIB, reslib=RESLIB, build_psb=True)


# 7. Invalid PSB and valid DBD, BUILD PSB=PSB_NAME and BUILD DBD=DBD_NAME,BLDPSB=NO
def test_acb_gen_build_invalid_psbName_valid_dbdName(ansible_zos_module):
    hosts = ansible_zos_module
    validate_acbgen(hosts, command_input=COMMAND_INPUT_BUILD, psb_name=INVALID_PSB, dbd_name=DBD_NAME, psb_lib=PSBLIB,
                    dbd_lib=DBDLIB, acb_lib=ACBLIB, steplib=STEPLIB, reslib=RESLIB, build_psb=False)


# 8. Invalid PSB, DELETE PSB=PSB_NAME
def test_acb_gen_delete_invalid_psbName(ansible_zos_module):
    hosts = ansible_zos_module
    validate_acbgen(hosts, command_input=COMMAND_INPUT_DELETE, psb_name=INVALID_PSB, psb_lib=PSBLIB, dbd_lib=DBDLIB,
                    acb_lib=ACBLIB, steplib=STEPLIB, reslib=RESLIB)


# 9. Invalid DBD, DELETE DBD=DBD_NAME
def test_acb_gen_delete_invalid_dbdName(ansible_zos_module):
    hosts = ansible_zos_module
    validate_acbgen(hosts, command_input=COMMAND_INPUT_DELETE, dbd_name=INVALID_DBD, psb_lib=PSBLIB, dbd_lib=DBDLIB,
                    acb_lib=ACBLIB, steplib=STEPLIB, reslib=RESLIB)


# 10. Invalid PSB and invalid DBD, DELETE PSB=PSB_NAME and DELETE DBD=DBD_NAME
def test_acb_gen_delete_invalid_psbName_invalid_dbdName(ansible_zos_module):
    hosts = ansible_zos_module
    validate_acbgen(hosts, command_input=COMMAND_INPUT_DELETE, psb_name=INVALID_PSB, dbd_name=INVALID_DBD, psb_lib=PSBLIB,
                    dbd_lib=DBDLIB, acb_lib=ACBLIB, steplib=STEPLIB, reslib=RESLIB)


# 11. Valid PSB and invalid DBD, DELETE PSB=PSB_NAME and DELETE DBD=DBD_NAME
def test_acb_gen_delete_valid_psbName_invalid_dbdName(ansible_zos_module):
    hosts = ansible_zos_module
    validate_acbgen(hosts, command_input=COMMAND_INPUT_DELETE, psb_name=PSB_NAME, dbd_name=INVALID_DBD, psb_lib=PSBLIB,
                    dbd_lib=DBDLIB, acb_lib=ACBLIB, steplib=STEPLIB, reslib=RESLIB)


# 12. Invalid PSB and valid DBD, DELETE PSB=PSB_NAME and DELETE DBD=DBD_NAME
def test_acb_gen_delete_invalid_psbName_valid_dbdName(ansible_zos_module):
    hosts = ansible_zos_module
    validate_acbgen(hosts, command_input=COMMAND_INPUT_DELETE, psb_name=INVALID_PSB, dbd_name=DBD_NAME, psb_lib=PSBLIB,
                    dbd_lib=DBDLIB, acb_lib=ACBLIB, steplib=STEPLIB, reslib=RESLIB)


# 13. Invalid PSBLIB - LIB not exists
def test_acb_gen_delete_invalid_psblib(ansible_zos_module):
    hosts = ansible_zos_module
    validate_acbgen(hosts, command_input=COMMAND_INPUT_DELETE, psb_name=PSB_NAME, dbd_name=DBD_NAME, psb_lib=INVALID_PSBLIB,
                    dbd_lib=DBDLIB, acb_lib=ACBLIB, steplib=STEPLIB, reslib=RESLIB)


# 14. Invalid DBDLIB - LIB not exists
def test_acb_gen_delete_invalid_dbdlib(ansible_zos_module):
    hosts = ansible_zos_module
    validate_acbgen(hosts, command_input=COMMAND_INPUT_DELETE, psb_name=PSB_NAME, dbd_name=DBD_NAME, psb_lib=PSBLIB,
                    dbd_lib=INVALID_DBDLIB, acb_lib=ACBLIB, steplib=STEPLIB, reslib=RESLIB)


# 15. Invalid STEPLIB - LIB authorized and exists
def test_acb_gen_delete_invalid_steplib(ansible_zos_module):
    hosts = ansible_zos_module
    validate_acbgen(hosts, command_input=COMMAND_INPUT_DELETE, psb_name=PSB_NAME, dbd_name=DBD_NAME, psb_lib=PSBLIB,
                    dbd_lib=DBDLIB, acb_lib=ACBLIB, steplib=INVALID_STEPLIB, reslib=RESLIB)


# 16. Invalid RESLIB - LIB authorized and exists
def test_acb_gen_delete_invalid_reslib(ansible_zos_module):
    hosts = ansible_zos_module
    validate_acbgen(hosts, command_input=COMMAND_INPUT_DELETE, psb_name=PSB_NAME, dbd_name=DBD_NAME, psb_lib=PSBLIB,
                    dbd_lib=DBDLIB, acb_lib=ACBLIB, steplib=STEPLIB, reslib=INVALID_RESLIB)


# 17. Invalid COMP
def test_acb_gen_build_invalid_comp(ansible_zos_module):
    hosts = ansible_zos_module
    validate_acbgen(hosts, command_input=COMMAND_INPUT_BUILD, psb_name=PSB_NAME, psb_lib=PSBLIB, dbd_lib=DBDLIB,
                    acb_lib=ACBLIB, steplib=STEPLIB, reslib=RESLIB, compression=INVALID_COMP, build_psb=True)


# 18. PSBLIB with no psbs and DBDLIB populated, BUILD PSB=ALL
def test_acb_gen_delete_no_psbs(ansible_zos_module):
    hosts = ansible_zos_module
    validate_acbgen(hosts, command_input=COMMAND_INPUT_DELETE, psb_name=PSB_NAME, dbd_name=DBD_NAME, psb_lib=EMPTY_PSBLIB,
                    dbd_lib=DBDLIB, acb_lib=ACBLIB, steplib=STEPLIB, reslib=INVALID_RESLIB)


# 19. DBDLIB with no dbds and PSBLIB populated, BUILD PSB=ALL
def test_acb_gen_delete_no_dbds(ansible_zos_module):
    hosts = ansible_zos_module
    validate_acbgen(hosts, command_input=COMMAND_INPUT_DELETE, psb_name=PSB_NAME, dbd_name=DBD_NAME, psb_lib=PSBLIB,
                    dbd_lib=EMPTY_DBDLIB, acb_lib=ACBLIB, steplib=STEPLIB, reslib=INVALID_RESLIB)


# 20. Invalid ACBLIB
def test_acb_gen_build_invalid_acblib(ansible_zos_module):
    hosts = ansible_zos_module
    validate_acbgen(hosts, command_input=COMMAND_INPUT_BUILD, psb_name=PSB_NAME, psb_lib=PSBLIB, dbd_lib=DBDLIB,
                    acb_lib=INVALID_ACBLIB, steplib=STEPLIB, reslib=RESLIB, compression=COMP, build_psb=False)
