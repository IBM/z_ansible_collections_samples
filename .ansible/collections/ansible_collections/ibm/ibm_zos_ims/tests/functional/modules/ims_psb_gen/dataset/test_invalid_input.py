# -*- coding: utf-8 -*-
from __future__ import (absolute_import, division, print_function)
from pprint import pprint
import pytest
from ibm_zos_ims.tests.functional.module_utils.ims_test_gen_utils import PSBInputParameters as ip

__metaclass__ = type

GEN_FAIL_MSG = 'FAILURE - PSBGEN execution unsuccessful'

SOURCE = ip.SOURCE
DESTINATION = ip.DESTINATION
SYSLIB = ip.SYSLIB
INVALID_SOURCE = ip.INVALID_SOURCE
INVALID_DESTINATION = ip.INVALID_DESTINATION
INVALID_SYSLIB = ip.INVALID_SYSLIB
DESTINATION_RF = ip.DESTINATION_RF

"""
Work flow for dataset functional tests with inputs as follows:
1. 1 valid dataset with 1 invalid member
2. 1 invalid dataset with valid list of members
3. List of datasets with one invalid dataset
4. List of datasets with list of valid and invalid members
5. Invalid destination
6. Invalid syslib
7. Invalid location
8. Invalid dataset allocated with record format as FB
9. Valid dataset with replace option set to False
10. Source is empty string
11. Valid dataset with empty member list
12. Valid dataset with list of empty strings
13. Valid data set with illegal target names in member list
14. Valid data set with list of lists of valid soures
15. Location is empty string
"""


def validate_single_src(hosts, dest, sys_lib, src, location='DATA_SET', replace=True, member_list=None, psb_name=None):
    # print(srcList)
    response = hosts.all.ims_psb_gen(dest=dest, sys_lib=sys_lib, src=src, location=location, replace=replace, member_list=member_list, psb_name=psb_name)
    for result in response.contacted.values():
        pprint(result)
        print("Changed:", result['changed'])
        assert not result['changed']
        assert result['rc'] != 0
        # Check for success message (if we remove return codes)
        # assert std_error_string in result['msg']


def validate_batch(hosts, batch_list, dest, sys_lib):
    # print(batch_list);
    response = hosts.all.ims_psb_gen(batch=batch_list, dest=dest, sys_lib=sys_lib)
    for result in response.contacted.values():
        pprint(result)
        print("Changed:", result['changed'])
        assert not result['changed']
        assert result['rc'] != 0
        # Check for success message (if we remove return codes)
        assert result['msg'] == GEN_FAIL_MSG
        # check for correct error message
        # assert std_error_string in result['batch_result'][-1]['return_text']


def test_psb_gen_valid_dataset_invalid_member(ansible_zos_module):
    hosts = ansible_zos_module
    validate_single_src(hosts, DESTINATION, SYSLIB, src=SOURCE, location='DATA_SET', member_list=['INVALID'])


def test_psb_gen_invalid_dataset(ansible_zos_module):
    hosts = ansible_zos_module
    validate_single_src(hosts, DESTINATION, SYSLIB, src=INVALID_SOURCE, location='DATA_SET', member_list=['PSBGENL', 'PSBLOAD', 'PSBLOAD'], replace=True)


def test_psb_gen_datasetList_invalid_dataset(ansible_zos_module):
    hosts = ansible_zos_module
    batch_list = [
        {'src': INVALID_SOURCE, 'location': 'DATA_SET', 'member_list': ['PSBLOAD']},
        {'src': SOURCE, 'member_list': ['PSBGENL'], 'replace': True}
    ]
    validate_batch(hosts, batch_list, DESTINATION, SYSLIB)


def test_psb_gen_datasetList_invalid_member(ansible_zos_module):
    hosts = ansible_zos_module
    batch_list = [
        {'src': SOURCE, 'location': 'DATA_SET', 'member_list': ['PSBLOAD', 'INVALID']},
        {'src': SOURCE, 'member_list': ['PSBLOAD', 'PSBGENL'], 'replace': True}
    ]
    validate_batch(hosts, batch_list, DESTINATION, SYSLIB)

# Destination is checked prior to processing any batch.
# Expect invalid destination message here instead of batch failed message.
# def test_psb_gen_invalid_destination(ansible_zos_module):
#     hosts = ansible_zos_module
#     batch_list = [
#         {'src': SOURCE, 'location': 'DATA_SET', 'member_list': ['PSBLOAD', 'PSBLOAD']},
#         {'src': SOURCE, 'location': 'DATA_SET', 'member_list': ['PSBGENL']}
#     ]
#     validate_batch(hosts, batch_list, INVALID_DESTINATION, SYSLIB)


def test_psb_gen_invalid_syslib(ansible_zos_module):
    hosts = ansible_zos_module
    batch_list = [
        {'src': SOURCE, 'location': 'DATA_SET', 'member_list': ['PSBLOAD', 'PSBLOAD']},
        {'src': SOURCE, 'location': 'DATA_SET', 'member_list': ['PSBGENL', 'PSBLOAD']}
    ]
    validate_batch(hosts, batch_list, DESTINATION, INVALID_SYSLIB)


def test_psb_gen_invalid_location(ansible_zos_module):
    hosts = ansible_zos_module
    validate_single_src(hosts, DESTINATION, SYSLIB, src=SOURCE, location='USS', member_list=['PSBLOAD'])


def test_psb_gen_destination_recordFormat(ansible_zos_module):
    hosts = ansible_zos_module
    validate_single_src(hosts, DESTINATION_RF, SYSLIB, src=SOURCE, location='DATA_SET', member_list=['PSBLOAD'])


def test_psb_gen_dataset_replace(ansible_zos_module):
    hosts = ansible_zos_module
    validate_single_src(hosts, DESTINATION, SYSLIB, src=SOURCE, location='DATA_SET', member_list=['PSBLOAD'], replace=False)


def test_psb_gen_source_empty(ansible_zos_module):
    hosts = ansible_zos_module
    validate_single_src(hosts, DESTINATION, SYSLIB, src='', location='DATA_SET', member_list=['PSBLOAD'])


def test_psb_gen_dataset_memberList_empty(ansible_zos_module):
    hosts = ansible_zos_module
    validate_single_src(hosts, DESTINATION, SYSLIB, src=SOURCE, location='DATA_SET', member_list=[], replace=True)


def test_psb_gen_dataset_memberList_empty_listOfString(ansible_zos_module):
    hosts = ansible_zos_module
    validate_single_src(hosts, DESTINATION, SYSLIB, src=SOURCE, location='DATA_SET', member_list=['', ''], replace=True)


# 13. Valid data set with illegal target names in member list
def test_dbd_gen_illegal_target_names(ansible_zos_module):
    hosts = ansible_zos_module
    validate_single_src(hosts, DESTINATION, SYSLIB, src=SOURCE, location='DATA_SET', member_list=[{'TEST1': 'DATA-SET'}, 'TEST2'], replace=True)


# 14. Valid data set with list of lists of valid soures
def test_dbd_gen_invalid_member_list_type(ansible_zos_module):
    hosts = ansible_zos_module
    validate_single_src(hosts, DESTINATION, SYSLIB, src=SOURCE, location='DATA_SET', member_list=[[{'A': 'B'}], 'C', 'D'], replace=True)


def test_psb_gen_location_empty(ansible_zos_module):
    hosts = ansible_zos_module
    validate_single_src(hosts, DESTINATION, SYSLIB, src=SOURCE, location='USS', member_list=['PSBLOAD'], replace=True)
