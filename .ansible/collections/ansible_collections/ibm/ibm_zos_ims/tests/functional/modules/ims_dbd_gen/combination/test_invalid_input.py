# -*- coding: utf-8 -*-
from __future__ import (absolute_import, division, print_function)
from pprint import pprint
import pytest
from ibm_zos_ims.tests.functional.module_utils.ims_test_gen_utils import DBDInputParameters as ip

__metaclass__ = type

GEN_FAIL_MSG = 'FAILURE - DBDGEN execution unsuccessful.'

SOURCE = ip.SOURCE
DESTINATION = ip.DESTINATION
SYSLIB = ip.SYSLIB
SEQ = ip.SEQ_SOURCE
USS = ip.REMOTE_DBDGEN04_SOURCE
INVALID_USS = ip.INVALID_USS_SOURCE
INVALID_SOURCE = ip.INVALID_SOURCE
INVALID_SEQ = ip.INVALID_SEQ_SOURCE

"""
Work flow for Combination functional tests goes as follows:
1. 3 inputs - invalid uss, valid dataset, valid seq dataset
2. 2 inputs - invalid dataset, valid uss
3. 2 inputs - invalid seq dataset, valid uss
4. 2 inputs - valid dataset - invalid member added in the list, valid seq dataset
5. 3 inputs - invalid uss, invalid dataset and invalid seq dataset
6. 1 input - valid uss invalid location
7. 1 input - valid dataset invalid location
8. 1 input - valid seq dataset invalid location
9. 1 input - valid USS with replace set to False
10. 1 input - empty string for USS source
11. 1 input - empty string for USS location
12. 1 input - empty string for destination
13. 1 input - empty list for syslib
14. 1 input - valid dataset with valid first string and empty second string in syslib
15. 1 input - valid dataset with empty first string and valid second string in syslib
"""


def validate_single_src(hosts, dest, sys_lib, src, location='DATA_SET', replace=True, member_list=None, dbd_name=None):
    # print(srcList);
    response = hosts.all.ims_dbd_gen(src=src, location=location, replace=replace, member_list=member_list, dbd_name=dbd_name, dest=dest, sys_lib=sys_lib)
    for result in response.contacted.values():
        pprint(result)
        print("Changed:", result['changed'])
        assert result['changed'] is False
        assert result['rc'] != 0
        # Check for success message (if we remove return codes)
        # assert std_error_string in result['msg']


def validate_batch(hosts, batch_list, dest, sys_lib):
    # print(batch_list);
    response = hosts.all.ims_dbd_gen(batch=batch_list, dest=dest, sys_lib=sys_lib)
    for result in response.contacted.values():
        pprint(result)
        print("Changed:", result['changed'])
        assert result['changed'] is False
        assert result['rc'] != 0
        # Check for success message (if we remove return codes)
        assert result['msg'] == GEN_FAIL_MSG
        # check for correct error message
        # assert std_error_string in result['batch_result'][-1]['return_text']


def test_dbd_gen_invalid_uss_valid_dataset_valid_seqDataset(ansible_zos_module):
    hosts = ansible_zos_module
    batch_list = [
        {'src': INVALID_USS, 'location': 'USS', 'replace': True},
        {'src': SOURCE, 'location': 'DATA_SET', 'member_list': ['DEDBJN21', 'AUTODB']},
        {'src': SOURCE, 'member_list': ['DEDBJN21', 'DEDBJNV1'], 'replace': True},
        {'src': SEQ, 'location': 'DATA_SET', 'dbd_name': 'SEQ1'}
    ]
    validate_batch(hosts, batch_list, DESTINATION, SYSLIB)


def test_dbd_gen_with_invalid_dataset_valid_uss(ansible_zos_module):
    hosts = ansible_zos_module
    batch_list = [
        {'src': INVALID_SOURCE, 'location': 'DATA_SET', 'member_list': ['DEDBJN21', 'DEDBJNV2']},
        {'src': SOURCE, 'member_list': ['DEDBJN21', 'AUTODB'], 'replace': True},
        {'src': INVALID_SOURCE, 'location': 'USS', 'replace': True}
    ]
    validate_batch(hosts, batch_list, DESTINATION, SYSLIB)


def test_dbd_gen_with_invalid_seqDataset_valid_uss(ansible_zos_module):
    hosts = ansible_zos_module
    batch_list = [
        {'src': INVALID_SEQ, 'location': 'DATA_SET', 'dbd_name': 'SEQ1'},
        {'src': USS, 'location': 'USS', 'replace': True}
    ]
    validate_batch(hosts, batch_list, DESTINATION, SYSLIB)


def test_dbd_gen_with_valid_dataset_invalid_member_valid_seqDataset(ansible_zos_module):
    hosts = ansible_zos_module
    batch_list = [
        {'src': SOURCE, 'location': 'DATA_SET', 'member_list': ['DEDBJN21', 'DEDBJNV1', 'INVALID']},
        {'src': SEQ, 'location': 'DATA_SET', 'dbd_name': 'SEQ1'}
    ]
    validate_batch(hosts, batch_list, DESTINATION, SYSLIB)


def test_dbd_gen_invalid_uss_invalid_dataset_invalid_seqDataset(ansible_zos_module):
    hosts = ansible_zos_module
    batch_list = [
        {'src': INVALID_USS, 'location': 'USS'},
        {'src': INVALID_SOURCE, 'location': 'DATA_SET', 'member_list': ['DEDBJN21']},
        {'src': INVALID_SEQ, 'location': 'DATA_SET', 'dbd_name': 'SEQ1'}
    ]
    validate_batch(hosts, batch_list, DESTINATION, SYSLIB)


def test_dbd_gen_valid_uss_invalid_location(ansible_zos_module):
    hosts = ansible_zos_module
    validate_single_src(hosts, DESTINATION, SYSLIB, USS, location='DATA_SET')


def test_dbd_gen_valid_dataset_invalid_location(ansible_zos_module):
    hosts = ansible_zos_module
    validate_single_src(hosts, DESTINATION, SYSLIB, src=SOURCE, location='USS', replace=True)


def test_dbd_gen_uss_replace(ansible_zos_module):
    hosts = ansible_zos_module
    validate_single_src(hosts, DESTINATION, SYSLIB, src=USS, location='USS', replace=False)


def test_dbd_gen_uss_emptyString(ansible_zos_module):
    hosts = ansible_zos_module
    validate_single_src(hosts, DESTINATION, SYSLIB, src='', location='USS', replace=True)


def test_dbd_gen_uss_destination_emptyString(ansible_zos_module):
    hosts = ansible_zos_module
    validate_single_src(hosts, "", SYSLIB, src=USS, location='USS', replace=True)


def test_dbd_gen_dataset_syslib_emptyList(ansible_zos_module):
    hosts = ansible_zos_module
    validate_single_src(hosts, DESTINATION, [], src=SOURCE, location='DATA_SET', member_list=['AUTODB'])


def test_dbd_gen_dataset_syslib_emptyFirstString(ansible_zos_module):
    hosts = ansible_zos_module
    validate_single_src(hosts, DESTINATION, ['', 'SYS1.MACLIB'], src=SOURCE, location='DATA_SET', member_list=['AUTODB'])


def test_dbd_gen_dataset_syslib_emptySecondString(ansible_zos_module):
    hosts = ansible_zos_module
    validate_single_src(hosts, DESTINATION, ['IMSBLD.I15RTSMM.SDFSMAC', ''], src=SOURCE, location='DATA_SET', member_list=['AUTODB'])


def test_dbd_gen_valid_seqDataset_invalid_location(ansible_zos_module):
    hosts = ansible_zos_module
    validate_single_src(hosts, DESTINATION, SYSLIB, src=SEQ, location='USS', dbd_name='SEQ1')
