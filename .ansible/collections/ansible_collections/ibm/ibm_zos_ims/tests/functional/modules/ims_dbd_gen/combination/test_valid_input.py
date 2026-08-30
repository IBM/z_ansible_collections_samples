# -*- coding: utf-8 -*-
from __future__ import (absolute_import, division, print_function)
from pprint import pprint
import pytest
from ibm_zos_ims.tests.functional.module_utils.ims_test_gen_utils import DBDInputParameters as ip

__metaclass__ = type


SOURCE = ip.SOURCE
DESTINATION = ip.DESTINATION
SYSLIB = ip.SYSLIB
SEQ = ip.SEQ_SOURCE
USS = ip.REMOTE_DBDGEN02_SOURCE

"""
Work flow for Combination functional tests goes as follows:
1. All three inputs - uss, dataset, seq dataset
2. 2 inputs - uss, dataset
3. 2 inputs - uss, seq dataset
4. 2 inputs - dataset, seq dataset
5. list of datasets with list of members -- batch and single src
6. memberlist with target names defined
7. prereq for acbgen tests
"""

GEN_SUCCESS_MSG = 'DBDGEN execution was successful.'
BATCH_SUCCESS_RETURN_TEXT = 'success'


def validate_single_src(hosts, dest, sys_lib, src, location='DATA_SET', replace=True, member_list=None, dbd_name=None):
    # print(srcList);
    response = hosts.all.ims_dbd_gen(src=src, location=location, replace=replace, member_list=member_list, dbd_name=dbd_name, dest=dest, sys_lib=sys_lib)
    for result in response.contacted.values():
        pprint(result)
        print("Changed:", result['changed'])
        assert result['changed']
        assert result['rc'] == 0
        # Check for success message (if we remove return codes)
        assert result['msg'] == GEN_SUCCESS_MSG


def validate_batch(hosts, batch_list, dest, sys_lib):
    print(batch_list)
    response = hosts.all.ims_dbd_gen(batch=batch_list, dest=dest, sys_lib=sys_lib)
    for result in response.contacted.values():
        pprint(result)
        print("Changed:", result['changed'])
        assert result['changed']
        assert result['rc'] == 0
        # Check for success message (if we remove return codes)
        assert result['msg'] == GEN_SUCCESS_MSG


def test_dbd_gen_basic_combination(ansible_zos_module):
    hosts = ansible_zos_module
    batch_list = [
        {'src': USS, 'location': 'USS', 'replace': True},
        {'src': SOURCE, 'location': 'DATA_SET', 'member_list': ['DEDBJN21', 'AUTODB']},
        {'src': SOURCE, 'member_list': ['DEDBJN21', 'DEDBJNV1'], 'replace': True},
        {'src': SEQ, 'location': 'DATA_SET', 'dbd_name': 'SEQ1'}
    ]
    validate_batch(hosts, batch_list, DESTINATION, SYSLIB)


def test_dbd_gen_uss_dataset(ansible_zos_module):
    hosts = ansible_zos_module
    batch_list = [
        {'src': USS, 'location': 'USS', 'replace': True},
        {'src': SOURCE, 'location': 'DATA_SET', 'member_list': ['DEDBJN21', 'DEDBJNV2']},
        {'src': SOURCE, 'member_list': ['DEDBJN21', 'AUTODB'], 'replace': True}]
    validate_batch(hosts, batch_list, DESTINATION, SYSLIB)


def test_dbd_gen_uss_seqDataset(ansible_zos_module):
    hosts = ansible_zos_module
    batch_list = [
        {'src': USS, 'location': 'USS', 'replace': True},
        {'src': SEQ, 'location': 'DATA_SET', 'dbd_name': 'SEQ1'}
    ]
    validate_batch(hosts, batch_list, DESTINATION, SYSLIB)


def test_dbd_gen_dataset_seqDataset(ansible_zos_module):
    hosts = ansible_zos_module
    batch_list = [
        {'src': SOURCE, 'location': 'DATA_SET', 'member_list': ['DEDBJN21', 'DEDBJNV1']},
        {'src': SEQ, 'location': 'DATA_SET', 'dbd_name': 'SEQ1', 'replace': True}
    ]
    validate_batch(hosts, batch_list, DESTINATION, SYSLIB)


def test_dbd_gen_list_datasets(ansible_zos_module):
    hosts = ansible_zos_module
    batch_list = [
        {'src': SOURCE,
            'location': 'DATA_SET', 'member_list': ['DEDBJN21', 'DEDBJNV2', 'DEDBJN21', 'AUTODB', 'DEDBJN21', 'AUTODB', 'DEDBJN21', 'AUTODB'],
            'replace': True},
        {'src': SOURCE, 'location': 'DATA_SET', 'member_list': ['DEDBJN21', 'DEDBJNV2']},
        {'src': SOURCE, 'member_list': ['DEDBJN21', 'AUTODB'], 'replace': True}
    ]
    validate_batch(hosts, batch_list, DESTINATION, SYSLIB)


def test_dbd_gen_single_src_member_list(ansible_zos_module):
    hosts = ansible_zos_module
    validate_single_src(
        hosts,
        DESTINATION,
        SYSLIB,
        src=SOURCE,
        location='DATA_SET',
        member_list=['DEDBJN21', 'DEDBJNV2', 'DEDBJN21', 'AUTODB', 'DEDBJN21', 'AUTODB', 'DEDBJN21', 'AUTODB'],
        replace=True)


def test_dbd_gen_target_name(ansible_zos_module):
    hosts = ansible_zos_module
    batch_list = [
        {'src': SOURCE, 'location': 'DATA_SET', 'member_list': [{'DEDBJNV1': 'D1'}, {'DEDBJNV2': 'D2'}], 'replace': True},
        {'src': SEQ, 'location': 'DATA_SET', 'dbd_name': 'D3', 'replace': True}
    ]
    validate_batch(hosts, batch_list, DESTINATION, SYSLIB)


def test_dbd_gen_dataset_prereq(ansible_zos_module):
    hosts = ansible_zos_module
    # validate_single_src(hosts, DESTINATION, SYSLIB, src=SOURCE, location='DATA_SET', member_list=['DEDBJN21'], replace=True)
    response = hosts.all.ims_dbd_gen(
        dest=DESTINATION,
        sys_lib=SYSLIB, src=SOURCE, location="DATA_SET",
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
        assert result['msg'] == GEN_SUCCESS_MSG
