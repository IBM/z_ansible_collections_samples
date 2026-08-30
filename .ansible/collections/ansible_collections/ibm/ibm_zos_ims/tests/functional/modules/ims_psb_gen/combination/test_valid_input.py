# -*- coding: utf-8 -*-

from __future__ import (absolute_import, division, print_function)
from pprint import pprint
import pytest
from ibm_zos_ims.tests.functional.module_utils.ims_test_gen_utils import PSBInputParameters as ip

__metaclass__ = type


SOURCE = ip.SOURCE
DESTINATION = ip.DESTINATION
SYSLIB = ip.SYSLIB
SEQ = ip.SEQ_SOURCE
USS = ip.REMOTE_PSBGEN01_SOURCE

"""
Work flow for Combination functional tests goes as follows:
1. prereq for acbgen
2. All three inputs - uss, dataset, seq dataset
3. 2 inputs - uss, dataset
4. 2 inputs - uss, seq dataset
5. 2 inputs - dataset, seq dataset
6. list of datasets with list of members -- single src and batch
7. memberlist with target names defined
"""

GEN_SUCCESS_MSG = 'PSBGEN execution was successful.'
BATCH_SUCCESS_RETURN_TEXT = 'success'


def validate_single_src(hosts, dest, sys_lib, src, location='DATA_SET', replace=True, member_list=None, psb_name=None):
    # print(srcList);
    response = hosts.all.ims_psb_gen(src=src, location=location, replace=replace, member_list=member_list, psb_name=psb_name, dest=dest, sys_lib=sys_lib)
    for result in response.contacted.values():
        pprint(result)
        print("Changed:", result['changed'])
        assert result['changed']
        assert result['rc'] == 0
        # Check for success message (if we remove return codes)
        assert result['msg'] == GEN_SUCCESS_MSG


def validate_batch(hosts, batch_list, dest, sys_lib):
    print(batch_list)
    response = hosts.all.ims_psb_gen(batch=batch_list, dest=dest, sys_lib=sys_lib)
    for result in response.contacted.values():
        pprint(result)
        print("Changed:", result['changed'])
        assert result['changed']
        assert result['changed']
        assert result['rc'] == 0
        # Check for success message (if we remove return codes)
        assert result['msg'] == GEN_SUCCESS_MSG


def test_psb_gen_dataset_prereq(ansible_zos_module):
    hosts = ansible_zos_module
    response = hosts.all.ims_psb_gen(
        dest=DESTINATION, sys_lib=SYSLIB, src=SOURCE, location="DATA_SET",
        replace=True, member_list=["PSBGENL"])
    for result in response.contacted.values():
        pprint(result)
        print("Changed:", result['changed'])
        assert result['changed']
        assert result['rc'] == 0
        # Check for success message (if we remove return codes)
        assert result['msg'] == GEN_SUCCESS_MSG


def test_psb_gen_basic_combination(ansible_zos_module):
    hosts = ansible_zos_module
    hosts.all.copy(src='./functional/modules/ims_psb_gen/uss_file/data/psbgen01', dest=USS, checksum='58715368daf0bcfddb5947900423702aad30fc51', mode='0777')
    batch_list = [
        {'src': USS, 'location': 'USS', 'replace': True},
        {'src': SOURCE, 'location': 'DATA_SET', 'member_list': ['PSBGENL', 'PSBLOAD']},
        {'src': SOURCE, 'member_list': ['PSBGENL', 'PSBLOAD'], 'replace': True},
        {'src': SEQ, 'location': 'DATA_SET', 'psb_name': 'SEQ1'}
    ]
    validate_batch(hosts, batch_list, DESTINATION, SYSLIB)


def test_psb_gen_uss_dataset(ansible_zos_module):
    hosts = ansible_zos_module
    hosts.all.copy(src='./functional/modules/ims_psb_gen/uss_file/data/psbgen01', dest=USS, checksum='58715368daf0bcfddb5947900423702aad30fc51', mode='0777')
    batch_list = [
        {'src': USS, 'location': 'USS', 'replace': True},
        {'src': SOURCE, 'location': 'DATA_SET', 'member_list': ['PSBGENL', 'PSBNOOG']},
        {'src': SOURCE, 'member_list': ['PSBGENL', 'PSBLOAD'], 'replace': True}]
    validate_batch(hosts, batch_list, DESTINATION, SYSLIB)


def test_psb_gen_uss_seqDataset(ansible_zos_module):
    hosts = ansible_zos_module
    hosts.all.copy(src='./functional/modules/ims_psb_gen/uss_file/data/psbgen01', dest=USS, checksum='58715368daf0bcfddb5947900423702aad30fc51', mode='0777')
    batch_list = [
        {'src': USS, 'location': 'USS', 'replace': True},
        {'src': SEQ, 'location': 'DATA_SET', 'psb_name': 'SEQ1'}
    ]
    validate_batch(hosts, batch_list, DESTINATION, SYSLIB)


def test_psb_gen_dataset_seqDataset(ansible_zos_module):
    hosts = ansible_zos_module
    batch_list = [
        {'src': SOURCE, 'location': 'DATA_SET', 'member_list': ['PSBGENL', 'PSBLOAD']},
        {'src': SEQ, 'location': 'DATA_SET', 'psb_name': 'SEQ1', 'replace': True}
    ]
    validate_batch(hosts, batch_list, DESTINATION, SYSLIB)


def test_pdb_gen_list_datasets(ansible_zos_module):
    hosts = ansible_zos_module
    batch_list = [
        {
            'src': SOURCE, 'location': 'DATA_SET',
            'member_list': ['PSBGENL', 'PSBNOOG', 'PSBGENL', 'PSBLOAD', 'PSBGENL', 'PSBLOAD', 'PSBGENL', 'PSBLOAD'],
            'replace': True
        },
        {'src': SOURCE, 'location': 'DATA_SET', 'member_list': ['PSBGENL', 'PSBNOOG']},
        {'src': SOURCE, 'member_list': ['PSBGENL', 'PSBLOAD'], 'replace': True}
    ]
    validate_batch(hosts, batch_list, DESTINATION, SYSLIB)


def test_psb_gen_single_src_member_list(ansible_zos_module):
    hosts = ansible_zos_module
    validate_single_src(
        hosts, DESTINATION, SYSLIB, src=SOURCE, location='DATA_SET',
        member_list=['PSBGENL', 'PSBNOOG', 'PSBGENL', 'PSBLOAD', 'PSBGENL', 'PSBLOAD', 'PSBGENL', 'PSBLOAD'], replace=True)


def test_psb_gen_target_name(ansible_zos_module):
    hosts = ansible_zos_module
    batch_list = [
        {'src': SOURCE, 'location': 'DATA_SET', 'member_list': [{'PSBGENL': 'H1'}, {'PSBGENL': 'H2'}], 'replace': True},
        {'src': SEQ, 'psb_name': 'H3'}
    ]
    validate_batch(hosts, batch_list, DESTINATION, SYSLIB)
