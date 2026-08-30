# -*- coding: utf-8 -*-

from __future__ import (absolute_import, division, print_function)
from pprint import pprint
import pytest
from ibm_zos_ims.tests.functional.module_utils.ims_test_gen_utils import PSBInputParameters as ip

__metaclass__ = type


GEN_SUCCESS_MSG = 'PSBGEN execution was successful.'
BATCH_SUCCESS_RETURN_TEXT = 'success'

SOURCE = ip.SOURCE
DESTINATION = ip.DESTINATION
SYSLIB = ip.SYSLIB

"""
Work flow for dataset functional tests with inputs as follows:
1. 1 dataset with 1 member
2. 1 dataset with 1 member with default location
3. 1 dataset with 1 member with replace set to True
4. 1 dataset with list of members
5. List of datasets with 1 member each
6. List of datasets with list of members
7. List of datasets with list of members with target names defined.
"""


def validate_single_src(hosts, dest, sys_lib, src, location='DATA_SET', replace=True, member_list=None, psb_name=None):
    # print(srcList)
    response = hosts.all.ims_psb_gen(dest=dest, sys_lib=sys_lib, src=src, location=location, replace=replace, member_list=member_list, psb_name=psb_name)
    for result in response.contacted.values():
        pprint(result)
        print("Changed:", result['changed'])
        assert result['changed']
        assert result['rc'] == 0
        # Check for success message (if we remove return codes)
        assert result['msg'] == GEN_SUCCESS_MSG


def validate_batch(hosts, batch_list, dest, sys_lib):
    # print(batch_list)
    response = hosts.all.ims_psb_gen(batch=batch_list, dest=dest, sys_lib=sys_lib)
    for result in response.contacted.values():
        pprint(result)
        print("Changed:", result['changed'])
        assert result['changed']
        assert result['changed']
        assert result['rc'] == 0
        # Check for success message (if we remove return codes)
        assert result['msg'] == GEN_SUCCESS_MSG
    for src_result in result['batch_result']:
        assert src_result['return_text'] == BATCH_SUCCESS_RETURN_TEXT


def test_psb_gen_dataset_member(ansible_zos_module):
    hosts = ansible_zos_module
    validate_single_src(hosts, DESTINATION, SYSLIB, src=SOURCE, location='DATA_SET', member_list=['PSBGENL'], replace=True)


def test_psb_gen_dataset_member_default_location(ansible_zos_module):
    hosts = ansible_zos_module
    validate_single_src(hosts, DESTINATION, SYSLIB, src=SOURCE, member_list=['PSBGENL'], replace=True)


def test_psb_gen_dataset_member_replace(ansible_zos_module):
    hosts = ansible_zos_module
    validate_single_src(hosts, DESTINATION, SYSLIB, src=SOURCE, location='DATA_SET', member_list=['PSBGENL'], replace=True)


def test_psb_gen_dataset_memberList(ansible_zos_module):
    hosts = ansible_zos_module
    validate_single_src(hosts, DESTINATION, SYSLIB, src=SOURCE, location='DATA_SET', member_list=["PSBGENL", "PSBLOAD"], replace=True)


def test_psb_gen_datasetList_member(ansible_zos_module):
    hosts = ansible_zos_module
    batch_list = [
        {'src': SOURCE, 'location': "DATA_SET", 'member_list': ["PSBLOAD"]},
        {'src': SOURCE, 'location': "DATA_SET", 'member_list': ["PSBGENL"], 'replace': True}
    ]
    validate_batch(hosts, batch_list, DESTINATION, SYSLIB)


def test_psg_gen_datasetList_memberList(ansible_zos_module):
    hosts = ansible_zos_module
    batch_list = [
        {'src': SOURCE, 'location': "DATA_SET", 'member_list': ["PSBGENL", "PSBLOAD"]},
        {'src': SOURCE, 'location': "DATA_SET", 'member_list': ["PSBGENL", "PSBNO"], 'replace': True}]
    validate_batch(hosts, batch_list, DESTINATION, SYSLIB)


def test_dbd_gen_dataset_targetName(ansible_zos_module):
    hosts = ansible_zos_module
    validate_single_src(hosts, DESTINATION, SYSLIB, src=SOURCE,
                        location='DATA_SET',
                        member_list=[{'PSBLOAD': 'psb1'}, {'PSBNO': 'psb2'}, {'PSBGENL': 'psb3'}],
                        replace=True)


def test_dbd_gen_datasetList_targetName(ansible_zos_module):
    hosts = ansible_zos_module
    batch_list = [
        {
            'src': SOURCE,
            'location': "DATA_SET",
            'member_list': [
                {"PSBGENL": 'psb4'},
                {"PSBLOAD": 'psb5'}]
        },
        {
            'src': SOURCE,
            'location': "DATA_SET",
            'member_list': [
                {"PSBLOAD": 'psb6'},
                {"PSBGENL": 'psb7'}
            ],
            'replace': True
        }
    ]
    validate_batch(hosts, batch_list, DESTINATION, SYSLIB)
