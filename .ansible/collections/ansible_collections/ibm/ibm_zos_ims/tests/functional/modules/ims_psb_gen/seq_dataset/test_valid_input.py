# -*- coding: utf-8 -*-

from __future__ import (absolute_import, division, print_function)
from ansible.module_utils.basic import AnsibleModule
from pprint import pprint
import pytest
import ansible.utils
import ansible.errors
import ansible.constants
import warnings
import os
import sys
CURR_DIR = os.path.dirname(__file__) + "/../helpers"
# print(CURR_DIR)
sys.path.append(CURR_DIR)
# for path in sys.path:
#    print(path)
import run_validate_success  # pylint: disable=import-error
from ibm_zos_ims.tests.functional.module_utils.ims_test_gen_utils import PSBInputParameters as ip

__metaclass__ = type


"""
Following datasets should be provisioned for the list of tests below
1. OMVSADM.IMSTESTU.ANSIBLE.PSBLIB : psb lib for destination
2. Non empty input sequential dataset OMVSADM.IMSTESTU.ANS.SEQ
3. Syslibs: IMSBLD.I15RTSMM.SDFSMAC, SYS1.MACLIB
"""

DESTINATION = ip.DESTINATION
SYSLIB = ip.SYSLIB
SOURCE = ip.SEQ_SOURCE

GEN_SUCCESS_MSG = 'PSBGEN execution was successful.'
BATCH_SUCCESS_RETURN_TEXT = 'success'


def process_single_src(hosts, dest, sys_lib, src, location='DATA_SET', replace=True, member_list=None, psb_name=None):
    # print(srcList)

    response = hosts.all.ims_psb_gen(dest=dest, sys_lib=sys_lib, src=src, location=location, replace=replace, member_list=member_list, psb_name=psb_name)
    for result in response.contacted.values():
        print("Changed:", result['changed'])
        assert result['changed']
        assert result['rc'] == 0
        # Check for success message (if we remove return codes)
        assert result['msg'] == GEN_SUCCESS_MSG


def process_batch(hosts, batch_list, dest, sys_lib):
    print(batch_list)
    response = hosts.all.ims_psb_gen(
        batch=batch_list, dest=dest, sys_lib=sys_lib)
    for result in response.contacted.values():
        print("Changed:", result['changed'])
        assert result['changed']
        assert result['rc'] == 0
        # Check for success message (if we remove return codes)
        assert result['msg'] == GEN_SUCCESS_MSG

    # Check return code for array of output for each source
    for src_result in result['batch_result']:
        assert src_result['return_text'] == BATCH_SUCCESS_RETURN_TEXT


# Here we pass valid seq data set as input source to expect successful generation of psblib
def test_valid_seq_data_input_single_src(ansible_zos_module):
    hosts = ansible_zos_module
    process_single_src(hosts, DESTINATION, SYSLIB, src=SOURCE, replace=True, location='DATA_SET', psb_name='SEQ1')


def test_valid_seq_data_input_batch(ansible_zos_module):
    hosts = ansible_zos_module
    batch_list = [{'src': SOURCE, 'replace': True, 'location': "DATA_SET", 'psb_name': 'SEQ1'}]
    process_batch(hosts, batch_list, DESTINATION, SYSLIB)
