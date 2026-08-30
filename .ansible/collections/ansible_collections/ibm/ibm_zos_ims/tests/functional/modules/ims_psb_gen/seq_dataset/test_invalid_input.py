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
sys.path.append(CURR_DIR)
import run_validate_failure  # pylint: disable=import-error
from ibm_zos_ims.tests.functional.module_utils.ims_test_gen_utils \
    import PSBInputParameters as ip

__metaclass__ = type

"""
Following datasets should be provisioned for the list of tests below
1. OMVSADM.IMSTESTU.ANSIBLE.PSBLIB : psb lib for destination
2. Empty OMVSADM.IMSTESTU.ANSIBLE.FB.PSBLIB in FB record format
3. Non empty input sequential dataset OMVSADM.IMSTESTU.ANS.SEQ
4. empty sequential dataset OMVSADM.IMSTESTU.ANS.EMPT.SEQ
5. Syslibs: IMSBLD.I15RTSMM.SDFSMAC, SYS1.MACLIB
"""

DESTINATION = ip.DESTINATION
SYSLIB = ip.SYSLIB
SOURCE = ip.SEQ_SOURCE

INVALID_DESTINATION = ip.INVALID_DESTINATION
INVALID_SEQ_SOURCE = ip.INVALID_SEQ_SOURCE
EMPTY_SEQ_SOURCE = ip.EMPTY_SEQ_SOURCE

GEN_FAIL_MSG = 'FAILURE - PSBGEN execution unsuccessful'
BATCH_SUCCESS_RETURN_TEXT = 'success'
INVALID_DEST_MSG = 'Destination data set does not exist or is not catalogued'


def process_batch(hosts, batch_list, dest, sys_lib, return_code, std_error_string, invalid_dest=False):
    print(batch_list)
    response = hosts.all.ims_psb_gen(
        batch=batch_list, dest=dest, sys_lib=sys_lib)
    for result in response.contacted.values():
        pprint(result)
        print("Changed:", result['changed'])
        assert not result['changed']
        assert result['rc'] == return_code
        # Check for success message (if we remove return codes)
        # TODO - message conflict (test_invalid_dest_seq_data_input_batch)
        if invalid_dest:
            assert result['msg'] == INVALID_DEST_MSG
        else:
            assert result['msg'] == GEN_FAIL_MSG

            # check for correct error message
            assert std_error_string in result['batch_result'][-1]['return_text']


def process_single_src(hosts, dest, sys_lib, return_code, std_error_string, src, location='DATA_SET', replace=True, member_list=None, psb_name=None):
    # print(srcList)
    response = hosts.all.ims_psb_gen(dest=dest, sys_lib=sys_lib, src=src, location=location, replace=replace, member_list=member_list, psb_name=psb_name)
    for result in response.contacted.values():
        pprint(result)
        print("Changed:", result['changed'])
        assert not result['changed']
        assert result['rc'] == return_code
        # Check for success message (if we remove return codes)
        assert std_error_string in result['msg']


# Here we pass seq data set as input with already existing destination with replace set to false and to expect fail
def test_valid_seq_data_set_input_single_src(ansible_zos_module):
    hosts = ansible_zos_module
    process_single_src(hosts, DESTINATION, SYSLIB, 3, 'Error assembling or linking source', src=SOURCE, replace=False, location='DATA_SET', psb_name='SEQ1')


def test_valid_seq_data_set_input_batch(ansible_zos_module):
    hosts = ansible_zos_module
    batch_list = [
        {'src': SOURCE, 'replace': False, 'location': "DATA_SET", 'psb_name': 'SEQ1'}
    ]
    process_batch(hosts, batch_list, DESTINATION, SYSLIB, 3, 'Error assembling or linking source')


# Here we pass invalid destination data set as input and and expect failure
def test_invalid_dest_seq_data_input_single_src(ansible_zos_module):
    hosts = ansible_zos_module
    process_single_src(hosts, INVALID_DESTINATION, SYSLIB, 1, INVALID_DEST_MSG, src=SOURCE, psb_name='SEQ1', location='DATA_SET')


def test_invalid_dest_seq_data_input_batch(ansible_zos_module):
    hosts = ansible_zos_module
    batch_list = [
        {'src': SOURCE, 'psb_name': 'SEQ1', 'location': 'DATA_SET'}
    ]
    process_batch(hosts, batch_list, INVALID_DESTINATION, SYSLIB, 1, INVALID_DEST_MSG, True)


# Here we pass invalid location name for SEQ data set as input source and expect failure
def test_invalid_location_seq_data_input_single_src(ansible_zos_module):
    hosts = ansible_zos_module
    process_single_src(hosts, DESTINATION, SYSLIB, 1, 'Failed to validate input file', src=SOURCE, psb_name='SEQ1', location='USS')


def test_invalid_location_seq_data_input_batch(ansible_zos_module):
    hosts = ansible_zos_module
    batch_list = [
        {'src': SOURCE, 'psb_name': 'SEQ1', 'location': 'USS'}
    ]
    process_batch(hosts, batch_list, DESTINATION, SYSLIB, 1, 'Failed to validate input file')


# Here we pass non existing SEQ data set as input source and expect failure
def test_non_existing_seq_data_input_single_src(ansible_zos_module):
    hosts = ansible_zos_module
    process_single_src(hosts, DESTINATION, SYSLIB, 1, 'Data set source does not exist', src=INVALID_SEQ_SOURCE, psb_name='SEQ1', location='DATA_SET')


def test_non_existing_seq_data_input_batch(ansible_zos_module):
    hosts = ansible_zos_module
    process_batch(hosts, [{'src': INVALID_SEQ_SOURCE, 'psb_name': 'SEQ1', 'location': "DATA_SET"}], DESTINATION, SYSLIB, 1, 'Data set source does not exist')


# Here we pass empty SEQ data set as input source and expect failure
def test_empty_seq_data_input_single_src(ansible_zos_module):
    hosts = ansible_zos_module

    process_single_src(
        hosts, DESTINATION, SYSLIB, 4,
        'Error assembling or linking source',
        src=EMPTY_SEQ_SOURCE, replace=True, psb_name='SEQ1',
        location='DATA_SET')


def test_empty_seq_data_input_batch(ansible_zos_module):
    hosts = ansible_zos_module
    batch_list = [
        {'src': EMPTY_SEQ_SOURCE, 'psb_name': 'SEQ1', 'location': 'DATA_SET'}
    ]
    process_batch(hosts, batch_list, DESTINATION, SYSLIB, 4, 'Error assembling or linking source')


def test_missing_psb_name_seq_data_input_single_src(ansible_zos_module):
    hosts = ansible_zos_module
    process_single_src(hosts, DESTINATION, SYSLIB, 1, 'psb_name is not set', src=SOURCE, location='DATA_SET')


def test_missing_psb_name_seq_data_input_batch(ansible_zos_module):
    hosts = ansible_zos_module
    batch_list = [{'src': SOURCE, 'location': "DATA_SET"}]
    process_batch(hosts, batch_list, DESTINATION, SYSLIB, 1, 'psb_name is not set')
