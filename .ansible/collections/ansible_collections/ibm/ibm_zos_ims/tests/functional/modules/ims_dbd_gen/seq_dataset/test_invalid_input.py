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
import run_validate_failure  # pylint: disable=import-error
from ibm_zos_ims.tests.functional.module_utils.ims_test_gen_utils import DBDInputParameters as ip

__metaclass__ = type

"""
Following datasets should be provisioned for the list of tests below
1. OMVSADM.IMSTESTU.ANSIBLE.DBDLIB : dbd lib for destination
2. Empty OMVSADM.IMSTESTU.ANSIBLE.FB.DBDLIB in FB record format
3. Non empty input sequential dataset OMVSADM.IMSTESTU.ANS.SEQ
4. empty sequential dataset OMVSADM.IMSTESTU.ANS.EMPT.SEQ
5. Syslibs: IMSBLD.I15RTSMM.SDFSMAC, SYS1.MACLIB
"""

DESTINATION = ip.DESTINATION
SYSLIB = ip.SYSLIB
SOURCE = ip.SEQ_SOURCE

FB_DESTINATION = ip.FB_DESTINATION
INVALID_SEQ_SOURCE = ip.INVALID_SEQ_SOURCE
EMPTY_SEQ_SOURCE = ip.EMPTY_SEQ_SOURCE


def test_valid_seq_data_set_input_single_src(ansible_zos_module):
    # Here we pass seq data set as input with already existing destination with replace set to false and to expect fail
    hosts = ansible_zos_module
    run_validate_failure.process_single_src(
        hosts,
        DESTINATION,
        SYSLIB,
        3,
        'Error assembling or linking source',
        src=SOURCE,
        replace=False,
        location='DATA_SET',
        dbd_name='SEQ1')


def test_valid_seq_data_set_input_batch(ansible_zos_module):
    hosts = ansible_zos_module
    batch_list = [
        {'src': SOURCE, 'replace': False, 'location': "DATA_SET", 'dbd_name': 'SEQ1'}]
    run_validate_failure.process_batch(hosts, batch_list, DESTINATION, SYSLIB, 3, 'Error assembling or linking source')


def test_non_existing_seq_data_input_single_src(ansible_zos_module):
    # Here we pass non existing SEQ data set as input source and expect failure
    hosts = ansible_zos_module
    run_validate_failure.process_single_src(
        hosts,
        DESTINATION,
        SYSLIB,
        1,
        'Data set source does not exist',
        src=INVALID_SEQ_SOURCE,
        dbd_name='SEQ1',
        location='DATA_SET')


def test_non_existing_seq_data_input_batch(ansible_zos_module):
    hosts = ansible_zos_module
    run_validate_failure.process_batch(
        hosts,
        [{'src': INVALID_SEQ_SOURCE, 'dbd_name': 'SEQ1', 'location': "DATA_SET"}],
        DESTINATION,
        SYSLIB,
        1,
        'Data set source does not exist')


def test_empty_seq_data_input_single_src(ansible_zos_module):
    # Here we pass empty SEQ data set as input source and expect failure
    hosts = ansible_zos_module
    run_validate_failure.process_single_src(
        hosts,
        DESTINATION,
        SYSLIB,
        4,
        'Error assembling or linking source',
        src=EMPTY_SEQ_SOURCE,
        replace=True,
        dbd_name='SEQ1',
        location='DATA_SET')


def test_empty_seq_data_input_batch(ansible_zos_module):
    hosts = ansible_zos_module
    batch_list = [
        {'src': EMPTY_SEQ_SOURCE, 'dbd_name': 'SEQ1', 'location': 'DATA_SET'}
    ]
    run_validate_failure.process_batch(hosts, batch_list, DESTINATION, SYSLIB, 4, 'Error assembling or linking source')


def test_missing_dbd_name_seq_data_input_single_src(ansible_zos_module):
    # Here we do not pass dbd_name for seq data set as input and expect failure
    hosts = ansible_zos_module
    run_validate_failure.process_single_src(hosts, DESTINATION, SYSLIB, 1, 'dbd_name is not set', src=SOURCE, location='DATA_SET')


def test_missing_dbd_name_seq_data_input_batch(ansible_zos_module):
    hosts = ansible_zos_module
    batch_list = [{'src': SOURCE, 'location': "DATA_SET"}]
    run_validate_failure.process_batch(hosts, batch_list, DESTINATION, SYSLIB, 1, 'dbd_name is not set')


def test_invalid_location_seq_data_input_single_src(ansible_zos_module):
    # Here we pass invalid location name for SEQ data set as input source and expect failure
    hosts = ansible_zos_module
    run_validate_failure.process_single_src(
        hosts,
        DESTINATION,
        SYSLIB,
        1,
        'Failed to validate input file',
        src=SOURCE,
        dbd_name='SEQ1',
        location='USS')


def test_invalid_location_seq_data_input_batch(ansible_zos_module):
    hosts = ansible_zos_module
    batch_list = [
        {'src': SOURCE, 'dbd_name': 'SEQ1', 'location': 'USS'}
    ]
    run_validate_failure.process_batch(hosts, batch_list, DESTINATION, SYSLIB, 1, 'Failed to validate input file')
