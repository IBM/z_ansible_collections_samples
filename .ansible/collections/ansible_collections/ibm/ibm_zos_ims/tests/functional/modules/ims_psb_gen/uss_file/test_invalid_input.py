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
from ibm_zos_ims.tests.functional.module_utils.ims_test_gen_utils import PSBInputParameters as ip

__metaclass__ = type

"""
Following datasets should be provisioned for the list of tests below
1. OMVSADM.IMSTESTU.ANSIBLE.PSBLIB : psb lib for destination
2. Syslibs: IMSBLD.I15RTSMM.SDFSMAC, SYS1.MACLIB
"""

DESTINATION = ip.DESTINATION
SYSLIB = ip.SYSLIB
SOURCE = ip.REMOTE_PSBGEN01_SOURCE
EMPTY_SOURCE = ip.LOCAL_EMPTY_SOURCE
FB_DESTINATION = ip.FB_DESTINATION

INVALID_SOURCE = "/tmp/psbgeninvalid"


# TODO: Revise this test, seems to have weird outcomes
# Here we pass invalid destination with FB record format as input source to expect failure
# def test_invalid_uss_file_input(ansible_zos_module):
#     DESTINATION = ip.FB_DESTINATION
#     hosts = ansible_zos_module
#     copy = hosts.all.copy(src='functional/modules/ims_psb_gen/uss_file/data/psbgen01',
#                           dest=SOURCE)
#     run_validate_failure.process(hosts, [{'src': SOURCE,
#                                           'location': "USS"}],
#                                  DESTINATION,
#                                  SYSLIB, 12, 'IEW2735S')
# Here we pass invalid destination with FB record format as input source to expect failure
# def test_invalid_uss_file_input_single_src(ansible_zos_module):
#     hosts = ansible_zos_module
#     copy = hosts.all.copy(src='functional/modules/ims_psb_gen/uss_file/data/psbgen01', dest=SOURCE)
#     pprint(copy)
#     run_validate_failure.process_single_src(hosts, FB_DESTINATION, SYSLIB, 12, 'Error assembling or linking source', src=SOURCE, location='USS', replace=True)

# def test_invalid_uss_file_input_batch(ansible_zos_module):
#     hosts = ansible_zos_module
#     copy = hosts.all.copy(src='functional/modules/ims_psb_gen/uss_file/data/psbgen01', dest=SOURCE)
#     pprint(copy)
#     batch_list = [{'src': SOURCE, 'location': "USS", 'replace':True}]
#     run_validate_failure.process_batch(hosts, batch_list, FB_DESTINATION, SYSLIB, 12, 'Error assembling or linking')

# Here we pass invalid location name for USS file as input source to expect failure

def test_invalid_location_file_input_single_src(ansible_zos_module):
    hosts = ansible_zos_module
    run_validate_failure.process_single_src(hosts, DESTINATION, SYSLIB, 1, 'invalid data set name', src=SOURCE, replace=True, location='DATA_SET')


def test_invalid_location_file_input_batch(ansible_zos_module):
    hosts = ansible_zos_module
    batch_list = [{'src': SOURCE, 'replace': True, 'location': 'DATA_SET'}]
    run_validate_failure.process_batch(hosts, batch_list, DESTINATION, SYSLIB, 1, 'invalid data set name')


# Here we pass non existing USS file as input source to expect failure
def test_non_existing_file_input_single_src(ansible_zos_module):
    hosts = ansible_zos_module
    run_validate_failure.process_single_src(hosts, DESTINATION, SYSLIB, 1, 'Failed to validate input file', src=INVALID_SOURCE, replace=True, location='USS')


def test_non_existing_file_input_batch(ansible_zos_module):
    hosts = ansible_zos_module
    batch_list = [{'src': INVALID_SOURCE, 'replace': True, 'location': "USS"}]
    run_validate_failure.process_batch(hosts, batch_list, DESTINATION, SYSLIB, 1, 'Failed to validate input file')


# Here we pass empty  USS file as input source to expect failure
def test_empty_uss_file_input_single_src(ansible_zos_module):
    hosts = ansible_zos_module
    SOURCE = "/tmp/psbgenEmpty"
    copy = hosts.all.copy(src=EMPTY_SOURCE, dest=SOURCE, mode='0777')
    pprint(copy)
    run_validate_failure.process_single_src(
        hosts, DESTINATION, SYSLIB, 4, 'Error assembling or linking source',
        src=SOURCE, replace=True, location='USS')


def test_empty_uss_file_input_batch(ansible_zos_module):
    hosts = ansible_zos_module
    SOURCE = "/tmp/psbgenEmpty"
    copy = hosts.all.copy(src=EMPTY_SOURCE, dest=SOURCE, mode='0777')
    pprint(copy)
    batch_list = [{'src': SOURCE, 'replace': True, 'location': "USS"}]
    run_validate_failure.process_batch(hosts, batch_list, DESTINATION, SYSLIB, 4, 'Error assembling or linking source')
