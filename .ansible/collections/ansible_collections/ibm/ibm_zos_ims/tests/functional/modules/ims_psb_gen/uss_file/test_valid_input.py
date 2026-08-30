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
2. Syslibs: IMSBLD.I15RTSMM.SDFSMAC, SYS1.MACLIB
"""

DESTINATION = ip.DESTINATION
SYSLIB = ip.SYSLIB
SOURCE = ip.REMOTE_PSBGEN01_SOURCE

LARGE_SOURCE = '/tmp/PWMGAT.psb'


# Here we pass valid USS file as input source to expect successful generation of psblib
def test_valid_uss_file_input_single_src(ansible_zos_module):
    hosts = ansible_zos_module
    hosts.all.copy(src='./functional/modules/ims_psb_gen/uss_file/data/psbgen01', dest=SOURCE, checksum='58715368daf0bcfddb5947900423702aad30fc51', mode='0777')
    run_validate_success.process_single_src(hosts, DESTINATION, SYSLIB, src=SOURCE, replace=True, location='USS')


def test_valid_large_uss_file_input_single_src(ansible_zos_module):
    hosts = ansible_zos_module
    hosts.all.copy(
        src='./functional/modules/ims_psb_gen/uss_file/data/PWMGAT.psb', dest=LARGE_SOURCE, checksum='0b335c7b949129115a7cc282f395bccec6675f7f', mode='0777')
    run_validate_success.process_single_src(hosts, DESTINATION, SYSLIB, src=LARGE_SOURCE, replace=True, location='USS')


def test_valid_uss_file_input_batch(ansible_zos_module):
    hosts = ansible_zos_module
    hosts.all.copy(src='./functional/modules/ims_psb_gen/uss_file/data/psbgen01', dest=SOURCE, checksum='58715368daf0bcfddb5947900423702aad30fc51', mode='0777')
    hosts.all.copy(
        src='./functional/modules/ims_psb_gen/uss_file/data/PWMGAT.psb', dest=LARGE_SOURCE,
        checksum='0b335c7b949129115a7cc282f395bccec6675f7f', mode='0777')
    batch_list = [
        {'src': SOURCE, 'location': "USS", 'replace': True},
        {'src': LARGE_SOURCE, 'location': "USS", 'replace': True}
    ]
    run_validate_success.process_batch(hosts, batch_list, DESTINATION, SYSLIB)
