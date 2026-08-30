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
from ibm_zos_ims.tests.functional.module_utils.ims_test_gen_utils import DBDInputParameters as ip

__metaclass__ = type


"""
Following datasets should be provisioned for the list of tests below
1. OMVSADM.IMSTESTU.ANSIBLE.DBDLIB : dbd lib for destination
2. Non empty input sequential dataset OMVSADM.IMSTESTU.ANS.SEQ
3. Syslibs: IMSBLD.I15RTSMM.SDFSMAC, SYS1.MACLIB
"""

DESTINATION = ip.DESTINATION
SYSLIB = ip.SYSLIB
SOURCE = ip.SEQ_SOURCE


def test_valid_seq_data_input_single_src(ansible_zos_module):
    # Here we pass valid seq data set as input source to expect successful generation of dbdlib
    hosts = ansible_zos_module
    run_validate_success.process_single_src(hosts, DESTINATION, SYSLIB, src=SOURCE, replace=True, location='DATA_SET', dbd_name='SEQ1')


def test_valid_seq_data_input_batch(ansible_zos_module):
    hosts = ansible_zos_module
    batch_list = [{'src': SOURCE, 'replace': True, 'location': "DATA_SET", 'dbd_name': 'SEQ1'}]
    run_validate_success.process_batch(hosts, batch_list, DESTINATION, SYSLIB)
