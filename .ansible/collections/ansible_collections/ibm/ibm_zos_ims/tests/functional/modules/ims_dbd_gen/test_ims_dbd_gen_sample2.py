# -*- coding: utf-8 -*-

from __future__ import (absolute_import, division, print_function)

import os
import sys
import warnings

import ansible.constants
import ansible.errors
import ansible.utils
import pytest
from pprint import pprint
from ibm_zos_ims.tests.functional.module_utils.ims_test_gen_utils import DBDInputParameters as ip

__metaclass__ = type

GEN_SUCCESS_MSG = 'DBDGEN execution was successful.'
BATCH_SUCCESS_RETURN_TEXT = 'success'


def test_ims_dbd_gen_sample(ansible_zos_module):
    hosts = ansible_zos_module
    source = ip.SOURCE

    dest = ip.DESTINATION
    sys_lib = ip.SYSLIB
    results = hosts.all.ims_dbd_gen(src=source, location="DATA_SET", replace=True, member_list=['DEDBJN21', 'DEDBJN21'], dest=dest, sys_lib=sys_lib)

    for result in results.contacted.values():
        pprint(result)
    assert result['changed']
    # Check return code for array of output for each source
    assert result['rc'] == 0

    # Check for success message (if we remove return codes)
    assert result['msg'] == GEN_SUCCESS_MSG


def test_ims_dbd_gen_sample_batch(ansible_zos_module):
    hosts = ansible_zos_module
    source = ip.SOURCE
    dest = ip.DESTINATION
    sys_lib = ip.SYSLIB
    batch_list = [{
        'src': source,
        'location': 'DATA_SET',
        'replace': True,
        'member_list': 'DEDBJN21'}]
    results = hosts.all.ims_dbd_gen(batch=batch_list, dest=dest, sys_lib=sys_lib)

    for result in results.contacted.values():
        pprint(result)
    assert result['changed']
    # Check return code for array of output for each source
    assert result['rc'] == 0
    # Check for success message (if we remove return codes)
    assert result['msg'] == GEN_SUCCESS_MSG

    for src_result in result['batch_result']:
        assert src_result['return_text'] == BATCH_SUCCESS_RETURN_TEXT
