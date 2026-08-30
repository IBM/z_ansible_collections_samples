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

USS = ip.REMOTE_DBDGEN02_SOURCE
SOURCE = ip.SOURCE
DESTINATION = ip.DESTINATION
SEQ = ip.SEQ_SOURCE
SYSLIB = ip.SYSLIB
LOCAL_SOURCE = "functional/modules/ims_dbd_gen/uss_file/data/dbdgen02"
REMOTE_SOURCE = ip.REMOTE_DBDGEN02_SOURCE


def test_ims_dbd_gen_sample(ansible_zos_module):
    hosts = ansible_zos_module
    dest = DESTINATION
    sys_lib = SYSLIB
    hosts.all.copy(
        src='./functional/modules/ims_dbd_gen/uss_file/data/dbdgen02', dest=REMOTE_SOURCE,
        checksum='5dd4785e9f4a7d4c4bc36e15ce3b58223113a680', mode='0777')
    results = hosts.all.ims_dbd_gen(src=SOURCE, location="DATA_SET", member_list=["DEDBJN21", "DEDBJN21"], dest=dest, sys_lib=sys_lib)
    for result in results.contacted.values():
        pprint(result)
        assert result['changed']
        assert result['rc'] == 0
        # Check for success message (if we remove return codes)
        assert result['msg'] == GEN_SUCCESS_MSG


def test_ims_dbd_gen_sample_batch(ansible_zos_module):
    hosts = ansible_zos_module
    src_list = [{'src': USS, 'location': "USS", "replace": True},
                {'src': SOURCE,
                    'location': "DATA_SET",
                    'member_list': ["DEDBJN21", "DEDBJN21"]},
                {'src': SOURCE,
                    'member_list': ["DEDBJN21", "DEDBJN21"],
                    'replace': True},
                {'src': SEQ,
                    'location': "DATA_SET",
                    'dbd_name': 'SEQ1'}]
    dest = DESTINATION
    sys_lib = SYSLIB
    results = hosts.all.ims_dbd_gen(batch=src_list, dest=dest, sys_lib=sys_lib)
    for result in results.contacted.values():
        pprint(result)
        assert result['changed']
        assert result['rc'] == 0
        # Check for success message (if we remove return codes)
        assert result['msg'] == GEN_SUCCESS_MSG

    # Check return code for array of output for each source
    for src_result in result['batch_result']:
        assert src_result['return_text'] == BATCH_SUCCESS_RETURN_TEXT
