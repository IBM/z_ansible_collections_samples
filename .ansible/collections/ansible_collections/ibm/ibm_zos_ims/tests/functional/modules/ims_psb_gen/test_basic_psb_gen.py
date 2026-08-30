# -*- coding: utf-8 -*-
from __future__ import (absolute_import, division, print_function)
import pytest
from pprint import pprint
from ibm_zos_ims.tests.functional.module_utils.ims_test_gen_utils import PSBInputParameters as ip

__metaclass__ = type

GEN_SUCCESS_MSG = 'PSBGEN execution was successful.'

DESTINATION = ip.DESTINATION
SYSLIB = ip.SYSLIB
SOURCE = ip.SOURCE
LOCAL_SOURCE = "functional/modules/ims_psb_gen/uss_file/data/psbgen01"
REMOTE_SOURCE = ip.REMOTE_PSBGEN01_SOURCE


def test_ims_psb_gen_sample_batch(ansible_zos_module):
    hosts = ansible_zos_module
    hosts.all.copy(src=LOCAL_SOURCE, dest=REMOTE_SOURCE, mode='0777')
    batch_list = [
        {
            'src': SOURCE,
            'member_list': ["PSBGENL", "PSBGENL"],
            'location': "DATA_SET",
            'replace': True
        }
    ]
    results = hosts.all.ims_psb_gen(batch=batch_list, dest=DESTINATION, sys_lib=SYSLIB)

    for result in results.contacted.values():
        pprint(result)
        assert result['changed']
        assert result['rc'] == 0
        # Check for success message (if we remove return codes)
        assert result['msg'] == GEN_SUCCESS_MSG


def test_ims_psb_gen_sample_single_src(ansible_zos_module):
    hosts = ansible_zos_module

    results = hosts.all.ims_psb_gen(src=SOURCE, member_list=["PSBGENL", "PSBGENL"], location="DATA_SET", replace=True, dest=DESTINATION, sys_lib=SYSLIB)

    for result in results.contacted.values():
        pprint(result)
        assert result['changed']
        assert result['rc'] == 0
        # Check for success message (if we remove return codes)
        assert result['msg'] == GEN_SUCCESS_MSG
