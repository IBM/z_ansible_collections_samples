# -*- coding: utf-8 -*-

from __future__ import (absolute_import, division, print_function)

from ansible.module_utils.basic import AnsibleModule
from pprint import pprint

import os
import sys

__metaclass__ = type

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
