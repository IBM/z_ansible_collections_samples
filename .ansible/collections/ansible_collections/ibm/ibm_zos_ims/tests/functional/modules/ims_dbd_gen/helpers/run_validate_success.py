from __future__ import (absolute_import, division, print_function)
from ansible.module_utils.basic import AnsibleModule
from pprint import pprint

import os
import sys

GEN_SUCCESS_MSG = 'DBDGEN execution was successful.'
BATCH_SUCCESS_RETURN_TEXT = 'success'

__metaclass__ = type


def process_single_src(hosts, dest, sys_lib, src, location='DATA_SET', replace=True, member_list=None, dbd_name=None):
    response = hosts.all.ims_dbd_gen(dest=dest, sys_lib=sys_lib, src=src, location=location, replace=replace, member_list=member_list, dbd_name=dbd_name)
    for result in response.contacted.values():
        print("Changed:", result['changed'])
        assert result['changed']
        assert result['rc'] == 0
        # Check for success message (if we remove return codes)
        assert result['msg'] == GEN_SUCCESS_MSG


def process_batch(hosts, batch_list, dest, sys_lib):
    print(batch_list)
    response = hosts.all.ims_dbd_gen(
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
