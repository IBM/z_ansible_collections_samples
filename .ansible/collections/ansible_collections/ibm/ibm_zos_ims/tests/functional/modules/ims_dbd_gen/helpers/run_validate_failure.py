from __future__ import (absolute_import, division, print_function)
from ansible.module_utils.basic import AnsibleModule
from pprint import pprint

import os
import sys

GEN_FAIL_MSG = 'FAILURE - DBDGEN execution unsuccessful.'
BATCH_SUCCESS_RETURN_TEXT = 'success'

__metaclass__ = type


def process_batch(hosts, batch_list, dest, sys_lib, return_code, std_error_string):
    # print(srcList)
    response = hosts.all.ims_dbd_gen(
        batch=batch_list,
        dest=dest,
        sys_lib=sys_lib)
    for result in response.contacted.values():
        pprint(result)
        print("Changed:", result['changed'])
        assert result['changed'] is False
        assert result['rc'] == return_code
        # Check for success message (if we remove return codes)
        assert result['msg'] == GEN_FAIL_MSG
        # check for correct error message
        assert std_error_string in result['batch_result'][-1]['return_text']


def process_single_src(hosts, dest, sys_lib, return_code, std_error_string, src, location='DATA_SET', replace=True, member_list=None, dbd_name=None):
    response = hosts.all.ims_dbd_gen(dest=dest, sys_lib=sys_lib, src=src, location=location, replace=replace, member_list=member_list, dbd_name=dbd_name)
    for result in response.contacted.values():
        pprint(result)
        print("Changed:", result['changed'])
        assert result['changed'] is False
        assert result['rc'] == return_code
        # Check for success message (if we remove return codes)
        assert std_error_string in result['msg']
