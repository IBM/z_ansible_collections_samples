################################################################################
# © Copyright IBM Corporation 2020, 2026
# Apache License, Version 2.0 (see https://opensource.org/licenses/Apache-2.0)
################################################################################

from __future__ import absolute_import, division, print_function
import json

__metaclass__ = type

def list_diff(git_list_str, apf_list_str):
    add_list_str = ""
    git_list = git_list_str.split(',')
    apf_data = json.loads(apf_list_str)
    
    if isinstance(apf_data, dict):
        apf_list = apf_data.get('entries', apf_data.get('list', []))
    else:
        apf_list = []
    
    for entry in git_list:
        ds = entry.split(':')[0]
        if not any(item.get('ds') == ds for item in apf_list):
            add_list_str += entry + ","
    
    return add_list_str[:-1] if add_list_str else ""

class FilterModule(object):
    """ Return the differ list to be added """

    def filters(self):
        filters = {
            "list_diff": list_diff,
        }
        return filters
