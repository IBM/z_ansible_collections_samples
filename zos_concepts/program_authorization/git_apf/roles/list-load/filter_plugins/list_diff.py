################################################################################
# © Copyright IBM Corporation 2020
# Apache License, Version 2.0 (see https://opensource.org/licenses/Apache-2.0)
################################################################################

from __future__ import absolute_import, division, print_function
import json

__metaclass__ = type

def list_diff(git_list_str, apf_list_str):
    add_list = []
    add_list_str = ""
    git_list = git_list_str.split(',')
    apf_list = json.loads(apf_list_str)

    for entry in git_list:
        ds = entry.split(':')[0]
        if not any(item['ds'] == ds for item in apf_list[2:]):
            add_list_str += entry + ","

    return add_list_str[:-1]


class FilterModule(object):
    """ Return the differ list to be added """

    def filters(self):
        filters = {
            "list_diff": list_diff,
        }
        return filters
