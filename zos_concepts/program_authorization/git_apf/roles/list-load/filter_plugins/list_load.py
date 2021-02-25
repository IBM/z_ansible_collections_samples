################################################################################
# © Copyright IBM Corporation 2020
# Apache License, Version 2.0 (see https://opensource.org/licenses/Apache-2.0)
################################################################################

from __future__ import absolute_import, division, print_function
import json

__metaclass__ = type

def list_load(content):
    git_list_str = ""
    lines = content.split('\n')
    for line in lines:
        if line:
            line_list = line.split()
            git_list_str += line_list[0] + ":"
            if len(line_list) > 1:
                git_list_str += line_list[1] + ","
            else:
                git_list_str += "none,"

    return git_list_str[:-1]


class FilterModule(object):
    """ Load the list from the file in the git repo """

    def filters(self):
        filters = {
            "list_load": list_load,
        }
        return filters
