# Apache License, Version 2.0 (see https://opensource.org/licenses/Apache-2.0)

from __future__ import absolute_import, division, print_function

__metaclass__ = type

def valid_list(input_list):
    return input_list and type(input_list) == list and len(input_list) != 0


class FilterModule(object):
    """ Filter for determining if a list is a valid non-empty list """

    def filters(self):
        filters = {
            "valid_list": valid_list,
        }
        return filters