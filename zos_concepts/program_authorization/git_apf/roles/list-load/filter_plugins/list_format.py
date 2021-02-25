################################################################################
# © Copyright IBM Corporation 2020
# Apache License, Version 2.0 (see https://opensource.org/licenses/Apache-2.0)
################################################################################

from __future__ import absolute_import, division, print_function

__metaclass__ = type

def list_format(apf_list_diff_str):
    add_list = []
    apf_list_diff = apf_list_diff_str.split(',')
    for entry_str in apf_list_diff:
        ds = entry_str.split(':')[0].upper()
        vol = entry_str.split(':')[1].upper()
        entry = {}
        entry['library'] = ds
        if vol == "SMS":
            entry['sms'] = True
        elif vol != "NONE":
            entry['volume'] = vol
        add_list.append(entry)

    return add_list


class FilterModule(object):
    """ Fix the format for APF batch insertion """

    def filters(self):
        filters = {
            "list_format": list_format,
        }
        return filters
