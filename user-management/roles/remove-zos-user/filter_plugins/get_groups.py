# Copyright (c) IBM Corporation 2020
# Apache License, Version 2.0 (see https://opensource.org/licenses/Apache-2.0)

from __future__ import absolute_import, division, print_function

__metaclass__ = type
import re

def user_groups(listuser_output, include_default_group=False):
    """Parse user groups from LISTUSER output.

    Args:
        listuser_output (Union[list, str]): The output of the TSO LISTUSER command.

    Returns:
        list[str]: The groups parsed from LISTUSER output.
    """
    if isinstance(listuser_output, list):
        listuser_output = "\n".join(listuser_output)
    default_group = ""
    default_group_match = re.search(r"^\s*DEFAULT-GROUP=(.*)\s+PASSDATE=", listuser_output, re.MULTILINE)
    if default_group_match:
        default_group = default_group_match.group(1).strip()
    matches = re.finditer(r"^\s*GROUP=(.*)\s+AUTH=", listuser_output, re.MULTILINE)

    groups = [match.group(1).strip() for match in matches if match.group(1).strip() != default_group]
    if include_default_group:
        groups.append(default_group)

    return groups



class FilterModule(object):
    """ Jinja2 filters for capturing user information from LISTUSER. """

    def filters(self):
        filters = {
            "user_groups": user_groups,
        }
        return filters
