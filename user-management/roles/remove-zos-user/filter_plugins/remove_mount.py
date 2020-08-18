# Copyright (c) IBM Corporation 2020
# Apache License, Version 2.0 (see https://opensource.org/licenses/Apache-2.0)

from __future__ import absolute_import, division, print_function

__metaclass__ = type

from re import match, MULTILINE


def remove_mount(content, omvs_zfs_data_set_name, omvs_home_directory):
    """Remove a mount command from BPXPRMxx member.

    Returns:
         str: The updated BPXPRMxx member's contents.
    """
    content_lines = content.split('\n')
    remove_starting_at_index = None
    for index, line in enumerate(content_lines):
        if match(r"^\s*MOUNT\s+FILESYSTEM\(" + omvs_zfs_data_set_name.upper() + r"\)", line, MULTILINE):
            remove_starting_at_index = index
            break
    if remove_starting_at_index is not None:
        del content_lines[remove_starting_at_index: remove_starting_at_index+3]
    return "\n".join(content_lines)


class FilterModule(object):
    """ Jinja2 filter for removing a mount from BPXPRMxx member. """

    def filters(self):
        filters = {
            "remove_mount": remove_mount,
        }
        return filters
