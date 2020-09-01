# Copyright (c) IBM Corporation 2020
# Apache License, Version 2.0 (see https://opensource.org/licenses/Apache-2.0)

from __future__ import absolute_import, division, print_function

__metaclass__ = type

from re import match, MULTILINE


def insert_mount(content, omvs_zfs_data_set_name, omvs_home_directory):
    """Insert a mount command into BPXPRMxx member.

    Returns:
         str: The updated BPXPRMxx member's contents.
    """
    content_lines = content.split('\n')
    mount_command = " MOUNT FILESYSTEM('{0}')\n   MOUNTPOINT('{1}')\n   TYPE(ZFS) MODE(RDWR)".format(
        omvs_zfs_data_set_name.upper(), omvs_home_directory)
    insert_at_index = None
    for index, line in enumerate(content_lines):
        if match(r"^\s*MOUNT\s+FILESYSTEM", line, MULTILINE):
            insert_at_index = index
            break
    if insert_at_index is None:
        content_lines.append(mount_command)
    else:
        content_lines.insert(insert_at_index, mount_command)
    return "\n".join(content_lines)


class FilterModule(object):
    """ Jinja2 filter for inserting a mount into BPXPRMxx member. """

    def filters(self):
        filters = {
            "insert_mount": insert_mount,
        }
        return filters
