from __future__ import absolute_import, division, print_function
__metaclass__ = type

from ansible.module_utils.six import PY3, iteritems, string_types
from ansible.module_utils._text import to_bytes, to_text

# since some variables are inserted
# into our templates dynamically, we can't
# hardcode the end of line character in position 71
# this modifies the string to fix the spacing


def fix_line_spacing(args):
    strs_to_format = args[0]  # pieces of final string, may not all be strings
    max_line_length = args[1]
    start_text_at = args[2]
    end_char = args[3]
    formatted_str = ''
    formatted_str += ' ' * (start_text_at - 1)

    str_to_format_length = 0
    if isinstance(strs_to_format, (list,)):
        for string in strs_to_format:
            formatted_str += str(string)
            str_to_format_length += len(string)
    else:
        formatted_str += str(strs_to_format)
        str_to_format_length = len(strs_to_format)

    remaining_length = (max_line_length + 1) - start_text_at - str_to_format_length
    formatted_str += ' ' * remaining_length
    formatted_str += end_char
    return formatted_str


class FilterModule(object):
    ''' Ansible core jinja2 filters '''

    def filters(self):
        filters = {
            'fix_line_spacing': fix_line_spacing,
        }
        return filters
