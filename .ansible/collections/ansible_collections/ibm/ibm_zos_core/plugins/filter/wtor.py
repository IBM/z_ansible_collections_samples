# Copyright (c) IBM Corporation 2020, 2024
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#     http://www.apache.org/licenses/LICENSE-2.0
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.

from __future__ import absolute_import, division, print_function

__metaclass__ = type

DOCUMENTATION = r"""
name: filter_wtor_messages
author: Demetrios Dimatos (@ddimatos)
version_added: "1.2.0"
short_description: Filter a list of WTOR messages
description:
    - Filter a list of WTOR (write to operator with reply) messages found by
      module zos_operator_action_query.
    - Filter using a string or regular expression.
options:
    wtor_response:
      description:
            - A list containing response property `message_text`, provided the
              module zos_operator_action_query.
            - The list can be the outstanding messages found in the  modules
              response under the `actions` property or the entire module
              response.
      type: list
      required: true
    text:
      description:
            - String of text to match or a regular expression to use as filter criteria.
      type: str
      required: true
    ingore_case:
      description:
            - Should the filter enable case sensitivity when performing a match.
      type: bool
      required: false
      default: false
"""

EXAMPLES = r"""
- name: Filter actionable messages that match 'IEE094D SPECIFY OPERAND' and if so, set is_specify_operand = true.
  set_fact:
    is_specify_operand: "{{ result | ibm.ibm_zos_core.filter_wtor_messages('IEE094D SPECIFY OPERAND') }}"
  when: result is defined and not result.failed

- name: Evaluate if there are any existing dump messages matching 'IEE094D SPECIFY OPERAND'
  assert:
    that:
      - is_specify_operand is defined
      - bool_zos_operator_action_continue
    success_msg: "Found 'IEE094D SPECIFY OPERAND' message."
    fail_msg: "Did not find 'IEE094D SPECIFY OPERAND' message."
"""

RETURN = r"""
  _value:
    description: A list containing dictionaries matching the WTOR.
    type: list
    elements: dict
"""

import re


def filter_wtor_messages(wtor_response, text, ingore_case=False):
    """Filter a list of WTOR messages based on message text.

    Arguments:
        wtor_response {Union[dict, list[dict]]} -- The list structure in "actions" list returned by
        zos_operator_action_query or the entire return object from zos_operator_action_query.
        text {str} -- String of text or regular expression to use as filter criteria.

    Keyword Arguments:
        ingore_case {bool} -- Should search be case insensitive (default: {False})

    Returns:
        list[dict] -- A list containing any WTOR objects matching search criteria
    """
    wtors = []
    if isinstance(wtor_response, dict):
        wtors = wtor_response.get("actions")
    elif isinstance(wtor_response, list):
        wtors = wtor_response
    found = []
    for wtor in wtors:
        result = None
        if ingore_case:
            result = re.search(text, wtor.get("message_text", ""), re.IGNORECASE)
        else:
            result = re.search(text, wtor.get("message_text", ""))
        if result:
            found.append(wtor)
    return found


class FilterModule(object):
    """ Jinja2 filters for use with WTOR response objects returned by zos_operator_action_query module. """

    def filters(self):
        filters = {
            "filter_wtor_messages": filter_wtor_messages,
        }
        return filters
