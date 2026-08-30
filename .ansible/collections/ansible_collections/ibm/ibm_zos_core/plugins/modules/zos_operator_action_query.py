#!/usr/bin/python
# -*- coding: utf-8 -*-

# Copyright (c) IBM Corporation 2019, 2025
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
---
module: zos_operator_action_query
version_added: '1.1.0'
short_description: Display messages requiring action
description:
  - Get a list of outstanding messages requiring operator action given one or
    more conditions.
author:
  - "Ping Xiao (@xiaoping8385)"
  - "Demetrios Dimatos (@ddimatos)"
  - "Ivan Moreno (@rexemin)"
  - "Rich Parker (@richp405)"
  - "Fernando Flores (@fernandofloresg)"

options:
  system:
    description:
        - Return outstanding messages requiring operator action awaiting a
          reply for a particular system.
        - If the system name is not specified, all outstanding messages for
          that system and for the local systems attached to it are returned.
        - A trailing asterisk, (*) wildcard is supported.
    type: str
    required: false
  msg_id:
    description:
        - Return outstanding messages requiring operator action awaiting a
          reply for a particular message identifier.
        - If the message identifier is not specified, all outstanding messages
          for all message identifiers are returned.
        - A trailing asterisk, (*) wildcard is supported.
    type: str
    required: false
    aliases: [ message_id ]
  job_name:
    description:
      - Return outstanding messages requiring operator action awaiting a reply
        for a particular job name.
      - If the message job name is not specified, all outstanding messages
        for all job names are returned.
      - A trailing asterisk, (*) wildcard is supported.
    type: str
    required: false
  msg_filter:
    description:
      - Return outstanding messages requiring operator action awaiting a
        reply that match a regular expression (regex) filter.
      - If the message filter is not specified, all outstanding messages
        are returned regardless of their content.
    type: dict
    required: false
    aliases: [ message_filter ]
    suboptions:
      filter:
        description:
          - Specifies the substring or regex to match to the outstanding messages,
            see I(literal).
          - All special characters in a filter string that are not a regex are escaped.
          - Valid Python regular expressions are supported. See L(the official
            documentation,https://docs.python.org/library/re.html) for more information.
          - Regular expressions are compiled with the flag B(re.DOTALL) which
            makes the B('.') special character match any character including a
            newline."
        required: True
        type: str
      literal:
        description:
          - Indicates that the value for I(filter) is a regex or a string to match.
          - If False, the module creates a regex from the I(filter) string and
            matches it to the outstanding messages.
          - If True, the module assumes that I(filter) is not a regex and
            matches the I(filter) substring on the outstanding messages.
        required: False
        type: bool
        default: True
seealso:
- module: ibm.ibm_zos_core.zos_operator

attributes:
  action:
    support: none
    description: Indicates this has a corresponding action plugin so some parts of the options can be executed on the controller.
  async:
    support: full
    description: Supports being used with the ``async`` keyword.
  check_mode:
    support: none
    description: Can run in check_mode and return changed status prediction without modifying target. If not supported, the action will be skipped.
"""

EXAMPLES = r"""
- name: Display all outstanding messages issued on system MV2H
  zos_operator_action_query:
      system: mv2h

- name: Display all outstanding messages whose job name begin with im5
  zos_operator_action_query:
      job_name: im5*

- name: Display all outstanding messages whose message id begin with dsi*
  zos_operator_action_query:
      msg_id: dsi*

- name: Display all outstanding messages that have the text IMS READY in them
  zos_operator_action_query:
      msg_filter:
          filter: IMS READY

- name: Display all outstanding messages where the job name begins with 'mq',
        message ID begins with 'dsi', on system 'mv29' and which contain the
        pattern 'IMS'
  zos_operator_action_query:
      job_name: mq*
      msg_id: dsi*
      system: mv29
      msg_filter:
          filter: ^.*IMS.*$
          literal: true
"""

RETURN = r"""
changed:
    description:
        Indicates if any changes were made during module operation. Given
        operator action commands query for messages, True is always returned
        unless either a module or command failure has occurred.
    returned: always
    type: bool
    sample: false
count:
    description:
        The total number of outstanding messages.
    returned: always
    type: int
    sample: 12
actions:
    description:
        The list of the outstanding messages.
    returned: always
    type: list
    elements: dict
    contains:
        number:
            description:
                The message identification number.
            returned: on success
            type: int
            sample: 001
        type:
            description:
                The action type,'R' means request.
            returned: on success
            type: str
            sample: R
        system:
            description:
                System on which the outstanding message requiring operator
                action awaiting a reply.
            returned: on success
            type: str
            sample: MV27
        job_id:
            description:
                Job identifier for the outstanding message requiring operator
                action awaiting a reply.
            returned: on success
            type: str
            sample: STC01537
        msg_txt:
            description:
                Content of the outstanding message requiring operator
                action awaiting a reply. If I(msg_filter) is set,
                I(msg_txt) will be filtered accordingly.
            returned: success
            type: str
            sample: "*399 HWSC0000I *IMS CONNECT READY* IM5HCONN"
        job_name:
            description:
                Job name for outstanding message requiring operator action
                awaiting a reply.
            returned: success
            type: str
            sample: IM5HCONN
        msg_id:
            description:
                Message identifier for outstanding message requiring operator
                action awaiting a reply.
            returned: success
            type: str
            sample: HWSC0000I
    sample:
        [
            {
                "number": '001',
                "type": 'R',
                "system": 'MV27',
                "job_id": 'STC01537',
                "msg_txt": '*399 HWSC0000I *IMS CONNECT READY* IM5HCONN',
                "job_name": 'IM5HCONN',
                "msg_id": 'HWSC0000I'
            },
            {
                "number": '002',
                "type": 'R',
                "system": 'MV27',
                "job_id": 'STC01533',
                "msg_txt": '*400 DFS3139I IMS INITIALIZED, AUTOMATIC RESTART PROCEEDING IM5H',
                "job_name": 'IM5HCTRL',
                "msg_id": 'DFS3139I'
            }
        ]
"""

from ansible.module_utils.basic import AnsibleModule
import re
import traceback
from ansible_collections.ibm.ibm_zos_core.plugins.module_utils.better_arg_parser import (
    BetterArgParser,
)
from ansible_collections.ibm.ibm_zos_core.plugins.module_utils.import_handler import (
    ZOAUImportError,
)

from ansible_collections.ibm.ibm_zos_core.plugins.module_utils import (
    zoau_version_checker
)
from ansible_collections.ibm.ibm_zos_core.plugins.module_utils.dependency_checker import (
    validate_dependencies,
)
from ansible_collections.ibm.ibm_zos_core.plugins.module_utils.log import SingletonLogger

try:
    from zoautil_py import opercmd
except Exception:
    opercmd = ZOAUImportError(traceback.format_exc())


def run_module():
    """Initialize module.

    Raises
    ------
    fail_json
        A non-zero return code was received while querying the operator.
    fail_json
        An unexpected error occurred.
    """
    module_args = dict(
        system=dict(type="str", required=False),
        msg_id=dict(type="str", required=False, aliases=['message_id']),
        job_name=dict(type="str", required=False),
        msg_filter=dict(
            type="dict",
            required=False,
            aliases=['message_filter'],
            options=dict(
                filter=dict(type="str", required=True),
                literal=dict(default=True, type="bool", required=False)
            )
        )
    )

    result = dict(changed=False, count=0, actions=[])
    module = AnsibleModule(argument_spec=module_args, supports_check_mode=False)
    validate_dependencies(module)
    requests = []
    try:
        new_params = parse_params(module.params)

        # Initialize logging module
        module_verbosity_level = module._verbosity
        SingletonLogger().get_logger(module_verbosity_level)

        kwargs = {}

        wait_s = 5

        use_wait_arg = False
        if zoau_version_checker.is_zoau_version_higher_than("1.2.4"):
            use_wait_arg = True

        if use_wait_arg:
            kwargs.update({"wait": True})

        args = []

        cmdtxt = "d r,a,s"

        cmd_result_a = execute_command(cmdtxt, timeout_s=wait_s, *args, **kwargs)

        if cmd_result_a.rc > 0:
            module.fail_json(
                msg="A non-zero return code was received while querying the operator.",
                stdout=cmd_result_a.stdout,
                stderr=cmd_result_a.stderr,
                rc=cmd_result_a.rc,
                stdout_lines=cmd_result_a.stdout.splitlines() if cmd_result_a.stdout else None,
                stderr_lines=cmd_result_a.stderr.splitlines() if cmd_result_a.stderr else None,
                cmd="d r,a,s",
            )

        cmdtxt = "d r,a,jn"

        cmd_result_b = execute_command(cmdtxt, timeout_s=wait_s, *args, **kwargs)

        if cmd_result_b.rc > 0:
            module.fail_json(
                msg="A non-zero return code was received while querying the operator.",
                stdout=cmd_result_b.stdout,
                stderr=cmd_result_b.stderr,
                rc=cmd_result_b.rc,
                stdout_lines=cmd_result_b.stdout.splitlines() if cmd_result_b.stdout else None,
                stderr_lines=cmd_result_b.stderr.splitlines() if cmd_result_b.stderr else None,
                cmd="d r,a,jn",
            )

        merged_list = create_merge_list(cmd_result_a.message, cmd_result_b.message, new_params['msg_filter'])
        requests = find_required_request(merged_list, new_params)
        if requests:
            result["count"] = len(requests)
    except Error as e:
        module.fail_json(msg=repr(e), **result)
    except Exception as e:
        module.fail_json(
            msg="An unexpected error occurred: {0}".format(repr(e)), **result
        )

    result["actions"] = requests
    module.exit_json(**result)


def parse_params(params):
    """Parse parameters using BetterArgParser.

    Parameters
    ----------
    params : dict
        Parameters to parse.

    Returns
    -------
    dict
        Parsed parameters.
    """
    arg_defs = dict(
        system=dict(arg_type=system_type, required=False),
        msg_id=dict(arg_type=msg_id_type, required=False),
        job_name=dict(arg_type=job_name_type, required=False),
        msg_filter=dict(arg_type=msg_filter_type, required=False)
    )
    parser = BetterArgParser(arg_defs)
    new_params = parser.parse_args(params)
    return new_params


def system_type(arg_val, params):
    """System type.

    Parameters
    ----------
    arg_val : str
        Argument to validate.
    params : dict
        Not used, but obligatory for BetterArgParser.

    Returns
    -------
    str
        arg_val validated in uppercase.
    """
    regex = "^(?:[a-zA-Z0-9]{1,8})|(?:[a-zA-Z0-9]{0,7}[*])$"
    validate_parameters_based_on_regex(arg_val, regex)
    return arg_val.upper()


def msg_id_type(arg_val, params):
    """Message id type.

    Parameters
    ----------
    arg_val : str
        Argument to validate.
    params : dict
        Not used, but obligatory for BetterArgParser.

    Returns
    -------
    str
        arg_val validated in uppercase.
    """
    regex = "^(?:[a-zA-Z0-9]{1,})|(?:[a-zA-Z0-9]{0,}[*])$"
    validate_parameters_based_on_regex(arg_val, regex)
    return arg_val.upper()


def job_name_type(arg_val, params):
    """Job name type.

    Parameters
    ----------
    arg_val : str
        Argument to validate.
    params : dict
        Not used, but obligatory for BetterArgParser.

    Returns
    -------
    str
        arg_val validated in uppercase.
    """
    regex = "^(?:[a-zA-Z0-9]{1,8})|(?:[a-zA-Z0-9]{0,7}[*])$"
    validate_parameters_based_on_regex(arg_val, regex)
    return arg_val.upper()


def msg_filter_type(arg_val, params):
    """Message filter type.

    Parameters
    ----------
    arg_val : str
        Argument to validate.
    params : dict
        Not used, but obligatory for BetterArgParser.

    Returns
    -------
    str
        regex of the given argument.

    Raises
    ------
    ValidationError
        An error occurred during validate the input parameters.
    """
    try:
        filter_text = arg_val.get("filter")
        literal = arg_val.get("literal")

        if literal:
            raw_arg_val = r'^.*{0}.*$'.format(re.escape(filter_text))
        else:
            raw_arg_val = r'{0}'.format(filter_text)

        re.compile(raw_arg_val)
    except re.error:
        raise ValidationError(str(arg_val))

    return raw_arg_val


def validate_parameters_based_on_regex(value, regex):
    """Validate parameters based on regex.

    Parameters
    ----------
    value : str
        Argument to compare to regex pattern.
    regex : str
        Regex to get pattern from.

    Returns
    -------
    str
        The value given.

    Raises
    ------
    ValidationError
        An error occurred during validate the input parameters.
    """
    pattern = re.compile(regex)
    if pattern.fullmatch(value):
        pass
    else:
        raise ValidationError(str(value))
    return value


def find_required_request(merged_list, params):
    """Find the request given the options provided.

    Parameters
    ----------
    merged_list : list
        Merged list to search.
    params : dict
        Parameters to get for the function.

    Returns
    -------
    Union
        Filtered list.
    """
    requests = filter_requests(merged_list, params)
    return requests


def create_merge_list(msg_a, msg_b, msg_filter):
    """Merge the return lists that execute both 'd r,a,s' and 'd r,a,jn'.
    For example, if we have:
    'd r,a,s' response like: "742 R MV28     JOB57578 &742 ARC0055A REPLY 'GO' OR 'CANCEL'"
    'd r,a,jn' response like:"742 R FVFNT29H &742 ARC0055A REPLY 'GO' OR 'CANCEL'"
    the results will be merged so that a full list of information returned on condition.

    Parameters
    ----------
    msg_a : str
        Result coming from command 'd r,a,s'.
    msg_b : str
        Result coming from command 'd r,a,jn'.
    msg_filter : str
        Message filter.

    Returns
    -------
    Union
        Merge of the result of msg_a and the result of msg_b.
    """
    list_a = parse_result_a(msg_a, msg_filter)
    list_b = parse_result_b(msg_b, msg_filter)
    merged_list = merge_list(list_a, list_b)
    return merged_list


def filter_requests(merged_list, params):
    """Filter the request given the params provided.

    Parameters
    ----------
    merged_list : list
        Merged list to filter.
    params : dict
        Parameters to get for the function.

    Returns
    -------
    Union
        Filtered list.
    """
    system = params.get("system")
    msg_id = params.get("msg_id")
    job_name = params.get("job_name")
    newlist = merged_list
    if system:
        newlist = handle_conditions(newlist, "system", system)
    if job_name:
        newlist = handle_conditions(newlist, "job_name", job_name)
    if msg_id:
        newlist = handle_conditions(newlist, "msg_id", msg_id)
    return newlist


def handle_conditions(merged_list, condition_type, value):
    """Handle conditions.

    Parameters
    ----------
    merged_list : list[dict]
        List to check.
    condition_type : str
        Condition type to check.
    value
        Value to check for.

    Returns
    -------
    Union[dict]
        The new list.
    """
    # regex = re.compile(condition_values)
    newlist = []
    exist = False
    for message in merged_list:
        if message.get(condition_type) is not None:
            if value.endswith("*"):
                exist = message.get(condition_type).startswith(value.rstrip("*"))
            else:
                exist = message.get(condition_type) == value

        if exist:
            newlist.append(message)
    return newlist


def execute_command(operator_cmd, timeout_s=1, *args, **kwargs):
    """Execute operator command.

    Parameters
    ----------
    operator_cmd : str
        Operator command.
    timeout_s : int
        Timeout to wait for the command execution, measured in centiseconds.
    *args : dict
        Arguments for the command.
    **kwargs : dict
        More arguments for the command.

    Returns
    -------
    OperatorQueryResult
        The result of the command.
    """
    # as of ZOAU v1.3.0, timeout is measured in centiseconds, therefore:
    timeout_c = 100 * timeout_s
    response = opercmd.execute(operator_cmd, timeout_c, *args, **kwargs)

    rc = response.rc
    stdout = response.stdout_response
    stderr = response.stderr_response
    return OperatorQueryResult(rc, stdout, stderr)


def match_raw_message(msg, msg_filter):
    """Match raw message.

    Parameters
    ----------
    msg : str
        Message to match.
    msg_filter : str
        Filter for the message.

    Return
    ------
    bool
        If the pattern matches msg.
    """
    pattern = re.compile(msg_filter, re.DOTALL)
    return pattern.match(msg)


def parse_result_a(result, msg_filter):
    """parsing the result that coming from command 'd r,a,s',
    there are usually two formats:
     - line with job_id: 810 R MV2D     JOB58389 &810 ARC0055A REPLY 'GO' OR 'CANCEL'
     - line without job_id: 574 R MV28              *574 IXG312E OFFLOAD DELAYED FOR..
    also the request contains multiple lines, we need to handle that as well.

    Parameters
    ----------
    result : str
        Result coming from command 'd r,a,s'.
    msg_filter : str
        Message filter.

    Returns
    -------
    Union[dict[str,str]]
        Resulting list.
    """

    dict_temp = {}
    list = []

    match_iter = re.finditer(
        r"^\s*([0-9]{2,})\s([A-Z]{1})\s([A-Z0-9]{1,8})\s+((?:[A-Z0-9]{1,8})?)\s*[&*]?[0-9]+(.*)",
        result,
        re.MULTILINE,
    )
    for match in match_iter:
        # If there was a filter specified, we skip messages that do not match it.
        if msg_filter is not None and not match_raw_message(match.string, msg_filter):
            continue

        dict_temp = {
            "number": match.group(1),
            "type": match.group(2),
            "system": match.group(3),
        }
        if match.group(4) != "":
            dict_temp["job_id"] = match.group(4)
        if match.group(5) != "":
            dict_temp["msg_txt"] = match.group(5).strip()
        list.append(dict_temp)

    return list


def parse_result_b(result, msg_filter):
    """Parse the result that comes from command 'd r,a,jn', the main purpose
    to use this command is to get the job_name and message id, which is not
    included in 'd r,a,s'

    Parameters
    ----------
    result : str
        Result coming from command 'd r,a,jn'.
    msg_filter : str
        Message filter.

    Returns
    -------
    Union[dict[str,str]]
        Resulting list.
    """

    dict_temp = {}
    list = []

    match_iter = re.finditer(
        r"^\s*([0-9]{2,})\s[A-Z]{1}\s+([A-Z0-9]{1,8})?\s*[&*]?[0-9]+\s([A-Z0-9]+)",
        result,
        re.MULTILINE,
    )

    for match in match_iter:
        # If there was a filter specified, we skip messages that do not match it.
        if msg_filter is not None and not match_raw_message(match.string, msg_filter):
            continue

        dict_temp = {
            "number": match.group(1),
            "job_name": match.group(2),
            "msg_id": match.group(3),
        }

        # Sometimes 'job_name' will be null because the operator action is a
        # WTOR like in a dump command so remove keys with None types
        dict_temp_result = {
            k: v for k, v in dict_temp.items() if (v is not None)}
        list.append(dict_temp_result)

    return list


def merge_list(list_a, list_b):
    """Merge lists.

    Parameters
    ----------
    list_a : list
        First list to be merged.
    list_b : list
        Second list to be merged.

    Returns
    -------
    Union
        Merged of list_a and list_b.
    """
    merged_list = []
    for dict_a in list_a:
        for dict_b in list_b:
            if dict_a.get("number") == dict_b.get("number"):
                dict_z = dict_a.copy()
                dict_z.update(dict_b)
                merged_list.append(dict_z)
    return merged_list


class Error(Exception):
    pass


class ValidationError(Error):
    def __init__(self, message):
        """An error occurred during validate the input parameters.

        Parameters
        ----------
        message : str
            Message of the error that ocurred.

        Attributes
        ----------
        msg : str
            Human readable string describing the exception.
        """
        self.msg = (
            'An error occurred during validate the input parameters: "{0}"'.format(
                message
            )
        )


class OperatorQueryResult:
    def __init__(
        self,
        rc,
        stdout,
        stderr
    ):
        """Response object class to manage the result from executing a command
        to query for actionable messages. Class will also generate a message
        by concatenating stdout and stderr.

        Parameters
        ----------
        rc : str
            The return code.
        stdout : str
            The standard out of the command run.
        stderr : str
            The standard error of the command run.

        Attributes
        ----------
        rc : str
            The return code.
        stdout : str
            The standard out of the command run.
        stderr : str
            The standard error of the command run.
        message : str
            The standard out of the command run.
        """
        self.rc = rc
        self.stdout = stdout
        self.stderr = stderr
        self.message = stdout


def main():
    run_module()


if __name__ == "__main__":
    main()
