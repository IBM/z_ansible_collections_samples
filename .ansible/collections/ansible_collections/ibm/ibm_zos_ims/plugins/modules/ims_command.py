#!/usr/bin/python
# -*- coding: utf-8 -*-
# Copyright (c) IBM Corporation 2019, 2020
# Apache License, Version 2.0 (see https://opensource.org/licenses/Apache-2.0)

from __future__ import (absolute_import, division, print_function)

__metaclass__ = type


DOCUMENTATION = r'''
---

module: ims_command

short_description: Submit IMS Commands
version_added: "2.9"

description:
  - Submit Type 1 and Type 2 IMS Commands. User specifies a well formatted
    IMS Command string along with PLEX and (optional) ROUTE information.
  - IMS will return a completion code, return code, and reason code along
    with any relevant text indicating the status of the command that was run.
author:
  - Ketan Kelkar (@ketankelkar)
  - Jerry Li (@th365thli)
  - Omar Elbarmawi (@oelbarmawi)
options:
  batch:
    description:
      - submit multiple IMS commands with a single invocation of the module.
    required: false
    type: list
    elements: dict
    suboptions:
      command:
        description:
          - This is the (well-formatted) command to submit to IMS Batch.
        type: str
        required: true
      plex:
        description:
          - Specify the IMSPLEX in which the IMS Command will be submitted.
        type: str
        required: true
      route:
        description:
          - Specify the IMS System in which the IMS Command will be submitted.
          - Leaving this field empty will result in invoking all available routes within the specified PLEX.
        type: list
        required: false
  command:
    description:
      - This is the (well-formatted) command to submit to IMS Batch.
    type: str
    required: true
  plex:
    description:
      - Specify the IMSPLEX in which the IMS Command will be submitted.
    type: str
    required: true
  route:
    description:
      - Specify the IMS System in which the IMS Command will be submitted.
      - Leaving this field empty will result in invoking all available routes within the specified PLEX.
    type: list
    required: false
notes:
  - This module requires Structured Call Interface (SCI) and Operations Manager (OM) to be active in the target IMSplex.
'''

EXAMPLES = '''
- name: Query all programs for IMS1 in PLEX1
  ims_command:
    command: QUERY PGM SHOW(ALL)
    plex: PLEX1
    route: IMS1

- name: Query all programs for IMS1 and IMS2 in PLEX1
  ims_command:
    command: QUERY PGM SHOW(ALL)
    plex: PLEX1
    route: ['IMS1', 'IMS2']

- name: Query all transactions for all routes in PLEX1
  ims_command:
    command: QUERY TRAN SHOW(ALL)
    plex: PLEX1

- name: Stop all transactions for IMS2 in PLEX1
  ims_command:
    command: UPDATE TRAN STOP(Q)
    plex: PLEX1
    route: IMS2

- name: Create a DB called IMSDB1 for IMS3 in PLEX2
  ims_command:
    command: CREATE DB NAME(IMSDB1)
    plex: PLEX2
    route: IMS3

- name: Batch call - query all pgms, create pgm, and query for new
  ims_command:
    batch:
      -
        command: QUERY PGM SHOW(ALL)
        plex: PLEX1
        route: IMS1
      -
        command: CREATE PGM NAME(EXAMPLE1)
        plex: PLEX1
        route: IMS1
      -
        command: QUERY PGM SHOW(ALL)
        plex: PLEX1
        route: IMS1
'''

RETURN = '''
failed:
  description:
    Indicates the outcome of the module.
  type: bool
  returned: always
ims_output:
  description:
    The output provided by the specified IMS Command.
    All the IMS return, reason, and completion codes from running the commands along with associated text.
  type: list
  elements: dict
  returned: sometimes
  contains:
    ims_member_data:
      description:
        Output from Type 1 commands.
      returned: sometimes
      type: dict
    ims_member_messages:
      description:
        Messages from the IMS instance in which the command was routed.
      returned: sometimes
      type: dict
    return_codes:
      description:
        Return codes indicating the general result of running the IMS command.
      returned: always
      type: dict
      contains:
        imsrc:
          description:
            General IMS return code.
          type: str
        reason:
          description:
            Return code indicating specific status of the command.
          type: str
        results:
          description:
            Return code indicating the results of the command.
          type: str
    subgroup_info:
      description:
        Returns output from the OM instance in which the command was routed.
      type: dict
      returned: always
      contains:
        ctl.rc:
          description:
            Return code (i.e. 0000000).
          type: str
        ctl.rsn:
          description:
            CTL reason code.
          type: str
    type_2_data:
      description:
        Data resulting from the output of the IMS command submitted.
      returned: sometimes
      type: dict
      contains:
        CC:
          description:
            Completion code for the line of output. Completion code is always returned.
          type: str
        CCText:
          description:
            Completion code text that describes the meaning of the nonzero completion code.
          type: str
'''

import json
import re
from ansible.module_utils.basic import AnsibleModule
from os import chmod, path, remove
from tempfile import NamedTemporaryFile
from ansible_collections.ibm.ibm_zos_ims.plugins.module_utils.ims_command_utils import REXX_TEMPLATE  # pylint: disable=import-error
from ansible_collections.ibm.ibm_zos_ims.plugins.module_utils.ims_module_error_messages import ErrorMessages as em  # pylint: disable=import-error
# from traceback import format_exc


def format_ims_command(raw_command):
    """Cleans and verifies the user entered an IMS Command.

    Arguments:
        raw_command {str} -- Raw user input for `command` parameter.

    Returns:
        {bool} -- Flag for valid command.
        {str} -- Error message, or None for valid command.
        {str} -- Verified IMS Command.
    """
    is_valid = True
    if not raw_command:
        is_valid = False
        error_msg = em.MISSING_COMMAND
        return is_valid, error_msg, None

    command = raw_command.strip()
    command = command.replace('\"', '\"\"')
    return is_valid, None, command


def format_plex(raw_plex):
    """Cleans and verifies the user input for Plex is valid.

    Arguments:
        raw_plex {str} -- User defined plex

    Returns:
        {bool} -- Flag for valid plex.
        {str} -- Error message, or None for valid plex.
        {str} -- Formatted plex.
    """
    if not raw_plex:
        is_valid = False
        error_msg = em.MISSING_PLEX
        return is_valid, error_msg, None

    plex = raw_plex.strip()
    if not plex.isalnum():
        is_valid = False
        error_msg = em.INVALID_PLEX_MSG
        return is_valid, error_msg, None
    is_valid = True
    return is_valid, None, plex


def format_route(raw_route):
    """Cleans and formats route list into valid REXX input.

        Arguments:
        route {list} -- user input for route

    Returns:
        {bool} -- Flag indicating valid route.
        {str} -- Error message.
        {list} -- List of validated routes.
    """
    raw_route_list = raw_route
    if isinstance(raw_route, str):
        raw_route_list = list(raw_route)
    if raw_route_list:
        delimiter = ","
        route_list = []
        for r in raw_route_list:
            if r.isalnum():
                route_list.append(r.strip())
            else:
                is_valid = False
                error_msg = em.INVALID_ROUTE_MSG
                return is_valid, error_msg, None
        is_valid = True
        return is_valid, None, delimiter.join(route_list)
    return True, None, None  # style?


def verify_return_code(output):
    """Verifies the output returned from the REXX script does not contain a non-zero
    return code.

    Arguments:
        output {json} -- JSON output received from REXX script

    Returns:
        {bool} -- Flag indicating return code is 0.
        {str} -- Error message.
    """
    try:
        rc = output['command_return']['ctl.rc']
        pattern = r"0+$"
        match = re.match(pattern, rc)
        if not match:
            msg = em.NON_ZERO_RC_MSG
            return False, msg
    except Exception:
        msg = em.NO_RC_MSG
        return False, msg
    return True, None


def submit_rexx(ims_command, plex, route, module):
    """This function will use the user input to submit a REXX script
    using the REXX template.

    Arguments:
        ims_command {str} -- User defined IMS Command
        plex {str} -- User defined Plex
        route {str} -- Concatenated list of routes defined by user
    Returns:
        {str} -- Return code
        {str} -- Output
        {str} -- Error
    """
    try:
        command = ims_command
        if route:
            command += "~{0}".format(route)
        rexx_script = REXX_TEMPLATE % (command, plex)
        dir_name, script_name = _copy_temp_file(rexx_script)
        cmd = path.join(dir_name, script_name)
        rc, out, err = module.run_command(args=cmd, cwd=dir_name, use_unsafe_shell=True)
    except Exception:
        err = em.SUBMISSION_ERROR_MSG
        return None, None, err  # style?
    finally:
        remove(path.join(dir_name, script_name))
    return rc, out, err


def _copy_temp_file(content):
    """This function creates a temporary file and sets the permissions of that file.

    Arguments:
        content {str} -- The contents of the file to be created.

    Returns:
        {str} -- Name of the directory in which the file is located.
        {str} -- Name of the file itself.
    """
    delete_on_close = False
    try:
        tmp_file = NamedTemporaryFile(delete=delete_on_close)
        with open(tmp_file.name, 'w') as f:
            f.write(content)
        f.close()
        chmod(tmp_file.name, 0o744)
        dir_name = path.dirname(tmp_file.name)
        script_name = path.basename(tmp_file.name)
    except Exception:
        remove(tmp_file)
        raise
    return dir_name, script_name


def scan_for_rexx_error(rexx_output):
    """Scans for a REXX error in the rexx_output

    Arguments:
        rexx_output {str} -- Output received from REXX script.
    Returns:
        {str} -- Error message.
    """
    if rexx_output:
        pattern = r"(\+{3}\sRC\([0-9\-]*\)\s\+{3})"
        match = re.search(pattern, rexx_output, flags=re.IGNORECASE)
        if match:
            return em.REXX_RETURN_CODE_MSG + match.group(1)
        elif "invalid character" in rexx_output.lower():
            return em.INVALID_CHAR_IN_CMD


def execute_ims_command(command, plex, route, module):
    """Performs ims command
    Arguments:
        command {str} -- The well-formatted ims command to run.
        plex {str} -- The well-formatted plex to run command on.
        route {str} -- The well-formatted route to run command on.

    Returns:
        {bool} -- Flag indicating succesful execution.
        {str} -- JSON string output of resulting from command
        {str} -- Error message.
    """
    result = {}

    is_valid, err, command = format_ims_command(command)
    if not is_valid:
        result['msg'] = err
        return False, result
    is_valid, err, plex = format_plex(plex)
    if not is_valid:
        result['msg'] = err
        return False, result
    is_valid, err, route = format_route(route)
    if not is_valid:
        result['msg'] = err
        return False, result

    rc, out, err = submit_rexx(command, plex, route, module)
    try:
        json_output = json.loads(out, strict=False)
    except ValueError:
        result['msg'] = em.JSON_DECODE_ERROR_MSG
        rexx_error = scan_for_rexx_error(out)
        if rexx_error:
            result['err'] = rexx_error
        else:
            result['err'] = err
        return False, result

    is_valid, err = verify_return_code(json_output)
    if not is_valid:
        json_output['msg'] = err
        json_output['err'] = em.NON_ZERO_ERR_MSG
        return False, json_output
    if rc is None:
        json_output['msg'] = err
        return False, json_output
    elif rc != 0:
        json_output['msg'] = em.NON_ZERO_RC_MSG
        return False, json_output
    if not out:
        json_output['msg'] = em.NO_OUTPUT_MSG
        json_output['err'] = err
        return False, json_output

    return True, json_output


def run_module():
    module_args = dict(
        command=dict(type='str', required=False),
        plex=dict(type='str', required=False),
        route=dict(type='list', required=False),

        batch=dict(
            type='list',
            required=False,
            elements='dict',
            options=dict(
                command=dict(type='str', required=True),
                plex=dict(type='str', required=True),
                route=dict(type='list', required=False)
            )
        )
    )

    result = dict(
        changed=False,
        msg='',
        ims_output=[]
    )

    module = AnsibleModule(
        argument_spec=module_args,
        supports_check_mode=True
    )

    # if the user is working with this module in only check mode we do not
    # want to make any changes to the environment, just return the current
    # state with no modifications
    if module.check_mode:
        module.exit_json(**result)

    failure_occured = False
    batch_input = module.params['batch']
    if batch_input:
        for batch_result in batch_input:
            command = batch_result['command']
            plex = batch_result['plex']
            route = batch_result['route']
            status, command_result_dict = execute_ims_command(command, plex, route, module)
            if not status:
                # If the command failed, set variable to true and continue batch commands
                failure_occured = True
            else:
                # If the command executed properly, set changed field to True
                result['changed'] = True
            result['ims_output'].append(command_result_dict)

    else:
        command = module.params['command']
        plex = module.params['plex']
        route = module.params['route']
        status, command_result_dict = execute_ims_command(command, plex, route, module)
        result['ims_output'].append(command_result_dict)
        if not status:
            module.fail_json(**result)
        else:
            result['changed'] = True

    if failure_occured:
        result['msg'] = em.BATCH_FAILURE_MSG
        module.fail_json(**result)

    result['msg'] = em.SUCCESS_MSG
    module.exit_json(**result)


def main():
    run_module()


if __name__ == '__main__':
    main()
