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
module: zos_tso_command
version_added: '1.1.0'
short_description: Execute TSO commands
description:
    - Execute TSO commands on the target z/OS system with the provided options and receive a structured response.
author:
    - "Xiao Yuan Ma (@bjmaxy)"
    - "Rich Parker (@richp405)"
    - "Demetrios Dimatos (@ddimatos)"
options:
  commands:
    description:
        - One or more TSO commands to execute on the target z/OS system.
        - Accepts a single string or list of strings as input.
        - If a list of strings is provided, processing will stop at the first failure, based on rc.
    required: true
    type: raw
    aliases:
        - command
  max_rc:
    description:
        - Specifies the maximum return code allowed for a TSO command.
        - If more than one TSO command is submitted, the I(max_rc) applies to all TSO commands.
    default: 0
    required: false
    type: int

attributes:
  action:
    support: none
    description: Indicates this has a corresponding action plugin so some parts of the options can be executed on the controller.
  async:
    support: full
    description: Supports being used with the ``async`` keyword.
  check_mode:
    support: full
    description: Can run in check_mode and return changed status prediction without modifying target. If not supported, the action will be skipped.
"""

RETURN = r"""
output:
    description:
        List of each TSO command output.
    returned: always
    type: list
    elements: dict
    contains:
        command:
            description:
                The executed TSO command.
            returned: always
            type: str
            sample: "LU TESTUSER"
        rc:
            description:
                The return code from the executed TSO command.
            returned: always
            type: int
            sample: 0
        stdout:
            description:
                The standard output from the TSO command execution.
            returned: always
            type: str
            sample: "NO MODEL DATA SET                                                OMVSADM"
        stdout_lines:
            description:
                The standard output split into individual lines.
            returned: always
            type: list
            elements: str
            sample:
                - "NO MODEL DATA SET                                                OMVSADM"
                - "TERMUACC                                                                "
                - "SUBGROUP(S)= VSAMDSET SYSCTLG  BATCH    SASS     MASS     IMSGRP1       "
                - "             IMSGRP2  IMSGRP3  DSNCAT   DSN120   J42      M63           "
                - "             J91      J09      J97      J93      M82      D67           "
                - "             D52      M12      CCG      D17      M32      IMSVS         "
                - "             DSN210   DSN130   RAD      CATLG4   VCAT     CSP           "
        line_count:
            description:
                The number of lines in the standard output.
            returned: always
            type: int
            sample: 7
        stderr:
            description:
                The standard error from the TSO command execution.
            returned: always
            type: str
            sample: ""
        stderr_lines:
            description:
                The standard error split into individual lines.
            returned: always
            type: list
            elements: str
            sample: [""]
        failed:
            description:
                Whether the command failed based on the return code exceeding max_rc.
            returned: always
            type: bool
            sample: false
max_rc:
    description:
        - The maximum return code that was allowed for the TSO commands.
        - If more than one TSO command is submitted, the I(max_rc) applies to all TSO commands.
    returned: always
    type: int
    sample: 0
"""

EXAMPLES = r"""
- name: Execute TSO commands to allocate a new dataset.
  zos_tso_command:
    commands:
      - alloc da('TEST.HILL3.TEST') like('TEST.HILL3')
      - delete 'TEST.HILL3.TEST'

- name: Execute TSO command List User (LU) for TESTUSER to obtain TSO information.
  zos_tso_command:
    commands:
      - LU TESTUSER

- name: Execute TSO command List Dataset (LISTDSD) and allow for maximum return code of 4.
  zos_tso_command:
    commands:
      - LISTDSD DATASET('HLQ.DATA.SET') ALL GENERIC
    max_rc: 4

- name: Execute TSO command to run a REXX script explicitly from a data set.
  zos_tso_command:
    commands:
      - EXEC HLQ.DATASET.REXX exec

- name: Chain multiple TSO commands into one invocation using semicolons.
  zos_tso_command:
    commands: >-
      ALLOCATE DDNAME(IN1) DSNAME('HLQ.PDSE.DATA.SRC(INPUT)') SHR;
      ALLOCATE DDNAME(OUT1) DSNAME('HLQ.PDSE.DATA.DEST(OUTPUT)') SHR;
      OCOPY INDD(IN1) OUTDD(OUT1) BINARY;

- name: Recall a migrated data set.
  zos_tso_command:
    commands:
      - HRECALL 'MY.DATASET' WAIT
"""

from ansible.module_utils.basic import AnsibleModule
from ansible_collections.ibm.ibm_zos_core.plugins.module_utils import data_set
from os import chmod
import re
from tempfile import NamedTemporaryFile
from stat import S_IEXEC, S_IREAD, S_IWRITE
from ansible_collections.ibm.ibm_zos_core.plugins.module_utils.better_arg_parser import (
    BetterArgParser,
)
from ansible_collections.ibm.ibm_zos_core.plugins.module_utils.dependency_checker import (
    validate_dependencies,
)
from ansible_collections.ibm.ibm_zos_core.plugins.module_utils.log import SingletonLogger


def run_tso_command(commands, module, max_rc):
    """Run tso command.

    Parameters
    ----------
    commands : str
        Commands to run.
    module : AnsibleModule
        Ansible module to run the command with.
    max_rc : int
        Max return code.

    Returns
    -------
    Union[dict]
        The command result details.

    """
    script = """/* REXX */
PARSE ARG cmd
address tso
x = outtrap('listcato.', '*')
cmd
rc = RC
do j = 1 to listcato.0
    say listcato.j
end
x = outtrap('OFF')
exit rc
"""
    command_detail_json = copy_rexx_and_run_commands(script, commands, module, max_rc)
    return command_detail_json


def copy_rexx_and_run_commands(script, commands, module, max_rc):
    """Copy rexx into a temporary file and run commands.

    Parameters
    ----------
    script : str
        Script to run the command.
    commands : str
        Commands to run.
    module : AnsibleModule
        Ansible module to run the command with.
    max_rc : int
        Max return code.

    Returns
    -------
    Union[dict]
        The command result details.
    """
    command_detail_json = []
    tmp_file = NamedTemporaryFile(delete=True)
    with open(tmp_file.name, "w") as f:
        f.write(script)
    chmod(tmp_file.name, S_IEXEC | S_IREAD | S_IWRITE)
    for command in commands:
        rc, stdout, stderr = module.run_command([tmp_file.name, command], errors='replace')
        command_results = {}
        command_results["command"] = command
        command_results["rc"] = rc
        command_results["stdout"] = stdout
        command_results["stdout_lines"] = stdout.split("\n")
        command_results["line_count"] = len(command_results.get("stdout_lines", []))
        command_results["stderr"] = stderr
        command_results["stderr_lines"] = stderr.split("\n")

        if rc <= max_rc:
            command_results["failed"] = False
        else:
            command_results["failed"] = True

        command_detail_json.append(command_results)
        if command_results["failed"]:
            break

    return command_detail_json


def list_or_str_type(contents, dependencies):
    """Checks if a variable contains a string or a list of strings and returns it as a list of strings.

    Parameters
    ----------
    contents : str | list[str]
        String or list of strings.
    dependencies
        Unused.

    Returns
    -------
    str | Union[str]
        The parameter given as a list of strings.

    Raises
    ------
    ValueError
        Invalid argument type. Expected "string or list of strings".
    """
    failed = False
    if isinstance(contents, list):
        for item in contents:
            if not isinstance(item, str):
                failed = True
                break
    elif isinstance(contents, str):
        contents = [contents]
    else:
        failed = True
    if failed:
        raise ValueError(
            'Invalid argument type for "{0}". expected "string or list of strings"'.format(
                contents
            )
        )
    return contents


def preprocess_data_set_names(command):
    """
    Applies necessary preprocessing to the data set names, such as converting
    a GDS relative name into an absolute one.

    Parameters
    ----------
    command : str
        command in which to look for a data set name.

    Returns
    -------
    str
        The command with the modified data set names if any.

    """
    pattern = r"(?:(?:[A-Z$#@]{1}[A-Z0-9$#@-]{0,7})(?:[.]{1})){1,21}[A-Z$#@]{1}[A-Z0-9$#@-]{0,7}(?:\([A-Z$#@]{1}[A-Z0-9$#@]{0,7}\)|\((?:[-+]?[0-9]+)\)){0,1}"
    data_set_list = re.findall(pattern, command)
    for name in data_set_list:
        if data_set.DataSet.is_gds_relative_name(name):
            dataset_name = data_set.DataSet.resolve_gds_absolute_name(name)
            command = command.replace(name, dataset_name)
    return command


def run_module():
    """Initialize module.

    Raises
    ------
    fail_json
        ValueError on BetterArgParser.
    fail_json
        Some command(s) failed.
    fail_json
        An unexpected error occurred.
    """
    module_args = dict(
        commands=dict(type="raw", required=True, aliases=["command"]),
        max_rc=dict(type="int", required=False, default=0),
    )

    module = AnsibleModule(argument_spec=module_args, supports_check_mode=True)
    validate_dependencies(module)
    result = dict(
        changed=False,
        failed=True,
    )

    arg_defs = dict(
        commands=dict(type=list_or_str_type, required=True, aliases=["command"]),
        max_rc=dict(type="int", required=False, default=0),
    )
    try:
        parser = BetterArgParser(arg_defs)
        parsed_args = parser.parse_args(module.params)
    except ValueError as e:
        module.fail_json(msg=repr(e), **result)

    # Initialize logging module
    module_verbosity_level = module._verbosity
    SingletonLogger().get_logger(module_verbosity_level)

    commands = parsed_args.get("commands")
    commands = list(map(preprocess_data_set_names, commands))
    max_rc = parsed_args.get("max_rc")
    if max_rc is None:
        max_rc = 0

    try:
        result["output"] = run_tso_command(commands, module, max_rc)
        result["max_rc"] = max_rc
        errors_found = False
        result_list = []

        for cmd in result.get("output"):
            tmp_string = 'Command "' + cmd.get("command", "") + '" execution'
            if cmd.get("rc") > max_rc:
                errors_found = True
                if max_rc > 0:
                    result_list.append(tmp_string + "failed.  RC was {0}; max_rc was {1}".format(cmd.get("rc"), max_rc))
                else:
                    result_list.append(tmp_string + "failed.  RC was {0}.".format(cmd.get("rc")))
            else:
                result_list.append(tmp_string + "succeeded.  RC was {0}.".format(cmd.get("rc")))

        if errors_found:
            result_string = "\n".join(result_list)

            module.fail_json(
                msg="Some ({0}) command(s) failed:\n{1}".format(errors_found, result_string),
                **result
            )

        result["changed"] = True
        result["failed"] = False
        module.exit_json(**result)

    except Exception as e:
        module.fail_json(
            msg="An unexpected error occurred: {0}".format(repr(e)), **result
        )


def main():
    run_module()


if __name__ == "__main__":
    main()
