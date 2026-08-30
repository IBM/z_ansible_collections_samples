#!/usr/bin/python
# -*- coding: utf-8 -*-

# Copyright (c) IBM Corporation 2019, 2026
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
module: zos_operator
version_added: '1.1.0'
short_description: Execute operator command
description:
    - Execute an operator command and receive the output.
author:
  - "Ping Xiao (@xiaopingBJ)"
  - "Demetrios Dimatos (@ddimatos)"
  - "Rich Parker (@richp405)"
  - "Oscar Fernando Flores (@fernandofloresg)"
  - "Ivan Moreno (@rexemin)"
options:
  cmd:
    description:
      - The command to execute.
      - If the command contains single-quotations, another set of single quotes must be added.
      - For example, change the command "...,P='DSN3EPX,-DBC1,S'" to "...,P=''DSN3EPX,-DBC1,S'' ".
      - If the command contains any special characters ($, &, etc), they must be escaped using
        double backslashes like \\\\\\$.
      - For example, to display job by job name the command would be C(cmd:"\\$dj''HELLO''")
      - By default, the command will be converted to uppercase before execution, to control this
        behavior, see the I(case_sensitive) option below.
    type: str
    required: true
  verbose:
    description:
      - Return diagnostic messages that describes the commands execution,
        options, buffer and response size.
    type: bool
    required: false
    default: false
  wait_time:
    description:
      - Set maximum time in seconds to wait for the commands to execute.
      - When set to 0, the system default is used.
      - This option is helpful on a busy system requiring more time to execute
        commands.
      - Setting I(wait) can instruct if execution should wait the
        full I(wait_time).
    type: int
    required: false
    default: 1
  time_unit:
    description:
      - Set the C(wait_time) unit of time, which can be C(s) (seconds) or C(cs) (centiseconds).
    type: str
    required: false
    default: s
    choices:
      - s
      - cs
  case_sensitive:
    description:
      - If C(true), the command will not be converted to uppercase before
        execution. Instead, the casing will be preserved just as it was
        written in a task.
    type: bool
    required: false
    default: false

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

notes:
    - Commands may need to use specific prefixes like $, they can be discovered by
      issuing the following command C(D OPDATA,PREFIX).
"""

EXAMPLES = r"""
- name: Execute an operator command to show device status and allocation
  zos_operator:
    cmd: 'd u'

- name: Execute an operator command to show device status and allocation with verbose information
  zos_operator:
    cmd: 'd u'
    verbose: true

- name: Execute an operator command to purge all job logs (requires escaping)
  zos_operator:
    cmd: "\\$PJ(*)"

- name: Execute operator command to show jobs, always waiting 5 seconds for response
  zos_operator:
    cmd: 'd a,all'
    wait_time: 5

- name: Display the system symbols and associated substitution texts.
  zos_operator:
    cmd: 'D SYMBOLS'

- name: Execute an operator command to show device status and allocation wait 10 centiseconds.
  zos_operator:
    cmd: 'd u'
    wait_time: 10
    time_unit: 'cs'
"""

RETURN = r"""
rc:
    description:
      Return code for the submitted operator command.
    returned: always
    type: int
    sample: 0
cmd:
    description:
      Operator command submitted.
    returned: always
    type: str
    sample: d u,all
elapsed:
    description:
      The number of seconds or centiseconds that elapsed waiting for the command to complete.
    returned: always
    type: float
    sample: 51.53
wait_time:
    description:
      The maximum time in the time_unit set to wait for the commands to execute.
    returned: always
    type: int
    sample: 5
time_unit:
    description:
      The time unit set for wait_time.
    returned: always
    type: str
    sample: s
stdout:
    description:
      The standard output from the operator command execution.
    returned: always
    type: str
    sample: >
        EC000000   2022244  16:00:49.00             ISF031I CONSOLE OMVS0000 ACTIVATED
        EC000000   2022244  16:00:49.00            -D U,ALL
        EC000000   2022244  16:00:49.00             IEE457I 16.00.49 UNIT STATUS 645
                                                   UNIT TYPE STATUS        VOLSER     VOLSTATE      SS
                                                   0000 3390 F-NRD                        /RSDNT     0
                                                   0001 3211 OFFLINE                                 0
stdout_lines:
    description:
      The standard output split into individual lines.
    returned: always
    type: list
    elements: str
    sample:
        - "EC000000   2022244  16:00:49.00             ISF031I CONSOLE OMVS0000 ACTIVATED"
        - "EC000000   2022244  16:00:49.00            -D U,ALL "
        - "EC000000   2022244  16:00:49.00             IEE457I 16.00.49 UNIT STATUS 645"
        - "                                           UNIT TYPE STATUS        VOLSER     VOLSTATE      SS"
        - "                                           0000 3390 F-NRD                        /RSDNT     0"
        - "                                           0001 3211 OFFLINE                                 0"
        - "                                           0002 3211 OFFLINE                                 0"
        - "                                           0008 3211 OFFLINE                                 0"
        - "                                           0009 3277 OFFLINE                                 0"
        - "                                           000C 2540 A                                       0"
        - "                                           000E 1403 A                                       0"
        - "                                           000F 1403 A                                       0"
        - "                                           0010 3211 A                                       0"
        - "                                           0011 3211 A                                       0"
stderr:
    description:
      The standard error from the operator command execution.
    returned: always
    type: str
    sample: ""
stderr_lines:
    description:
      The standard error split into individual lines.
    returned: always
    type: list
    elements: str
    sample: []
changed:
    description:
      Indicates if any changes were made during module operation.
      Given operator commands may introduce changes that are unknown to the
      module. True is always returned unless either a module or
      command failure has occurred.
    returned: always
    type: bool
    sample: true
"""

import traceback
from timeit import default_timer as timer
from ansible.module_utils.basic import AnsibleModule
from ansible.module_utils._text import to_text
from ansible_collections.ibm.ibm_zos_core.plugins.module_utils.ansible_module import (
    AnsibleModuleHelper,
)

from ansible_collections.ibm.ibm_zos_core.plugins.module_utils.import_handler import (
    # MissingZOAUImport,
    ZOAUImportError
)

from ansible_collections.ibm.ibm_zos_core.plugins.module_utils.better_arg_parser import (
    BetterArgParser,
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


def execute_command(operator_cmd, time_unit, timeout=1, preserve=False, *args, **kwargs):
    """
    Executes an operator command.

    Parameters
    ----------
    operator_cmd : str
        Command to execute.
    time_unit : str
        Unit of time to wait of execution of the command.
    timeout : int
        Time until it stops whether it finished or not.
    preserve : bool
        Whether to tell opercmd to preserve the case in the command.
    *args : dict
        Some arguments to pass on.
    **kwargs : dict
        Some other arguments to pass on.

    Returns
    -------
    tuple(int, str, str, int)
        Return code, standard output, standard error and time elapsed from start to finish.
    """
    # as of ZOAU v1.3.0, timeout is measured in centiseconds, therefore:
    if time_unit == "s":
        timeout = 100 * timeout

    start = timer()
    response = opercmd.execute(operator_cmd, timeout=timeout, preserve=preserve, *args, **kwargs)
    end = timer()
    rc = response.rc
    stdout = response.stdout_response
    stderr = response.stderr_response
    if time_unit == "cs":
        elapsed = round((end - start) * 100, 2)
    else:
        elapsed = round(end - start, 2)

    return rc, stdout, stderr, elapsed


def run_module():
    """Initialize the module.

    Raises
    ------
    fail_json
        An error ocurred while importing ZOAU.
    fail_json
        Expected response to be more than 2 lines.
    fail_json
        A non-zero return code was received.
    fail_json
        An unexpected error occurred.
    """
    module_args = dict(
        cmd=dict(type="str", required=True),
        verbose=dict(type="bool", required=False, default=False),
        wait_time=dict(type="int", required=False, default=1),
        time_unit=dict(type="str", required=False, choices=["s", "cs"], default="s"),
        case_sensitive=dict(type="bool", required=False, default=False),
    )

    result = dict(changed=False)
    module = AnsibleModule(argument_spec=module_args, supports_check_mode=False)
    validate_dependencies(module)

    # Checking that we can actually use ZOAU.
    if isinstance(opercmd, ZOAUImportError):
        module.fail_json(msg="An error ocurred while importing ZOAU: {0}".format(opercmd.traceback))

    try:

        new_params = parse_params(module.params)

        # Initialize logging module
        module_verbosity_level = module._verbosity
        SingletonLogger().get_logger(module_verbosity_level)

        rc_message = run_operator_command(new_params)
        result["rc"] = rc_message.get("rc")
        result["elapsed"] = rc_message.get("elapsed")

        # Process stdout and stderr
        stdout = rc_message.get("stdout")
        stderr = rc_message.get("stderr")

        result["stdout"] = stdout if stdout is not None else ""
        result["stderr"] = stderr if stderr is not None else ""

        # Build stdout_lines and stderr_lines
        result["stdout_lines"] = []
        if stdout is not None:
            for out in stdout.split("\n"):
                if out:
                    result["stdout_lines"].append(out)

        result["stderr_lines"] = []
        if stderr is not None:
            for err in stderr.split("\n"):
                if err:
                    result["stderr_lines"].append(err)

        # call is returned from run_operator_command, specifying what was run.
        result["cmd"] = rc_message.get("call")
        result["wait_time"] = new_params.get("wait_time")
        result["time_unit"] = new_params.get("time_unit")
        result["changed"] = False

        # rc=0, something succeeded (the calling script ran),
        # but it could still be a bad/invalid command.
        # As long as there are more than 2 lines in stdout, it's worth looking through.
        if int(result["rc"]) == 0:
            if len(result["stdout_lines"]) > 2:
                result["changed"] = True
            else:
                module.fail_json(msg="Expected response to be more than 2 lines.", **result)
        else:
            module.fail_json(msg=("A non-zero return code was received : {0}. Review the response for more details.").format(result["rc"]),
                             cmd=result["cmd"],
                             elapsed_time=result["elapsed"],
                             wait_time=result["wait_time"],
                             time_unit=result["time_unit"],
                             stderr=result["stderr"],
                             stderr_lines=result["stderr_lines"],
                             stdout=result["stdout"],
                             stdout_lines=result["stdout_lines"],
                             changed=result["changed"],)
    except Error as e:
        module.fail_json(msg=to_text(e), **result)
    except Exception as e:
        module.fail_json(
            msg="An unexpected error occurred: {0}".format(to_text(e)), **result
        )

    module.exit_json(**result)


def parse_params(params):
    """Use BetterArgParser to parse the module parameters.

    Parameters
    ----------
    params : dict
        Parameters to parse.

    Returns
    -------
    dict
        New parameters.
    """
    arg_defs = dict(
        cmd=dict(arg_type="str", required=True),
        verbose=dict(arg_type="bool", required=False, default=False),
        wait_time=dict(arg_type="int", required=False, default=1),
        time_unit=dict(type="str", required=False, choices=["s", "cs"], default="s"),
        case_sensitive=dict(arg_type="bool", required=False, default=False),
    )
    parser = BetterArgParser(arg_defs)
    new_params = parser.parse_args(params)
    return new_params


def run_operator_command(params):
    """Runs operator command based on a given parameters in a dictionary.

    Parameters
    ----------
    params : dict
        Operator command parameters to pass into the function.

    Returns
    -------
    dict
        Return code, standard output, standard error, the cmd call
        and time elapsed from beginning to end.
    """
    AnsibleModuleHelper(argument_spec={})

    kwargs = {}

    if params.get("verbose"):
        kwargs.update({"verbose": True})
        kwargs.update({"debug": True})

    wait_time = params.get("wait_time")
    time_unit = params.get("time_unit")
    cmdtxt = params.get("cmd")
    preserve = params.get("case_sensitive")

    use_wait_arg = False
    if zoau_version_checker.is_zoau_version_higher_than("1.2.4"):
        use_wait_arg = True

    if use_wait_arg:
        kwargs.update({"wait": True})

    args = []
    rc, stdout, stderr, elapsed = execute_command(cmdtxt, time_unit=time_unit, timeout=wait_time, preserve=preserve, *args, **kwargs)

    if rc > 0:
        message = "\nOut: {0}\nErr: {1}\nRan: {2}".format(stdout, stderr, cmdtxt)
        raise OperatorCmdError(cmdtxt, rc, message.split("\n"))

    return {
        "rc": rc,
        "stdout": stdout,
        "stderr": stderr,
        "call": cmdtxt,
        "elapsed": elapsed,
    }


class Error(Exception):
    pass


class OperatorCmdError(Error):
    """Exception raised when an error occurred executing the operator command.

    Parameters
    ----------
    cmd : str
        Command that failed.
    rc : int
        Return code.
    message : str
        Human readable string describing the exception.

    Attributes
    ----------
    msg : str
        Human readable string describing the exception.
    """
    def __init__(self, cmd, rc, message):
        self.msg = 'An error occurred executing the operator command "{0}", with RC={1} and response "{2}"'.format(
            cmd, str(rc), message
        )


def main():
    run_module()


if __name__ == "__main__":
    main()
