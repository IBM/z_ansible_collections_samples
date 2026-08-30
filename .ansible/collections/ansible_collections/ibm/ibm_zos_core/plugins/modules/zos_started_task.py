#!/usr/bin/python
# -*- coding: utf-8 -*-

# Copyright (c) IBM Corporation 2026
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#     http://www.apache.org/licenses/LICENSE-2.0
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.


from __future__ import (absolute_import, division, print_function)

__metaclass__ = type

DOCUMENTATION = r"""
module: zos_started_task
version_added: 1.16.0
author:
  - "Ravella Surendra Babu (@surendrababuravella)"
short_description: Perform operations on started tasks.
description:
  - start, display, modify, cancel, force and stop a started task

options:
  arm:
    description:
        - I(arm) indicates to execute normal task termination routines without causing address space destruction.
        - Only applicable when I(state) is C(forced), otherwise ignored.
    required: false
    type: bool
  armrestart:
    description:
        - Indicates that the batch job or started task should be automatically restarted after CANCEL or FORCE
          completes, if it is registered as an element of the automatic restart manager. If the job or task is
          not registered or if you do not specify this parameter, MVS will not automatically restart the job or task.
        - Only applicable when I(state) is C(cancelled) or C(forced), otherwise ignored.
    required: false
    type: bool
  asidx:
    description:
        - When I(state) is C(cancelled), C(stopped) or C(forced), I(asidx) is the hexadecimal address space
          identifier of the work unit you want to cancel, stop or force.
        - Only applicable when I(state) is C(stopped), C(cancelled), or C(forced), otherwise ignored.
    required: false
    type: str
#   device_type:
#     description:
#         - Type of the output device (if any) associated with the task.
#         - Only applicable when I(state) is C(started), otherwise ignored.
#     required: false
#     type: str
#   device_number:
#     description:
#         - Number of the device to be started. A device number is 3 or 4 hexadecimal digits. A slash (/) must
#           precede a 4-digit number but not a 3-digit number.
#         - Only applicable when I(state) is C(started), otherwise ignored.
#     required: false
#     type: str
  dump:
    description:
        - Whether to perform a dump. The type of dump (SYSABEND, SYSUDUMP, or SYSMDUMP)
          depends on the JCL for the job.
        - Only applicable when I(state) is C(cancelled), otherwise ignored.
    required: false
    type: bool
  identifier_name:
    description:
        - Option I(identifier_name) is the name that identifies the task. This name can be up to 8
          characters long. The first character must be alphabetical.
    required: false
    type: str
    aliases:
        - identifier
  system_logs:
    description:
        - When C(system_logs=true), the module will return system logs that describe the task's execution.
          This option can return a big response depending on system load, also it could surface other
          program's activity.
        - It is not recommended to have this option on all the time, but rather use it as a debugging option.
    required: false
    type: bool
    default: false
  job_account:
    description:
        - Specifies accounting data in the JCL JOB statement for the started task. If the source JCL
          already had accounting data, the value that is specified on this parameter overrides it.
        - Only applicable when I(state) is C(started), otherwise ignored.
    required: false
    type: str
  job_name:
    description:
        - When I(state) is started, this is the name which will be assigned to a started task while starting it.
          If I(job_name) is not specified, then I(member_name) is used as job's name.
        - When I(state) is C(displayed), C(modified), C(cancelled), C(stopped), or C(forced), I(job_name) is the
          started task name used to query the system.
    required: false
    type: str
    aliases:
        - job
        - task
        - task_name
  keyword_parameters:
    description:
        - Any appropriate keyword parameter that you specify to override the corresponding parameter in the cataloged
          procedure. The maximum length of each keyword=option pair is 66 characters. No individual value within this
          field can be longer than 44 characters in length.
        - Only applicable when I(state) is C(started), otherwise ignored.
    required: false
    type: dict
  member_name:
    description:
        - Name of a member of a partitioned data set that contains the source JCL for the task to be started. The member
          can be either a job or a cataloged procedure.
        - I(member_name) is mandatory and only applicable when I(state) is C(started), otherwise ignored.
    required: false
    type: str
    aliases:
        - member
  parameters:
    description:
        - Program parameters passed to the started program.
        - Only applicable when I(state) is C(started) or C(modified), otherwise ignored.
        - For example, REFRESH or REPLACE parameters can be passed while modifying a started task.
    required: false
    type: list
    elements: str
#   retry_force:
#     description:
#         - Indicates whether retry will be attempted on ABTERM(abnormal termination).
#         - I(tcb_address) is mandatory to use I(retry_force).
#         - Only applicable when I(state) is C(forced), otherwise ignored.
#     required: false
#     type: bool
  reusable_asid:
    description:
        - When I(reusable_asid) is C(true) and REUSASID(YES) is specified in the DIAGxx parmlib member, a reusable ASID is assigned
          to the address space created by the START command. If I(reusable_asid) is not specified or REUSASID(NO) is specified in
          DIAGxx, an ordinary ASID is assigned.
        - Only applicable when I(state) is C(started), otherwise ignored.
    required: false
    type: bool
  state:
    description:
        - I(state) is the desired state of the started task after the module is executed.
        - If I(state) is C(started) and the respective member is not present on the managed node, then error will be thrown with C(rc=1),
          C(changed=false) and I(stderr) which contains error details.
        - If I(state) is C(cancelled), C(modified), C(displayed), C(stopped) or C(forced) and the started task is not running on the managed node,
          then error will be thrown with C(rc=1), C(changed=false) and I(stderr) contains error details.
        - If I(state) is C(displayed) and the started task is running, then the module will return the started task details along with
          C(changed=true).
    required: True
    type: str
    choices:
        - started
        - displayed
        - modified
        - cancelled
        - stopped
        - forced
  subsystem:
    description:
        - The name of the subsystem that selects the task for processing. The name must be 1-4
          characters long, which are defined in the IEFSSNxx parmlib member, and the subsystem must
          be active.
        - Only applicable when I(state) is C(started), otherwise ignored.
    required: false
    type: str
  task_id:
    description:
        - A unique system-generated identifier that represents a specific started task running in z/OS. This id starts with STC.
        - Only applicable when I(state) is C(displayed), C(modified), C(cancelled), C(stopped), or C(forced), otherwise ignored.
    required: false
    type: str
#   tcb_address:
#     description:
#         - 6-digit hexadecimal TCB address of the task to terminate.
#         - Only applicable when I(state) is C(forced), otherwise ignored.
#     required: false
#     type: str
#   volume:
#     description:
#         - If I(device_type) is a tape or direct-access device, the serial number of the volume,
#           mounted on the device.
#         - Only applicable when I(state) is C(started), otherwise ignored.
#     required: false
#     type: str
  user_id:
    description:
        - The user ID of the time-sharing user you want to cancel or force.
        - Only applicable when I(state) is C(cancelled) or C(forced), otherwise ignored.
    required: false
    type: str
  verbose:
    description:
        - When C(verbose=true), the module will return the started task execution logs.
    required: false
    type: bool
    default: false
  wait_full_time:
    description:
        - For a started task that takes time to initialize, I(wait_time) with C(wait_full_time=true) ensures the started task
          completes initialization and JES updates the system control blocks.
        - If C(wait_full_time=false), the module polls every 5 seconds to check the status of the started task and returns
          immediately once the task is successfully validated.
        - When C(wait_full_time=true), the module waits for the duration specified in I(wait_time), even after the started
          task operation has been successfully validated.
    required: false
    default: false
    type: bool
  wait_time:
    description:
        - Total time that the module will wait for a submitted task, measured in seconds.
          The time begins when the module is executed on the managed node. Default value of 0 means to wait the default
          amount of time.
        - The default value is 10 seconds if this value is not specified, or if the specified value is less than 10.
    required: false
    default: 10
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
EXAMPLES = r"""
- name: Start a started task using a member in a partitioned data set.
  zos_started_task:
    state: "started"
    member_name: "PROCAPP"

- name: Start a started task using a member name and giving it an identifier.
  zos_started_task:
    state: "started"
    member_name: "PROCAPP"
    identifier_name: "SAMPLE"

- name: Start a started task using both a member and a job name.
  zos_started_task:
    state: "started"
    member_name: "PROCAPP"
    job_name: "SAMPLE"

- name: Start a started task and enable verbose output.
  zos_started_task:
    state: "started"
    member_name: "PROCAPP"
    job_name: "SAMPLE"
    verbose: true

- name: Start a started task and wait for 30 seconds before fetching task details.
  zos_started_task:
    state: "started"
    member_name: "PROCAPP"
    verbose: true
    wait_time: 30
    wait_full_time: true

- name: Start a started task specifying the subsystem and enabling a reusable ASID.
  zos_started_task:
    state: "started"
    member_name: "PROCAPP"
    subsystem: "MSTR"
    reusable_asid: "YES"

- name: Display a started task using a started task name.
  zos_started_task:
    state: "displayed"
    job_name: "PROCAPP"

- name: Display a started task using a started task id.
  zos_started_task:
    state: "displayed"
    task_id: "STC00012"

- name: Display all started tasks that begin with an s using a wildcard.
  zos_started_task:
    state: "displayed"
    job_name: "s*"

- name: Display all started tasks.
  zos_started_task:
    state: "displayed"
    job_name: "all"

- name: Cancel a started task using task name.
  zos_started_task:
    state: "cancelled"
    job_name: "SAMPLE"

- name: Cancel a started task using a started task id.
  zos_started_task:
    state: "cancelled"
    task_id: "STC00093"

- name: Cancel a started task using it's task name and ASID.
  zos_started_task:
    state: "cancelled"
    job_name: "SAMPLE"
    asidx: '0014'

- name: Modify a started task's parameters.
  zos_started_task:
    state: "modified"
    job_name: "SAMPLE"
    parameters: ["XX=12"]

- name: Modify a started task's parameters using a started task id.
  zos_started_task:
    state: "modified"
    task_id: "STC00034"
    parameters: ["XX=12"]

- name: Stop a started task using it's task name.
  zos_started_task:
    state: "stopped"
    job_name: "SAMPLE"

- name: Stop a started task using a started task id.
  zos_started_task:
    state: "stopped"
    task_id: "STC00087"

- name: Stop a started task using it's task name, identifier and ASID.
  zos_started_task:
    state: "stopped"
    job_name: "SAMPLE"
    identifier_name: "SAMPLE"
    asidx: 00A5

- name: Force a started task using it's task name.
  zos_started_task:
    state: "forced"
    job_name: "SAMPLE"

- name: Force a started task using it's task id.
  zos_started_task:
    state: "forced"
    task_id: "STC00065"
"""

RETURN = r"""
changed:
  description:
    - True if the state was changed, otherwise False.
  returned: always
  type: bool
cmd:
  description:
    - Command executed via opercmd to achieve the desired state.
  returned: changed
  type: str
  sample: S SAMPLE
msg:
  description:
    - Failure or skip message returned by the module.
  returned: failure or skipped
  type: str
  sample: Command parameters are invalid.
rc:
  description:
    - The return code is 0 when command executed successfully.
    - The return code is 1 when opercmd throws any error.
    - The return code is 4 when task_id format is invalid.
    - The return code is 5 when any parameter validation failed.
    - The return code is 8 when started task is not found using task_id.
  returned: changed
  type: int
  sample: 0
state:
  description:
    - The final state of the started task, after execution.
  returned: success
  type: str
  sample: S SAMPLE
stderr:
  description:
    - The STDERR from the command, may be empty.
  returned: failure
  type: str
  sample: An error has occurred.
stderr_lines:
  description:
    - List of strings containing individual lines from STDERR.
  returned: failure
  type: list
  sample: ["An error has occurred"]
stdout:
  description:
    - The STDOUT from the command, may be empty.
  returned: success
  type: str
  sample: ISF031I CONSOLE OMVS0000 ACTIVATED.
stdout_lines:
  description:
    - List of strings containing individual lines from STDOUT.
  returned: success
  type: list
  sample: ["Allocation to SYSEXEC completed."]
tasks:
  description:
    - The output information for a list of started tasks matching specified criteria.
    - If no started task is found then this will return empty.
  returned: success
  type: list
  elements: dict
  contains:
    asidx:
      description:
         - Address space identifier (ASID), in hexadecimal.
      type: str
      sample: 0054
    cpu_time:
      description:
         - The processor time used by the address space, including the initiator. This time does not include SRB time.
         - I(cpu_time) format is hhhhh.mm.ss.SSS(hours.minutes.seconds.milliseconds).
         - C(********) when time exceeds 100000 hours.
         - C(NOTAVAIL) when the TOD clock is not working.
      type: str
      sample: 00000.00.00.003
    elapsed_time:
      description:
         - For address spaces other than system address spaces, this value represents the elapsed time since the task
           was selected for execution.
         - For system address spaces created before master scheduler initialization, this value represents the elapsed
           time since the master scheduler was initialized.
         - For system address spaces created after master scheduler initialization, this value represents the elapsed
           time since the system address space was created.
         - I(elapsed_time) format is hhhhh.mm.ss.SSS(hours.minutes.seconds.milliseconds).
         - C(********) when time exceeds 100000 hours.
         - C(NOTAVAIL) when the TOD clock is not working.
      type: str
      sample: 00003.20.23.013
    started_time:
      description:
         - The time when the started task started.
         - C(********) when time exceeds 100000 hours.
         - C(NOTAVAIL) when the TOD clock is not working.
      type: str
      sample: "2025-09-11 18:21:50.293644+00:00"
    task_id:
      description:
         - The started task id.
      type: str
      sample: STC00018
    task_identifier:
      description:
         - The name of a system address space.
         - The name of a step, for a job or attached APPC transaction program attached by an initiator.
         - The identifier of a task created by the START command.
         - The name of a step that called a cataloged procedure.
         - C(STARTING) if initiation of a started job, system task, or attached APPC transaction program is incomplete.
         - C(*MASTER*) for the master address space.
         - The name of an initiator address space.
      type: str
      sample: SPROC
    task_name:
      description:
         - The name of the started task.
      type: str
      sample: SAMPLE
verbose_output:
  description:
     - If C(verbose=true), the system logs related to the started task executed state will be shown.
  returned: success
  type: str
  sample:  04.33.04 STC00077 ---- SUNDAY,    12 OCT 2025 ----....
"""

from ansible.module_utils.basic import AnsibleModule
import traceback
import re
import math
import time
from datetime import datetime, timedelta, timezone
from ansible_collections.ibm.ibm_zos_core.plugins.module_utils import (
    better_arg_parser
)
from ansible_collections.ibm.ibm_zos_core.plugins.module_utils.import_handler import (
    ZOAUImportError
)
from ansible_collections.ibm.ibm_zos_core.plugins.module_utils.dependency_checker import (
    validate_dependencies,
)

from ansible_collections.ibm.ibm_zos_core.plugins.module_utils.log import SingletonLogger

try:
    from zoautil_py import opercmd, zsystem, jobs
except ImportError:
    opercmd = ZOAUImportError(traceback.format_exc())
    zsystem = ZOAUImportError(traceback.format_exc())
    jobs = ZOAUImportError(traceback.format_exc())


def execute_command(operator_cmd):
    """Execute operator command.

    Parameters
    ----------
    operator_cmd : str
        Operator command.

    Returns
    -------
    tuple
        Tuple containing the RC, standard out, standard err of the
        query script and started task parameters.
    """
    # as of ZOAU v1.3.0, timeout is measured in centiseconds
    timeout_c = 0
    response = opercmd.execute(operator_cmd, timeout_c)

    rc = response.rc
    stdout = response.stdout_response
    stderr = response.stderr_response
    return rc, stdout, stderr


def fetch_current_time():
    """Execute operator command to fetch current time.

    Returns
    -------
    timestamp
        Current timestamp from z/OS server.
    """

    try:
        response = opercmd.execute("D T")
        stdout = response.stdout_response
        stdout_split_lines = stdout.split('\n')
        match = re.search(r'UTC.*?TIME=(\d{1,2}\.\d{1,2}\.\d{1,2})\s+DATE=(\d+\.\d+)', stdout_split_lines[2])
        year, day = match.groups()[1].split(".")
        h, m, s = match.groups()[0].split(".")
        date_time = datetime(int(year), 1, 1) + timedelta(
            days=int(day) - 1,
            hours=int(h),
            minutes=int(m),
            seconds=int(s)
        )
        date_time_utc = date_time.replace(tzinfo=timezone.utc)
    except Exception:
        return datetime.now(timezone.utc)
    return date_time_utc


def execute_display_command(started_task_name, asidx=None, task_params_before=None, state=None, wait_time=5, wait_full_time=False):
    """Execute operator display command.

    Parameters
    ----------
    started_task_name : str
        Name of the started task.
    asidx : string
        The HEX adress space identifier.
    task_params_before: list
        List of started task details which have same started task name.
    state : string
        State value passed in input.
    wait_time: int
        The time period to wait for a started task operation.
    wait_full_time: bool
        Indicates whether full amount of time mentioned in wait_time need to be waited.

    Returns
    -------
    list
        List contains extracted parameters from display command output of started task
    """
    cmd = f"d a,{started_task_name}"
    wait_interval = 5
    iteration_count = wait_time // 5
    is_response_required = False
    if state in ["started", "modified"]:
        is_response_required = True
    task_params = []
    for i in range(iteration_count):
        time.sleep(wait_interval)
        display_response = opercmd.execute(cmd)
        if display_response.rc == 0 and display_response.stderr_response == "":
            if state == "started":
                task_params = extract_keys(display_response.stdout_response, asidx, task_params_before)
            else:
                task_params = extract_keys(display_response.stdout_response, asidx)
        if (is_response_required and task_params) or (not is_response_required and not task_params):
            if wait_full_time:
                pending_time = wait_time - (i + 1) * 5
                time.sleep(pending_time)
                break
            else:
                break
    return task_params


def validate_and_prepare_start_command(module):
    """Validates parameters and creates start command

    Parameters
    ----------
    module : dict
        The started task start command parameters.

    Returns
    -------
    started_task_name
        The name of started task.
    cmd
        The start command in string format.
    """
    member = module.params.get('member_name')
    identifier = module.params.get('identifier_name')
    job_name = module.params.get('job_name')
    job_account = module.params.get('job_account')
    parameters = module.params.get('parameters', [])
    device_type = module.params.get('device_type') or ""
    device_number = module.params.get('device_number') or ""
    volume_serial = module.params.get('volume') or ""
    subsystem_name = module.params.get('subsystem')
    reusable_asid = ''
    if module.params.get('reusable_asid') is not None:
        if module.params.get('reusable_asid'):
            reusable_asid = 'YES'
        else:
            reusable_asid = 'NO'
    keyword_parameters = module.params.get('keyword_parameters')
    keyword_parameters_string = ""
    device = device_type if device_type else device_number
    # Validations
    if job_account and len(job_account) > 55:
        module.fail_json(
            rc=5,
            msg="The length of job_account exceeded 55 characters.",
            changed=False
        )
    if device_number:
        devnum_len = len(device_number)
        if devnum_len not in (3, 5) or (devnum_len == 5 and not device_number.startswith("/")):
            module.fail_json(
                rc=5,
                msg="device_number should be 3 or 4 characters long and preceded by / when it is 4 characters long.",
                changed=False
            )
    if subsystem_name and len(subsystem_name) > 4:
        module.fail_json(
            rc=5,
            msg="The subsystem_name must be 1-4 characters long.",
            changed=False
        )
    if keyword_parameters:
        for key, value in keyword_parameters.items():
            key_len = len(key)
            value_len = len(value)
            if key_len > 44 or value_len > 44 or key_len + value_len > 65:
                module.fail_json(
                    rc=5,
                    msg="The length of a keyword=option exceeded 66 characters or length of an individual value exceeded 44 characters."
                        + "key:{0}, value:{1}".format(key, value),
                    changed=False
                )
            else:
                if keyword_parameters_string:
                    keyword_parameters_string = f"{keyword_parameters_string},{key}={value}"
                else:
                    keyword_parameters_string = f"{key}={value}"
    if job_name:
        started_task_name = f"{job_name}.{job_name}"
    elif member:
        started_task_name = member
        if identifier:
            started_task_name = f"{started_task_name}.{identifier}"
        else:
            started_task_name = f"{started_task_name}.{started_task_name}"
    else:
        module.fail_json(
            rc=5,
            msg="member_name is missing which is mandatory to start a started task.",
            changed=False
        )
    if not member:
        module.fail_json(
            rc=5,
            msg="member_name is missing which is mandatory to start a started task.",
            changed=False
        )
    if job_name and identifier:
        module.fail_json(
            rc=5,
            msg="job_name and identifier_name are mutually exclusive while starting a started task.",
            changed=False
        )
    parameters_updated = ""
    if parameters:
        if len(parameters) == 1:
            parameters_updated = f"'{parameters[0]}'"
        else:
            parameters_updated = f"({','.join(parameters)})"

    cmd = f"S {member}"
    if identifier:
        cmd = f"{cmd}.{identifier}"
    if parameters:
        cmd = f"{cmd},{device},{volume_serial},{parameters_updated}"
    elif volume_serial:
        cmd = f"{cmd},{device},{volume_serial}"
    elif device:
        cmd = f"{cmd},{device}"
    if job_name:
        cmd = f"{cmd},JOBNAME={job_name}"
    if job_account:
        cmd = f"{cmd},JOBACCT={job_account}"
    if subsystem_name:
        cmd = f"{cmd},SUB={subsystem_name}"
    if reusable_asid:
        cmd = f"{cmd},REUSASID={reusable_asid}"
    if keyword_parameters_string:
        cmd = f"{cmd},{keyword_parameters_string}"
    return started_task_name, cmd


def fetch_task_name_and_asidx(module, task_id):
    """Executes JLS command and fetches task name

    Parameters
    ----------
    module : dict
        The started task display command parameters.
    task_id : str
        The started task id starts with STC.

    Returns
    -------
    task_name
        The name of started task.
    """
    try:
        task_details = jobs.fetch(task_id)
        if not isinstance(task_details, jobs.Job):
            module.fail_json(
                rc=1,
                msg=f"Fetching started task details using task_id: {task_id} is failed",
                changed=False
            )
    except Exception as err:
        module.fail_json(
            rc=err.response.rc,
            msg=f"Fetching started task details using task_id: {task_id} is failed with ZOAU error: {err.response.stderr_response}",
            changed=False
        )
    task_name = task_details.name
    asidx = f"{task_details.asid:04X}"
    return task_name, asidx


def prepare_display_command(module):
    """Validates parameters and creates display command

    Parameters
    ----------
    module : dict
        The started task display command parameters.

    Returns
    -------
    started_task_name
        The name of started task.
    asidx
        The address space identifier value, in hexadecimal.
    cmd
        The display command in string format.
    """
    identifier = module.params.get('identifier_name')
    job_name = module.params.get('job_name')
    task_id = module.params.get('task_id')
    started_task_name = ""
    task_name = asidx = ""
    if task_id:
        task_name, asidx = fetch_task_name_and_asidx(module, task_id)
    if task_name:
        started_task_name = task_name
    elif job_name:
        started_task_name = job_name
        if identifier:
            started_task_name = f"{started_task_name}.{identifier}"
    else:
        module.fail_json(
            rc=5,
            msg="either job_name or task_id is mandatory to display started task details.",
            changed=False
        )
    cmd = f"D A,{started_task_name}"
    return started_task_name, asidx, cmd


def prepare_stop_command(module, started_task=None, asidx=None, duplicate_tasks=False):
    """Validates parameters and creates stop command

    Parameters
    ----------
    module : dict
        The started task stop command parameters.
    started_task: string
        The started task name.
    asidx : string
        The address space identifier value, in hexadecimal.
    duplicate_tasks: bool
        Indicates if duplicate tasks are running.

    Returns
    -------
    started_task_name
        The name of started task.
    cmd
        The stop command in string format.
    """
    identifier = module.params.get('identifier_name')
    job_name = module.params.get('job_name')
    asidx = module.params.get('asidx') or asidx
    started_task_name = ""
    if started_task:
        started_task_name = started_task
    elif job_name:
        started_task_name = job_name
        if identifier:
            started_task_name = f"{started_task_name}.{identifier}"
        else:
            started_task_name = f"{started_task_name}.{started_task_name}"
    else:
        module.fail_json(
            rc=5,
            msg="either job_name or task_id is mandatory to stop a running started task.",
            changed=False
        )
    cmd = f"P {started_task_name}"
    if asidx or duplicate_tasks:
        cmd = f"{cmd},A={asidx}"
    return started_task_name, cmd


def prepare_modify_command(module, started_task=None):
    """Validates parameters and creates modify command

    Parameters
    ----------
    module : dict
        The started task modify command parameters.
    started_task: string
        The started task name.

    Returns
    -------
    started_task_name
        The name of started task.
    cmd
        The modify command in string format.
    """
    identifier = module.params.get('identifier_name')
    job_name = module.params.get('job_name')
    parameters = module.params.get('parameters')
    started_task_name = ""
    if started_task:
        started_task_name = started_task
    elif job_name:
        started_task_name = job_name
        if identifier:
            started_task_name = f"{started_task_name}.{identifier}"
        else:
            started_task_name = f"{started_task_name}.{started_task_name}"
    else:
        module.fail_json(
            rc=5,
            msg="either job_name or task_id is mandatory to modify a running started task.",
            changed=False
        )
    if parameters is None:
        module.fail_json(
            rc=5,
            msg="parameters are mandatory while modifying a started task.",
            changed=False
        )
    cmd = f"F {started_task_name},{','.join(parameters)}"
    return started_task_name, cmd


def prepare_cancel_command(module, started_task=None, asidx=None, duplicate_tasks=False):
    """Validates parameters and creates cancel command

    Parameters
    ----------
    module : dict
        The started task modify command parameters.
    started_task: string
        The started task name.
    asidx : string
        The address space identifier value, in hexadecimal.
    duplicate_tasks: bool
        Indicates if duplicate tasks are running.

    Returns
    -------
    started_task_name
        The name of started task.
    cmd
        The cancel command in string format.
    """
    identifier = module.params.get('identifier_name')
    job_name = module.params.get('job_name')
    asidx = module.params.get('asidx') or asidx
    dump = module.params.get('dump')
    armrestart = module.params.get('armrestart')
    user_id = module.params.get('user_id')
    started_task_name = ""
    if started_task:
        started_task_name = started_task
    elif job_name:
        started_task_name = job_name
        if identifier:
            started_task_name = f"{started_task_name}.{identifier}"
        else:
            started_task_name = f"{started_task_name}.{started_task_name}"
    else:
        if not user_id:
            module.fail_json(
                rc=5,
                msg="job_name, task_id and user_id are missing, one of them is needed to cancel a task.",
                changed=False
            )
    if user_id and armrestart:
        module.fail_json(
            rc=5,
            msg="The ARMRESTART parameter is not valid with the U=user_id parameter.",
            changed=False
        )
    if started_task_name:
        cmd = f"C {started_task_name}"
    else:
        cmd = f"FORCE U={user_id}"
    if asidx or duplicate_tasks:
        cmd = f"{cmd},A={asidx}"
    if dump:
        cmd = f"{cmd},DUMP"
    if armrestart:
        cmd = f"{cmd},ARMRESTART"
    return started_task_name, cmd


def prepare_force_command(module, started_task=None, asidx=None, duplicate_tasks=False):
    """Validates parameters and creates force command

    Parameters
    ----------
    module : dict
        The started task force command parameters.
    started_task: string
        The started task name.
    asidx : string
        The address space identifier value, in hexadecimal.
    duplicate_tasks: bool
        Indicates if duplicate tasks are running.

    Returns
    -------
    started_task_name
        The name of started task.
    cmd
        The force command in string format.
    """
    identifier = module.params.get('identifier_name')
    job_name = module.params.get('job_name')
    asidx = module.params.get('asidx') or asidx
    arm = module.params.get('arm')
    armrestart = module.params.get('armrestart')
    user_id = module.params.get('user_id')
    tcb_address = module.params.get('tcb_address')
    retry = ''
    if module.params.get('retry_force') is not None:
        if module.params.get('retry_force'):
            retry = 'YES'
        else:
            retry = 'NO'
    started_task_name = ""
    if tcb_address and len(tcb_address) != 6:
        module.fail_json(
            rc=5,
            msg="The TCB address of the task should be exactly 6-digit hexadecimal.",
            changed=False
        )
    if user_id and armrestart:
        module.fail_json(
            rc=5,
            msg="The ARMRESTART parameter is not valid with the U=user_id parameter.",
            changed=False
        )
    if started_task:
        started_task_name = started_task
    elif job_name:
        started_task_name = job_name
        if identifier:
            started_task_name = f"{started_task_name}.{identifier}"
        else:
            started_task_name = f"{started_task_name}.{started_task_name}"
    else:
        if not user_id:
            module.fail_json(
                rc=5,
                msg="job_name, task_id and user_id are missing, one of them is needed to force stop a running started task.",
                changed=False
            )
    if started_task_name:
        cmd = f"FORCE {started_task_name}"
    else:
        cmd = f"FORCE U={user_id}"
    if asidx or duplicate_tasks:
        cmd = f"{cmd},A={asidx}"
    if arm:
        cmd = f"{cmd},ARM"
    if armrestart:
        cmd = f"{cmd},ARMRESTART"
    if tcb_address:
        cmd = f"{cmd},TCB={tcb_address}"
    if retry:
        cmd = f"{cmd},RETRY={retry}"
    return started_task_name, cmd


def extract_keys(stdout, asidx=None, task_params_before=None):
    """Extracts keys and values from the given stdout

    Parameters
    ----------
    stdout : string
        The started task display command output
    asidx : string
        The address space identifier value, in hexadecimal.
    task_params_before: list
        List of started task details which have same started task name.

    Returns
    -------
    tasks
        The list of task parameters.
    """
    keys = {
        'A': 'asidx',
        'CT': 'cpu_time',
        'ET': 'elapsed_time',
        'WUID': 'task_id'
    }
    lines = stdout.strip().split('\n')
    tasks = []
    current_task = {}
    task_header_regex = re.compile(r'^\s*(\S+)\s+(\S+)\s+(\S+)\s+(\S+)\s+(\S+)')
    kv_pattern = re.compile(r'(\S+)=(\S+)')
    for line in lines[5:]:
        line = line.strip()
        match_firstline = task_header_regex.search(line)
        if len(line.split()) >= 5 and match_firstline:
            if current_task:
                current_task['started_time'] = ""
                el_time = current_task.get('elapsed_time')
                if el_time:
                    current_task['elapsed_time'] = convert_cpu_time(el_time) or current_task['elapsed_time']
                    current_task['started_time'] = calculate_start_time(el_time)
                if asidx:
                    if asidx == current_task.get('asidx'):
                        tasks.append(current_task)
                        current_task = {}
                        break
                elif task_params_before:
                    current_asid = current_task.get('asidx')
                    task_exists_before = False
                    for task in task_params_before:
                        if task.get('asidx') == current_asid:
                            task_exists_before = True
                            break
                    if not task_exists_before:
                        tasks.append(current_task)
                else:
                    tasks.append(current_task)
                current_task = {}
            current_task['task_name'] = match_firstline.group(1)
            current_task['task_identifier'] = match_firstline.group(2)
            for match in kv_pattern.finditer(line):
                key, value = match.groups()
                if key in keys:
                    key = keys[key]
                    current_task[key.lower()] = value
        elif current_task:
            for match in kv_pattern.finditer(line):
                key, value = match.groups()
                if key in keys:
                    key = keys[key]
                    current_task[key.lower()] = value
    if current_task:
        current_task['started_time'] = ""
        el_time = current_task.get('elapsed_time')
        if el_time:
            current_task['elapsed_time'] = convert_cpu_time(el_time) or current_task['elapsed_time']
            current_task['started_time'] = calculate_start_time(el_time)
        cpu_time = current_task.get('cpu_time')
        if cpu_time:
            current_task['cpu_time'] = convert_cpu_time(cpu_time) or current_task['cpu_time']
        if asidx:
            if asidx == current_task.get('asidx'):
                tasks.append(current_task)
        elif task_params_before:
            current_asid = current_task.get('asidx')
            task_exists_before = False
            for task in task_params_before:
                if task.get('asidx') == current_asid:
                    task_exists_before = True
                    break
            if not task_exists_before:
                tasks.append(current_task)
        else:
            tasks.append(current_task)
    return tasks


def parse_time(ts_str):
    """Parse timestamp

    Parameters
    ----------
    ts_str : string
        The time stamp in string format

    Returns
    -------
    timestamp
        Transformed timestamp
    """
    try:
        # Case 1: Duration like "000.005seconds"
        sec_match = re.match(r"^(\d+\.?\d*)\s*S?$", ts_str, re.IGNORECASE)
        if sec_match:
            return timedelta(seconds=float(sec_match.group(1)))
        # Case 2: hh.mm.ss
        hms_match = re.match(r"^(\d+).(\d{2}).(\d{2})$", ts_str)
        if hms_match:
            h, m, s = map(int, hms_match.groups())
            return timedelta(hours=h, minutes=m, seconds=s)
        # Case 3: hhhhh.mm
        hm_match = re.match(r"^(\d{1,5}).(\d{2})$", ts_str)
        if hm_match:
            h, m = map(int, hm_match.groups())
            return timedelta(hours=h, minutes=m)
    except Exception:
        return ""
    return ""


def calculate_start_time(ts_str):
    now = datetime.now().astimezone()
    parsed = parse_time(ts_str)
    if parsed is None:
        return ""
    # If it's a timedelta (duration), subtract from now → absolute datetime
    if isinstance(parsed, timedelta):
        return f"{now - parsed}"
    return ""


def convert_cpu_time(ts_str):
    parsed = parse_time(ts_str)
    if parsed is None:
        return ""
    # If it's a timedelta (duration), subtract from now → absolute datetime
    if isinstance(parsed, timedelta):
        total_seconds = int(parsed.total_seconds())
        milliseconds = int(parsed.microseconds / 1000)

        hours = total_seconds // 3600
        minutes = (total_seconds % 3600) // 60
        seconds = total_seconds % 60

        # Format: HHHHH.MM.SS.SSS
        return f"{hours:05}.{minutes:02}.{seconds:02}.{milliseconds:03}"
    return ""


def fetch_logs(command, before_time):
    """Extracts keys and values from the given stdout

    Parameters
    ----------
    command : string
        The comand which need to be checked in system logs
    before_time: timestamp
        The timestamp before executing the opercmd.

    Returns
    -------
    str
        Logs from SYSLOG
    """
    current_time = fetch_current_time()
    time_diff = current_time - before_time
    diff_minutes = math.ceil(time_diff.total_seconds() / 60)
    option = '-t' + str(diff_minutes)
    stdout = zsystem.read_console(options=option)
    stdout_lines = stdout.splitlines()
    first = None
    pattern = rf"\b{command}\b"
    for i, line in enumerate(stdout_lines):
        if re.search(pattern, line, re.IGNORECASE):
            first = i
    if not first:
        return ""
    logs = "\n".join(stdout_lines[first:])
    return logs


def get_task_logs(task_id):
    try:
        task_logs = jobs.read_output(task_id)
    except Exception:
        return ""

    return task_logs


def run_module():
    """Initialize the module.

    Raises
    ------
    fail_json
        z/OS started task operation failed.

    Note:
    5 arguments(device_number, device_type, volume, retry_force, tcb_address) are commented due to
    not tested those values in positive scenarios. These options will be enabled after successful
    testing. Below Git issues are created to track this.
    https://github.com/ansible-collections/ibm_zos_core/issues/2339
    https://github.com/ansible-collections/ibm_zos_core/issues/2340
    """
    module = AnsibleModule(
        argument_spec={
            'state': {
                'type': 'str',
                'required': True,
                'choices': ['started', 'stopped', 'modified', 'displayed', 'forced', 'cancelled']
            },
            'arm': {
                'type': 'bool',
                'required': False
            },
            'armrestart': {
                'type': 'bool',
                'required': False
            },
            'asidx': {
                'type': 'str',
                'required': False
            },
            # 'device_number': {
            #     'type': 'str',
            #     'required': False
            # },
            # 'device_type': {
            #     'type': 'str',
            #     'required': False
            # },
            'dump': {
                'type': 'bool',
                'required': False
            },
            'system_logs': {
                'type': 'bool',
                'required': False,
                'default': False
            },
            'identifier_name': {
                'type': 'str',
                'required': False,
                'aliases': ['identifier']
            },
            'job_account': {
                'type': 'str',
                'required': False
            },
            'job_name': {
                'type': 'str',
                'required': False,
                'aliases': ['job', 'task_name', 'task']
            },
            'keyword_parameters': {
                'type': 'dict',
                'required': False,
                'no_log': False
            },
            'member_name': {
                'type': 'str',
                'required': False,
                'aliases': ['member']
            },
            'parameters': {
                'type': 'list',
                'elements': 'str',
                'required': False
            },
            # 'retry_force': {
            #     'type': 'bool',
            #     'required': False
            # },
            'reusable_asid': {
                'type': 'bool',
                'required': False
            },
            'subsystem': {
                'type': 'str',
                'required': False
            },
            'task_id': {
                'type': 'str',
                'required': False
            },
            # 'tcb_address': {
            #     'type': 'str',
            #     'required': False
            # },
            'user_id': {
                'type': 'str',
                'required': False
            },
            'verbose': {
                'type': 'bool',
                'required': False,
                'default': False
            },
            # 'volume': {
            #     'type': 'str',
            #     'required': False
            # },
            'wait_full_time': {
                'type': 'bool',
                'required': False,
                'default': False
            },
            'wait_time': {
                'type': 'int',
                'required': False,
                'default': 10
            }
        },
        mutually_exclusive=[
            # ['device_number', 'device_type'],
            ['job_name', 'task_id'],
            ['identifier_name', 'task_id']
        ],
        # required_by={'retry_force': ['tcb_address']},
        supports_check_mode=True
    )

    validate_dependencies(module)

    args_def = {
        'state': {
            'arg_type': 'str',
            'required': True
        },
        'arm': {
            'arg_type': 'bool',
            'required': False
        },
        'armrestart': {
            'arg_type': 'bool',
            'required': False
        },
        'asidx': {
            'arg_type': 'str',
            'required': False
        },
        # 'device_number': {
        #     'arg_type': 'str',
        #     'required': False
        # },
        # 'device_type': {
        #     'arg_type': 'str',
        #     'required': False
        # },
        'dump': {
            'arg_type': 'bool',
            'required': False
        },
        'system_logs': {
            'arg_type': 'bool',
            'required': False,
            'default': False
        },
        'identifier_name': {
            'arg_type': 'identifier_name',
            'required': False,
            'aliases': ['identifier']
        },
        'job_account': {
            'arg_type': 'str',
            'required': False
        },
        'job_name': {
            'arg_type': 'str',
            'required': False,
            'aliases': ['job', 'task_name', 'task']
        },
        'keyword_parameters': {
            'arg_type': 'basic_dict',
            'required': False
        },
        'member_name': {
            'arg_type': 'member_name',
            'required': False,
            'aliases': ['member']
        },
        'parameters': {
            'arg_type': 'list',
            'elements': 'str',
            'required': False
        },
        # 'retry_force': {
        #     'arg_type': 'bool',
        #     'required': False
        # },
        'reusable_asid': {
            'arg_type': 'bool',
            'required': False
        },
        'subsystem': {
            'arg_type': 'str',
            'required': False
        },
        'task_id': {
            'type': 'str',
            'required': False
        },
        # 'tcb_address': {
        #     'arg_type': 'str',
        #     'required': False
        # },
        'user_id': {
            'arg_type': 'str',
            'required': False
        },
        'verbose': {
            'arg_type': 'bool',
            'required': False
        },
        # 'volume': {
        #     'arg_type': 'str',
        #     'required': False
        # },
        'wait_full_time': {
            'arg_type': 'bool',
            'required': False,
            'default': False
        },
        'wait_time': {
            'arg_type': 'int',
            'required': False
        }
    }
    # Validate input parameters
    try:
        parser = better_arg_parser.BetterArgParser(args_def)
        parsed_args = parser.parse_args(module.params)
        module.params = parsed_args
    except ValueError as err:
        module.fail_json(
            msg='Parameter verification failed.',
            stderr=str(err)
        )

    result = dict()
    if module.check_mode:
        module.exit_json(**result)

    # Initialize logging module
    module_verbosity_level = module._verbosity
    SingletonLogger().get_logger(module_verbosity_level)

    before_time = ""
    state = module.params.get('state')
    wait_time_s = module.params.get('wait_time')
    if wait_time_s < 10:
        wait_time_s = 10
    wait_full_time = module.params.get('wait_full_time')
    verbose = module.params.get('verbose')
    # Fetch started task name if task_id is present in the request by executing display command
    execute_display_before = True
    task_id = module.params.get('task_id')
    task_name = ""
    asidx = module.params.get('asidx')
    duplicate_tasks = False
    started_task_name_from_id = ""
    get_system_logs = module.params.get('system_logs')
    task_params_before = []
    task_params_after = []
    if task_id and state != "displayed":
        execute_display_before = False
        if state == "started":
            module.fail_json(
                rc=1,
                msg="task_id is not a valid parameter when state=started.",
                changed=False
            )
        else:
            task_name, asidx = fetch_task_name_and_asidx(module, task_id)
            task_id_params = execute_display_command(task_name)
            if len(task_id_params) > 1:
                duplicate_tasks = True
            for task in task_id_params:
                if task['asidx'] == asidx:
                    task_params_before.append(task)
                    started_task_name_from_id = f"{task['task_name']}.{task['task_identifier']}"
            if not task_params_before:
                module.fail_json(
                    rc=1,
                    msg="Started task of the given task_id is not active.",
                    changed=False
                )
    """
    Below error messages or error codes are used to determine if response has any error.

    JCL ERROR - IEE122I: Response contains this keyword when JCL contains syntax error.
    INVALID PARAMETER - IEE535I: When invalid parameter passed in command line.
    NOT ACTIVE - IEE341I: When started task with the given job name is not active
    REJECTED: When modify command is not supported by respective started task.
    NOT LOGGED ON - IEE324I: When invalid user_id passed in command.
    DUPLICATE NAME FOUND - IEE842I: When multiple started tasks exist with same name.
    NON-CANCELABLE - IEE838I: When cancel command can't stop job and force command is needed.
    CANCELABLE - IEE838I: When force command used without using cancel command
    """
    start_errmsg = ['IEE122I', 'IEE535I', 'IEE307I', 'ERROR', 'IEE708I']
    stop_errmsg = ['IEE341I', 'IEE535I', 'IEE708I']
    display_errmsg = ['IEE341I', 'IEE535I', 'NOT FOUND', 'IEE708I']
    modify_errmsg = ['REJECTED', 'IEE341I', 'IEE535I', 'IEE311I', 'IEE708I', 'ISF302E']
    cancel_errmsg = ['IEE341I', 'IEE324I', 'IEE535I', 'IEE842I', 'NON-CANCELABLE', 'IEE708I']
    force_errmsg = ['IEE341I', 'IEE324I', 'IEE535I', 'CANCELABLE', 'IEE842I', 'IEE708I']
    error_details = {
        'IEE122I': 'Specified member is missing or PROC/JOB contains incorrect JCL statements.',
        'IEE535I': 'A parameter on a command is not valid.',
        'IEE307I': 'Command parameter punctuation is incorrect or parameter is not followed by a blank.',
        'ERROR': 'Member is missing in PROCLIB or JCL is invalid or issue with JCL execution.',
        'NOT FOUND': 'Started task is not active',
        'IEE341I': 'Started task is not active',
        'REJECTED': 'Started task is not accepting modification.',
        'IEE324I': 'The user_id specified on the command is not currently active in the system..',
        'IEE842I': 'More than one active job with the specified name exist.',
        'NON-CANCELABLE': 'The task cannot be canceled. Use the FORCE ARM command.',
        'CANCELABLE': 'The task can be canceled. Use the CANCEL command.',
        'IEE311I': 'Required parameter is missing.',
        'IEE708I': 'The value of a keyword specified on a command is incorrect.',
        'ISF302E': 'Parameters are invalid.'
    }
    err_msg = []
    kwargs = {}
    kwargs.update({"wait": True})
    cmd = ""
    # Create comannd depending on the input operation and parameters
    if state == "started":
        err_msg = start_errmsg
        started_task_name, cmd = validate_and_prepare_start_command(module)
        before_time = fetch_current_time()
    elif state == "displayed":
        err_msg = display_errmsg
        started_task_name, asidx, cmd = prepare_display_command(module)
    elif state == "stopped":
        err_msg = stop_errmsg
        started_task_name, cmd = prepare_stop_command(module, started_task_name_from_id, asidx, duplicate_tasks)
    elif state == "cancelled":
        err_msg = cancel_errmsg
        started_task_name, cmd = prepare_cancel_command(module, started_task_name_from_id, asidx, duplicate_tasks)
    elif state == "forced":
        err_msg = force_errmsg
        started_task_name, cmd = prepare_force_command(module, started_task_name_from_id, asidx, duplicate_tasks)
    elif state == "modified":
        err_msg = modify_errmsg
        started_task_name, cmd = prepare_modify_command(module, started_task_name_from_id)
    # Note the timestamp before operation execution to fetch system logs within that time frame.
    if get_system_logs:
        before_time = fetch_current_time()
    changed = False
    stdout = ""
    stderr = ""
    # Execute display before to validate whether started task exists or not to perform required operation
    if execute_display_before and started_task_name:
        task_params_before = execute_display_command(started_task_name, asidx)
        if not task_params_before and state != 'started':
            module.fail_json(
                rc=1,
                msg="No started task is active with the given name.",
                changed=False
            )
    # Execute the operation on started task as mentioned in input
    rc, out, err = execute_command(cmd)
    is_failed = False
    system_logs = ""
    task_output_logs = ""
    msg = ""
    task_params = []
    # Validate the sysout of opercmd with the above listed error codes to find if any error occured
    found_msg = next((msg for msg in err_msg if msg in out), None)
    if err != "" or found_msg:
        is_failed = True
        msg = error_details.get(found_msg)
    # If error did not find in previous step then run display command again and validate operation result
    if not is_failed:
        if state == "displayed":
            task_params_after = extract_keys(out, asidx)
            if not task_params_after:
                module.fail_json(
                    rc=1,
                    msg="No started task is active with the given name.",
                    changed=False
                )
        else:
            task_params_after = execute_display_command(started_task_name, asidx, task_params_before, state, wait_time_s, wait_full_time)
        # Assign tasks depending on state value.
        if state in ("cancelled", "forced", "stopped"):
            task_params = task_params_before
            if task_params_after:
                is_failed = True
        else:
            task_params = task_params_after
            if not task_params_after:
                is_failed = True
            elif verbose:
                task_output_logs = get_task_logs(task_params_after[0].get('task_id'))
    # Fetch system logs as per the recorded timestamp after operation execution
    if get_system_logs:
        system_logs = fetch_logs(cmd.upper(), before_time)
    # Create response to return
    current_state = ""
    if is_failed:
        if rc == 0:
            rc = 1
        changed = False
        if not msg:
            msg = (
                f"Status of the started task is not as expected after executing the operation {state}."
                "Get system logs by enabling system_logs option to know more details."
            )
        stdout = out
        stderr = err
        if err == "" or err is None:
            stderr = out
            stdout = ""
    else:
        current_state = state
        changed = True
        stdout = out
        stderr = err

    if state == "displayed":
        changed = False

    result = dict(
        changed=changed,
        state=current_state,
        cmd=cmd,
        msg=msg,
        tasks=task_params,
        rc=rc,
        stdout=stdout,
        stderr=stderr,
        stdout_lines=stdout.split('\n'),
        stderr_lines=stderr.split('\n'),
        system_logs=system_logs,
        verbose_output=task_output_logs
    )

    module.exit_json(**result)


if __name__ == '__main__':
    run_module()
