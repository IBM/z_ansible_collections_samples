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
module: zos_job_query
version_added: '1.0.0'
short_description: Query job status
description:
  - List z/OS job(s) and the current status of the job(s).
  - Uses job_name to filter the jobs by the job name.
  - Uses job_id to filter the jobs by the job identifier.
  - Uses owner to filter the jobs by the job owner.
  - Uses system to filter the jobs by system where the job is running (or ran) on.
author:
  - "Ping Xiao (@xiaopingBJ)"
  - "Demetrios Dimatos (@ddimatos)"
  - "Rich Parker (@richp405)"
options:
  job_name:
    description:
       - The job name to query.
       - A job name can be up to 8 characters long.
       - The I(job_name) can contain include multiple wildcards.
       - The asterisk (`*`) wildcard will match zero or more specified characters.
       - Note that using this value will query the system for '*' and then return just matching values.
       - This may lead to security issues if there are read-access limitations on some users or jobs.
    type: str
    required: False
    default: "*"
  owner:
    description:
      - Identifies the owner of the job.
      - If no owner is set, the default set is 'none' and all jobs will be
        queried.
    type: str
    required: False
  job_id:
    description:
      - The job id that has been assigned to the job.
      - A job id must begin with `STC`, `JOB`, `TSU` and are
        followed by up to 5 digits.
      - When a job id is greater than 99,999, the job id format will begin
        with `S`, `J`, `T` and are followed by 7 digits.
      - The I(job_id) can contain include multiple wildcards.
      - The asterisk (`*`) wildcard will match zero or more specified characters.
    type: str
    required: False

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
- name: Query a job with a job name of 'JOB12345'
  zos_job_query:
    job_name: "JOB12345"

- name: Query jobs using a wildcard to match any job id begging with 'JOB12'
  zos_job_query:
    job_id: "JOB12*"

- name: Query jobs using wildcards to match any job name begging with 'H' and ending in 'O'.
  zos_job_query:
    job_name: "H*O"

- name: Query jobs using a wildcards to match a range of job id(s) that include 'JOB' and '014'.
  zos_job_query:
    job_id: JOB*014*

- name: Query all job names beginning wih 'H' that match job id that includes '14'.
  zos_job_query:
    job_name: "H*"
    job_id: "JOB*14*"

- name: Query all jobs names beginning with 'LINK' for owner 'ADMIN'.
  zos_job_query:
    job_name: "LINK*"
    owner: ADMIN
"""

RETURN = r"""
changed:
  description:
    True if the state was changed, otherwise False.
  returned: always
  type: bool
  sample: True
jobs:
  description:
    The output information for a list of jobs matching specified criteria.
    If no job status is found, this will return ret_code dictionary with
    parameter msg_txt = The job could not be found.
  returned: success
  type: list
  elements: dict
  contains:
    job_name:
      description:
         The name of the batch job.
      type: str
      sample: LINKJOB
    owner:
      description:
         The owner who ran the job.
      type: str
      sample: ADMIN
    job_id:
      description:
         Unique job identifier assigned to the job by JES.
      type: str
      sample: JOB01427
    content_type:
      description:
        - Type of address space used by the job, can be one of the following types.
        - APPC for an APPC Initiator.
        - JGRP for a JOBGROUP.
        - JOB for a Batch job.
        - STC for a Started task.
        - TSU for a Time sharing user.
        - \? for an unknown or pending job.
      type: str
      sample: STC
    system:
      description:
         The job entry system that MVS uses to do work.
      type: str
      sample: STL1
    subsystem:
      description:
         The job entry subsystem that MVS uses to do work.
      type: str
      sample: STL1
    origin_node:
      description:
        Origin node that submitted the job.
      type: str
      sample: "STL1"
    execution_node:
      description:
        Execution node that picked the job and executed it.
      type: str
      sample: "STL1"
    cpu_time:
      description:
        Sum of the CPU time used by each job step, in microseconds.
      type: int
      sample: 5
    ret_code:
      description:
         Return code output collected from job log.
      type: dict
      contains:
        msg:
          description:
            Return code or abend resulting from the job submission.
          type: str
          sample: CC 0000
        msg_code:
          description:
            Return code extracted from the `msg` so that it can be evaluated.
            For example, ABEND(S0C4) would yield "S0C4".
          type: str
          sample: S0C4
        msg_txt:
          description:
             Returns additional information related to the job.
          type: str
          sample: "No job can be located with this job name: HELLO"
        code:
          description:
             Return code converted to integer value (when possible).
          type: int
          sample: 00
        steps:
          description:
            Series of JCL steps that were executed and their return codes.
          type: list
          elements: dict
          contains:
            step_name:
              description:
                Name of the step shown as "was executed" in the DD section.
              type: str
              sample: "STEP0001"
            step_cc:
              description:
                The CC returned for this step in the DD section.
              type: int
              sample: 0
      sample:
        ret_code: {
         "msg": "CC 0000",
         "msg_code": "0000",
         "msg_txt": "",
         "code": 0
        }
    steps:
      description:
        Series of JCL steps that were executed and their return codes.
      type: list
      elements: dict
      contains:
        step_name:
          description:
            Name of the step shown as "was executed" in the DD section.
          type: str
          sample: "STEP0001"
        step_cc:
          description:
            The CC returned for this step in the DD section.
          type: int
          sample: 0
      sample:
        "steps": [
          { "step_name": "STEP0001",
            "step_cc": 0
          }
        ]
    job_class:
      description:
        Job class for this job.
      type: str
      sample: A
    svc_class:
      description:
        Service class for this job.
      type: str
      sample: C
    priority:
      description:
        A numeric indicator of the job priority assigned through JES.
      type: int
      sample: 4
    asid:
      description:
        The address Space Identifier (ASID) that is a unique descriptor for the job address space.
        Zero if not active.
      type: int
      sample: 0
    creation_date:
      description:
        Date, local to the target system, when the job was created.
      type: str
      sample: "2023-05-04"
    creation_time:
      description:
        Time, local to the target system, when the job was created.
      type: str
      sample: "14:15:00"
    queue_position:
      description:
        The position within the job queue where the jobs resides.
      type: int
      sample: 3
    program_name:
      description:
        The name of the program found in the job's last completed step found in the PGM parameter.
      type: str
      sample: "IEBGENER"
    execution_time:
      description:
        Total duration time of the job execution, if it has finished. If the job is still running,
        it represents the time elapsed from the job execution start and current time.
      type: str
      sample: 00:00:10

  sample:
    [
        {
            "job_name": "LINKJOB",
            "owner": "ADMIN",
            "job_id": "JOB01427",
            "content_type": "JOB",
            "ret_code": { "msg" : "CC", "msg_code" : "0000", "code" : "0", msg_txt : "CC" },
            "steps": [
              { "step_name": "STEP0001",
                "step_cc": 0
              }
            ],
            "job_class": "STC",
            "svc_class": "null",
            "priority": 1,
            "asid": 0,
            "creation_date": "2023-05-03",
            "creation_time": "12:13:00",
            "program_name": "BPXBATCH",
            "queue_position": 3,
            "execution_time": "00:00:02",
            "system": "STL1",
            "subsystem": "STL1",
            "cpu_time": 1414,
            "execution_node": "STL1",
            "origin_node": "STL1"
        },
        {
            "job_name": "LINKCBL",
            "owner": "ADMIN",
            "job_id": "JOB16577",
            "content_type": "JOB",
            "ret_code": { "msg" : "CANCELED", "msg_code" : "null", "code" : "null", msg_txt : "CANCELED" },
            "steps" : [],
            "job_class": "A",
            "svc_class": "E",
            "priority": 0,
            "asid": 4,
            "creation_date": "2023-05-03",
            "creation_time": "12:14:00",
            "queue_position": 0,
            "execution_time": "00:00:03",
            "program_name": "null",
            "system": "STL1",
            "subsystem": "STL1",
            "cpu_time": 1414,
            "execution_node": "STL1",
            "origin_node": "STL1"
        },
    ]
msg:
  description:
     Message returned on failure.
  type: str
  returned: failure
  sample:
     msg: "List FAILED! no such job been found: IYK3Z0R9"
"""

from ansible_collections.ibm.ibm_zos_core.plugins.module_utils.job import (
    job_status,
)
from ansible_collections.ibm.ibm_zos_core.plugins.module_utils import (
    better_arg_parser
)
from ansible.module_utils.basic import AnsibleModule
from ansible.module_utils._text import to_text
from ansible_collections.ibm.ibm_zos_core.plugins.module_utils.dependency_checker import (
    validate_dependencies,
)
from ansible_collections.ibm.ibm_zos_core.plugins.module_utils.log import SingletonLogger


def run_module():
    """Initialize the module.

    Raises
    ------
    fail_json
        Parameter verification failed.
    fail_json
        Any exception while getting job params.
    """
    module_args = dict(
        job_name=dict(type="str", required=False, default="*"),
        owner=dict(type="str", required=False),
        job_id=dict(type="str", required=False),
    )

    result = dict(changed=False)

    module = AnsibleModule(argument_spec=module_args, supports_check_mode=True)
    # Validate dependencies
    validate_dependencies(module)

    args_def = dict(
        job_name=dict(type="job_identifier", required=False),
        owner=dict(type="str", required=False),
        job_id=dict(type="job_identifier", required=False),
    )

    try:
        parser = better_arg_parser.BetterArgParser(args_def)
        parsed_args = parser.parse_args(module.params)
        module.params = parsed_args
    except ValueError as err:
        module.fail_json(
            msg='Parameter verification failed.',
            stderr=str(err)
        )

    # Initialize logging module
    module_verbosity_level = module._verbosity
    SingletonLogger().get_logger(module_verbosity_level)

    if module.check_mode:
        return result

    try:
        name = module.params.get("job_name")
        id = module.params.get("job_id")
        owner = module.params.get("owner")
        jobs_raw = query_jobs(name, id, owner)
        if jobs_raw:
            jobs = parsing_jobs(jobs_raw)
            result["changed"] = True
        else:
            jobs = None

    except Exception as e:
        module.fail_json(msg=to_text(e), **result)
    result["jobs"] = jobs
    module.exit_json(**result)


def query_jobs(job_name, job_id, owner):
    """Returns jobs that coincide with the given arguments.

    Parameters
    ----------
    job_name : str
        Name of the jobs.
    job_id : str
        Id of the jobs.
    owner : str
        Owner of the jobs.

    Returns
    -------
    Union[str]
        List with the jobs.

    Raises
    ------
    RuntimeError
        No job with was found.
    """
    jobs = []
    if job_id:
        jobs = job_status(job_id=job_id)
    elif owner:
        jobs = job_status(owner=owner, job_name=job_name)
    else:
        jobs = job_status(job_name=job_name)
    if not jobs:
        raise RuntimeError("List FAILED! no such job was found.")
    return jobs


def parsing_jobs(jobs_raw):
    """Parse job into an understandable format.

    Parameters
    ----------
    jobs_raw : dict
        Raw jobs.

    Returns
    -------
    dict
        Parsed jobs.
    """
    jobs = []
    for job in jobs_raw:
        ret_code = job.get("ret_code")
        # Easier to see than checking for an empty string, JOB NOT FOUND was
        # replaced with None in the jobs.py and msg_txt field describes the job query instead
        if job.get("ret_code") is None:
            status_raw = "JOB NOT FOUND"
            ret_code["msg"] = status_raw
        elif job.get("ret_code").get("msg", "JOB NOT FOUND") is None:
            status_raw = "JOB NOT FOUND"
            ret_code["msg"] = status_raw
        else:
            status_raw = job.get("ret_code").get("msg", "JOB NOT FOUND")

        if "AC" in status_raw:
            ret_code["msg"] = status_raw

        elif "CC" in status_raw:
            # status = 'Completed normally'
            ret_code["msg"] = status_raw

        elif "ABEND" in status_raw:
            # status = 'Ended abnormally'
            ret_code["msg"] = status_raw

        elif "ABENDU" in status_raw:
            # status = 'Ended abnormally'
            ret_code["msg"] = status_raw

        elif "CANCELED" in status_raw or "JCLERR" in status_raw or "JCL ERROR" in status_raw or "JOB NOT FOUND" in status_raw:
            # status = status_raw
            ret_code["msg"] = status_raw

        else:
            # status = 'Unknown'
            ret_code["msg"] = status_raw

        job_dict = {
            "job_name": job.get("job_name"),
            "owner": job.get("owner"),
            "job_id": job.get("job_id"),
            "content_type": job.get("content_type"),
            "system": job.get("system"),
            "subsystem": job.get("subsystem"),
            "cpu_time": job.get("cpu_time"),
            "execution_node": job.get("execution_node"),
            "origin_node": job.get("origin_node"),
            "ret_code": ret_code,
            "steps": job.get("steps"),
            "job_class": job.get("job_class"),
            "svc_class": job.get("svc_class"),
            "priority": job.get("priority"),
            "asid": job.get("asid"),
            "creation_date": job.get("creation_date"),
            "creation_time": job.get("creation_time"),
            "execution_time": job.get("execution_time"),
            "queue_position": job.get("queue_position"),
            "program_name": job.get("program_name"),
        }
        jobs.append(job_dict)
    return jobs


def main():
    run_module()


if __name__ == "__main__":
    main()
