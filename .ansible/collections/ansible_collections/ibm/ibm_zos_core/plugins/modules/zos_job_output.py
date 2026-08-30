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
module: zos_job_output
short_description: Display job output
description:
  - Display the z/OS job output for a given criteria (Job id/Job name/owner)
    with/without a data definition name as a filter.
  - At least provide a job id/job name/owner.
  - The job id can be specific such as "STC02560", or one that uses a pattern
    such as "STC*" or "*".
  - The job name can be specific such as "TCPIP", or one that uses a pattern
    such as "TCP*" or "*".
  - The owner can be specific such as "IBMUSER", or one that uses a pattern
    like "*".
  - If there is no dd_name, or if dd_name="?", output of all the dds under
    the given job will be displayed.
  - If SYSIN DDs are needed, I(sysin_dd) should be set to C(true).
version_added: "1.0.0"
author:
  - "Jack Ho (@jacklotusho)"
  - "Demetrios Dimatos (@ddimatos)"
  - "Rich Parker (@richp405)"
options:
  job_id:
    description:
      - The z/OS job ID of the job containing the spool file.
        (e.g "STC02560", "STC*")
    type: str
    required: false
  job_name:
    description:
      - The name of the batch job. (e.g "TCPIP", "C*")
    type: str
    required: false
  owner:
    description:
      - The owner who ran the job. (e.g "IBMUSER", "*")
    type: str
    required: false
  dd_name:
    description:
      - Data definition name (show only this DD on a found job).
        (e.g "JESJCL", "?")
    type: str
    required: false
    aliases: [ ddname ]
  sysin_dd:
    description:
      - Whether to include SYSIN DDs as part of the output.
    type: bool
    default: false
    required: false

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
- name: Job output with dd_name
  zos_job_output:
    job_id: "STC02560"
    dd_name: "JESMSGLG"

- name: JES Job output without dd_name
  zos_job_output:
    job_id: "STC02560"

- name: JES Job output with all dd_name
  zos_job_output:
    job_id: "STC*"
    job_name: "*"
    owner: "IBMUSER"
    dd_name: "?"

- name: Query a job's output including SYSIN DDs
  zos_job_output:
    job_id: "JOB00548"
    sysin_dd: true
"""

RETURN = r"""
jobs:
  description:
    The output information for a list of jobs matching specified criteria.
    If no job status is found, this will return ret_code dictionary with
    parameter msg_txt = The job could not be found.
  returned: success
  type: list
  elements: dict
  contains:
    job_id:
      description:
         The z/OS job ID of the job containing the spool file.
      type: str
      sample: JOB00134
    job_name:
      description:
         The name of the batch job.
      type: str
      sample: HELLO
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
    cpu_time:
      description:
        Sum of the CPU time used by each job step, in microseconds.
      type: int
      sample: 5
    execution_node:
      description:
        Execution node that picked the job and executed it.
      type: str
      sample: "STL1"
    origin_node:
      description:
        Origin node that submitted the job.
      type: str
      sample: "STL1"
    class:
      description:
        Identifies the data set used in a system output data set, usually called a sysout data set.
      type: str
      sample:
    content_type:
      description:
        Type of address space used by the job, can be one of the following types.
        - APPC for an APPC Initiator.
        - JGRP for a JOBGROUP.
        - JOB for a Batch job.
        - STC for a Started task.
        - TSU for a Time sharing user.
        - \? for an unknown or pending job.
      type: str
      sample: JOB
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
    execution_time:
      description:
        Total duration time of the job execution, if it has finished. If the job is still running,
        it represents the time elapsed from the job execution start and current time.
      type: str
      sample: 00:00:10
    dds:
      description:
        Data definition names.
      type: list
      elements: dict
      contains:
        dd_name:
          description:
            Data definition name.
          type: str
          sample: JESMSGLG
        record_count:
          description:
              Count of the number of lines in a print data set.
          type: int
          sample: 17
        id:
          description:
            The file ID.
          type: str
          sample: 2
        stepname:
          description:
              A step name is name that identifies the job step so that other
              JCL statements or the operating system can refer to it.
          type: str
          sample: JES2
        procstep:
          description:
            Identifies the set of statements inside JCL grouped together to
            perform a particular function.
          type: str
          sample: PROC1
        byte_count:
          description:
              Byte size in a print data set.
          type: int
          sample: 574
        content:
          description:
            The dd content.
          type: list
          elements: str
          sample:
             [ "         1 //HELLO    JOB (T043JM,JM00,1,0,0,0),'HELLO WORLD - JRM',CLASS=R,       JOB00134",
               "           //             MSGCLASS=X,MSGLEVEL=1,NOTIFY=S0JM                                ",
               "           //*                                                                             ",
               "           //* PRINT \"HELLO WORLD\" ON JOB OUTPUT                                         ",
               "           //*                                                                             ",
               "           //* NOTE THAT THE EXCLAMATION POINT IS INVALID EBCDIC FOR JCL                   ",
               "           //*   AND WILL CAUSE A JCL ERROR                                                ",
               "           //*                                                                             ",
               "         2 //STEP0001 EXEC PGM=IEBGENER                                                    ",
               "         3 //SYSIN    DD DUMMY                                                             ",
               "         4 //SYSPRINT DD SYSOUT=*                                                          ",
               "         5 //SYSUT1   DD *                                                                 ",
               "         6 //SYSUT2   DD SYSOUT=*                                                          ",
               "         7 //                                                                              "
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
      sample:
        ret_code: {
         "code": 0,
         "msg": "CC 0000",
         "msg_code": "0000",
         "msg_txt": "",
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
      sample: [
        { "step_name": "STEP0001",
          "step_cc": 0
        }
      ]
  sample:
     [
      {
        "class": "R",
        "content_type": "JOB",
        "dds": [
          {
            "byte_count": "775",
            "content": [
              "1                       J E S 2  J O B  L O G  --  S Y S T E M  S T L 1  --  N O D E  S T L 1            ",
              "0 ",
              " 10.25.48 JOB00134 ---- TUESDAY,   18 FEB 2020 ----",
              " 10.25.48 JOB00134  IRR010I  USERID OMVSADM  IS ASSIGNED TO THIS JOB.",
              " 10.25.48 JOB00134  $HASP375 JES2     ESTIMATED  LINES EXCEEDED",
              " 10.25.48 JOB00134  ICH70001I OMVSADM  LAST ACCESS AT 10:25:47 ON TUESDAY, FEBRUARY 18, 2020",
              " 10.25.48 JOB00134  $HASP375 HELLO    ESTIMATED  LINES EXCEEDED",
              " 10.25.48 JOB00134  $HASP373 HELLO    STARTED - INIT 3    - CLASS R        - SYS STL1",
              " 10.25.48 JOB00134  SMF000I  HELLO       STEP0001    IEBGENER    0000",
              " 10.25.48 JOB00134  $HASP395 HELLO    ENDED - RC=0000",
              "0------ JES2 JOB STATISTICS ------",
              "-  18 FEB 2020 JOB EXECUTION DATE",
              "-           16 CARDS READ",
              "-           59 SYSOUT PRINT RECORDS",
              "-            0 SYSOUT PUNCH RECORDS",
              "-            6 SYSOUT SPOOL KBYTES",
              "-         0.00 MINUTES EXECUTION TIME"
            ],
            "dd_name": "JESMSGLG",
            "id": "2",
            "procstep": "",
            "record_count": "17",
            "stepname": "JES2"
          },
          {
            "byte_count": "574",
            "content": [
              "         1 //HELLO    JOB (T043JM,JM00,1,0,0,0),'HELLO WORLD - JRM',CLASS=R,       JOB00134",
              "           //             MSGCLASS=X,MSGLEVEL=1,NOTIFY=S0JM                                ",
              "           //*                                                                             ",
              "           //* PRINT \"HELLO WORLD\" ON JOB OUTPUT                                           ",
              "           //*                                                                             ",
              "           //* NOTE THAT THE EXCLAMATION POINT IS INVALID EBCDIC FOR JCL                   ",
              "           //*   AND WILL CAUSE A JCL ERROR                                                ",
              "           //*                                                                             ",
              "         2 //STEP0001 EXEC PGM=IEBGENER                                                    ",
              "         3 //SYSIN    DD DUMMY                                                             ",
              "         4 //SYSPRINT DD SYSOUT=*                                                          ",
              "         5 //SYSUT1   DD *                                                                 ",
              "         6 //SYSUT2   DD SYSOUT=*                                                          ",
              "         7 //                                                                              "
            ],
            "dd_name": "JESJCL",
            "id": "3",
            "procstep": "",
            "record_count": "14",
            "stepname": "JES2"
          },
          {
            "byte_count": "1066",
            "content": [
              " ICH70001I OMVSADM  LAST ACCESS AT 10:25:47 ON TUESDAY, FEBRUARY 18, 2020",
              " IEF236I ALLOC. FOR HELLO STEP0001",
              " IEF237I DMY  ALLOCATED TO SYSIN",
              " IEF237I JES2 ALLOCATED TO SYSPRINT",
              " IEF237I JES2 ALLOCATED TO SYSUT1",
              " IEF237I JES2 ALLOCATED TO SYSUT2",
              " IEF142I HELLO STEP0001 - STEP WAS EXECUTED - COND CODE 0000",
              " IEF285I   OMVSADM.HELLO.JOB00134.D0000102.?            SYSOUT        ",
              " IEF285I   OMVSADM.HELLO.JOB00134.D0000101.?            SYSIN         ",
              " IEF285I   OMVSADM.HELLO.JOB00134.D0000103.?            SYSOUT        ",
              " IEF373I STEP/STEP0001/START 2020049.1025",
              " IEF032I STEP/STEP0001/STOP  2020049.1025 ",
              "         CPU:     0 HR  00 MIN  00.00 SEC    SRB:     0 HR  00 MIN  00.00 SEC    ",
              "         VIRT:    60K  SYS:   240K  EXT:        0K  SYS:    11548K",
              "         ATB- REAL:                     8K  SLOTS:                     0K",
              "              VIRT- ALLOC:      10M SHRD:       0M",
              " IEF375I  JOB/HELLO   /START 2020049.1025",
              " IEF033I  JOB/HELLO   /STOP  2020049.1025 ",
              "         CPU:     0 HR  00 MIN  00.00 SEC    SRB:     0 HR  00 MIN  00.00 SEC    "
            ],
            "dd_name": "JESYSMSG",
            "id": "4",
            "procstep": "",
            "record_count": "19",
            "stepname": "JES2"
          },
          {
            "byte_count": "251",
            "content": [
              "1DATA SET UTILITY - GENERATE                                                                       PAGE 0001             ",
              "-IEB352I WARNING: ONE OR MORE OF THE OUTPUT DCB PARMS COPIED FROM INPUT                                                  ",
              "                                                                                                                         ",
              " PROCESSING ENDED AT EOD                                                                                                 "
            ],
            "dd_name": "SYSPRINT",
            "id": "102",
            "procstep": "",
            "record_count": "4",
            "stepname": "STEP0001"
          },
          {
            "byte_count": "49",
            "content": [
              " HELLO, WORLD                                                                    "
            ],
            "dd_name": "SYSUT2",
            "id": "103",
            "procstep": "",
            "record_count": "1",
            "stepname": "STEP0001"
          }
        ],
        "duration": 0,
        "execution_time": "00:00:03",
        "job_class": "R",
        "job_id": "JOB00134",
        "job_name": "HELLO",
        "priority": "1",
        "program_name": "IEBGENER",
        "queue_position": "58",
        "owner": "OMVSADM",
        "ret_code": {
          "code": 0,
          "msg": "CC 0000",
          "msg_code": "0000",
          "msg_txt": "",
        },
        "steps": [
          { "step_name": "STEP0001",
            "step_cc": 0
          }
        ],
        "system": "STL1",
        "subsystem": "STL1",
        "cpu_time": 1414,
        "execution_node": "STL1",
        "origin_node": "STL1"
      }
  ]
changed:
    description:
      Indicates if any changes were made during module operation
    type: bool
    returned: always
"""


from ansible.module_utils.basic import AnsibleModule
import traceback
from ansible_collections.ibm.ibm_zos_core.plugins.module_utils.import_handler import (
    ZOAUImportError,
)
from ansible_collections.ibm.ibm_zos_core.plugins.module_utils.job import (
    job_output,
)
from ansible_collections.ibm.ibm_zos_core.plugins.module_utils import (
    better_arg_parser
)
from ansible_collections.ibm.ibm_zos_core.plugins.module_utils.dependency_checker import (
    validate_dependencies,
)
from ansible_collections.ibm.ibm_zos_core.plugins.module_utils.log import SingletonLogger

try:
    from zoautil_py import exceptions as zoau_exceptions
except Exception:
    zoau_exceptions = ZOAUImportError(traceback.format_exc())


def run_module():
    """Initialize module.

    Raises
    ------
    fail_json
        Parameter verification failed.
    fail_json
        job_id or job_name or owner not provided.
    fail_json
        ZOAU exception.
    fail_json
        Any exception while fetching jobs.
    """
    module_args = dict(
        job_id=dict(type="str", required=False),
        job_name=dict(type="str", required=False),
        owner=dict(type="str", required=False),
        dd_name=dict(type="str", required=False, aliases=['ddname']),
        sysin_dd=dict(type="bool", required=False, default=False),
    )

    module = AnsibleModule(argument_spec=module_args, supports_check_mode=True)
    # Validate dependencies
    validate_dependencies(module)
    args_def = dict(
        job_id=dict(type="job_identifier", required=False),
        job_name=dict(type="job_identifier", required=False),
        owner=dict(type="str", required=False),
        dd_name=dict(
            type="str",
            required=False,
            aliases=['ddname'],
        ),
        sysin_dd=dict(type="bool", required=False, default=False),
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

    results = {}
    results["changed"] = False

    job_id = module.params.get("job_id")
    job_name = module.params.get("job_name")
    owner = module.params.get("owner")
    dd_name = module.params.get("dd_name")
    sysin = module.params.get("sysin_dd")

    if not job_id and not job_name and not owner:
        module.fail_json(msg="Please provide a job_id or job_name or owner", stderr="", **results)

    try:
        results = {}
        results["jobs"] = job_output(job_id=job_id, owner=owner, job_name=job_name, dd_name=dd_name, sysin=sysin)
        for job in results["jobs"]:
            if "job_not_found" in job:
                results["changed"] = False
                del job['job_not_found']
            else:
                results["changed"] = True
    except zoau_exceptions.JobFetchException as fetch_exception:
        module.fail_json(
            msg=f"ZOAU exception {fetch_exception.response.stdout_response} rc {fetch_exception.response.rc}",
            stderr=fetch_exception.response.stderr_response,
            changed=False
        )
    except Exception as e:
        module.fail_json(msg=repr(e), **results)

    module.exit_json(**results)


def main():
    run_module()


if __name__ == "__main__":
    main()
