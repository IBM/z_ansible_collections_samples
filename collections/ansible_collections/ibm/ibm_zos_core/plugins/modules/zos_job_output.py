#!/usr/bin/python
# -*- coding: utf-8 -*-

# Copyright (c) IBM Corporation 2019, 2020
# Apache License, Version 2.0 (see https://opensource.org/licenses/Apache-2.0)

from __future__ import absolute_import, division, print_function

__metaclass__ = type

ANSIBLE_METADATA = {
    "metadata_version": "1.1",
    "status": ["stableinterface"],
    "supported_by": "community",
}

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
  - If there is no ddname, or if ddname="?", output of all the ddnames under
    the given job will be displayed.
version_added: "2.9"
author: "Jack Ho (@jacklotusho)"
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
  ddname:
    description:
      - Data definition name. (e.g "JESJCL", "?")
    type: str
    required: false
"""

EXAMPLES = r"""
- name: Job output with ddname
  zos_job_output:
    job_id: "STC02560"
    ddname: "JESMSGLG"

- name: JES Job output without ddname
  zos_job_output:
    job_id: "STC02560"

- name: JES Job output with all ddnames
  zos_job_output:
    job_id: "STC*"
    job_name: "*"
    owner: "IBMUSER"
    ddname: "?"
"""

RETURN = r"""
jobs:
  description:
      List of jobs output.
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
    subsystem:
      description:
         The job entry subsystem that MVS uses to do work.
      type: str
      sample: STL1
    class:
      description:
         Identifies the data set used in a system output data set, usually called a sysout data set.
      type: str
      sample:
    content_type:
      description:
         Type of address space.
      type: str
      sample: JOB
    ddnames:
      description:
         Data definition names.
      type: list
      elements: dict
      contains:
        ddname:
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
             The ddname content.
          type: list[str]
          sample:
             [ "         1 //HELLO    JOB (T043JM,JM00,1,0,0,0),'HELLO WORLD - JRM',CLASS=R,       JOB00134",
               "           //             MSGCLASS=X,MSGLEVEL=1,NOTIFY=S0JM                                ",
               "           //*                                                                             ",
               "           //* PRINT \"HELLO WORLD\" ON JOB OUTPUT                                          ",
               "           //*                                                                             ",
               "           //* NOTE THAT THE EXCLAMATION POINT IS INVALID EBCDIC FOR JCL                   ",
               "           //*   AND WILL CAUSE A JCL ERROR                                                ",
               "           //*                                                                             ",
               "         2 //STEP0001 EXEC PGM=IEBGENER                                                    ",
               "         3 //SYSIN    DD DUMMY                                                             ",
               "         4 //SYSPRINT DD SYSOUT=*                                                          ",
               "         5 //SYSUT1   DD *                                                                 ",
               "         6 //SYSUT2   DD SYSOUT=*                                                          ",
               "         7 //                                                                              "
             ]
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
            Return code extracted from the `msg` so that it can better
            evaluated. For example , ABEND(S0C4) would yield ""S0C4".
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
         - "code": 0
         -  "msg": "CC 0000"
         - "msg_code": "0000"
         - "msg_txt": ""
  sample:
     [
      {
        "class": "R",
        "content_type": "JOB",
        "ddnames": [
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
            "ddname": "JESMSGLG",
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
            "ddname": "JESJCL",
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
            "ddname": "JESYSMSG",
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
            "ddname": "SYSPRINT",
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
            "ddname": "SYSUT2",
            "id": "103",
            "procstep": "",
            "record_count": "1",
            "stepname": "STEP0001"
          }
        ],
        "job_id": "JOB00134",
        "job_name": "HELLO",
        "owner": "OMVSADM",
        "ret_code": {
          "code": 0,
          "msg": "CC 0000",
          "msg_code": "0000",
          "msg_txt": ""
        },
        "subsystem": "STL1"
      }
  ]
changed:
    description:
      Indicates if any changes were made during module operation
    type: bool
    returned: on success
"""


from ansible.module_utils.basic import AnsibleModule
from ansible_collections.ibm.ibm_zos_core.plugins.module_utils.job import job_output
from tempfile import NamedTemporaryFile


def run_module():
    module_args = dict(
        job_id=dict(type="str", required=False),
        job_name=dict(type="str", required=False),
        owner=dict(type="str", required=False),
        ddname=dict(type="str", required=False),
    )

    module = AnsibleModule(argument_spec=module_args, supports_check_mode=True)

    job_id = module.params.get("job_id")
    job_name = module.params.get("job_name")
    owner = module.params.get("owner")
    ddname = module.params.get("ddname")

    if not job_id and not job_name and not owner:
        module.fail_json(msg="Please provide a job_id or job_name or owner")

    try:
        results = {}
        results["jobs"] = job_output(job_id, owner, job_name, ddname)
        results["changed"] = False
    except Exception as e:
        module.fail_json(msg=repr(e))
    module.exit_json(**results)


def main():
    run_module()


if __name__ == "__main__":
    main()
