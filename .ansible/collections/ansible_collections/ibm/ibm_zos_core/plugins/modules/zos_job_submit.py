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
module: zos_job_submit
author:
    - "Xiao Yuan Ma (@bjmaxy)"
    - "Rich Parker (@richp405)"
    - "Demetrios Dimatos (@ddimatos)"
    - "Ivan Moreno (@rexemin)"
short_description: Submit JCL
description:
    - Submit JCL in a data set, USS file, or file on the controller.
    - Submit a job and monitor for completion.
    - For an uncataloged dataset, specify the volume serial number.
version_added: "1.0.0"
options:
  src:
    required: true
    type: str
    description:
      - The source file or data set containing the JCL to submit.
      - It could be a physical sequential data set, a partitioned data set
        qualified by a member or a path (e.g. C(USER.TEST), ``USER.JCL(TEST)``),
        or a generation data set from a generation data group
        (for example, ``USER.TEST.GDG(-2)``).
      - Or a USS file. (e.g C(/u/tester/demo/sample.jcl))
      - Or a LOCAL file in ansible control node.
        (e.g C(/User/tester/ansible-playbook/sample.jcl))
      - When using a generation data set, only already created generations
        are valid. If either the relative name is positive, or negative but
        not found, the module will fail.
  remote_src:
    required: false
    default: true
    type: bool
    description:
      - If set to C(false), the module searches for C(src) in the controller node.
      - If set to C(true), the module searches for the file C(src) in the managed node.
  wait_time:
    required: false
    default: 10
    type: int
    description:
      - Option I(wait_time) is the total time that module
        L(zos_job_submit,./zos_job_submit.html) will wait for a submitted job
        to complete. The time begins when the module is executed on the managed
        node.
      - I(wait_time) is measured in seconds and must be a value greater than 0
        and less than 86400.
      - The module can submit and forget jobs by setting I(wait_time) to 0. This way the
        module will not try to retrieve the job details other than job id.
        Job details and contents can be retrieved later by using
        L(zos_job_query,./zos_job_query.html) or L(zos_job_output,./zos_job_output.html)
        if needed.
      - If I(remote_src=False) and I(wait_time=0), the module will not clean the copy
        of the file on the remote system, to avoid problems with job submission.
  max_rc:
    required: false
    type: int
    description:
      - Specifies the maximum return code allowed for any job step for the submitted job.
  return_output:
    required: false
    default: true
    type: bool
    description:
      - Whether to print the DD output.
      - If false, an empty list will be returned in the dds field.
  volume:
    required: false
    type: str
    description:
      - The volume serial (VOLSER) is where the data set resides. The option
        is required only when the data set is not cataloged on the system.
      - When configured, the L(zos_job_submit,./zos_job_submit.html) will try to
        catalog the data set for the volume serial. If it is not able to, the
        module will fail.
      - Ignored for I(remote_src=False).
  encoding:
    description:
      - Specifies which encoding the local JCL file should be converted from
        and to, before submitting the job.
      - This option is only supported for when I(remote_src=False).
      - If this parameter is not provided, and the z/OS systems default encoding
        can not be identified, the JCL file will be converted from UTF-8 to
        IBM-1047 by default, otherwise the module will detect the z/OS system
        encoding.
    required: false
    type: dict
    suboptions:
      from:
        description:
          - The character set of the local JCL file; defaults to UTF-8.
          - Supported character sets rely on the target platform; the most
            common character sets are supported.
        required: false
        type: str
        default: UTF-8
      to:
        description:
          - The character set to convert the local JCL file to on the remote
            z/OS system; defaults to IBM-1047 when z/OS systems default encoding
            can not be identified.
          - If not provided, the module will attempt to identify and use the
            default encoding on the z/OS system.
          - Supported character sets rely on the target version; the most
            common character sets are supported.
        required: false
        type: str
        default: IBM-1047

extends_documentation_fragment:
  - ibm.ibm_zos_core.template

attributes:
  action:
    support: full
    description: Indicates this has a corresponding action plugin so some parts of the options can be executed on the controller.
  async:
    support: full
    description: Supports being used with the ``async`` keyword.
  check_mode:
    support: full
    description: Can run in check_mode and return changed status prediction without modifying target. If not supported, the action will be skipped.

notes:
  - For supported character sets used to encode data, refer to the
    L(documentation,https://ibm.github.io/z_ansible_collections_doc/ibm_zos_core/docs/source/resources/character_set.html).
  - This module uses L(zos_copy,./zos_copy.html) to copy local scripts to
    the remote machine which uses SFTP (Secure File Transfer Protocol) for the
    underlying transfer protocol; SCP (secure copy protocol) and Co:Z SFTP are not
    supported. In the case of Co:z SFTP, you can exempt the Ansible user id on z/OS
    from using Co:Z thus falling back to using standard SFTP. If the module detects
    SCP, it will temporarily use SFTP for transfers, if not available, the module
    will fail.
"""

RETURN = r"""
jobs:
  description:
     List of jobs output.
     If no job status is found, this will return an empty ret_code with msg_txt explanation.
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
    duration:
      description: The total lapsed time the JCL ran for.
      type: int
      sample: 0
    execution_time:
      description: Total duration time of the job execution, if it has finished.
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
    ret_code:
      description:
         Return code output collected from the job log.
      type: dict
      contains:
        msg:
          description:
            - Job status resulting from the job submission.
            - Job status `ABEND` indicates the job ended abnormally.
            - Job status `AC` indicates the job is active, often a started task or job taking long.
            - Job status `CAB` indicates a converter abend.
            - Job status `CANCELED` indicates the job was canceled.
            - Job status `CNV` indicates a converter error.
            - Job status `FLU` indicates the job was flushed.
            - Job status `JCLERR` or `JCL ERROR` indicates the JCL has an error.
            - Job status `SEC` or `SEC ERROR` indicates the job as encountered a security error.
            - Job status `SYS` indicates a system failure.
            - Job status `?` indicates status can not be determined.
            - Job status `TYPRUN=SCAN` indicates that the job had the TYPRUN parameter with SCAN option.
            - Job status `TYPRUN=COPY` indicates that the job had the TYPRUN parameter with COPY option.
            - Job status `HOLD` indicates that the job had the TYPRUN parameter with either the HOLD or JCLHOLD options.
            - Jobs where status can not be determined will result in None (NULL).
          type: str
          sample: AC
        msg_code:
          description:
            - The return code from the submitted job as a string.
            - Jobs which have no return code will result in None (NULL), such
              is the case of a job that errors or is active.
          type: str
          sample: 0000
        msg_txt:
          description:
            - Returns additional information related to the submitted job.
            - Jobs which have no additional information will result in None (NULL).
          type: str
          sample: The job JOB00551 was run with special job processing TYPRUN=SCAN.
                  This will result in no completion, return code or job steps and
                  changed will be false.
        code:
          description:
            - The return code converted to an integer value when available.
            - Jobs which have no return code will result in None (NULL), such
              is the case of a job that errors or is active.
          type: int
          sample: 0
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
  sample:
     [
          {
              "content_type": "JOB",
              "dds": [
                  {
                      "byte_count": "677",
                      "content": [
                          "1                       J E S 2  J O B  L O G  --  S Y S T E M  S T L 1  --  N O D E  S T L 1            ",
                          "0 ",
                          " 12.50.08 JOB00361 ---- FRIDAY,    13 MAR 2020 ----",
                          " 12.50.08 JOB00361  IRR010I  USERID OMVSADM  IS ASSIGNED TO THIS JOB.",
                          " 12.50.08 JOB00361  ICH70001I OMVSADM  LAST ACCESS AT 12:50:03 ON FRIDAY, MARCH 13, 2020",
                          " 12.50.08 JOB00361  $HASP373 DBDGEN00 STARTED - INIT 15   - CLASS K        - SYS STL1",
                          " 12.50.08 JOB00361  SMF000I  DBDGEN00    C           ASMA90      0000",
                          " 12.50.09 JOB00361  SMF000I  DBDGEN00    L           IEWL        0000",
                          " 12.50.09 JOB00361  $HASP395 DBDGEN00 ENDED - RC=0000",
                          "0------ JES2 JOB STATISTICS ------",
                          "-  13 MAR 2020 JOB EXECUTION DATE",
                          "-           28 CARDS READ",
                          "-          158 SYSOUT PRINT RECORDS",
                          "-            0 SYSOUT PUNCH RECORDS",
                          "-           12 SYSOUT SPOOL KBYTES",
                          "-         0.00 MINUTES EXECUTION TIME"
                      ],
                      "dd_name": "JESMSGLG",
                      "id": "2",
                      "procstep": "",
                      "record_count": "16",
                      "stepname": "JES2"
                  },
                  {
                      "byte_count": "2136",
                      "content": [
                          "         1 //DBDGEN00 JOB MSGLEVEL=1,MSGCLASS=E,CLASS=K,                           JOB00361",
                          "           //   LINES=999999,TIME=1440,REGION=0M,                                          ",
                          "           //   MEMLIMIT=NOLIMIT                                                           ",
                          "         2 /*JOBPARM  SYSAFF=*                                                             ",
                          "           //*                                                                             ",
                          "         3 //DBDGEN   PROC MBR=TEMPNAME                                                    ",
                          "           //C        EXEC PGM=ASMA90,                                                     ",
                          "           //             PARM='OBJECT,NODECK,NOLIST'                                      ",
                          "           //SYSLIB   DD DISP=SHR,                                                         ",
                          "           //      DSN=IMSBLD.I15RTSMM.SDFSMAC                                             ",
                          "           //SYSLIN   DD DISP=(NEW,PASS),RECFM=F,LRECL=80,BLKSIZE=80,                      ",
                          "           //         UNIT=SYSDA,SPACE=(CYL,(10,5),RLSE,,)                                 ",
                          "           //SYSUT1   DD DISP=(NEW,DELETE),UNIT=SYSDA,SPACE=(CYL,                          ",
                          "           //         (10,5),,,)                                                           ",
                          "           //SYSPRINT DD SYSOUT=*                                                          ",
                          "           //L        EXEC PGM=IEWL,                                                       ",
                          "           //             PARM='XREF,NOLIST',                                              ",
                          "           //             COND=(0,LT,C)                                                    ",
                          "           //SYSLMOD  DD DISP=SHR,                                                         ",
                          "           //      DSN=IMSTESTL.IMS1.DBDLIB(&MBR)                                          ",
                          "           //SYSLIN   DD DSN=*.C.SYSLIN,DISP=(OLD,DELETE)                                  ",
                          "           //SYSPRINT DD SYSOUT=*                                                          ",
                          "           //*                                                                             ",
                          "           //         PEND                                                                 ",
                          "         4 //DLORD6   EXEC DBDGEN,                                                         ",
                          "           //             MBR=DLORD6                                                       ",
                          "         5 ++DBDGEN   PROC MBR=TEMPNAME                                                    ",
                          "         6 ++C        EXEC PGM=ASMA90,                                                     ",
                          "           ++             PARM='OBJECT,NODECK,NOLIST'                                      ",
                          "         7 ++SYSLIB   DD DISP=SHR,                                                         ",
                          "           ++      DSN=IMSBLD.I15RTSMM.SDFSMAC                                             ",
                          "         8 ++SYSLIN   DD DISP=(NEW,PASS),RECFM=F,LRECL=80,BLKSIZE=80,                      ",
                          "           ++         UNIT=SYSDA,SPACE=(CYL,(10,5),RLSE,,)                                 ",
                          "         9 ++SYSUT1   DD DISP=(NEW,DELETE),UNIT=SYSDA,SPACE=(CYL,                          ",
                          "           ++         (10,5),,,)                                                           ",
                          "        10 ++SYSPRINT DD SYSOUT=*                                                          ",
                          "        11 //SYSIN    DD DISP=SHR,                                                         ",
                          "           //      DSN=IMSTESTL.IMS1.DBDSRC(DLORD6)                                        ",
                          "        12 ++L        EXEC PGM=IEWL,                                                       ",
                          "           ++             PARM='XREF,NOLIST',                                              ",
                          "           ++             COND=(0,LT,C)                                                    ",
                          "        13 ++SYSLMOD  DD DISP=SHR,                                                         ",
                          "           ++      DSN=IMSTESTL.IMS1.DBDLIB(&MBR)                                          ",
                          "           IEFC653I SUBSTITUTION JCL - DISP=SHR,DSN=IMSTESTL.IMS1.DBDLIB(DLORD6)",
                          "        14 ++SYSLIN   DD DSN=*.C.SYSLIN,DISP=(OLD,DELETE)                                  ",
                          "        15 ++SYSPRINT DD SYSOUT=*                                                          ",
                          "           ++*                                                                             "
                      ],
                      "dd_name": "JESJCL",
                      "id": "3",
                      "procstep": "",
                      "record_count": "47",
                      "stepname": "JES2"
                  },
                  {
                      "byte_count": "2414",
                      "content": [
                          "  STMT NO. MESSAGE",
                          "         4 IEFC001I PROCEDURE DBDGEN WAS EXPANDED USING INSTREAM PROCEDURE DEFINITION",
                          " ICH70001I OMVSADM  LAST ACCESS AT 12:50:03 ON FRIDAY, MARCH 13, 2020",
                          " IEF236I ALLOC. FOR DBDGEN00 C DLORD6",
                          " IEF237I 083C ALLOCATED TO SYSLIB",
                          " IGD100I 0940 ALLOCATED TO DDNAME SYSLIN   DATACLAS (        )",
                          " IGD100I 0942 ALLOCATED TO DDNAME SYSUT1   DATACLAS (        )",
                          " IEF237I JES2 ALLOCATED TO SYSPRINT",
                          " IEF237I 01A0 ALLOCATED TO SYSIN",
                          " IEF142I DBDGEN00 C DLORD6 - STEP WAS EXECUTED - COND CODE 0000",
                          " IEF285I   IMSBLD.I15RTSMM.SDFSMAC                      KEPT          ",
                          " IEF285I   VOL SER NOS= IMSBG2.                            ",
                          " IEF285I   SYS20073.T125008.RA000.DBDGEN00.R0101894     PASSED        ",
                          " IEF285I   VOL SER NOS= 000000.                            ",
                          " IEF285I   SYS20073.T125008.RA000.DBDGEN00.R0101895     DELETED       ",
                          " IEF285I   VOL SER NOS= 333333.                            ",
                          " IEF285I   OMVSADM.DBDGEN00.JOB00361.D0000101.?         SYSOUT        ",
                          " IEF285I   IMSTESTL.IMS1.DBDSRC                         KEPT          ",
                          " IEF285I   VOL SER NOS= USER03.                            ",
                          " IEF373I STEP/C       /START 2020073.1250",
                          " IEF032I STEP/C       /STOP  2020073.1250 ",
                          "         CPU:     0 HR  00 MIN  00.03 SEC    SRB:     0 HR  00 MIN  00.00 SEC    ",
                          "         VIRT:   252K  SYS:   240K  EXT:  1876480K  SYS:    11896K",
                          "         ATB- REAL:                  1048K  SLOTS:                     0K",
                          "              VIRT- ALLOC:      14M SHRD:       0M",
                          " IEF236I ALLOC. FOR DBDGEN00 L DLORD6",
                          " IEF237I 01A0 ALLOCATED TO SYSLMOD",
                          " IEF237I 0940 ALLOCATED TO SYSLIN",
                          " IEF237I JES2 ALLOCATED TO SYSPRINT",
                          " IEF142I DBDGEN00 L DLORD6 - STEP WAS EXECUTED - COND CODE 0000",
                          " IEF285I   IMSTESTL.IMS1.DBDLIB                         KEPT          ",
                          " IEF285I   VOL SER NOS= USER03.                            ",
                          " IEF285I   SYS20073.T125008.RA000.DBDGEN00.R0101894     DELETED       ",
                          " IEF285I   VOL SER NOS= 000000.                            ",
                          " IEF285I   OMVSADM.DBDGEN00.JOB00361.D0000102.?         SYSOUT        ",
                          " IEF373I STEP/L       /START 2020073.1250",
                          " IEF032I STEP/L       /STOP  2020073.1250 ",
                          "         CPU:     0 HR  00 MIN  00.00 SEC    SRB:     0 HR  00 MIN  00.00 SEC    ",
                          "         VIRT:    92K  SYS:   256K  EXT:     1768K  SYS:    11740K",
                          "         ATB- REAL:                  1036K  SLOTS:                     0K",
                          "              VIRT- ALLOC:      11M SHRD:       0M",
                          " IEF375I  JOB/DBDGEN00/START 2020073.1250",
                          " IEF033I  JOB/DBDGEN00/STOP  2020073.1250 ",
                          "         CPU:     0 HR  00 MIN  00.03 SEC    SRB:     0 HR  00 MIN  00.00 SEC    "
                      ],
                      "dd_name": "JESYSMSG",
                      "id": "4",
                      "procstep": "",
                      "record_count": "44",
                      "stepname": "JES2"
                  },
                  {
                      "byte_count": "1896",
                      "content": [
                          "1z/OS V2 R2 BINDER     12:50:08 FRIDAY MARCH 13, 2020                                                                    ",
                          " BATCH EMULATOR  JOB(DBDGEN00) STEP(DLORD6  ) PGM= IEWL      PROCEDURE(L       )                                         ",
                          " IEW2278I B352 INVOCATION PARAMETERS - XREF,NOLIST                                                                       ",
                          " IEW2650I 5102 MODULE ENTRY NOT PROVIDED.  ENTRY DEFAULTS TO SECTION DLORD6.                                             ",
                          "                                                                                                                         ",
                          "                                                                                                                         ",
                          "1                                       C R O S S - R E F E R E N C E  T A B L E                                         ",
                          "                                        _________________________________________                                        ",
                          "                                                                                                                         ",
                          " TEXT CLASS = B_TEXT                                                                                                     ",
                          "                                                                                                                         ",
                          " ---------------  R E F E R E N C E  --------------------------  T A R G E T  -------------------------------------------",
                          "   CLASS                            ELEMENT       |                                            ELEMENT                  |",
                          "   OFFSET SECT/PART(ABBREV)          OFFSET  TYPE | SYMBOL(ABBREV)   SECTION (ABBREV)           OFFSET CLASS NAME       |",
                          "                                                  |                                                                     |",
                          "                                        *** E N D  O F  C R O S S  R E F E R E N C E ***                                 ",
                          "1z/OS V2 R2 BINDER     12:50:08 FRIDAY MARCH 13, 2020                                                                    ",
                          " BATCH EMULATOR  JOB(DBDGEN00) STEP(DLORD6  ) PGM= IEWL      PROCEDURE(L       )                                         ",
                          " IEW2850I F920 DLORD6 HAS BEEN SAVED WITH AMODE  24 AND RMODE    24.  ENTRY POINT NAME IS DLORD6.                        ",
                          " IEW2231I 0481 END OF SAVE PROCESSING.                                                                                   ",
                          " IEW2008I 0F03 PROCESSING COMPLETED.  RETURN CODE =  0.                                                                  ",
                          "                                                                                                                         ",
                          "                                                                                                                         ",
                          "                                                                                                                         ",
                          "1----------------------                                                                                                  ",
                          " MESSAGE SUMMARY REPORT                                                                                                  ",
                          " ----------------------                                                                                                  ",
                          "  TERMINAL MESSAGES      (SEVERITY = 16)                                                                                 ",
                          "  NONE                                                                                                                   ",
                          "                                                                                                                         ",
                          "  SEVERE MESSAGES        (SEVERITY = 12)                                                                                 ",
                          "  NONE                                                                                                                   ",
                          "                                                                                                                         ",
                          "  ERROR MESSAGES         (SEVERITY = 08)                                                                                 ",
                          "  NONE                                                                                                                   ",
                          "                                                                                                                         ",
                          "  WARNING MESSAGES       (SEVERITY = 04)                                                                                 ",
                          "  NONE                                                                                                                   ",
                          "                                                                                                                         ",
                          "  INFORMATIONAL MESSAGES (SEVERITY = 00)                                                                                 ",
                          "  2008  2231  2278  2650  2850                                                                                           ",
                          "                                                                                                                         ",
                          "                                                                                                                         ",
                          "  **** END OF MESSAGE SUMMARY REPORT ****                                                                                ",
                          "                                                                                                                         "
                      ],
                      "dd_name": "SYSPRINT",
                      "id": "102",
                      "procstep": "L",
                      "record_count": "45",
                      "stepname": "DLORD6"
                  }
              ],
              "job_id": "JOB00361",
              "job_name": "DBDGEN00",
              "ret_code": {
                  "code": 0,
                  "msg": "CC 0000",
                  "msg_code": "0000",
                  "msg_txt": "",
              },
              "steps": [
                { "step_name": "DLORD6",
                  "step_cc": 0
                }
              ],
              "job_class": "K",
              "execution_time": "00:00:10",
              "svc_class": "?",
              "priority": 1,
              "program_name": "IEBGENER",
              "asid": 0,
              "creation_date": "2023-05-03",
              "creation_time": "12:13:00",
              "queue_position": 3,
              "subsystem": "STL1",
              "system": "STL1",
              "cpu_time": 1,
              "execution_node": "STL1",
              "origin_node": "STL1"
          }
     ]
"""

EXAMPLES = r"""
- name: Submit JCL in a PDSE member.
  zos_job_submit:
    src: HLQ.DATA.LLQ(SAMPLE)
    remote_src: true
  register: response

- name: Submit JCL in USS with no DDs in the output.
  zos_job_submit:
    src: /u/tester/demo/sample.jcl
    remote_src: true
    return_output: false

- name: Convert local JCL to IBM-037 and submit the job.
  zos_job_submit:
    src: /Users/maxy/ansible-playbooks/provision/sample.jcl
    remote_src: false
    encoding:
      from: ISO8859-1
      to: IBM-037

- name: Submit JCL in an uncataloged PDSE on volume P2SS01.
  zos_job_submit:
    src: HLQ.DATA.LLQ(SAMPLE)
    remote_src: true
    volume: P2SS01

- name: Submit a long running PDS job and wait up to 30 seconds for completion.
  zos_job_submit:
    src: HLQ.DATA.LLQ(LONGRUN)
    remote_src: true
    wait_time: 30

- name: Submit a long running PDS job and wait up to 30 seconds for completion.
  zos_job_submit:
    src: HLQ.DATA.LLQ(LONGRUN)
    remote_src: true
    wait_time: 30

- name: Submit JCL and set the max return code the module should fail on to 16.
  zos_job_submit:
    src: HLQ.DATA.LLQ
    remote_src: true
    max_rc: 16

- name: Submit JCL from the latest generation data set in a generation data group.
  zos_job_submit:
    src: HLQ.DATA.GDG(0)
    remote_src: true

- name: Submit JCL from a previous generation data set in a generation data group.
  zos_job_submit:
    src: HLQ.DATA.GDG(-2)
    remote_src: true
"""

from ansible_collections.ibm.ibm_zos_core.plugins.module_utils.encode import (
    Defaults,
)
from ansible_collections.ibm.ibm_zos_core.plugins.module_utils.better_arg_parser import (
    BetterArgParser,
)
from ansible_collections.ibm.ibm_zos_core.plugins.module_utils.job import (
    job_output, search_dictionaries, JOB_ERROR_STATUSES
)
from ansible_collections.ibm.ibm_zos_core.plugins.module_utils.import_handler import (
    ZOAUImportError,
)
from ansible_collections.ibm.ibm_zos_core.plugins.module_utils import (
    data_set,
)
from ansible_collections.ibm.ibm_zos_core.plugins.module_utils.data_set import (
    DataSet,
)
from ansible_collections.ibm.ibm_zos_core.plugins.module_utils.dependency_checker import (
    validate_dependencies,
)
from ansible.module_utils.basic import AnsibleModule
from ansible.module_utils._text import to_text
from timeit import default_timer as timer
from os import path
import shutil
import traceback
from time import sleep
import re
from ansible_collections.ibm.ibm_zos_core.plugins.module_utils.log import SingletonLogger

try:
    from zoautil_py import exceptions as zoau_exceptions
except ImportError:
    zoau_exceptions = ZOAUImportError(traceback.format_exc())

try:
    from zoautil_py import jobs
except Exception:
    jobs = ZOAUImportError(traceback.format_exc())


JOB_STATUSES = list(dict.fromkeys(JOB_ERROR_STATUSES))
JOB_STATUSES.append("CC")

JOB_SPECIAL_PROCESSING = frozenset(["TYPRUN"])
MAX_WAIT_TIME_S = 86400


def submit_src_jcl(module, src, src_name=None, timeout=0, is_unix=True, start_time=timer()):
    """Submit src JCL whether JCL is local (Ansible Controller), USS or in a data set.

        Parameters
        ----------
        module: AnsibleModule
            module instance to access the module api.
        src : str
            JCL, can be relative or absolute paths either on controller or USS
            - Data set, can be PS, PDS, PDSE Member.
        src_name : str
            The src name that was provided in the module because through
            the runtime src could be replace with a temporary file name.
        timeout : int
            How long to wait in seconds for a job to complete.
        is_unix : bool
            True if JCL is a file in USS, otherwise False; Note that all
            JCL local to a controller is transfered to USS thus would be
            True.
        start_time : int
            time the JCL started its submission.

        Returns
        -------
        str
            the JCL job ID returned from submitting a job, else if no
            job submits, None will be returned.
        int
            how long the job ran for in this method.

        Raises
        ------
        fail_json
            Unable to submit job because the data set could not be cataloged on the volume.
        fail_json
            Unable to submit job, the job submission has failed.
        fail_json
            The JCL has been submitted but there was an error while fetching its status.
        fail_json
            The job has been submitted and no job id was returned.
    """

    kwargs = {
        # Since every fetch retry waits for a second before continuing,
        # we can just pass the timeout (also in seconds) to this arg.
        # When timeout is 0 we still want to retry to get the job id.
        "fetch_max_retries": timeout if timeout > 0 else 3,
    }

    duration = 0
    job_submitted = None
    result = {}

    try:
        job_submitted = jobs.submit(src, is_unix=is_unix, **kwargs)

        if timeout > 0:

            # Introducing a sleep to ensure we have the result of job submit carrying the job id.
            while (job_submitted is None and duration <= timeout):
                current_time = timer()
                duration = round(current_time - start_time)
                sleep(0.5)

            # Second sleep is to wait long enough for the job rc to not equal a `?`
            # which is what ZOAU sends back, opitonally we can check the 'status' as
            # that is sent back as `AC` when the job is not complete but the problem
            # with monitoring 'AC' is that STARTED tasks never exit the AC status.
            job_fetched = None
            job_fetch_rc = None
            job_fetch_status = None

            if job_submitted:
                try:
                    job_fetched = jobs.fetch_multiple(job_submitted.job_id)[0]
                    job_fetch_rc = job_fetched.return_code
                    job_fetch_status = job_fetched.status
                except zoau_exceptions.JobFetchException:
                    pass

                # Before moving forward lets ensure our job has completed but if we see
                # status that matches one in JOB_STATUSES, don't wait, let the code
                # drop through and get analyzed in the main as it will scan the job ouput
                # Any match to JOB_STATUSES ends our processing and wait times
                while (job_fetch_status not in JOB_STATUSES and
                        job_fetch_status == 'AC' and
                        ((job_fetch_rc is None or len(job_fetch_rc) == 0 or
                          job_fetch_rc == '?') and duration < timeout)):
                    current_time = timer()
                    duration = round(current_time - start_time)
                    sleep(1)
                    try:
                        job_fetched = jobs.fetch_multiple(job_submitted.job_id)[0]
                        job_fetch_rc = job_fetched.return_code
                        job_fetch_status = job_fetched.status
                    # Allow for jobs that need more time to be fectched to run the wait_time
                    except zoau_exceptions.JobFetchException as err:
                        if duration >= timeout:
                            raise err
                        else:
                            continue

    # ZOAU throws a JobSubmitException when the job sumbission fails thus there is no
    # JCL RC to share with the user, if there is a RC, that will be processed
    # in the job_output parser.
    except zoau_exceptions.JobSubmitException as err:
        result["changed"] = False
        result["failed"] = True
        result["stderr"] = to_text(err)
        result["duration"] = duration
        result["job_id"] = job_submitted.job_id if job_submitted else None
        result["msg"] = ("Unable to submit job {0}, the job submission has failed. "
                         "Without the job id, the error can not be determined. "
                         "Consider using module `zos_job_query` to poll for the "
                         "job by name or review the system log for purged jobs "
                         "resulting from an abend. Standard error may have "
                         "additional information.".format(src_name))
        module.fail_json(**result)

    # ZOAU throws a JobFetchException when it is unable to fetch a job.
    # This could happen while trying to fetch a job still running.
    except zoau_exceptions.JobFetchException as err:
        result["changed"] = False
        result["failed"] = False
        result["stderr"] = to_text(err)
        result["duration"] = duration
        result["job_id"] = job_submitted.job_id
        _msg_detail = "the job with status {0}".format(job_fetch_status) if job_fetch_status else "its status"
        result["msg"] = ("The JCL has been submitted {0} with ID {1} but there was an "
                         "error while fetching {2} within the allocated time of {3} "
                         "seconds. Consider using module zos_job_query to poll for the "
                         "job for more information. Standard error may have additional "
                         "information.".format(src_name, job_submitted.job_id, _msg_detail, str(timeout)))
        module.fail_json(**result)

    # Between getting a job_submitted and the jobs.fetch_multiple(job_submitted.job_id)[0].return_code
    # is enough time for the system to purge an invalid job, so catch it and let
    # it fall through to the catchall.
    except IndexError:
        job_submitted = None

    # There appears to be a small fraction of time when ZOAU has a handle on the
    # job and suddenly its purged, this check is to ensure the job is there
    # long after the purge else we throw an error here if its been purged.
    if job_submitted is None:
        result["changed"] = False
        result["failed"] = True
        result["duration"] = duration
        result["job_id"] = job_submitted.job_id if job_submitted else None
        result["msg"] = ("The job {0} has been submitted and no job id was returned "
                         "within the allocated time of {1} seconds. Without the "
                         "job id, the error can not be determined, consider using "
                         "module `zos_job_query` to poll for the job by name or "
                         "review the system log for purged jobs resulting from an "
                         "abend.".format(src_name, str(timeout)))
        module.fail_json(**result)

    return job_submitted.job_id if job_submitted else None, duration


def run_module():
    """Initialize module.

    Raises
    ------
    fail_json
        Parameter verification failed.
    fail_json
        The value for option 'wait_time' is not valid.
    """
    module_args = dict(
        src=dict(type="str", required=True),
        remote_src=dict(
            type="bool",
            default=True,
            required=False
        ),
        encoding=dict(
            type="dict",
            required=False,
            options={
                "from": dict(
                    type="str",
                    required=False,
                    default=Defaults.DEFAULT_ASCII_CHARSET
                ),
                "to": dict(
                    type="str",
                    required=False,
                    default=Defaults.DEFAULT_EBCDIC_USS_CHARSET
                )
            }
        ),
        volume=dict(type="str", required=False),
        return_output=dict(type="bool", required=False, default=True),
        wait_time=dict(type="int", default=10),
        max_rc=dict(type="int", required=False),
        use_template=dict(type='bool', default=False),
        template_parameters=dict(
            type='dict',
            required=False,
            options=dict(
                variable_start_string=dict(type='str', default='{{'),
                variable_end_string=dict(type='str', default='}}'),
                block_start_string=dict(type='str', default='{%'),
                block_end_string=dict(type='str', default='%}'),
                comment_start_string=dict(type='str', default='{#'),
                comment_end_string=dict(type='str', default='#}'),
                line_statement_prefix=dict(type='str', required=False),
                line_comment_prefix=dict(type='str', required=False),
                lstrip_blocks=dict(type='bool', default=False),
                trim_blocks=dict(type='bool', default=True),
                keep_trailing_newline=dict(type='bool', default=False),
                newline_sequence=dict(
                    type='str',
                    default='\n',
                    choices=['\n', '\r', '\r\n']
                ),
                auto_reload=dict(type='bool', default=False),
                autoescape=dict(type='bool', default=True),
            )
        ),
    )

    module = AnsibleModule(argument_spec=module_args, supports_check_mode=True)
    validate_dependencies(module)

    if module.params.get("encoding"):
        module.params.update(
            dict(
                from_encoding=module.params.get("encoding").get("from"),
                to_encoding=module.params.get("encoding").get("to"),
            )
        )
    else:
        module.params.update(
            dict(
                from_encoding=Defaults.DEFAULT_ASCII_CHARSET,
                to_encoding=Defaults.get_default_system_charset(),
            )
        )

    arg_defs = dict(
        src=dict(arg_type="data_set_or_path", required=True),
        remote_src=dict(
            arg_type="bool",
            default=True,
            required=False
        ),
        from_encoding=dict(
            arg_type="encoding", default=Defaults.DEFAULT_ASCII_CHARSET, required=False),
        to_encoding=dict(
            arg_type="encoding", default=Defaults.get_default_system_charset(), required=False
        ),
        volume=dict(arg_type="volume", required=False),
        return_output=dict(arg_type="bool", default=True),
        wait_time=dict(arg_type="int", required=False, default=10),
        max_rc=dict(arg_type="int", required=False),
    )

    # ********************************************************************
    # Verify the validity of module args. BetterArgParser raises ValueError
    # when a parameter fails its validation check
    # ********************************************************************
    try:
        parser = BetterArgParser(arg_defs)
        parsed_args = parser.parse_args(module.params)
    except ValueError as err:
        module.fail_json(
            msg="Parameter verification failed", stderr=str(err))

    # Initialize logging module
    module_verbosity_level = module._verbosity
    SingletonLogger().get_logger(module_verbosity_level)

    # Extract values from set module options
    remote_src = parsed_args.get("remote_src")
    volume = parsed_args.get("volume")
    src = parsed_args.get("src")
    return_output = parsed_args.get("return_output")
    wait_time = parsed_args.get("wait_time")
    max_rc = parsed_args.get("max_rc")
    temp_file = parsed_args.get("src") if not remote_src else None

    # Default 'changed' is False in case the module is not able to execute
    result = dict(changed=False)
    # Builds return value schema to make sure we return the return values schema.

    if wait_time < 0 or wait_time > MAX_WAIT_TIME_S:
        result["failed"] = True
        result["msg"] = ("The value for option 'wait_time' is not valid, it must "
                         "be greater than 0 and less than {0}.".format(str(MAX_WAIT_TIME_S)))
        module.fail_json(**result)

    job_submitted_id = None
    duration = 0
    start_time = timer()

    if remote_src:
        if "/" in src:
            if path.exists(src):
                if path.isfile(src):
                    job_submitted_id, duration = submit_src_jcl(
                        module, src, src_name=src, timeout=wait_time, is_unix=True)
                else:
                    module.fail_json(msg=f"Unable to submit job {src} is a folder, must be a file.", **result)
            else:
                module.fail_json(msg=f"Unable to submit job {src} does not exists.", **result)
        else:
            # Resolving a relative GDS name and escaping special symbols if needed.
            src_data = data_set.MVSDataSet(src)

            # Checking that the source is actually present on the system.
            if volume is not None:
                volumes = [volume]
                # Get the data set name to catalog it.
                src_ds_name = data_set.extract_dsname(src_data.name)
                present, changed = DataSet.attempt_catalog_if_necessary(src_ds_name, volumes)

                if not present:
                    module.fail_json(
                        msg=(f"Unable to submit job {src_data.name} because the data set could "
                             f"not be cataloged on the volume {volume}."), **result
                    )
            elif data_set.is_member(src_data.name):
                if not DataSet.data_set_member_exists(src_data.name):
                    module.fail_json(msg=f"Cannot submit job, the data set member {src_data.raw_name} was not found.", **result)
            else:
                if not DataSet.data_set_exists(src_data.name):
                    module.fail_json(msg=f"Cannot submit job, the data set {src_data.raw_name} was not found.", **result)

            job_submitted_id, duration = submit_src_jcl(
                module, src_data.name, src_name=src_data.raw_name, timeout=wait_time, is_unix=False, start_time=start_time)
    else:
        job_submitted_id, duration = submit_src_jcl(
            module, src, src_name=src, timeout=wait_time, is_unix=True)

    # Explictly pass None for the unused args else a default of '*' will be
    # used and return undersirable results
    job_output_txt = None
    is_changed = True
    # If wait_time is 0, we do a deploy and forget strategy.
    if wait_time != 0:

        try:
            job_output_txt = job_output(
                job_id=job_submitted_id, owner=None, job_name=None, dd_name=None,
                dd_scan=return_output, duration=duration, timeout=wait_time, start_time=start_time)
            # This is resolvig a bug where the duration coming from job_output is passed by value, duration
            # being an immutable type can not be changed and must be returned or accessed from the job.py.
            if job_output_txt is not None:
                duration = job_output_txt[0].get("duration") if not None else duration
                job_output_txt = parsing_job_response(job_output_txt, duration)

            result["duration"] = duration
            job_msg = job_output_txt[0].get("ret_code", {}).get("msg")
            if duration >= wait_time and job_msg != "HOLD":
                result["failed"] = True
                result["changed"] = False
                _msg = ("The JCL submitted with job id {0} but appears to be a long "
                        "running job that exceeded its maximum wait time of {1} "
                        "second(s). Consider using module zos_job_query to poll for "
                        "a long running job or increase option 'wait_time' to a value "
                        "greater than {2}.".format(str(job_submitted_id), str(wait_time), str(duration)))
                _msg_suffix = ("Consider using module zos_job_query to poll for "
                               "a long running job or increase option 'wait_time' to a value "
                               "greater than {0}.".format(str(duration)))

                if job_output_txt is not None:
                    result["jobs"] = job_output_txt
                    job_ret_code = job_output_txt[0].get("ret_code")
                    job_ret_code.update({"msg_txt": _msg_suffix})
                result["msg"] = _msg
                module.exit_json(**result)

            # Job has submitted, the module changed the managed node
            is_changed = True

            if job_output_txt:
                result["jobs"] = job_output_txt
                job_ret_code = job_output_txt[0].get("ret_code")
                steps = job_output_txt[0].get("steps")

                if job_ret_code:
                    job_ret_code_msg = job_ret_code.get("msg")
                    job_ret_code_code = job_ret_code.get("code")
                    job_ret_code_msg_code = job_ret_code.get("msg_code")

                    if return_output is True and max_rc is not None:
                        is_changed = assert_valid_return_code(max_rc, job_ret_code_code, job_ret_code, steps, result)

                    if job_ret_code_msg is not None:
                        if re.search("^(?:{0})".format("|".join(JOB_STATUSES)), job_ret_code_msg):
                            # If the job_ret_code_msg doesn't have a CC (completion code), the job failed.
                            if re.search("^(?:CC)", job_ret_code_msg) is None:
                                _msg = ("The job completion code (CC) was not in the job log. "
                                        "please review the job log for status {0}.".format(job_ret_code_msg))
                                result["stderr"] = _msg
                                job_ret_code.update({"msg_txt": _msg})
                                raise Exception(_msg)

                    if job_ret_code_msg == 'HOLD':
                        _msg = ("The job was run with TYPRUN=HOLD or TYPRUN=JCLHOLD "
                                "to request special job processing. This will result in no completion, "
                                "no return code, no job steps and changed will be set to false.")
                        job_ret_code.update({"msg_txt": _msg})
                        is_changed = False
                    elif job_ret_code_code is None and job_ret_code.get("msg") == 'NOEXEC':
                        # If there is no job_ret_code_code (Job return code) it may NOT be an error,
                        # some jobs will never return have an RC, eg Started tasks(which are not supported),
                        # so further analyze the
                        # JESJCL DD to figure out if its a TYPRUN job

                        job_dd_names = job_output_txt[0].get("dds")
                        jes_jcl_dd = search_dictionaries("dd_name", "JESJCL", job_dd_names)

                        # Its possible jobs don't have a JESJCL which are active and this would
                        # mean the job had TYPRUN=COPY.
                        if not jes_jcl_dd:
                            job_ret_code.update({"msg": "TYPRUN=COPY"})
                            _msg = ("The job was run with TYPRUN=COPY. "
                                    "This way, the steps are not executed, but the JCL is validated and stored "
                                    "in the JES spool. "
                                    "Please review the job log for further details.")
                            job_ret_code.update({"msg_txt": _msg})
                        else:
                            jes_jcl_dd_content = jes_jcl_dd[0].get("content")
                            jes_jcl_dd_content_str = " ".join(jes_jcl_dd_content)
                            # The regex can be r"({0})\s*=\s*(COPY|HOLD|JCLHOLD|SCAN)" once zoau support is in.
                            special_processing_keyword = re.search(r"({0})\s*=\s*(SCAN)"
                                                                   .format("|".join(JOB_SPECIAL_PROCESSING)), jes_jcl_dd_content_str)

                            job_ret_code.update({"msg": special_processing_keyword[0]})
                            job_ret_code.update({"code": None})
                            job_ret_code.update({"msg_code": None})
                            job_ret_code.update({"msg_txt": "The job {0} was run with special job "
                                                 "processing {1}. This will result in no completion, "
                                                 "return code or job steps and changed will be false."
                                                 .format(job_submitted_id, special_processing_keyword[0])})
                            is_changed = False
                    elif job_ret_code_code is None:
                        # The job_ret_code_code is None at this point, but the job_ret_code_msg_code could be populated
                        # so check both and provide a proper response.

                        if job_ret_code_msg_code is None:
                            _msg_detail = " for status {0}.".format(job_ret_code_msg) if job_ret_code_msg else "."
                            _msg = ("The job return code was not available in the job log, "
                                    "please review the job log{0}".format(_msg_detail))
                            job_ret_code.update({"msg_txt": _msg})
                            raise Exception(_msg)

                    elif job_ret_code_code != 0 and max_rc is None:
                        _msg = ("The job return code {0} was non-zero in the "
                                "job output, this job has failed.".format(str(job_ret_code_code)))
                        job_ret_code.update({"msg_txt": _msg})
                        result["stderr"] = _msg
                        raise Exception(_msg)

                    if not return_output:
                        for job in result.get("jobs", []):
                            job["dds"] = []
                else:
                    _msg = "The 'ret_code' dictionary was unavailable in the job log."
                    result["ret_code"] = None
                    result["stderr"] = _msg
                    raise Exception(_msg)
            else:
                _msg = "The job output log is unavailable."
                result["stderr"] = _msg
                result["jobs"] = []
                raise Exception(_msg)
        except Exception as err:
            result["failed"] = True
            result["changed"] = False
            result["msg"] = ("The JCL submitted with job id {0} but "
                             "there was an error, please review "
                             "the error for further details: {1}".format
                             (str(job_submitted_id), to_text(err)))
            module.exit_json(**result)

        finally:
            if temp_file is not None:
                shutil.rmtree(path.dirname(temp_file))
    else:
        result["jobs"] = build_empty_response(job_submitted_id)

    # If max_rc is set, we don't want to default to changed=True, rely on 'is_changed'
    result["changed"] = True if is_changed else False
    module.exit_json(**result)


def assert_valid_return_code(max_rc, job_rc, ret_code, steps, result):
    """Asserts valid return code.

    Parameters
    ----------
    max_rc : int
        Max return code.
    joc_rc : int
        Job return code.
    ret_code : int
        Return code.
    result : dict()
        Result dictionary.

    Returns
    -------
    bool
        If job_rc is not 0.

    Raises
    ------
    Exception
        The job return code was not available in the jobs output.
    Exception
        The job return code for the submitted job is greater than the value set for option 'max_rc'.
    Exception
        The step return code for the submitted job is greater than the value set for option 'max_rc'.
    """

    if job_rc is None:
        raise Exception(
            "The job return code (ret_code[code]) was not available in the jobs output, "
            "this job has failed.")

    if job_rc > max_rc:
        _msg = ("The job return code, 'ret_code[code]' {0} for the submitted job is "
                "greater than the value set for option 'max_rc' {1}. "
                "Increase the value for 'max_rc' otherwise this job submission "
                "has failed.".format(str(job_rc), str(max_rc)))
        ret_code.update({"msg_txt": _msg})
        result["stderr"] = _msg
        raise Exception(_msg)

    for step in steps:
        step_cc_rc = int(step["step_cc"])
        step_name_for_rc = step["step_name"]
        if step_cc_rc > max_rc:
            _msg = ("The step name {0} with return code {1} for the submitted job is "
                    "greater than the value set for option 'max_rc' {2}. "
                    "Increase the value for 'max_rc' otherwise this job submission "
                    "has failed.".format(step_name_for_rc, str(step_cc_rc), str(max_rc)))
            ret_code.update({"msg_txt": _msg})
            result["stderr"] = _msg
            raise Exception(_msg)
    # If there is NO exception rasied it means that max_rc is larger than the
    # actual RC from the submitted job. In this case, the ansible changed status
    # should NOT be 'changed=true' even though the user did override the return code,
    # a non-zero return code means the job did not change anything, so set it as
    # result["chagned"]=False,
    if max_rc and job_rc > max_rc:
        return False
    elif job_rc != 0 and max_rc is None:
        return False

    return True


def parsing_job_response(jobs_raw, duration):
    """_summary_

    Args:
        jobs_raw (_type_): _description_
    """
    job = jobs_raw[0]
    jobs = []
    for job in jobs_raw:
        job_dict = {
            "job_id": job.get("job_id"),
            "job_name": job.get("job_name"),
            "content_type": job.get("content_type"),
            "duration": duration,
            "execution_time": job.get("execution_time"),
            "dds": job.get("dds"),
            "ret_code": job.get("ret_code"),
            "steps": job.get("steps"),
            "job_class": job.get("job_class"),
            "svc_class": job.get("svc_class"),
            "system": job.get("system"),
            "subsystem": job.get("subsystem"),
            "origin_node": job.get("origin_node"),
            "cpu_time": job.get("cpu_time"),
            "execution_node": job.get("execution_node"),
            "priority": job.get("priority"),
            "asid": job.get("asid"),
            "creation_date": job.get("creation_date"),
            "creation_time": job.get("creation_time"),
            "queue_position": job.get("queue_position"),
            "program_name": job.get("program_name"),
        }
        jobs.append(job_dict)
    return jobs


def build_empty_response(job_submitted_id):
    """_summary_

    Args:
        jobs_raw (_type_): _description_
    """
    jobs = []
    job_dict = {
        "job_id": job_submitted_id,
        "job_name": None,
        "content_type": None,
        "duration": None,
        "execution_time": None,
        "dds": [],
        "ret_code": {"code": None, "msg": None, "msg_code": None, "msg_txt": None},
        "steps": [],
        "job_class": None,
        "svc_class": None,
        "system": None,
        "subsystem": None,
        "origin_node": None,
        "cpu_time": None,
        "execution_node": None,
        "priority": None,
        "asid": None,
        "creation_date": None,
        "creation_time": None,
        "queue_position": None,
        "program_name": None,
    }
    jobs.append(job_dict)
    return jobs


def main():
    run_module()


if __name__ == "__main__":
    main()
