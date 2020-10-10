# -*- coding: utf-8 -*-

# Copyright (c) IBM Corporation 2019, 2020
# Apache License, Version 2.0 (see https://opensource.org/licenses/Apache-2.0)

from __future__ import absolute_import, division, print_function

__metaclass__ = type

from shellescape import quote
import tempfile


JCL_FILE_CONTENTS = """//HELLO    JOB (T043JM,JM00,1,0,0,0),'HELLO WORLD - JRM',CLASS=R,
//             MSGCLASS=X,MSGLEVEL=1,NOTIFY=S0JM
//STEP0001 EXEC PGM=IEBGENER
//SYSIN    DD DUMMY
//SYSPRINT DD SYSOUT=*
//SYSUT1   DD *
HELLO, WORLD
/*
//SYSUT2   DD SYSOUT=*
//
"""

TEMP_PATH = "/tmp/ansible/jcl"


# def test_zos_job_output_no_job_id(ansible_zos_module):
#     hosts = ansible_zos_module
#     results = hosts.all.zos_job_output(job_id="NO_JOBID")
#     for result in results.contacted.values():
#         print(result)
#         assert result.get("changed") is False
#         assert result.get("jobs") is not None


# def test_zos_job_output_no_job_name(ansible_zos_module):
#     hosts = ansible_zos_module
#     results = hosts.all.zos_job_output(job_name="NO_JOBNAME")
#     for result in results.contacted.values():
#         print(result)
#         assert result.get("changed") is False
#         assert result.get("jobs") is not None


# def test_zos_job_output_no_owner(ansible_zos_module):
#     hosts = ansible_zos_module
#     results = hosts.all.zos_job_output(owner="NO_OWNER")
#     for result in results.contacted.values():
#         print(result)
#         assert result.get("changed") is False
#         assert result.get("jobs") is not None


def test_zos_job_output_reject(ansible_zos_module):
    hosts = ansible_zos_module
    results = hosts.all.zos_job_output()
    for result in results.contacted.values():
        print(result)
        assert result.get("changed") is False
        assert result.get("msg") is not None


def test_zos_job_output_job_exists(ansible_zos_module):
    hosts = ansible_zos_module
    hosts.all.file(path=TEMP_PATH, state="directory")
    hosts.all.shell(
        cmd="echo {0} > {1}/SAMPLE".format(quote(JCL_FILE_CONTENTS), TEMP_PATH)
    )
    hosts.all.zos_job_submit(
        src="{0}/SAMPLE".format(TEMP_PATH), location="USS", wait=True, volume=None
    )
    hosts.all.file(path=TEMP_PATH, state="absent")
    results = hosts.all.zos_job_output(job_name="SAMPLE")
    for result in results.contacted.values():
        assert result.get("changed") is False
        assert result.get("jobs") is not None
