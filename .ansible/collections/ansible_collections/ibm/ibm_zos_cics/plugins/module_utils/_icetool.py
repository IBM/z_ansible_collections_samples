# -*- coding: utf-8 -*-

# (c) Copyright IBM Corp. 2023,2024
# Apache License, Version 2.0 (see https://opensource.org/licenses/Apache-2.0)

# FOR INTERNAL USE IN THE COLLECTION ONLY.

from __future__ import (absolute_import, division, print_function)
__metaclass__ = type

from ansible_collections.ibm.ibm_zos_core.plugins.module_utils.zos_mvs_raw import MVSCmd, MVSCmdResponse
from ansible_collections.ibm.ibm_zos_core.plugins.module_utils.dd_statement import StdoutDefinition, DatasetDefinition, DDStatement, InputDefinition
from ansible_collections.ibm.ibm_zos_cics.plugins.module_utils._response import _execution, MVSExecutionException
from ansible_collections.ibm.ibm_zos_cics.plugins.module_utils._data_set_utils import MVS_CMD_RETRY_ATTEMPTS


def _get_icetool_dds(location):  # type: (str) -> list[DDStatement]
    return [
        DDStatement('sysprint', StdoutDefinition()),
        DDStatement('dd1', DatasetDefinition(dataset_name=location, disposition="SHR")),
        DDStatement('toolmsg', StdoutDefinition()),
        DDStatement('dfsmsg', StdoutDefinition()),
        DDStatement('showdef', StdoutDefinition()),
        DDStatement('toolin', InputDefinition(content="COUNT FROM(DD1)")),
    ]


def _get_reason_code(filtered):  # type: (list[str]) -> str
    if len(filtered) == 0:
        return ""

    elements2 = list(filtered[0].split(','))
    filtered2 = list(filter(lambda x: "REASON:X" in x, elements2))
    if len(filtered2) == 0:
        return ""

    elements3 = [element.replace("0", "")
                 for element in filtered2[0].split("'")]
    return elements3[1]


def _get_record_count(stdout):  # type: (str) -> int
    record_count = -1
    elements = ['{0}'.format(element.replace(" ", "").upper())
                for element in stdout.split("\n")]

    lines = list(filter(lambda x: "RECORDCOUNT:" in x, elements))
    if len(lines) > 0:
        record_count = int(lines[0].split(":")[1])

    return record_count


def _run_icetool(location):  # type: (str) -> tuple[list[_execution], int]
    executions = []

    for x in range(MVS_CMD_RETRY_ATTEMPTS):
        icetool_response = _execute_icetool(location)

        executions.append(
            _execution(
                name="ICETOOL - Get record count - Run {0}".format(x + 1),
                rc=icetool_response.rc,
                stdout=icetool_response.stdout,
                stderr=icetool_response.stderr))

        if icetool_response.rc != 0:
            elements = ["{0}".format(element.replace(" ", "").upper())
                        for element in icetool_response.stdout.split("\n")]
            filtered = list(filter(lambda x: "REASON:X" in x, elements))

            reason_code = _get_reason_code(filtered)
            if reason_code != "":
                raise MVSExecutionException(
                    "ICETOOL failed with RC {0} - {1}".format(icetool_response.rc, filtered[0]), executions)
            else:
                raise MVSExecutionException(
                    "ICETOOL failed with RC {0}".format(icetool_response.rc), executions)
        elif icetool_response.stdout != "":
            break

    if (icetool_response.stdout == "") and (icetool_response.stderr == ""):
        raise MVSExecutionException("ICETOOL Command output not recognised", executions)

    return executions, _get_record_count(icetool_response.stdout)


def _execute_icetool(location):  # type: (str) -> MVSCmdResponse
    return MVSCmd.execute(
        pgm="ICETOOL",
        dds=_get_icetool_dds(location=location),
        verbose=True,
        debug=False)
