# -*- coding: utf-8 -*-

# (c) Copyright IBM Corp. 2023,2024
# Apache License, Version 2.0 (see https://opensource.org/licenses/Apache-2.0)

# FOR INTERNAL USE IN THE COLLECTION ONLY.

from __future__ import (absolute_import, division, print_function)
__metaclass__ = type


from ansible_collections.ibm.ibm_zos_core.plugins.module_utils.zos_mvs_raw import MVSCmd, MVSCmdResponse
from ansible_collections.ibm.ibm_zos_core.plugins.module_utils.dd_statement import StdoutDefinition, DatasetDefinition, DDStatement, InputDefinition
from ansible_collections.ibm.ibm_zos_cics.plugins.module_utils._response import MVSExecutionException, _execution
from ansible_collections.ibm.ibm_zos_cics.plugins.module_utils._data_set_utils import MVS_CMD_RETRY_ATTEMPTS


def _get_value_from_line(line):  # type: (list[str]) -> str | None
    val = None
    if len(line) == 1:
        val = line[0].split(":")[1]
    return val


def _get_filtered_list(elements, target):  # type: (list[str],str) -> list[str]
    return list(filter(lambda x: target in x, elements))


def _get_rmutl_dds(
        location,
        sdfhload,
        cmd):  # type: (str, str, str) -> list[DDStatement]
    return [
        DDStatement('steplib', DatasetDefinition(sdfhload)),
        DDStatement('dfhgcd', DatasetDefinition(location)),
        DDStatement('sysin', InputDefinition(content=cmd)),
        DDStatement('sysprint', StdoutDefinition()),
    ]


def _get_reason_code(stdout_lines_arr):  # type: (list[str]) -> str | None
    if len(stdout_lines_arr) == 0:
        return None

    stdout_comma_sep = list(stdout_lines_arr[0].split(","))
    filtered_for_reason_code = list(
        filter(lambda x: "REASON:X" in x, stdout_comma_sep))
    if len(filtered_for_reason_code) == 0:
        return None

    reason_code = [element.replace("0", "")
                   for element in filtered_for_reason_code[0].split("'")]
    return reason_code[1]


def _get_catalog_records(stdout):  # type: (str) -> tuple[str | None, str | None]
    elements = ['{0}'.format(element.replace(" ", "").upper())
                for element in stdout.split("\n")]

    autostart_filtered = _get_filtered_list(
        elements, "AUTO-STARTOVERRIDE:")
    nextstart_filtered = _get_filtered_list(elements, "NEXTSTARTTYPE:")

    autostart_override = _get_value_from_line(
        autostart_filtered)
    nextstart = _get_value_from_line(nextstart_filtered)

    return (autostart_override, nextstart)


def _run_dfhrmutl(
        location,  # type: str
        sdfhload,  # type: str
        cmd=""  # type: str
):
    # type: (...) -> tuple[list[dict[str, str| int]], tuple[str | None, str | None]] | list[dict[str, str| int]]

    executions = []

    for x in range(MVS_CMD_RETRY_ATTEMPTS):
        dfhrmutl_response = _execute_dfhrmutl(location, sdfhload, cmd)
        executions.append(
            _execution(
                name="DFHRMUTL - {0} - Run {1}".format(
                    "Get current catalog" if cmd == "" else "Updating autostart override",
                    x + 1),
                rc=dfhrmutl_response.rc,
                stdout=dfhrmutl_response.stdout,
                stderr=dfhrmutl_response.stderr))

        if dfhrmutl_response.rc == 0:
            break
        if dfhrmutl_response.rc == 16:
            formatted_stdout_lines = [
                "{0}".format(element.replace(" ", "").upper())
                for element in dfhrmutl_response.stdout.split("\n")
            ]
            stdout_with_rc = list(filter(lambda x: "REASON:X" in x, formatted_stdout_lines))

            reason_code = _get_reason_code(stdout_with_rc)
            if reason_code and reason_code != "A8":
                raise MVSExecutionException(
                    "DFHRMUTL failed with RC 16 - {0}".format(stdout_with_rc[0]), executions
                )
            elif reason_code is None:
                raise MVSExecutionException(
                    "DFHRMUTL failed with RC 16 but no reason code was found",
                    executions,
                )

        else:
            raise MVSExecutionException(
                "DFHRMUTL failed with RC {0}".format(
                    dfhrmutl_response.rc), executions)

    if cmd != "":
        return executions

    return executions, _get_catalog_records(dfhrmutl_response.stdout)


def _execute_dfhrmutl(location, sdfhload, cmd=""):   # type: (str, str, str) -> MVSCmdResponse
    return MVSCmd.execute(
        pgm="DFHRMUTL",
        dds=_get_rmutl_dds(location=location, sdfhload=sdfhload, cmd=cmd),
        verbose=True,
        debug=False)


def _get_idcams_cmd_gcd(dataset):   # type: (dict) -> dict
    defaults = {
        "CLUSTER": {
            "RECORDSIZE": "{0} {1}".format(RECORD_COUNT_DEFAULT, RECORD_SIZE_DEFAULT),
            "INDEXED": None,
            "KEYS": "{0} {1}".format(KEY_LENGTH, KEY_OFFSET),
            "FREESPACE": "{0} {1}".format(CI_PERCENT, CA_PERCENT),
            "SHAREOPTIONS": str(SHARE_CROSSREGION),
            "REUSE": None
        },
        "DATA": {
            "CONTROLINTERVALSIZE": str(CONTROL_INTERVAL_SIZE_DEFAULT)
        },
        "INDEX": {
            None
        }
    }
    defaults.update(dataset)
    return defaults


RECORD_COUNT_DEFAULT = 4089
RECORD_SIZE_DEFAULT = 32760
CONTROL_INTERVAL_SIZE_DEFAULT = 32768
KEY_LENGTH = 52
KEY_OFFSET = 0
CI_PERCENT = 10
CA_PERCENT = 10
SHARE_CROSSREGION = 2
