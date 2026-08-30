# -*- coding: utf-8 -*-

# (c) Copyright IBM Corp. 2023,2024
# Apache License, Version 2.0 (see https://opensource.org/licenses/Apache-2.0)

# FOR INTERNAL USE IN THE COLLECTION ONLY.

from __future__ import (absolute_import, division, print_function)
__metaclass__ = type


from ansible_collections.ibm.ibm_zos_core.plugins.module_utils.dd_statement import StdoutDefinition, DatasetDefinition, DDStatement
from ansible_collections.ibm.ibm_zos_core.plugins.module_utils.zos_mvs_raw import MVSCmd
from ansible_collections.ibm.ibm_zos_cics.plugins.module_utils._data_set_utils import MVS_CMD_RETRY_ATTEMPTS
from ansible_collections.ibm.ibm_zos_cics.plugins.module_utils._response import MVSExecutionException, _execution


def _get_ccmutl_dds(catalog):   # type: (dict) -> list[DDStatement]
    return [
        DDStatement('steplib', DatasetDefinition(catalog["sdfhload"])),
        DDStatement('sysprint', StdoutDefinition()),
        DDStatement('sysudump', StdoutDefinition()),
        DDStatement(
            'dfhlcd',
            DatasetDefinition(
                dataset_name=catalog["name"],
                disposition="SHR")),
    ]


def _run_dfhccutl(starting_catalog):  # type: (dict) -> list
    executions = []

    for x in range(MVS_CMD_RETRY_ATTEMPTS):
        dfhccutl_response = _execute_dfhccutl(starting_catalog)

        executions.append(_execution(
            name="DFHCCUTL - Initialise Local Catalog",
            rc=dfhccutl_response.rc,
            stdout=dfhccutl_response.stdout,
            stderr=dfhccutl_response.stderr))

        if dfhccutl_response.rc != 0:
            raise MVSExecutionException(
                "DFHCCUTL failed with RC {0}".format(
                    dfhccutl_response.rc
                ), executions
            )
        else:
            break

    return executions


def _execute_dfhccutl(starting_catalog):
    return MVSCmd.execute(
        pgm="DFHCCUTL",
        dds=_get_ccmutl_dds(catalog=starting_catalog),
        verbose=True,
        debug=False)


def _get_idcams_cmd_lcd(data_set):  # type: (dict) -> dict
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
    defaults.update(data_set)
    return defaults


RECORD_COUNT_DEFAULT = 70
RECORD_SIZE_DEFAULT = 2041
CONTROL_INTERVAL_SIZE_DEFAULT = 2048
KEY_LENGTH = 52
KEY_OFFSET = 0
CI_PERCENT = 10
CA_PERCENT = 10
SHARE_CROSSREGION = 2
