#!/usr/bin/python
# -*- coding: utf-8 -*-

# (c) Copyright IBM Corp. 2023,2024
# Apache License, Version 2.0 (see https://opensource.org/licenses/Apache-2.0)

from __future__ import absolute_import, division, print_function

__metaclass__ = type

DOCUMENTATION = r'''
---
module: csd
short_description: Create, remove, and manage the CICS CSD
description:
  - Create, remove, and manage the
    L(CICS system definition data set,https://www.ibm.com/docs/en/cics-ts/latest?topic=configuring-setting-up-shared-data-sets-csd-sysin) (CSD) used by a CICS®
    region.
  - You can use this module when provisioning or de-provisioning a CICS region, or when managing
    the state of the CSD during upgrades or restarts.
  - Use the O(state) option to specify the intended state for the CSD.
    For example, use O(state=initial) to create and initialize a CSD
    if it doesn't exist, or empty an existing CSD of all records.
author: Thomas Latham (@Thomas-Latham3)
version_added: 2.1.0
extends_documentation_fragment:
  - ibm.ibm_zos_cics.csd
'''


EXAMPLES = r"""
- name: Initialize a CSD by using the templated location
  ibm.ibm_zos_cics.csd:
    region_data_sets:
      template: "REGIONS.ABCD0001.<< data_set_name >>"
    cics_data_sets:
      template: "CICSTS61.CICS.<< lib_name >>"
    state: "initial"

- name: Initialize a user specified CSD
  ibm.ibm_zos_cics.csd:
    region_data_sets:
      dfhcsd:
        dsn: "REGIONS.ABCD0001.DFHCSD"
    cics_data_sets:
      sdfhload: "CICSTS61.CICS.SDFHLOAD"
    state: "initial"

- name: Initialize a large CSD by using the templated location
  ibm.ibm_zos_cics.csd:
    region_data_sets:
      template: "REGIONS.ABCD0001.<< data_set_name >>"
    cics_data_sets:
      template: "CICSTS61.CICS.<< lib_name >>"
    space_primary: 10
    space_type: "M"
    state: "initial"

- name: Delete a CSD defined by the template
  ibm.ibm_zos_cics.csd:
    region_data_sets:
      template: "REGIONS.ABCD0001.<< data_set_name >>"
    cics_data_sets:
      template: "CICSTS61.CICS.<< lib_name >>"
    state: "absent"

- name: Delete a user specified CSD
  ibm.ibm_zos_cics.csd:
    region_data_sets:
      dfhcsd:
        dsn: "REGIONS.ABCD0001.DFHCSD"
    cics_data_sets:
      sdfhload: "CICSTS61.CICS.SDFHLOAD"
    state: "absent"

- name: Retain the existing state of a CSD defined by the template
  ibm.ibm_zos_cics.csd:
    region_data_sets:
      template: "REGIONS.ABCD0001.<< data_set_name >>"
    cics_data_sets:
      template: "CICSTS61.CICS.<< lib_name >>"
    state: "warm"

- name: Retain the existing state of a user specified CSD
  ibm.ibm_zos_cics.csd:
    region_data_sets:
      dfhcsd:
        dsn: "REGIONS.ABCD0001.DFHCSD"
    cics_data_sets:
      sdfhload: "CICSTS61.CICS.SDFHLOAD"
    state: "warm"

- name: Run a DFHCSDUP script from a data set
  ibm.ibm_zos_cics.csd:
    region_data_sets:
      template: "REGIONS.ABCD0001.<< data_set_name >>"
    cics_data_sets:
      template: "CICSTS61.CICS.<< lib_name >>"
    state: "changed"
    input_location: "DATA_SET"
    input_src: "TESTER.DEFS.SCRIPT"

- name: Run a DFHCSDUP script from a USS file
  ibm.ibm_zos_cics.csd:
    region_data_sets:
      template: "REGIONS.ABCD0001.<< data_set_name >>"
    cics_data_sets:
      template: "CICSTS61.CICS.<< lib_name >>"
    input_location: "USS"
    input_src: "/u/tester/defs/script.csdup"

- name: Run a DFHCSDUP script from a local file
  ibm.ibm_zos_cics.csd:
    region_data_sets:
      template: "REGIONS.ABCD0001.<< data_set_name >>"
    cics_data_sets:
      template: "CICSTS61.CICS.<< lib_name >>"
    input_location: "LOCAL"
    input_src: "/User/tester/defs/script.csdup"

- name: Run a DFHCSDUP script inline
  ibm.ibm_zos_cics.csd:
    region_data_sets:
      template: "REGIONS.ABCD0001.<< data_set_name >>"
    cics_data_sets:
      template: "CICSTS61.CICS.<< lib_name >>"
    input_location: "INLINE"
    input_content: |
      DEFINE PROGRAM(TESTPRG1) GROUP(TESTGRP1)
      DEFINE PROGRAM(TESTPRG2) GROUP(TESTGRP2)
"""


RETURN = r"""
changed:
  description: True if the state was changed, otherwise False.
  returned: always
  type: bool
failed:
  description: True if the Ansible task failed, otherwise False.
  returned: always
  type: bool
start_state:
  description:
    - The state of the CSD before the Ansible task runs.
  returned: always
  type: dict
  contains:
    data_set_organization:
      description: The organization of the data set at the start of the Ansible task.
      returned: always
      type: str
      sample: "VSAM"
    exists:
      description: True if the CSD exists.
      type: bool
      returned: always
end_state:
  description: The state of the CSD at the end of the Ansible task.
  returned: always
  type: dict
  contains:
    data_set_organization:
      description: The organization of the data set at the end of the Ansible task.
      returned: always
      type: str
      sample: "VSAM"
    exists:
      description: True if the CSD exists.
      type: bool
      returned: always
executions:
  description: A list of program executions performed during the Ansible task.
  returned: always
  type: list
  elements: dict
  contains:
    name:
      description: A human-readable name for the program execution.
      type: str
      returned: always
    rc:
      description: The return code for the program execution.
      type: int
      returned: always
    stdout:
      description: The standard output stream returned from the program execution.
      type: str
      returned: always
    stderr:
      description: The standard error stream returned from the program execution.
      type: str
      returned: always
msg:
  description: A string containing an error message if applicable
  returned: always
  type: str
"""


from ansible_collections.ibm.ibm_zos_cics.plugins.module_utils._response import MVSExecutionException
from ansible_collections.ibm.ibm_zos_core.plugins.module_utils.dd_statement import DatasetDefinition, StdinDefinition
from ansible_collections.ibm.ibm_zos_cics.plugins.module_utils._data_set_utils import (
    _build_idcams_define_cmd
)
from ansible_collections.ibm.ibm_zos_cics.plugins.module_utils._data_set import (
    CICS_DATA_SETS,
    MEGABYTES,
    KILOBYTES,
    RECORDS,
    CYLINDERS,
    TRACKS,
    REGION_DATA_SETS,
    SPACE_PRIMARY,
    SPACE_SECONDARY,
    SPACE_TYPE,
    STATE,
    ABSENT,
    INITIAL,
    WARM,
    DataSet
)
from ansible_collections.ibm.ibm_zos_cics.plugins.module_utils._csd import (
    _get_csdup_initilize_cmd,
    _get_idcams_cmd_csd,
    _run_dfhcsdup
)

DSN = "dfhcsd"
SPACE_PRIMARY_DEFAULT = 4
SPACE_SECONDARY_DEFAULT = 1
SPACE_OPTIONS = [KILOBYTES, MEGABYTES, RECORDS, CYLINDERS, TRACKS]
CHANGED = "changed"
STATE_OPTIONS = [ABSENT, INITIAL, WARM, CHANGED]
INPUT_SOURCE = "input_src"
INPUT_LOCATION = "input_location"
INPUT_CONTENT = "input_content"
DATA_SET = "DATA_SET"
USS = "USS"
LOCAL = "LOCAL"
INLINE = "INLINE"
INPUT_LOCATION_OPTIONS = [DATA_SET, USS, LOCAL, INLINE]
INPUT_LOCATION_DEFAULT = DATA_SET
LOG = "log"
LOG_OPTIONS = ["NONE", "UNDO", "ALL"]
LOGSTREAMID = "logstream_id"


class AnsibleCSDModule(DataSet):
    def __init__(self):
        self.input_src = ""
        self.input_location = ""
        super(AnsibleCSDModule, self).__init__(SPACE_PRIMARY_DEFAULT, SPACE_SECONDARY_DEFAULT)
        self._validate_log_args()
        self.name = self.region_param[DSN]["dsn"].upper()
        self.expected_data_set_organization = "VSAM"

    def _get_arg_spec(self):  # type: () -> dict
        arg_spec = super(AnsibleCSDModule, self)._get_arg_spec()

        arg_spec[SPACE_PRIMARY].update({
            "default": SPACE_PRIMARY_DEFAULT
        })
        arg_spec[SPACE_SECONDARY].update({
            "default": SPACE_SECONDARY_DEFAULT
        })
        arg_spec[SPACE_TYPE].update({
            "default": MEGABYTES,
            "choices": SPACE_OPTIONS,
        })
        arg_spec[STATE].update({
            "choices": STATE_OPTIONS
        })
        arg_spec[REGION_DATA_SETS]["options"].update({
            DSN: {
                "type": "dict",
                "required": False,
                "options": {
                    "dsn": {
                        "type": "str",
                        "required": False,
                    },
                },
            },
        })
        arg_spec[CICS_DATA_SETS] = {
            "type": "dict",
            "required": True,
            "options": {
                "template": {
                    "type": "str",
                    "required": False,
                },
                "sdfhload": {
                    "type": "str",
                    "required": False,
                },
            },
        }
        arg_spec.update({
            INPUT_SOURCE: {
                "type": "str"
            },
            INPUT_LOCATION: {
                "type": "str",
                "choices": INPUT_LOCATION_OPTIONS,
                "default": DATA_SET
            },
            INPUT_CONTENT: {
                "type": "str"
            },
        })
        arg_spec.update({
            LOG: {
                "type": "str",
                "choices": LOG_OPTIONS,
                "required": False
            },
        })
        arg_spec.update({
            LOGSTREAMID: {
                "type": "str",
                "required": False
            },
        })
        return arg_spec

    def _validate_log_args(self):
        if self._module.params.get(LOG, "") == "ALL" and self._module.params.get(LOGSTREAMID) is None:
            self._fail("LOGSTREAMID must be provided when LOG is set to ALL.")

    def get_arg_defs(self):  # type: () -> dict
        defs = super().get_arg_defs()
        defs[REGION_DATA_SETS]["options"][DSN]["options"]["dsn"].update({
            "arg_type": "data_set_base"
        })
        defs[REGION_DATA_SETS]["options"][DSN]["options"]["dsn"].pop("type")
        if INPUT_LOCATION == DATA_SET:
            defs[INPUT_SOURCE].update({
                "arg_type": "data_set_base"
            })
            defs[INPUT_SOURCE].pop("type")
        return defs

    def assign_parameters(self, params):  # type: (dict) -> None
        super().assign_parameters(params)
        if params.get(INPUT_SOURCE):
            self.input_src = params[INPUT_SOURCE]
        if params.get(INPUT_LOCATION):
            self.input_location = params[INPUT_LOCATION]
        if params.get(INPUT_CONTENT):
            self.input_content = params[INPUT_CONTENT]

    def execute_target_state(self):   # type: () -> None
        if self.target_state == ABSENT:
            self.delete_data_set()
        elif self.target_state == INITIAL:
            self.init_data_set()
        elif self.target_state == WARM:
            self.warm_with_records()
        elif self.target_state == CHANGED:
            self.csdup_script()
        else:
            self.invalid_target_state()

    def get_data_set(self):
        data_set = super().get_data_set()
        data_set.update({
            LOG: self._module.params.get(LOG),
            LOGSTREAMID: self._module.params.get(LOGSTREAMID)
        })
        return data_set

    def create_data_set(self):  # type: () -> None
        create_cmd = _build_idcams_define_cmd(_get_idcams_cmd_csd(self.get_data_set()))
        super().build_vsam_data_set(create_cmd)

    def init_data_set(self):  # type: () -> None
        super().init_data_set()
        try:
            csdup_initialize_executions = _run_dfhcsdup(self.get_data_set(), _get_csdup_initilize_cmd())
            self.executions.extend(csdup_initialize_executions)
        except MVSExecutionException as e:
            self.executions.extend(e.executions)
            self._fail(e.message)

    def csdup_script(self):

        if not self.input_location:
            self._fail("input_location required")

        if self.input_location == INLINE:
            if not self.input_content:
                self._fail("input_content required when input_location={0}".format(self.input_location))
        else:
            if not self.input_src:
                self._fail("input_src required when input_location={0}".format(self.input_location))

        try:
            csdup_script_executions = []
            if self.input_location == DATA_SET:
                csdup_script_executions.extend(_run_dfhcsdup(self.get_data_set(), DatasetDefinition(self.input_src)))
            elif self.input_location == USS:
                file = open(self.input_src)
                file_content = file.read()
                csdup_script_executions.extend(_run_dfhcsdup(self.get_data_set(), StdinDefinition(content=file_content)))
            elif self.input_location in [LOCAL, INLINE]:
                csdup_script_executions.extend(_run_dfhcsdup(self.get_data_set(), StdinDefinition(content=self.input_content)))
            else:
                self._fail("input_location: {0} not recognised.".format(self.input_location))

            self.executions.extend(csdup_script_executions)

            self.changed = True
        except MVSExecutionException as e:
            self.executions.extend(e.executions)
            self._fail(e.message)
        except (OSError, ValueError) as e:
            # Handles the 'open' method failures
            self.executions.extend(csdup_script_executions)
            self._fail("{0} - {1}".format(type(e).__name__, str(e)))


def main():
    AnsibleCSDModule().main()


if __name__ == '__main__':
    main()
