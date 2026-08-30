#!/usr/bin/python
# -*- coding: utf-8 -*-

# (c) Copyright IBM Corp. 2023,2024
# Apache License, Version 2.0 (see https://opensource.org/licenses/Apache-2.0)

from __future__ import absolute_import, division, print_function
__metaclass__ = type

DOCUMENTATION = r'''
---
module: global_catalog
short_description: Create, remove, and manage the CICS global catalog
description:
  - Create, remove, and manage the L(global catalog,https://www.ibm.com/docs/en/cics-ts/latest?topic=catalogs-global-catalog)
    data set used by a CICS® region. The global catalog is used to store start type information, location of the CICS system log,
    installed resource definitions, terminal control information and profiles. It contains information that CICS requires on a restart.
  - You can use this module when provisioning or de-provisioning a CICS region, or when managing
    the state of the global catalog during upgrades or restarts.
  - Use the O(state) option to specify the intended state for the global catalog. For example, use O(state=initial) to create
    and initialize a global catalog data set if it doesn't exist, or set the autostart override record of an existing
    global catalog to C(AUTOINIT). In either case, a CICS region that is using this global catalog and set with the
    C(START=AUTO) system initialization parameter performs an initial start.
author: Andrew Twydell (@AndrewTwydell)
version_added: 2.1.0
seealso:
  - module: local_catalog
extends_documentation_fragment:
  - ibm.ibm_zos_cics.global_catalog
'''


EXAMPLES = r"""
- name: Initialize a global catalog by using the templated location
  ibm.ibm_zos_cics.global_catalog:
    region_data_sets:
      template: "REGIONS.ABCD0001.<< data_set_name >>"
    cics_data_sets:
      template: "CICSTS61.CICS.<< lib_name >>"
    state: "initial"

- name: Initialize a large global catalog by using the templated location
  ibm.ibm_zos_cics.global_catalog:
    region_data_sets:
      template: "REGIONS.ABCD0001.<< data_set_name >>"
    cics_data_sets:
      template: "CICSTS61.CICS.<< lib_name >>"
    space_primary: 100
    space_type: "M"
    state: "initial"

- name: Initialize a large user specified global catalog
  ibm.ibm_zos_cics.global_catalog:
    region_data_sets:
      dfhgcd:
        dsn: "REGIONS.ABCD0001.DFHGCD"
    cics_data_sets:
      sdfhload: "CICSTS61.CICS.SDFHLOAD"
    space_primary: 100
    space_type: "M"
    state: "initial"

- name: Set the autostart override record to AUTOASIS for a global catalog defined by the template
  ibm.ibm_zos_cics.global_catalog:
    region_data_sets:
      template: "REGIONS.ABCD0001.<< data_set_name >>"
    cics_data_sets:
      template: "CICSTS61.CICS.<< lib_name >>"
    state: "warm"

- name: Set the autostart override record to AUTOASIS for a user specified global catalog
  ibm.ibm_zos_cics.global_catalog:
    region_data_sets:
      dfhgcd:
        dsn: "REGIONS.ABCD0001.DFHGCD"
    cics_data_sets:
      sdfhload: "CICSTS61.CICS.SDFHLOAD"
    state: "warm"

- name: Set the autostart override record to AUTOCOLD for a global catalog defined by the template
  ibm.ibm_zos_cics.global_catalog:
    region_data_sets:
      template: "REGIONS.ABCD0001.<< data_set_name >>"
    cics_data_sets:
      template: "CICSTS61.CICS.<< lib_name >>"
    state: "cold"

- name: Set the autostart override record to AUTOCOLD for a user specified global catalog
  ibm.ibm_zos_cics.global_catalog:
    region_data_sets:
      dfhgcd:
        dsn: "REGIONS.ABCD0001.DFHGCD"
    cics_data_sets:
      sdfhload: "CICSTS61.CICS.SDFHLOAD"
    state: "cold"

- name: Delete a global catalog defined by the template
  ibm.ibm_zos_cics.global_catalog:
    region_data_sets:
      template: "REGIONS.ABCD0001.<< data_set_name >>"
    cics_data_sets:
      template: "CICSTS61.CICS.<< lib_name >>"
    state: "absent"

- name: Delete a user specified global catalog
  ibm.ibm_zos_cics.global_catalog:
    region_data_sets:
      dfhgcd:
        dsn: "REGIONS.ABCD0001.DFHGCD"
    cics_data_sets:
      sdfhload: "CICSTS61.CICS.SDFHLOAD"
    state: "absent"
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
    - The state of the global catalog before the Ansible task runs.
  returned: always
  type: dict
  contains:
    autostart_override:
      description: The current autostart override record.
      returned: always
      type: str
    next_start:
      description: The next start type listed in the global catalog.
      returned: always
      type: str
    exists:
      description: True if the specified global catalog data set exists.
      type: bool
      returned: always
    data_set_organization:
      description: The organization of the data set at the start of the Ansible task.
      returned: always
      type: str
      sample: "VSAM"
end_state:
  description: The state of the global catalog at the end of the Ansible task.
  returned: always
  type: dict
  contains:
    autostart_override:
      description: The current autostart override record.
      returned: always
      type: str
    next_start:
      description: The next start type listed in the global catalog
      returned: always
      type: str
    exists:
      description: True if the specified global catalog data set exists.
      type: bool
      returned: always
    data_set_organization:
      description: The organization of the data set at the end of the Ansible task.
      returned: always
      type: str
      sample: "VSAM"
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
from ansible_collections.ibm.ibm_zos_cics.plugins.module_utils._global_catalog import (
    _get_idcams_cmd_gcd,
    _run_dfhrmutl
)

COLD = "cold"
STATE_OPTIONS = [ABSENT, INITIAL, WARM, COLD]
SPACE_OPTIONS = [KILOBYTES, MEGABYTES, RECORDS, CYLINDERS, TRACKS]
DSN = "dfhgcd"
AUTO_START_WARM = "AUTOASIS"
AUTO_START_COLD = "AUTOCOLD"
AUTO_START_INIT = "AUTOINIT"
NEXT_START_EMERGENCY = "EMERGENCY"
NEXT_START_WARM = "WARM"
NEXT_START_COLD = "COLD"
NEXT_START_UNKNOWN = "UNKNOWN"
SPACE_PRIMARY_DEFAULT = 5
SPACE_SECONDARY_DEFAULT = 1


class AnsibleGlobalCatalogModule(DataSet):
    def __init__(self):
        self.autostart_override = ""
        self.next_start = ""
        super(AnsibleGlobalCatalogModule, self).__init__(SPACE_PRIMARY_DEFAULT, SPACE_SECONDARY_DEFAULT)
        self.start_state = dict(
            exists=False,
            data_set_organization=self.data_set_organization,
            autostart_override=self.autostart_override,
            next_start=self.next_start
        )
        self.end_state = dict(
            exists=False,
            data_set_organization=self.data_set_organization,
            autostart_override=self.autostart_override,
            next_start=self.next_start
        )
        self.name = self.region_param[DSN]["dsn"].upper()
        self.expected_data_set_organization = "VSAM"

    def get_data_set(self):  # type: () -> dict
        data_set = super().get_data_set()
        data_set.update({
            "autostart_override": self.autostart_override,
            "next_start": self.next_start,
        })
        return data_set

    def set_start_state(self):  # type: () -> None
        self.start_state = dict(
            exists=self.exists,
            data_set_organization=self.data_set_organization,
            autostart_override=self.autostart_override,
            next_start=self.next_start
        )

    def set_end_state(self):  # type: () -> None
        self.end_state = dict(
            exists=self.exists,
            data_set_organization=self.data_set_organization,
            autostart_override=self.autostart_override,
            next_start=self.next_start
        )

    def _get_arg_spec(self):  # type: () -> dict
        arg_spec = super(AnsibleGlobalCatalogModule, self)._get_arg_spec()

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

        return arg_spec

    def get_arg_defs(self):  # type: () -> dict
        defs = super().get_arg_defs()
        defs[REGION_DATA_SETS]["options"][DSN]["options"]["dsn"].update({
            "arg_type": "data_set_base"
        })
        defs[REGION_DATA_SETS]["options"][DSN]["options"]["dsn"].pop("type")
        return defs

    def create_data_set(self):  # type: () -> None
        create_cmd = _build_idcams_define_cmd(_get_idcams_cmd_gcd(self.get_data_set()))
        super().build_vsam_data_set(create_cmd)

    def init_data_set(self):  # type: () -> None
        if self.exists and self.autostart_override == AUTO_START_INIT:
            self._exit()

        if not self.exists:
            self.create_data_set()

        self.check_emergency()
        try:
            dfhrmutl_executions = _run_dfhrmutl(
                self.name,
                self.sdfhload,
                cmd="SET_AUTO_START=AUTOINIT")
            self.changed = True
            self.executions.extend(dfhrmutl_executions)
        except MVSExecutionException as e:
            self.executions.extend(e.executions)
            self._fail(e.message)

    def warm_data_set(self):  # type: () -> None
        super().warm_with_records()

        if self.autostart_override == AUTO_START_WARM:
            self._exit()

        if (
            self.autostart_override == AUTO_START_INIT and
            self.next_start == NEXT_START_UNKNOWN
        ):
            self._fail(
                "Unused catalog. The catalog must be used by CICS before doing a warm start.")
        try:
            dfhrmutl_executions = _run_dfhrmutl(
                self.name,
                self.sdfhload,
                cmd="SET_AUTO_START=AUTOASIS")
            self.changed = True
            self.executions.extend(dfhrmutl_executions)
        except MVSExecutionException as e:
            self.executions.extend(e.executions)
            self._fail(e.message)

    def cold_data_set(self):  # type: () -> None
        if not self.exists:
            self._fail("Data set {0} does not exist.".format(self.name))

        self.check_emergency()
        if self.autostart_override == AUTO_START_COLD:
            self._exit()

        if (
            self.autostart_override == AUTO_START_INIT and
            self.next_start == NEXT_START_UNKNOWN
        ):
            self._fail(
                "Unused catalog. The catalog must be used by CICS before doing a cold start.")
        try:
            dfhrmutl_executions = _run_dfhrmutl(
                self.name,
                self.sdfhload,
                cmd="SET_AUTO_START=AUTOCOLD")
            self.changed = True
            self.executions.extend(dfhrmutl_executions)
        except MVSExecutionException as e:
            self.executions.extend(e.executions)
            self._fail(e.message)

    def execute_target_state(self):   # type: () -> None
        if self.target_state == ABSENT:
            self.delete_data_set()
        elif self.target_state == INITIAL:
            self.init_data_set()
        elif self.target_state == WARM:
            self.warm_data_set()
        elif self.target_state == COLD:
            self.cold_data_set()
        else:
            self.invalid_target_state()

    def update_data_set_state(self):  # type: () -> None
        super().update_data_set_state()

        if self.exists and (self.data_set_organization == self.expected_data_set_organization):
            try:
                dfhrmutl_executions, (self.autostart_override, self.next_start) = _run_dfhrmutl(
                    self.name, self.sdfhload)

                self.executions.extend(dfhrmutl_executions)
            except MVSExecutionException as e:
                self.executions.extend(e.executions)
                self._fail(e.message)
        else:
            self.autostart_override = ""
            self.next_start = ""

    def check_emergency(self):  # type: () -> None
        if self.next_start and self.next_start.upper() == NEXT_START_EMERGENCY:
            self._fail(
                "Next start type is {0}. Potential data loss prevented."
                .format(NEXT_START_EMERGENCY))


def main():
    AnsibleGlobalCatalogModule().main()


if __name__ == '__main__':
    main()
