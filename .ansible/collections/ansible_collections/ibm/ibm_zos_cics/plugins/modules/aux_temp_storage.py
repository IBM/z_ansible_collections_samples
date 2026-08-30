#!/usr/bin/python
# -*- coding: utf-8 -*-

# (c) Copyright IBM Corp. 2023,2024
# Apache License, Version 2.0 (see https://opensource.org/licenses/Apache-2.0)

from __future__ import absolute_import, division, print_function

__metaclass__ = type

DOCUMENTATION = r"""
---
module: aux_temp_storage
short_description: Create and remove the CICS auxiliary temporary storage data set
description:
  - Create and remove the L(auxiliary temporary storage,https://www.ibm.com/docs/en/cics-ts/latest?topic=sets-defining-auxiliary-temporary-storage-data-set)
    data set used by a CICS® region.
  - You can use this module when provisioning or de-provisioning a CICS region.
  - Use the O(state) option to specify the intended state for the auxiliary
    temporary storage data set. For example, use O(state=initial) to create an auxiliary temporary storage
    data set if it doesn't exist.
author: Andrew Twydell (@andrewtwydell)
version_added: 2.1.0
extends_documentation_fragment:
  - ibm.ibm_zos_cics.aux_temp_storage
"""


EXAMPLES = r"""
- name: Initialize an auxiliary temporary storage data set by using the templated location
  ibm.ibm_zos_cics.aux_temp_storage:
    region_data_sets:
      template: "REGIONS.ABCD0001.<< data_set_name >>"
    state: "initial"

- name: Initialize a user specified auxiliary temporary storage data set
  ibm.ibm_zos_cics.aux_temp_storage:
    region_data_sets:
      dfhtemp:
        dsn: "REGIONS.ABCD0001.DFHTEMP"
    state: "initial"

- name: Initialize a large auxiliary temporary storage data set by using the templated location
  ibm.ibm_zos_cics.aux_temp_storage:
    region_data_sets:
      template: "REGIONS.ABCD0001.<< data_set_name >>"
    space_primary: 50
    space_type: "M"
    state: "initial"

- name: Retain the existing state of an auxiliary temporary storage data set defined by the template
  ibm.ibm_zos_cics.aux_temp_storage:
    region_data_sets:
      template: "REGIONS.ABCD0001.<< data_set_name >>"
    state: "warm"

- name: Retain the existing state of a user specified auxiliary temporary storage data set
  ibm.ibm_zos_cics.aux_temp_storage:
    region_data_sets:
      dfhtemp:
        dsn: "REGIONS.ABCD0001.DFHTEMP"
    state: "warm"

- name: Delete an existing auxiliary temporary storage data set defined by the template
  ibm.ibm_zos_cics.aux_temp_storage:
    region_data_sets:
      template: "REGIONS.ABCD0001.<< data_set_name >>"
    state: "absent"

- name: Delete an existing user specified auxiliary temporary storage data set
  ibm.ibm_zos_cics.aux_temp_storage:
    region_data_sets:
      dfhtemp:
        dsn: "REGIONS.ABCD0001.DFHTEMP"
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
    - The state of the auxiliary temporary storage data set before the Ansible task runs.
  returned: always
  type: dict
  contains:
    data_set_organization:
      description: The organization of the data set at the start of the Ansible task.
      returned: always
      type: str
      sample: "VSAM"
    exists:
      description: True if the specified auxiliary temporary storage data set exists.
      type: bool
      returned: always
end_state:
  description: The state of the auxiliary temporary storage data set at the end of the Ansible task.
  returned: always
  type: dict
  contains:
    data_set_organization:
      description: The organization of the data set at the end of the Ansible task.
      returned: always
      type: str
      sample: "VSAM"
    exists:
      description: True if the specified auxiliary temporary storage data set exists.
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

from ansible_collections.ibm.ibm_zos_cics.plugins.module_utils._data_set_utils import (
    _build_idcams_define_cmd
)
from ansible_collections.ibm.ibm_zos_cics.plugins.module_utils._data_set import (
    RECORDS,
    KILOBYTES,
    MEGABYTES,
    CYLINDERS,
    TRACKS,
    REGION_DATA_SETS,
    SPACE_PRIMARY,
    SPACE_SECONDARY,
    SPACE_TYPE,
    DataSet
)
from ansible_collections.ibm.ibm_zos_cics.plugins.module_utils._aux_temp_storage import (
    _get_idcams_cmd_temp
)


DSN = "dfhtemp"
SPACE_PRIMARY_DEFAULT = 200
SPACE_SECONDARY_DEFAULT = 10
SPACE_OPTIONS = [KILOBYTES, MEGABYTES, RECORDS, CYLINDERS, TRACKS]


class AnsibleAuxiliaryTempModule(DataSet):
    def __init__(self):
        super(AnsibleAuxiliaryTempModule, self).__init__(SPACE_PRIMARY_DEFAULT, SPACE_SECONDARY_DEFAULT)
        self.name = self.region_param[DSN]["dsn"].upper()
        self.expected_data_set_organization = "VSAM"

    def _get_arg_spec(self):  # type: () -> dict
        arg_spec = super(AnsibleAuxiliaryTempModule, self)._get_arg_spec()

        arg_spec[SPACE_PRIMARY].update({
            "default": SPACE_PRIMARY_DEFAULT
        })
        arg_spec[SPACE_SECONDARY].update({
            "default": SPACE_SECONDARY_DEFAULT
        })
        arg_spec[SPACE_TYPE].update({
            "default": RECORDS,
            "choices": SPACE_OPTIONS,
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

        return arg_spec

    def get_arg_defs(self):  # type: () -> dict
        defs = super().get_arg_defs()
        defs[REGION_DATA_SETS]["options"][DSN]["options"]["dsn"].update({
            "arg_type": "data_set_base"
        })
        defs[REGION_DATA_SETS]["options"][DSN]["options"]["dsn"].pop("type")
        return defs

    def create_data_set(self):  # type: () -> None
        create_cmd = _build_idcams_define_cmd(_get_idcams_cmd_temp(self.get_data_set()))
        super().build_vsam_data_set(create_cmd)


def main():
    AnsibleAuxiliaryTempModule().main()


if __name__ == "__main__":
    main()
