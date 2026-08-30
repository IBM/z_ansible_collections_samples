#!/usr/bin/python
# -*- coding: utf-8 -*-

# (c) Copyright IBM Corp. 2024
# Apache License, Version 2.0 (see https://opensource.org/licenses/Apache-2.0)

from __future__ import absolute_import, division, print_function
__metaclass__ = type

DOCUMENTATION = r'''
---
module: transaction_dump
short_description: Allocate transaction dump data sets
description:
  - Allocates the two L(transaction dump ,https://www.ibm.com/docs/en/cics-ts/latest?topic=sets-defining-transaction-dump-data)
    data sets used by a CICS® region.
  - The two data sets are referred to as transaction dump data set A (DFHDMPA) and transaction dump data set B (DFHDMPB).
author: Thomas Foyle (@tom-foyle)
version_added: 2.1.0
extends_documentation_fragment:
  - ibm.ibm_zos_cics.transaction_dump
'''


EXAMPLES = r"""
- name: Allocate transaction dump data set A (implicit) by using the templated location
  ibm.ibm_zos_cics.transaction_dump:
    region_data_sets:
      template: "REGIONS.ABCD0001.<< data_set_name >>"
    state: initial

- name: Allocate a user specified data set as transaction dump data set A (implicit)
  ibm.ibm_zos_cics.transaction_dump:
    region_data_sets:
      dfhdmpa:
        dsn: "REGIONS.ABCD0001.DFHDMPA"
    state: initial

- name: Allocate transaction dump data set A by using the templated location
  ibm.ibm_zos_cics.transaction_dump:
    region_data_sets:
      template: "REGIONS.ABCD0001.<< data_set_name >>"
    state: initial
    destination: A

- name: Allocate a user specified data set as transaction dump data set A
  ibm.ibm_zos_cics.transaction_dump:
    region_data_sets:
      dfhdmpa:
        dsn: "REGIONS.ABCD0001.DFHDMPA"
    state: initial
    destination: A

- name: Allocate transaction dump data set B by using the templated location
  ibm.ibm_zos_cics.transaction_dump:
    region_data_sets:
      template: "REGIONS.ABCD0001.<< data_set_name >>"
    state: initial
    destination: B

- name: Allocate a user specified data set as transaction dump data set B
  ibm.ibm_zos_cics.transaction_dump:
    region_data_sets:
      dfhdmpb:
        dsn: "REGIONS.ABCD0001.DFHDMPB"
    state: initial
    destination: B

- name: Retain the existing state of transaction dump data set A (implicit) defined by the template
  ibm.ibm_zos_cics.transaction_dump:
    region_data_sets:
      template: "REGIONS.ABCD0001.<< data_set_name >>"
    state: "warm"

- name: Retain the existing state of a user specified transaction dump data set A (implicit)
  ibm.ibm_zos_cics.transaction_dump:
    region_data_sets:
      dfhdmpa:
        dsn: "REGIONS.ABCD0001.DFHDMPA"
    state: "warm"

- name: Retain the existing state of transaction dump data set B defined by the template
  ibm.ibm_zos_cics.transaction_dump:
    region_data_sets:
      template: "REGIONS.ABCD0001.<< data_set_name >>"
    state: "warm"
    destination: B

- name: Retain the existing state of a user specified transaction dump data set B
  ibm.ibm_zos_cics.transaction_dump:
    region_data_sets:
      dfhdmpb:
        dsn: "REGIONS.ABCD0001.DFHDMPB"
    state: "warm"
    destination: B

- name: Delete transaction dump data set A (implicit) defined by the template
  ibm.ibm_zos_cics.transaction_dump:
    region_data_sets:
      template: "REGIONS.ABCD0001.<< data_set_name >>"
    state: absent

- name: Delete a user specified transaction dump data set A (implicit)
  ibm.ibm_zos_cics.transaction_dump:
    region_data_sets:
      dfhdmpa:
        dsn: "REGIONS.ABCD0001.DFHDMPA"
    state: absent

- name: Delete transaction dump data set B defined by the template
  ibm.ibm_zos_cics.transaction_dump:
    region_data_sets:
      template: "REGIONS.ABCD0001.<< data_set_name >>"
    state: absent
    destination: B

- name: Delete a user specified transaction dump data set B
  ibm.ibm_zos_cics.transaction_dump:
    region_data_sets:
      dfhdmpb:
        dsn: "REGIONS.ABCD0001.DFHDMPB"
    state: absent
    destination: B
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
    - The state of the transaction dump data set before the Ansible task runs.
  returned: always
  type: dict
  contains:
    data_set_organization:
      description: The organization of the data set at the start of the Ansible task.
      returned: always
      type: str
      sample: "Sequential"
    exists:
      description: True if the specified transaction dump data set exists.
      type: bool
      returned: always
end_state:
  description: The state of the transaction dump data set at the end of the Ansible task.
  returned: always
  type: dict
  contains:
    data_set_organization:
      description: The organization of the data set at the end of the Ansible task.
      returned: always
      type: str
      sample: "Sequential"
    exists:
      description: True if the specified transaction dump data set exists.
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

from ansible_collections.ibm.ibm_zos_cics.plugins.module_utils._data_set import (
    DESTINATION,
    DESTINATION_OPTIONS,
    DESTINATION_DEFAULT_VALUE,
    MEGABYTES,
    REGION_DATA_SETS,
    SPACE_PRIMARY,
    SPACE_SECONDARY,
    SPACE_TYPE,
    DataSet
)
from ansible_collections.ibm.ibm_zos_cics.plugins.module_utils._transaction_dump import (
    _build_seq_data_set_definition_transaction_dump
)


DSN_A = "dfhdmpa"
DSN_B = "dfhdmpb"
SPACE_PRIMARY_DEFAULT = 20
SPACE_SECONDARY_DEFAULT = 4


class AnsibleTransactionDumpModule(DataSet):
    def __init__(self):  # type: () -> None
        self.ds_destination = ""
        super(AnsibleTransactionDumpModule, self).__init__(SPACE_PRIMARY_DEFAULT, SPACE_SECONDARY_DEFAULT)
        self.ds_destination = DSN_B if self.destination == "B" else DSN_A
        self.name = self.region_param[self.ds_destination]["dsn"].upper()
        self.expected_data_set_organization = "Sequential"

    def _get_arg_spec(self):  # type: () -> dict
        arg_spec = super(AnsibleTransactionDumpModule, self)._get_arg_spec()

        arg_spec.update({
            DESTINATION: {
                "type": "str",
                "choices": DESTINATION_OPTIONS,
                "default": DESTINATION_DEFAULT_VALUE
            }
        })

        arg_spec[SPACE_PRIMARY].update({
            "default": SPACE_PRIMARY_DEFAULT
        })
        arg_spec[SPACE_SECONDARY].update({
            "default": SPACE_SECONDARY_DEFAULT
        })
        arg_spec[SPACE_TYPE].update({
            "default": MEGABYTES
        })
        arg_spec[REGION_DATA_SETS]["options"].update({
            DSN_A: {
                "type": "dict",
                "required": False,
                "options": {
                    "dsn": {
                        "type": "str",
                        "required": False,
                    },
                },
            },
            DSN_B: {
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
        defs[REGION_DATA_SETS]["options"][DSN_A]["options"]["dsn"].update({
            "arg_type": "data_set_base"
        })
        defs[REGION_DATA_SETS]["options"][DSN_B]["options"]["dsn"].update({
            "arg_type": "data_set_base"
        })
        defs[REGION_DATA_SETS]["options"][DSN_A]["options"]["dsn"].pop("type")
        defs[REGION_DATA_SETS]["options"][DSN_B]["options"]["dsn"].pop("type")
        return defs

    def create_data_set(self):  # type: () -> None
        definition = _build_seq_data_set_definition_transaction_dump(self.get_data_set())
        super().build_seq_data_set(self.ds_destination, definition)


def main():
    AnsibleTransactionDumpModule().main()


if __name__ == '__main__':
    main()
