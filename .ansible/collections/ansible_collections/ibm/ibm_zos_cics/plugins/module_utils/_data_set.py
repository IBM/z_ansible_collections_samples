# -*- coding: utf-8 -*-

# (c) Copyright IBM Corp. 2023,2024
# Apache License, Version 2.0 (see https://opensource.org/licenses/Apache-2.0)

# FOR INTERNAL USE IN THE COLLECTION ONLY.

from __future__ import (absolute_import, division, print_function)


__metaclass__ = type

from ansible.module_utils.basic import AnsibleModule
from ansible_collections.ibm.ibm_zos_cics.plugins.module_utils._data_set_utils import (
    _build_idcams_define_cmd,
    _run_idcams,
    _run_listds,
    _run_iefbr14
)
from ansible_collections.ibm.ibm_zos_cics.plugins.module_utils._icetool import _run_icetool
from ansible_collections.ibm.ibm_zos_cics.plugins.module_utils._response import MVSExecutionException
from ansible_collections.ibm.ibm_zos_core.plugins.module_utils.better_arg_parser import BetterArgParser
from ansible_collections.ibm.ibm_zos_core.plugins.module_utils.dd_statement import DatasetDefinition

SDFHLOAD = "sdfhload"
STATE = "state"
SPACE_PRIMARY = "space_primary"
SPACE_SECONDARY = "space_secondary"
SPACE_TYPE = "space_type"
VOLUMES = "volumes"
KILOBYTES = "K"
MEGABYTES = "M"
RECORDS = "REC"
CYLINDERS = "CYL"
TRACKS = "TRK"
SPACE_OPTIONS = [KILOBYTES, MEGABYTES, CYLINDERS, TRACKS]
ABSENT = "absent"
INITIAL = "initial"
WARM = "warm"
STATE_OPTIONS = [ABSENT, INITIAL, WARM]
CICS_DATA_SETS = "cics_data_sets"
REGION_DATA_SETS = "region_data_sets"
DESTINATION = "destination"
DESTINATION_OPTIONS = ["A", "B"]
DESTINATION_DEFAULT_VALUE = "A"


class DataSet():
    def __init__(self, primary, secondary):
        self.name = ""
        self.target_state = ""
        self.exists = False
        self.data_set_organization = ""
        self.expected_data_set_organization = ""
        self.unit = ""
        self.primary = primary
        self.secondary = secondary
        self.volumes = None
        self.sdfhload = ""
        self.destination = ""

        self.changed = False
        self.failed = False
        self.start_state = dict(exists=False, data_set_organization=self.data_set_organization)
        self.end_state = dict(exists=False, data_set_organization=self.data_set_organization)
        self.executions = list()
        self.region_param = dict()
        self.msg = ""

        self._module = AnsibleModule(
            argument_spec=self._get_arg_spec(),
        )
        self.process_volume_arg()
        self.validate_parameters()

    def get_result(self):  # type: () -> dict
        return {
            "changed": self.changed,
            "failed": self.failed,
            "executions": self.executions,
            "start_state": self.start_state,
            "end_state": self.end_state,
            "msg": self.msg,
        }

    def get_data_set(self):  # type: () -> dict
        return {
            "name": self.name,
            "state": self.target_state,
            "exists": self.exists,
            "data_set_organization": self.data_set_organization,
            "unit": self.unit,
            "primary": self.primary,
            "secondary": self.secondary,
            "volumes": self.volumes,
            "sdfhload": self.sdfhload,
        }

    def set_start_state(self):   # type: () -> None
        self.start_state = dict(
            exists=self.exists,
            data_set_organization=self.data_set_organization
        )

    def set_end_state(self):  # type: () -> None
        self.end_state = dict(
            exists=self.exists,
            data_set_organization=self.data_set_organization
        )

    def _fail(self, msg):  # type: (str) -> None
        self.failed = True
        self.msg = msg
        self.set_end_state()
        self.result = self.get_result()
        self._module.fail_json(**self.result)

    def _exit(self):  # type: () -> None
        self.set_end_state()
        self.result = self.get_result()
        self._module.exit_json(**self.result)

    def _get_arg_spec(self):  # type: () -> dict
        """
        Get the arg spec, which is the set of arguments that can be passed into the Ansible module
        """
        return {
            SPACE_PRIMARY: {
                "type": "int",
            },
            SPACE_SECONDARY: {
                "type": "int",
            },
            SPACE_TYPE: {
                "type": "str",
                "choices": SPACE_OPTIONS,
            },
            VOLUMES: {
                "type": "raw"
            },
            STATE: {
                "type": "str",
                "required": True,
                "choices": STATE_OPTIONS
            },
            REGION_DATA_SETS: {
                "type": "dict",
                "required": True,
                "options": {
                    "template": {
                        "type": "str",
                        "required": False,
                    }
                },
            }
        }

    def get_arg_defs(self):  # type: () -> dict
        """
        Get the arg defs, which is a copy of the arg spec, but with certain types changed to the ones used by BetterArgParser
        """
        defs = self._get_arg_spec()
        if defs.get(CICS_DATA_SETS):
            defs[CICS_DATA_SETS]["options"]["sdfhload"].update({
                "arg_type": "data_set_base"
            })
            defs[CICS_DATA_SETS]["options"]["sdfhload"].pop("type")

        defs[VOLUMES].pop("type")
        defs[VOLUMES]["arg_type"] = "list"
        defs[VOLUMES]["elements"] = "volume"
        return defs

    def process_volume_arg(self):
        """
        Ensure Volumes is a string or list of strings
        """
        if self._module.params.get(VOLUMES):
            volumes_param = self._module.params[VOLUMES]
            if isinstance(volumes_param, str):
                self._module.params[VOLUMES] = volumes_param.split()

    def validate_parameters(self):  # type: () -> dict
        """
        Use BetterArgParser to parse the parameters passed in, which also does some validation
        """
        try:
            params = BetterArgParser(self.get_arg_defs()).parse_args(self._module.params)
        except ValueError as e:
            self._fail(str(e))
        self.assign_parameters(params)

    def assign_parameters(self, params):  # type: (dict) -> None
        """
        Assign parameters to the relevant fields
        """
        # Mandatory parameters
        self.target_state = params[STATE]
        self.region_param = params[REGION_DATA_SETS]

        # Optional parameters
        if params.get(SPACE_PRIMARY):
            self.primary = params[SPACE_PRIMARY]
        if params.get(SPACE_SECONDARY):
            self.secondary = params[SPACE_SECONDARY]
        if params.get(SPACE_TYPE):
            self.unit = params[SPACE_TYPE]
        if params.get(CICS_DATA_SETS) and params.get(CICS_DATA_SETS).get("sdfhload"):
            self.sdfhload = params[CICS_DATA_SETS]["sdfhload"].upper()
        if params.get(VOLUMES):
            self.volumes = params[VOLUMES]
        if params.get(DESTINATION):
            self.destination = params[DESTINATION]

    def create_data_set(self):  # type: () -> None
        _build_idcams_define_cmd({})

    def build_vsam_data_set(self, create_cmd):  # type: (str) -> None
        try:
            message = "Creating {0} data set".format(self.name)
            idcams_executions = _run_idcams(
                cmd=create_cmd,
                name=message,
                location=self.name,
                delete=False)
            self.executions.extend(idcams_executions)

            self.changed = True
        except MVSExecutionException as e:
            self.executions.extend(e.executions)
            self._fail(e.message)

    def build_seq_data_set(self, ddname, definition):  # type: (str, DatasetDefinition) -> None
        try:
            iefbr14_executions = _run_iefbr14(ddname, definition)
            self.executions.extend(iefbr14_executions)

            self.changed = True
        except MVSExecutionException as e:
            self.executions.extend(e.executions)
            self._fail(e.message)

    def delete_data_set(self):  # type: () -> None
        if self.exists:
            delete_cmd = '''
            DELETE {0}
            '''.format(self.name)

            try:
                idcams_executions = _run_idcams(
                    cmd=delete_cmd,
                    name=self.name,
                    location=self.name,
                    delete=True)
                self.executions.extend(idcams_executions)
                self.changed = True
            except MVSExecutionException as e:
                self.executions.extend(e.executions)
                self._fail(e.message)

    def init_data_set(self):   # type: () -> None
        if self.exists:
            try:
                icetool_executions, record_count = _run_icetool(self.name)
                self.executions.extend(icetool_executions)
                if record_count > 0:
                    self.delete_data_set()
                    self.update_data_set_state()
                    self.create_data_set()

            except MVSExecutionException as e:
                self.executions.extend(e.executions)
                self._fail(e.message)
        else:
            self.create_data_set()

    def warm_data_set(self):  # type: () -> None
        if not self.exists:
            self._fail("Data set {0} does not exist.".format(self.name))

    def warm_with_records(self):
        if self.exists:
            try:
                icetool_executions, record_count = _run_icetool(self.name)
                self.executions.extend(icetool_executions)
                if record_count <= 0:
                    self._fail("Data set {0} is empty.".format(self.name))
            except MVSExecutionException as e:
                self.executions.extend(e.executions)
                self._fail(e.message)
        else:
            self._fail("Data set {0} does not exist.".format(self.name))

    def invalid_target_state(self):   # type: () -> None
        self._fail("{0} is not a valid target state.".format(
            self.target_state))

    def execute_target_state(self):   # type: () -> None
        if self.target_state == ABSENT:
            self.delete_data_set()
        elif self.target_state == INITIAL:
            self.init_data_set()
        elif self.target_state == WARM:
            self.warm_data_set()
        else:
            self.invalid_target_state()

    def update_data_set_state(self):   # type: () -> None
        try:
            listds_executions, self.exists, self.data_set_organization = _run_listds(self.name)

            self.executions.extend(listds_executions)
        except MVSExecutionException as e:
            self.executions.extend(e.executions)
            self._fail(e.message)

    def main(self):  # type: () -> None
        self.update_data_set_state()
        self.set_start_state()

        if self.exists and (self.data_set_organization != self.expected_data_set_organization):
            self._fail(
                "Data set {0} is not in expected format {1}.".format(
                    self.name, self.expected_data_set_organization))

        self.execute_target_state()

        self.update_data_set_state()

        self._exit()
