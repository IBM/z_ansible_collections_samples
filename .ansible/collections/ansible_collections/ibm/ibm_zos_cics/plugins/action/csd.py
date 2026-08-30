# (c) Copyright IBM Corp. 2023,2024
# Apache License, Version 2.0 (see https://opensource.org/licenses/Apache-2.0)

from __future__ import absolute_import, division, print_function

__metaclass__ = type

from ansible_collections.ibm.ibm_zos_cics.plugins.plugin_utils._module_action_plugin import _DataSetActionPlugin
from ansible_collections.ibm.ibm_zos_cics.plugins.modules.csd import LOCAL, INPUT_CONTENT, INPUT_LOCATION, INPUT_SOURCE


class ActionModule(_DataSetActionPlugin):
    def run(self, tmp=None, task_vars=None):
        return super(ActionModule, self)._run(
            ds_name="dfhcsd",
            module_name="csd",
            cics_data_sets_required=True,
            tmp=tmp,
            task_vars=task_vars,
        )

    def _process_module_args(self, module_args, _templar, ds_name, task_vars, cics_data_sets_required):
        super(ActionModule, self)._process_module_args(module_args, _templar, ds_name, task_vars, cics_data_sets_required)
        input_location = module_args.get(INPUT_LOCATION)
        if input_location == LOCAL:
            input_src = module_args.get(INPUT_SOURCE)
            with open(input_src, 'r') as input_file:
                module_args[INPUT_CONTENT] = input_file.read()
