# (c) Copyright IBM Corp. 2024
# Apache License, Version 2.0 (see https://opensource.org/licenses/Apache-2.0)

# FOR INTERNAL USE IN THE COLLECTION ONLY.

from __future__ import absolute_import, division, print_function

__metaclass__ = type

from ansible.plugins.action import ActionBase

REGION_DS_KEYS = ["dfhgcd", "dfhlcd", "dfhintra", "dfhlrq", "dfhtemp", "dfhauxt", "dfhbuxt", "dfhdmpa", "dfhdmpb", "dfhcsd", "dfhstart"]
CICS_DS_KEYS = ["sdfhload", "sdfhauth", "sdfhlic"]
LE_DS_KEYS = ["sceecics", "sceerun", "sceerun2"]
CPSM_DS_KEYS = ["seyuauth", "seyuload"]
LIBRARY_KEYS = ["steplib", "dfhrpl"]


class _DataSetActionPlugin(ActionBase):
    def _run(self, ds_name, module_name, cics_data_sets_required, tmp=None, task_vars=None):
        super(_DataSetActionPlugin, self).run(tmp, task_vars)
        self.module_args = self._task.args.copy()

        return_structure = {
            "failed": False,
            "changed": False,
            "msg": "",
            "executions": [],
            "start_state": {"data_set_organization": "NONE", "exists": False},
            "end_state": {"data_set_organization": "NONE", "exists": False},
        }

        try:
            self._process_module_args(
                self.module_args,
                self._templar,
                ds_name,
                task_vars,
                cics_data_sets_required,
            )
        except (KeyError, ValueError) as e:
            return_structure.update({
                "failed": True,
                "changed": False,
                "msg": e.args[0],
            })
        else:
            return_structure.update(
                self._execute_module(
                    module_name="ibm.ibm_zos_cics.{0}".format(module_name),
                    module_args=self.module_args,
                    task_vars=task_vars,
                    tmp=tmp,
                )
            )

        return return_structure

    def _process_module_args(self, module_args, _templar, ds_name, task_vars, cics_data_sets_required):
        _process_module_args(module_args, _templar, ds_name, task_vars, cics_data_sets_required)


def _process_module_args(module_args, _templar, ds_name, task_vars, cics_data_sets_required):
    _process_region_data_set_args(module_args, _templar, ds_name, task_vars)
    _remove_region_data_set_args(module_args, ds_name)

    if cics_data_sets_required:
        _process_libraries_args(module_args, _templar, task_vars, "cics_data_sets", "sdfhload")
        _remove_cics_data_set_args(module_args, "sdfhload")
    else:
        if module_args.get("cics_data_sets"):
            del module_args["cics_data_sets"]

    if module_args.get("le_data_sets"):
        del module_args["le_data_sets"]

    if module_args.get("cpsm_data_sets"):
        del module_args["cpsm_data_sets"]


def _check_region_override(module_args, ds_name):
    return module_args["region_data_sets"].get(ds_name, {}).get("dsn") is not None


def _check_library_override(module_args, lib_type, lib_ds_name):
    if module_args[lib_type].get(lib_ds_name):
        return True
    else:
        return False


def _remove_region_data_set_args(module_args, ds_name):
    for region_key in list(module_args["region_data_sets"]):
        if region_key in REGION_DS_KEYS and region_key != ds_name:
            del module_args["region_data_sets"][region_key]


def _remove_cics_data_set_args(module_args, ds_name):
    for cics_key in list(module_args["cics_data_sets"]):
        if cics_key in CICS_DS_KEYS and cics_key != ds_name:
            del module_args["cics_data_sets"][cics_key]


def _process_region_data_set_args(module_args, _templar, ds_name, task_vars):
    if not module_args.get("region_data_sets"):
        raise KeyError("Required argument region_data_sets not found")

    if not _check_region_override(module_args, ds_name):
        if _check_template(module_args, "region_data_sets"):
            module_args["region_data_sets"].update({
                ds_name: {
                    "dsn": _template_dsn(
                        _templar=_templar,
                        task_vars=task_vars,
                        var_name="data_set_name",
                        replace_val=ds_name.upper(),
                        template=module_args["region_data_sets"]["template"],
                    )
                }}
            )
        else:
            raise KeyError("No template or data set override found for {0}".format(ds_name))
    return _validate_data_set_length(module_args["region_data_sets"][ds_name]["dsn"])


def _validate_list_of_data_set_lengths(data_set_list):
    for data_set in data_set_list:
        _validate_data_set_length(data_set)


def _validate_data_set_length(data_set):
    if len(data_set) > 44:
        raise ValueError("Data set: {0} is longer than 44 characters.".format(data_set))


def _process_libraries_args(module_args, _templar, task_vars, lib_type, lib_ds_name):
    if not _check_library_override(module_args, lib_type, lib_ds_name):
        if _check_template(module_args, lib_type):
            module_args[lib_type][lib_ds_name] = _template_dsn(
                _templar=_templar,
                task_vars=task_vars,
                var_name="lib_name",
                replace_val=lib_ds_name.upper(),
                template=module_args[lib_type]["template"],
            )
        else:
            raise KeyError("No template or library override found for {0}".format(lib_ds_name))
    return _validate_data_set_length(module_args[lib_type][lib_ds_name])


def _template_dsn(_templar, task_vars, var_name, replace_val, template):
    cpy = task_vars.copy()
    cpy.update({var_name: replace_val})
    return _templar.copy_with_new_env(
        variable_start_string="<<",
        variable_end_string=">>",
        available_variables=cpy,
    ).template(template)


def _check_template(module_args, arg_dict):
    return module_args.get(arg_dict, {}).get("template") is not None


def _set_top_libraries_key(module_args, dict_key):
    if module_args.get(dict_key) is None:
        module_args[dict_key] = {"top_data_sets": []}
    elif module_args[dict_key].get("top_data_sets") is None:
        module_args[dict_key].update({"top_data_sets": []})
