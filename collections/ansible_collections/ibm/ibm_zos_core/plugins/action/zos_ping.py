# (c) 2012, Michael DeHaan <michael.dehaan@gmail.com>
# Copyright (c) 2017 Ansible Project
# Copyright IBM Corporation 2020
# GNU General Public License v3.0+ (see COPYING or https://www.gnu.org/licenses/gpl-3.0.txt)

from __future__ import (absolute_import, division, print_function)

__metaclass__ = type

from ansible.plugins.action import ActionBase
from ansible.utils.vars import merge_hash


class ActionModule(ActionBase):
    def run(self, tmp=None, task_vars=None):
        # individual modules might disagree but as the generic the action plugin, pass at this point.
        self._supports_check_mode = True
        self._supports_async = True

        result = super(ActionModule, self).run(tmp, task_vars)
        del tmp  # tmp no longer has any effect

        if not result.get("skipped"):

            if result.get("invocation", {}).get("module_args"):
                # avoid passing to modules in case of no_log
                # should not be set anymore but here for backwards compatibility
                del result["invocation"]["module_args"]

            # FUTURE: better to let _execute_module calculate this internally?
            wrap_async = self._task.async_val and not self._connection.has_native_async

            # do work!
            result = merge_hash(
                result, self._execute_module(task_vars=task_vars, wrap_async=wrap_async)
            )

            # hack to keep --verbose from showing all the setup module result
            # moved from setup module as now we filter out all _ansible_ from result
            if self._task.action == "setup":
                result["_ansible_verbose_override"] = True

        if not wrap_async:
            # remove a temporary path we created
            self._remove_tmp_path(self._connection._shell.tmpdir)

        return result

    def _configure_module(self, module_name, module_args, task_vars=None):
        module_style, module_shebang, module_data, module_path = super(
            ActionModule, self
        )._configure_module(module_name, module_args, task_vars)
        if not module_shebang:
            module_shebang = " "
        return (module_style, module_shebang, module_data, module_path)
