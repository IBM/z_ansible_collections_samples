# (c) 2012, Michael DeHaan <michael.dehaan@gmail.com>
# Copyright (c) 2017 Ansible Project
# Copyright IBM Corporation 2020, 2022
# GNU General Public License v3.0+ (see COPYING or https://www.gnu.org/licenses/gpl-3.0.txt)

from __future__ import (absolute_import, division, print_function)

__metaclass__ = type

from ansible.plugins.action import ActionBase
from ansible.utils.vars import merge_hash
from ansible import cli
from ansible.utils.display import Display

display = Display()


class ActionModule(ActionBase):
    def run(self, tmp=None, task_vars=None):
        # individual modules might disagree but as the generic the action plugin, pass at this point.
        self._supports_check_mode = True
        self._supports_async = True

        result = super(ActionModule, self).run(tmp, task_vars)
        del tmp  # tmp no longer has any effect

        if not result.get("skipped"):
            user_ssh_transfer_method = None
            is_ssh_transfer_method_updated = False
            org_module_implementation_preferences = None

            try:
                if result.get("invocation", {}).get("module_args"):
                    # avoid passing to modules in case of no_log
                    # should not be set anymore but here for backwards compatibility
                    del result["invocation"]["module_args"]

                # FUTURE: better to let _execute_module calculate this internally?
                wrap_async = self._task.async_val and not self._connection.has_native_async

                # To support multiple Ansible versions we must do some version detection and act accordingly
                version_inf = cli.CLI.version_info(False)
                version_major = version_inf['major']
                version_minor = version_inf['minor']

                # Override the Ansible Connection behavior for this module including execution order for rexx
                scp_transfer_method = "scp"
                org_module_implementation_preferences = self._connection.module_implementation_preferences
                self._connection.module_implementation_preferences = ('.rexx', '.py', '')

                if version_major == 2 and version_minor >= 11:
                    user_ssh_transfer_method = self._connection.get_option('ssh_transfer_method')

                    if user_ssh_transfer_method != scp_transfer_method:
                        self._connection.set_option('ssh_transfer_method', scp_transfer_method)
                        is_ssh_transfer_method_updated = True

                elif version_major == 2 and version_minor <= 10:
                    user_ssh_transfer_method = self._play_context.ssh_transfer_method

                    if user_ssh_transfer_method != scp_transfer_method:
                        self._play_context.ssh_transfer_method = scp_transfer_method
                        is_ssh_transfer_method_updated = True

                if is_ssh_transfer_method_updated:
                    display.vvv(u"SSH transfer method updated from {0} to {1}.".format(user_ssh_transfer_method,
                                scp_transfer_method), host=self._play_context.remote_addr)

                # do work!
                result = merge_hash(
                    result, self._execute_module(task_vars=task_vars, wrap_async=wrap_async)
                )

                # hack to keep --verbose from showing all the setup module result
                # moved from setup module as now we filter out all _ansible_ from result
                if self._task.action == "setup":
                    result["_ansible_verbose_override"] = True

            finally:
                # Restore the users defined option `ssh_transfer_method` and the
                # module implementation preferences (order of module execution
                # by extension)

                if is_ssh_transfer_method_updated:
                    if version_major == 2 and version_minor >= 11:
                        self._connection.set_option('ssh_transfer_method', user_ssh_transfer_method)

                    elif version_major == 2 and version_minor <= 10:
                        self._play_context.ssh_transfer_method = user_ssh_transfer_method

                    display.vvv(u"SSH transfer method restored to {0}".format(user_ssh_transfer_method), host=self._play_context.remote_addr)
                    is_ssh_transfer_method_updated = False

                self._connection.module_implementation_preferences = org_module_implementation_preferences

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
