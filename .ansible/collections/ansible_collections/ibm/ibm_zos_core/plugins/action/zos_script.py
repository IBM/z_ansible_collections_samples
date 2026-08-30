# Copyright (c) IBM Corporation 2023, 2025
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#     http://www.apache.org/licenses/LICENSE-2.0
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.

from __future__ import absolute_import, division, print_function
__metaclass__ = type

import shlex
from os import path

from ansible.plugins.action import ActionBase
from ansible.module_utils.parsing.convert_bool import boolean

from ansible.utils.display import Display
display = Display()


class ActionModule(ActionBase):
    def run(self, tmp=None, task_vars=None):
        self._supports_async = True
        if task_vars is None:
            task_vars = dict()

        result = super(ActionModule, self).run(tmp, task_vars)
        if result.get("skipped"):
            return result

        module_args = self._task.args.copy()

        # First separating the command into the script path and its args
        # if they are present.
        cmd_parts = shlex.split(module_args.get('cmd'))
        if len(cmd_parts) == 0:
            result.update(dict(
                changed=False,
                failed=True,
                invocation=dict(module_args=self._task.args),
                msg="The command could not be validated, please check that it conforms to shell syntax."
            ))
            return result

        script_path = cmd_parts[0]
        script_args = ' '.join(cmd_parts[1:]) if len(cmd_parts) > 1 else ""
        remote_src = self._process_boolean(module_args.get('remote_src'))
        user_cmd = tempfile_path = None

        # Copying the script when it's a local file.
        if not remote_src:
            script_path = path.abspath(path.normpath(script_path))
            script_name = path.basename(script_path)
            # Accessing the globally-defined temporary directory
            # that Ansible expects to be used.
            tmp_path = self._connection._shell._options.get("remote_tmp")

            # Getting a temporary path for the script.
            tempfile_args = dict(
                state="file",
                path=tmp_path,
                prefix="zos_script.",
                suffix=".{0}".format(script_name)
            )

            tempfile_result = self._execute_module(
                module_name="ansible.builtin.tempfile",
                module_args=tempfile_args,
                task_vars=task_vars
            )
            result.update(tempfile_result)

            if not result.get("changed") or result.get("failed"):
                result.update(dict(
                    changed=False,
                    failed=True,
                    invocation=dict(
                        module_args=self._task.args,
                        tempfile_args=tempfile_result.get('invocation', dict()).get('module_args')
                    ),
                    msg="An error ocurred while trying to create a tempfile for the script."
                ))
                return result

            tempfile_path = tempfile_result.get('path')

            # Letting zos_copy handle the transfer of the script.
            copy_module_args = dict(
                src=script_path,
                dest=tempfile_path,
                replace=True,
                binary=False,
                encoding=module_args.get('encoding'),
                use_template=module_args.get('use_template', False),
                template_parameters=module_args.get('template_parameters', dict())
            )
            copy_task = self._task.copy()
            copy_task.args = copy_module_args
            # Making the zos_copy task run synchronously every time.
            copy_task.async_val = 0
            copy_action = self._shared_loader_obj.action_loader.get(
                'ibm.ibm_zos_core.zos_copy',
                task=copy_task,
                connection=self._connection,
                play_context=self._play_context,
                loader=self._loader,
                templar=self._templar,
                shared_loader_obj=self._shared_loader_obj)
            zos_copy_result = copy_action.run(task_vars=task_vars)
            result.update(zos_copy_result)

            if not result.get("changed") or result.get("failed"):
                result.update(dict(
                    changed=False,
                    failed=True,
                    invocation=dict(
                        module_args=self._task.args,
                        tempfile_args=tempfile_result.get('invocation', dict()).get('module_args'),
                        zos_copy_args=zos_copy_result.get('invocation', dict()).get('module_args')
                    ),
                    msg="An error ocurred while trying to copy the script to the managed node: {0}.".format(
                        zos_copy_result.get('msg')
                    )
                ))
                return result

            # We're going to shadow the command supplied by the user with the remote
            # tempfile we just created.
            user_cmd = module_args.get('cmd')
            module_args['cmd'] = '{0} {1}'.format(tempfile_path, script_args)

        module_result = self._execute_module(
            module_name='ibm.ibm_zos_core.zos_script',
            module_args=module_args,
            task_vars=task_vars,
            wrap_async=self._task.async_val
        )

        result = module_result
        if result.get('changed') and tempfile_path:
            result['tempfile_path'] = tempfile_path
            # The cmd field will return using the tempfile created, so we
            # restore it to what the user supplied.
            result['cmd'] = user_cmd

        return result

    def _process_boolean(self, arg, default=False):
        try:
            return boolean(arg)
        except TypeError:
            return default
