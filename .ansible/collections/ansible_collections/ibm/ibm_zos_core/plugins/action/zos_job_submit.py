# Copyright (c) IBM Corporation 2019, 2025
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

from ansible.plugins.action import ActionBase
from ansible.errors import AnsibleError, AnsibleFileNotFound
from ansible.utils.display import Display
# from ansible.module_utils._text import to_bytes, to_text
from ansible.module_utils.common.text.converters import to_bytes, to_text
from ansible.module_utils.parsing.convert_bool import boolean
import os

from ansible_collections.ibm.ibm_zos_core.plugins.module_utils import template

from datetime import datetime
from os import path

display = Display()


class ActionModule(ActionBase):
    def run(self, tmp=None, task_vars=None):
        """ handler for file transfer operations """
        self._supports_async = True

        if task_vars is None:
            task_vars = {}

        result = super(ActionModule, self).run(tmp, task_vars)

        if result.get("skipped"):
            return result

        module_args = self._task.args.copy()

        use_template = _process_boolean(module_args.get("use_template"))
        remote_src = module_args.get("remote_src")
        if use_template and remote_src:
            result.update(dict(
                failed=True,
                changed=False,
                msg="Use of Jinja2 templates is only valid for local files. remote_src is set to '{0}' but should be False".format(remote_src)
            ))
            return result

        if not remote_src:

            source = self._task.args.get("src", None)

            result["failed"] = True
            if source is None:
                result["msg"] = "Source is required."
            elif source is not None and os.path.isdir(to_bytes(source, errors="surrogate_or_strict")):
                result["msg"] = "Source must be a file."
            else:
                del result["failed"]

            if result.get("failed"):
                return result

            try:
                source = self._find_needle("files", source)
            except AnsibleError as e:
                result["failed"] = True
                result["msg"] = to_text(e)
                return result

            tmp_dir = self._connection._shell._options.get("remote_tmp")
            temp_file_dir = f'zos_job_submit_{datetime.now().strftime("%Y%m%d%S%f")}'
            dest_path = path.join(tmp_dir, temp_file_dir)

            rc, stdout, stderr = self._connection.exec_command("mkdir -p {0}".format(dest_path))
            display.vvv(f"zos_job_submit: mkdir result {rc}, {stdout}, {stderr} dest path {dest_path}")
            if rc > 0:
                result["failed"] = True
                result["msg"] = f"Failed to create remote temporary directory in {tmp_dir}. Ensure that user has proper access."
                return result

            # The temporary dir was created successfully using ssh connection user.
            rc, stdout, stderr = self._connection.exec_command(F"cd {dest_path} && pwd")
            display.vvv(f"zos_job_submit: remote pwd result {rc}, {stdout}, {stderr} path {dest_path}")
            if rc > 0:
                result["failed"] = True
                result["msg"] = f"Failed to resolve remote temporary directory {dest_path}. Ensure that user has proper access."
                return result
            dest_path = stdout.decode("utf-8").replace("\r", "").replace("\n", "")
            dest_file = path.join(dest_path, path.basename(source))

            source_full = None
            try:
                source_full = self._loader.get_real_file(source)
            except AnsibleFileNotFound as e:
                result["failed"] = True
                result["msg"] = "Source {0} not found. {1}".format(source_full, e)
                self._remove_tmp_path(tmp)
                return result

            rendered_file = None
            if use_template:
                template_parameters = module_args.get("template_parameters", dict())
                encoding = module_args.get("encoding", dict())

                if template_parameters and not template_parameters.get("autoescape", True):
                    display.warning(
                        msg="Disabling autoescaping in Jinja may result in security risks, use with caution."
                    )

                try:
                    renderer = template.create_template_environment(
                        template_parameters,
                        source_full,
                        encoding.get("from", None)
                    )
                    template_dir, rendered_file = renderer.render_file_template(
                        os.path.basename(source_full),
                        task_vars
                    )
                except Exception as err:
                    result["msg"] = to_text(err)
                    result["failed"] = True
                    result["changed"] = False
                    result["invocation"] = dict(module_args=module_args)
                    return result

                source_full = rendered_file
                if os.path.exists(rendered_file):
                    with open(rendered_file, 'r') as file:
                        rendered_content = file.read()
                        display.vvv(u"Template Content {0}:\n{1}".format(os.path.basename(source), rendered_content), host=self._play_context.remote_addr)
                else:
                    display.vvv(u"Template file {0} does not exist.".format(rendered_file))

            result = {}
            copy_module_args = {}
            module_args = self._task.args.copy()

            copy_module_args.update(
                dict(
                    src=source_full,
                    dest=dest_file,
                    mode="0666",
                    replace=True,
                    encoding=module_args.get('encoding'),
                    remote_src=False,
                )
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
                shared_loader_obj=self._shared_loader_obj
            )

            result.update(copy_action.run(task_vars=task_vars))
            if result.get("msg") is None:
                module_args["src"] = dest_file
                result.update(
                    self._execute_module(
                        module_name="ibm.ibm_zos_core.zos_job_submit",
                        module_args=module_args,
                        task_vars=task_vars,
                        wrap_async=self._task.async_val
                    )
                )
            else:
                result.update(dict(failed=True))

        else:
            result.update(
                self._execute_module(
                    module_name="ibm.ibm_zos_core.zos_job_submit",
                    module_args=module_args,
                    task_vars=task_vars,
                    wrap_async=self._task.async_val
                )
            )

        def delete_dict_entries(entries, dictionary):
            """ Deletes entries from a dictionary when provided key and dictionary.

                Arguments:
                    entries    (tuple)  - entries to delete from dictionary
                    dictionary (dic)    - dictionary to remove entries
            """
            for key in entries:
                if key in dictionary:
                    del dictionary[key]

        # Currently the direction is undecided if we should continue to use the
        # community action plugins or transition to SFTP, so this code
        # can remain should we want to clean up unrelated response values.
        # entries = ('checksum', 'dest', 'gid', 'group', 'md5sum', 'mode', 'owner', 'size', 'src', 'state', 'uid')
        # delete_dict_entries(entries, result)
        return result


def _process_boolean(arg, default=False):
    try:
        return boolean(arg)
    except TypeError:
        return default
