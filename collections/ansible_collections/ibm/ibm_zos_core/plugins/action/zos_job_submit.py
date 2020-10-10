# Copyright (c) IBM Corporation 2019, 2020
# GNU General Public License v3.0+ (see COPYING or https://www.gnu.org/licenses/gpl-3.0.txt)

from __future__ import absolute_import, division, print_function

__metaclass__ = type

from ansible.plugins.action import ActionBase
from ansible.errors import AnsibleError, AnsibleFileNotFound
from ansible.module_utils._text import to_bytes, to_text
import os


class ActionModule(ActionBase):
    def run(self, tmp=None, task_vars=None):
        """ handler for file transfer operations """
        if task_vars is None:
            task_vars = dict()

        result = super(ActionModule, self).run(tmp, task_vars)

        if result.get("skipped"):
            return result

        module_args = self._task.args.copy()
        if module_args["location"] == "LOCAL":

            source = self._task.args.get("src", None)

            # Get a temporary file on the managed node
            dest_path = self._execute_module(
                module_name="tempfile", module_args={}, task_vars=task_vars,
            ).get("path")

            result["failed"] = True
            if source is None or dest_path is None:
                result["msg"] = "src and dest are required"
            elif source is not None and source.endswith("/"):
                result["msg"] = "src must be a file"
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

            if os.path.isdir(to_bytes(source, errors="surrogate_or_strict")):
                result["failed"] = True
                result["msg"] = to_text(u"NOT SUPPORTING THE DIRECTORY.")
                return result

            changed = False
            module_return = dict(changed=False)

            if tmp is None or "-tmp-" not in tmp:
                tmp = self._make_tmp_path()

            try:
                source_full = self._loader.get_real_file(source)
                source_rel = os.path.basename(source)
            except AnsibleFileNotFound as e:
                result["failed"] = True
                result["msg"] = "could not find src=%s, %s" % (source_full, e)
                self._remove_tmp_path(tmp)
                return result

            # if self._connection._shell.path_has_trailing_slash(dest):
            #     dest_file = self._connection._shell.join_path(dest, source_rel)
            # else:
            dest_file = self._connection._shell.join_path(dest_path)

            dest_status = self._execute_remote_stat(
                dest_file, all_vars=task_vars, follow=False
            )

            if dest_status["exists"] and dest_status["isdir"]:
                self._remove_tmp_path(tmp)
                result["failed"] = True
                result["msg"] = "can not use content with a dir as dest"
                return result

            tmp_src = self._connection._shell.join_path(tmp, "source")

            remote_path = None
            remote_path = self._transfer_file(source_full, tmp_src)

            if remote_path:
                self._fixup_perms2((tmp, remote_path))

            result = {}
            copy_module_args = {}
            module_args = self._task.args.copy()
            module_args["temp_file"] = dest_path

            copy_module_args.update(
                dict(
                    src=tmp_src,
                    dest=dest_path,
                    mode="0600",
                    _original_basename=source_rel,
                )
            )
            result.update(
                self._execute_module(
                    module_name="copy",
                    module_args=copy_module_args,
                    task_vars=task_vars,
                )
            )
            result.update(
                self._execute_module(
                    module_name="ibm.ibm_zos_core.zos_job_submit",
                    module_args=module_args,
                    task_vars=task_vars,
                )
            )
        else:
            result.update(
                self._execute_module(
                    module_name="ibm.ibm_zos_core.zos_job_submit",
                    module_args=module_args,
                    task_vars=task_vars,
                )
            )

        return result
