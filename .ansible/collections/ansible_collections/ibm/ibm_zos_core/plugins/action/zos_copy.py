# Copyright (c) IBM Corporation 2019, 2026
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

import os
import stat
import time
import shutil

from tempfile import mkstemp

from ansible.errors import AnsibleError
from ansible.module_utils._text import to_text
from ansible.module_utils.six import string_types
from ansible.module_utils.parsing.convert_bool import boolean
from ansible.plugins.action import ActionBase
from ansible.utils.display import Display
from ansible import cli

from ansible_collections.ibm.ibm_zos_core.plugins.module_utils import encode

from ansible_collections.ibm.ibm_zos_core.plugins.module_utils import template

display = Display()


class ActionModule(ActionBase):
    def run(self, tmp=None, task_vars=None):
        """ handler for file transfer operations """
        self._supports_async = True

        if task_vars is None:
            task_vars = dict()

        result = super(ActionModule, self).run(tmp, task_vars)
        task_args = self._task.args.copy()
        del tmp

        src = task_args.get('src', None)
        dest = task_args.get('dest', None)
        content = task_args.get('content', None)

        replace = _process_boolean(task_args.get('replace'), default=True)
        backup = _process_boolean(task_args.get('backup'), default=False)
        local_follow = _process_boolean(task_args.get('local_follow'), default=False)
        remote_src = _process_boolean(task_args.get('remote_src'), default=False)
        binary = _process_boolean(task_args.get('binary'), default=False)
        force = _process_boolean(task_args.get('force'), default=False)
        executable = _process_boolean(task_args.get('executable'), default=False)
        asa_text = _process_boolean(task_args.get('asa_text'), default=False)
        ignore_sftp_stderr = _process_boolean(task_args.get("ignore_sftp_stderr"), default=True)
        backup_name = task_args.get("backup_name", None)
        encoding = task_args.get("encoding", None)
        mode = task_args.get("mode", None)
        owner = task_args.get("owner", None)
        group = task_args.get("group", None)

        is_src_dir = False
        temp_path = is_uss = None

        self.tmp_dir = None

        if dest:
            if not isinstance(dest, string_types):
                msg = "Invalid type supplied for 'dest' option, it must be a string"
                return self._exit_action(result, msg, failed=True)
            else:
                is_uss = "/" in dest
        else:
            msg = "Destination is required"
            return self._exit_action(result, msg, failed=True)

        if src:
            if content:
                msg = "Either 'src' or 'content' can be provided; not both."
                return self._exit_action(result, msg, failed=True)

            elif not isinstance(src, string_types):
                msg = "Invalid type supplied for 'src' option, it must be a string"
                return self._exit_action(result, msg, failed=True)

            elif len(src) < 1 or len(dest) < 1:
                msg = "'src' or 'dest' must not be empty"
                return self._exit_action(result, msg, failed=True)
            else:
                if not remote_src:
                    if src.startswith('~'):
                        src = os.path.expanduser(src)
                    src = os.path.realpath(src)
                    is_src_dir = os.path.isdir(src)

        if not src and not content:
            msg = "'src' or 'content' is required"
            return self._exit_action(result, msg, failed=True)

        if encoding and binary:
            msg = "The 'encoding' parameter is not valid for binary transfer"
            return self._exit_action(result, msg, failed=True)

        if (not backup) and backup_name is not None:
            msg = "Backup file provided but 'backup' parameter is False"
            return self._exit_action(result, msg, failed=True)

        if binary and asa_text:
            msg = "Both 'binary' and 'asa_text' are True. Unable to copy binary data as an ASA text file."
            return self._exit_action(result, msg, failed=True)

        if executable and asa_text:
            msg = "Both 'executable' and 'asa_text' are True. Unable to copy an executable as an ASA text file."
            return self._exit_action(result, msg, failed=True)

        use_template = _process_boolean(task_args.get("use_template"), default=False)
        if remote_src and use_template:
            msg = "Use of Jinja2 templates is only valid for local files, remote_src cannot be set to true."
            return self._exit_action(result, msg, failed=True)

        if not is_uss:
            if mode or owner or group:
                msg = "Cannot specify 'mode', 'owner' or 'group' for MVS destination"
                return self._exit_action(result, msg, failed=True)

        if force:
            display.warning(
                msg="Using force uses operations that are subject to race conditions and can lead to data loss, use with caution.")
        template_dir = None

        if not remote_src:
            if local_follow and not src:
                msg = "No path given for local symlink"
                return self._exit_action(result, msg, failed=True)

            elif src and not os.path.exists(src):
                msg = "The local file {0} does not exist".format(src)
                return self._exit_action(result, msg, failed=True)

            elif src and not os.access(src, os.R_OK):
                msg = (
                    "The local file {0} does not have appropriate "
                    "read permission".format(src)
                )
                return self._exit_action(result, msg, failed=True)

            if content:
                try:
                    local_content = _write_content_to_temp_file(content)
                    transfer_res = self._copy_to_remote(
                        local_content, ignore_stderr=ignore_sftp_stderr, task_vars=task_vars
                    )
                finally:
                    os.remove(local_content)
            else:
                if is_src_dir:
                    path, dirs, files = next(os.walk(src))
                    if not is_uss and dirs:
                        result["msg"] = "Cannot copy a source directory with subdirectories to a data set, the destination must be another directory"
                        result.update(
                            dict(src=src, dest=dest, changed=False, failed=True)
                        )
                        return result

                    if use_template:
                        template_parameters = task_args.get("template_parameters", dict())

                        if template_parameters and not template_parameters.get("autoescape", True):
                            display.warning(
                                msg="Disabling autoescaping in Jinja may result in security risks, use with caution."
                            )

                        if encoding:
                            template_encoding = encoding.get("from", None)
                        else:
                            template_encoding = None

                        try:
                            renderer = template.create_template_environment(
                                template_parameters,
                                src,
                                template_encoding
                            )
                            template_dir, rendered_dir = renderer.render_dir_template(
                                task_vars.get("vars", dict())
                            )
                        except Exception as err:
                            if template_dir:
                                shutil.rmtree(template_dir, ignore_errors=True)
                            return self._exit_action(result, str(err), failed=True)

                        src = rendered_dir

                else:
                    if mode == "preserve":
                        task_args["mode"] = "0{0:o}".format(
                            stat.S_IMODE(os.stat(src).st_mode)
                        )

                    if use_template:
                        template_parameters = task_args.get("template_parameters", dict())

                        if template_parameters and not template_parameters.get("autoescape", True):
                            display.warning(
                                msg="Disabling autoescaping in Jinja may result in security risks, use with caution."
                            )

                        if encoding:
                            template_encoding = encoding.get("from", None)
                        else:
                            template_encoding = None

                        try:
                            renderer = template.create_template_environment(
                                template_parameters,
                                src,
                                template_encoding
                            )
                            template_dir, rendered_file = renderer.render_file_template(
                                os.path.basename(src),
                                task_vars.get("vars", dict())
                            )
                        except Exception as err:
                            if template_dir:
                                shutil.rmtree(template_dir, ignore_errors=True)
                            return self._exit_action(result, str(err), failed=True)

                        src = rendered_file
                        if os.path.exists(rendered_file):
                            file_n = os.path.basename(rendered_file)
                            with open(rendered_file, 'r') as file:
                                rendered_content = file.read()
                                display.vvv(f"Template Content ({file_n}):\n{rendered_content}", host=self._play_context.remote_addr)
                        else:
                            display.vvv(u"Template File {0} does not exist.".format(rendered_file))

                display.vvv(u"ibm_zos_copy calculated size: {0}".format(os.stat(src).st_size), host=self._play_context.remote_addr)
                transfer_res = self._copy_to_remote(
                    src, is_dir=is_src_dir, ignore_stderr=ignore_sftp_stderr, task_vars=task_vars
                )

            temp_path = transfer_res.get("temp_path")
            if transfer_res.get("msg"):
                return transfer_res
            display.vvv(u"ibm_zos_copy temp path: {0}".format(transfer_res.get("temp_path")), host=self._play_context.remote_addr)

        if not encoding:
            encoding = {
                "from": encode.Defaults.get_default_system_charset(),
            }

        """
        We format temp_path correctly to pass it as src option to the module,
        we keep the original source to return to the user and avoid confusion
        by returning the temp_path created.
        """
        original_src = task_args.get("src")
        if original_src:
            if not remote_src:
                if original_src.endswith("/"):
                    src = temp_path + "/"
                else:
                    src = temp_path
        else:
            src = temp_path

        task_args.update(
            dict(
                src=src,
                encoding=encoding,
            )
        )

        copy_res = self._execute_module(
            module_name="ibm.ibm_zos_core.zos_copy",
            module_args=task_args,
            task_vars=task_vars,
            wrap_async=self._task.async_val
        )

        # Erasing all rendered Jinja2 templates from the controller.
        if template_dir:
            shutil.rmtree(template_dir, ignore_errors=True)
        # Remove temporary directory from remote
        if self.tmp_dir is not None:
            path = os.path.normpath(f"{self.tmp_dir}/ansible-zos-copy")
            self._connection.exec_command(f"rm -rf {path}*")

        if copy_res.get("note") and not replace:
            result["note"] = copy_res.get("note")
            return result

        if copy_res.get("msg"):
            result.update(
                dict(
                    msg=copy_res.get("msg"),
                    stdout=copy_res.get("stdout") or copy_res.get("module_stdout"),
                    stderr=copy_res.get("stderr") or copy_res.get("module_stderr"),
                    stdout_lines=copy_res.get("stdout_lines"),
                    stderr_lines=copy_res.get("stderr_lines"),
                    rc=copy_res.get("rc"),
                    invocation=dict(module_args=self._task.args),
                )
            )
            if backup or backup_name:
                result["backup_name"] = copy_res.get("backup_name")
            return result

        # Erasing all rendered Jinja2 templates from the controller.
        if template_dir:
            shutil.rmtree(template_dir, ignore_errors=True)

        return copy_res

    def _copy_to_remote(self, src, is_dir=False, ignore_stderr=False, task_vars=None):
        """Copy a file or directory to the remote z/OS system """
        self.tmp_dir = self._connection._shell._options.get("remote_tmp")
        temp_path = os.path.join(self.tmp_dir, _create_temp_path_name())

        rc, stdout, stderr = self._connection.exec_command("mkdir -p {0}".format(temp_path))
        display.vvv(f"ibm_zos_copy: remote mkdir result {rc}, {stdout}, {stderr} path {temp_path}")
        if rc > 0:
            msg = f"Failed to create remote temporary directory in {self.tmp_dir}. Ensure that user has proper access."
            return self._exit_action({}, msg, failed=True)

        # The temporary dir was created successfully using ssh connection user.
        rc, stdout, stderr = self._connection.exec_command(F"cd {temp_path} && pwd")
        display.vvv(f"ibm_zos_copy: remote pwd result {rc}, {stdout}, {stderr} path {temp_path}")
        if rc > 0:
            msg = f"Failed to resolve remote temporary directory {temp_path}. Ensure that user has proper access."
            return self._exit_action({}, msg, failed=True)
        temp_path = stdout.decode("utf-8").replace("\r", "").replace("\n", "")

        _sftp_action = 'put'

        temp_path = os.path.join(temp_path, os.path.basename(src))
        _src = src.replace("#", "\\#")
        full_temp_path = temp_path

        if is_dir:
            src = src.rstrip("/") if src.endswith("/") else src
            temp_path = os.path.dirname(temp_path)
            base = os.path.basename(src)
            self._connection.exec_command("mkdir -p {0}/{1}".format(temp_path, base))
            _sftp_action += ' -r'    # add '-r` to clone the source trees

        # To support multiple Ansible versions we must do some version detection and act accordingly
        version_inf = cli.CLI.version_info(False)
        version_major = version_inf['major']
        version_minor = version_inf['minor']

        # Override the Ansible Connection behavior for this module and track users configuration
        sftp_transfer_method = "sftp"
        user_ssh_transfer_method = None
        is_ssh_transfer_method_updated = False

        try:
            if version_major == 2 and version_minor >= 11:
                user_ssh_transfer_method = self._connection.get_option('ssh_transfer_method')

                if user_ssh_transfer_method != sftp_transfer_method:
                    self._connection.set_option('ssh_transfer_method', sftp_transfer_method)
                    is_ssh_transfer_method_updated = True

            elif version_major == 2 and version_minor <= 10:
                user_ssh_transfer_method = self._play_context.ssh_transfer_method

                if user_ssh_transfer_method != sftp_transfer_method:
                    self._play_context.ssh_transfer_method = sftp_transfer_method
                    is_ssh_transfer_method_updated = True

            if is_ssh_transfer_method_updated:
                display.vvv(u"ibm_zos_copy SSH transfer method updated from {0} to {1}.".format(user_ssh_transfer_method,
                            sftp_transfer_method), host=self._play_context.remote_addr)

            display.vvv(u"ibm_zos_copy: {0} {1} TO {2}".format(_sftp_action, _src, temp_path), host=self._play_context.remote_addr)

            (returncode, stdout, stderr) = self._connection._file_transport_command(_src, temp_path, _sftp_action)

            display.vvv(u"ibm_zos_copy return code: {0}".format(returncode), host=self._play_context.remote_addr)
            display.vvv(u"ibm_zos_copy stdout: {0}".format(stdout), host=self._play_context.remote_addr)
            display.vvv(u"ibm_zos_copy stderr: {0}".format(stderr), host=self._play_context.remote_addr)

            ansible_verbosity = None
            ansible_verbosity = display.verbosity
            display.vvv(u"play context verbosity: {0}".format(ansible_verbosity), host=self._play_context.remote_addr)

            # ************************************************************************* #
            # When plugin shh connection member _build_command(..) detects verbosity    #
            # greater than 3, it constructs a command that includes verbosity like      #
            # 'EXEC sftp -b - -vvv ...' where this then is returned in the connections  #
            # stream as 'stderr' and if a user has not set ignore_stderr it will fail   #
            # the modules execution. So in cases where verbosity                        #
            # (ansible.cfg verbosity = n || CLI -vvv) are collectively summed and       #
            # amount to greater than 3, ignore_stderr will be set to 'True' so that     #
            # 'err' which will not be None won't fail the module. 'stderr' does not     #
            # in our z/OS case actually mean an error happened, it just so happens      #
            # the verbosity is returned as 'stderr'.                                    #
            # ************************************************************************* #

            err = _detect_sftp_errors(stderr)

            if ansible_verbosity > 3:
                ignore_stderr = True

            if returncode != 0 or (err and not ignore_stderr):
                return dict(
                    msg="Error transferring source '{0}' to remote z/OS system".format(src),
                    rc=returncode,
                    stderr=err,
                    stderr_lines=err.splitlines(),
                    failed=True,
                )

        finally:
            # Restore the users defined option `ssh_transfer_method` if it was overridden
            if is_ssh_transfer_method_updated:
                if version_major == 2 and version_minor >= 11:
                    self._connection.set_option('ssh_transfer_method', user_ssh_transfer_method)

                elif version_major == 2 and version_minor <= 10:
                    self._play_context.ssh_transfer_method = user_ssh_transfer_method

                display.vvv(u"ibm_zos_copy SSH transfer method restored to {0}".format(user_ssh_transfer_method), host=self._play_context.remote_addr)
                is_ssh_transfer_method_updated = False

        return dict(temp_path=full_temp_path)

    def _exit_action(self, result, msg, failed=False):
        """Exit action plugin with a message"""
        result.update(
            dict(
                changed=False,
                failed=failed,
                invocation=dict(module_args=self._task.args),
            )
        )
        if failed:
            result["msg"] = msg
        else:
            result["note"] = msg
        return result


def _process_boolean(arg, default=False):
    try:
        return boolean(arg)
    except TypeError:
        return default


def _create_temp_path_name():
    """Create a temporary path name"""
    current_date = time.strftime("D%y%m%d", time.localtime())
    current_time = time.strftime("T%H%M%S", time.localtime())
    return "ansible-zos-copy-payload-{0}-{1}".format(current_date, current_time)


def _detect_sftp_errors(stderr):
    """Detects if the stderr of the SFTP command contains any errors.
       The SFTP command usually returns zero return code even if it
       encountered an error while transferring data. Hence the need to parse
       its stderr to determine what error it ran into.
    """
    # The first line of stderr is a connection acknowledgement,
    # which can be ignored
    lines = to_text(stderr).splitlines()
    if len(lines) > 1:
        return "".join(lines[1:])
    return ""


def _write_content_to_temp_file(content):
    """Write given content to a temp file and return its path """
    fd, path = mkstemp()
    try:
        with os.fdopen(fd, "w") as infile:
            infile.write(content)
    except (OSError, IOError) as err:
        os.remove(path)
        raise AnsibleError(
            "Unable to write content to temporary file: {0}".format(repr(err))
        )
    return path
