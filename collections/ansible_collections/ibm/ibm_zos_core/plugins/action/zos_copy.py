# Copyright (c) IBM Corporation 2019, 2020
# Apache License, Version 2.0 (see https://opensource.org/licenses/Apache-2.0)

from __future__ import absolute_import, division, print_function

__metaclass__ = type

import os
import stat
import time
import subprocess

from tempfile import mkstemp, gettempprefix

from ansible.errors import AnsibleError
from ansible.module_utils._text import to_bytes, to_text
from ansible.module_utils.six import string_types
from ansible.module_utils.parsing.convert_bool import boolean
from ansible.plugins.action import ActionBase

from ansible_collections.ibm.ibm_zos_core.plugins.module_utils.data_set import (
    is_member,
    is_data_set,
    extract_member_name
)

from ansible_collections.ibm.ibm_zos_core.plugins.module_utils import encode


class ActionModule(ActionBase):
    def run(self, tmp=None, task_vars=None):
        """ handler for file transfer operations """
        if task_vars is None:
            task_vars = dict()

        result = super(ActionModule, self).run(tmp, task_vars)
        task_args = self._task.args.copy()
        del tmp

        src = task_args.get('src', None)
        b_src = to_bytes(src, errors='surrogate_or_strict')
        dest = task_args.get('dest', None)
        content = task_args.get('content', None)

        # If self._play_context.port is None, that implies the default port 22
        # was used to connect to the remote host.
        sftp_port = task_args.get('sftp_port', self._play_context.port or 22)
        force = _process_boolean(task_args.get('force'), default=True)
        backup = _process_boolean(task_args.get('backup'), default=False)
        local_follow = _process_boolean(task_args.get('local_follow'), default=False)
        remote_src = _process_boolean(task_args.get('remote_src'), default=False)
        is_binary = _process_boolean(task_args.get('is_binary'), default=False)
        ignore_sftp_stderr = _process_boolean(task_args.get("ignore_sftp_stderr"), default=False)
        backup_name = task_args.get("backup_name", None)
        encoding = task_args.get("encoding", None)
        mode = task_args.get("mode", None)
        owner = task_args.get("owner", None)
        group = task_args.get("group", None)

        is_pds = is_src_dir = False
        temp_path = is_uss = is_mvs_dest = copy_member = src_member = None

        if dest:
            if not isinstance(dest, string_types):
                msg = "Invalid type supplied for 'dest' option, it must be a string"
                return self._exit_action(result, msg, failed=True)
            else:
                is_uss = "/" in dest
                is_mvs_dest = is_data_set(dest)
                copy_member = is_member(dest)
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
                src_member = is_member(src)
                if not remote_src:
                    src = os.path.realpath(src)
                    is_src_dir = os.path.isdir(src)
                    is_pds = is_src_dir and is_mvs_dest

        if not src and not content:
            msg = "'src' or 'content' is required"
            return self._exit_action(result, msg, failed=True)

        if encoding and is_binary:
            msg = "The 'encoding' parameter is not valid for binary transfer"
            return self._exit_action(result, msg, failed=True)

        if (not backup) and backup_name is not None:
            msg = "Backup file provided but 'backup' parameter is False"
            return self._exit_action(result, msg, failed=True)

        if not is_uss:
            if mode or owner or group:
                msg = "Cannot specify 'mode', 'owner' or 'group' for MVS destination"
                return self._exit_action(result, msg, failed=True)

        if not isinstance(sftp_port, int) or not 0 < sftp_port <= 65535:
            msg = "Invalid port provided for SFTP. Expected an integer between 0 to 65535."
            return self._exit_action(result, msg, failed=True)

        if (not force) and self._dest_exists(src, dest, task_vars):
            return self._exit_action(result, "Destination exists. No data was copied.")

        if not remote_src:
            if local_follow and not src:
                msg = "No path given for local symlink"
                return self._exit_action(result, msg, failed=True)

            elif src and not os.path.exists(b_src):
                msg = "The local file {0} does not exist".format(src)
                return self._exit_action(result, msg, failed=True)

            elif src and not os.access(b_src, os.R_OK):
                msg = (
                    "The local file {0} does not have appropriate "
                    "read permission".format(src)
                )
                return self._exit_action(result, msg, failed=True)

            if content:
                try:
                    local_content = _write_content_to_temp_file(content)
                    transfer_res = self._copy_to_remote(
                        local_content, sftp_port, ignore_stderr=ignore_sftp_stderr
                    )
                finally:
                    os.remove(local_content)
            else:
                if is_src_dir:
                    path, dirs, files = next(os.walk(src))
                    if dirs:
                        result["msg"] = "Subdirectory found inside source directory"
                        result.update(
                            dict(src=src, dest=dest, changed=False, failed=True)
                        )
                        return result
                    task_args["size"] = sum(
                        os.stat(path + "/" + f).st_size for f in files
                    )
                else:
                    if mode == "preserve":
                        task_args["mode"] = "0{0:o}".format(
                            stat.S_IMODE(os.stat(b_src).st_mode)
                        )
                    task_args["size"] = os.stat(src).st_size
                transfer_res = self._copy_to_remote(
                    src, sftp_port, is_dir=is_src_dir, ignore_stderr=ignore_sftp_stderr
                )

            temp_path = transfer_res.get("temp_path")
            if transfer_res.get("msg"):
                return transfer_res

        task_args.update(
            dict(
                is_uss=is_uss,
                is_pds=is_pds,
                copy_member=copy_member,
                src_member=src_member,
                temp_path=temp_path,
                is_mvs_dest=is_mvs_dest,
                local_charset=encode.Defaults.get_default_system_charset()
            )
        )
        copy_res = self._execute_module(
            module_name="ibm.ibm_zos_core.zos_copy",
            module_args=task_args,
            task_vars=task_vars,
        )

        if copy_res.get("note") and not force:
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
            self._remote_cleanup(dest, copy_res.get("dest_exists"), task_vars)
            return result

        return _update_result(is_binary, copy_res, self._task.args)

    def _copy_to_remote(self, src, port, is_dir=False, ignore_stderr=False):
        """Copy a file or directory to the remote z/OS system """
        ansible_user = self._play_context.remote_user
        ansible_host = self._play_context.remote_addr
        temp_path = "/{0}/{1}".format(gettempprefix(), _create_temp_path_name())
        cmd = ["sftp", "-oPort={0}".format(port), ansible_user + "@" + ansible_host]
        stdin = "put -r {0} {1}".format(src.replace("#", "\\#"), temp_path)

        if is_dir:
            src = src.rstrip("/") if src.endswith("/") else src
            base = os.path.basename(src)
            self._connection.exec_command("mkdir -p {0}/{1}".format(temp_path, base))
        else:
            stdin = stdin.replace(" -r", "", 1)
        transfer_data = subprocess.Popen(
            cmd, stdin=subprocess.PIPE, stdout=subprocess.PIPE, stderr=subprocess.PIPE
        )
        out, err = transfer_data.communicate(to_bytes(stdin))
        err = _detect_sftp_errors(err)

        if transfer_data.returncode != 0 or (err and not ignore_stderr):
            return dict(
                msg="Error transfering source '{0}' to remote z/OS system".format(src),
                rc=transfer_data.returncode,
                stderr=err,
                stderr_lines=err.splitlines(),
                failed=True,
            )

        return dict(temp_path=temp_path)

    def _remote_cleanup(self, dest, dest_exists, task_vars):
        """Remove all files or data sets pointed to by 'dest' on the remote
        z/OS system. The idea behind this cleanup step is that if, for some
        reason, the module fails after copying the data, we want to return the
        remote system to its original state. Which means deleting any newly
        created files or data sets.
        """
        if dest_exists is False:
            if "/" in dest:
                self._connection.exec_command("rm -rf {0}".format(dest))
            else:
                module_args = dict(name=dest, state="absent")
                if is_member(dest):
                    module_args["type"] = "MEMBER"
                self._execute_module(
                    module_name="ibm.ibm_zos_core.zos_data_set",
                    module_args=module_args,
                    task_vars=task_vars,
                )

    def _dest_exists(self, src, dest, task_vars):
        """Determine if destination exists on remote z/OS system"""
        if "/" in dest:
            rc, out, err = self._connection.exec_command("ls -l {0}".format(dest))
            if rc != 0:
                return False
            if len(to_text(out).split("\n")) == 2:
                return True
            if "/" in src:
                src = src.rstrip("/") if src.endswith("/") else src
                dest += "/" + os.path.basename(src)
            else:
                dest += "/" + extract_member_name(src) if is_member(src) else src
            rc, out, err = self._connection.exec_command("ls -l {0}".format(dest))
            if rc != 0:
                return False
        else:
            cmd = "LISTDS '{0}'".format(dest)
            tso_cmd = self._execute_module(
                module_name="ibm.ibm_zos_core.zos_tso_command",
                module_args=dict(commands=[cmd]),
                task_vars=task_vars,
            ).get("output")[0]
            if tso_cmd.get("rc") != 0:
                for line in tso_cmd.get("content"):
                    if "NOT IN CATALOG" in line:
                        return False
        return True

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


def _update_result(is_binary, copy_res, original_args):
    """ Helper function to update output result with the provided values """
    ds_type = copy_res.get("ds_type")
    src = copy_res.get("src")
    note = copy_res.get("note")
    backup_name = copy_res.get("backup_name")
    updated_result = dict(
        dest=copy_res.get("dest"),
        is_binary=is_binary,
        changed=copy_res.get("changed"),
        invocation=dict(module_args=original_args),
    )
    if src:
        updated_result["src"] = src
    if note:
        updated_result["note"] = note
    if backup_name:
        updated_result["backup_name"] = backup_name

    if ds_type == "USS":
        updated_result.update(
            dict(
                gid=copy_res.get("gid"),
                uid=copy_res.get("uid"),
                group=copy_res.get("group"),
                owner=copy_res.get("owner"),
                mode=copy_res.get("mode"),
                state=copy_res.get("state"),
                size=copy_res.get("size"),
            )
        )
        checksum = copy_res.get("checksum")
        if checksum:
            updated_result["checksum"] = checksum

    return updated_result


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
