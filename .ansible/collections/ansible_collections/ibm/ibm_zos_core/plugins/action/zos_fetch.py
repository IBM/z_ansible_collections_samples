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

import os
import re

from hashlib import sha256
# from ansible.module_utils._text import to_bytes, to_text
from ansible.module_utils.common.text.converters import to_bytes, to_text
from ansible.module_utils.six import string_types
from ansible.module_utils.parsing.convert_bool import boolean
from ansible.plugins.action import ActionBase
from ansible.errors import AnsibleError
from ansible.utils.display import Display
from ansible import cli

from ansible_collections.ibm.ibm_zos_core.plugins.module_utils import encode, validation, data_set

SUPPORTED_DS_TYPES = frozenset({
    "PS", "SEQ", "BASIC",
    "PO", "PE", "PDS", "PDSE",
    "VSAM", "KSDS",
    "GDG",
    "USS"
})

display = Display()


def _update_result(result, src, dest, ds_type="USS", binary=False):
    """ Helper function to update output result with the provided values """
    data_set_types = {
        "PS": "Sequential",
        "SEQ": "Sequential",
        "BASIC": "Sequential",
        "PO": "Partitioned",
        "PDS": "Partitioned",
        "PE": "Partitioned Extended",
        "PDSE": "Partitioned Extended",
        "VSAM": "VSAM",
        "KSDS": "VSAM",
        "GDG": "Generation Data Group",
        "USS": "USS",
    }
    updated_result = dict((k, v) for k, v in result.items())
    updated_result.update(
        {
            "src": src,
            "dest": dest,
            "data_set_type": data_set_types[ds_type],
            "binary": binary,
        }
    )
    return updated_result


def _process_boolean(arg, default=False):
    """ Return boolean representation of arg.
        If arg is None, return the default value
    """
    try:
        return boolean(arg)
    except TypeError:
        return default


def _get_file_checksum(src):
    """ Calculate SHA256 hash for a given file """
    b_src = to_bytes(src)
    if not os.path.exists(b_src) or os.path.isdir(b_src):
        return None
    blksize = 64 * 1024
    hash_digest = sha256()
    try:
        with open(to_bytes(src, errors="surrogate_or_strict"), "rb") as infile:
            block = infile.read(blksize)
            while block:
                hash_digest.update(block)
                block = infile.read(blksize)
    except Exception as err:
        raise AnsibleError("Unable to calculate checksum: {0}".format(str(err)))
    return hash_digest.hexdigest()


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


class ActionModule(ActionBase):
    def run(self, tmp=None, task_vars=None):
        result = super(ActionModule, self).run(tmp, task_vars)
        del tmp

        # ********************************************************** #
        #                 Parameter initializations                  #
        # ********************************************************** #

        src = self._task.args.get('src')
        dest = self._task.args.get('dest')
        encoding = self._task.args.get('encoding', None)
        flat = _process_boolean(self._task.args.get('flat'), default=False)
        fail_on_missing = _process_boolean(self._task.args.get('fail_on_missing'), default=True)
        binary = _process_boolean(self._task.args.get('binary'))
        ignore_sftp_stderr = _process_boolean(
            self._task.args.get("ignore_sftp_stderr"), default=True
        )
        validate_checksum = _process_boolean(
            self._task.args.get("validate_checksum"), default=True
        )

        # ********************************************************** #
        #                 Parameter checks                           #
        # ********************************************************** #

        msg = None
        if src is None or dest is None:
            msg = "Source and destination are required"
        elif not isinstance(src, string_types):
            msg = "Invalid type supplied for 'source' option, value must be a string"
        elif not isinstance(dest, string_types):
            msg = (
                "Invalid type supplied for 'destination' option, value must be a string"
            )
        elif len(src) < 1 or len(dest) < 1:
            msg = "Source and destination parameters must not be empty"

        if msg:
            result["msg"] = msg
            result["failed"] = True
            return result

        ds_type = None
        fetch_member = data_set.is_member(src)
        if fetch_member:
            member_name = src[src.find("(") + 1: src.find(")")]
        src = self._connection._shell.join_path(src)
        src = self._remote_expand_user(src)

        # To manage relative paths we verify and add the current directory
        # Required to be cast to str
        if not (dest.startswith("/")):
            if dest.startswith("~"):
                dest = os.path.expanduser(dest)
            dest = os.path.realpath(dest)
            dest = os.path.join(os.getcwd(), dest)
            dest = f"{dest}/" if os.path.isdir(dest) else str(dest)
        # ********************************************************** #
        #                Execute module on remote host               #
        # ********************************************************** #
        new_module_args = self._task.args.copy()
        new_module_args.update(dest=dest)
        encoding_to = None
        if encoding:
            encoding_to = encoding.get("to", None)
        if encoding is None or encoding_to is None:
            new_module_args.update(
                dict(encoding=dict(to=encode.Defaults.get_default_system_charset()))
            )
        remote_path = None

        try:
            fetch_res = self._execute_module(
                module_name="ibm.ibm_zos_core.zos_fetch",
                module_args=new_module_args,
                task_vars=task_vars
            )
            ds_type = fetch_res.get("ds_type")
            src = fetch_res.get("src")
            remote_path = fetch_res.get("remote_path")
            # Create a dictionary that is a schema for the return values
            result = dict(
                src="",
                dest="",
                binary=False,
                checksum="",
                changed=False,
                data_set_type="",
                msg="",
                stdout="",
                stderr="",
                stdout_lines=[],
                stderr_lines=[],
                rc=0,
                encoding=new_module_args.get("encoding"),
            )
            # Populate it with the modules response
            result["src"] = fetch_res.get("src")
            result["dest"] = fetch_res.get("dest")
            result["binary"] = fetch_res.get("binary", False)
            result["checksum"] = fetch_res.get("checksum")
            result["changed"] = fetch_res.get("changed", False)
            result["data_set_type"] = fetch_res.get("data_set_type")
            result["msg"] = fetch_res.get("msg")
            result["stdout"] = fetch_res.get("stdout")
            result["stderr"] = fetch_res.get("stderr")
            result["stdout_lines"] = fetch_res.get("stdout_lines")
            result["stderr_lines"] = fetch_res.get("stderr_lines")
            result["rc"] = fetch_res.get("rc", 0)
            result["encoding"] = fetch_res.get("encoding")

            if fetch_res.get("failed", False):
                result["stdout"] = fetch_res.get("stdout") or fetch_res.get(
                    "module_stdout"
                )
                result["stderr"] = fetch_res.get("stderr") or fetch_res.get(
                    "module_stderr"
                )
                result["failed"] = True
                return result
            if "No data was fetched." in result["msg"]:
                if fail_on_missing:
                    result["failed"] = True
                return result

        except Exception as err:
            result["msg"] = f"Failure during module execution {msg}"
            result["stderr"] = str(err)
            result["stderr_lines"] = str(err).splitlines()
            result["failed"] = True
            return result

        # ********************************************************** #
        #  Determine destination path:                               #
        #  1. If the 'flat' parameter is 'false', then hostname      #
        #     will be appended to dest.                              #
        #  2. If 'flat' is 'true', then dest must not be a directory.#
        #     If it is a directory, a trailing forward slash must    #
        #     be added.                                              #
        # ********************************************************** #

        # ********************************************************** #
        #  If a data set member is being fetched, extract the member #
        #  name and update dest with the name of the member.         #
        #  For instance: If src is: USER.TEST.PROCLIB(DATA)          #
        #  and dest is: /tmp/, then updated dest would be /tmp/DATA  #
        # ********************************************************** #
        if os.path.sep not in self._connection._shell.join_path("a", ""):
            src = self._connection._shell._unquote(src)
            source_local = src.replace("\\", "/")
        else:
            source_local = src

        dest = os.path.expanduser(dest)
        if flat:
            if os.path.isdir(
                to_bytes(dest, errors="surrogate_or_strict")
            ) and not dest.endswith(os.sep):
                result["msg"] = (
                    "dest is an existing directory, append a forward "
                    "slash to the dest if you want to fetch src into "
                    "that directory"
                )
                result["failed"] = True
                return result
            if dest.endswith(os.sep):
                if fetch_member:
                    base = os.path.dirname(dest)
                    dest = os.path.join(validation.validate_safe_path(base), validation.validate_safe_path(member_name))
                    display.vvv(u"This is how dest looks {0}".format(dest), host=self._play_context.remote_addr)
                else:
                    base = os.path.basename(source_local)
                    dest = os.path.join(validation.validate_safe_path(dest), validation.validate_safe_path(base))
                    display.vvv(u"This is how dest looks {0}".format(dest), host=self._play_context.remote_addr)
            if not dest.startswith("/"):
                dest = self._loader.path_dwim(dest)
        else:
            if "inventory_hostname" in task_vars:
                target_name = task_vars["inventory_hostname"]
            else:
                target_name = self._play_context.remote_addr
            suffix = member_name if fetch_member else source_local
            dest = "{0}/{1}/{2}".format(
                self._loader.path_dwim(dest), target_name, suffix
            )
            try:
                dirname = os.path.dirname(dest).replace("//", "/")
                if not os.path.exists(dirname):
                    os.makedirs(dirname)
            except OSError as err:
                result["msg"] = "Unable to create destination directory {0}".format(
                    dirname
                )
                result["stderr"] = str(err)
                result["stderr_lines"] = str(err).splitlines()
                result["failed"] = True
                return result

        dest = dest.replace("//", "/")
        local_checksum = _get_file_checksum(dest)

        # ********************************************************** #
        # Fetch remote data.                                         #
        # ********************************************************** #
        try:
            if ds_type in SUPPORTED_DS_TYPES:
                if ds_type == "PO" and os.path.isfile(dest) and not fetch_member:
                    result["msg"] = "Destination must be a directory to fetch a partitioned data set"
                    result["failed"] = True
                    return result
                if ds_type == "GDG" and os.path.isfile(dest):
                    result["msg"] = "Destination must be a directory to fetch a generation data group"
                    result["failed"] = True
                    return result

                fetch_content = self._transfer_remote_content(
                    dest,
                    remote_path,
                    ds_type,
                    ignore_stderr=ignore_sftp_stderr,
                )
                if fetch_content.get("msg"):
                    result.update(fetch_content)
                    return result

                if validate_checksum and ds_type != "GDG" and ds_type != "PO" and not binary:
                    new_checksum = _get_file_checksum(dest)
                    result["changed"] = local_checksum != new_checksum
                    result["checksum"] = new_checksum
                else:
                    result["changed"] = True

            else:
                result["msg"] = (
                    "The data set type '{0}' is not"
                    " currently supported".format(ds_type)
                )
                result["failed"] = True
                return result

        except Exception as err:
            result["msg"] = "Failure during module execution"
            result["stderr"] = str(err)
            result["stderr_lines"] = str(err).splitlines()
            result["failed"] = True
            return result

        # ********************************************************** #
        #              Cleanup temp files and directories            #
        # ********************************************************** #

        finally:
            self._remote_cleanup(remote_path, ds_type, encoding)
        return _update_result(result, src, dest, ds_type, binary=binary)

    def _transfer_remote_content(
        self, dest, remote_path, src_type, ignore_stderr=False
    ):
        """ Transfer a file or directory from USS to local machine.
            After the transfer is complete, the USS file or directory will
            be removed.
        """
        result = dict()
        _sftp_action = 'get'

        if src_type == "PO" or src_type == "GDG":
            _sftp_action += ' -r'    # add '-r` to clone the source trees

        # To support multiple Ansible versions we must do some version detection and act accordingly
        version_inf = cli.CLI.version_info(False)
        version_major = version_inf['major']
        version_minor = version_inf['minor']

        # Override the Ansible Connection behavior for this module and track users configuration
        sftp_transfer_method = "sftp"
        user_ssh_transfer_method = None
        is_ssh_transfer_method_updated = False
        was_user_updated = False

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
                display.vvv(u"ibm_zos_fetch SSH transfer method updated from {0} to {1}.".format(user_ssh_transfer_method,
                            sftp_transfer_method), host=self._play_context.remote_addr)

            if self._connection.become:
                was_user_updated = True
                self._connection.set_option('remote_user', self._play_context._become_user)
                display.vvv(
                    u"ibm_zos_fetch SSH transfer user updated to {0}".format(self._play_context._become_user),
                    host=self._play_context.remote_addr
                )

            display.vvv(u"{0} {1} TO {2}".format(_sftp_action, remote_path, dest), host=self._play_context.remote_addr)
            display.vvv(u"{0}, {1}".format(vars(self._connection), vars(self._play_context)))
            (returncode, stdout, stderr) = self._connection._file_transport_command(remote_path, dest, _sftp_action)

            display.vvv(u"ibm_zos_fetch return code: {0}".format(returncode), host=self._play_context.remote_addr)
            display.vvv(u"ibm_zos_fetch stdout: {0}".format(stdout), host=self._play_context.remote_addr)
            display.vvv(u"ibm_zos_fetch stderr: {0}".format(stderr), host=self._play_context.remote_addr)

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

            if re.findall(r"Permission denied", err):
                result["msg"] = "Insufficient write permission for destination {0}".format(
                    dest
                )
            elif returncode != 0 or (err and not ignore_stderr):
                result["msg"] = "Error transferring remote data from z/OS system"
                result["rc"] = returncode
            if result.get("msg"):
                result["stderr"] = err
                result["failed"] = True

        finally:
            if was_user_updated:
                self._connection.set_option('remote_user', self._play_context._remote_user)
                display.vvv(
                    u"ibm_zos_fetch SSH transfer user restored to {0}".format(self._play_context._remote_user),
                    host=self._play_context.remote_addr
                )

            # Restore the users defined option `ssh_transfer_method` if it was overridden

            if is_ssh_transfer_method_updated:
                if version_major == 2 and version_minor >= 11:
                    self._connection.set_option('ssh_transfer_method', user_ssh_transfer_method)

                elif version_major == 2 and version_minor <= 10:
                    self._play_context.ssh_transfer_method = user_ssh_transfer_method

                display.vvv(u"ibm_zos_fetch SSH transfer method restored to {0}".format(user_ssh_transfer_method), host=self._play_context.remote_addr)
                is_ssh_transfer_method_updated = False

        return result

    def _remote_cleanup(self, remote_path, src_type, encoding):
        """Remove all temporary files and directories from the remote system"""
        # When fetching USS files and no encoding parameter is provided
        # do not remove the original file.
        if not (src_type == "USS" and not encoding):
            rm_cmd = "rm -r {0}".format(remote_path)
            if src_type != "PO" and src_type != "GDG":
                rm_cmd = rm_cmd.replace(" -r", "")

            # If another user created the temporary files, we'll need to run rm
            # with it too, lest we get a permissions issue.
            if self._connection.become:
                self._connection.set_option('remote_user', self._play_context._become_user)
                display.vvv(
                    u"ibm_zos_fetch SSH cleanup user updated to {0}".format(self._play_context._become_user),
                    host=self._play_context.remote_addr
                )

            self._connection.exec_command(rm_cmd)

            if self._connection.become:
                self._connection.set_option('remote_user', self._play_context._remote_user)
                display.vvv(
                    u"ibm_zos_fetch SSH cleanup user restored to {0}".format(self._play_context._remote_user),
                    host=self._play_context.remote_addr
                )
