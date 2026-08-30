#!/usr/bin/python
# -*- coding: utf-8 -*-

# Copyright (c) IBM Corporation 2025
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

DOCUMENTATION = r"""
module: zos_zfs_resize
version_added: '1.13.0'
short_description: Resize a zfs data set.
description:
  - The module L(zos_zfs_resize.,/zos_zfs_resize.html) can resize a zFS aggregate data set.
  - The I(target) data set must be a unique and a Fully Qualified Name (FQN) of a z/OS zFS aggregate data set.
  - The data set must be attached as read-write.
  - I(size) must be provided.
author:
  - "Rich Parker (@richp405)"
  - "Marcel Gutierrez (@AndreMarcel99)"
options:
  target:
    description:
      - The Fully Qualified Name of the zFS data set that will be resized.
    required: true
    type: str
    aliases: [ src ]
  size:
    description:
      - The desired size of the data set after the resizing is performed.
    required: true
    type: int
  space_type:
    description:
      - The unit of measurement to use when defining the size.
      - Valid units are C(k) (kilobytes), C(m) (megabytes), C(g) (gigabytes), C(cyl) (cylinders), and C(trk) (tracks).
    required: false
    type: str
    choices:
      - k
      - m
      - g
      - cyl
      - trk
    default: k
  no_auto_increase:
    description:
      - Option controls whether the data set size will be automatically increased when performing a shrink operation.
      - When set to C(true), during the shrinking of the zFS aggregate, if more space be needed the total size will
        not be increased and the module will fail.
    required: false
    type: bool
    default: false
  verbose:
    description:
      - Return diagnostic messages that describe the module's execution.
      - Verbose includes standard out (stdout) of the module's execution which can be excessive, to avoid writing
        this to stdout, optionally you can set the C(trace_destination) instead.
    required: false
    type: bool
    default: false
  trace_destination:
    description:
      - Specify a unique USS file name or data set name for C(trace_destination).
      - If the destination C(trace_destination) is a USS file or path, the C(trace_destination) must
        be an absolute path name.
      - Support MVS data set type PDS, PDS/E(MEMBER)
      - If the destination is an MVS data set name, the C(trace_destination) provided must meet data set naming
        conventions of one or more qualifiers, each from one to eight characters long, that are delimited by periods
      - Recommended characteristics for MVS data set are record length of 200, record format as vb and space primary
        42000 kilobytes.
    required: false
    type: str

attributes:
  action:
    support: none
    description: Indicates this has a corresponding action plugin so some parts of the options can be executed on the controller.
  async:
    support: full
    description: Supports being used with the ``async`` keyword.
  check_mode:
    support: none
    description: Can run in check_mode and return changed status prediction without modifying target. If not supported, the action will be skipped.

notes:
  - If needed, allocate the zFS trace output data set as a PDSE with RECFM=VB, LRECL=133 with a primary allocation of at least
    50 cylinders and a secondary allocation of 30 cylinders.
  - L(zfsadm documentation,https://www.ibm.com/docs/en/zos/latest?topic=commands-zfsadm).
"""

EXAMPLES = r"""
- name: Resize an aggregate data set to 2500 kilobytes.
  zos_zfs_resize:
    target: TEST.ZFS.DATA
    size: 2500

- name: Resize an aggregate data set to 20 tracks.
  zos_zfs_resize:
    target: TEST.ZFS.DATA
    space_type: trk
    size: 20

- name: Resize an aggregate data set to 4 megabytes.
  zos_zfs_resize:
    target: TEST.ZFS.DATA
    space_type: m
    size: 4

- name: Resize an aggregate data set to 1000 kilobytes and set no auto increase if it's shrinking.
  zos_zfs_resize:
    target: TEST.ZFS.DATA
    size: 1000
    no_auto_increase: true

- name: Resize an aggregate data set and get verbose output.
  zos_zfs_resize:
    target: TEST.ZFS.DATA
    size: 2500
    verbose: true

- name: Resize an aggregate data set and get the full trace on a file.
  zos_zfs_resize:
    target: TEST.ZFS.DATA
    size: 2500
    trace_destination: /tmp/trace.txt

- name: Resize an aggregate data set and capture the trace into a PDS member.
  zos_zfs_resize:
    target: TEST.ZFS.DATA
    size: 2500
    trace_destination: "TEMP.HELPER.STORAGE(RESIZE)"

- name: Resize an aggregate data set and capture the trace into a file with verbose output.
  zos_zfs_resize:
    target: TEST.ZFS.DATA
    size: 2500
    verbose: true
    trace_destination: /tmp/trace.txt
"""

RETURN = r"""
cmd:
    description: The zfsadm command executed on the remote node.
    returned: always
    type: str
    sample: zfsadm grow -aggregate SOMEUSER.VVV.ZFS -size 4096
target:
    description: The Fully Qualified Name of the resized zFS data set.
    returned: always
    type: str
    sample: SOMEUSER.VVV.ZFS
mount_target:
    description: The original share/mount of the data set.
    returned: always
    type: str
    sample: /tmp/zfs_agg
size:
    description: The desired size from option C(size) according to C(space_type).
      The resulting size can vary slightly, the actual space utilization is returned in C(new_size).
    returned: always
    type: int
    sample: 4024
rc:
    description: The return code of the zfsadm command.
    returned: always
    type: int
    sample: 0
old_size:
    description: The original data set size according to C(space_type) before resizing was performed.
    returned: always
    type: float
    sample: 3096
old_free_space:
    description: The original data sets free space according to C(space_type) before resizing was performed.
    returned: always
    type: float
    sample: 2.1
new_size:
    description: The data set size according to C(space_type) after resizing was performed.
    returned: success
    type: float
    sample: 4032
new_free_space:
    description: The data sets free space according to C(space_type) after resizing was performed.
    returned: success
    type: float
    sample: 1.5
space_type:
    description: The measurement unit of space used to report all size values.
    returned: always
    type: str
    sample: k
stdout:
    description: The modules standard out (stdout) that is returned.
    returned: always
    type: str
    sample: IOEZ00173I Aggregate TEST.ZFS.DATA.USER successfully grown.
stderr:
    description: The modules standard error (stderr) that is returned. it may have no return value.
    returned: always
    type: str
    sample: IOEZ00181E Could not open trace output dataset.
stdout_lines:
    description: List of strings containing individual lines from standard out (stdout).
    returned: always
    type: list
    sample: ["IOEZ00173I Aggregate TEST.ZFS.DATA.USER successfully grown."]
stderr_lines:
    description: List of strings containing individual lines from standard error (stderr).
    returned: always
    type: list
    sample: ["IOEZ00181E Could not open trace output dataset."]
verbose_output:
    description: If C(verbose=true), the operation's full traceback will show for this property.
    returned: always
    type: str
    sample: 6FB2F8 print_trace_table printing contents of table Main Trace Table...
"""

import os
import tempfile
import traceback
from pathlib import Path
from ansible.module_utils.basic import AnsibleModule
from ansible_collections.ibm.ibm_zos_core.plugins.module_utils.import_handler import (
    ZOAUImportError
)
from ansible_collections.ibm.ibm_zos_core.plugins.module_utils import (
    better_arg_parser,
    data_set,
)

from ansible_collections.ibm.ibm_zos_core.plugins.module_utils.zfsadm import zfsadm
from ansible_collections.ibm.ibm_zos_core.plugins.module_utils.dependency_checker import (
    validate_dependencies,
)
from ansible_collections.ibm.ibm_zos_core.plugins.module_utils.log import SingletonLogger

try:
    from zoautil_py import datasets
except Exception:
    datasets = ZOAUImportError(traceback.format_exc())


def calculate_size_on_k(size, space_type):
    """Function to convert size depending on the space_type.

    Parameters
    ----------
        size : int
            Size to grow or shrink the zfs
        space_type : str
            Type of space to be use

    Returns
    -------
        size : int
            Size on kilobytes
    """
    if space_type == "m":
        size *= 1024
    if space_type == "g":
        size *= 1048576
    if space_type == "cyl":
        size *= 830
    if space_type == "trk":
        size *= 55
    return size


def get_full_output(file, module):
    """Function to get the verbose output and delete the tmp file

    Parameters
    ----------
        file : str
            Name of the tmp file where the full verbose output is store
        module : object
            Ansible object to execute commands.

    Returns
    -------
        output : str
            Verbose output
    """
    output = ""

    if "/" in file:
        cmd = f"cat {file}"
    else:
        cmd = f"dcat '{file}'"

    rc, output, stderr = module.run_command(cmd)

    if rc != 0:
        output = "Unable to obtain full output for verbose mode."

    return output


def get_size_and_free(line):
    """Function to parsing the response of get aggregation size.

    Parameters
    ----------
        line : str
            stout of the get aggregation size function

    Returns
    -------
        numbers[1] : int
            Size on kilobytes of the zfs

        numbers[0] : int
            Total free on kilobytes of the zfs
    """
    numbers = [int(word) for word in line.split() if word.isdigit()]
    return numbers[1], numbers[0]


def find_mount_target(module, target, results):
    """Execute df command to access the information of mount points.

    Parameters
    ----------
        module : object
            Ansible object to execute commands.
        target : str
            ZFS to check.

    Returns
    -------
        mount_point : str
            The folder where the zfs is mount
    """
    rc, stdout, stderr = module.run_command("df")
    found = False
    if rc == 0:
        stdout_lines = stdout.split("\n")
        for line in stdout_lines:
            if len(line) > 2:
                columns = line.split()
                if target in columns[1]:
                    mount_point = columns[0]
                    found = True
                    break
        if found is False:
            module.fail_json(msg=f"No mount points were found in the following output: {stdout}", **results)
    return mount_point


def convert_size(size, space_type):
    """Function to convert size from kb to the space_type.

    Parameters
    ----------
        size : int
            Size to grow or shrink the zfs
        space_type : str
            Type of space to be use

    Returns
    -------
        size : int
            Size on kilobytes
    """
    if space_type == "m":
        size /= 1024
    if space_type == "g":
        size /= 1048576
    if space_type == "cyl":
        size /= 830
    if space_type == "trk":
        size /= 55
    return size


def proper_size_str(size, space_type):
    """Function to convert size to a proper response for output.

    Parameters
    ----------
        size : int
            Size to grow or shrink the zfs
        space_type : str
            Type of space to be use

    Returns
    -------
        size : int or float
            Size on specific space_type notation 2 if is 2.0 or 1.5
    """
    if space_type == "k" or space_type == "trk" or space_type == "cyl":
        return int(size)
    else:
        split_size = str(size).split(".")
        if split_size[1].startswith("0"):
            return int(size)
        else:
            return float("{:.1f}".format(size))


def create_trace_dataset(name, member=False):
    """Function to create data sets for traceback if is not created.

    Parameters
    ----------
        name : str
            Full size of the dataset
        member : bool
            If the dataset include a member to create

    Returns
    -------
        rc : bool
            Indicates if datasets were made.
    """
    if member:
        dataset_name = data_set.extract_dsname(name)
        data_set.DataSet.ensure_present(name=dataset_name, replace=False, type="PDSE", record_length=200, record_format="VB",
                                        space_type="K", space_primary="42000", space_secondary="25000")
        rc = data_set.DataSet.ensure_member_present(name)
    else:
        rc, zoau_data_set = data_set.DataSet.ensure_present(name=name, replace=False, type="PDS", record_length=200, record_format="VB",
                                                            space_type="K", space_primary="42000", space_secondary="25000")

    return rc


def validate_dataset_info(dataset):
    """Function to validates the proper characteristics of the dataset to use on trace output.

    Args:
        dataset : str
            dataset name

    Returns:
        bool :
            if the dataset is valid or not
        str :
            specification of the problem
    """
    dataset = data_set.extract_dsname(dataset)

    trace_ds = data_set.DataSetUtils(data_set=dataset)
    trace_information = trace_ds._gather_data_set_info()

    if trace_information["dsorg"] != "PO":
        return False, "data set type required for trace_destination is PDS or PDSE."

    if trace_information["lrecl"] < 80:
        return False, "record length is not enough. Recommended length is 200."

    if trace_information["recfm"] != "VB":
        return False, f"record format is {trace_information['recfm'].lower()} required vb."

    ds_attributes = datasets.list_datasets(dataset)[0]
    space_primary = int(ds_attributes.total_space)

    if space_primary < 42498000:
        return False, "not enought primary space is below 50 cyl. Recommended space 42000 k."

    return True, ""


def run_module():
    module = AnsibleModule(
        argument_spec=dict(
            target=dict(type="str", required=True, aliases=['src']),
            size=dict(type="int", required=True),
            space_type=dict(
                type="str",
                required=False,
                choices=["k", "m", "g", "cyl", "trk"],
                default="k",
            ),
            no_auto_increase=dict(type="bool", required=False, default=False),
            verbose=dict(type="bool", required=False, default=False),
            trace_destination=dict(type="str", required=False),
        ),
        supports_check_mode=False
    )
    validate_dependencies(module)
    args_def = dict(
        target=dict(type="data_set", required=True, aliases=['src']),
        size=dict(type="int", required=True),
        space_type=dict(
            type="str",
            required=False,
            choices=["k", "m", "g", "cyl", "trk"],
            default="k",
        ),
        no_auto_increase=dict(type="bool", required=False, default=False),
        verbose=dict(type="bool", required=False, default=False),
        trace_destination=dict(type="data_set_or_path", required=False),
    )

    try:
        parser = better_arg_parser.BetterArgParser(args_def)
        parsed_args = parser.parse_args(module.params)
        module.params = parsed_args
    except ValueError as err:
        module.fail_json(
            msg='Parameter verification failed.',
            stderr=str(err)
        )

    # Initialize logging module
    module_verbosity_level = module._verbosity
    SingletonLogger().get_logger(module_verbosity_level)

    result = dict()
    target = module.params.get("target")
    size = module.params.get("size")
    space_type = module.params.get("space_type")
    noai = module.params.get("no_auto_increase")
    verbose = module.params.get("verbose")
    trace_destination = module.params.get("trace_destination")

    changed = False
    # Variables to return the value on the space_type by the user
    size_on_type = ""
    free_on_type = ""

    result.update(
        dict(
            target=target,
            size=size,
            space_type=space_type,
            cmd="",
            changed=changed,
            rc=1,
            stdout="",
            stderr="",
            verbose_output="",
        )
    )

    if module.check_mode:
        module.exit_json(**result)

    # Validate if the target zFS exist
    if not (data_set.DataSet.data_set_exists(target)):
        module.fail_json(msg=f"zFS Target {target} does not exist", **result)

    # Validation to found target on the system and also get the mount_point
    mount_target = find_mount_target(module=module, target=target, results=result)

    if size <= 0:
        module.fail_json(msg=f"Can not resize zFS aggregate Target {target} to 0", **result)

    # Initialize the class with the target
    zfsadm_obj = zfsadm(aggregate_name=target, module=module)

    rc, stdout, stderr = zfsadm.get_aggregate_size(zfsadm_obj.aggregate_name, module)

    if rc == 0:
        old_size, old_free = get_size_and_free(line=stdout)

    if space_type != "k":
        space = calculate_size_on_k(size=size, space_type=space_type)
    else:
        space = size

    # Validations to know witch function will be execute
    operation = ""
    minimum_size_t_shrink = old_size - old_free

    if space_type != "k":
        size_on_type = convert_size(size=old_size, space_type=space_type)
        free_on_type = convert_size(size=old_free, space_type=space_type)

    str_old_size = old_size if space_type == "k" else size_on_type
    str_old_size = proper_size_str(str_old_size, space_type)
    str_old_free = old_free if space_type == "k" else free_on_type
    str_old_free = proper_size_str(str_old_free, space_type)

    result.update(
        dict(
            mount_target=mount_target,
            old_size=str_old_size,
            old_free_space=str_old_free,
        )
    )

    if space == old_size:
        result.update(
            dict(
                cmd="",
                rc=0,
                stdout=f"Size provided is the current size of the zFS {target}",
                stderr="",
                changed=False,
                size=size,
                new_size=str_old_size,
                new_free_space=str_old_free,
            )
        )
        module.exit_json(**result)

    elif space < minimum_size_t_shrink:
        module.fail_json(msg="There is not enough available space in the zFS aggregate to perform a shrink operation.", **result)

    elif space > old_size:
        operation = "grow"

    elif space >= minimum_size_t_shrink and space < old_size:
        operation = "shrink"

    noai = " -noai " if noai else ""
    noai = "" if operation == "grow" else noai

    # Variables for the verbose output or trace destination
    trace = ""
    tmp_file = ""
    trace_uss = True
    trace_destination_created = True
    is_valid_trace_destination = True
    msg_trace = ""

    if trace_destination is not None:
        if data_set.is_data_set(data_set.extract_dsname(trace_destination)):
            if data_set.is_member(trace_destination):
                if not data_set.DataSet.data_set_exists(data_set.extract_dsname(trace_destination)):
                    trace_destination_created = create_trace_dataset(name=trace_destination, member=True)
                else:
                    is_valid_trace_destination, msg_trace = validate_dataset_info(dataset=trace_destination)
            else:
                if not (data_set.DataSet.data_set_exists(trace_destination)):
                    trace_destination_created = create_trace_dataset(name=trace_destination, member=False)
                else:
                    is_valid_trace_destination, msg_trace = validate_dataset_info(dataset=trace_destination)
            trace_uss = False
        else:
            trace_destination = better_arg_parser.BetterArgHandler.fix_local_path(trace_destination)
            trace_uss = True
        tmp_file = trace_destination

    if not is_valid_trace_destination:
        module.fail_json(
            msg=f"Trace destination {trace_destination} does not meet minimal criteria to be used. The problem is {msg_trace}",
            **result
        )

    if not trace_destination_created:
        stderr_trace = f"\nUnable to create trace_destination {trace_destination}."
    else:
        stderr_trace = ""

    if verbose and trace_destination is None:
        home_folder = Path.home()
        tmp_fld = module._remote_tmp.replace("~", str(home_folder))
        tmp_fld = tmp_fld.replace("//", "/")
        temp = tempfile.NamedTemporaryFile(dir=tmp_fld, delete=False)
        tmp_file = temp.name
        trace_uss = True

    if verbose or trace_destination is not None:
        trace = f" -trace '{tmp_file}'" if trace_uss else f" -trace \"//'{trace_destination}'\" "

    # Execute the function
    rc, stdout, stderr, cmd = zfsadm_obj.execute_resizing(operation=operation, size=space, noai=noai, verbose=trace)

    # Get the output, calculate size and verbose if required
    if rc == 0:
        changed = True

        rc_size, stdout_size, stderr_size = zfsadm.get_aggregate_size(zfsadm_obj.aggregate_name, module)
        if rc_size == 0:
            new_size, new_free = get_size_and_free(line=stdout_size)

        if space_type != "k":
            size_on_type = convert_size(size=new_size, space_type=space_type)
            free_on_type = convert_size(size=new_free, space_type=space_type)

        if verbose:
            if trace_destination is None:
                output = get_full_output(file=tmp_file, module=module)
                result.update(
                    verbose_output=output,
                )
                os.remove(tmp_file)
            else:
                output = get_full_output(file=tmp_file, module=module)
                result.update(
                    verbose_output=output,
                )

    else:
        if verbose and trace_destination is None:
            os.remove(tmp_file)

        msg = "No enough space on device to grow." if operation == 'grow' else "No space to properly shrink."

        raise ResizingOperationError(
            msg=f"Resize: resize command returned non-zero code. {msg}",
            target=target,
            mount_target=mount_target,
            cmd=cmd,
            rc=rc,
            size=size,
            stdout=stdout,
            stderr=stderr + stderr_trace,
            changed=False,
            old_size=str_old_size,
            old_free=str_old_free,
            verbose_output="",
        )

    str_new_size = new_size if space_type == "k" else size_on_type
    str_new_size = proper_size_str(str_new_size, space_type)
    str_new_free = new_free if space_type == "k" else free_on_type
    str_new_free = proper_size_str(str_new_free, space_type)

    result.update(
        dict(
            cmd=cmd,
            rc=rc,
            stdout=stdout,
            stderr=stderr + stderr_trace,
            changed=changed,
            new_size=str_new_size,
            new_free_space=str_new_free,
        )
    )

    module.exit_json(**result)


class ResizingOperationError(Exception):
    def __init__(
            self,
            msg,
            target="",
            mount_target="",
            size="",
            cmd="",
            rc="",
            stdout="",
            stderr="",
            changed=False,
            old_size="",
            old_free="",
            verbose_output="",
    ):
        """Error in a copy operation.

        Parameters
        ----------
        msg : str
            Human readable string describing the exception.
        target : str
            The Fully Qualified Name of the resized zfs data set
        mount_target : str
            The original share/mount
        size : str
            The approximate size of the target
        rc : int
            Result code.
        stdout : str
            Standart output.
        stderr : str
            Standart error.
        cmd : str
            The zfsadm command try to execute on the remote node.
        changed : bool
            If the operation was executed.
        old_size : float or int
            The reported size, of the data set before the resizing is performed.
        old_free : float or int
            The reported size, of the free space in the data set before the resizing is performed.
        """
        self.json_args = dict(
            msg=msg,
            target=target,
            mount_target=mount_target,
            cmd=cmd,
            rc=rc,
            size=size,
            stdout=stdout,
            stderr=stderr,
            changed=changed,
            old_size=old_size,
            old_free_space=old_free,
            verbose_output=verbose_output,
        )
        super().__init__(msg)


def main():
    run_module()


if __name__ == '__main__':
    main()
