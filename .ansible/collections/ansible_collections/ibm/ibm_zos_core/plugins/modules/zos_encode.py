#!/usr/bin/python
# -*- coding: utf-8 -*-

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


DOCUMENTATION = r"""
module: zos_encode
version_added: '1.1.0'
author:
  - "Zhao Lu (@yourfuwa2015)"
  - "Blake Becker (@blakeinate)"
short_description: Perform encoding operations.
description:
  - Converts the encoding of characters that are read from a UNIX System
    Services (USS) file or path, PS (sequential data set), PDS, PDSE, or
    KSDS (VSAM data set).
  - Writes the data to a UNIX System Services (USS) file or path,
    PS (sequential data set), PDS, PDSE, or KSDS (VSAM data set).
options:
  encoding:
    description:
      - Specifies which encodings the destination file or data set should be
        converted from and to.
      - Supported character sets rely on the charset conversion utility (iconv)
        version; the most common character sets are supported.
    type: dict
    required: false
    suboptions:
      from:
        description:
          - The character set of the source I(src).
        required: false
        type: str
        default: IBM-1047
      to:
        description:
          - The destination I(dest) character set for the output to be written as.
        required: false
        type: str
        default: ISO8859-1
  src:
    description:
      - The location can be a UNIX System Services (USS) file or path,
        PS (sequential data set), PDS, PDSE, member of a PDS or PDSE, a
        generation data set (GDS) or KSDS (VSAM data set).
      - The USS path or file must be an absolute pathname.
      - If I(src) is a USS directory, all files will be encoded.
      - Encoding a whole generation data group (GDG) is not supported.
    required: true
    type: str
  dest:
    description:
      - The location where the converted characters are output.
      - The destination I(dest) can be a UNIX System Services (USS) file or path,
        PS (sequential data set), PDS, PDSE, member of a PDS or PDSE, a
        generation data set (GDS) or KSDS (VSAM data set).
      - If the length of the PDSE member name used in I(dest) is greater
        than 8 characters, the member name will be truncated when written out.
      - If I(dest) is not specified, the I(src) will be used as the destination
        and will overwrite the I(src) with the character set in the
        option I(to_encoding).
      - The USS file or path must be an absolute pathname.
      - If I(dest) is a data set, it must be already allocated.
    required: false
    type: str
  backup:
    description:
      - Creates a backup file or backup data set for I(dest), including the
        timestamp information to ensure that you retrieve the original file.
      - I(backup_name) can be used to specify a backup file name
        if I(backup=true).
    required: false
    type: bool
    default: false
  backup_name:
    description:
      - Specify the USS file name or data set name for the dest backup.
      - If dest is a USS file or path, I(backup_name) must be a file or
        path name, and the USS path or file must be an absolute pathname.
      - If dest is an MVS data set, the I(backup_name) must be an MVS data
        set name.
      - If I(backup_name) is not provided, the default backup name will be used.
        The default backup name for a USS file or path will be the destination
        file or path name appended with a timestamp,
        e.g. /path/file_name.2020-04-23-08-32-29-bak.tar. If dest is an
        MVS data set, the default backup name will be a random name generated
        by IBM Z Open Automation Utilities.
      - C(backup_name) will be returned on either success or failure of module
        execution such that data can be retrieved.
      - If I(backup_name) is a generation data set (GDS), it must be a relative
        positive name (for example, V(HLQ.USER.GDG(+1\))).
    required: false
    type: str
  backup_compress:
    description:
      - Determines if backups to USS files or paths should be compressed.
      - I(backup_compress) is only used when I(backup=true).
    type: bool
    required: false
    default: false
  tmp_hlq:
    description:
      - Override the default high level qualifier (HLQ) for temporary and backup
        datasets.
      - The default HLQ is the Ansible user used to execute the module and if
        that is not available, then the value C(TMPHLQ) is used.
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
  - It is the playbook author or user's responsibility to avoid files that should
    not be encoded, such as binary files. A user is described as the remote user,
    configured either for the playbook or playbook tasks, who can
    also obtain escalated privileges to execute as root or another user.
  - All data sets are always assumed to be cataloged. If an uncataloged data
    set needs to be encoded, it should be cataloged first.
  - For supported character sets used to encode data, refer to the
    L(documentation,https://ibm.github.io/z_ansible_collections_doc/ibm_zos_core/docs/source/resources/character_set.html).
"""

EXAMPLES = r"""
- name: Convert file encoding from IBM-1047 to ISO8859-1 for the same file
  zos_encode:
    src: /zos_encode/test.data

- name: Convert file encoding from IBM-1047 to ISO8859-1 to another file with
    backup
  zos_encode:
    src: /zos_encode/test.data
    dest: /zos_encode_out/test.out
    encoding:
      from: IBM-1047
      to: ISO8859-1
    backup: true
    backup_compress: true

- name: Convert file encoding from IBM-1047 to ISO8859-1 to a directory
  zos_encode:
    src: /zos_encode/test.data
    dest: /zos_encode_out/

- name: Convert file encoding from all files in a directory to another
    directory
  zos_encode:
    src: /zos_encode/
    dest: /zos_encode_out/
    encoding:
      from: ISO8859-1
      to: IBM-1047

- name: Convert file encoding from a USS file to a sequential data set
  zos_encode:
    src: /zos_encode/test.data
    dest: USER.TEST.PS
    encoding:
      from: IBM-1047
      to: ISO8859-1

- name: Convert file encoding from files in a directory to a partitioned
    data set
  zos_encode:
    src: /zos_encode/
    dest: USER.TEST.PDS
    encoding:
      from: ISO8859-1
      to: IBM-1047

- name: Convert file encoding from a USS file to a partitioned data set
    member
  zos_encode:
    src: /zos_encode/test.data
    dest: USER.TEST.PDS(TESTDATA)
    encoding:
      from: ISO8859-1
      to: IBM-1047

- name: Convert file encoding from a sequential data set to a USS file
  zos_encode:
    src: USER.TEST.PS
    dest: /zos_encode/test.data
    encoding:
      from: IBM-1047
      to: ISO8859-1

- name: Convert file encoding from a PDS encoding to a USS directory
  zos_encode:
    src: USER.TEST.PDS
    dest: /zos_encode/
    encoding:
      from: IBM-1047
      to: ISO8859-1

- name: Convert file encoding from a sequential data set to another
    sequential data set
  zos_encode:
    src: USER.TEST.PS
    dest: USER.TEST1.PS
    encoding:
      from: IBM-1047
      to: ISO8859-1

- name: Convert file encoding from a sequential data set to a
    partitioned data set (extended) member
  zos_encode:
    src: USER.TEST.PS
    dest: USER.TEST1.PDS(TESTDATA)
    encoding:
      from: IBM-1047
      to: ISO8859-1

- name: Convert file encoding from a USS file to a VSAM data set
  zos_encode:
    src: /zos_encode/test.data
    dest: USER.TEST.VS
    encoding:
      from: ISO8859-1
      to: IBM-1047

- name: Convert file encoding from a VSAM data set to a USS file
  zos_encode:
    src: USER.TEST.VS
    dest: /zos_encode/test.data
    encoding:
      from: IBM-1047
      to: ISO8859-1

- name: Convert file encoding from a VSAM data set to a sequential
    data set
  zos_encode:
    src: USER.TEST.VS
    dest: USER.TEST.PS
    encoding:
      from: IBM-1047
      to: ISO8859-1

- name: Convert file encoding from a sequential data set a VSAM data set
  zos_encode:
    src: USER.TEST.PS
    dest: USER.TEST.VS
    encoding:
      from: ISO8859-1
      to: IBM-1047

- name: Convert file encoding from a USS file to a generation data set
  zos_encode:
    src: /zos_encode/test.data
    dest: USER.TEST.GDG(0)
    encoding:
      from: ISO8859-1
      to: IBM-1047

- name: Convert file encoding from a USS file to a data set while using a GDG for backup
  zos_encode:
    src: /zos_encode/test.data
    dest: USER.TEST.PS
    encoding:
      from: ISO8859-1
      to: IBM-1047
    backup: true
    backup_name: USER.BACKUP.GDG(+1)
"""

RETURN = r"""
src:
    description:
       The location of the input characters identified in option I(src).
    returned: always
    type: str
dest:
    description:
       The name of the output file or data set. If dest is a USS file or
       path and the status has been changed in the conversion, the file
       status will also be returned.
    returned: always
    type: str
backup_name:
    description:
       Name of the backup file created.
    returned: changed and if backup=yes
    type: str
    sample: /path/file_name.2020-04-23-08-32-29-bak.tar
encoding:
  description:
    - Specifies which encodings the destination file or data set was
      converted from and to.
  type: dict
  returned: always
  contains:
    from:
      description:
        - The character set of the source I(src).
      type: str
      sample: IBM-1047
      returned: always
    to:
      description:
        - The destination I(dest) character set for the output that was written as.
      type: str
      sample: ISO8859-1
      returned: always
"""
from ansible_collections.ibm.ibm_zos_core.plugins.module_utils.import_handler import (
    ZOAUImportError,
)
from ansible_collections.ibm.ibm_zos_core.plugins.module_utils import (
    better_arg_parser,
    data_set,
    encode,
    backup as zos_backup,
)
from ansible_collections.ibm.ibm_zos_core.plugins.module_utils.dependency_checker import (
    validate_dependencies,
)
from ansible.module_utils.basic import AnsibleModule
from os import path
from os import makedirs
from os import listdir
import re
import traceback
from ansible_collections.ibm.ibm_zos_core.plugins.module_utils.log import SingletonLogger

try:
    from zoautil_py import datasets
except Exception:
    datasets = ZOAUImportError(traceback.format_exc())


def check_pds_member(ds, mem):
    """Check if a member exists in a PDS.

    Parameters
    ----------
    ds : str
        PDS data set name.
    mem : str
        Member name to check if is under PDS.

    Returns
    -------
    bool
        If it is a member of the data set.

    Raises
    ------
    EncodeError
        Can not find member in provided dataset.
    """
    check_rc = False
    if mem in datasets.list_members(ds):
        check_rc = True
    else:
        raise EncodeError("Cannot find member {0} in {1}".format(mem, ds))
    return check_rc


def check_mvs_dataset(ds, tmphlq=None):
    """To call data_set utils to check if the MVS data set exists or not.

    Parameters
    ----------
    ds : str
        Data set name.
    tmphlq : str
        High Level Qualifier for temporary datasets.

    Returns
    -------
    tuple(bool,str)
        If the data set exists and it's type.

    Raises
    ------
    EncodeError
        If data set is not cataloged.
    EncodeError
        Unable to determine data set type.
    """
    check_rc = False
    ds_type = None
    if not data_set.DataSet.data_set_exists(ds, tmphlq=tmphlq):
        raise EncodeError(
            "Data set {0} is not cataloged, please check data set provided in"
            "the src option.".format(ds)
        )
    else:
        check_rc = True
        ds_type = data_set.DataSetUtils(ds, tmphlq=tmphlq).ds_type()
        if not ds_type:
            raise EncodeError("Unable to determine data set type of {0}".format(ds))
    return check_rc, ds_type


def check_file(file, tmphlq=None):
    """Check file is a USS file or an MVS data set.

    Parameters
    ----------
    file : str
        File to check.
    tmphlq : str
        High Level Qualifier for temporary datasets.

    Returns
    -------
    tuple(bool,bool,str)
        If is USS file, MVS dataset, and the dataset type.

    Raises
    ------
    EncodeError
        The data set is not partitioned.
    """
    is_uss = False
    is_mvs = False
    ds_type = None
    if path.sep in file:
        is_uss = True
    else:
        ds = file.upper()
        if "(" in ds:
            dsn = ds[: ds.rfind("(", 1)]
            mem = "".join(re.findall(r"[(](.*?)[)]", ds))
            rc, ds_type = check_mvs_dataset(dsn, tmphlq=tmphlq)
            if rc:
                if ds_type == "PO":
                    is_mvs = check_pds_member(dsn, mem)
                    ds_type = "PS"
                else:
                    raise EncodeError(
                        "Data set {0} is not a partitioned data set".format(dsn)
                    )
        else:
            is_mvs, ds_type = check_mvs_dataset(ds, tmphlq=tmphlq)
    return is_uss, is_mvs, ds_type


def verify_uss_path_exists(file):
    """Verify if USS path exists.

    Parameters
    ----------
    file : str
        Path of the file.

    Raises
    ------
    EncodeError
        File does not exist in the directory.
    """
    if not path.exists(file):
        mypath = "/" + file.split("/")[0] + "/*"
        ld = listdir(mypath)
        raise EncodeError(
            "File {0} does not exist in directory {1}; files found {2}.".format(
                file, mypath, str(ld)
            )
        )
    return


def run_module():
    """Runs the module.

    Raises
    ------
    fail_json
        Exception during execution.
    """
    module_args = dict(
        src=dict(type="str", required=True),
        dest=dict(type="str"),
        encoding=dict(
            type="dict",
            required=False,
            options={
                "from": dict(type="str", required=False, default="IBM-1047"),
                "to": dict(type="str", required=False, default="ISO8859-1"),
            }
        ),
        backup=dict(type="bool", default=False),
        backup_name=dict(type="str", required=False, default=None),
        backup_compress=dict(type="bool", required=False, default=False),
        tmp_hlq=dict(type='str', required=False, default=None),
    )

    module = AnsibleModule(argument_spec=module_args)
    validate_dependencies(module)

    if module.params.get("encoding"):
        module.params.update(
            dict(
                from_encoding=module.params.get("encoding").get("from"),
                to_encoding=module.params.get("encoding").get("to"),
            )
        )
    else:
        module.params.update(
            dict(
                from_encoding="IBM-1047",
                to_encoding="ISO8859-1",
            )
        )

    arg_defs = dict(
        src=dict(arg_type="data_set_or_path", required=True),
        dest=dict(arg_type="data_set_or_path", required=False),
        from_encoding=dict(arg_type="str", default="IBM-1047"),
        to_encoding=dict(arg_type="str", default="ISO8859-1", required=False),
        backup=dict(arg_type="bool", default=False, required=False),
        backup_name=dict(arg_type="data_set_or_path", required=False, default=None),
        backup_compress=dict(arg_type="bool", required=False, default=False),
        tmp_hlq=dict(type='qualifier_or_empty', required=False, default=None),
    )

    parser = better_arg_parser.BetterArgParser(arg_defs)
    parsed_args = parser.parse_args(module.params)

    # Initialize logging module
    module_verbosity_level = module._verbosity
    SingletonLogger().get_logger(module_verbosity_level)

    src = parsed_args.get("src")
    dest = parsed_args.get("dest")
    backup = parsed_args.get("backup")
    backup_name = parsed_args.get("backup_name")
    backup_compress = parsed_args.get("backup_compress")
    from_encoding = parsed_args.get("from_encoding").upper()
    to_encoding = parsed_args.get("to_encoding").upper()
    tmphlq = module.params.get('tmp_hlq')

    # is_uss_src(dest) to determine whether the src(dest) is a USS file/path or not
    # is_mvs_src(dest) to determine whether the src(dest) is a MVS data set or not
    is_uss_src = False
    is_mvs_src = False
    is_uss_dest = False
    is_mvs_dest = False
    ds_type_src = None
    ds_type_dest = None
    src_data_set = None
    dest_data_set = None
    convert_rc = False
    changed = False
    encoding_dict = {"from": from_encoding, "to": to_encoding}
    result = dict(changed=changed, src=src, dest=dest, encoding=encoding_dict, backup_name=None)

    try:
        # Check the src is a USS file/path or an MVS data set
        # is_uss_src, is_mvs_src, ds_type_src = check_file(src)

        if path.sep in src:
            is_uss_src = True
            # ds_type_src = "USS"
            verify_uss_path_exists(src)  # This can raise an exception.
        else:
            is_mvs_src = True
            src_data_set = data_set.MVSDataSet(src)
            is_name_member = data_set.is_member(src_data_set.name)
            dest_exists = False

            if not is_name_member:
                dest_exists = data_set.DataSet.data_set_exists(src_data_set.name, tmphlq=tmphlq)
            else:
                dest_exists = data_set.DataSet.data_set_exists(
                    data_set.extract_dsname(src_data_set.name),
                    tmphlq=tmphlq
                )

            if not dest_exists:
                raise EncodeError(
                    "Data set {0} is not cataloged, please check data set provided in "
                    "the src option.".format(data_set.extract_dsname(src_data_set.raw_name))
                )

            if is_name_member:
                if not data_set.DataSet.data_set_member_exists(src_data_set.name):
                    raise EncodeError("Cannot find member {0} in {1}".format(
                        data_set.extract_member(src_data_set.raw_name),
                        data_set.extract_dsname(src_data_set.raw_name)
                    ))
                ds_type_src = "PS"
            else:
                ds_type_src = data_set.DataSet.data_set_type(src_data_set.name, tmphlq=tmphlq)

            if not ds_type_src:
                raise EncodeError("Unable to determine data set type of {0}".format(src_data_set.raw_name))

        result["src"] = src

        # Check the dest is a USS file/path or an MVS data set
        # if the dest is not specified, the value in the src will be used
        if not dest:
            if src_data_set:
                dest = src_data_set.name
            else:
                dest = src

            is_uss_dest = is_uss_src
            is_mvs_dest = is_mvs_src
            ds_type_dest = ds_type_src
        else:
            if path.sep in dest:
                is_uss_dest = True
            else:
                is_mvs_dest = True
                dest_data_set = data_set.MVSDataSet(dest)
                is_name_member = data_set.is_member(dest_data_set.name)

                if not is_name_member:
                    dest_exists = data_set.DataSet.data_set_exists(dest_data_set.name, tmphlq=tmphlq)
                else:
                    dest_exists = data_set.DataSet.data_set_exists(
                        data_set.extract_dsname(dest_data_set.name),
                        tmphlq=tmphlq
                    )

                if not dest_exists:
                    raise EncodeError(
                        "Data set {0} is not cataloged, please check data set provided in "
                        "the dest option.".format(data_set.extract_dsname(dest_data_set.raw_name))
                    )

                if is_name_member:
                    ds_type_dest = "PS"
                else:
                    ds_type_dest = data_set.DataSet.data_set_type(dest_data_set.name, tmphlq=tmphlq)

            if (not is_uss_dest) and (path.sep in dest):
                try:
                    if path.isfile(src) or ds_type_src in ["PS", "VSAM"]:
                        head, tail = path.split(dest)
                        if not path.exists(head):
                            makedirs(head)
                        with open(dest, "w"):
                            pass
                    else:
                        makedirs(dest)
                    is_uss_dest = True
                except OSError:
                    raise EncodeError("Failed when creating the {0}".format(dest))
        result["dest"] = dest

        if ds_type_dest == "GDG":
            raise EncodeError("Encoding of a whole generation data group is not supported.")

        new_src = src_data_set.name if src_data_set else src
        new_dest = dest_data_set.name if dest_data_set else dest

        # Check if the dest is required to be backup before conversion
        if backup:
            if backup_name:
                backup_data_set = data_set.MVSDataSet(backup_name)
                if backup_data_set.is_gds_active:
                    raise EncodeError(
                        f"The generation data set {backup_name} cannot be used as backup. "
                        "Please use a new generation for this purpose."
                    )

            if is_uss_dest:
                backup_name = zos_backup.uss_file_backup(
                    new_dest, backup_name, backup_compress
                )
            if is_mvs_dest:
                backup_name = zos_backup.mvs_file_backup(new_dest, backup_name, tmphlq)
            result["backup_name"] = backup_name

        eu = encode.EncodeUtils()
        # Check input code set is valid or not
        # If the value specified in from_encoding or to_encoding is not in the code_set, exit with an error message
        # If the values specified in from_encoding and to_encoding are the same, exit with an message
        code_set = eu.get_codeset()
        # set the tmphlq in the encodeutils
        eu.tmphlq = tmphlq
        if from_encoding not in code_set:
            raise EncodeError(
                "Invalid codeset: Please check the value of the from_encoding!"
            )
        if to_encoding not in code_set:
            raise EncodeError(
                "Invalid codeset: Please check the value of the to_encoding!"
            )
        if from_encoding == to_encoding:
            raise EncodeError(
                "The value of the from_encoding and to_encoding are the same, no need to do the conversion!"
            )

        if is_uss_src and is_uss_dest:
            convert_rc = eu.uss_convert_encoding_prev(
                new_src, new_dest, from_encoding, to_encoding
            )
        else:
            convert_rc = eu.mvs_convert_encoding(
                new_src,
                new_dest,
                from_encoding,
                to_encoding,
                src_type=ds_type_src,
                dest_type=ds_type_dest,
                tmphlq=tmphlq
            )

        if convert_rc:
            if is_uss_dest:
                eu.uss_tag_encoding(new_dest, to_encoding)

            changed = True
        result.update(dict(src=new_src, dest=new_dest, changed=changed, backup_name=backup_name))
    except encode.TaggingError as e:
        module.fail_json(
            msg=e.msg,
            rc=e.rc,
            stdout=e.stdout,
            stderr=e.stderr,
            stdout_lines=e.stdout.splitlines(),
            stderr_lines=e.stderr.splitlines(),
        )
    except Exception as e:
        module.fail_json(msg=repr(e), **result)

    module.exit_json(**result)


class EncodeError(Exception):
    def __init__(self, message):
        """Error during encoding.

        Parameters
        ----------
        message : str
            Human readable string describing the exception.

        Attributes
        ----------
        msg : str
            Human readable string describing the exception.
        """
        self.msg = 'An error occurred during encoding: "{0}"'.format(message)
        super(EncodeError, self).__init__(self.msg)


def main():
    run_module()


if __name__ == "__main__":
    main()
