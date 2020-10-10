#!/usr/bin/python
# -*- coding: utf-8 -*-

# Copyright (c) IBM Corporation 2019, 2020
# Apache License, Version 2.0 (see https://opensource.org/licenses/Apache-2.0)

from __future__ import absolute_import, division, print_function

__metaclass__ = type

ANSIBLE_METADATA = {
    "metadata_version": "1.1",
    "status": ["stableinterface"],
    "supported_by": "community",
}

DOCUMENTATION = r"""
module: zos_encode
author:
  - "Zhao Lu (@yourfuwa2015)"
  - "Blake Becker (@blakeinate)"
short_description: Perform encoding operations.
description:
  - Converts the encoding of characters that are read from a UNIX System
    Services (USS) file or path, PS(sequential data set), PDS, PDSE, or
    KSDS(VSAM data set).
  - Writes the data to a UNIX System Services (USS) file or path,
    PS(sequential data set), PDS, PDSE, or KSDS(VSAM data set).
options:
  from_encoding:
    description:
      - The character set of the source I(src).
      - Supported character sets rely on the charset conversion utility (iconv)
        version; the most common character sets are supported.
    required: false
    type: str
    default: IBM-1047
  to_encoding:
    description:
      - The destination I(dest) character set for the output to be written as.
      - Supported character sets rely on the charset conversion utility (iconv)
        version; the most common character sets are supported.
    required: false
    type: str
    default: ISO8859-1
  src:
    description:
      - The location can be a UNIX System Services (USS) file or path,
        PS(sequential data set), PDS, PDSE, member of a PDS or PDSE, or
        KSDS(VSAM data set).
      - The USS path or file must be an absolute pathname.
      - If I(src) is a USS directory, all files will be encoded.
    required: true
    type: str
  dest:
    description:
      - The location where the converted characters are output.
      - The destination I(dest) can be a UNIX System Services (USS) file or path,
        PS(sequential data set), PDS, PDSE, member of a PDS or PDSE, or
        KSDS(VSAM data set).
      - If the length of the PDSE member name used in I(dest) is greater
        than 8 characters, the member name will be truncated when written out.
      - If I(dest) is not specified, the I(src) will be used as the destination
        and will overwrite the I(src) with the character set in the
        option I(to_encoding).
      - The USS file or path must be an absolute pathname.
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
    required: false
    type: str
  backup_compress:
    description:
      - Determines if backups to USS files or paths should be compressed.
      - I(backup_compress) is only used when I(backup=true).
    type: bool
    required: false
    default: false
notes:
  - It is the playbook author or user's responsibility to avoid files that should
    not be encoded, such as binary files. A user is described as the remote user,
    configured either for the playbook or playbook tasks, who can
    also obtain escalated privileges to execute as root or another user.
  - All data sets are always assumed to be cataloged. If an uncataloged data
    set needs to be encoded, it should be cataloged first.
  - For supported character sets used to encode data, refer to
    U(https://ansible-collections.github.io/ibm_zos_core/supplementary.html#encode)
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
    from_encoding: IBM-1047
    to_encoding: ISO8859-1
    backup: yes
    backup_compress: yes

- name: Convert file encoding from IBM-1047 to ISO8859-1 to a directory
  zos_encode:
    src: /zos_encode/test.data
    dest: /zos_encode_out/

- name: Convert file encoding from all files in a directory to another
    directory
  zos_encode:
    src: /zos_encode/
    dest: /zos_encode_out/
    from_encoding: ISO8859-1
    to_encoding: IBM-1047

- name: Convert file encoding from a USS file to a sequential data set
  zos_encode:
    src: /zos_encode/test.data
    dest: USER.TEST.PS
    from_encoding: IBM-1047
    to_encoding: ISO8859-1

- name: Convert file encoding from files in a directory to a partitioned
    data set
  zos_encode:
    src: /zos_encode/
    dest: USER.TEST.PDS
    from_encoding: ISO8859-1
    to_encoding: IBM-1047

- name: Convert file encoding from a USS file to a partitioned data set
    member
  zos_encode:
    src: /zos_encode/test.data
    dest: USER.TEST.PDS(TESTDATA)
    from_encoding: ISO8859-1
    to_encoding: IBM-1047

- name: Convert file encoding from a sequential data set to a USS file
  zos_encode:
    src: USER.TEST.PS
    dest: /zos_encode/test.data
    from_encoding: IBM-1047
    to_encoding: ISO8859-1

- name: Convert file encoding from a PDS encoding to a USS directory
  zos_encode:
    src: USER.TEST.PDS
    dest: /zos_encode/
    from_encoding: IBM-1047
    to_encoding: ISO8859-1

- name: Convert file encoding from a sequential data set to another
    sequential data set
  zos_encode:
    src: USER.TEST.PS
    dest: USER.TEST1.PS
    from_encoding: IBM-1047
    to_encoding: ISO8859-1

- name: Convert file encoding from a sequential data set to a
    partitioned data set (extended) member
  zos_encode:
    src: USER.TEST.PS
    dest: USER.TEST1.PDS(TESTDATA)
    from_encoding: IBM-1047
    to_encoding: ISO8859-1

- name: Convert file encoding from a USS file to a VSAM data set
  zos_encode:
    src: /zos_encode/test.data
    dest: USER.TEST.VS
    from_encoding: ISO8859-1
    to_encoding: IBM-1047

- name: Convert file encoding from a VSAM data set to a USS file
  zos_encode:
    src: USER.TEST.VS
    dest: /zos_encode/test.data
    from_encoding: IBM-1047
    to_encoding: ISO8859-1

- name: Convert file encoding from a VSAM data set to a sequential
    data set
  zos_encode:
    src: USER.TEST.VS
    dest: USER.TEST.PS
    from_encoding: IBM-1047
    to_encoding: ISO8859-1

- name: Convert file encoding from a sequential data set a VSAM data set
  zos_encode:
    src: USER.TEST.PS
    dest: USER.TEST.VS
    from_encoding: ISO8859-1
    to_encoding: IBM-1047

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
"""

import re
from os import path, makedirs
from ansible.module_utils.basic import AnsibleModule
from ansible_collections.ibm.ibm_zos_core.plugins.module_utils import (
    better_arg_parser,
    data_set,
    encode,
    backup as zos_backup,
)
from ansible_collections.ibm.ibm_zos_core.plugins.module_utils.import_handler import (
    MissingZOAUImport,
)

try:
    from zoautil_py import datasets
except Exception:
    datasets = MissingZOAUImport()


def check_pds_member(ds, mem):
    check_rc = False
    if mem in datasets.list_members(ds):
        check_rc = True
    else:
        raise EncodeError("Cannot find member {0} in {1}".format(mem, ds))
    return check_rc


def check_mvs_dataset(ds):
    """ To call data_set utils to check if the MVS data set exists or not """
    check_rc = False
    ds_type = None
    if not data_set.DataSet.data_set_exists(ds):
        raise EncodeError(
            "Data set {0} is not cataloged, please check data set provided in"
            "the src option.".format(ds)
        )
    else:
        check_rc = True
        ds_type = data_set.DataSetUtils(ds).ds_type()
        if not ds_type:
            raise EncodeError("Unable to determine data set type of {0}".format(ds))
    return check_rc, ds_type


def check_file(file):
    """ check file is a USS file/path or an MVS data set """
    is_uss = False
    is_mvs = False
    ds_type = None
    if path.sep in file:
        is_uss = True
    else:
        ds = file.upper()
        if "(" in ds:
            dsn = ds[0: ds.rfind("(", 1)]
            mem = "".join(re.findall(r"[(](.*?)[)]", ds))
            rc, ds_type = check_mvs_dataset(dsn)
            if rc:
                if ds_type == "PO":
                    is_mvs = check_pds_member(dsn, mem)
                    ds_type = "PS"
                else:
                    raise EncodeError(
                        "Data set {0} is not a partitioned data set".format(dsn)
                    )
        else:
            is_mvs, ds_type = check_mvs_dataset(ds)
    return is_uss, is_mvs, ds_type


def verify_uss_path_exists(file):
    if not path.exists(file):
        raise EncodeError("File {0} does not exist.".format(file))
    return


def run_module():
    module_args = dict(
        src=dict(type="str", required=True),
        dest=dict(type="str"),
        from_encoding=dict(type="str", default="IBM-1047"),
        to_encoding=dict(type="str", default="ISO8859-1"),
        backup=dict(type="bool", default=False),
        backup_name=dict(type="str", required=False, default=None),
        backup_compress=dict(type="bool", required=False, default=False),
    )

    module = AnsibleModule(argument_spec=module_args)

    arg_defs = dict(
        src=dict(arg_type="data_set_or_path", required=True),
        dest=dict(arg_type="data_set_or_path", required=False),
        from_encoding=dict(arg_type="str", default="IBM-1047"),
        to_encoding=dict(arg_type="str", default="ISO8859-1", required=False),
        backup=dict(arg_type="bool", default=False, required=False),
        backup_name=dict(arg_type="data_set_or_path", required=False, default=None),
        backup_compress=dict(arg_type="bool", required=False, default=False),
    )

    parser = better_arg_parser.BetterArgParser(arg_defs)
    parsed_args = parser.parse_args(module.params)
    src = parsed_args.get("src")
    dest = parsed_args.get("dest")
    backup = parsed_args.get("backup")
    backup_name = parsed_args.get("backup_name")
    backup_compress = parsed_args.get("backup_compress")
    from_encoding = parsed_args.get("from_encoding").upper()
    to_encoding = parsed_args.get("to_encoding").upper()

    # is_uss_src(dest) to determine whether the src(dest) is a USS file/path or not
    # is_mvs_src(dest) to determine whether the src(dest) is a MVS data set or not
    is_uss_src = False
    is_mvs_src = False
    is_uss_dest = False
    is_mvs_dest = False
    ds_type_src = None
    ds_type_dest = None
    convert_rc = False
    changed = False

    result = dict(changed=changed, src=src, dest=dest)
    if backup:
        result["backup_name"] = None

    try:
        # Check the src is a USS file/path or an MVS data set
        is_uss_src, is_mvs_src, ds_type_src = check_file(src)
        if is_uss_src:
            verify_uss_path_exists(src)
        result["src"] = src

        # Check the dest is a USS file/path or an MVS data set
        # if the dest is not specified, the value in the src will be used
        if not dest:
            dest = src
            is_uss_dest = is_uss_src
            is_mvs_dest = is_mvs_src
            ds_type_dest = ds_type_src
        else:
            is_uss_dest, is_mvs_dest, ds_type_dest = check_file(dest)
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

        # Check if the dest is required to be backup before conversion
        if backup:
            if is_uss_dest:
                backup_name = zos_backup.uss_file_backup(
                    dest, backup_name, backup_compress
                )
            if is_mvs_dest:
                backup_name = zos_backup.mvs_file_backup(dest, backup_name)
            result["backup_name"] = backup_name

        eu = encode.EncodeUtils()
        # Check input code set is valid or not
        # If the value specified in from_encoding or to_encoding is not in the code_set, exit with an error message
        # If the values specified in from_encoding and to_encoding are the same, exit with an message
        code_set = eu.get_codeset()
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
                src, dest, from_encoding, to_encoding
            )
        else:
            convert_rc = eu.mvs_convert_encoding(
                src,
                dest,
                from_encoding,
                to_encoding,
                src_type=ds_type_src,
                dest_type=ds_type_dest,
            )

        if convert_rc:
            changed = True
            result = dict(changed=changed, src=src, dest=dest, backup_name=backup_name)
        else:
            result = dict(src=src, dest=dest, changed=changed, backup_name=backup_name)
    except Exception as e:
        module.fail_json(msg=repr(e), **result)

    module.exit_json(**result)


class EncodeError(Exception):
    def __init__(self, message):
        self.msg = 'An error occurred during encoding: "{0}"'.format(message)
        super(EncodeError, self).__init__(self.msg)


def main():
    run_module()


if __name__ == "__main__":
    main()
