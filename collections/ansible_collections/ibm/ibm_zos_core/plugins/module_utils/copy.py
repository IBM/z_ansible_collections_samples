# Copyright (c) IBM Corporation 2020
# Apache License, Version 2.0 (see https://opensource.org/licenses/Apache-2.0)


from __future__ import absolute_import, division, print_function

__metaclass__ = type


from ansible.module_utils.six import PY3
from ansible_collections.ibm.ibm_zos_core.plugins.module_utils.ansible_module import (
    AnsibleModuleHelper,
)
from ansible_collections.ibm.ibm_zos_core.plugins.module_utils.better_arg_parser import (
    BetterArgParser,
)

if PY3:
    from shlex import quote
else:
    from pipes import quote


REPRO = """  REPRO INDATASET({}) -
    OUTDATASET({}) REPLACE """


def _validate_data_set_name(ds):
    arg_defs = dict(ds=dict(arg_type="data_set"),)
    parser = BetterArgParser(arg_defs)
    parsed_args = parser.parse_args({"ds": ds})
    return parsed_args.get("ds")


def _validate_path(path):
    arg_defs = dict(path=dict(arg_type="path"),)
    parser = BetterArgParser(arg_defs)
    parsed_args = parser.parse_args({"path": path})
    return parsed_args.get("path")


def copy_uss2mvs(src, dest, ds_type, is_binary=False):
    """Copy uss a file or path to an MVS data set

    Arguments:
        src: {str} -- The uss file or path to be copied
        dest: {str} -- The destination MVS data set, it must be a PS or PDS(E)
        ds_type: {str} -- The dsorg of the dest.

    Keyword Arguments:
        is_binary: {bool} -- Whether the file to be copied contains binary data

    Raises:
        USSCmdExecError: When any exception is raised during the conversion.
    Returns:
        boolean -- The return code after the copy command executed successfully
        str -- The stdout after the copy command executed successfully
        str -- The stderr after the copy command executed successfully
    """
    module = AnsibleModuleHelper(argument_spec={})
    src = _validate_path(src)
    dest = _validate_data_set_name(dest)
    if ds_type == "PO":
        cp_uss2mvs = "cp -CM -F rec {0} \"//'{1}'\"".format(quote(src), dest)
    else:
        cp_uss2mvs = "cp -F rec {0} \"//'{1}'\"".format(quote(src), dest)
    if is_binary:
        cp_uss2mvs = cp_uss2mvs.replace("rec", "bin", 1)
    rc, out, err = module.run_command(cp_uss2mvs)
    if rc:
        raise USSCmdExecError(cp_uss2mvs, rc, out, err)
    return rc, out, err


def copy_ps2uss(src, dest, is_binary=False):
    """Copy a PS data set to a uss file

    Arguments:
        src: {str} -- The MVS data set to be copied, it must be a PS data set
        or a PDS(E) member
        dest: {str} -- The destination uss file

    Keyword Arguments:
        is_binary: {bool} -- Whether the file to be copied contains binary data

    Raises:
        USSCmdExecError: When any exception is raised during the conversion
    Returns:
        boolean -- The return code after the copy command executed successfully
        str -- The stdout after the copy command executed successfully
        str -- The stderr after the copy command executed successfully
    """
    module = AnsibleModuleHelper(argument_spec={})
    src = _validate_data_set_name(src)
    dest = _validate_path(dest)
    cp_ps2uss = "cp -F rec \"//'{0}'\" {1}".format(src, quote(dest))
    if is_binary:
        cp_ps2uss = cp_ps2uss.replace("rec", "bin", 1)
    rc, out, err = module.run_command(cp_ps2uss)
    if rc:
        raise USSCmdExecError(cp_ps2uss, rc, out, err)
    return rc, out, err


def copy_pds2uss(src, dest, is_binary=False):
    """Copy the whole PDS(E) to a uss path

    Arguments:
        src: {str} -- The MVS data set to be copied, it must be a PDS(E) data set
        dest: {str} -- The destination uss path

    Keyword Arguments:
        is_binary: {bool} -- Whether the file to be copied contains binary data

    Raises:
        USSCmdExecError: When any exception is raised during the conversion.
    Returns:
        boolean -- The return code after the USS command executed successfully
        str -- The stdout after the USS command executed successfully
        str -- The stderr after the USS command executed successfully
    """
    module = AnsibleModuleHelper(argument_spec={})
    src = _validate_data_set_name(src)
    dest = _validate_path(dest)
    cp_pds2uss = "cp -U -F rec \"//'{0}'\" {1}".format(src, quote(dest))
    if is_binary:
        cp_pds2uss = cp_pds2uss.replace("rec", "bin", 1)
    rc, out, err = module.run_command(cp_pds2uss)
    if rc:
        raise USSCmdExecError(cp_pds2uss, rc, out, err)
    return rc, out, err


def copy_uss2uss_binary(src, dest):
    """Copy a USS file to a USS location in binary mode

    Arguments:
        src: {str} -- The source USS path
        dest: {str} -- The destination USS path
    Raises:
        USSCmdExecError: When any exception is raised during the conversion.
    Returns:
        boolean -- The return code after the USS command executed successfully
        str -- The stdout after the USS command executed successfully
        str -- The stderr after the USS command executed successfully
    """
    module = AnsibleModuleHelper(argument_spec={})
    src = _validate_path(src)
    dest = _validate_path(dest)
    cp_uss2uss = "cp -F bin {0} {1}".format(quote(src), quote(dest))
    rc, out, err = module.run_command(cp_uss2uss)
    if rc:
        raise USSCmdExecError(cp_uss2uss, rc, out, err)
    return rc, out, err


def copy_mvs2mvs(src, dest, is_binary=False):
    """Copy an MVS source to MVS target

    Arguments:
        src: {str} -- Name of source data set
        dest: {str} -- Name of destination data set

    Keyword Arguments:
        is_binary: {bool} -- Whether the data set to be copied contains binary data

    Raises:
        USSCmdExecError: When any exception is raised during the conversion.
    Returns:
        boolean -- The return code after the USS command executed successfully
        str -- The stdout after the USS command executed successfully
        str -- The stderr after the USS command executed successfully
    """
    module = AnsibleModuleHelper(argument_spec={})
    src = _validate_data_set_name(src)
    dest = _validate_data_set_name(dest)
    cp_mvs2mvs = "cp -F rec \"//'{0}'\" \"//'{1}'\"".format(src, dest)
    if is_binary:
        cp_mvs2mvs = cp_mvs2mvs.replace("rec", "bin", 1)
    rc, out, err = module.run_command(cp_mvs2mvs)
    if rc:
        raise USSCmdExecError(cp_mvs2mvs, rc, out, err)
    return rc, out, err


def copy_vsam_ps(src, dest):
    """Copy a VSAM(KSDS) data set to a PS data set vise versa

    Arguments:
        src: {str} -- The VSAM(KSDS) or PS data set to be copied
        dest: {str} -- The PS or VSAM(KSDS) data set

    Raises:
        USSCmdExecError: When any exception is raised during the conversion
    Returns:
        boolean -- The return code after the USS command executed successfully
        str -- The stdout after the USS command executed successfully
        str -- The stderr after the USS command executed successfully
    """
    module = AnsibleModuleHelper(argument_spec={})
    src = _validate_data_set_name(src)
    dest = _validate_data_set_name(dest)
    repro_cmd = REPRO.format(src, dest)
    cmd = "mvscmdauth --pgm=idcams --sysprint=stdout --sysin=stdin"
    rc, out, err = module.run_command(cmd, data=repro_cmd)
    if rc:
        raise USSCmdExecError(cmd, rc, out, err)
    return rc, out, err


class USSCmdExecError(Exception):
    def __init__(self, uss_cmd, rc, out, err):
        self.msg = (
            "Failed during execution of usscmd: {0}, Return code: {1}; "
            "stdout: {2}; stderr: {3}".format(uss_cmd, rc, out, err)
        )
        super().__init__(self.msg)
