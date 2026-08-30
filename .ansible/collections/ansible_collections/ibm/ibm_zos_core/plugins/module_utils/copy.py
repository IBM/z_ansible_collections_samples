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


import traceback
from os import path
from ansible_collections.ibm.ibm_zos_core.plugins.module_utils.ansible_module import (
    AnsibleModuleHelper,
)
from ansible_collections.ibm.ibm_zos_core.plugins.module_utils.better_arg_parser import (
    BetterArgParser,
)
from ansible_collections.ibm.ibm_zos_core.plugins.module_utils.mvs_cmd import (
    ikjeft01
)
from ansible_collections.ibm.ibm_zos_core.plugins.module_utils.import_handler import \
    ZOAUImportError

try:
    from zoautil_py import datasets, gdgs, exceptions as zoau_exceptions
except Exception:
    datasets = ZOAUImportError(traceback.format_exc())
    gdgs = ZOAUImportError(traceback.format_exc())
    zoau_exceptions = ZOAUImportError(traceback.format_exc())

REPRO = """  REPRO INDATASET({}) -
    OUTDATASET({}) REPLACE """


def _validate_data_set_name(ds):
    """Validate data set name using BetterArgParser.

    Parameters
    ----------
    ds : str
        The source dataset.

    Returns
    -------
    str
        Parsed dataset.
    """
    arg_defs = dict(ds=dict(arg_type="data_set"),)
    parser = BetterArgParser(arg_defs)
    parsed_args = parser.parse_args({"ds": ds})
    return parsed_args.get("ds")


def _validate_path(path):
    """Validate path using BetterArgParser.

    Parameters
    ----------
    path : str
        The path.

    Returns
    -------
    str
        Parsed path.
    """
    arg_defs = dict(path=dict(arg_type="path"),)
    parser = BetterArgParser(arg_defs)
    parsed_args = parser.parse_args({"path": path})
    return parsed_args.get("path")


def copy_uss_mvs(src, dest, binary=False):
    """Wrapper function for datasets.copy that handles possible
    exceptions that may occur.

    Parameters
    ----------
    src : str
        Source dataset or file.
    dest : str
        Destination dataset name or file.

    Keyword Parameters
    ------------------
    binary : bool
        Whether to perform a binary copy.

    Returns
    -------
    tuple
        Tuple containing return code, standard output and standard
        error from the datasets API.

    Raises
    ------
    ZOAUImportError
        When there's an issue calling the datasets API.
    """
    copy_args = {
        "options": ""
    }

    if binary:
        copy_args["options"] = "-B"

    try:
        datasets.copy(source=src, target=dest, **copy_args)
    except zoau_exceptions.ZOAUException as copy_exception:
        # Returning the exception content instead of raising it
        # since a lot of code that uses this function expects it
        # so they can decide what to do in case of an error.
        return copy_exception.response.rc, \
            copy_exception.response.stdout_response, \
            copy_exception.response.stderr_response

    return 0, "", ""


def copy_gdg2uss(src, dest, binary=False, asa_text=False):
    """Copy a whole GDG to a USS path.

    Parameters
    ----------
    src : str
        The MVS data set to be copied, it must be a generation data group.
    dest : str
        The destination USS path.

    Keyword Parameters
    ------------------
    binary : bool
        Whether the file to be copied contains binary data.
    asa_text : bool
        Whether the file to be copied contains ASA control
        characters.

    Returns
    -------
    bool
        True if all copies were successful, False otherwise.
    """
    src_view = gdgs.GenerationDataGroupView(src)
    generations = src_view.generations

    copy_args = {
        "options": ""
    }

    if binary or asa_text:
        copy_args["options"] = "-B"

    for gds in generations:
        dest_file = path.join(dest, gds.name)
        rc = datasets.copy(gds.name, dest_file, **copy_args)

        if rc != 0:
            return False

    return True


def copy_vsam_ps(src, dest, tmphlq=None):
    """Copy a VSAM(KSDS) data set to a PS data set vise versa.

    Parameters
    ----------
    src : str
        The VSAM(KSDS) or PS data set to be copied.
    dest : str
        The PS or VSAM(KSDS) data set.
    tmphlq : str
        High Level Qualifier for temporary datasets.

    Returns
    -------
    bool
        The return code after the USS command executed successfully.
    str
        The stdout after the USS command executed successfully.
    str
        The stderr after the USS command executed successfully.

    Raises
    ------
    USSCmdExecError
        When any exception is raised during the conversion.
    """
    module = AnsibleModuleHelper(argument_spec={})
    src = _validate_data_set_name(src)
    dest = _validate_data_set_name(dest)
    repro_cmd = REPRO.format(src, dest)

    cmd = "mvscmdauth --pgm=idcams --sysprint=stdout --sysin=stdin"
    if tmphlq:
        cmd = "{0} -Q={1}".format(cmd, tmphlq)

    rc, out, err = module.run_command(cmd, data=repro_cmd, errors='replace')
    if rc:
        raise USSCmdExecError(cmd, rc, out, err)
    return rc, out, err


def copy_asa_uss2mvs(src, dest, tmphlq=None, force=False):
    """Copy a file from USS to an ASA sequential data set or PDS/E member.

    Parameters
    ----------
    src : str
        Path of the USS file.
    dest : str
        The MVS destination data set or member.
    tmphlq : str
        High Level Qualifier for temporary datasets.
    force : bool
        Whether to open the destination in SHR mode.

    Returns
    -------
    bool
        The return code after the copy command executed successfully.
    str
        The stdout after the copy command executed successfully.
    str
        The stderr after the copy command executed successfully.
    """

    # Removes escaping to execute this command
    dest = dest.replace('\\', '')
    src = src.replace('\\', '')
    dest_dsp = "shr" if force else "old"

    ocopy_cmd = "OCOPY INDD(DSSRC) OUTDD(DSTAR) TEXT"
    ocopy_dds = {
        "dssrc": src,
        "dstar": f"{dest},{dest_dsp}"
    }
    rc, out, err = ikjeft01(ocopy_cmd, dds=ocopy_dds, authorized=True, tmphlq=tmphlq)

    return TSOCmdResponse(rc, out, err)


def copy_asa_mvs2uss(src, dest, tmphlq=None):
    """Copy an ASA sequential data set or member to USS.

    Parameters
    ----------
    src : str
        The MVS data set to be copied.
    dest : str
        Destination path in USS.
    tmphlq : str
        High Level Qualifier for temporary datasets.

    Returns
    -------
    bool
        The return code after the copy command executed successfully.
    str
        The stdout after the copy command executed successfully.
    str
        The stderr after the copy command executed successfully.
    """
    src = _validate_data_set_name(src)
    dest = _validate_path(dest)
    # Removes escaping to execute this command
    new_dest = dest.replace('\\', '')
    new_source = src.replace('\\', '')
    oput_cmd = "OPUT '{0}' '{1}'".format(new_source, new_dest)
    rc, out, err = ikjeft01(oput_cmd, authorized=True, tmphlq=tmphlq)

    return TSOCmdResponse(rc, out, err)


def copy_asa_pds2uss(src, dest, tmphlq=None):
    """Copy all members from an ASA PDS/E to USS.

    Parameters
    ----------
    src : str
        The MVS data set to be copied.
    dest : str
        Destination path in USS (must be a directory).
    tmphlq : str
        High Level Qualifier for temporary datasets.

    Returns
    -------
    bool
        The return code after the copy command executed successfully.
    str
        The stdout after the copy command executed successfully.
    str
        The stderr after the copy command executed successfully.
    """
    from os import path
    import traceback
    from ansible_collections.ibm.ibm_zos_core.plugins.module_utils.import_handler import (
        ZOAUImportError,
    )

    try:
        from zoautil_py import datasets
    except Exception:
        datasets = ZOAUImportError(traceback.format_exc())

    src = _validate_data_set_name(src)
    dest = _validate_path(dest)

    for member in datasets.list_members(src):
        src_member = '{0}({1})'.format(src, member)
        dest_path = path.join(dest, member)

        oput_cmd = "OPUT '{0}' '{1}'".format(src_member, dest_path)
        rc, out, err = ikjeft01(oput_cmd, authorized=True, tmphlq=tmphlq)

        if rc != 0:
            return TSOCmdResponse(rc, out, err)

    return TSOCmdResponse(0, '', '')


class TSOCmdResponse():
    def __init__(self, rc, stdout, stderr):
        """Builds TSO cmd response.

        Parameters
        ----------
        rc : int
            Return code.
        stdout : str
            Standard output.
        stderr : str
            Standard error.

        Attributes
        ----------
        rc : int
            Return code.
        stdout : str
            Standard output.
        stderr : str
            Standard error.
        """
        self.rc = rc
        self.stdout_response = stdout
        self.stderr_response = stderr


class USSCmdExecError(Exception):
    def __init__(self, uss_cmd, rc, out, err):
        """Error during USS cmd execution.

        Parameters
        ----------
        uss_cmd : str
            USS command.
        rc : int
            Return code.
        out : str
            Standard output.
        err : str
            Standard error.

        Attributes
        ----------
        msg : str
            Human readable string describing the exception.
        """
        self.msg = (
            "Failed during execution of usscmd: {0}, Return code: {1}; "
            "stdout: {2}; stderr: {3}".format(uss_cmd, rc, out, err)
        )
        super().__init__(self.msg)
