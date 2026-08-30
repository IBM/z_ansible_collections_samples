# Copyright (c) IBM Corporation 2020 - 2024
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
from ansible_collections.ibm.ibm_zos_core.plugins.module_utils.ansible_module import (
    AnsibleModuleHelper,
)

import time
from shutil import copy2, copytree, rmtree
import traceback
from ansible_collections.ibm.ibm_zos_core.plugins.module_utils.import_handler import (
    ZOAUImportError,
)
from ansible_collections.ibm.ibm_zos_core.plugins.module_utils.better_arg_parser import (
    BetterArgParser,
)
from ansible_collections.ibm.ibm_zos_core.plugins.module_utils.file import make_dirs

from ansible_collections.ibm.ibm_zos_core.plugins.module_utils.data_set import (
    is_member,
    extract_dsname,
    temp_member_name,
    DataSet,
)
from ansible_collections.ibm.ibm_zos_core.plugins.module_utils.mvs_cmd import iebcopy

try:
    from zoautil_py import datasets, exceptions
except Exception:
    datasets = ZOAUImportError(traceback.format_exc())
    exceptions = ZOAUImportError(traceback.format_exc())

from shlex import quote


def _validate_data_set_name(ds):
    """Validate data set name.

    Parameters
    ----------
    ds : str
        The source dataset.

    Returns
    -------
    str
        Parsed dataset.
    """
    arg_defs = dict(ds=dict(arg_type="data_set"))
    parser = BetterArgParser(arg_defs)
    parsed_args = parser.parse_args({"ds": ds})
    return parsed_args.get("ds")


def mvs_file_backup(dsn, bk_dsn=None, tmphlq=None):
    """Create a backup data set for an MVS data set.

    Parameters
    ----------
    dsn : str
        The name of the data set to backup.
        It could be an MVS PS/PDS/PDSE/VSAM(KSDS), etc.
    bk_dsn : str
        The name of the backup data set.
    tmphlq : str
        High Level Qualifier for temporary datasets.

    Returns
    -------
    str
        The backup dataset

    Raises
    ------
    BackupError
        When backup data set exists.
    BackupError
        When creation of backup data set fails.
    """
    dsn = _validate_data_set_name(dsn).upper()
    if is_member(dsn):
        # added the check for a sub-mmember, just in this case
        if not bk_dsn or "(" not in bk_dsn:
            bk_dsn = extract_dsname(dsn) + "({0})".format(temp_member_name())
        elif DataSet.is_gds_positive_relative_name(bk_dsn):
            bk_dsn = datasets.create(bk_dsn)

        bk_dsn = _validate_data_set_name(bk_dsn).upper()
        try:
            datasets.copy(dsn, bk_dsn)
        except exceptions.ZOAUException as copy_exception:
            raise BackupError(
                "Unable to backup {0} to {1}".format(dsn, bk_dsn),
                rc=copy_exception.response.rc,
                stdout=copy_exception.response.stdout_response,
                stderr=copy_exception.response.stderr_response
            )
    else:
        if not bk_dsn:
            if tmphlq:
                hlq = tmphlq
            else:
                hlq = datasets.get_hlq()
            bk_dsn = datasets.tmp_name(high_level_qualifier=hlq)
        bk_dsn = _validate_data_set_name(bk_dsn).upper()

        # In case the backup ds is a member we trust that the PDS attributes are ok to fit the src content.
        # This should not delete a PDS just to create a backup member.
        # Otherwise, we allocate the appropriate space for the backup ds based on src.
        if is_member(bk_dsn):
            try:
                cp_rc = datasets.copy(dsn, bk_dsn)
            except exceptions.ZOAUException as copy_exception:
                cp_rc = copy_exception.response.rc
        else:
            if DataSet.is_gds_positive_relative_name(bk_dsn):
                cp_rc = datasets.copy(dsn, bk_dsn)
            else:
                cp_rc = _copy_ds(dsn, bk_dsn, tmphlq=tmphlq)

        if cp_rc == 12:  # The data set is probably a PDS or PDSE
            # Delete allocated backup that was created when attempting to use _copy_ds()
            # Safe to delete because _copy_ds() would have raised an exception if it did
            # not successfully create the backup data set, so no risk of it predating module invocation
            datasets.delete(bk_dsn)
            _allocate_model(bk_dsn, dsn, tmphlq=tmphlq)
            rc, out, err = _copy_pds(dsn, bk_dsn)
            if rc != 0:
                raise BackupError(
                    "Unable to backup data set {0} to {1}.".format(dsn, bk_dsn),
                    rc=rc,
                    stdout=out,
                    stderr=err
                )
    return bk_dsn


def uss_file_backup(path, backup_name=None, compress=False):
    """Create a backup file for a USS file or path.

    Parameters
    ----------
    path : str
        The name of the USS file or path to backup.
    backup_name : str
        The name of the backup file.

    Keyword Parameters
    ------------------
    compress : bool
        Determines if the backup be compressed. (default: {False})

    Returns
    -------
    str
        Name of the backup file.

    Raises
    ------
    BackupError
        When creating compressed backup fails.
    """
    abs_path = os.path.abspath(path)

    if not os.path.exists(abs_path):
        raise BackupError("Path to be backed up does not exist.")

    module = AnsibleModuleHelper(argument_spec={})

    ext = time.strftime("%Y-%m-%d-%H-%M-%S", time.localtime()).lower()
    if os.path.isdir(abs_path):
        default_backup_name = "{0}@{1}-bak".format(abs_path[:-1], ext)
    else:
        default_backup_name = "{0}@{1}-bak".format(abs_path, ext)

    backup_base = os.path.basename(default_backup_name)
    backup_name_provided = True

    if not backup_name:
        backup_name = default_backup_name
        backup_name_provided = False

    if os.path.isdir(abs_path) and backup_name[-1] != "/" and not compress:
        backup_name += "/"
        make_dirs(backup_name, mode_from=abs_path)
    if backup_name[-1] == "/" and not os.path.isdir(backup_name):
        make_dirs(backup_name)
    elif os.path.isdir(backup_name) and backup_name[-1] != "/":
        backup_name += "/"

    if compress:
        if backup_name_provided and os.path.isdir(backup_name):
            backup_name += backup_base
        bk_cmd = "tar -cpf {0}.tar {1}".format(quote(backup_name), quote(abs_path))
        rc, out, err = module.run_command(bk_cmd, errors='replace')
        if rc:
            raise BackupError(err)
        backup_name += ".tar"
    else:
        if os.path.isdir(abs_path):
            if os.path.exists(backup_name):
                rmtree(backup_name)
            copytree(abs_path, backup_name)
        elif not os.path.isdir(abs_path) and os.path.isdir(backup_name):
            backup_name = backup_name + os.path.basename(abs_path)
            copy2(abs_path, backup_name)
        else:
            copy2(abs_path, backup_name)

    return backup_name


def _copy_ds(ds, bk_ds, tmphlq=None):
    """Copy the contents of a data set to another.

    Parameters
    ----------
    ds : str
        The source data set to be copied from. Should be SEQ or VSAM.
    bk_dsn : str
        The destination data set to copy to.
    tmphlq : str
        High Level Qualifier for temporary datasets.

    Returns
    -------
    int
        Return code.

    Raises
    ------
    BackupError
        When copying data fails.
    """
    module = AnsibleModuleHelper(argument_spec={})
    _allocate_model(bk_ds, ds, tmphlq=tmphlq)
    repro_cmd = """  REPRO -
    INDATASET('{0}') -
    OUTDATASET('{1}')""".format(
        ds, bk_ds
    )

    cmd = "mvscmdauth --pgm=idcams --sysprint=* --sysin=stdin"
    if tmphlq:
        cmd = "{0} -Q={1}".format(cmd, tmphlq)

    rc, out, err = module.run_command(
        cmd, data=repro_cmd, errors='replace'
    )
    if rc != 0 and rc != 12:
        datasets.delete(bk_ds)
        raise BackupError(
            "Unable to backup data set {0}; stdout: {1}; stderr: {2}".format(
                ds, out, err
            )
        )
    if rc != 0 and DataSet.is_empty(ds, tmphlq=tmphlq):
        rc = 0
    return rc


def _allocate_model(ds, model, tmphlq=None):
    """Allocate a data set using allocation information of a model data set.

    Parameters
    ----------
    ds : str
        The name of the data set to be allocated.
    model : str
        The name of the data set whose allocation parameters should be used.
    tmphlq : str
        High Level Qualifier for temporary datasets.

    Returns
    -------
    int
        Return code.

    Raises
    ------
    BackupError
        When allocation fails.
    """
    module = AnsibleModuleHelper(argument_spec={})
    alloc_cmd = """  ALLOC -
    DS('{0}') -
    LIKE('{1}')""".format(
        ds, model
    )

    cmd = "mvscmdauth --pgm=ikjeft01 --systsprt=* --systsin=stdin"
    if tmphlq:
        cmd = "{0} -Q={1}".format(cmd, tmphlq)

    rc, out, err = module.run_command(cmd, data=alloc_cmd, errors='replace')
    if rc != 0:
        raise BackupError(
            "Unable to allocate data set {0}; stdout: {1}; stderr: {2}".format(
                ds, out, err
            )
        )
    return rc


def _copy_pds(ds, bk_dsn):
    """Copy a dataset.

    Parameters
    ----------
    ds : str
        The name of the data set to be allocated.
    bk_dsn : str
        The destination data set to copy to.

    Returns
    -------
    str
        Copied dataset.
    """
    dds = dict(OUTPUT=bk_dsn, INPUT=ds)
    copy_cmd = "   COPY OUTDD=OUTPUT,INDD=((INPUT,R))"
    return iebcopy(copy_cmd, dds=dds)


class BackupError(Exception):
    def __init__(self, message, rc=None, stdout=None, stderr=None):
        """Error during backup.

        Parameters
        ----------
        message : str
            Human readable string describing the exception.
        rc : int
            Return code.
        stdout : str
            Standard output.
        stderr : str
            Standard error.

        Attributes
        ----------
        msg : str
            Human readable string describing the exception.
        rc : int
            Return code.
        stdout : str
            Standard output.
        stderr : str
            Standard error.
        """
        self.msg = 'An error occurred during backup: "{0}"'.format(message)
        self.rc = rc
        self.stdout = stdout
        self.stderr = stderr
        super(BackupError, self).__init__(self.msg)
