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
---
module: zos_fetch
version_added: "1.1.0"
short_description: Fetch data from z/OS
description:
  - This module fetches a UNIX System Services (USS) file,
    PS (sequential data set), PDS, PDSE, member of a PDS or PDSE,
    generation data set (GDS), generation data group (GDG), or
    KSDS (VSAM data set) from a remote z/OS system.
  - When fetching a sequential data set, the destination file name will be the
    same as the data set name.
  - When fetching a PDS or PDSE, the destination will be a directory with the
    same name as the PDS or PDSE.
  - When fetching a PDS/PDSE member, destination will be a file.
  - Files that already exist at C(dest) will be overwritten if they are different
    than C(src).
  - When fetching a GDS, the relative name will be resolved to its absolute one.
  - When fetching a generation data group, the destination will be a directory
    with the same name as the GDG.
author:
    - "Asif Mahmud (@asifmahmud)"
    - "Demetrios Dimatos (@ddimatos)"
options:
  src:
    description:
      - Name of a UNIX System Services (USS) file, PS (sequential data set), PDS,
        PDSE, member of a PDS, PDSE, GDS, GDG or KSDS (VSAM data set).
      - USS file paths should be absolute paths.
    required: true
    type: str
  dest:
    description:
      - Local path where the file or data set will be stored.
      - If dest is an existing file or directory, the contents will be
        overwritten.
    required: true
    type: path
  fail_on_missing:
    description:
      - When set to true, the task will fail if the source file is missing.
    required: false
    default: "true"
    type: bool
  validate_checksum:
    description:
      - Verify that the source and destination checksums match after the files
        are fetched.
    required: false
    default: "true"
    type: bool
  flat:
    description:
      - If set to "true", override the default behavior of appending hostname/path/to/file to the
        destination, instead the file or data set will be fetched to
        the destination directory without appending remote hostname to the
        destination.
    required: false
    default: "false"
    type: bool
  binary:
    description:
      - Specifies if the file being fetched is a binary.
    required: false
    default: "false"
    type: bool
  use_qualifier:
    description:
      - Indicates whether the data set high level qualifier should be used when
        fetching.
    required: false
    default: "false"
    type: bool
  encoding:
    description:
      - Specifies which encodings the fetched data set should be converted from
        and to. If this parameter is not provided, encoding conversions will
        not take place.
    required: false
    type: dict
    suboptions:
      from:
        description:
          - The character set of the source I(src).
          - Supported character sets rely on the charset conversion utility
            (iconv) version; the most common character sets are supported.
        required: true
        type: str
      to:
        description:
          - The destination I(dest) character set for the output to be written as.
          - Supported character sets rely on the charset conversion utility
            (iconv) version; the most common character sets are supported.
        required: true
        type: str
  tmp_hlq:
    description:
      - Override the default high level qualifier (HLQ) for temporary and backup
        datasets.
      - The default HLQ is the Ansible user used to execute the module and if
        that is not available, then the value C(TMPHLQ) is used.
    required: false
    type: str
  ignore_sftp_stderr:
    description:
      - During data transfer through SFTP, the SFTP command directs content to
        stderr. By default, the module essentially ignores the stderr stream
        produced by SFTP and continues execution. The user is able to override
        this behavior by setting this parameter to C(false). By doing so, any
        content written to stderr is considered an error by Ansible and will
        cause the module to fail.
      - When Ansible verbosity is set to greater than 3, either through the
        command line interface (CLI) using B(-vvvv) or through environment
        variables such as B(verbosity = 4), then this parameter will
        automatically be set to C(true).
    type: bool
    required: false
    default: true

attributes:
  action:
    support: full
    description: Indicates this has a corresponding action plugin so some parts of the options can be executed on the controller.
  async:
    support: none
    description: Supports being used with the ``async`` keyword.
  check_mode:
    support: none
    description: Can run in check_mode and return changed status prediction without modifying target. If not supported, the action will be skipped.

notes:
    - When fetching PDSE and VSAM data sets, temporary storage will be used
      on the remote z/OS system. After the PDSE or VSAM data set is
      successfully transferred, the temporary storage will be deleted. The size
      of the temporary storage will correspond to the size of PDSE or VSAM
      data set being fetched. If module execution fails, the temporary
      storage will be deleted.
    - To ensure optimal performance, data integrity checks for PDS, PDSE, and
      members of PDS or PDSE are done through the transfer methods used.
      As a result, the module response will not include
      the C(checksum) parameter.
    - All data sets are always assumed to be cataloged. If an uncataloged
      data set needs to be fetched, it should be cataloged first.
    - Fetching HFS or ZFS type data sets is currently not supported.
    - For supported character sets used to encode data, refer to the
      L(documentation,https://ibm.github.io/z_ansible_collections_doc/ibm_zos_core/docs/source/resources/character_set.html).
    - This module uses SFTP (Secure File Transfer Protocol) for the underlying
      transfer protocol; SCP (secure copy protocol) and Co:Z SFTP are not supported. In the
      case of Co:z SFTP, you can exempt the Ansible user id on z/OS from using Co:Z thus falling
      back to using standard SFTP. If the module detects SCP, it will temporarily use SFTP for
      transfers, if not available, the module will fail.
seealso:
- module: ibm.ibm_zos_core.zos_data_set
- module: ibm.ibm_zos_core.zos_copy
"""

EXAMPLES = r"""
- name: Fetch file from USS and store in /tmp/fetched/hostname/tmp/somefile
  zos_fetch:
    src: /tmp/somefile
    dest: /tmp/fetched

- name: Fetch a sequential data set and store in /tmp/SOME.DATA.SET
  zos_fetch:
    src: SOME.DATA.SET
    dest: /tmp/
    flat: true

- name: Fetch a PDS as binary and store in /tmp/SOME.PDS.DATASET
  zos_fetch:
    src: SOME.PDS.DATASET
    dest: /tmp/
    flat: true
    binary: true

- name: Fetch a UNIX file and don't validate its checksum
  zos_fetch:
    src: /tmp/somefile
    dest: /tmp/
    flat: true
    validate_checksum: false

- name: Fetch a VSAM data set
  zos_fetch:
    src: USER.TEST.VSAM
    dest: /tmp/
    flat: true

- name: Fetch a PDS member named 'DATA'
  zos_fetch:
    src: USER.TEST.PDS(DATA)
    dest: /tmp/
    flat: true

- name: Fetch a USS file and convert from IBM-037 to ISO8859-1
  zos_fetch:
    src: /etc/profile
    dest: /tmp/
    encoding:
      from: IBM-037
      to: ISO8859-1
    flat: true

- name: Fetch the current generation data set from a GDG
  zos_fetch:
    src: USERHLQ.DATA.SET(0)
    dest: /tmp/
    flat: true

- name: Fetch a previous generation data set from a GDG
  zos_fetch:
    src: USERHLQ.DATA.SET(-3)
    dest: /tmp/
    flat: true

- name: Fetch a generation data group
  zos_fetch:
    src: USERHLQ.TEST.GDG
    dest: /tmp/
    flat: true
"""

RETURN = r"""
src:
    description:
        - The source file path or data set on the remote machine.
        - If the source is not found, then src will be empty.
    returned: success
    type: str
    sample: SOME.DATA.SET
dest:
    description: The destination file path on the controlling machine.
    returned: success
    type: str
    sample: /tmp/SOME.DATA.SET
binary:
    description: Indicates the transfer mode that was used to fetch.
    returned: success
    type: bool
    sample: True
checksum:
    description: The SHA256 checksum of the fetched file or data set. checksum
       validation is performed for all USS files and sequential data sets.
    returned: success and src is a non-partitioned data set
    type: str
    sample: 8d320d5f68b048fc97559d771ede68b37a71e8374d1d678d96dcfa2b2da7a64e
data_set_type:
    description: Indicates the fetched data set type.
    returned: success
    type: str
    sample: PDSE
msg:
    description: Any important messages from the module.
    returned: always
    type: str
    sample: The source 'TEST.DATA.SET' does not exist or is uncataloged.
stdout:
    description: The stdout from a USS command or MVS command, if applicable.
    returned: failure
    type: str
    sample: DATA SET 'USER.PROCLIB' NOT IN CATALOG
stderr:
    description: The stderr of a USS command or MVS command, if applicable
    returned: failure
    type: str
    sample: File /tmp/result.log not found.
stdout_lines:
    description: List of strings containing individual lines from stdout
    returned: failure
    type: list
    sample: [u'USER.TEST.PDS NOT IN CATALOG..']
stderr_lines:
    description: List of strings containing individual lines from stderr.
    returned: failure
    type: list
    sample: [u'Unable to traverse PDS USER.TEST.PDS not found']
rc:
    description: The return code of a USS command or MVS command, if applicable.
    returned: failure
    type: int
    sample: 8
"""


import tempfile
import re
import os
import traceback
from math import ceil
from shutil import rmtree
from ansible.module_utils.basic import AnsibleModule
# from ansible.module_utils._text import to_bytes
from ansible.module_utils.common.text.converters import to_bytes
from ansible.module_utils.parsing.convert_bool import boolean
from ansible_collections.ibm.ibm_zos_core.plugins.module_utils import (
    better_arg_parser,
    data_set,
    encode,
    validation,
)
from ansible_collections.ibm.ibm_zos_core.plugins.module_utils.import_handler import (
    ZOAUImportError,
)
from ansible_collections.ibm.ibm_zos_core.plugins.module_utils.dependency_checker import (
    validate_dependencies,
)
from ansible_collections.ibm.ibm_zos_core.plugins.module_utils.log import SingletonLogger

try:
    from zoautil_py import datasets, mvscmd, ztypes, gdgs
except Exception:
    datasets = ZOAUImportError(traceback.format_exc())
    mvscmd = ZOAUImportError(traceback.format_exc())
    ztypes = ZOAUImportError(traceback.format_exc())

try:
    from zoautil_py import exceptions as zoau_exceptions
except Exception:
    zoau_exceptions = ZOAUImportError(traceback.format_exc())


class FetchHandler:
    def __init__(self, module):
        self.module = module

    def _fail_json(self, **kwargs):
        """Wrapper for AnsibleModule.fail_json.

        Parameters
        ----------
        **kwargs : dict
            Arguments to pass to fail_json().
        """
        self.module.fail_json(**kwargs)

    def _run_command(self, cmd, **kwargs):
        """Wrapper for AnsibleModule.run_command.

        Parameters
        ----------
        cmd : str
            Command to run.
        **kwargs : dict
            Arguments to pass to run_command().

        Returns
        -------
        tuple(int,str,str)
            Return code, standard output and standard error.
        """
        return self.module.run_command(cmd, errors='replace', **kwargs)

    def _get_vsam_size(self, vsam):
        """Invoke IDCAMS LISTCAT command to get the record length and space used.
        Then estimate the space used by the VSAM data set.

        Parameters
        ----------
        vsam : str
            VSAM data set name.

        Returns
        -------
        tuple(int,int,int)
            Total size, max_recl and rec_total.

        Raises
        ------
        fail_json
            Unable to obtain data set information.
        """
        space_pri = 0
        total_size = 0
        # Default record length
        max_recl = 80
        # Bytes per cylinder for a 3390 DASD
        bytes_per_cyl = 849960
        rec_total = 80

        listcat_cmd = " LISTCAT ENT('{0}') ALL".format(vsam)
        cmd = "mvscmdauth --pgm=idcams --sysprint=stdout --sysin=stdin"
        rc, out, err = self._run_command(cmd, data=listcat_cmd)

        if not rc:
            find_space_pri = re.findall(r"SPACE-PRI-*\d+", out)
            if find_space_pri:
                space_pri = int("".join(re.findall(r"\d+", find_space_pri[0])))
            total_size = ceil((bytes_per_cyl * space_pri) / 1024)
            find_max_recl = re.findall(r"MAXLRECL-*\d+", out)
            if find_max_recl:
                max_recl = int("".join(re.findall(r"\d+", find_max_recl[0])))
            find_rec_total = re.findall(r"REC-TOTAL-*\d+", out)
            if find_rec_total:
                rec_total = int("".join(re.findall(r"\d+", find_rec_total[0])))
        else:
            self._fail_json(
                msg="Unable to obtain data set information for {0}: {1}".format(
                    vsam, err
                ),
                stdout=out,
                stderr=err,
                stdout_lines=out.splitlines(),
                stderr_lines=err.splitlines(),
                rc=rc,
            )
        return total_size, max_recl, rec_total

    def _copy_vsam_to_temp_data_set(self, ds_name):
        """Copy VSAM data set to a temporary sequential data set.

        Parameters
        ----------
        ds_name : str
            VSAM dataset name to be copied into a temp data set.

        Returns
        -------
        str
            Temporary dataset name.

        Raises
        ------
        fail_json
            OS error.
        fail_json
            cmd error while copying dataset.
        fail_json
            Failed to call IDCAMS.
        """
        mvs_rc = 0
        vsam_size, max_recl, rec_total = self._get_vsam_size(ds_name)
        # Default in case of max recl being 80 to avoid failures when fetching and empty vsam.
        if max_recl == 0:
            max_recl = 80
        # RDW takes the first 4 bytes or records in the VB format, hence we need to add an extra buffer to the vsam max recl.
        max_recl += 4

        sysprint = sysin = out_ds_name = None
        tmphlq = self.module.params.get("tmp_hlq")
        if tmphlq is None:
            tmphlq = "MVSTMP"
        try:
            sysin = data_set.DataSet.create_temp(tmphlq)
            sysprint = data_set.DataSet.create_temp(tmphlq)
            out_ds_name = data_set.DataSet.create_temp(
                tmphlq, space_primary=vsam_size, space_type="K", record_format="VB", record_length=max_recl
            )
            repro_sysin = " REPRO INFILE(INPUT)  OUTFILE(OUTPUT) "
            datasets.write(sysin, repro_sysin)

            dd_statements = []
            dd_statements.append(
                ztypes.DDStatement(
                    name="sysin", definition=ztypes.DatasetDefinition(sysin)
                )
            )
            dd_statements.append(
                ztypes.DDStatement(
                    name="input", definition=ztypes.DatasetDefinition(ds_name)
                )
            )
            dd_statements.append(
                ztypes.DDStatement(
                    name="output", definition=ztypes.DatasetDefinition(out_ds_name)
                )
            )
            dd_statements.append(
                ztypes.DDStatement(
                    name="sysprint", definition=ztypes.FileDefinition(sysprint)
                )
            )

            response = mvscmd.execute_authorized(pgm="idcams", dds=dd_statements)
            mvs_rc, mvs_stdout, mvs_stderr = response.rc, response.stdout_response, response.stderr_response

            # When vsam is empty mvs return code is 12 hence checking for rec total as well, is not failed rather get an empty file.
            if mvs_rc != 0 and rec_total > 0:
                self._fail_json(
                    msg=(
                        "Non-zero return code received while executing mvscmd "
                        "to copy VSAM data set {0}".format(ds_name)
                    ),
                    rc=mvs_rc,
                    stderr=mvs_stderr,
                    stdout=mvs_stdout
                )

        except OSError as err:
            self._fail_json(msg=str(err))

        except Exception as err:
            if datasets.exists(out_ds_name):
                datasets.delete(out_ds_name)

            if mvs_rc != 0:
                self._fail_json(
                    msg=(
                        "Non-zero return code received while executing MVSCmd "
                        "to copy VSAM data set {0}".format(ds_name)
                    ),
                    rc=mvs_rc,
                )
            self._fail_json(
                msg=(
                    "Failed to call IDCAMS to copy VSAM data set {0} to a temporary"
                    " sequential data set".format(ds_name)
                ),
                stderr=str(err),
                rc=mvs_rc,
            )

        finally:
            datasets.delete(sysprint)
            datasets.delete(sysin)

        return out_ds_name

    def _fetch_uss_file(self, src, binary, encoding=None):
        """Convert encoding of a USS file. Return a tuple of temporary file
        name containing converted data.

        Parameters
        ----------
        src : str
            Source of the file.
        binary : bool
            If is binary.
        encoding : str
            The file encoding.

        Returns
        -------
        str
            File name with the converted data.

        Raises
        ------
        fail_json
            Any exception ocurred while converting encoding.
        """
        file_path = None
        if (not binary) and encoding:
            fd, file_path = tempfile.mkstemp()
            from_code_set = encoding.get("from")
            to_code_set = encoding.get("to")
            enc_utils = encode.EncodeUtils()
            try:
                enc_utils.uss_convert_encoding(
                    src, file_path, from_code_set, to_code_set
                )
            except Exception as err:
                os.remove(file_path)
                self._fail_json(
                    msg=(
                        "An error occured while converting encoding of the file "
                        "{0} from {1} to {2}"
                    ).format(src, from_code_set, to_code_set),
                    stderr=str(err),
                    stderr_lines=str(err).splitlines(),
                )
            finally:
                os.close(fd)

        return file_path if file_path else src

    def _fetch_vsam(self, src, binary, encoding=None):
        """Copy the contents of a VSAM to a sequential data set.
        Afterwards, copy that data set to a USS file.

        Parameters
        ----------
        src : str
            Source of the file.
        binary : bool
            If is binary.
        encoding : str
            The file encoding.

        Returns
        -------
        str
            USS File containing the encoded content of the input data set.

        Raises
        ------
        fail_json
            Unable to delete temporary dataset.
        """
        temp_ds = self._copy_vsam_to_temp_data_set(src)
        file_path = self._fetch_mvs_data(temp_ds, binary, encoding=encoding)
        rc = datasets.delete(temp_ds)
        if rc != 0:
            os.remove(file_path)
            self._fail_json(
                msg="Unable to delete temporary data set {0}".format(temp_ds), rc=rc
            )

        return file_path

    def _fetch_pdse(self, src, binary, temp_dir=None, encoding=None):
        """Copy a partitioned data set to a USS directory. If the data set
        is not being fetched in binary mode, encoding for all members inside
        the data set will be converted.

        Parameters
        ----------
        src : str
            Source of the dataset.
        binary : bool
            If it is binary.
        temp_dir : str
            Parent directory for the temp directory of the copy.
        encoding : str
            The file encoding.

        Returns
        -------
        str
            Directory path containing the files of the converted data set members.

        Raises
        ------
        ZOSFetchError
            Error copying partitioned dataset to USS.
        fail_json
            Error converting encoding of the member.
        """
        dir_path = tempfile.mkdtemp(dir=temp_dir)

        copy_args = {
            "options": ""
        }

        if binary:
            copy_args["options"] = "-B"

        try:
            datasets.copy(source=src, target=dir_path, **copy_args)

        except zoau_exceptions.ZOAUException as copy_exception:
            rmtree(dir_path)
            raise ZOSFetchError(
                msg=(
                    "Error copying partitioned data set {0} to USS. Make sure it is"
                    " not empty".format(src)
                ),
                rc=copy_exception.response.rc,
                stdout=copy_exception.response.stdout_response,
                stderr=copy_exception.response.stderr_response,
                stdout_lines=copy_exception.response.stdout_response.splitlines(),
                stderr_lines=copy_exception.response.stderr_response.splitlines(),
            )

        if (not binary) and encoding:
            enc_utils = encode.EncodeUtils()
            from_code_set = encoding.get("from")
            to_code_set = encoding.get("to")
            root, dirs, files = next(os.walk(dir_path))
            try:
                for file in files:
                    file_path = os.path.join(validation.validate_safe_path(root), validation.validate_safe_path(file))
                    enc_utils.uss_convert_encoding(
                        file_path, file_path, from_code_set, to_code_set
                    )
            except Exception as err:
                rmtree(dir_path)
                self._fail_json(
                    msg=(
                        "An error occured while converting encoding of the member "
                        "{0} from {1} to {2}"
                    ).format(file, from_code_set, to_code_set),
                    stderr=str(err),
                    stderr_lines=str(err).splitlines(),
                )
        return dir_path

    def _fetch_gdg(self, src, binary, encoding=None):
        """Copy a generation data group to a USS directory. If the data set
        is not being fetched in binary mode, encoding for all data sets inside
        the GDG will be converted.

        Parameters
        ----------
        src : str
            Source of the generation data group.
        binary : bool
            If it is binary.
        encoding : str
            The file encoding.

        Returns
        -------
        str
            Directory path containing the files of the converted generation data sets.

        Raises
        ------
        fail_json
            Error copying a GDS to USS.
        fail_json
            Error converting encoding of a GDS.
        """
        dir_path = tempfile.mkdtemp()

        data_group = gdgs.GenerationDataGroupView(src)
        for current_gds in data_group.generations:
            if current_gds.organization in data_set.DataSet.MVS_SEQ:
                self._fetch_mvs_data(
                    current_gds.name,
                    binary,
                    temp_dir=dir_path,
                    file_override=current_gds.name,
                    encoding=encoding
                )
            elif current_gds.organization in data_set.DataSet.MVS_PARTITIONED:
                self._fetch_pdse(
                    current_gds.name,
                    binary,
                    temp_dir=dir_path,
                    encoding=encoding
                )

        return dir_path

    def _fetch_mvs_data(self, src, binary, temp_dir=None, file_override=None, encoding=None):
        """Copy a sequential data set or a partitioned data set member
        to a USS file.

        Parameters
        ----------
        src : str
            Source of the dataset.
        binary : bool
            If it is binary.
        temp_dir : str
            Parent directory for the temp directory of the copy.
        file_override : str
            File name that will override the random one made by Python when
            creating a temp file.
        encoding : str
            The file encoding.

        Returns
        -------
        str
            USS File containing the encoded content of the input data set.

        Raises
        ------
        ZOSFetchError
            Unable to copy to USS.
        fail_json
            Error converting encoding of the dataset.
        """
        if file_override:
            file_path = file_override

            if temp_dir:
                file_path = os.path.join(temp_dir, file_path)
        else:
            fd, file_path = tempfile.mkstemp(dir=temp_dir)
            os.close(fd)

        copy_args = {
            "options": ""
        }

        if binary:
            copy_args["options"] = "-B"

        try:
            datasets.copy(source=src, target=file_path, **copy_args)

        except zoau_exceptions.ZOAUException as copy_exception:
            os.remove(file_path)
            raise ZOSFetchError(
                msg="Unable to copy {0} to USS".format(src),
                rc=copy_exception.response.rc,
                stdout=copy_exception.response.stdout_response,
                stderr=copy_exception.response.stderr_response,
                stdout_lines=copy_exception.response.stdout_response.splitlines(),
                stderr_lines=copy_exception.response.stderr_response.splitlines(),
            )

        if (not binary) and encoding:
            enc_utils = encode.EncodeUtils()
            from_code_set = encoding.get("from")
            to_code_set = encoding.get("to")
            try:
                enc_utils.uss_convert_encoding(
                    file_path, file_path, from_code_set, to_code_set
                )
            except Exception as err:
                os.remove(file_path)
                self._fail_json(
                    msg=(
                        "An error occured while converting encoding of the data set"
                        " {0} from {1} to {2}"
                    ).format(src, from_code_set, to_code_set),
                    stderr=str(err),
                    stderr_lines=str(err).splitlines(),
                )
        return file_path


def run_module():
    """Runs the module.

    Raises
    ------
    fail_json
        When parameter verification fails.
    fail_json
        When the source does not exist or is uncataloged.
    fail_json
        When it's unable to determine dataset type.
    fail_json
        While gathering dataset information.
    fail_json
        When the data set member was not found inside a dataset.
    fail_json
        When the file does not have appropriate read permissions.
    """
    # ********************************************************** #
    #                Module initialization                       #
    # ********************************************************** #
    module = AnsibleModule(
        argument_spec=dict(
            src=dict(required=True, type="str"),
            dest=dict(required=True, type="path"),
            fail_on_missing=dict(required=False, default=True, type="bool"),
            flat=dict(required=False, default=False, type="bool"),
            binary=dict(required=False, default=False, type="bool"),
            use_qualifier=dict(required=False, default=False, type="bool"),
            validate_checksum=dict(required=False, default=True, type="bool"),
            encoding=dict(required=False, type="dict"),
            ignore_sftp_stderr=dict(type="bool", default=True, required=False),
            tmp_hlq=dict(required=False, type="str", default=None),
        )
    )
    validate_dependencies(module)

    src = module.params.get("src")
    hlq = None

    if module.params.get("use_qualifier"):
        hlq = datasets.get_hlq()
        module.params["src"] = hlq + "." + src

    # ********************************************************** #
    #                   Verify paramater validity                #
    # ********************************************************** #

    arg_def = dict(
        src=dict(arg_type="data_set_or_path", required=True),
        dest=dict(arg_type="path", required=True),
        fail_on_missing=dict(arg_type="bool", required=False, default=True),
        binary=dict(arg_type="bool", required=False, default=False),
        use_qualifier=dict(arg_type="bool", required=False, default=False),
        tmp_hlq=dict(type='qualifier_or_empty', required=False, default=None),
    )

    if not module.params.get("encoding").get("from") and not module.params.get("binary"):
        mvs_src = data_set.is_data_set(src)
        remote_charset = encode.Defaults.get_default_system_charset()

        module.params["encoding"] = {
            "from": encode.Defaults.DEFAULT_EBCDIC_MVS_CHARSET
            if mvs_src
            else remote_charset,
            "to": module.params.get("encoding").get("to"),
        }

    # We check encoding 'from' and 'to' because if the user pass both arguments of encoding,
    # we honor those but encoding 'to' is an argument that the code obtain any time.
    # Encoding will not be null and will generate problems as encoding 'from' could came empty.
    if module.params.get("encoding").get("from") and module.params.get("encoding").get("to"):
        module.params.update(
            dict(
                from_encoding=module.params.get("encoding").get("from"),
                to_encoding=module.params.get("encoding").get("to"),
            )
        )
        arg_def.update(
            dict(
                from_encoding=dict(arg_type="encoding"),
                to_encoding=dict(arg_type="encoding"),
            )
        )
    fetch_handler = FetchHandler(module)
    try:
        parser = better_arg_parser.BetterArgParser(arg_def)
        parsed_args = parser.parse_args(module.params)
    except ValueError as err:
        module.fail_json(msg="Parameter verification failed", stderr=str(err))

    # Initialize logging module
    module_verbosity_level = module._verbosity
    SingletonLogger().get_logger(module_verbosity_level)

    src = parsed_args.get("src")
    b_src = to_bytes(src)
    fail_on_missing = boolean(parsed_args.get("fail_on_missing"))
    binary = boolean(parsed_args.get("binary"))
    encoding = module.params.get("encoding")
    tmphlq = module.params.get("tmp_hlq")

    # ********************************************************** #
    #  Check for data set existence and determine its type       #
    # ********************************************************** #
    encoding_dict = {"from": encoding.get("from"), "to": encoding.get("to")}
    result = dict(
        src=src,
        dest="",
        binary=binary,
        checksum="",
        changed=False,
        data_set_type="",
        remote_path="",
        msg="",
        stdout="",
        stderr="",
        stdout_lines=[],
        stderr_lines=[],
        rc=0,
        encoding=encoding_dict,
    )
    src_data_set = None
    ds_type = None
    is_member = False
    try:
        # Checking the source actually exists on the system.
        if "/" in src:  # USS
            src_exists = os.path.exists(b_src)
        else:  # MVS
            src_data_set = data_set.MVSDataSet(src)
            is_member = data_set.is_member(src_data_set.name)

            if is_member:
                src_exists = data_set.DataSet.data_set_member_exists(src_data_set.name)
            else:
                src_exists = data_set.DataSet.data_set_exists(
                    src_data_set.name,
                    tmphlq=tmphlq
                )

        if not src_exists:
            if fail_on_missing:
                if is_member:
                    module.fail_json(
                        msg=(
                            "The data set member '{0}' was not found inside data "
                            "set '{1}'"
                        ).format(
                            data_set.extract_member_name(src_data_set.raw_name),
                            data_set.extract_dsname(src_data_set.raw_name)
                        )
                    )
                else:
                    module.fail_json(
                        msg=(
                            "The source '{0}' does not exist or is "
                            "uncataloged.".format(src)
                        )
                    )
            else:
                module.exit_json(
                    msg=("Source '{0}' was not found. No data was fetched.".format(src))
                )

        if "/" in src:
            ds_type = "USS"
        else:
            ds_type = data_set.DataSet.data_set_type(
                data_set.extract_dsname(src_data_set.name),
                tmphlq=tmphlq
            )

        if not ds_type:
            module.fail_json(msg="Unable to determine source type. No data was fetched.")

    except Exception as err:
        module.fail_json(
            msg="Error while gathering source information", stderr=str(err)
        )

    # ********************************************************** #
    #                  Fetch a sequential data set               #
    # ********************************************************** #

    if ds_type in data_set.DataSet.MVS_SEQ:
        file_path = fetch_handler._fetch_mvs_data(
            src_data_set.name,
            binary,
            encoding=encoding
        )
        result["remote_path"] = file_path

    # ********************************************************** #
    #    Fetch a partitioned data set or one of its members      #
    # ********************************************************** #

    elif ds_type in data_set.DataSet.MVS_PARTITIONED:
        if is_member:
            file_path = fetch_handler._fetch_mvs_data(
                src_data_set.name,
                binary,
                encoding=encoding
            )
            result["remote_path"] = file_path
        else:
            result["remote_path"] = fetch_handler._fetch_pdse(
                src_data_set.name,
                binary,
                encoding=encoding
            )

    # ********************************************************** #
    #                  Fetch a USS file                          #
    # ********************************************************** #

    elif ds_type == "USS":
        if not os.access(b_src, os.R_OK):
            module.fail_json(
                msg="File '{0}' does not have appropriate read permission".format(src)
            )
        file_path = fetch_handler._fetch_uss_file(
            src,
            binary,
            encoding=encoding
        )
        result["remote_path"] = file_path

    # ********************************************************** #
    #                  Fetch a VSAM data set                     #
    # ********************************************************** #

    elif ds_type in data_set.DataSet.MVS_VSAM:
        file_path = fetch_handler._fetch_vsam(
            src_data_set.name,
            binary,
            encoding=encoding
        )
        result["remote_path"] = file_path

    # ********************************************************** #
    #                  Fetch a GDG                               #
    # ********************************************************** #

    elif ds_type == "GDG":
        result["remote_path"] = fetch_handler._fetch_gdg(
            src_data_set.name,
            binary,
            encoding=encoding
        )

    if ds_type == "USS":
        result["src"] = src
    else:
        result["src"] = src_data_set.name

        # Removing the HLQ since the user is probably not expecting it. The module
        # hasn't returned it ever since it was originally written. Changes made to
        # add GDG/GDS support started leaving the HLQ behind in the file name.
        if hlq:
            result["src"] = result["src"].replace(f"{hlq}.", "")

    result["ds_type"] = ds_type
    module.exit_json(**result)


class ZOSFetchError(Exception):
    def __init__(self, msg, rc="", stdout="", stderr="", stdout_lines="", stderr_lines=""):
        """Error in a copy operation.

        Parameters
        ----------
        msg : str
            Human readable string describing the exception.
        rc : int
            Result code.
        stdout : str
            Standart output.
        stderr : str
            Standart error.
        stdout_lines : str
            Standart output divided in lines.
        stderr_lines : str
            Standart error divided in lines.
        """
        self.json_args = dict(
            msg=msg,
            rc=rc,
            stdout=stdout,
            stderr=stderr,
            stdout_lines=stdout_lines,
            stderr_lines=stderr_lines,
        )
        super().__init__(msg)


def main():
    run_module()


if __name__ == "__main__":
    main()
