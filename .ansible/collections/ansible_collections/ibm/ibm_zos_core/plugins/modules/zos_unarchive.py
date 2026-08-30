#!/usr/bin/python
# -*- coding: utf-8 -*-

# Copyright (c) IBM Corporation 2023, 2026
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#     http://www.apache.org/licenses/LICENSE-2.0
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.

from __future__ import (absolute_import, division, print_function)
__metaclass__ = type

DOCUMENTATION = r'''
---
module: zos_unarchive
version_added: "1.7.0"
author:
  - Oscar Fernando Flores Garcia (@fernandofloresg)
short_description: Unarchive files and data sets in z/OS.
description:
  - The C(zos_unarchive) module unpacks an archive after optionally
    transferring it to the remote system.
  - For supported archive formats, see option C(format).
  - Supported sources are USS (UNIX System Services) or z/OS data sets.
  - Mixing MVS data sets with USS files for unarchiving is not supported.
  - The archive is sent to the remote as binary, so no encoding is performed.
options:
  src:
    description:
      - The remote absolute path or data set of the archive to be uncompressed.
      - I(src) can be a USS file or MVS data set name.
      - USS file paths should be absolute paths.
      - MVS data sets supported types are C(SEQ), C(PDS), C(PDSE).
      - GDS relative names are supported. e.g. I(USER.GDG(-1)).
    type: str
    required: true
  format:
    description:
      - The compression type and corresponding options to use when archiving
        data.
    type: dict
    required: true
    suboptions:
      type:
        description:
          - The compression format used while archiving.
        type: str
        required: true
        choices:
          - bz2
          - gz
          - tar
          - zip
          - terse
          - xmit
          - pax
      options:
        description:
          - Options specific to a compression format.
        type: dict
        required: false
        suboptions:
          xmit_log_data_set:
            description:
              - Provide the name of a data set to store xmit log output.
              - If the data set provided does not exist, the program
                will create it.
              - 'If the data set provided exists, the data set must have
                the following attributes: LRECL=255, BLKSIZE=3120, and
                RECFM=VB'
              - When providing the I(xmit_log_data_set) name, ensure there
                is adequate space.
            type: str
          adrdssu:
            description:
              - If set to true, the C(zos_unarchive) module will use Data
                Facility Storage Management Subsystem data set services
                (DFSMSdss) program ADRDSSU to uncompress data sets from
                a portable format after using C(xmit) or C(terse).
            type: bool
            default: False
          dest_volumes:
            description:
              - When I(adrdssu=True), specify the volume the data sets
                will be written to.
              - If no volume is specified, storage management rules will be
                used to determine the volume where the file will be
                unarchived.
              - If the storage administrator has specified a system default
                unit name and you do not set a volume name for
                non-system-managed data sets, then the system uses the
                volumes associated with the default unit name. Check with
                your storage administrator to determine whether a default
                unit name has been specified.
            type: list
            elements: str
  dest:
    description:
      - The remote absolute path or data set where the content should be unarchived to.
      - I(dest) can be a USS file, directory or MVS data set name.
      - If dest has missing parent directories, they will not be created.
    type: str
    required: false
  group:
    description:
      - Name of the group that will own the file system objects.
      - When left unspecified, it uses the current group of the current user
        unless you are root, in which case it can preserve the previous
        ownership.
      - This option is only applicable if C(dest) is USS, otherwise ignored.
    type: str
    required: false
  mode:
    description:
      - The permission of the uncompressed files.
      - If C(dest) is USS, this will act as Unix file mode, otherwise ignored.
      - It should be noted that modes are octal numbers.
        The user must either add a leading zero so that Ansible's YAML parser
        knows it is an octal number (like C(0644) or C(01777))or quote it
        (like C('644') or C('1777')) so Ansible receives a string and can do
        its own conversion from string into number. Giving Ansible a number
        without following one of these rules will end up with a decimal number
        which will have unexpected results.
      - The mode may also be specified as a symbolic mode
        (for example, ``u+rwx`` or ``u=rw,g=r,o=r``) or a special
        string `preserve`.
      - I(mode=preserve) means that the file will be given the same permissions
        as
        the source file.
    type: str
    required: false
  owner:
    description:
      - Name of the user that should own the filesystem object, as would be
        passed to the chown command.
      - When left unspecified, it uses the current user unless you are root,
        in which case it can preserve the previous ownership.
    type: str
    required: false
  include:
    description:
      - A list of directories, files or data set names to extract from the
        archive.
      - GDS relative names are supported. e.g. I(USER.GDG(-1)).
      - When C(include) is set, only those files will we be extracted leaving
        the remaining files in the archive.
      - Mutually exclusive with exclude.
    type: list
    elements: str
    required: false
  exclude:
    description:
      - List the directory and file or data set names that you would like to
        exclude from the unarchive action.
      - GDS relative names are supported. e.g. I(USER.GDG(-1)).
      - Mutually exclusive with include.
    type: list
    elements: str
    required: false
  list:
    description:
      - Will list the contents of the archive without unpacking.
    type: bool
    required: false
    default: false
  dest_data_set:
    description:
      - Data set attributes to customize a C(dest) data set that the archive will be copied into.
    required: false
    type: dict
    suboptions:
      name:
        description:
          - Desired name for destination dataset.
        type: str
        required: false
      type:
        description:
          - Organization of the destination
        type: str
        required: false
        default: seq
        choices:
          - seq
          - pds
          - pdse
      space_primary:
        description:
          - If the destination I(dest) data set does not exist , this sets the
            primary space allocated for the data set.
          - The unit of space used is set using I(space_type).
        type: int
        required: false
      space_secondary:
        description:
          - If the destination I(dest) data set does not exist , this sets the
            secondary space allocated for the data set.
          - The unit of space used is set using I(space_type).
        type: int
        required: false
      space_type:
        description:
          - If the destination data set does not exist, this sets the unit of
            measurement to use when defining primary and secondary space.
          - Valid units of size are C(k), C(m), C(g), C(cyl), and C(trk).
        type: str
        choices:
          - k
          - m
          - g
          - cyl
          - trk
        required: false
      record_format:
        description:
          - If the destination data set does not exist, this sets the format of
            the
            data set. (e.g C(fb))
          - Choices are case-sensitive.
        required: false
        choices:
          - fb
          - vb
          - fba
          - vba
          - u
        type: str
      record_length:
        description:
          - The length of each record in the data set, in bytes.
          - For variable data sets, the length must include the 4-byte prefix
            area.
          - "Defaults vary depending on format: If FB/FBA 80, if VB/VBA 137,
            if U 0."
        type: int
        required: false
      block_size:
        description:
          - The block size to use for the data set.
        type: int
        required: false
      directory_blocks:
        description:
          - The number of directory blocks to allocate to the data set.
        type: int
        required: false
      key_offset:
        description:
          - The key offset to use when creating a KSDS data set.
          - I(key_offset) is required when I(type=ksds).
          - I(key_offset) should only be provided when I(type=ksds)
        type: int
        required: false
      key_length:
        description:
          - The key length to use when creating a KSDS data set.
          - I(key_length) is required when I(type=ksds).
          - I(key_length) should only be provided when I(type=ksds)
        type: int
        required: false
      sms_storage_class:
        description:
          - The storage class for an SMS-managed dataset.
          - Required for SMS-managed datasets that do not match an SMS-rule.
          - Not valid for datasets that are not SMS-managed.
          - Note that all non-linear VSAM datasets are SMS-managed.
        type: str
        required: false
      sms_data_class:
        description:
          - The data class for an SMS-managed dataset.
          - Optional for SMS-managed datasets that do not match an SMS-rule.
          - Not valid for datasets that are not SMS-managed.
          - Note that all non-linear VSAM datasets are SMS-managed.
        type: str
        required: false
      sms_management_class:
        description:
          - The management class for an SMS-managed dataset.
          - Optional for SMS-managed datasets that do not match an SMS-rule.
          - Not valid for datasets that are not SMS-managed.
          - Note that all non-linear VSAM datasets are SMS-managed.
        type: str
        required: false
  tmp_hlq:
    description:
      - Override the default high level qualifier (HLQ) for temporary data
        sets.
      - The default HLQ is the Ansible user used to execute the module and if
        that is not available, then the environment variable value C(TMPHLQ) is
        used.
    type: str
    required: false
  force:
    description:
      - If set to true and the remote file or data set dest exists, the dest
        will be deleted.
    type: bool
    required: false
    default: false
  remote_src:
    description:
      - If set to true, C(zos_unarchive) retrieves the archive from the remote
        system.
      - If set to false, C(zos_unarchive) searches the local machine (Ansible
        controller) for the archive.
    type: bool
    required: false
    default: false
  encoding:
    description:
      - Specifies the character encoding conversion to be applied to the
        destination files after unarchiving.
      - Supported character sets rely on the charset conversion utility
        C(iconv) version the most common character sets are supported.
      - After conversion the files are stored in same location as they
        were unarchived to under the same original name. No backup of the
        original unconverted files is there as for that unarchive can be
        executed again without encoding params on same source archive files.
      - Destination files will be converted to the new encoding and will not
        be restored to their original encoding.
      - If encoding fails for any file in a set of multiple files, an
        exception will be raised and the name of the file skipped will be
        provided completing the task successfully with rc code 0.
      - Encoding does not check if the file is already present or not.
        It works on the file/files successfully unarchived.
    type: dict
    required: false
    suboptions:
      from:
        description:
          - The character set of the source I(src).
        required: false
        type: str
      to:
        description:
          - The destination I(dest) character set for the files to be written as.
        required: false
        type: str
      skip_encoding:
        description:
          - List of names to skip encoding after unarchiving. This is only used if I(encoding) is set, otherwise is ignored.
        required: false
        type: list
        elements: str
attributes:
  action:
    support: full
    description: Indicates this has a corresponding action plugin so some parts of the options can be executed on the controller.
  async:
    support: full
    description: Supports being used with the ``async`` keyword.
  check_mode:
    support: full
    description: Can run in check_mode and return changed status prediction without modifying target. If not supported, the action will be skipped.

notes:
  - VSAMs are not supported.
  - This module uses L(zos_copy,./zos_copy.html) to copy local scripts to
    the remote machine which uses SFTP (Secure File Transfer Protocol) for the
    underlying transfer protocol; SCP (secure copy protocol) and Co:Z SFTP are not
    supported. In the case of Co:z SFTP, you can exempt the Ansible user id on z/OS
    from using Co:Z thus falling back to using standard SFTP. If the module detects
    SCP, it will temporarily use SFTP for transfers, if not available, the module
    will fail.
seealso:
  - module: ibm.ibm_zos_core.zos_archive
'''

EXAMPLES = r'''
# Simple extract
- name: Copy local tar file and unpack it on the managed z/OS node.
  zos_unarchive:
    src: "./files/archive_folder_test.tar"
    format:
      type: tar

# use include
- name: Unarchive a bzip file selecting only a file to unpack.
  zos_unarchive:
    src: "/tmp/test.bz2"
    format:
      type: bz2
    include:
      - 'foo.txt'

# Use exclude
- name: Unarchive a terse data set and excluding data sets from unpacking.
  zos_unarchive:
    src: "USER.ARCHIVE.RESULT.TRS"
    format:
      type: terse
    exclude:
      - USER.ARCHIVE.TEST1
      - USER.ARCHIVE.TEST2

# Unarchive a GDS
- name: Unarchive a terse data set and excluding data sets from unpacking.
  zos_unarchive:
    src: "USER.ARCHIVE(0)"
    format:
      type: terse

# List option
- name: List content from XMIT
  zos_unarchive:
    src: "USER.ARCHIVE.RESULT.XMIT"
    format:
      type: xmit
      options:
        adrdssu: true
    list: true

# Encoding example
- name: Encode the destination data set into Latin-1 after unarchiving.
  zos_unarchive:
    src: "USER.ARCHIVE.RESULT.TRS"
    format:
      type: terse
    encoding:
      from: IBM-1047
      to: ISO8859-1

- name: Encode the destination data set into Latin-1 after unarchiving.
  zos_unarchive:
    src: "USER.ARCHIVE.RESULT.TRS"
    format:
      type: terse
    encoding:
      from: IBM-1047
      to: ISO8859-1
      skip_encoding:
        - USER.ARCHIVE.TEST1
'''

RETURN = r'''
src:
  description:
    File path or data set name unpacked.
  type: str
  returned: always
dest_path:
  description:
    - Destination path where archive was unpacked.
  type: str
  returned: always
targets:
  description:
    List of files or data sets in the archive.
  type: list
  elements: str
  returned: success
missing:
  description:
    Any files or data sets not found during extraction.
  type: str
  returned: success
encoded:
    description:
      List of files or data sets that were successfully encoded.
    type: list
    returned: success
failed_on_encoding:
    description:
      List of files or data sets that were failed while encoding.
    type: list
    returned: success
skipped_encoding_targets:
    description:
      List of files or data sets that were skipped while encoding.
    type: list
    returned: success
'''

import abc
from ansible.module_utils.basic import AnsibleModule
from ansible_collections.ibm.ibm_zos_core.plugins.module_utils import (
    better_arg_parser,
    data_set,
    validation,
    mvs_cmd,
    encode)
import re
import os
import zipfile
import tarfile
import traceback
from ansible_collections.ibm.ibm_zos_core.plugins.module_utils.import_handler import (
    ZOAUImportError,
)
from ansible_collections.ibm.ibm_zos_core.plugins.module_utils.dependency_checker import (
    validate_dependencies,
)
from ansible_collections.ibm.ibm_zos_core.plugins.module_utils.log import SingletonLogger

try:
    from zoautil_py import datasets
except Exception:
    datasets = ZOAUImportError(traceback.format_exc())

data_set_regex = r"(?:(?:[A-Z$#@]{1}[A-Z0-9$#@-]{0,7})(?:[.]{1})){1,21}[A-Z$#@]{1}[A-Z0-9$#@-]{0,7}(?:\([A-Z$#@]{1}[A-Z0-9$#@]{0,7}\)){0,1}"

XMIT_RECORD_LENGTH = 80
AMATERSE_RECORD_LENGTH = 1024


class Unarchive():
    def __init__(self, module):
        """Handles Unarchive operations.
        Parameters
        ----------
        module : AnsibleModule
            AnsibleModule to use.
        Attributes
        ----------
        module : AnsibleModule
            AnsibleModule to use.
        src : str
            Source path of the unarchive.
        dest : str
            Destination of the unarchive.
        format : str
            Name of the format of the module.
        options : list[str]
            Options of the format of the module.
        tmphql : str
            High level qualifier for temporary datasets.
        force : str
            Whether force an operation or not.
        targets : list
            List of members in the source archive.
        include : list[str]
            List of paths to include in extraction.
        exclude : list[str]
            List of paths to exclude in extraction.
        list : bool
            Whether to execute list_archive_content() or not.
        changed : bool
            If there are targeted paths.
        missing : list
            Paths that were on include, but are missing from the files in archive.
        remote_src : bool
            If the source is remote or not.
        from_encoding: str
            The encoding of the source file.
        to_encoding: str
            The required encoding of the destination file.
        skip_encoding : list[str]
            List of paths to exclude in encoding.
        """
        self.module = module
        self.src = module.params.get("src")
        self.dest = module.params.get("dest")
        self.format = module.params.get("format").get("type")
        self.options = module.params.get("format").get("options")
        self.tmphlq = module.params.get("tmp_hlq")
        self.force = module.params.get("force")
        self.targets = list()
        self.include = module.params.get("include")
        self.exclude = module.params.get("exclude")
        self.list = module.params.get("list")
        self.changed = False
        self.missing = list()
        self.remote_src = module.params.get("remote_src")
        encoding_param = module.params.get("encoding") or {}
        self.from_encoding = encoding_param.get("from")
        self.to_encoding = encoding_param.get("to")
        if self.dest == '':
            self.dest = os.path.dirname(self.src)
        self.encoded = list()
        self.failed_on_encoding = list()
        self.skip_encoding = encoding_param.get("skip_encoding")
        self.skipped_encoding_targets = list()

    @abc.abstractmethod
    def extract_src(self):
        pass

    @abc.abstractmethod
    def _list_content(self):
        pass

    def src_exists(self):
        """If USS source exists.
        Returns
        -------
        bool
            If USS source exists.
        """
        return self.src and os.path.exists(self.src)

    def dest_type(self):
        """The destination type.
        Returns
        -------
        str
            USS destination type.
        """
        return "USS"

    def dest_unarchived(self):
        """If destination was unarchived.
        Returns
        -------
        bool
            Boolean expression of the targets attribute.
        """
        return bool(self.targets)

    def update_permissions(self):
        """Update permissions in unarchived files.
        """
        for target in self.targets:
            file_name = os.path.join(validation.validate_safe_path(self.dest), validation.validate_safe_path(target))
            file_args = self.module.load_file_common_arguments(self.module.params, path=file_name)
            self.module.set_fs_attributes_if_different(file_args, self.changed)

    def encoding_targets(self):
        """Finds encoding target files in host.
        """
        if self.skip_encoding:
            self.encode_targets = [
                path for path in self.targets if path not in self.skip_encoding
            ]
            self.skipped_encoding_targets = self.skip_encoding
        else:
            self.encode_targets = self.targets

    def encode_destination(self):
        """Convert encoding for given destination
        Returns
        -------
        Union
            encoded or failed_on_encoding list
        """
        enc_utils = encode.EncodeUtils()
        self.encoded = []
        self.failed_on_encoding = []

        for target in self.encode_targets:
            try:
                file_path = os.path.normpath(os.path.join(self.dest, target))
                convert_rc = enc_utils.uss_convert_encoding_prev(
                    file_path, file_path, self.from_encoding, self.to_encoding
                )
                if convert_rc:
                    enc_utils.uss_tag_encoding(file_path, self.to_encoding)
                self.encoded.append(os.path.abspath(target))

            except Exception:
                self.failed_on_encoding.append(os.path.abspath(target))

        return {
            "encoded": self.encoded,
            "failed_on_encoding": self.failed_on_encoding,
            "skipped_encoding_targets": self.skipped_encoding_targets
        }

    @property
    def result(self):
        """Result.
        Returns
        -------
        dict
            Arguments showing the result.
        """
        return {
            'src': self.src,
            'dest_path': self.dest,
            'changed': self.changed,
            'targets': self.targets,
            'missing': self.missing,
            'encoded': getattr(self, 'encoded', []),
            'failed_on_encoding': getattr(self, 'failed_on_encoding', []),
            'skipped_encoding_targets': getattr(self, 'skipped_encoding_targets', []),
        }

    def extract_all(self, members):
        """Extract all members given.
        Parameters
        ----------
        members : list
            List of members to extract.
        """
        for member in members:
            self.file.extract(member)


class TarUnarchive(Unarchive):
    def __init__(self, module):
        """Unarchive for tar archives.
        Parameters
        ----------
        module : AnsibleModule
            AnsibleModule to use.
        """
        super(TarUnarchive, self).__init__(module)

    def open(self, path):
        """Open an archive using tarfile lib for read.

        Parameters
        ----------
        path : str
            Path to a tar, pax, gz or bz2 file to be opened.

        Returns
        -------
        TarFile
        """
        if self.format == 'tar':
            file = tarfile.open(path, 'r')
        elif self.format in ('pax'):
            file = tarfile.open(path, 'r', format=tarfile.GNU_FORMAT)
        elif self.format in ('gz', 'bz2'):
            file = tarfile.open(path, 'r:' + self.format)
        else:
            self.module.fail_json(msg="%s is not a valid archive format for listing contents" % self.format)
        return file

    def list_archive_content(self, path):
        """Sets a list of members in the source archive to the targets attribute.
        Parameters
        ----------
        path : str
            Path to a tar, pax, gz or bz2 file to list its contents.
        """
        self.targets = self._list_content(self.src)

    def _list_content(self, path):
        """Returns a list of members in an archive.

        Parameters
        ----------
        path : str
            Path to a tar, pax, gz or bz2 file to list its contents.

        Returns
        -------
        Union[str]
            List of members inside the archive.
        """
        self.file = self.open(path)
        members = self.file.getnames()
        self.file.close()
        return members

    def extract_src(self):
        """Unpacks the contents of the archive stored in path into dest folder.
        """
        original_working_dir = os.getcwd()
        # The function gets relative paths, so it changes the current working
        # directory to the root of src.
        os.chdir(self.dest)
        self.file = self.open(self.src)

        files_in_archive = self.file.getnames()
        if self.include:
            for path in self.include:
                if path not in files_in_archive:
                    self.missing.append(path)
                else:
                    self.file.extract(path)
                    self.targets.append(path)
        elif self.exclude:
            for path in files_in_archive:
                if path not in self.exclude:
                    self.file.extract(path)
                    self.targets.append(path)
        else:
            self.extract_all(members=sanitize_members(self.file.getmembers(), self.dest, self.format))
            self.targets = files_in_archive
        self.file.close()
        # Returning the current working directory to what it was before to not
        # interfere with the rest of the module.
        os.chdir(original_working_dir)
        self.changed = bool(self.targets)


class ZipUnarchive(Unarchive):
    def __init__(self, module):
        """Unarchive for zip archives.
        Parameters
        ----------
        module : AnsibleModule
            AnsibleModule to use.
        """
        super(ZipUnarchive, self).__init__(module)

    def open(self, path):
        """Unpacks the contents of the archive stored in path into dest folder.

        Parameters
        ----------
        path : str
            Path to the zip archive.
        Returns
        -------
        ZipFile
            Zip file on the specified path.
        Raises
        ------
        BadZipFile
            Improperly compressed zip file, unable to to open.
        """
        try:
            file = zipfile.ZipFile(path, 'r', zipfile.ZIP_DEFLATED, True)
        except zipfile.BadZipFile:
            self.module.fail_json(
                msg="Improperly compressed zip file, unable to to open file {0} ".format(path)
            )
        return file

    def list_archive_content(self):
        """Sets a list of members in the source archive to the targets attribute.
        """
        self.targets = self._list_content(self.src)

    def _list_content(self, path):
        """Returns a list of members in an archive.

        Parameters
        ----------
        path : str
            Path to a tar, pax, gz or bz2 file to list its contents.

        Returns
        -------
        Union[str]
            List of members inside the archive.
        """
        self.file = self.open(path)
        members = self.file.namelist()
        self.file.close()
        return members

    def extract_src(self):
        """Returns a list of members in an archive.

        Parameters
        ----------
        path : str
            Path to a tar, pax, gz or bz2 file to list its contents.

        Returns
        -------
        Union[str]
            List of members inside the archive.
        """
        original_working_dir = os.getcwd()
        # The function gets relative paths, so it changes the current working
        # directory to the root of src.
        os.chdir(self.dest)
        self.file = self.open(self.src)

        files_in_archive = self.file.namelist()
        if self.include:
            for path in self.include:
                if path not in files_in_archive:
                    self.missing.append(path)
                else:
                    self.file.extract(path)
                    self.targets.append(path)
        elif self.exclude:
            for path in files_in_archive:
                if path not in self.exclude:
                    self.file.extract(path)
                    self.targets.append(path)
        else:
            self.extract_all(members=sanitize_members(self.file.infolist(), self.dest, self.format))
            self.targets = files_in_archive
        self.file.close()
        # Returning the current working directory to what it was before to not
        # interfere with the rest of the module.
        os.chdir(original_working_dir)
        self.changed = bool(self.targets)


class MVSUnarchive(Unarchive):
    def __init__(self, module):
        """Unarchive for MVS archives.
        Parameters
        ----------
        module : AnsibleModule
            AnsibleModule to use.
        Attributes
        ----------
        volumes : list[str]
            List of destination volumes.
        adrdssu : bool
            Whether to use Data Facility Storage Management Subsystem data set services
            program ADRDSSU to uncompress data sets or not.
        dest_dat_set : dict
            Destination data set.
        source_size : int
            Source size.
        """
        super(MVSUnarchive, self).__init__(module)
        self.volumes = self.options.get("dest_volumes")
        self.adrdssu = self.options.get("adrdssu")
        self.dest_data_set = module.params.get("dest_data_set")
        self.dest_data_set = dict() if self.dest_data_set is None else self.dest_data_set
        self.source_size = 0
        if data_set.DataSet.is_gds_relative_name(self.src):
            self.src = data_set.DataSet.resolve_gds_absolute_name(self.src)

    def dest_type(self):
        """Returns the destination type.
        Returns
        -------
        str
            'MVS'
        """
        return "MVS"

    def _compute_dest_data_set_size(self):
        """Computes the attributes that the destination data set or temporary destination
        data set should have in terms of size, record_length, etc.

        Returns
        -------
        int
            Source size.
        """

        """
        - Size of temporary DS for archive handling.

        If remote_src then we can get the source_size from archive on the system.

        If not remote_src then we can get the source_size from temporary_ds.
        Both are named src so no problemo.

        If format is xmit, dest_data_set size is the same as source_size.

        If format is terse, dest_data_set size is different than the source_size, has to be greater,
        but how much? In this case we can add dest_data_set option.

        Apparently the only problem is when format name is terse.
        """

        # Get the size from the system
        src_attributes = datasets.list_datasets(self.src)[0]
        # The size returned by list_datasets is in bytes.
        source_size = int(src_attributes.total_space)
        if self.format == 'terse':
            source_size = int(source_size * 1.5)
        return source_size

    def _create_dest_data_set(
            self,
            name=None,
            replace=None,
            type=None,
            space_primary=None,
            space_secondary=None,
            space_type=None,
            record_format=None,
            record_length=None,
            block_size=None,
            directory_blocks=None,
            key_length=None,
            key_offset=None,
            sms_storage_class=None,
            sms_data_class=None,
            sms_management_class=None,
            volumes=None,
            tmp_hlq=None,
            force=None,
    ):
        """Create a temporary data set.

        Parameters
        ----------
        name : str
            Name for the temporary data set.
        replace : bool
            Used to determine behavior when data set already exists.
        type : str
            Type of the dataset.
        space_primary : int
            Size of the source.
        space_secondary : int
            The amount of secondary space to allocate for the dataset.
        space_type : str
            The unit of measurement to use when defining primary and secondary space.
        record_format : str
            The record format to use for the dataset.
        record_length : int
            The length, in bytes, of each record in the data set.
        block_size : int
            The block size to use for the data set.
        directory_blocks : int
            The number of directory blocks to allocate to the data set.
        key_length : int
            The key length of a record.
        key_offset : int
            The key offset is the position of the first byte of the key
            in each logical record of a the specified VSAM data set.
        sms_storage_class : str
            The storage class for an SMS-managed dataset.
        sms_data_class : str
            The data class for an SMS-managed dataset.
        sms_management_class : str
            The management class for an SMS-managed dataset.
        volumes : list[str,list[str]]
            A list of volume serials.
        tmp_hlq : str
            A HLQ specified by the user for temporary data sets.
        force : bool
            Used to determine behavior when performing member operations on a pdse.

        Returns
        -------
        tuple(str,bool)
            Name of the temporary data set created and if something changed.
        """
        arguments = locals()
        if name is None:
            if tmp_hlq:
                hlq = tmp_hlq
            else:
                hlq = datasets.get_hlq()
            temp_ds = datasets.tmp_name(high_level_qualifier=hlq)
            arguments.update(name=temp_ds)
        if record_format is None:
            arguments.update(record_format="fb")
        if record_length is None:
            arguments.update(record_length=80)
        if type is None:
            arguments.update(type="seq")
        if space_primary is None:
            arguments.update(space_primary=self._compute_dest_data_set_size())
        arguments.pop("self")
        changed, zoau_data_set = data_set.DataSet.ensure_present(**arguments)
        return arguments["name"], changed

    def _get_include_data_sets_cmd(self):
        """Get the command with the datasets from the paths in include.
        Returns
        -------
        str
            Command to include the datasets from the paths in include.
        """
        include_cmd = "INCL( "
        for include_ds in self.include:
            include_cmd += " '{0}', - \n".format(include_ds.upper())
        include_cmd += " ) - \n"
        return include_cmd

    def _get_exclude_data_sets_cmd(self):
        """Get the command with the datasets from the paths in exclude.
        Returns
        -------
        str
            Command to exclude the datasets from the paths in exclude.
        """
        exclude_cmd = "EXCL( - \n"
        for exclude_ds in self.exclude:
            exclude_cmd += " '{0}', - \n".format(exclude_ds.upper())
        exclude_cmd += " ) - \n"
        return exclude_cmd

    def _get_volumes(self):
        """Get the command with the volumes.
        Returns
        -------
        str
            Command with the volumes.
        """
        volumes_cmd = "OUTDYNAM( - \n"
        for volume in self.volumes:
            volumes_cmd += " ('{0}'), - \n".format(volume)
        volumes_cmd += " ) - \n"
        return volumes_cmd

    def _restore(self, source):
        """
        Calls ADDRSU using RESTORE to unpack the dump datasets.

        Parameters
        ----------
        source : str
            Name of the data set to use as archive in ADRDSSU restore operation.

        Returns
        -------
        int
            Return code result of restore operation.
        """
        filter = "INCL(**) "
        volumes = ""
        force = "REPLACE -\n TOLERATE(ENQFAILURE) " if self.force else ""
        if self.include:
            filter = self._get_include_data_sets_cmd()
        if self.exclude:
            filter = self._get_exclude_data_sets_cmd()
        if self.volumes:
            volumes = self._get_volumes()
        restore_cmd = """ RESTORE INDD(ARCHIVE) -
                          DS( -
                            {0} ) -
                            {1} -
                        CATALOG -
                        {2} """.format(filter, volumes, force)
        dds = dict(archive="{0},old".format(source))
        rc, out, err = mvs_cmd.adrdssu(cmd=restore_cmd, dds=dds, authorized=True)
        self._get_restored_datasets(out)

        if rc != 0:
            # AdrddssuRestoreError
            unrestored_data_sets = self._get_unrestored_datasets(out)
            unrestored_data_sets = ", ".join(unrestored_data_sets)
            self.clean_environment(data_sets=[source], uss_files=[], remove_targets=True)
            self.module.fail_json(
                msg="Failed executing ADRDSSU to unarchive {0}. List of data sets not restored : {1}".format(source, unrestored_data_sets),
                stdout=f"command: {restore_cmd} \n stdout:{out}",
                stderr=err,
                stdout_lines=f"command: {restore_cmd} \n stdout:{out}".splitlines(),
                stderr_lines=err.splitlines(),
                rc=rc,
            )
        return rc

    def src_exists(self):
        """Checks if the source exists or not
        Returns
        -------
        bool
            If the source exists.
        """
        return data_set.DataSet.data_set_exists(self.src, tmphlq=self.tmphlq)

    def _get_restored_datasets(self, output):
        """Gets the datasets that were successfully restored.
        Parameters
        ----------
        output : str
            Output to search the successful datasets from.
        Returns
        -------
        Union
          The list with all the restored datasets.
        """
        ds_list = list()
        find_ds_list = re.findall(r"SUCCESSFULLY PROCESSED\n(?:.*\n)*", output)
        if find_ds_list:
            ds_list = re.findall(data_set_regex, find_ds_list[0])
        self.targets = ds_list
        return ds_list

    def _get_unrestored_datasets(self, output):
        """Gets the datasets that were not successfully restored.
        Parameters
        ----------
        output : str
            Output to search the not successful datasets from.
        Returns
        -------
        Union
          The list with all the not restored datasets.
        """
        ds_list = list()
        output = output.split("SUCCESSFULLY PROCESSED")[0]
        find_ds_list = re.findall(r"NOT PROCESSED FROM THE LOGICALLY FORMATTED DUMP TAPE DUE TO \n(?:.*\n)*", output)
        if find_ds_list:
            ds_list = re.findall(data_set_regex, find_ds_list[0])
        return ds_list

    @abc.abstractmethod
    def unpack(self):
        pass

    def extract_src(self):
        """Extract the MVS path contents.

        """
        temp_ds = ""
        if not self.adrdssu:
            temp_ds, rc = self._create_dest_data_set(**self.dest_data_set)
            rc = self.unpack(self.src, temp_ds)
            self.targets = [temp_ds]

        else:
            temp_ds, rc = self._create_dest_data_set(type="seq",
                                                     record_format="u",
                                                     record_length=0,
                                                     tmp_hlq=self.tmphlq,
                                                     replace=True)
            self.unpack(self.src, temp_ds)
            rc = self._restore(temp_ds)
            datasets.delete(temp_ds)
        self.changed = not rc

        if not self.remote_src:
            datasets.delete(self.src)
        return

    def _list_content(self, source):
        """Runs a command to get datasets and gives the output to _get_restored_datasets().
        Parameters
        ----------
        source : str
            The source of the archive.
        """
        restore_cmd = " RESTORE INDD(ARCHIVE) DS(INCL(**)) "
        cmd = " mvscmdauth --pgm=ADRDSSU --archive={0},old --args='TYPRUN=NORUN' --sysin=stdin --sysprint=*".format(source)
        rc, out, err = self.module.run_command(cmd, data=restore_cmd, errors='replace')
        self._get_restored_datasets(out)

    def list_archive_content(self):
        """Creates a temporary dataset to use in _list_content().
        """
        temp_ds, rc = self._create_dest_data_set(type="seq", record_format="u", record_length=0, tmp_hlq=self.tmphlq, replace=True)
        self.unpack(self.src, temp_ds)
        self._list_content(temp_ds)
        datasets.delete(temp_ds)
        if not self.remote_src:
            datasets.delete(self.src)

    def clean_environment(self, data_sets=None, uss_files=None, remove_targets=False):
        """Removes any allocated data sets that won't be needed after module termination.
        Parameters
        ----------
        data_sets : list[str]
            List of data sets to remove.
        uss_files : list[str]
            List of uss files to remove.
        remove_targets : bool
            Indicates if already unpacked data sets need to be removed too.
        """
        if data_set is not None:
            for ds in data_sets:
                data_set.DataSet.ensure_absent(ds)
        if uss_files is not None:
            for file in uss_files:
                os.remove(file)
        if remove_targets:
            for target in self.targets:
                data_set.DataSet.ensure_absent(target)

    def encoding_targets(self):
        """Finds encoding target datasets in host.
        """
        if self.skip_encoding:
            self.encode_targets = [
                path for path in self.targets if path not in self.skip_encoding
            ]
            self.skipped_encoding_targets = self.skip_encoding
        else:
            self.encode_targets = self.targets

    def encode_destination(self):
        """Convert encoding for given destination
        Returns
        -------
        Union
            encoded or failed_on_encoding list
        """
        enc_utils = encode.EncodeUtils()
        self.encoded = []
        self.failed_on_encoding = []

        for target in self.encode_targets:
            try:
                ds_utils = data_set.DataSetUtils(target, tmphlq=self.tmphlq)
                ds_type = ds_utils.ds_type()
                if not ds_type:
                    ds_type = "PS"
                enc_utils.mvs_convert_encoding(
                    target,
                    target,
                    self.from_encoding,
                    self.to_encoding,
                    src_type=ds_type,
                    dest_type=ds_type,
                    tmphlq=self.tmphlq
                )
                self.encoded.append(os.path.abspath(target))
            except Exception:
                self.failed_on_encoding.append(os.path.abspath(target))
        return {
            "encoded": self.encoded,
            "failed_on_encoding": self.failed_on_encoding,
            "skipped_encoding_targets": self.skipped_encoding_targets
        }


class AMATerseUnarchive(MVSUnarchive):
    def __init__(self, module):
        """Unarchive for AMATerse archives.
        Parameters
        ----------
        module : AnsibleModule
            AnsibleModule to use.
        """
        super(AMATerseUnarchive, self).__init__(module)

    def unpack(self, src, dest):
        """Unpacks using AMATerse, assumes the data set has only been packed once.
        Parameters
        ----------
        src : str
            Source of the archive to unpack.
        dest : str
            Destination dataset to unpack the file.
        Returns
        -------
        int
            Return code.
        Raises
        ------
        fail_json
            Failed executing AMATERSE to restore source into destination.
        """
        dds = {'args': 'UNPACK', 'sysut1': src, 'sysut2': dest}
        rc, out, err = mvs_cmd.amaterse(cmd="", dds=dds)
        if rc != 0:
            ds_remove_list = [dest, src] if not self.remote_src else [dest]
            self.clean_environment(data_sets=ds_remove_list, uss_files=[], remove_targets=True)
            self.module.fail_json(
                msg="Failed executing AMATERSE to restore {0} into {1}".format(src, dest),
                stdout=out,
                stderr=err,
                stdout_lines=out.splitlines(),
                stderr_lines=err.splitlines(),
                rc=rc,
            )
        return rc


class XMITUnarchive(MVSUnarchive):
    def __init__(self, module):
        super(XMITUnarchive, self).__init__(module)
        """Unarchive for XMIT archives.

        Parameters
        ----------
        module : AnsibleModule
            AnsibleModule to use.
        super(XMITUnarchive, self).__init__(module)
        """

    def unpack(self, src, dest):
        """Unpacks using XMIT.
        Parameters
        ----------
        src : str
            Is the archive.
        dest : str
            Is the destination dataset.
        Returns
        -------
        int
            Return code.
        Raises
        ------
        fail_json
            Failed executing RECEIVE to restore source into destination.
        """
        unpack_cmd = """
        PROFILE NOPROMPT
        RECEIVE INDSN('{0}')
        DA('{1}')
        """.format(src, dest)
        rc, out, err = mvs_cmd.ikjeft01(cmd=unpack_cmd, authorized=True)
        if rc != 0:
            ds_remove_list = [dest, src] if not self.remote_src else [dest]
            self.clean_environment(data_sets=ds_remove_list, uss_files=[], remove_targets=True)
            self.module.fail_json(
                msg="Failed executing RECEIVE to restore {0} into {1}".format(src, dest),
                stdout=out,
                stderr=err,
                stdout_lines=out.splitlines(),
                stderr_lines=err.splitlines(),
                rc=rc,
            )
        return rc


def get_unarchive_handler(module):
    """Returns the appropriate class for the format used.
    Parameters
    ----------
    Module : AnsibleModule
        AnsibleModule to use.
    Returns
    -------
    TarUnarchive
        The appropriate object type for 'tar', 'gz', 'bz2' and 'pax' formats.
    AMATerseUnarchive
        The appropriate object type for 'terse' format.
    XMITUnarchive
        The appropriate object type for 'xmit' format.
    ZipUnarchive
        The appropriate object type for any other format.
    """
    format = module.params.get("format").get("type")
    if format in ["tar", "gz", "bz2", "pax"]:
        return TarUnarchive(module)
    elif format == "terse":
        return AMATerseUnarchive(module)
    elif format == "xmit":
        return XMITUnarchive(module)
    return ZipUnarchive(module)


def tar_filter(member, dest_path):
    """Filter for tar format members.
    Parameters
    ----------
    member : tarinfo
        Object containing information about the member.
    dest_path : str
        Real destination path the member is in.
    Raises
    ------
    AbsolutePathError
        Unable to extract as the files extracted can not contain an absolute path.
    OutsideDestinationError
        Unable to extract to a path which is outside the designated destination.
    AbsoluteLinkError
        File is a symlink to an absolute path.
    LinkOutsideDestinationError
        Unable to extract, since it would link to a path which is outside the designated destination.
    """
    name = member.name
    if name.startswith(('/', os.sep)):
        name = member.path.lstrip('/' + os.sep)
    if os.path.isabs(name):
        raise AbsolutePathError
    target_path = os.path.realpath(os.path.join(validation.validate_safe_path(dest_path), validation.validate_safe_path(name)))
    if os.path.commonpath([target_path, dest_path]) != dest_path:
        raise OutsideDestinationError(member, target_path)
    if member.islnk() or member.issym():
        if os.path.isabs(member.linkname):
            raise AbsoluteLinkError(member)
        target_path = os.path.realpath(os.path.join(validation.validate_safe_path(dest_path), validation.validate_safe_path(member.linkname)))
        if os.path.commonpath([target_path, dest_path]) != dest_path:
            raise LinkOutsideDestinationError(member, target_path)


def zip_filter(member, dest_path):
    """Filter for zip format members.
    Parameters
    ----------
    member : tarinfo
        Object containing information about the member.
    dest_path : str
        Real destination path the member is in.
    Raises
    ------
    AbsolutePathError
        Unable to extract as the files extracted can not contain an absolute path.
    OutsideDestinationError
        Unable to extract to a path which is outside the designated destination.
    """
    name = member.filename
    if name.startswith(('/', os.sep)):
        name = name.lstrip('/' + os.sep)
    if os.path.isabs(name):
        raise AbsolutePathError
    target_path = os.path.realpath(os.path.join(validation.validate_safe_path(dest_path), validation.validate_safe_path(name)))
    if os.path.commonpath([target_path, dest_path]) != dest_path:
        raise OutsideDestinationError(member, target_path)


def sanitize_members(members, dest, format):
    """Filter inspired by (PEP 706)
    - Refuse to extract any absolute path
    - Refuse to extract any member with leading '/'

    Parameters
    ----------
    members : list[tarinfo]
        List containing information about the members.
    dest : str
        Destination path the members are in.
    format : str
        Format of the member.
    Returns
    -------
    Union[tarinfo]
        Sanitized members list.
    """
    dest_path = os.path.realpath(dest)
    for member in members:
        if format == 'zip':
            zip_filter(member, dest_path)
        else:
            tar_filter(member, dest_path)
    return members


class AbsolutePathError(Exception):
    def __init__(self, tarinfo):
        """Unable to extract as the files extracted can not contain an absolute path.
        Parameters
        ----------
        tarinfo : tarinfo
            Information about the tar archive.
        Attributes
        ----------
        msg : str
            Human readable string describing the exception.
        """
        self.msg = "Unable to extract {0} as the files extracted can not contain an absolute path".format(tarinfo.name)
        super().__init__(self.msg)


class OutsideDestinationError(Exception):
    def __init__(self, tarinfo, path):
        """Unable to extract to a path which is outside the designated destination.
        Parameters
        ----------
        tarinfo : tarinfo
            Information about the tar archive.
        path : str
            Path to the directory that was tried to extract into.
        Attributes
        ----------
        msg : str
            Human readable string describing the exception.
        """
        self.msg = 'Unable to extract {0} to {1}, which is outside the designated destination'.format(tarinfo.name, path)
        super().__init__(self.msg)


class AbsoluteLinkError(Exception):
    def __init__(self, tarinfo):
        """File is a symlink to an absolute path.
        Parameters
        ----------
        tarinfo : tarinfo
            Information about the tar archive.
        Attributes
        ----------
        msg : str
            Human readable string describing the exception.
        """
        self.msg = '{0} is a symlink to an absolute path'.format(tarinfo.name)
        super().__init__(self.msg)


class LinkOutsideDestinationError(Exception):
    def __init__(self, tarinfo, path):
        """Unable to extract, since it would link to a path which is outside the designated destination.
        Parameters
        ----------
        tarinfo : tarinfo
            Information about the tar archive.
        path : str
            Path to the directory that was tried to extract into.
        Attributes
        ----------
        msg : str
            Human readable string describing the exception.
        """
        self.msg = 'Unable to extract {0} it would link to {1}, which is outside the designated destination'.format(tarinfo.name, path)
        super().__init__()


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


def run_module():
    """Initialize module.
    Raises
    ------
    fail_json
        Parameter verification failed.
    fail_json
        Source does not exists, please provide a valid src.
    """
    module = AnsibleModule(
        argument_spec=dict(
            src=dict(type='str', required=True),
            dest=dict(type='str'),
            include=dict(type='list', elements='str'),
            exclude=dict(type='list', elements='str'),
            list=dict(type='bool', default=False),
            format=dict(
                type='dict',
                required=True,
                options=dict(
                    type=dict(
                        type='str',
                        required=True,
                        choices=['bz2', 'gz', 'tar', 'zip', 'terse', 'xmit', 'pax'],
                    ),
                    options=dict(
                        type='dict',
                        required=False,
                        options=dict(
                            xmit_log_data_set=dict(
                                type='str',
                                required=False,
                            ),
                            dest_volumes=dict(
                                type='list',
                                elements='str',
                            ),
                            adrdssu=dict(
                                type='bool',
                                default=False,
                            )
                        )
                    ),
                ),
            ),
            group=dict(type='str'),
            mode=dict(type='str'),
            owner=dict(type='str'),
            dest_data_set=dict(
                type='dict',
                required=False,
                options=dict(
                    name=dict(
                        type='str', required=False,
                    ),
                    type=dict(
                        type='str',
                        choices=['seq', 'pds', 'pdse'],
                        required=False,
                        default='seq',
                    ),
                    space_primary=dict(
                        type='int', required=False),
                    space_secondary=dict(
                        type='int', required=False),
                    space_type=dict(
                        type='str',
                        choices=['k', 'm', 'g', 'cyl', 'trk'],
                        required=False,
                    ),
                    record_format=dict(
                        type='str',
                        choices=["fb", "vb", "fba", "vba", "u"],
                        required=False
                    ),
                    record_length=dict(type='int', required=False),
                    block_size=dict(type='int', required=False),
                    directory_blocks=dict(type="int", required=False),
                    key_offset=dict(type="int", required=False, no_log=False),
                    key_length=dict(type="int", required=False, no_log=False),
                    sms_storage_class=dict(type="str", required=False),
                    sms_data_class=dict(type="str", required=False),
                    sms_management_class=dict(type="str", required=False),
                )
            ),
            tmp_hlq=dict(type='str'),
            force=dict(type='bool', default=False),
            remote_src=dict(type='bool', default=False),
            encoding=dict(
                type='dict',
                required=False,
                options={
                    'from': dict(
                        type='str',
                        required=False,
                    ),
                    "to": dict(
                        type='str',
                        required=False,
                    ),
                    "skip_encoding": dict(
                        type='list',
                        elements='str',
                        required=False,
                    )
                }
            ),
        ),
        mutually_exclusive=[
            ['include', 'exclude'],
        ],
        supports_check_mode=True,
    )
    validate_dependencies(module)

    arg_defs = dict(
        src=dict(type='str', required=True),
        dest=dict(type='str', required=False, default=''),
        include=dict(type='list', elements='str'),
        exclude=dict(type='list', elements='str'),
        list=dict(type='bool', default=False),
        format=dict(
            type='dict',
            required=True,
            options=dict(
                type=dict(
                    type='str',
                    required=True,
                    default='gz',
                    choices=['bz2', 'gz', 'tar', 'zip', 'terse', 'xmit', 'pax'],
                ),
                options=dict(
                    type='dict',
                    required=False,
                    options=dict(
                        xmit_log_data_set=dict(
                            type='str',
                            required=False,
                        ),
                        dest_volumes=dict(
                            type='list',
                            elements='str',
                        ),
                        adrdssu=dict(
                            type='bool',
                            default=False,
                        ),
                    ),
                    default=dict(xmit_log_data_set=""),
                )
            ),
            default=dict(type="", options=dict(xmit_log_data_set="")),
        ),
        dest_data_set=dict(
            arg_type='dict',
            required=False,
            options=dict(
                name=dict(arg_type='str', required=False),
                type=dict(arg_type='str', required=False, default="seq"),
                space_primary=dict(arg_type='int', required=False),
                space_secondary=dict(
                    arg_type='int', required=False),
                space_type=dict(arg_type='str', required=False),
                record_format=dict(
                    arg_type='str', required=False),
                record_length=dict(type='int', required=False),
                block_size=dict(arg_type='int', required=False),
                directory_blocks=dict(arg_type="int", required=False),
                key_offset=dict(arg_type="int", required=False),
                key_length=dict(arg_type="int", required=False),
                sms_storage_class=dict(arg_type="str", required=False),
                sms_data_class=dict(arg_type="str", required=False),
                sms_management_class=dict(arg_type="str", required=False),
            )
        ),
        group=dict(type='str'),
        mode=dict(type='str'),
        owner=dict(type='str'),
        tmp_hlq=dict(type='qualifier_or_empty', default=''),
        force=dict(type='bool', default=False),
        remote_src=dict(type='bool', default=False),
        mutually_exclusive=[
            ['include', 'exclude'],
        ],
        encoding=dict(
            type='dict',
            options={
                'from': dict(type='str'),
                'to': dict(type='str'),
                'skip_encoding': dict(type='list', elements='str', required=False),
            }
        ),
    )

    try:
        parser = better_arg_parser.BetterArgParser(arg_defs)
        parsed_args = parser.parse_args(module.params)
        module.params = parsed_args
    except ValueError as err:
        module.fail_json(msg="Parameter verification failed", stderr=str(err))

    # Initialize logging module
    module_verbosity_level = module._verbosity
    SingletonLogger().get_logger(module_verbosity_level)

    unarchive = get_unarchive_handler(module)

    if not unarchive.src_exists():
        module.fail_json(msg="{0} does not exists, please provide a valid src.".format(module.params.get("src")))

    if unarchive.list:
        unarchive.list_archive_content()
        module.exit_json(**unarchive.result)

    unarchive.extract_src()

    if unarchive.dest_unarchived() and unarchive.dest_type() == "USS":
        unarchive.update_permissions()

    encoding = parsed_args.get("encoding")
    if unarchive.dest_unarchived() and encoding:
        unarchive.encoding_targets()
        encoding_result = unarchive.encode_destination()
        unarchive.result.update({
            "encoded": encoding_result.get("encoded", []),
            "failed_on_encoding": encoding_result.get("failed_on_encoding", []),
            "skipped_encoding_targets": encoding_result.get("skipped_encoding_targets", [])
        })

    module.exit_json(**unarchive.result)


def main():
    run_module()


if __name__ == '__main__':
    main()
