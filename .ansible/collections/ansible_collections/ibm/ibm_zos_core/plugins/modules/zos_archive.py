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

from __future__ import absolute_import, division, print_function

__metaclass__ = type

DOCUMENTATION = r'''
---
module: zos_archive
version_added: "1.7.0"
author:
  - Oscar Fernando Flores Garcia (@fernandofloresg)
short_description: Archive files and data sets on z/OS.

description:
  - Create or extend an archive on a remote z/OS system.
  - Sources for archiving must be on the remote z/OS system.
  - Supported sources are USS (UNIX System Services) or z/OS data sets.
  - The archive remains on the remote z/OS system.
  - For supported archive formats, see option C(format).

options:
  src:
    description:
      - List of names or globs of UNIX System Services (USS) files,
        PS (sequential data sets), PDS, PDSE to compress or archive.
      - USS file paths should be absolute paths.
      - GDS relative notation is supported.
      - "MVS data sets supported types are: C(SEQ), C(PDS), C(PDSE)."
      - VSAMs are not supported.
      - GDS relative names are supported. e.g. I(USER.GDG(-1)).
    type: list
    required: true
    elements: str
  format:
    description:
      - The compression type and corresponding options to use when archiving
        data.
    type: dict
    required: false
    suboptions:
      type:
        description:
          - The compression format to use.
        type: str
        required: false
        default: gz
        choices:
          - bz2
          - gz
          - tar
          - zip
          - terse
          - xmit
          - pax
        aliases: [ name ]
      options:
        description:
          - Options specific to a compression format.
        type: dict
        required: false
        suboptions:
          spack:
            description:
              - Compression option for use with the terse format,
                I(type=terse).
              - Pack will compress records in a data set so that the output
                results in lossless data compression.
              - Spack will compress records in a data set so the output results
                in complex data compression.
              - Spack will produce smaller output and take approximately 3
                times longer than pack compression.
            type: bool
            required: false
            default: true
          xmit_log_data_set:
            description:
              - Provide the name of a data set to store xmit log output.
              - If the data set provided does not exist, the program
                will create it.
              - "If the data set provided exists, the data set must have
                the following attributes: LRECL=255, BLKSIZE=3120, and
                RECFM=VB"
              - When providing the I(xmit_log_data_set) name, ensure there
                is adequate space.
            type: str
          adrdssu:
            description:
              - If set to true, the C(zos_archive) module will use Data
                Facility Storage Management Subsystem data set services
                (DFSMSdss) program ADRDSSU to compress data sets into a
                portable format before using C(xmit) or C(terse).
            type: bool
            default: false
  dest:
    description:
      - The remote absolute path or data set where the archive should be
        created.
      - I(dest) can be a USS file or MVS data set name.
      - If I(dest) has missing parent directories, they will be created.
      - If I(dest) is a nonexistent USS file, it will be created.
      - If I(dest) is an existing file or data set and I(force=true),
        the existing I(dest) will be deleted and recreated with attributes
        defined in the I(dest_data_set) option or computed by the module.
      - If I(dest) is an existing file or data set and I(force=false) or not
        specified, the module exits with a note to the user.
      - Destination data set attributes can be set using I(dest_data_set).
      - Destination data set space will be calculated based on space of
        source data sets provided and/or found by expanding the pattern name.
        Calculating space can impact module performance. Specifying space attributes
        in the I(dest_data_set) option will improve performance.
      - GDS relative names are supported. e.g. I(USER.GDG(-1)).
    type: str
    required: true
  exclude:
    description:
      - Remote absolute path, glob, or list of paths, globs, data set name
        patterns or generation data sets (GDSs) in relative notation for the file,
        files or data sets to exclude from src list and glob expansion.
      - "Patterns (wildcards) can contain one of the following, `?`, `*`."
      - "* matches everything."
      - "? matches any single character."
      - GDS relative names are supported. e.g. I(USER.GDG(-1)).
    type: list
    required: false
    elements: str
  group:
    description:
      - Name of the group that will own the archive file.
      - When left unspecified, it uses the current group of the current use
        unless you are root, in which case it can preserve the previous
        ownership.
      - This option is only applicable if C(dest) is USS, otherwise ignored.
    type: str
    required: false
  mode:
    description:
      - The permission of the destination archive file.
      - If C(dest) is USS, this will act as Unix file mode, otherwise
        ignored.
      - It should be noted that modes are octal numbers.
        The user must either add a leading zero so that Ansible's YAML
        parser knows it is an octal number (like C(0644) or C(01777))or
        quote it (like C('644') or C('1777')) so Ansible receives a string
        and can do its own conversion from string into number. Giving Ansible
        a number without following one of these rules will end up with a
        decimal number which will have unexpected results.
      - The mode may also be specified as a symbolic mode
        (for example, 'u+rwx' or 'u=rw,g=r,o=r') or a special
        string 'preserve'.
      - I(mode=preserve) means that the file will be given the same permissions
        as the src file.
    type: str
    required: false
  owner:
    description:
      - Name of the user that should own the archive file, as would be
        passed to the chown command.
      - When left unspecified, it uses the current user unless you are root,
        in which case it can preserve the previous ownership.
      - This option is only applicable if C(dest) is USS, otherwise ignored.
    type: str
    required: false
  remove:
    description:
      - Remove any added source files , trees or data sets after module
        L(zos_archive,./zos_archive.html) adds them to the archive.
        Source files, trees and data sets are identified with option I(src).
    type: bool
    required: false
    default: false
  dest_data_set:
    description:
      - Data set attributes to customize a C(dest) data set to be archived into.
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
            data set. (e.g C(FB))
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
      - The default HLQ is the Ansible user used to execute the module and
        if that is not available, then the environment variable value
        C(TMPHLQ) is used.
    required: false
    type: str
  force:
    description:
      - If set to C(true) and the remote file or data set C(dest) will be
        deleted. Otherwise it will be created with the C(dest_data_set)
        attributes or default values if C(dest_data_set) is not specified.
      - If set to C(false), the file or data set will only be copied if the
        destination does not exist.
      - If set to C(false) and destination exists, the module exits with a
        note to the user.
    type: bool
    default: false
    required: false
  encoding:
    description:
      - Specifies the character encoding conversion to be applied to the
        source files before archiving.
      - Supported character sets rely on the charset conversion utility
        C(iconv) version the most common character sets are supported.
      - After conversion the files are stored in same location and name
        as src and the same src is taken in consideration for archive.
      - Source files will be converted to the new encoding and will not
        be restored to their original encoding.
      - If encoding fails for any file in a set of multiple files, an
        exception will be raised and archiving will be skipped.
      - The original files in C(src) will be converted. The module will
        revert the encoding conversion after a successful archive, but
        no backup will be created. If you need to encode using a backup
        and then archive take a look at L(zos_encode,./zos_encode.html) module.
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
          - List of names to skip encoding before archiving. This is only used if I(encoding) is set, otherwise is ignored.
        required: false
        type: list
        elements: str
attributes:
  action:
    support: none
    description: Indicates this has a corresponding action plugin so some parts of the options can be executed on the controller.
  async:
    support: full
    description: Supports being used with the ``async`` keyword.
  check_mode:
    support: full
    description: Can run in check_mode and return changed status prediction without modifying target. If not supported, the action will be skipped.

notes:
  - This module does not perform a send or transmit operation to a remote
    node. If you want to transport the archive you can use zos_fetch to
    retrieve to the controller and then zos_copy or zos_unarchive for
    copying to a remote or send to the remote and then unpack the archive
    respectively.
  - When packing and using C(adrdssu) flag the module will take up to two
    times the space indicated in C(dest_data_set).
  - tar, zip, bz2 and pax are archived using python C(tarfile) library which
    uses the latest version available for each format, for compatibility when
    opening from system make sure to use the latest available version for the
    intended format.


seealso:
  - module: ibm.ibm_zos_core.zos_fetch
  - module: ibm.ibm_zos_core.zos_unarchive
'''

EXAMPLES = r'''
# Simple archive
- name: Archive file into a tar
  zos_archive:
    src: /tmp/archive/foo.txt
    dest: /tmp/archive/foo_archive_test.tar
    format:
      type: tar

# Archive multiple files
- name: Archive list of files into a zip
  zos_archive:
    src:
      - /tmp/archive/foo.txt
      - /tmp/archive/bar.txt
    dest: /tmp/archive/foo_bar_archive_test.zip
    format:
    type: zip

# Archive one data set into terse
- name: Archive data set into a terse
  zos_archive:
    src: "USER.ARCHIVE.TEST"
    dest: "USER.ARCHIVE.RESULT.TRS"
    format:
      type: terse

# Use terse with different options
- name: Archive data set into a terse, specify pack algorithm and use adrdssu
  zos_archive:
    src: "USER.ARCHIVE.TEST"
    dest: "USER.ARCHIVE.RESULT.TRS"
    format:
      type: terse
      options:
        spack: true
        adrdssu: true

# Use a pattern to store
- name: Archive data set pattern using xmit
  zos_archive:
    src: "USER.ARCHIVE.*"
    exclude_sources: "USER.ARCHIVE.EXCLUDE.*"
    dest: "USER.ARCHIVE.RESULT.XMIT"
    format:
      type: xmit

- name: Archive multiple GDSs into a terse
  zos_archive:
    src:
      - "USER.GDG(0)"
      - "USER.GDG(-1)"
      - "USER.GDG(-2)"
    dest: "USER.ARCHIVE.RESULT.TRS"
    format:
      type: terse
      options:
        adrdssu: true

- name: Archive multiple data sets into a new GDS
  zos_archive:
    src: "USER.ARCHIVE.*"
    dest: "USER.GDG(+1)"
    format:
      type: terse
      options:
        adrdssu: true

- name: Encode the source data set into Latin-1 before archiving into a terse data set
  zos_archive:
    src: "USER.ARCHIVE.TEST"
    dest: "USER.ARCHIVE.RESULT.TRS"
    format:
      type: terse
    encoding:
      from: IBM-1047
      to: ISO8859-1

- name: Encode and archive multiple data sets but skip encoding for a few.
  zos_archive:
    src:
      - "USER.ARCHIVE1.TEST"
      - "USER.ARCHIVE2.TEST"
    dest: "USER.ARCHIVE.RESULT.TRS"
    format:
      type: terse
      options:
        adrdssu: true
    encoding:
      from: IBM-1047
      to: ISO8859-1
      skip_encoding:
        - "USER.ARCHIVE2.TEST"
'''

RETURN = r'''
dest:
    description:
        - The remote absolute path or data set where the archive was
          created.
    type: str
    returned: always
state:
    description:
        - The state of the input C(src).
        - C(absent) when the source files or data sets were removed.
        - C(present) when the source files or data sets were not removed.
        - C(incomplete) when C(remove) was true and the source files or
          data sets were not removed.
    type: str
    returned: always
dest_state:
    description:
      - The state of the I(dest) file or data set.
      - C(absent) when the file does not exist.
      - C(archive) when the file is an archive.
      - C(compress) when the file is compressed, but not an archive.
      - C(incomplete) when the file is an archive, but some files under
        I(src) were not found.
    type: str
    returned: success
missing:
    description: Any files or data sets that were missing from the source.
    type: list
    returned: success
archived:
    description:
    - Any files or data sets that were compressed or added to the
      archive.
    type: list
    returned: success
arcroot:
    description:
      - If C(src) is a list of USS files, this returns the top most parent
        folder of the list of files, otherwise is empty.
    type: str
    returned: always
expanded_sources:
    description: The list of matching paths from the src option.
    type: list
    returned: always
expanded_exclude_sources:
    description: The list of matching exclude paths from the exclude option.
    type: list
    returned: always
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
import glob
import math
import os
import re
import tarfile
import traceback
import zipfile
from hashlib import sha256

from ansible.module_utils._text import to_bytes
from ansible.module_utils.basic import AnsibleModule
from ansible_collections.ibm.ibm_zos_core.plugins.module_utils import (
    better_arg_parser, data_set, mvs_cmd, validation, encode)
from ansible_collections.ibm.ibm_zos_core.plugins.module_utils.import_handler import \
    ZOAUImportError
from ansible_collections.ibm.ibm_zos_core.plugins.module_utils.dependency_checker import (
    validate_dependencies,
)
from ansible_collections.ibm.ibm_zos_core.plugins.module_utils.log import SingletonLogger

try:
    from zoautil_py import datasets
except Exception:
    datasets = ZOAUImportError(traceback.format_exc())

XMIT_RECORD_LENGTH = 80
AMATERSE_RECORD_LENGTH = 1024

STATE_ABSENT = 'absent'
STATE_PRESENT = 'present'
STATE_ARCHIVE = 'archive'
STATE_COMPRESSED = 'compressed'
STATE_INCOMPLETE = 'incomplete'


def get_archive_handler(module):
    """
    Return the proper archive handler based on archive format.

    Parameters
    ----------
    module : AnsibleModule
        Ansible Module.

    Returns
    -------
    Archive : Archive
        The archive format for the module.

    """
    format = module.params.get("format").get("type")
    if format in ["tar", "gz", "bz2", "pax"]:
        return TarArchive(module)
    elif format == "terse":
        return AMATerseArchive(module)
    elif format == "xmit":
        return XMITArchive(module)
    return ZipArchive(module)


def strip_prefix(prefix, string):
    """Strip prefix.

    Parameters
    ---------
    prefix : str
        Prefix to take out of the string.
    string : str
        String with the prefix.

    Returns
    -------
    str
        Given string without the prefix.
    """
    return string[len(prefix):] if string.startswith(prefix) else string


def expand_paths(paths):
    """Expand paths.

    Parameters
    ----------
    paths : list[str]
        List with the paths.

    Returns
    -------
    Union[str]
        Expanded path.
    """
    expanded_path = []
    for path in paths:
        if '*' in path or '?' in path:
            e_paths = glob.glob(path)
        else:
            e_paths = [path]
        expanded_path.extend(e_paths)
    return expanded_path


def is_archive(path):
    """If a path refers to an archive.

    Parameters
    ----------
    path : str
        The path to the archive.

    Returns
    -------
    bool
        If is archive.
    """
    return re.search(r'\.(tar|tar\.(gz|bz2|xz)|tgz|tbz2|zip|gz|bz2|xz|pax)$', os.path.basename(path), re.IGNORECASE)


class Archive():
    def __init__(self, module):
        """Handles archive operations.

        Parameters
        ----------
        module : AnsibleModule
            Ansible module to get parameters from.

        Attributes
        ----------
        module : AnsibleModule
            AnsibleModule to use.
        dest : str
            Destination path.
        format : dict
            The compression type and corresponding options to use when archiving
            data.
        remove : bool
            Whether to remove any added source files, trees or data sets after module
            adds them to the archive.
        changed : bool
            If there are targeted paths.
        errors : str
            Errors ocurred.
        found : list[str]
            List of found datasets.
        targets : list[str]
            List of paths that are in sources given.
        archived : list[str]
            Any files or data sets that were compressed or added to the
            archive.
        not_found : list[str]
            List of paths that are missing from the sources.
        force : bool
            If set to true and the remote file or data set dest will be
            deleted.
        sources : list[str]
            List of sources to get files from.
        arcroot : str
            If src is a list of USS files, this returns the top most parent
            folder of the list of files, otherwise is empty.
        expanded_sources : list[str]
            The list of matching paths from the src option.
        expanded_exclude_sources : list[str]
            The list of matching exclude paths from the exclude option.
        dest_state : str
            The state of the dest file or data set.
        state : str
            The state of the input C(src).
        xmit_log_data_set : str
            The name of the data set to store xmit log output.
        skip_encoding : list[str]
            List of paths to exclude in encoding.
        from_encoding: str
            The encoding of the source file.
        to_encoding : str
            The required encoding of the destination file.
        skipped_encoding_targets : list[str]
            List of paths to exclude in encoding return value.

        """
        self.module = module
        self.dest = module.params['dest']
        self.format = module.params.get("format").get("type")
        self.remove = module.params['remove']
        self.changed = False
        self.errors = []
        self.found = []
        self.targets = []
        self.archived = []
        self.not_found = []
        self.force = module.params['force']
        self.sources = module.params['src']
        self.arcroot = ""
        self.expanded_sources = ""
        self.expanded_exclude_sources = ""
        self.dest_state = STATE_ABSENT
        self.state = STATE_PRESENT
        self.xmit_log_data_set = ""
        encoding_param = module.params.get("encoding") or {}
        self.from_encoding = encoding_param.get("from")
        self.to_encoding = encoding_param.get("to")
        self.encoded = list()
        self.failed_on_encoding = list()
        self.skip_encoding = encoding_param.get("skip_encoding")
        self.skipped_encoding_targets = ""
        self.encode_targets = []

    def targets_exist(self):
        """Returns if there are targets or not.

        Returns
        -------
        bool
            If the targets list is not empty.
        """
        return bool(self.targets)

    @abc.abstractmethod
    def dest_exists(self):
        pass

    @abc.abstractmethod
    def dest_type(self):
        pass

    @abc.abstractmethod
    def update_permissions(self):
        return

    @abc.abstractmethod
    def find_targets(self):
        pass

    @abc.abstractmethod
    def encoding_targets(self):
        pass

    @abc.abstractmethod
    def _get_checksums(self, src):
        pass

    @abc.abstractmethod
    def dest_checksums(self):
        pass

    @abc.abstractmethod
    def is_different_from_original(self):
        pass

    @abc.abstractmethod
    def remove_targets(self):
        pass

    @abc.abstractmethod
    def compute_dest_size(self):
        pass

    @abc.abstractmethod
    def encode_source(self):
        pass

    @abc.abstractmethod
    def revert_encoding(self):
        pass

    @property
    def result(self):
        """Returns a dict with the results.

        Returns
        -------
        dict
            Arguments showing the result.
        """
        return {
            'archived': self.archived,
            'dest': self.dest,
            'state': self.state,
            'arcroot': self.arcroot,
            'dest_state': self.dest_state,
            'changed': self.changed,
            'missing': self.not_found,
            'expanded_sources': list(self.expanded_sources),
            'expanded_exclude_sources': list(self.expanded_exclude_sources),
            'xmit_log_data_set': self.xmit_log_data_set,
            'encoded': getattr(self, 'encoded'),
            'failed_on_encoding': getattr(self, 'failed_on_encoding'),
            'skipped_encoding_targets': getattr(self, 'skipped_encoding_targets'),
        }


class USSArchive(Archive):
    def __init__(self, module):
        """Archive for USS files.

        Parameters
        ----------
        module : AnsibleModule
            Ansible module to get parameters from.

        Attributes
        ----------
        original_checksums : str
            The SHA256 hash of the contents of input file.
        arcroot : str
            If src is a list of USS files, this returns the top most parent
            folder of the list of files, otherwise is empty.
        expanded_sources : list[str]
            The list of matching paths from the src option.
        expanded_exclude_sources : list[str]
            The list of matching exclude paths from the exclude option.
        sources : list[str]
            List of sources to get files from.
        """
        super(USSArchive, self).__init__(module)
        self.original_checksums = self.dest_checksums()
        if len(self.sources) == 1:
            self.arcroot = os.path.dirname(os.path.commonpath(self.sources))
        else:
            self.arcroot = os.path.commonpath(self.sources)
        self.expanded_sources = expand_paths(self.sources)
        self.expanded_exclude_sources = expand_paths(module.params['exclude'])
        self.expanded_exclude_sources = "" if len(self.expanded_exclude_sources) == 0 else self.expanded_exclude_sources

        self.sources = sorted(set(self.expanded_sources) - set(self.expanded_exclude_sources))

    def dest_exists(self):
        """Returns if destination path exits.

        Returns
        -------
        bool
            If destination path exists.
        """
        return os.path.exists(self.dest)

    def dest_type(self):
        """Returns the destination type.

        Returns
        str
            "USS".
        """
        return "USS"

    def update_permissions(self):
        """Updates permissions.
        """
        file_args = self.module.load_file_common_arguments(self.module.params, path=self.dest)
        self.changed = self.module.set_fs_attributes_if_different(file_args, self.changed)

    def find_targets(self):
        """Classifies paths in source to either targets or not_found based on whether they exist or not.
        """
        for path in self.sources:
            if os.path.exists(path):
                self.targets.append(path)
            else:
                self.not_found.append(path)

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

    def _get_checksums(self, src):
        """Calculate SHA256 hash for a given file.

        Parameters
        ----------
        src : str
            The absolute path of the file.

        Returns
        -------
        str
            The SHA256 hash of the contents of input file.
        """
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
        except Exception:
            raise
        return hash_digest.hexdigest()

    def dest_checksums(self):
        """Returns destination file checksums if it exists.

        Returns
        -------
        str
            The SHA256 hash of the contents of destination file.
        """
        if self.dest_exists():
            return self._get_checksums(self.dest)
        return None

    def is_different_from_original(self):
        """Checks if the destination is different from the original based on checksums.

        Returns
        -------
        bool
            If the SHA256 hash of the contents of destination file is different from the original's.
        """
        if self.original_checksums is not None:
            return self.original_checksums != self.dest_checksums()
        return True

    def remove_targets(self):
        """Removes the archived targets and changes the state accordingly.
        """
        self.state = STATE_ABSENT
        for target in self.archived:
            if os.path.isdir(target):
                try:
                    os.removedirs(target)
                except Exception:
                    self.state = STATE_INCOMPLETE
            else:
                try:
                    os.remove(target)
                except PermissionError:
                    self.state = STATE_INCOMPLETE

    def archive_targets(self):
        """Archives targets
        """
        self.file = self.open(self.dest)

        try:
            for target in self.targets:
                if os.path.isdir(target):
                    for directory_path, directory_names, file_names in os.walk(target, topdown=True):
                        for directory_name in directory_names:
                            full_path = os.path.join(
                                validation.validate_safe_path(directory_path),
                                validation.validate_safe_path(directory_name)
                            )
                            self.add(full_path, strip_prefix(self.arcroot, full_path))

                        for file_name in file_names:
                            full_path = os.path.join(
                                validation.validate_safe_path(directory_path),
                                validation.validate_safe_path(file_name)
                            )
                            self.add(full_path, strip_prefix(self.arcroot, full_path))
                else:
                    self.add(target, strip_prefix(self.arcroot, target))
        except Exception as e:
            self.dest_state = STATE_INCOMPLETE
            if self.format == 'tar':
                archive_format = self.format
            else:
                archive_format = 'tar.' + self.format
            self.module.fail_json(
                msg='Error when writing %s archive at %s: %s' % (
                    archive_format, self.destination, e
                ),
                exception=e
            )
        self.file.close()

    def add(self, source, arcname):
        """Add source into the destination archive.

        Parameters
        ----------
        source : str
            Source of the file.
        arcname : str
            Destination archive name for where to add the source into.
        """
        self._add(source, arcname)
        self.archived.append(source)

    def get_state(self):
        """Sets dest_state attribute based on if the destination exists, is an archive or any path was not found.
        """
        if not self.dest_exists():
            self.dest_state = STATE_ABSENT
        else:
            if is_archive(self.dest):
                self.dest_state = STATE_ARCHIVE
            if bool(self.not_found):
                self.dest_state = STATE_INCOMPLETE

    def encode_source(self):
        """Convert encoding for given src
        Returns
        -------
        Union
            encoded, failed_on_encoding or skipped_encoding list
        """
        enc_utils = encode.EncodeUtils()
        self.encoded = []
        self.failed_on_encoding = []

        for target in self.encode_targets:
            try:
                convert_rc = enc_utils.uss_convert_encoding_prev(
                    target, target, self.from_encoding, self.to_encoding
                )
                if convert_rc:
                    enc_utils.uss_tag_encoding(target, self.to_encoding)
                self.encoded.append(os.path.abspath(target))

            except Exception:
                self.failed_on_encoding.append(os.path.abspath(target))

        return {
            "encoded": self.encoded,
            "failed_on_encoding": self.failed_on_encoding,
            "skipped_encoding_targets": self.skipped_encoding_targets
        }

    def revert_encoding(self):
        """Revert src encoding to original
        """
        enc_utils = encode.EncodeUtils()

        for target in self.encoded:
            try:
                convert_rc = enc_utils.uss_convert_encoding_prev(
                    target, target, self.to_encoding, self.from_encoding
                )
                if convert_rc:
                    enc_utils.uss_tag_encoding(target, self.from_encoding)

            except Exception as e:
                warning_message = f"Failed to revert source file {os.path.abspath(target)} to its original encoding."
                raise EncodeError(warning_message) from e


class TarArchive(USSArchive):
    def __init__(self, module):
        """Archive for Tar.

        Parameters
        ----------
        module : AnsibleModule
            AnsibleModule to use.
        """
        super(TarArchive, self).__init__(module)

    def open(self, path):
        """Open the archive with the given path.

        Parameters
        ----------
        path : str
            Path of the archive.

        Returns
        -------
        TarFile
            The opened TarFile.
        """
        if self.format == 'tar':
            file = tarfile.open(path, 'w')
        elif self.format == 'pax':
            file = tarfile.open(path, 'w', format=tarfile.GNU_FORMAT)
        elif self.format in ('gz', 'bz2'):
            file = tarfile.open(path, 'w|' + self.format)
        return file

    def _add(self, source, arcname):
        """Add source into the destination archive.

        Parameters
        ----------
        source : str
            Source of the file.
        arcname : str
            Destination archive name for where to add the source into.
        """
        self.file.add(source, arcname)


class ZipArchive(USSArchive):
    def __init__(self, module):
        """Archive for Zip.

        Parameters
        ----------
        module : AnsibleModule
            AnsibleModule to use.
        """
        super(ZipArchive, self).__init__(module)

    def open(self, path):
        """Open the archive with the given path.

        Parameters
        ----------
        path : str
            Path of the archive.

        Returns
        -------
        ZipFile
            The opened ZipFile.

        Raises
        ------
        BadZipFile
            Improperly compressed zip file, unable to to open file.
        """
        try:
            file = zipfile.ZipFile(path, 'w', zipfile.ZIP_DEFLATED, True)
        except zipfile.BadZipFile:
            self.module.fail_json(
                msg="Improperly compressed zip file, unable to to open file {0} ".format(path)
            )
        return file

    def _add(self, source, arcname):
        """Add source into the destination archive.

        Parameters
        ----------
        source : str
            Source of the file.
        arcname : str
            Destination archive name for where to add the source into.
        """
        self.file.write(source, arcname)


class MVSArchive(Archive):
    def __init__(self, module):
        """Archive for MVS files.

        Parameters
        ----------
        module : AnsibleModule
            AnsibleModule to use.

        Attributes
        ----------
        original_checksums : str
            The SHA256 hash of the contents of input file.
        adrdssu : bool
            Whether to use Data Facility Storage Management Subsystem data set services
            program ADRDSSU to uncompress data sets or not.
        expanded_sources : list[str]
            The list of matching paths from the src option.
        expanded_exclude_sources : list[str]
            The list of matching exclude paths from the exclude option.
        sources : list[str]
            List of sources to get files from.
        dest_dat_set : dict
            Destination data set.
        source_size : int
            Source size.
        tmphlq : str
            High level qualifier for temporary datasets.
        """
        super(MVSArchive, self).__init__(module)
        self.tmphlq = module.params.get("tmp_hlq")
        self.original_checksums = self.dest_checksums()
        self.adrdssu = module.params.get("format").get("options").get("adrdssu")
        self.expanded_sources = self.expand_mvs_paths(self.sources)
        self.expanded_exclude_sources = self.expand_mvs_paths(module.params['exclude'])
        self.sources = sorted(set(self.expanded_sources) - set(self.expanded_exclude_sources))
        self.tmp_data_sets = list()
        self.dest_data_set = module.params.get("dest_data_set")
        self.dest_data_set = dict() if self.dest_data_set is None else self.dest_data_set
        self.ds_types = {}

    def open(self):
        pass

    def close(self):
        pass

    def find_targets(self):
        """Finds target datasets in host.
        """
        for path in self.sources:
            if data_set.DataSet.data_set_exists(path, tmphlq=self.tmphlq):
                self.targets.append(path)
            else:
                self.not_found.append(path)

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
            arguments.update(space_primary=5)
        if space_secondary is None:
            arguments.update(space_secondary=3)
        if space_type is None:
            arguments.update(space_type="m")
        arguments.pop("self")
        changed, zoau_data_set = data_set.DataSet.ensure_present(**arguments)
        return arguments["name"], changed

    def create_dest_ds(self, name):
        """Create destination data set to use as an archive.

        Parameters
        ----------
        name : str
            Name for the dataset.

        Returns
        -------
        str
            Name of the newly created data set.
        """
        record_length = XMIT_RECORD_LENGTH if self.format == "xmit" else AMATERSE_RECORD_LENGTH
        data_set.DataSet.ensure_present(name=name, replace=True, type='seq', record_format='fb', record_length=record_length, tmphlq=self.tmphlq)
        return name

    def dump_into_temp_ds(self, temp_ds):
        """Dump src datasets identified as self.targets into a temporary dataset using ADRDSSU.

        Parameters
        ----------
        temp_ds : str
            Temporal dataset name.

        Returns
        -------
        int
            Return code.

        Raises
        ------
        fail_json
            Failed executing ADRDSSU to archive.
        """
        dump_cmd = """ DUMP OUTDDNAME(TARGET) -
         OPTIMIZE(4) DS(INCL( - """

        for target in self.targets:
            dump_cmd += "\n {0}, - ".format(target)
        dump_cmd += '\n ) '

        if self.force:
            dump_cmd += '- \n ) TOL( ENQF IOER ) '

        dump_cmd += ' )'
        dds = dict(target="{0},old".format(temp_ds))
        rc, out, err = mvs_cmd.adrdssu(cmd=dump_cmd, dds=dds, authorized=True)

        if rc != 0:
            self.module.fail_json(
                msg="Failed executing ADRDSSU to archive {0}".format(temp_ds),
                stdout=out,
                stderr=err,
                stdout_lines=dump_cmd,
                rc=rc,
            )
        return rc

    def _get_checksums(self, src):
        """Calculate SHA256 hash for a given file.

        Parameters
        ----------
        src : str
            The absolute path of the file.

        Returns
        -------
        str
            The SHA256 hash of the contents of input file.
        """
        sha256_cmd = "sha256 \"//'{0}'\"".format(src)
        rc, out, err = self.module.run_command(sha256_cmd, errors='replace')
        checksums = out.split("= ")
        if len(checksums) > 0:
            return checksums[1]
        return None

    def dest_checksums(self):
        """Returns destination file checksums if it exists.

        Returns
        -------
        str
            The SHA256 hash of the contents of destination file.
        """
        if self.dest_exists():
            return self._get_checksums(self.dest)
        return None

    def is_different_from_original(self):
        """Checks if the destination is different from the original based on checksums.

        Returns
        -------
        bool
            If the SHA256 hash of the contents of destination file is different from the original's.
        """
        if self.original_checksums is not None:
            return self.original_checksums != self.dest_checksums()
        return True

    def dest_type(self):
        """Returns the destination type.

        Returns
        str
            "MVS".
        """
        return "MVS"

    def dest_exists(self):
        """Returns if destination path exits.

        Returns
        -------
        bool
            If destination path exists.
        """
        return data_set.DataSet.data_set_exists(self.dest, tmphlq=self.tmphlq)

    def remove_targets(self):
        """Removes the archived targets and changes the state accordingly.
        """
        self.state = STATE_ABSENT
        for target in self.archived:
            try:
                changed = data_set.DataSet.ensure_absent(target)
            except Exception:
                self.state = STATE_INCOMPLETE
            if not changed:
                self.state = STATE_INCOMPLETE
        return

    def expand_mvs_paths(self, paths):
        """Expand mvs paths.

        Parameters
        ----------
        paths : list[str]
            List of paths to expand.

        Returns
        -------
        Union[str]
            Extended paths.
        """
        expanded_path = []
        for path in paths:
            e_path = []
            if '*' in path:
                # list_dataset_names returns a list of data set names or empty.
                e_paths = datasets.list_dataset_names(path)
            else:
                e_paths = [path]

            # resolve GDS relative names
            for index, e_path in enumerate(e_paths):
                if data_set.DataSet.is_gds_relative_name(e_path):
                    e_paths[index] = data_set.DataSet.resolve_gds_absolute_name(e_path)
            expanded_path.extend(e_paths)
        return expanded_path

    def get_state(self):
        """Sets dest_state attribute based on if the destination exists, is an archive or any path was not found.
        """
        if not self.dest_exists():
            self.dest_state = STATE_ABSENT
        else:
            if bool(self.not_found):
                self.dest_state = STATE_INCOMPLETE
            elif bool(self.archived):
                self.dest_state = STATE_ARCHIVE

    def clean_environment(self, data_sets=None, uss_files=None, remove_targets=False):
        """Removes any allocated data sets that won't be needed after module termination.

        Parameters
        ----------
        data_sets : list(str)
            list of data sets to remove
        uss_files : list(str)
            list of uss files to remove
        remove_targets : bool
            Indicates if already unpacked data sets need to be removed too.
        """
        if data_set is not None:
            for ds in data_sets:
                data_set.DataSet.ensure_absent(ds)
        if uss_files is not None:
            for file in uss_files:
                try:
                    os.remove(file)
                except PermissionError:
                    self.state = STATE_INCOMPLETE
        if remove_targets:
            self.remove_targets()

    def compute_dest_size(self):
        """Calculate the destination data set based on targets found. And sets it do space_primary attribute.
        """
        if self.dest_data_set.get("space_primary") is None:
            dest_space = 1
            for target in self.targets:
                data_sets = datasets.list_datasets(target)
                for ds in data_sets:
                    dest_space += int(ds.total_space)
            # space unit returned from listings is bytes
            dest_space = math.ceil(dest_space / 1024)
            self.dest_data_set.update(space_primary=dest_space, space_type="k")

    def encode_source(self):
        """Convert encoding for given src
        Returns
        -------
        Union
            encoded, failed_on_encoding or skipped_encoding list
        """
        enc_utils = encode.EncodeUtils()
        self.encoded = []
        self.failed_on_encoding = []
        for target in self.encode_targets:
            try:
                ds_type = data_set.DataSetUtils(target, tmphlq=self.tmphlq).ds_type()
                if not ds_type:
                    ds_type = "PS"
                self.ds_types[target] = ds_type
                enc_utils.mvs_convert_encoding(
                    target,
                    target,
                    self.from_encoding,
                    self.to_encoding,
                    src_type=ds_type,
                    dest_type=ds_type,
                    tmphlq=self.tmphlq
                )
                self.encoded.append(target)
            except Exception:
                self.failed_on_encoding.append(os.path.abspath(target))
        return {
            "encoded": self.encoded,
            "failed_on_encoding": self.failed_on_encoding,
            "skipped_encoding_targets": self.skipped_encoding_targets
        }

    def revert_encoding(self):
        """Revert src encoding to original
        """
        enc_utils = encode.EncodeUtils()

        for target in self.encoded:
            try:
                ds_type = self.ds_types.get(target, "PS")
                enc_utils.mvs_convert_encoding(
                    target,
                    target,
                    self.to_encoding,
                    self.from_encoding,
                    src_type=ds_type,
                    dest_type=ds_type,
                    tmphlq=self.tmphlq
                )
            except Exception as e:
                warning_message = f"Failed to revert source file {os.path.abspath(target)} to its original encoding."
                raise EncodeError(warning_message) from e


class AMATerseArchive(MVSArchive):
    def __init__(self, module):
        """Archive for XMIT.

        Parameters
        ----------
        module : AnsibleModule
            AnsibleModule to use.

        Attributes
        ----------
        pack_arg : str
            Compression option for use with the terse format.
        """
        super(AMATerseArchive, self).__init__(module)
        spack = module.params.get("format").get("options").get("spack")
        # We store pack_ard in uppercase because the AMATerse command requires
        # it in uppercase.
        self.pack_arg = "SPACK" if spack else "PACK"

    def add(self, src, archive):
        """Archive src into archive using AMATERSE program.

        Parameters
        ----------
        src : str
            Source of the archive.
        archive : str
            Destination archive.

        Returns
        -------
        int
            Return code.

        Raises
        ------
        fail_json
            Failed executing AMATERSE to archive source.
        """
        dds = {'args': self.pack_arg, 'sysut1': src, 'sysut2': archive}
        rc, out, err = mvs_cmd.amaterse(cmd="", dds=dds)
        if rc != 0:
            self.module.fail_json(
                msg="Failed executing AMATERSE to archive {0} into {1}".format(src, archive),
                stdout=out,
                stderr=err,
                rc=rc,
            )
        self.archived = self.targets[:]
        return rc

    def archive_targets(self):
        """Add MVS Datasets to the AMATERSE Archive by creating a temporary dataset and dumping the source datasets into it.

        Raises
        ------
        fail_json
            To archive multiple source data sets, you must use option 'adrdssu=True'.
        """
        if self.adrdssu:
            source, changed = self._create_dest_data_set(
                type="seq",
                record_format="u",
                record_length=0,
                tmp_hlq=self.tmphlq,
                replace=True,
                space_primary=self.dest_data_set.get("space_primary"),
                space_type=self.dest_data_set.get("space_type"))
            self.dump_into_temp_ds(source)
            self.tmp_data_sets.append(source)
        else:
            # If we don't use a adrdssu container we cannot pack multiple data sets
            if len(self.targets) > 1:
                self.module.fail_json(
                    msg="To archive multiple source data sets, you must use option 'adrdssu=True'.")
            source = self.targets[0]
        dataset = data_set.MVSDataSet(
            name=self.dest,
            data_set_type='seq',
            record_format='fb',
            record_length=AMATERSE_RECORD_LENGTH,
            space_primary=self.dest_data_set.get("space_primary"),
            space_type=self.dest_data_set.get("space_type")
        )
        changed = dataset.create(replace=True)
        self.changed = self.changed or changed
        self.dest = dataset.name
        self.add(source, self.dest)
        self.clean_environment(data_sets=self.tmp_data_sets)


class XMITArchive(MVSArchive):
    def __init__(self, module):
        """Archive for XMIT.

        Parameters
        ----------
        module : AnsibleModule
            AnsibleModule to use.

        Attributes
        ----------
        xmit_log_data_set : str
            The name of the data set to store xmit log output.
        """
        super(XMITArchive, self).__init__(module)
        self.xmit_log_data_set = module.params.get("format").get("options").get("xmit_log_data_set")

    def add(self, src, archive):
        """Archive src into archive using TSO XMIT.

        Parameters
        ----------
        src : str
            Source of the archive.
        archive : str
            Destination archive.

        Raises
        ------
        fail_json
            An error occurred while executing 'TSO XMIT' to archive source.
        """
        log_option = "LOGDSNAME({0})".format(self.xmit_log_data_set) if self.xmit_log_data_set else "NOLOG"
        xmit_cmd = """
        PROFILE NOPREFIX
        XMIT A.B -
        FILE(SYSUT1) OUTFILE(SYSUT2) -
        {0} -
        """.format(log_option)
        dds = {"SYSUT1": "{0},shr".format(src), "SYSUT2": archive}
        rc, out, err = mvs_cmd.ikjeft01(cmd=xmit_cmd, authorized=True, dds=dds)
        if rc != 0:
            # self.get_error_hint handles the raw output of XMIT executed through TSO, contains different
            # error hints based on the abend code returned.
            error_hint = self.get_error_hint(out)
            self.module.fail_json(
                msg="An error occurred while executing 'TSO XMIT' to archive {0} into {1}.{2}".format(src, archive, error_hint),
                stdout=out,
                stderr=err,
                rc=rc,
            )
        self.archived = self.targets[:]
        return rc

    def archive_targets(self):
        """Adds MVS Datasets to the TSO XMIT Archive by creating a temporary dataset and dumping the source datasets into it.

        Raises
        ------
        fail_json
            To archive multiple source data sets, you must use option 'adrdssu=True'.
        """
        if self.adrdssu:
            source, changed = self._create_dest_data_set(
                type="seq",
                record_format="u",
                record_length=0,
                tmp_hlq=self.tmphlq,
                replace=True,
                space_primary=self.dest_data_set.get("space_primary"),
                space_type=self.dest_data_set.get("space_type"))
            self.dump_into_temp_ds(source)
            self.tmp_data_sets.append(source)
        else:
            # If we don't use a adrdssu container we cannot pack multiple data sets
            if len(self.sources) > 1:
                self.module.fail_json(
                    msg="To archive multiple source data sets, you must use option 'adrdssu=True'.")
            source = self.sources[0]
        # dest = self.create_dest_ds(self.dest)
        dataset = data_set.MVSDataSet(
            name=self.dest,
            data_set_type='seq',
            record_format='fb',
            record_length=XMIT_RECORD_LENGTH,
            space_primary=self.dest_data_set.get("space_primary"),
            space_type=self.dest_data_set.get("space_type")
        )
        changed = dataset.create(replace=True)
        self.changed = self.changed or changed
        self.changed = self.changed or changed
        self.dest = dataset.name
        self.add(source, self.dest)
        self.clean_environment(data_sets=self.tmp_data_sets)

    def get_error_hint(self, output):
        """Takes a raw TSO XMIT output and parses the abend code and return code to provide an
        appropriate error hint for the failure.
        If parsing is not possible then return an empty string.

        Parameters
        ----------
        output : str
            Raw TSO XMIT output returned from ikjeft01 when the command fails.

        Returns
        -------
        str
            Operation failed.
        str
            '', if IndexError.
        """
        error_messages = dict(D37={"00000004": "There appears to be a space issue. Ensure that there is adequate space and log data sets are not full."})

        sys_abend, reason_code, error_hint = "", "", ""
        find_abend = re.findall(r"ABEND CODE.*REASON", output)
        if find_abend:
            try:
                sys_abend = find_abend[0].split("ABEND CODE ")[1].split(" ")[0]
            except IndexError:
                return ""

        find_reason_code = re.findall(r"REASON CODE.*", output)
        if find_reason_code:
            try:
                reason_code = find_reason_code[0].split("REASON CODE ")[1].split(" ")[0]
            except IndexError:
                return ""

        msg = "Operation failed with abend code {0} and reason code {1}. {2}"
        if sys_abend in error_messages:
            if reason_code in error_messages[sys_abend]:
                error_hint = error_messages[sys_abend][reason_code]
        return msg.format(sys_abend, reason_code, error_hint)


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
        The file already exists. Use force flag to replace destination.
    """
    module = AnsibleModule(
        argument_spec=dict(
            src=dict(type='list', elements='str', required=True),
            dest=dict(type='str', required=True),
            exclude=dict(type='list', elements='str'),
            format=dict(
                type='dict',
                options=dict(
                    type=dict(
                        type='str',
                        default='gz',
                        choices=['bz2', 'gz', 'tar', 'zip', 'terse', 'xmit', 'pax'],
                        aliases=['name'],
                    ),
                    options=dict(
                        type='dict',
                        required=False,
                        options=dict(
                            spack=dict(
                                type='bool',
                                default=True,
                            ),
                            xmit_log_data_set=dict(
                                type='str',
                            ),
                            adrdssu=dict(
                                type='bool',
                                default=False,
                            )
                        ),
                    ),
                )
            ),
            group=dict(type='str'),
            mode=dict(type='str'),
            owner=dict(type='str'),
            remove=dict(type='bool', default=False),
            dest_data_set=dict(
                type='dict',
                required=False,
                options=dict(
                    name=dict(
                        type='str', required=False,
                    ),
                    type=dict(
                        type='str',
                        choices=['seq'],
                        required=False,
                        default="seq",
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
                    sms_storage_class=dict(type="str", required=False),
                    sms_data_class=dict(type="str", required=False),
                    sms_management_class=dict(type="str", required=False),
                )
            ),
            tmp_hlq=dict(type='str'),
            force=dict(type='bool', default=False),
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
            )
        ),
        supports_check_mode=True,
    )
    validate_dependencies(module)

    arg_defs = dict(
        src=dict(type='list', elements='str', required=True),
        dest=dict(type='str', required=True),
        exclude=dict(type='list', elements='str', default=[]),
        format=dict(
            type='dict',
            options=dict(
                type=dict(
                    type='str',
                    default='gz',
                    choices=['bz2', 'gz', 'tar', 'zip', 'terse', 'xmit', 'pax'],
                    aliases=['name'],
                ),
                options=dict(
                    type='dict',
                    required=False,
                    options=dict(
                        spack=dict(
                            type='bool',
                            required=False,
                            default=True,
                        ),
                        xmit_log_data_set=dict(
                            type='str',
                            required=False,
                        ),
                        adrdssu=dict(
                            type='bool',
                            default=False,
                        )
                    ),
                    default=dict(
                        spack=True,
                        xmit_log_data_set="",
                        adrdssu=False),
                ),
            ),
            default=dict(
                type="",
                options=dict(
                    spack=True,
                    xmit_log_data_set="",
                    adrdssu=False
                )
            ),
        ),
        group=dict(type='str'),
        mode=dict(type='str'),
        owner=dict(type='str'),
        remove=dict(type='bool', default=False),
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
                sms_storage_class=dict(arg_type="str", required=False),
                sms_data_class=dict(arg_type="str", required=False),
                sms_management_class=dict(arg_type="str", required=False),
            )
        ),
        tmp_hlq=dict(type='qualifier_or_empty', default=''),
        force=dict(type='bool', default=False),
        encoding=dict(
            type='dict',
            options={
                'from': dict(type='str'),
                'to': dict(type='str'),
                'skip_encoding': dict(type='list', elements='str', required=False),
            }
        )
    )

    result = dict(
        changed=False,
        original_message='',
        message=''
    )

    if module.check_mode:
        module.exit_json(**result)

    try:
        parser = better_arg_parser.BetterArgParser(arg_defs)
        parsed_args = parser.parse_args(module.params)
        module.params = parsed_args
    except ValueError as err:
        module.fail_json(msg="Parameter verification failed", stderr=str(err))

    # Initialize logging module
    module_verbosity_level = module._verbosity
    SingletonLogger().get_logger(module_verbosity_level)

    encoding = parsed_args.get("encoding")
    archive = get_archive_handler(module)

    if archive.dest_exists() and not archive.force:
        module.fail_json(msg="%s file exists. Use force flag to replace dest" % archive.dest)

    encoding_result = None
    archive.find_targets()
    if archive.targets_exist():
        # encoding the source if encoding is provided.
        if encoding:
            archive.encoding_targets()
            encoding_result = archive.encode_source()
            archive.result.update({
                "encoded": encoding_result.get("encoded", []),
                "failed_on_encoding": encoding_result.get("failed_on_encoding", []),
                "skipped_encoding_targets": encoding_result.get("skipped_encoding_targets", [])
            })
        archive.compute_dest_size()
        archive.archive_targets()
        if archive.remove:
            archive.remove_targets()
    if archive.dest_exists():
        if archive.dest_type() == "USS":
            archive.update_permissions()
        archive.changed = archive.is_different_from_original()
        # after successful archive revert the source encoding for all the files encoded.
        if encoding_result:
            archive.revert_encoding()
    archive.get_state()

    module.exit_json(**archive.result)


def main():
    run_module()


if __name__ == '__main__':
    main()
