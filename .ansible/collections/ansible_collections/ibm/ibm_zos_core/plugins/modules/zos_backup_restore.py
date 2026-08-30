#!/usr/bin/python
# -*- coding: utf-8 -*-

# Copyright (c) IBM Corporation 2020, 2026
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
module: zos_backup_restore
version_added: '1.3.0'
author: "Blake Becker (@blakeinate)"
short_description: Backup and restore data sets and volumes
description:
  - Create and restore from backups of data sets and volumes.
  - Data set backups are performed using logical dumps, volume backups are performed
    using physical dumps.
  - Backups are compressed using AMATERSE.
  - Backups are created by first dumping data sets with ADRDSSU, followed by compression with AMATERSE.
  - Restoration is performed by first decompressing an archive with AMATERSE, then restoring with ADRDSSU.
  - Since ADRDSSU and AMATERSE are used to create and restore backups,
    backups can be restored to systems where Ansible and ZOAU are not available.
    Conversely, dumps created with ADRDSSU and AMATERSE can be restored using this module.
options:
  access:
    description:
      - Specifies how the module will access data sets and z/OS UNIX files when
        performing a backup or restore operation.
    type: dict
    required: false
    suboptions:
      share:
        description:
          - Specifies that the module allow data set read access to other programs
            while backing up or restoring.
          - I(share) and C(full_volume) are mutually exclusive; you cannot use both.
          - Option I(share)is conditionally supported for I(operation=backup) or
            I(operation=restore).
          - When I(operation=backup), and source backup is a VSAM data set, the
            option is only supported for VSAM data sets which are not defined with
            VSAM SHAREOPTIONS (1,3) or (1,4).
            - When I(operation=restore), and restore target is a VSAM data set or
            PDSE data set, this option is not supported. Both data set types will
            be accessed exlusivly preventing reading or writing to the VSAM, PDSE,
            or PDSE members.
          - The SHAREOPTIONS for VSAM data sets.
          - (1) the data set can be shared by multiple programs for read-only
              processing, or a single program for read and write processing.
          - (2) the data set can be accessed by multiple programs for read-only
            processing, and can also be accessed by a program for write processing.
          - (3) the data set can be shared by multiple programs where each
            program is responsible for maintaining both read and write data integrity.
          - (4) the data set can be shared by multiple programs where each program is
            responsible for maintaining both read and write data integrity differing
            from (3) in that I/O buffers are updated for each request.
        type: bool
        required: false
        default: false
      auth:
        description:
          - I(auth=true) allows you to act as an administrator, where it will disable
            checking the current users privileges for z/OS UNIX files, data sets and
            catalogs.
          - This is option is supported both, I(operation=backup) and I(operation=restore).
          - If you are not authorized to use this option, the module ends with an
            error message.
          - Some authorization checking for data sets is unavoidable, when when I(auth)
            is specified because some checks are initiated by services and programs
            invoked by this module which can not be bypassed.
        type: bool
        required: false
        default: false
  operation:
    description:
      - Used to specify the operation to perform.
    required: True
    type: str
    choices:
      - backup
      - restore
  data_sets:
    description:
      - Determines which data sets to include in the backup.
    required: False
    type: dict
    suboptions:
      include:
        description:
          - When I(operation=backup), specifies a list of data sets or data set patterns
            to include in the backup.
          - When I(operation=backup) GDS relative names are supported.
          - When I(operation=restore), specifies a list of data sets or data set patterns
            to include when restoring from a backup.
          - The single asterisk, C(*), is used in place of exactly one qualifier.
            In addition, it can be used to indicate to DFSMSdss that only part of a
            qualifier has been specified.
          - When used with other qualifiers, the double asterisk, C(**),
            indicates either the nonexistence of leading, trailing,
            or middle qualifiers, or the fact that they play no role in the selection process.
          - Two asterisks are the maximum permissible in a qualifier.
            If there are two asterisks in a qualifier, they must be the first and last characters.
          - A question mark C(?) or percent sign C(%) matches a single character.
        type: raw
        required: False
      exclude:
        description:
          - When I(operation=backup), specifies a list of data sets or data set patterns
            to exclude from the backup.
          - When I(operation=backup) GDS relative names are supported.
          - When I(operation=restore), specifies a list of data sets or data set patterns
            to exclude when restoring from a backup.
          - The single asterisk, C(*), is used in place of exactly one qualifier.
            In addition, it can be used to indicate that only part of a
            qualifier has been specified."
          - When used with other qualifiers, the double asterisk, C(**),
            indicates either the nonexistence of leading, trailing,
            or middle qualifiers, or the fact that they play no role in the selection process.
          - Two asterisks are the maximum permissible in a qualifier.
            If there are two asterisks in a qualifier, they must be the first and last characters.
          - A question mark C(?) or percent sign C(%) matches a single character.
        type: raw
        required: False
  volume:
    description:
      - This applies to both data set restores and volume restores.
      - When I(operation=backup) and I(data_sets) are provided, specifies the volume
        that contains the data sets to backup.
      - When I(operation=restore), specifies the volume the backup should be restored to.
      - I(volume) is required when restoring a full volume backup.
    type: str
    required: False
  full_volume:
    description:
      - When I(operation=backup) and I(full_volume=True), specifies that the entire volume
        provided to I(volume) should be backed up.
      - When I(operation=restore) and I(full_volume=True), specifies that the volume should be
        restored (default is dataset).
      - I(volume) must be provided when I(full_volume=True).
    type: bool
    default: False
    required: False
  temp_volume:
    description:
      - Specifies a particular volume on which the temporary data sets should be
        created during the backup and restore process.
      - When I(operation=backup) and I(backup_name) is a data set, specifies the
        volume the backup should be placed in.
      - When backing up a data set from one volume to another, the operation
        will fail if temporary data sets are created on the same volume as the volume containing
        the source data set(s). To avoid this, set I(temp_volume) to a different volume
        than the source volume.
    type: str
    required: False
    aliases:
      - dest_volume
  backup_name:
    description:
      - When I(operation=backup), the destination data set or UNIX file to hold the backup.
      - When I(operation=restore), the destination data set or UNIX file backup to restore.
      - There are no enforced conventions for backup names.
        However, using a common extension like C(.dzp) for UNIX files and C(.DZP) for data sets will
        improve readability.
      - GDS relative names are supported when I(operation=restore).
    type: str
    required: True
  recover:
    description:
      - When I(recover=true) and I(operation=backup) then potentially recoverable errors will be ignored.
    type: bool
    default: False
  overwrite:
    description:
      - When I(operation=backup), specifies if an existing data set or UNIX file matching
        I(backup_name) should be deleted.
      - When I(operation=restore), specifies if the module should overwrite existing data sets
        with matching name on the target device.
    type: bool
    default: False
  compress:
    description:
      - When I(operation=backup), enables compression of partitioned data sets using system-level
        compression features. If supported, this may utilize zEDC hardware compression.
      - This option can reduce the size of the temporary dataset generated during backup operations
        either before the AMATERSE step when I(terse) is True or the resulting backup
        when I(terse) is False.
    type: bool
    default: False
  terse:
    description:
      - When I(operation=backup), executes an AMATERSE step to compress and pack the temporary data set
        for the backup. This creates a backup with a format suitable for transferring off-platform.
      - If I(operation=backup) and if I(dataset=False) then option I(terse) must be True.
    type: bool
    default: True
  sms:
    description:
      - Specifies how System Managed Storage (SMS) interacts with the storage class
        and management class when either backup or restore operations are occurring.
      - Storage class contains performance and availability attributes related to the storage occupied by the data set.
        A data set that has a storage class assigned to it is defined as an 'SMS-managed' data set.
      - Management class contains the data set attributes related to the migration and backup of the data set and the
        expiration date of the data set. A management class can be assigned only to a data set that also has a
        storage class assigned.
    type: dict
    required: false
    suboptions:
      storage_class:
        description:
          - When I(operation=restore), specifies the storage class to use. The storage class will
            also be used for temporary data sets created during restore process.
          - When I(operation=backup), specifies the storage class to use for temporary data sets
            created during backup process.
          - If neither of I(sms_storage_class) or I(sms_management_class) are specified, the z/OS
            system's Automatic Class Selection (ACS) routines will be used.
        type: str
        required: False
      management_class:
        description:
          - When I(operation=restore), specifies the management class to use. The management class
            will also be used for temporary data sets created during restore process.
          - When I(operation=backup), specifies the management class to use for temporary data sets
            created during backup process.
          - If neither of I(sms_storage_class) or I(sms_management_class) are specified, the z/OS
            system's Automatic Class Selection (ACS) routines will be used.
        type: str
        required: False
      disable_automatic_class:
        description:
          - Specifies that the automatic class selection (ACS) routines will not be
            used to determine the target data set class names for the provided list.
          - The list must contain fully or partially qualified data set names.
          - To include all selected data sets, "**" in a list.
          - You must have READ access to RACF FACILITY class profile
            `STGADMIN.ADR.RESTORE.BYPASSACS` to use this option.
        type: list
        elements: str
        required: false
        default: []
      disable_automatic_storage_class:
        description:
          - Specifies that automatic class selection (ACS) routines will not be used
            to determine the source data set storage class.
          - Enabling I(disable_automatic_storage_class) ensures ACS is null.
          - I(storage_class) and I(disable_automatic_storage_class) are mutually exclusive; you cannot use both.
          - The combination of I(disable_automatic_storage_class) and C(disable_automatic_class=[dsn,dsn1,...])
            ensures the selected data sets will not be SMS-managed.
        type: bool
        required: false
        default: false
      disable_automatic_management_class:
        description:
          - Specifies that automatic class selection (ACS) routines will not be used
            to determine the source data set management class.
          - Enabling I(disable_automatic_storage_class) ensures ACS is null.
          - I(management_class) and I(disable_automatic_management_class) are mutually exclusive; you cannot use both.
        type: bool
        required: false
        default: false
  space:
    description:
      - If I(operation=backup), specifies the amount of space to allocate for the backup.
        Please note that even when backing up to a UNIX file, backup contents will be temporarily
        held in a data set.
      - If I(operation=restore), specifies the amount of space to allocate for data sets temporarily
        created during the restore process.
      - The unit of space used is set using I(space_type).
      - When I(full_volume=True), I(space) defaults to C(1), otherwise default is C(25)
    type: int
    required: false
    aliases:
      - size
  space_type:
    description:
      - The unit of measurement to use when defining data set space.
      - Valid units of size are C(k), C(m), C(g), C(cyl), and C(trk).
      - When I(full_volume=True), I(space_type) defaults to C(g), otherwise default is C(m)
    type: str
    default: m
    choices:
      - k
      - m
      - g
      - cyl
      - trk
    required: false
    aliases:
      - unit
  tmp_hlq:
    description:
      - Override the default high level qualifier (HLQ) for temporary
        data sets used in the module's operation.
      - If I(tmp_hlq) is set, this value will be applied to all temporary
        data sets.
      - If I(tmp_hlq) is not set, the value will be the username who submits
        the ansible task, this is the default behavior. If the username can
        not be identified, the value C(TMPHLQ) is used.
    required: false
    type: str
  index:
    description:
      - When C(operation=backup) specifies that for any VSAM cluster backup, the backup must also contain
        all the associated alternate index (AIX) clusters and paths.
      - When C(operation=restore) specifies that for any VSAM cluster dumped with the SPHERE keyword,
        the module must also restore all associated AIX clusters and paths.
      - The alternate index is a VSAM function that allows logical records of a
        KSDS or ESDS to be accessed sequentially and directly by more than one key
        field. The cluster that has the data is called the base cluster. An
        alternate index cluster is then built from the base cluster.
    type: bool
    required: false
    default: false
  output:
    description:
      - Specifies how a backup will be restored to the filesystem when I(operation=restore).
      - Option C(output) does not perform any operations when I(operation=backup), it will be ignored.
    type: dict
    required: false
    suboptions:
      hlq:
        description:
          - Specifies the new HLQ to use for the data sets being restored.
          - If I(hlq) is not provided, the original HLQ remains unchanged.
        type: str
        required: false

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
    - It is the playbook author or user's responsibility to ensure they have
      appropriate authority to the RACF FACILITY resource class. A user is
      described as the remote user, configured to run either the playbook or
      playbook tasks, who can also obtain escalated privileges to execute as
      root or another user.
    - When using this module, if the RACF FACILITY class
      profile B(STGADMIN.ADR.DUMP.TOLERATE.ENQF) is active, you must
      have READ access authority to use the module option I(recover=true).
      If the RACF FACILITY class checking is not set up, any user can use
      the module option without access to the class.
    - If your system uses a different security product, consult that product's
      documentation to configure the required security classes.
    - When backing up a data set from one volume to another, the operation will
      fail if temporary data sets are created on the same volume as the volume
      containing the source data set(s). To avoid this, use the I(temp_volume)
      option to specify a different volume than the source volume for temporary
      data sets.
"""

EXAMPLES = r"""
- name: Backup all data sets matching the pattern USER.** to data set MY.BACKUP.DZP
  zos_backup_restore:
    operation: backup
    data_sets:
      include: user.**
    backup_name: MY.BACKUP.DZP

- name: Backup all data sets matching the patterns USER.** or PRIVATE.TEST.*
    excluding data sets matching the pattern USER.PRIVATE.* to data set MY.BACKUP.DZP
  zos_backup_restore:
    operation: backup
    data_sets:
      include:
        - user.**
        - private.test.*
      exclude: user.private.*
    backup_name: MY.BACKUP.DZP

- name: Backup a list of GDDs to data set my.backup.dzp
  zos_backup_restore:
    operation: backup
    data_sets:
      include:
        - user.gdg(-1)
        - user.gdg(0)
    backup_name: my.backup.dzp

- name: Backup datasets using compress
  zos_backup_restore:
    operation: backup
    compress: true
    terse: true
    data_sets:
      include: someds.name.here
    backup_name: my.backup.dzp

- name: Backup all datasets matching the pattern USER.** to UNIX file /tmp/temp_backup.dzp, ignore recoverable errors.
  zos_backup_restore:
    operation: backup
    data_sets:
      include: user.**
    backup_name: /tmp/temp_backup.dzp
    recover: true

- name: Backup all datasets matching the pattern USER.** to data set MY.BACKUP.DZP,
    allocate 100MB for data sets used in backup process.
  zos_backup_restore:
    operation: backup
    data_sets:
      include: user.**
    backup_name: MY.BACKUP.DZP
    space: 100
    space_type: m

- name:
    Backup all datasets matching the pattern USER.** that are present on the volume MYVOL1 to data set MY.BACKUP.DZP,
    allocate 100MB for data sets used in the backup process.
  zos_backup_restore:
    operation: backup
    data_sets:
      include: user.**
    volume: MYVOL1
    backup_name: MY.BACKUP.DZP
    space: 100
    space_type: m

- name: Backup an entire volume, MYVOL1, to the UNIX file /tmp/temp_backup.dzp,
    allocate 1GB for data sets used in backup process.
  zos_backup_restore:
    operation: backup
    backup_name: /tmp/temp_backup.dzp
    volume: MYVOL1
    full_volume: true
    space: 1
    space_type: g

- name: Restore data sets from a backup stored in the UNIX file /tmp/temp_backup.dzp.
    Restore the data sets with the original high level qualifiers.
  zos_backup_restore:
    operation: restore
    backup_name: /tmp/temp_backup.dzp

- name: Restore data sets from backup stored in the UNIX file /tmp/temp_backup.dzp.
    Only restore data sets whose last, or only qualifier is TEST.
    Use MYHLQ as the new HLQ for restored data sets.
  zos_backup_restore:
    operation: restore
    data_sets:
      include: "**.TEST"
    backup_name: /tmp/temp_backup.dzp
    hlq: MYHLQ

- name: Restore data sets from backup stored in the UNIX file /tmp/temp_backup.dzp.
    Only restore data sets whose last, or only qualifier is TEST.
    Use MYHLQ as the new HLQ for restored data sets. Restore data sets to volume MYVOL2.
  zos_backup_restore:
    operation: restore
    data_sets:
      include: "**.TEST"
    volume: MYVOL2
    backup_name: /tmp/temp_backup.dzp
    hlq: MYHLQ

- name: Restore data sets from backup stored in the data set MY.BACKUP.DZP.
    Use MYHLQ as the new HLQ for restored data sets.
  zos_backup_restore:
    operation: restore
    backup_name: MY.BACKUP.DZP
    hlq: MYHLQ

- name: Restore volume from backup stored in the data set MY.BACKUP.DZP.
    Restore to volume MYVOL2.
  zos_backup_restore:
    operation: restore
    volume: MYVOL2
    full_volume: true
    backup_name: MY.BACKUP.DZP
    space: 1
    space_type: g

- name: Restore data sets from backup stored in the UNIX file /tmp/temp_backup.dzp.
    Specify DB2SMS10 for the SMS storage and management classes to use for the restored
    data sets.
  zos_backup_restore:
    operation: restore
    volume: MYVOL2
    backup_name: /tmp/temp_backup.dzp
    sms:
      storage_class: DB2SMS10
      management_class: DB2SMS10

- name: Restore data sets from backup stored in the UNIX file /tmp/temp_backup.dzp.
    Disable for all datasets SMS storage and management classes data sets.
  zos_backup_restore:
    operation: restore
    volume: MYVOL2
    backup_name: /tmp/temp_backup.dzp
    sms:
      disable_automatic_class:
        - "**"
      disable_automatic_storage_class: true
      disable_automatic_management_class: true

- name: Restore data sets from backup stored in the MVS file MY.BACKUP.DZP
    Disable for al some datasets SMS storage and management classes data sets.
  zos_backup_restore:
    operation: restore
    volume: MYVOL2
    backup_name: MY.BACKUP.DZP
    sms:
      disable_automatic_class:
        - "ANSIBLE.TEST.**"
        - "**.ONE.**"
      disable_automatic_storage_class: true
      disable_automatic_management_class: true

- name: Backup all data sets matching the pattern USER.VSAM.** to z/OS UNIX
    file /tmp/temp_backup.dzp and ensure the VSAM alternate index are preserved.
  zos_backup_restore:
    operation: backup
    data_sets:
      include: user.vsam.**
    backup_name: /tmp/temp_backup.dzp
    index: true

- name: Restore data sets from backup stored in the UNIX file /tmp/temp_backup.dzp
    whether they exist or not and do so as authorized disabling any security checks.
  zos_backup_restore:
    operation: restore
    backup_name: /tmp/temp_backup.dzp
    access:
      auth: true
      share: true
"""

RETURN = r"""
changed:
  description:
    - Indicates if the operation made changes.
    - C(true) when backup/restore was successful, C(false) otherwise.
  returned: always
  type: bool
  sample: true
backup_name:
  description:
    - The USS file name or data set name that was used as a backup.
    - Matches the I(backup_name) parameter provided as input.
  returned: always
  type: str
  sample: "/u/oeusr03/my_backup.dzp"
message:
  description:
    - Returns any important messages about the modules execution, if any.
  returned: always
  type: str
  sample: ""
"""

import traceback
from os import path
from re import IGNORECASE, match, search

from ansible.module_utils.basic import AnsibleModule
from ansible_collections.ibm.ibm_zos_core.plugins.module_utils.better_arg_parser import \
    BetterArgParser
from ansible_collections.ibm.ibm_zos_core.plugins.module_utils.data_set import \
    DataSet
from ansible_collections.ibm.ibm_zos_core.plugins.module_utils.import_handler import \
    ZOAUImportError
from ansible_collections.ibm.ibm_zos_core.plugins.module_utils.dependency_checker import (
    validate_dependencies,
)
from ansible_collections.ibm.ibm_zos_core.plugins.module_utils.log import SingletonLogger

try:
    from zoautil_py import datasets
    from zoautil_py import exceptions as zoau_exceptions
except ImportError:
    datasets = ZOAUImportError(traceback.format_exc())
    zoau_exceptions = ZOAUImportError(traceback.format_exc())


def main():
    """Run the zos_backup_restore module core functions.

    Raises
    ------
    fail_json
        Any error ocurred during execution.
    """
    result = dict(changed=False, message="", backup_name="")
    module_args = dict(
        access=dict(
            type='dict',
            required=False,
            options=dict(
                share=dict(type='bool', default=False),
                auth=dict(type='bool', default=False)
            )
        ),
        operation=dict(type="str", required=True, choices=["backup", "restore"]),
        data_sets=dict(
            required=False,
            type="dict",
            options=dict(
                include=dict(type="raw", required=False),
                exclude=dict(type="raw", required=False),
            ),
        ),
        space=dict(type="int", required=False, aliases=["size"]),
        space_type=dict(type="str", required=False, aliases=["unit"], choices=["k", "m", "g", "cyl", "trk"], default="m"),
        volume=dict(type="str", required=False),
        full_volume=dict(type="bool", default=False),
        temp_volume=dict(type="str", required=False, aliases=["dest_volume"]),
        backup_name=dict(type="str", required=True),
        recover=dict(type="bool", default=False),
        overwrite=dict(type="bool", default=False),
        compress=dict(type="bool", default=False),
        terse=dict(type="bool", default=True),
        sms=dict(
            type='dict',
            required=False,
            options=dict(
                storage_class=dict(type="str", required=False),
                management_class=dict(type="str", required=False),
                disable_automatic_class=dict(type="list", elements="str", required=False, default=[]),
                disable_automatic_storage_class=dict(type="bool", required=False, default=False),
                disable_automatic_management_class=dict(type="bool", required=False, default=False),
            )
        ),
        output=dict(
            type='dict',
            required=False,
            options=dict(
                hlq=dict(type="str", required=False),
            )
        ),
        tmp_hlq=dict(type="str", required=False),
        # 2.0 redesign extra values for ADRDSSU keywords
        index=dict(type="bool", required=False, default=False),
    )

    module = AnsibleModule(argument_spec=module_args, supports_check_mode=False)

    validate_dependencies(module)
    try:
        params = parse_and_validate_args(module.params)

        # Initialize logging module
        module_verbosity_level = module._verbosity
        SingletonLogger().get_logger(module_verbosity_level)

        operation = params.get("operation")
        data_sets = params.get("data_sets", {})
        space = params.get("space")
        space_type = params.get("space_type", "m")
        volume = params.get("volume")
        full_volume = params.get("full_volume")
        temp_volume = params.get("temp_volume")
        backup_name = params.get("backup_name")
        recover = params.get("recover")
        overwrite = params.get("overwrite")
        compress = params.get("compress")
        terse = params.get("terse")
        sms = params.get("sms")
        output = params.get("output")
        tmp_hlq = params.get("tmp_hlq")
        sphere = params.get("index")
        access = params.get('access')

        if sms and bool(sms.get("storage_class")) and sms.get("disable_automatic_storage_class"):
            module.fail_json(msg="storage_class and disable_automatic_storage_class are mutually exclusive, only one can be use by operation.")

        if sms and bool(sms.get("management_class")) and sms.get("disable_automatic_management_class"):
            module.fail_json(msg="management_class and disable_automatic_management_class are mutually exclusive, only one can be use by operation.")

        if access and access.get("share") and full_volume:
            module.fail_json(msg="access.share cannot be used with full_volume. These options are mutually exclusive.")

        if operation == "backup":
            backup(
                backup_name=backup_name,
                include_data_sets=resolve_gds_name_if_any(data_sets.get("include")),
                exclude_data_sets=resolve_gds_name_if_any(data_sets.get("exclude")),
                volume=volume,
                full_volume=full_volume,
                temp_volume=temp_volume,
                overwrite=overwrite,
                compress=compress,
                terse=terse,
                recover=recover,
                space=space,
                space_type=space_type,
                sms=sms,
                tmp_hlq=tmp_hlq,
                sphere=sphere,
                access=access,
            )
        else:
            restore(
                backup_name=backup_name,
                include_data_sets=data_sets.get("include"),
                exclude_data_sets=data_sets.get("exclude"),
                volume=volume,
                full_volume=full_volume,
                temp_volume=temp_volume,
                overwrite=overwrite,
                recover=recover,
                output=output,
                space=space,
                space_type=space_type,
                sms=sms,
                tmp_hlq=tmp_hlq,
                sphere=sphere,
                access=access,
            )
        result["backup_name"] = backup_name
        result["changed"] = True

    except Exception as e:
        module.fail_json(msg=repr(e), **result)
    module.exit_json(**result)


def resolve_gds_name_if_any(data_set_list):
    """Resolve all gds names in a list, if no gds relative name is found then
    the original name will be kept.
    Parameters
    ----------
    data_set_list : list
        List of data set names.

    Returns
    -------
    list
        List of data set names with resolved gds names.
    """
    if isinstance(data_set_list, list):
        for index, name in enumerate(data_set_list):
            if DataSet.is_gds_relative_name(name):
                data_set_list[index] = DataSet.resolve_gds_absolute_name(name)
    return data_set_list


def parse_and_validate_args(params):
    """Parse and validate arguments to be used by remainder of module.

    Parameters
    ----------
    params : dict
        The params as returned from AnsibleModule instantiation.

    Returns
    -------
    dict
        The updated params after additional parsing and validation.
    """
    arg_defs = dict(
        access=dict(
            type='dict',
            required=False,
            options=dict(
                share=dict(type='bool', default=False),
                auth=dict(type='bool', default=False)
            )
        ),
        operation=dict(type="str", required=True, choices=["backup", "restore"]),
        data_sets=dict(
            required=False,
            type="dict",
            options=dict(
                include=dict(type=data_set_pattern_type, required=False),
                exclude=dict(type=data_set_pattern_type, required=False),
            ),
        ),
        space=dict(
            type=space_type,
            required=False,
            aliases=["size"],
            dependencies=["full_volume"],
        ),
        space_type=dict(
            type=space_type_type,
            required=False,
            aliases=["unit"],
            dependencies=["full_volume"],
        ),
        volume=dict(type="volume", required=False, dependencies=["data_sets"]),
        full_volume=dict(type=full_volume_type, default=False, dependencies=["volume"]),
        temp_volume=dict(type="volume", required=False, aliases=["dest_volume"]),
        backup_name=dict(type=backup_name_type, required=False),
        recover=dict(type="bool", default=False),
        overwrite=dict(type="bool", default=False),
        compress=dict(type="bool", default=False),
        terse=dict(type="bool", default=True),
        sms=dict(
            type='dict',
            required=False,
            options=dict(
                storage_class=dict(type=sms_type, required=False),
                management_class=dict(type=sms_type, required=False),
                disable_automatic_class=dict(type="list", elements="str", required=False, default=[]),
                disable_automatic_storage_class=dict(type="bool", required=False),
                disable_automatic_management_class=dict(type="bool", required=False),
            )
        ),
        output=dict(
            type='dict',
            required=False,
            options=dict(
                hlq=dict(type=hlq_type, required=False),
            )
        ),
        tmp_hlq=dict(type=hlq_type, required=False),
        index=dict(type="bool", required=False, default=False),
    )

    parsed_args = BetterArgParser(arg_defs).parse_args(params)
    parsed_args = {
        key: value for key, value in parsed_args.items() if value is not None
    }
    return parsed_args


def backup(
    backup_name,
    include_data_sets,
    exclude_data_sets,
    volume,
    full_volume,
    temp_volume,
    overwrite,
    compress,
    terse,
    recover,
    space,
    space_type,
    sms,
    tmp_hlq,
    sphere,
    access,
):
    """Backup data sets or a volume to a new data set or unix file.

    Parameters
    ----------
    backup_name : str
        The data set or UNIX path to place the backup.
    compress : bool
        Compress the dataset or file produced by ADRDSSU before taking backup.
    include_data_sets : list
        A list of data set patterns to include in the backup.
    exclude_data_sets : list
        A list of data set patterns to exclude from the backup.
    volume : str
        The volume that contains the data sets to backup.
    full_volume : bool
        Specifies if a backup will be made of the entire volume.
    temp_volume : bool
        Specifies the volume that should be used to store temporary files.
    terse : bool
        Uses AMATERSE to compress and pack the dump generated by ADRDSSU.
    overwrite : bool
        Specifies if existing data set or UNIX file matching I(backup_name) should be deleted.
    recover : bool
        Specifies if potentially recoverable errors should be ignored.
    space : int
        Specifies the amount of space to allocate for the backup.
    space_type : str
        The unit of measurement to use when defining data set space.
    sms : dict
        Specifies how System Managed Storage (SMS) interacts with the storage class.
    tmp_hlq : str
        Specifies the tmp hlq to temporary datasets.
    sphere : dict
        Specifies ADRDSSU keywords that is passed directly to the dunzip utility.
    access : dict
        Specifies keywords for share and administration permission.
    """
    args = locals()
    zoau_args = to_dzip_args(**args)
    datasets.dzip(**zoau_args)


def restore(
    backup_name,
    include_data_sets,
    exclude_data_sets,
    volume,
    full_volume,
    temp_volume,
    overwrite,
    recover,
    output,
    space,
    space_type,
    sms,
    tmp_hlq,
    sphere,
    access,
):
    """Restore data sets or a volume from the backup.

    Parameters
    ----------
    backup_name : str
        The data set or UNIX path containing the backup.
    include_data_sets : list
        A list of data set patterns to include in the restore
        that are present in the backup.
    exclude_data_sets : list
        A list of data set patterns to exclude from the restore
        that are present in the backup.
    volume : str
        The volume that contains the data sets to backup.
    full_volume : bool
        Specifies if a backup will be made of the entire volume.
    temp_volume : bool
        Specifies the volume that should be used to store temporary files.
    overwrite : bool
        Specifies if module should overwrite existing data sets with
        matching name on the target device.
    recover : bool
        Specifies if potentially recoverable errors should be ignored.
    hlq : str
        Specifies the new HLQ to use for the data sets being restored.
    space : int
        Specifies the amount of space to allocate for data sets temporarily
        created during the restore process.
    space_type : str
        The unit of measurement to use when defining data set space.
    sms : dict
        Specifies how System Managed Storage (SMS) interacts with the storage class.
    tmp_hlq : str
        Specifies the tmp hlq to temporary datasets.
    sphere : dict
        Specifies ADRDSSU keywords that is passed directly to the dunzip utility.
    access : dict
        Specifies keywords for share and administration permission.

    Raises
    ------
    ZOAUException
        When any exception is raised during the data set allocation.
    """
    args = locals()
    zoau_args = to_dunzip_args(**args)
    dunzip_output = ""
    try:
        rc = datasets.dunzip(**zoau_args)
    except zoau_exceptions.ZOAUException as dunzip_exception:
        dunzip_output = dunzip_exception.response.stdout_response
        dunzip_output = dunzip_output + dunzip_exception.response.stderr_response
        rc = get_real_rc(dunzip_output)
    failed = False
    if rc > 0 and rc <= 4:
        if recover is not True:
            failed = True
    elif rc > 4:
        failed = True
    if failed:
        raise zoau_exceptions.ZOAUException(
            "{0}, RC={1}".format(dunzip_output, rc)
        )


def set_adrdssu_keywords(sphere, sms=None, access=None):
    """Set the values for special keywords, dunzip use key value for most special words.

    Parameters
    ----------
        sms : dict
          Dictionary of key value of management an storage class.
        sphere : bool
          Value if sphere will be use on dictionary for VSAM.
        access : dict
          Dictionary of key values for management classes.
    Returns
    -------
        keywords : dict
          Dictionary with key value paris.
    """
    keywords = {}

    if sphere:
        keywords.update(sphere=None)

    if sms:
        if sms.get("disable_automatic_management_class"):
            sms["management_class"] = "NULLMGMTCLAS"

        if sms.get("disable_automatic_storage_class"):
            sms["storage_class"] = "NULLSTORCLAS"

        if len(sms.get("disable_automatic_class")) > 0:
            bypassacs = set_bypassacs_str(sms.get("disable_automatic_class"))
            keywords.update(bypass_acs=bypassacs)

    if access:
        if access.get("auth"):
            keywords.update(ADMINISTRATOR="ADMINistrator")

        if access.get("share"):
            keywords.update(SHARE="SHAre")

    return keywords


def set_bypassacs_str(ds):
    """_summary_

    Parameters
    ----------
        ds : list
          List of datasets to be use.

    Returns
    -------
        str : Datasets on str format.
    """
    datasets = ""
    if len(ds) > 0:
        for dataset in ds:
            if dataset == "**":
                return "**"
            datasets += f"{datasets} "

    return datasets


def get_real_rc(dunzip_output):
    """Parse out the final RC from MVS program output.

    Parameters
    ----------
    output : str
        The MVS program output.

    Returns
    -------
    int
        The true program RC.
    """
    true_rc = None
    match = search(
        r"HIGHEST\sRETURN\sCODE\sIS\s([0-9]+)",
        dunzip_output,
    )
    if match:
        true_rc = int(match.group(1))
    return true_rc


def data_set_pattern_type(contents, dependencies):
    """Validates provided data set patterns.

    Parameters
    ----------
    contents : Union[str, list[str]
        One or more data set patterns.
    dependencies : dict
        Any dependent arguments.

    Returns
    -------
    Union[str]
        A list of uppercase data set patterns.

    Raises
    ------
    ValueError
        When provided argument is not a string or a list.
    ValueError
        When provided argument is an invalid data set pattern.
    """
    if contents is None:
        return None
    if isinstance(contents, str):
        contents = [contents]
    elif not isinstance(contents, list):
        raise ValueError(
            "Invalid argument for data set pattern, expected string or list."
        )
    for pattern in contents:
        if not match(
            r"^(?:(?:[A-Za-z$#@\?\*]{1}[A-Za-z0-9$#@\-\?\*]{0,7})(?:[.]{1})){1,21}[A-Za-z$#@\*\?]{1}[A-Za-z0-9$#@\-\*\?]{0,7}(?:\(([-+]?[0-9]+)\)){0,1}$",
            str(pattern),
            IGNORECASE,
        ):
            raise ValueError(
                "Invalid argument {0} for data set pattern.".format(contents)
            )
    return [x.upper() for x in contents]


def hlq_type(contents, dependencies):
    """Validates provided HLQ is valid and is not specified for a backup operation.

    Parameters
    ----------
    contents : str
        The HLQ to use.
    dependencies : dict
        Any dependent arguments.

    Returns
    -------
    str
        The HLQ to use.

    Raises
    ------
    ValueError
        When operation is restore and HLQ is provided.
    ValueError
        When an invalid HLQ is provided.
    """
    if contents is None:
        return None
    if dependencies.get("operation") == "backup":
        raise ValueError("hlq_type is only valid when operation=restore.")
    if not match(r"^(?:[A-Z$#@]{1}[A-Z0-9$#@-]{0,7})$", contents, IGNORECASE):
        raise ValueError("Invalid argument {0} for hlq_type.".format(contents))
    return contents


def sms_type(contents, dependencies):
    """Validates the SMS class provided matches a valid format.

    Parameters
    ----------
    contents : str
        The SMS class name.
    dependencies : dict
        Any dependent arguments.

    Returns
    -------
    str
        The uppercase SMS class name.

    Raises
    ------
    ValueError
        When invalid argument provided for SMS class.
    """
    if contents is None:
        return None
    if not match(r"^[A-Z\$\*\@\#\%]{1}[A-Z0-9\$\*\@\#\%]{0,7}$", contents, IGNORECASE):
        raise ValueError("Invalid argument {0} for SMS class.".format(contents))
    return str(contents).upper()


def space_type(contents, dependencies):
    """Validates amount of space provided.

    Parameters
    ----------
    contents : str
        The amount of space.
    dependencies : dict
        Any dependent arguments.

    Returns
    -------
    int
        The amount of space.
    """
    if contents is None:
        if dependencies.get("full_volume"):
            return 1
        else:
            return 25
    return contents


def space_type_type(contents, dependencies):
    """Validates provided data set unit of space.
    Returns the unit of space.

    Parameters
    ----------
    contents : str
        The space type to use.
    dependencies : dict
        Any dependent arguments.

    Returns
    -------
    str
        The unit of space.

    Raises
    ------
    ValueError
        When an invalid space unit is provided.
    """
    if contents is None:
        if dependencies.get("full_volume"):
            return "g"
        else:
            return "m"
    if not match(r"^(m|g|k|trk|cyl)$", contents, IGNORECASE):
        raise ValueError(
            'Value {0} is invalid for space_type argument. Valid space types are "k", "m", "g", "trk" or "cyl".'.format(
                contents
            )
        )
    return contents


def backup_name_type(contents, dependencies):
    """Validates provided backup name.

    Parameters
    ----------
        contents : str
            The backup name to use
        dependencies : dict
            Any dependent arguments

    Returns
    -------
        str
            The backup name to use

    Raises
    ------
        ValueError
            When an invalid backup name is provided
    """
    if contents is None:
        return None
    if not match(
        r"^(?:(?:[A-Za-z$#@\?\*]{1}[A-Za-z0-9$#@\-\?\*]{0,7})(?:[.]{1})){1,21}[A-Za-z$#@\*\?]{1}[A-Za-z0-9$#@\-\*\?]{0,7}(?:\(([-+]?[0-9]+)\)){0,1}$",
        str(contents),
        IGNORECASE,
    ):
        if not path.isabs(str(contents)):
            raise ValueError(
                'Invalid argument "{0}" for type "data_set" or "path".'.format(contents)
            )
    return str(contents)


def full_volume_type(contents, dependencies):
    """Validates dependent arguments are also specified for full_volume argument.

    Parameters
    ----------
    contents : bool
        Whether we are making a full volume backup or not.
    dependencies : dict
        Any dependent arguments.

    Returns
    -------
    bool
        Whether we are making a full volume backup or not.

    Raises
    ------
    ValueError
        When volume argument is not provided.
    """
    if contents is True and dependencies.get("volume") is None:
        raise ValueError("full_volume=True is only valid when volume is specified.")
    return contents


def to_dzip_args(**kwargs):
    """API adapter for ZOAU dzip method.

    Returns
    -------
    dict
        The arguments for ZOAU dzip method translated from module arguments.
    """
    zoau_args = {}
    if kwargs.get("backup_name"):
        zoau_args["file"] = kwargs.get("backup_name")
        if not kwargs.get("backup_name").startswith("/"):
            zoau_args["dataset"] = True

    if kwargs.get("include_data_sets"):
        zoau_args["target"] = ",".join(kwargs.get("include_data_sets"))

    if kwargs.get("volume"):
        if kwargs.get("full_volume"):
            zoau_args["volume"] = True
            zoau_args["target"] = kwargs.get("volume")
        else:
            zoau_args["src_volume"] = kwargs.get("volume")

    if kwargs.get("temp_volume"):
        zoau_args["dest_volume"] = kwargs.get("temp_volume")

    if kwargs.get("exclude_data_sets"):
        zoau_args["exclude"] = ",".join(kwargs.get("exclude_data_sets"))

    if kwargs.get("recover"):
        zoau_args["force"] = kwargs.get("recover")

    if kwargs.get("overwrite"):
        zoau_args["overwrite"] = kwargs.get("overwrite")

    if kwargs.get("compress") is not None:
        zoau_args["compress"] = kwargs.get("compress")

    if kwargs.get("terse") is not None:
        zoau_args["terse"] = kwargs.get("terse")

    if kwargs.get("space"):
        size = str(kwargs.get("space"))
        if kwargs.get("space_type"):
            size += kwargs.get("space_type")
        zoau_args["size"] = size

    if kwargs.get("tmp_hlq"):
        zoau_args["tmphlq"] = str(kwargs.get("tmp_hlq"))

    sms = kwargs.get("sms")
    keywords = set_adrdssu_keywords(sphere=kwargs.get("sphere"), sms=sms)

    if sms:
        if sms.get("storage_class"):
            zoau_args["storage_class_name"] = sms.get("storage_class")

        if sms.get("management_class"):
            zoau_args["management_class_name"] = sms.get("management_class")

    if keywords:
        zoau_args["keywords"] = keywords

    return zoau_args


def to_dunzip_args(**kwargs):
    """API adapter for ZOAU dunzip method.

    Returns
    -------
    dict
        The arguments for ZOAU dunzip method translated from module arguments.
    """
    zoau_args = {}
    if kwargs.get("backup_name"):
        zoau_args["file"] = kwargs.get("backup_name")
        if not kwargs.get("backup_name").startswith("/"):
            zoau_args["dataset"] = True

    if kwargs.get("include_data_sets"):
        zoau_args["include"] = ",".join(kwargs.get("include_data_sets"))

    if kwargs.get("volume"):
        if kwargs.get("full_volume"):
            zoau_args["volume"] = kwargs.get("volume")
        else:
            zoau_args["src_volume"] = kwargs.get("volume")

    if kwargs.get("temp_volume"):
        zoau_args["dest_volume"] = kwargs.get("temp_volume")

    if kwargs.get("exclude_data_sets"):
        zoau_args["exclude"] = ",".join(kwargs.get("exclude_data_sets"))

    if kwargs.get("recover"):
        zoau_args["force"] = kwargs.get("recover")

    if kwargs.get("overwrite"):
        zoau_args["overwrite"] = kwargs.get("overwrite")

    sms_specified = False
    if sms_specified:
        zoau_args["sms_for_tmp"] = True

    if kwargs.get("space"):
        size = str(kwargs.get("space"))
        if kwargs.get("space_type"):
            size += kwargs.get("space_type")
        zoau_args["size"] = size

    output = kwargs.get("output")
    if output and output.get("hlq"):
        zoau_args["high_level_qualifier"] = output.get("hlq")
    else:
        zoau_args["keep_original_hlq"] = True

    if kwargs.get("tmp_hlq"):
        zoau_args["high_level_qualifier"] = str(kwargs.get("tmp_hlq"))
        zoau_args["keep_original_hlq"] = False

    sms = kwargs.get("sms")
    access = kwargs.get("access")
    keywords = set_adrdssu_keywords(sphere=kwargs.get("sphere"))

    if sms:
        if sms.get("sms_storage_class"):
            zoau_args["storage_class_name"] = sms.get("storage_class")

        if sms.get("sms_management_class"):
            zoau_args["management_class_name"] = sms.get("management_class")

        if sms.get("disable_automatic_management_class"):
            zoau_args["null_management_class"] = sms.get("disable_automatic_management_class")

        if sms.get("disable_automatic_storage_class"):
            zoau_args["null_storage_class"] = sms.get("disable_automatic_storage_class")

        if len(sms.get("disable_automatic_class")) > 0:
            bypassacs = set_bypassacs_str(ds=sms.get("disable_automatic_class"))
            zoau_args["bypass_acs"] = bypassacs

    if access:
        if access.get("auth"):
            zoau_args['admin'] = access.get("auth")

        if access.get("share"):
            zoau_args['share'] = access.get("share")

    if keywords:
        zoau_args["keywords"] = keywords

    return zoau_args


if __name__ == "__main__":
    main()
