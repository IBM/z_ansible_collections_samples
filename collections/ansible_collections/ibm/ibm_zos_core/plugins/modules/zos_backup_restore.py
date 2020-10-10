#!/usr/bin/python
# -*- coding: utf-8 -*-
# Copyright (c) IBM Corporation 2020
# Apache License, Version 2.0 (see https://opensource.org/licenses/Apache-2.0)

from __future__ import absolute_import, division, print_function

__metaclass__ = type

ANSIBLE_METADATA = {
    "metadata_version": "1.1",
    "status": ["preview"],
    "supported_by": "community",
}


DOCUMENTATION = r"""
module: zos_backup_restore
author: "Blake Becker (@blakeinate)"
short_description: Backup and restore data sets and volumes
description:
  - Create and restore from backups of data sets and volumes.
  - Data set backups are performed using logical dumps, volume backups are performed
    using physical dumps.
  - Backups are compressed.
options:
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
    required: True
    type: dict
    suboptions:
      include:
        description:
          - When I(operation=backup), specifies a list of data sets or data set patterns
            to include in the backup.
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
        required: True
      exclude:
        description:
          - When I(operation=backup), specifies a list of data sets or data set patterns
            to exclude from the backup.
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
    type: str
    required: False
    aliases:
      - dest_volume
  backup_name:
    description:
      - When I(operation=backup), the destination data set or UNIX file to hold the backup.
      - When I(operation=restore), the destination data set or UNIX file backup to restore.
    type: str
    required: True
  recover:
    description:
      - Specifies if potentially recoverable errors should be ignored.
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
  sms_storage_class:
    description:
      - When I(operation=restore), specifies the storage class to use. The storage class will
        also be used for temporary data sets created during restore process.
      - When I(operation=backup), specifies the storage class to use for temporary data sets
        created during backup process.
      - If neither of I(sms_storage_class) or I(sms_management_class) are specified, the z/OS
        system's Automatic Class Selection (ACS) routines will be used.
    type: str
    required: False
  sms_management_class:
    description:
      - When I(operation=restore), specifies the management class to use. The management class
        will also be used for temporary data sets created during restore process.
      - When I(operation=backup), specifies the management class to use for temporary data sets
        created during backup process.
      - If neither of I(sms_storage_class) or I(sms_management_class) are specified, the z/OS
        system's Automatic Class Selection (ACS) routines will be used.
    type: str
    required: False
  space:
    description:
      - If I(operation=backup), specifies the amount of space to allocate for the backup.
        Please note that even when backing up to a UNIX file, backup contents will be temporarily
        held in a data set.
      - If I(operation=backup), specifies the amount of space to allocate for data sets temporarily
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
      - Valid units of size are C(K), C(M), C(G), C(CYL), and C(TRK).
      - When I(full_volume=True), I(space_type) defaults to C(G), otherwise default is C(M)
    type: str
    choices:
      - K
      - M
      - G
      - CYL
      - TRK
    required: false
    aliases:
      - unit
  hlq:
    description:
      - Specifies the new HLQ to use for the data sets being restored.
      - Defaults to running user's username.
    type: str
    required: false
"""

RETURN = r""""""

EXAMPLES = r"""
- name: Backup all data sets matching the pattern USER.** to data set MY.BACKUP
  zos_backup_restore:
    operation: backup
    data_sets:
      include: user.**
    backup_name: MY.BACKUP

- name: Backup all data sets matching the patterns USER.** or PRIVATE.TEST.*
    excluding data sets matching the pattern USER.PRIVATE.* to data set MY.BACKUP
  zos_backup_restore:
    operation: backup
    data_sets:
      include:
        - user.**
        - private.test.*
      exclude: user.private.*
    backup_name: MY.BACKUP

- name: Backup all datasets matching the pattern USER.** to UNIX file /tmp/temp_backup.dzp, ignore recoverable errors.
  zos_backup_restore:
    operation: backup
    data_sets:
      include: user.**
    backup_name: /tmp/temp_backup.dzp
    recover: yes

- name: Backup all datasets matching the pattern USER.** to data set MY.BACKUP,
    allocate 100MB for data sets used in backup process.
  zos_backup_restore:
    operation: backup
    data_sets:
      include: user.**
    backup_name: MY.BACKUP
    space: 100
    space_type: M

- name:
    Backup all datasets matching the pattern USER.** that are present on the volume MYVOL1 to data set MY.BACKUP,
    allocate 100MB for data sets used in the backup process.
  zos_backup_restore:
    operation: backup
    data_sets:
      include: user.**
    volume: MYVOL1
    backup_name: MY.BACKUP
    space: 100
    space_type: M

- name: Backup an entire volume, MYVOL1, to the UNIX file /tmp/temp_backup.dzp,
    allocate 1GB for data sets used in backup process.
  zos_backup_restore:
    operation: backup
    backup_name: /tmp/temp_backup.dzp
    volume: MYVOL1
    full_volume: yes
    space: 1
    space_type: G

- name: Restore data sets from backup stored in the UNIX file /tmp/temp_backup.dzp.
    Use z/OS username as new HLQ.
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

- name: Restore data sets from backup stored in the data set MY.BACKUP.
    Use MYHLQ as the new HLQ for restored data sets.
  zos_backup_restore:
    operation: restore
    backup_name: MY.BACKUP
    hlq: MYHLQ

- name: Restore volume from backup stored in the data set MY.BACKUP.
    Restore to volume MYVOL2.
  zos_backup_restore:
    operation: restore
    volume: MYVOL2
    full_volume: yes
    backup_name: MY.BACKUP
    space: 1
    space_type: G

- name: Restore data sets from backup stored in the UNIX file /tmp/temp_backup.dzp.
    Specify DB2SMS10 for the SMS storage and management classes to use for the restored
    data sets.
  zos_backup_restore:
    operation: restore
    volume: MYVOL2
    backup_name: /tmp/temp_backup.dzp
    sms_storage_class: DB2SMS10
    sms_management_class: DB2SMS10
"""

from ansible_collections.ibm.ibm_zos_core.plugins.module_utils.better_arg_parser import (
    BetterArgParser,
)
from ansible.module_utils.basic import AnsibleModule

from re import match, search, IGNORECASE

from ansible_collections.ibm.ibm_zos_core.plugins.module_utils.import_handler import (
    MissingZOAUImport,
)
from os import path

try:
    from zoautil_py import datasets, exceptions
except ImportError:
    datasets = MissingZOAUImport()
    exceptions = MissingZOAUImport()


def main():
    """Run the zos_backup_restore module core functions."""
    result = dict(changed=False, message="", backup_name="")
    module_args = dict(
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
        space_type=dict(type="str", required=False, aliases=["unit"]),
        volume=dict(type="str", required=False),
        full_volume=dict(type="bool", default=False),
        temp_volume=dict(type="str", required=False, aliases=["dest_volume"]),
        backup_name=dict(type="str", required=True),
        recover=dict(type="bool", default=False),
        overwrite=dict(type="bool", default=False),
        sms_storage_class=dict(type="str", required=False),
        sms_management_class=dict(type="str", required=False),
        hlq=dict(type="str", required=False),
    )

    module = AnsibleModule(argument_spec=module_args, supports_check_mode=False)
    try:
        params = parse_and_validate_args(module.params)
        operation = params.get("operation")
        data_sets = params.get("data_sets", {})
        space = params.get("space")
        space_type = params.get("space_type")
        volume = params.get("volume")
        full_volume = params.get("full_volume")
        temp_volume = params.get("temp_volume")
        backup_name = params.get("backup_name")
        recover = params.get("recover")
        overwrite = params.get("overwrite")
        sms_storage_class = params.get("sms_storage_class")
        sms_management_class = params.get("sms_management_class")
        hlq = params.get("hlq")

        if operation == "backup":
            backup(
                backup_name=backup_name,
                include_data_sets=data_sets.get("include"),
                exclude_data_sets=data_sets.get("exclude"),
                volume=volume,
                full_volume=full_volume,
                temp_volume=temp_volume,
                overwrite=overwrite,
                recover=recover,
                space=space,
                space_type=space_type,
                sms_storage_class=sms_storage_class,
                sms_management_class=sms_management_class,
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
                hlq=hlq,
                space=space,
                space_type=space_type,
                sms_storage_class=sms_storage_class,
                sms_management_class=sms_management_class,
            )
        result["changed"] = True

    except Exception as e:
        module.fail_json(msg=repr(e), **result)
    module.exit_json(**result)


def parse_and_validate_args(params):
    """Parse and validate arguments to be used by remainder of module.

    Args:
        params (dict): The params as returned from AnsibleModule instantiation.

    Returns:
        dict: The updated params after additional parsing and validation.
    """
    arg_defs = dict(
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
        volume=dict(type="str", required=False, dependencies=["data_sets"]),
        full_volume=dict(type=full_volume_type, default=False, dependencies=["volume"]),
        temp_volume=dict(type="str", required=False, aliases=["dest_volume"]),
        backup_name=dict(type=backup_name_type, required=False),
        recover=dict(type="bool", default=False),
        overwrite=dict(type="bool", default=False),
        sms_storage_class=dict(type=sms_type, required=False),
        sms_management_class=dict(type=sms_type, required=False),
        hlq=dict(type=hlq_type, default=hlq_default, dependencies=["operation"]),
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
    recover,
    space,
    space_type,
    sms_storage_class,
    sms_management_class,
):
    """Backup data sets or a volume to a new data set or unix file.

    Args:
        backup_name (str): The data set or UNIX path to place the backup.
        include_data_sets (list): A list of data set patterns to include in the backup.
        exclude_data_sets (list): A list of data set patterns to exclude from the backup.
        volume (str): The volume that contains the data sets to backup.
        full_volume (bool): Specifies if a backup will be made of the entire volume.
        temp_volume (bool): Specifies the volume that should be used to store temporary files.
        overwrite (bool): Specifies if existing data set or UNIX file matching I(backup_name) should be deleted.
        recover (bool): Specifies if potentially recoverable errors should be ignored.
        space (int): Specifies the amount of space to allocate for the backup.
        space_type (str): The unit of measurement to use when defining data set space.
        sms_storage_class (str): Specifies the storage class to use.
        sms_management_class (str): Specifies the management class to use.
    """
    args = locals()
    zoau_args = to_dzip_args(**args)
    datasets.zip(**zoau_args)


def restore(
    backup_name,
    include_data_sets,
    exclude_data_sets,
    volume,
    full_volume,
    temp_volume,
    overwrite,
    recover,
    hlq,
    space,
    space_type,
    sms_storage_class,
    sms_management_class,
):
    """[summary]

    Args:
        backup_name (str): The data set or UNIX path containing the backup.
        include_data_sets (list): A list of data set patterns to include in the restore
          that are present in the backup.
        exclude_data_sets (list): A list of data set patterns to exclude from the restore
          that are present in the backup.
        volume (str): The volume that contains the data sets to backup.
        full_volume (bool): Specifies if a backup will be made of the entire volume.
        temp_volume (bool): Specifies the volume that should be used to store temporary files.
        overwrite (bool): Specifies if module should overwrite existing data sets with
          matching name on the target device.
        recover (bool): Specifies if potentially recoverable errors should be ignored.
        hlq (str): Specifies the new HLQ to use for the data sets being restored.
        space (int): Specifies the amount of space to allocate for data sets temporarily
          created during the restore process.
        space_type (str): The unit of measurement to use when defining data set space.
        sms_storage_class (str): Specifies the storage class to use.
        sms_management_class (str): Specifies the management class to use.
    """
    args = locals()
    zoau_args = to_dunzip_args(**args)
    response = datasets._unzip(**zoau_args)
    failed = False
    true_rc = response.rc
    if response.rc > 0:
        output = response.stdout_response + response.stderr_response
        true_rc = get_real_rc(output) or true_rc
    if true_rc > 0 and true_rc <= 4:
        if recover is not True:
            failed = True
    elif true_rc > 0:
        failed = True
    if failed:
        raise exceptions.ZOAUException(
            "%s,RC=%s" % (response.stderr_response, response.rc)
        )


def get_real_rc(output):
    """Parse out the final RC from MVS program output.

    Args:
        output (str): The MVS program output.

    Returns:
        int: The true program RC.
    """
    true_rc = None
    match = search(
        r"HIGHEST\sRETURN\sCODE\sIS\s([0-9]+)",
        output,
    )
    if match:
        true_rc = int(match.group(1))
    return true_rc


def data_set_pattern_type(contents, dependencies):
    """Validates provided data set patterns.

    Args:
        contents (Union[str, list[str]]): One or more data set patterns
        dependencies (dict): Any dependent arguments

    Raises:
        ValueError: When provided argument is not a string or a list
        ValueError: When provided argument is an invalid data set pattern

    Returns:
        list[str]: A list of uppercase data set patterns
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
            r"^(?:(?:[A-Za-z$#@\?\*]{1}[A-Za-z0-9$#@\-\?\*]{0,7})(?:[.]{1})){1,21}[A-Za-z$#@\*\?]{1}[A-Za-z0-9$#@\-\*\?]{0,7}$",
            str(pattern),
            IGNORECASE,
        ):
            raise ValueError(
                "Invalid argument {0} for data set pattern.".format(contents)
            )
    return [x.upper() for x in contents]


def hlq_type(contents, dependencies):
    """Validates provided HLQ is valid and is not specified for a backup operation.

    Args:
        contents (str): The HLQ to use
        dependencies (dict): Any dependent arguments

    Raises:
        ValueError: When operation is restore and HLQ is provided
        ValueError: When an invalid HLQ is provided

    Returns:
        str: The HLQ to use
    """
    if contents is None:
        return None
    if dependencies.get("operation") == "backup":
        raise ValueError("hlq_type is only valid when operation=restore.")
    if not match(r"^(?:[A-Z$#@]{1}[A-Z0-9$#@-]{0,7})$", contents, IGNORECASE):
        raise ValueError("Invalid argument {0} for hlq_type.".format(contents))
    return contents.upper()


def hlq_default(contents, dependencies):
    """Sets the default HLQ to use if none is provided.

    Args:
        contents (str): The HLQ to use
        dependencies (dict): Any dependent arguments

    Returns:
        str: The HLQ to use
    """
    hlq = None
    if dependencies.get("operation") == "restore":
        hlq = datasets.hlq()
    return hlq


def sms_type(contents, dependencies):
    """Validates the SMS class provided matches a valid format.

    Args:
        contents (str): The SMS class name
        dependencies (dict): Any dependent arguments

    Raises:
        ValueError: When invalid argument provided for SMS class.

    Returns:
        str: The uppercase SMS class name
    """
    if contents is None:
        return None
    if not match(r"^[A-Z\$\*\@\#\%]{1}[A-Z0-9\$\*\@\#\%]{0,7}$", contents, IGNORECASE):
        raise ValueError("Invalid argument {0} for SMS class.".format(contents))
    return str(contents).upper()


def space_type(contents, dependencies):
    """Validates amount of space provided.

    Args:
        contents (str): The amount of space
        dependencies (dict): Any dependent arguments

    Returns:
        int: The amount of space
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

    Args:
        contents (str): The space type to use
        dependencies (dict): Any dependent arguments

    Raises:
        ValueError: When an invalid space unit is provided

    Returns:
        str: The unit of space
    """
    if contents is None:
        if dependencies.get("full_volume"):
            return "G"
        else:
            return "M"
    if not match(r"^(M|G|K|TRK|CYL)$", contents, IGNORECASE):
        raise ValueError(
            'Value {0} is invalid for space_type argument. Valid space types are "K", "M", "G", "TRK" or "CYL".'.format(
                contents
            )
        )
    return contents


def backup_name_type(contents, dependencies):
    """Validates provided backup name.

    Args:
        contents (str): The backup name to use
        dependencies (dict): Any dependent arguments

    Raises:
        ValueError: When an invalid backup name is provided

    Returns:
        str: The backup name to use
    """
    if contents is None:
        return None
    if not match(
        r"^(?:(?:[A-Za-z$#@\?\*]{1}[A-Za-z0-9$#@\-\?\*]{0,7})(?:[.]{1})){1,21}[A-Za-z$#@\*\?]{1}[A-Za-z0-9$#@\-\*\?]{0,7}$",
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

    Args:
        contents (bool): Whether we are making a full volume backup or not
        dependencies (dict): Any dependent arguments

    Raises:
        ValueError: When volume argument is not provided

    Returns:
        bool: Whether we are making a full volume backup or not
    """
    if contents is True and dependencies.get("volume") is None:
        raise ValueError("full_volume=True is only valid when volume is specified.")
    return contents


def to_dzip_args(**kwargs):
    """API adapter for ZOAU dzip method.

    Returns:
        dict: The arguments for ZOAU dzip method translated from module arguments
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

    if kwargs.get("sms_storage_class"):
        zoau_args["storage_class_name"] = kwargs.get("sms_storage_class")

    if kwargs.get("sms_management_class"):
        zoau_args["management_class_name"] = kwargs.get("sms_management_class")

    if kwargs.get("space"):
        size = str(kwargs.get("space"))
        if kwargs.get("space_type"):
            size += kwargs.get("space_type")
        zoau_args["size"] = size
    return zoau_args


def to_dunzip_args(**kwargs):
    """API adapter for ZOAU dunzip method.

    Returns:
        dict: The arguments for ZOAU dunzip method translated from module arguments
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
    if kwargs.get("sms_storage_class"):
        zoau_args["storage_class_name"] = kwargs.get("sms_storage_class")

    if kwargs.get("sms_management_class"):
        zoau_args["management_class_name"] = kwargs.get("sms_management_class")

    if sms_specified:
        zoau_args["sms_for_tmp"] = True

    if kwargs.get("space"):
        size = str(kwargs.get("space"))
        if kwargs.get("space_type"):
            size += kwargs.get("space_type")
        zoau_args["size"] = size

    if kwargs.get("hlq"):
        zoau_args["hlq"] = kwargs.get("hlq")

    return zoau_args


if __name__ == "__main__":
    main()
