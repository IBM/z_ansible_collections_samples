#!/usr/bin/python
# -*- coding: utf-8 -*-

# Copyright (c) IBM Corporation 2020, 2025
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
module: zos_apf
version_added: '1.3.0'
author:
  - "Behnam (@balkajbaf)"
  - "Rich Parker (@richp405)"
  - "Fernando Flores (@fernandofloresg))"
short_description: Add or remove libraries to Authorized Program Facility (APF)
description:
  - Adds or removes libraries to Authorized Program Facility (APF).
  - Manages APF statement persistent entries to a data set or data set member.
  - Changes APF list format to "DYNAMIC" or "STATIC".
  - Gets the current APF list entries.
options:
  library:
    description:
      - The library name to be added or removed from the APF list.
    required: False
    type: str
    aliases: [ name, lib ]
  state:
    description:
      - Ensure that the library is added C(state=present) or removed
        C(state=absent).
      - The APF list format has to be "DYNAMIC".
    required: False
    type: str
    choices:
      - absent
      - present
    default: present
  force_dynamic:
    description:
      - Will force the APF list format to "DYNAMIC" before adding or removing
        libraries.
      - If the format is "STATIC", the format will be changed to "DYNAMIC".
    required: False
    type: bool
    default: False
  volume:
    description:
      - The identifier for the volume containing the library specified in
        the C(library) parameter. The values must be one the following.
      - 1. The volume serial number.
      - 2. Six asterisks C(******), indicating that the system must use the
        volume serial number of the current system residence (SYSRES) volume.
      - 3. *MCAT*, indicating that the system must use the volume serial number
        of the volume containing the master catalog.
      - If C(volume) is not specified, C(library) has to be cataloged.
    required: False
    type: str
  sms:
    description:
      - Indicates that the library specified in the C(library) parameter is
        managed by the storage management subsystem (SMS), and therefore no
        volume is associated with the library.
      - If C(sms=True), C(volume) value will be ignored.
    required: False
    type: bool
    default: False
  operation:
    description:
      - Change APF list format to "DYNAMIC" C(operation=set_dynamic) or "STATIC"
        C(operation=set_static)
      - Display APF list current format C(operation=check_format)
      - Display APF list entries when C(operation=list)
        C(library), C(volume) and C(sms) will be used as filters.
      - If C(operation) is not set, add or remove operation will be ignored.
    required: False
    type: str
    choices:
      - set_dynamic
      - set_static
      - check_format
      - list
  tmp_hlq:
    description:
      - Override the default high level qualifier (HLQ) for temporary and backup
        datasets.
      - The default HLQ is the Ansible user used to execute the module and if
        that is not available, then the value C(TMPHLQ) is used.
    required: false
    type: str
  persistent:
    description:
      - Add/remove persistent entries to or from I(target)
      - C(library) will not be persisted or removed if C(persistent=None)
    required: False
    type: dict
    suboptions:
      target:
        description:
          - The data set name used for persisting or removing a C(library) from
            the APF list.
        required: True
        type: str
        aliases: [data_set_name]
      marker:
        description:
          - The marker line template.
          - C({mark}) will be replaced with "BEGIN" and "END".
          - Using a custom marker without the C({mark}) variable may result
            in the block being repeatedly inserted on subsequent playbook runs.
          - C({mark}) length may not exceed 72 characters.
          - The timestamp (<timestamp>) used in the default marker
            follows the '+%Y%m%d-%H%M%S' date format
        required: False
        type: str
        default: "/* {mark} ANSIBLE MANAGED BLOCK <timestamp> */"
      backup:
        description:
          - Creates a backup file or backup data set for I(target),
            including the timestamp information to ensure that you retrieve the
            original APF list
            defined in I(target)".
          - I(backup_name) can be used to specify a backup file name if
            I(backup=true).
          - The backup file name will be return on either success or failure
            of module execution such that data can be retrieved.
        required: false
        type: bool
        default: false
      backup_name:
        description:
          - Specify the USS file name or data set name for the destination
            backup.
          - If the source I(target) is a USS file or path, the
            backup_name name must be a file or path name, and the USS file or
            path must be an absolute path name.
          - If the source is an MVS data set, the backup_name must be
            an MVS data set name.
          - If the backup_name is not provided, the default backup_name
            will be used. If the source is a USS file or path, the name of
            the backup file will be the source file or path name appended
            with a timestamp.
            For example, C(/path/file_name.2020-04-23-08-32-29-bak.tar).
          - If the source is an MVS data set, it will be a data set with a
            random name generated by calling the ZOAU API. The MVS backup
            data set recovery can be done by renaming it.
        required: false
        type: str
  batch:
    description:
      - A list of dictionaries for adding or removing libraries.
      - This is mutually exclusive with C(library), C(volume), C(sms)
      - Can be used with C(persistent)
    type: list
    elements: dict
    required: false
    suboptions:
      library:
        description:
          - The library name to be added or removed from the APF list.
        type: str
        required: True
        aliases: [ name, lib ]
      volume:
        description:
          - The identifier for the volume containing the library
            specified on the C(library) parameter. The values must be one of the
            following.
          - 1. The volume serial number
          - 2. Six asterisks C(******), indicating that the system must use the
            volume serial number of the current system residence (SYSRES)
            volume.
          - 3. *MCAT*, indicating that the system must use the volume serial
            number of the volume containing the master catalog.
          - If C(volume) is not specified, C(library) has to be cataloged.
        required: False
        type: str
      sms:
        description:
          - Indicates that the library specified in the C(library) parameter is
            managed by the storage management subsystem (SMS), and therefore no
            volume is associated with the library.
          - If true C(volume) will be ignored.
        required: False
        type: bool
        default: False

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
      appropriate authority to the RACF® FACILITY resource class. A user is
      described as the remote user, configured either for the playbook or
      playbook tasks, who can also obtain escalated privileges to execute as
      root or another user.
    - To add or delete the APF list entry for library libname, you must have
      UPDATE authority to the RACF® FACILITY resource class entity CSVAPF.libname,
      or there must be no FACILITY class profile that protects that entity.
    - To change the format of the APF list to dynamic, you must have UPDATE
      authority to the RACF FACILITY resource class profile
      CSVAPF.MVS.SETPROG.FORMAT.DYNAMIC, or there must be no FACILITY class
      profile that protects that entity.
    - To change the format of the APF list back to static, you must have UPDATE
      authority to the RACF FACILITY resource class profile
      CSVAPF.MVS.SETPROG.FORMAT.STATIC, or there must be no FACILITY class profile
      that protects that entity.
'''


EXAMPLES = r'''
- name: Add a library to the APF list
  zos_apf:
    library: SOME.SEQUENTIAL.DATASET
    volume: T12345
- name: Add a library (cataloged) to the APF list and persistence
  zos_apf:
    library: SOME.SEQUENTIAL.DATASET
    force_dynamic: true
    persistent:
      target: SOME.PARTITIONED.DATASET(MEM)
- name: Remove a library from the APF list and persistence
  zos_apf:
    state: absent
    library: SOME.SEQUENTIAL.DATASET
    volume: T12345
    persistent:
      target: SOME.PARTITIONED.DATASET(MEM)
- name: Batch libraries with custom marker, persistence for the APF list
  zos_apf:
    persistent:
      target: "SOME.PARTITIONED.DATASET(MEM)"
      marker: "/* {mark} PROG001 USR0010 */"
    batch:
      - library: SOME.SEQ.DS1
      - library: SOME.SEQ.DS2
        sms: true
      - library: SOME.SEQ.DS3
        volume: T12345
- name: Print the APF list matching library pattern or volume serial number
  zos_apf:
    operation: list
    library: SOME.SEQ.*
    volume: T12345
- name: Set the APF list format to STATIC
  zos_apf:
    operation: set_static
'''


RETURN = r'''
stdout:
  description:
    - The stdout from ZOAU command apfadm. Output varies based on the type of
      operation.
    - state> stdout of the executed operator command (opercmd), "SETPROG" from
      ZOAU command apfadm
    - "operation> stdout of operation options
                 list> Returns a list of dictionaries of APF list entries
                       [{'vol': 'PP0L6P', 'ds': 'DFH.V5R3M0.CICS.SDFHAUTH'},
                       {'vol': 'PP0L6P', 'ds': 'DFH.V5R3M0.CICS.SDFJAUTH'}, ...]
                 set_dynamic>  Set to DYNAMIC
                 set_static>   Set to STATIC
                 check_format> DYNAMIC or STATIC"
  returned: always
  type: str
stdout_lines:
  description: List of strings containing individual lines from STDOUT.
  returned: always
  type: list
stderr:
  description: The error messages from ZOAU command apfadm
  returned: always
  type: str
  sample: "BGYSC1310E ADD Error: Dataset COMMON.LINKLIB volume COMN01 is already
  present in APF list."
stderr_lines:
  description: List of strings containing individual lines from STDERR.
  returned: always
  type: list
  sample: ["BGYSC1310E ADD Error: Dataset COMMON.LINKLIB volume COMN01 is already
  present in APF list."]
rc:
  description: The return code from ZOAU command apfadm
  returned: always
  type: int
msg:
  description: The module messages
  returned: failure
  type: str
  sample: Parameter verification failed
backup_name:
    description: Name of the backup file or data set that was created.
    returned: if backup=true, always
    type: str
'''

import os
import re
import json
from ansible.module_utils._text import to_text
from ansible.module_utils.basic import AnsibleModule
from ansible_collections.ibm.ibm_zos_core.plugins.module_utils import (
    better_arg_parser, data_set, backup as Backup)

from ansible_collections.ibm.ibm_zos_core.plugins.module_utils.import_handler import (
    ZOAUImportError,
)
from ansible_collections.ibm.ibm_zos_core.plugins.module_utils.dependency_checker import (
    validate_dependencies,
)
import traceback
from ansible_collections.ibm.ibm_zos_core.plugins.module_utils.log import SingletonLogger

try:
    from zoautil_py import zsystem
except Exception:
    zsystem = ZOAUImportError(traceback.format_exc())


# supported data set types
DS_TYPE = data_set.DataSet.MVS_SEQ.union(data_set.DataSet.MVS_PARTITIONED)


def backupOper(module, src, backup, tmphlq=None):
    """Create a backup for a specified USS file or MVS data set.

    Parameters
    ----------
    module : AnsibleModule
    src : str
        Source USS file or data set to backup.
    backup : str
        Name for the backup.
    tmphlq : str
        The name of the temporary high level qualifier to use.

    Returns
    -------
    str
        Backup name.

    Raises
    ------
    fail_json
        Data set type is NOT supported.
    fail_json
        Creating backup has failed.
    """
    file_type = None
    if data_set.is_data_set(src):
        file_type = data_set.DataSet.data_set_type(src, tmphlq=tmphlq)
    else:
        if os.path.exists(src):
            file_type = 'USS'

    if file_type != 'USS' and file_type not in DS_TYPE:
        message = "Dataset {0} of type {1} is NOT supported".format(src, str(file_type))
        module.fail_json(msg=message)

    # backup can be True(bool) or none-zero length string. string indicates that backup_name was provided.
    # setting backup to None if backup_name wasn't provided. if backup=None, Backup module will use
    # pre-defined naming scheme and return the created destination name.
    if isinstance(backup, bool):
        backup = None
    try:
        if file_type == 'USS':
            backup_name = Backup.uss_file_backup(src, backup_name=backup, compress=False)
        else:
            backup_name = Backup.mvs_file_backup(dsn=src, bk_dsn=backup, tmphlq=tmphlq)
    except Backup.BackupError as exc:
        module.fail_json(
            msg=exc.msg,
            rc=exc.rc,
            stdout=exc.stdout,
            stderr=exc.stderr
        )
    except Exception:
        module.fail_json(
            msg="An error ocurred during backup."
        )

    return backup_name


def main():
    """Initialize the module.

    Raises
    ------
    fail_json
        Parameter verification failed.
    fail_json
        Marker length may not exceed 72 characters.
    fail_json
        library is required.
    fail_json
        An exception occurred.
    """
    module = AnsibleModule(
        argument_spec=dict(
            library=dict(
                type='str',
                required=False,
                aliases=['lib', 'name']
            ),
            state=dict(
                type='str',
                default='present',
                choices=['absent', 'present']
            ),
            force_dynamic=dict(
                type='bool',
                default=False
            ),
            volume=dict(
                type='str',
                required=False
            ),
            sms=dict(
                type='bool',
                required=False,
                default=False
            ),
            operation=dict(
                type='str',
                required=False,
                choices=['set_dynamic', 'set_static', 'check_format', 'list']
            ),
            persistent=dict(
                type='dict',
                required=False,
                options=dict(
                    target=dict(
                        type='str',
                        required=True,
                        aliases=["data_set_name"],
                    ),
                    marker=dict(
                        type='str',
                        required=False,
                        default='/* {mark} ANSIBLE MANAGED BLOCK <timestamp> */'
                    ),
                    backup=dict(
                        type='bool',
                        default=False
                    ),
                    backup_name=dict(
                        type='str',
                        required=False,
                        default=None
                    ),
                )
            ),
            batch=dict(
                type='list',
                elements='dict',
                required=False,
                options=dict(
                    library=dict(
                        type='str',
                        required=True,
                        aliases=['lib', 'name']
                    ),
                    volume=dict(
                        type='str',
                        required=False
                    ),
                    sms=dict(
                        type='bool',
                        required=False,
                        default=False
                    ),
                )
            ),
            tmp_hlq=dict(
                type='str',
                required=False,
                default=None),
        ),
        mutually_exclusive=[
            # batch
            ['batch', 'library'],
            ['batch', 'volume'],
            # operation
            ['operation', 'persistent'],
            ['batch', 'operation'],
        ],
    )
    validate_dependencies(module)

    arg_defs = dict(
        library=dict(arg_type='str', required=False, aliases=['lib', 'name']),
        state=dict(arg_type='str', default='present', choices=['absent', 'present']),
        force_dynamic=dict(arg_type='bool', default=False),
        volume=dict(arg_type='str', required=False),
        sms=dict(arg_type='bool', required=False, default=False),
        operation=dict(arg_type='str', required=False, choices=['set_dynamic', 'set_static', 'check_format', 'list']),
        persistent=dict(
            arg_type='dict',
            required=False,
            options=dict(
                target=dict(arg_type='str', required=True, aliases=["data_set_name"]),
                marker=dict(arg_type='str', required=False, default='/* {mark} ANSIBLE MANAGED BLOCK <timestamp> */'),
                backup=dict(arg_type='bool', default=False),
                backup_name=dict(arg_type='str', required=False, default=None),
            )
        ),
        batch=dict(
            arg_type='list',
            elements='dict',
            required=False,
            default=None,
            options=dict(
                library=dict(arg_type='str', required=True, aliases=['lib', 'name']),
                volume=dict(arg_type='str', required=False),
                sms=dict(arg_type='bool', required=False, default=False),
            )
        ),
        tmp_hlq=dict(type='qualifier_or_empty', required=False, default=None),
        mutually_exclusive=[
            # batch
            ['batch', 'library'],
            ['batch', 'volume'],
            # operation
            ['operation', 'persistent'],
            ['batch', 'operation'],
        ],
    )
    result = dict(changed=False)
    try:
        parser = better_arg_parser.BetterArgParser(arg_defs)
        parsed_args = parser.parse_args(module.params)

    except ValueError as err:
        module.fail_json(msg="Parameter verification failed", stderr=str(err))

    # Initialize logging module
    module_verbosity_level = module._verbosity
    SingletonLogger().get_logger(module_verbosity_level)

    library = parsed_args.get("library")

    state = parsed_args.get('state')
    force_dynamic = parsed_args.get('force_dynamic')
    volume = parsed_args.get('volume')
    sms = parsed_args.get('sms')
    operation = parsed_args.get('operation')
    persistent = parsed_args.get('persistent')
    batch = parsed_args.get('batch')
    tmphlq = module.params.get('tmp_hlq')

    if persistent:
        target = persistent.get('target')
        backup = persistent.get('backup')
        marker = persistent.get('marker')
        if len(marker) > 71:
            module.fail_json(msg='marker length may not exceed 72 characters')
        if backup:
            if persistent.get('backup_name'):
                backup = persistent.get('backup_name')
                del persistent['backup_name']
            result['backup_name'] = backupOper(module, target, backup, tmphlq)
            del persistent['backup']
        if state == "present":
            persistent['addDataset'] = target
        else:
            persistent['delDataset'] = target
        del persistent['target']

    if operation:
        ret = zsystem.apf(opt=operation)
    else:
        if state == "present":
            opt = "add"
        else:
            opt = "del"
        if batch:
            for item in batch:
                item['opt'] = opt
                item['dsname'] = item.get('library')
                del item['library']
            # ignore=true is added so that it's ignoring in case of addition if already present
            # ignore=true is added so that it's ignoring in case the file is not in apf list while deletion
            ret = zsystem.apf(batch=batch, forceDynamic=force_dynamic, persistent=persistent, ignore=True)
        else:
            if not library:
                module.fail_json(msg='library is required')
            # ignore=true is added so that it's ignoring in case of addition if already present
            # ignore=true is added so that it's ignoring in case the file is not in apf list while deletion
            ret = zsystem.apf(opt=opt, dsname=library, volume=volume, sms=sms, forceDynamic=force_dynamic, persistent=persistent, ignore=True)

    operOut = ret.stdout_response
    operErr = ret.stderr_response
    operRc = ret.rc
    result['stderr'] = operErr
    result['stderr_lines'] = operErr.split("\n")
    result['rc'] = operRc
    result['stdout'] = operOut
    result['stdout_lines'] = operOut.split("\n")

    if operation != 'list' and operRc == 0:
        if operErr.strip():
            result['changed'] = False
        else:
            result['changed'] = True

    if operation == 'list':
        try:
            data = json.loads(operOut)
            data_sets = data["data"]["datasets"]
        except Exception as e:
            err_msg = "An exception occurred. See stderr for more details."
            module.fail_json(msg=err_msg, stderr=to_text(e), rc=operErr)
        if not library:
            library = ""
        if not volume:
            volume = ""
        if sms:
            sms = "*SMS*"
        if library or volume or sms:
            ds_list = ""
            for d in data_sets:
                ds = d.get('ds')
                vol = d.get('vol')
                try:
                    if (library and re.match(library, ds)) or (volume and re.match(volume, vol)) or (sms and sms == vol):
                        ds_list = ds_list + "{0} {1}\n".format(vol, ds)
                except re.error:
                    module.exit_json(**result)
            result['stdout'] = ds_list
            result['stdout_lines'] = ds_list.split("\n")
        else:
            """
            ZOAU 1.3 changed the output from apf, having the data set list inside a new "data" tag.
            To keep consistency with previous ZOAU versions now we have to filter the json response.
            """
            try:
                result['stdout'] = json.dumps(data.get("data"))
                result['stdout_lines'] = json.dumps(data.get("data")).split("\n")
            except Exception as e:
                err_msg = "An exception occurred. See stderr for more details."
                module.fail_json(msg=err_msg, stderr=to_text(e), rc=operErr)
    module.exit_json(**result)


if __name__ == '__main__':
    main()
