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
module: zos_blockinfile
version_added: '1.3.0'
author:
  - "Behnam (@balkajbaf)"
  - "Demetrios Dimatos (@ddimatos)"
short_description: Manage block of multi-line textual data on z/OS
description:
  - Manage block of multi-lines in z/OS UNIX System Services (USS) files,
    PS (sequential data set), PDS, PDSE, or member of a PDS or PDSE.
  - This module ensures a particular block of multi-line text surrounded
    by customizable marker lines is present in a USS file or data set, or
    replaces an existing block identified by the markers.
  - This is primarily useful when you want to change a block of multi-line
    text in a USS file or data set.
options:
  src:
    description:
      - The location can be a UNIX System Services (USS) file,
        PS (sequential data set), member of a PDS or PDSE, PDS, PDSE.
      - The USS file must be an absolute pathname.
      - Generation data set (GDS) relative name of generation already
        created. e.g. I(SOME.CREATION(-1)).
    type: str
    aliases: [ path, destfile, name ]
    required: true
  state:
    description:
      - Whether the block should be inserted or replaced using I(state=present).
      - Whether the block should be removed using I(state=absent).
    type: str
    choices:
      - absent
      - present
    default: present
  marker:
    description:
    - The marker line template.
    - C({mark}) will be replaced with the values C(in marker_begin)
      (default="BEGIN") and C(marker_end) (default="END").
    - Using a custom marker without the C({mark}) variable may result
      in the block being repeatedly inserted on subsequent playbook runs.
    required: false
    type: str
    default: '# {mark} ANSIBLE MANAGED BLOCK'
  block:
    description:
    - The text to insert inside the marker lines.
    - Multi-line can be separated by '\n'.
    - Any double-quotation marks will be removed.
    required: false
    type: str
    default: ''
    aliases: [ content ]
  insertafter:
    description:
    - If specified, the block will be inserted after the last match of the specified
      regular expression.
    - A special value C(EOF) for inserting a block at the end of the file is
      available.
    - If a specified regular expression has no matches, C(EOF) will be used instead.
    - Choices are EOF or '*regex*'.
    - Default is EOF.
    required: false
    aliases: ['after']
    type: str
  insertbefore:
    description:
    - If specified, the block will be inserted before the last match of specified
      regular expression.
    - A special value C(BOF) for inserting the block at the beginning of the file
      is available.
    - If a specified regular expression has no matches, the block will be inserted
      at the end of the file.
    - Choices are BOF or '*regex*'.
    required: false
    aliases: ['before']
    type: str
  marker_begin:
    description:
    - This will be inserted at C({mark}) in the opening ansible block marker.
    - Value needs to be different from I(marker_end).
    required: false
    type: str
    default: BEGIN
  marker_end:
    required: false
    description:
    - This will be inserted at C({mark}) in the closing ansible block marker.
    - Value must be different from I(marker_begin).
    type: str
    default: END
  backup:
    description:
      - Specifies whether a backup of destination should be created before
        editing the source I(src).
      - When set to C(true), the module creates a backup file or data set.
      - The backup file name will be returned on either success or failure of
        module execution such that data can be retrieved.
      - Use generation data set (GDS) relative positive name. e.g. I(SOME.CREATION(+1)).
    required: false
    type: bool
    default: false
  backup_name:
    description:
      - Specify the USS file name or data set name for the destination backup.
      - If the source I(src) is a USS file or path, the backup_name name must be a file
        or path name, and the USS file or path must be an absolute path name.
      - If the source is an MVS data set, the backup_name name must be an MVS
        data set name, and the dataset must not be preallocated.
      - If the backup_name is not provided, the default backup_name name will
        be used. If the source is a USS file or path, the name of the backup
        file will be the source file or path name appended with a
        timestamp, e.g. C(/path/file_name.2020-04-23-08-32-29-bak.tar).
      - If the source is an MVS data set, it will be a data set with a random
        name generated by calling the ZOAU API. The MVS backup data set
        recovery can be done by renaming it.
      - If I(src) is a data set member and backup_name is not provided, the data set
        member will be backed up to the same partitioned data set with a randomly generated
        member name.
    required: false
    type: str
  tmp_hlq:
    description:
      - Override the default high level qualifier (HLQ) for temporary and backup
        datasets.
      - The default HLQ is the Ansible user used to execute the module and if
        that is not available, then the value C(TMPHLQ) is used.
    required: false
    type: str
  encoding:
    description:
      - The character set of the source I(src). L(zos_blockinfile,./zos_blockinfile.html)
        requires it to be provided with correct encoding to read the content
        of a USS file or data set. If this parameter is not provided, this
        module assumes that USS file or data set is encoded in IBM-1047.
      - Supported character sets rely on the charset conversion utility (iconv)
        version; the most common character sets are supported.
    required: false
    type: str
    default: IBM-1047
  force:
    description:
      - Specifies that the data set can be shared with others during an update
        which results in the data set you are updating to be simultaneously
        updated by others.
      - This is helpful when a data set is being used in a long running process
        such as a started task and you are wanting to update or read.
      - The C(force) option enables sharing of data sets through the disposition
        I(DISP=SHR).
    required: false
    type: bool
    default: false
  indentation:
    description:
      - Defines the number of spaces needed to prepend in every line of the block.
    required: false
    type: int
    default: 0

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
  - It is the playbook author or user's responsibility to avoid files
    that should not be encoded, such as binary files. A user is described
    as the remote user, configured either for the playbook or playbook
    tasks, who can also obtain escalated privileges to execute as root
    or another user.
  - All data sets are always assumed to be cataloged. If an uncataloged data set
    needs to be encoded, it should be cataloged first. The
    L(zos_data_set,./zos_data_set.html) module can be used to catalog uncataloged
    data sets.
  - For supported character sets used to encode data, refer to the
    L(documentation,https://ibm.github.io/z_ansible_collections_doc/ibm_zos_core/docs/source/resources/character_set.html).
  - When using ``with_*`` loops be aware that if you do not set a unique mark
    the block will be overwritten on each iteration.
  - When more then one block should be handled in a file you must change
    the I(marker) per task.
  - When working with a backup of a sequential dataset, the backup name should also be a sequential dataset.
    This will avoid the false positive and error condition during backup.
seealso:
- module: ibm.ibm_zos_core.zos_data_set
'''

EXAMPLES = r'''
- name: Insert/Update new mount point
  zos_blockinfile:
    src: SYS1.PARMLIB(BPXPRM00)
    marker: "/* {mark} ANSIBLE MANAGED BLOCK */"
    block: |
       MOUNT FILESYSTEM('SOME.DATA.SET') TYPE(ZFS) MODE(READ)
          MOUNTPOINT('/tmp/src/somedirectory')
- name: Remove a library as well as surrounding markers
  zos_blockinfile:
    state: absent
    src: SYS1.PARMLIB(PROG00)
    marker: "/* {mark} ANSIBLE MANAGED BLOCK FOR SOME.DATA.SET */"
- name: Add ZOAU path to PATH in /etc/profile
  zos_blockinfile:
    src: /etc/profile
    insertafter: "PATH="
    block: |
      ZOAU=/path/to/zoau_dir/bin
      export ZOAU
      PATH=$ZOAU:$PATH
- name: Insert/Update HTML surrounded by custom markers after <body> line
  zos_blockinfile:
    path: /var/www/html/index.html
    marker: "<!-- {mark} ANSIBLE MANAGED BLOCK -->"
    insertafter: "<body>"
    block: |
      <h1>Welcome to {{ ansible_hostname }}</h1>
      <p>Last updated on {{ ansible_date_time.iso8601 }}</p>
- name: Remove HTML as well as surrounding markers
  zos_blockinfile:
    path: /var/www/html/index.html
    state: absent
    marker: "<!-- {mark} ANSIBLE MANAGED BLOCK -->"
- name: Add mappings to /etc/hosts
  zos_blockinfile:
    path: /etc/hosts
    block: |
      {{ item.ip }} {{ item.name }}
    marker: "# {mark} ANSIBLE MANAGED BLOCK {{ item.name }}"
  loop:
    - { name: host1, ip: 10.10.1.10 }
    - { name: host2, ip: 10.10.1.11 }
    - { name: host3, ip: 10.10.1.12 }
- name: Add a code block to a member using a predefined indentation.
  zos_blockinfile:
    path: SYS1.PARMLIB(BPXPRM00)
    block: |
          DSN SYSTEM({{ DB2SSID }})
          RUN  PROGRAM(DSNTEP2) PLAN(DSNTEP12) -
          LIB('{{ DB2RUN }}.RUNLIB.LOAD')
    indentation: 16

- name: Update a script with commands containing quotes.
  zos_blockinfile:
    src: "/u/scripts/script.sh"
    insertafter: "EOF"
    block: |
          cat "//'{{ DS_NAME }}'"
          cat "//'{{ DS_NAME_2 }}'"

- name: Set facts for the following two tasks.
  set_fact:
    HLQ: 'ANSIBLE'
    MLQ: 'MEMBER'
    LLQ: 'TEST'
    MEM: '(JCL)'
    MSG: 'your first JCL program'
    CONTENT: "{{ lookup('file', 'files/content.txt') }}"

- name: Update JCL in a PDS member with Jinja2 variable syntax.
  zos_blockinfile:
    src: "{{ HLQ }}.{{MLQ}}.{{LLQ}}{{MEM}}"
    insertafter: "HELLO, WORLD"
    marker: "//* {mark} *//"
    marker_begin: "Begin Ansible Block Insertion 1"
    marker_end: "End Ansible Block Insertion 1"
    state: present
    block: |
      This is {{ MSG }}, and its now
      managed by Ansible.

- name: Update JCL in PDS member with content from a file.
  zos_blockinfile:
    src: "{{ HLQ }}.{{MLQ}}.{{LLQ}}{{MEM}}"
    insertafter: "End Ansible Block Insertion 1"
    marker: "//* {mark} *//"
    marker_begin: "Begin Ansible Block Insertion 2"
    marker_end: "End Ansible Block Insertion 2"
    block: "{{ CONTENT }}"

- name: Add a block to a gds
  zos_blockinfile:
    src: TEST.SOME.CREATION(0)
    insertafter: EOF
    block: "{{ CONTENT }}"

- name: Add a block to dataset and backup in a new generation of gds
  zos_blockinfile:
    src: SOME.CREATION.TEST
    insertbefore: BOF
    backup: true
    backup_name: CREATION.GDS(+1)
    block: "{{ CONTENT }}"
'''

RETURN = r"""
changed:
  description:
    Indicates if the source was modified.
    Value of 1 represents `true`, otherwise `false`.
  returned: success
  type: bool
  sample: 1
found:
  description: Number of the matching patterns
  returned: success
  type: int
  sample: 5
cmd:
  description: Constructed ZOAU dmod shell command based on the parameters
  returned: success
  type: str
  sample: dmod -d -b -c IBM-1047 -m "BEGIN\nEND\n# {mark} ANSIBLE MANAGED BLOCK" -e "$ a\\PATH=/dir/bin:$PATH" /etc/profile
msg:
  description: The module messages
  returned: failure
  type: str
  sample: Parameter verification failed
stdout:
  description: The stdout from ZOAU dmod when json.loads() fails to parse the result from dmod
  returned: failure
  type: str
stderr:
  description: The error messages from ZOAU dmod
  returned: failure
  type: str
  sample: BGYSC1311E Iconv error, cannot open converter from ISO-88955-1 to IBM-1047
stdout_lines:
    description: List of strings containing individual lines from stdout.
    returned: failure
    type: list
stderr_lines:
    description: List of strings containing individual lines from stderr.
    returned: failure
    type: list
rc:
  description: The return code from ZOAU dmod when json.loads() fails to parse the result from dmod
  returned: failure
  type: bool
backup_name:
    description: Name of the backup file or data set that was created.
    returned: if backup=true, always
    type: str
    sample: /path/to/file.txt.2015-02-03@04:15~
"""

import json
import traceback
from ansible.module_utils.basic import AnsibleModule
from ansible_collections.ibm.ibm_zos_core.plugins.module_utils import (
    better_arg_parser, data_set, backup as Backup)
from ansible_collections.ibm.ibm_zos_core.plugins.module_utils.import_handler import (
    ZOAUImportError
)
from ansible_collections.ibm.ibm_zos_core.plugins.module_utils.dependency_checker import (
    validate_dependencies,
)
from ansible_collections.ibm.ibm_zos_core.plugins.module_utils.log import SingletonLogger

try:
    from zoautil_py import datasets
except Exception:
    datasets = ZOAUImportError(traceback.format_exc())


# supported data set types
DS_TYPE = ['PS', 'PO']


def transformBlock(block, indentation_char, indentation_spaces):
    """Prepends the specified number of spaces to the block in all lines.

    Parameters
    ----------
    block : str
        The block text to be transformed.
    indentation_char : str
        The indentation char to be used.
    indentation_spaces : int
        Number of times the indentation char to prepend.

    Returns
    -------
    str
        The text block after applying the necessary transformations.
    """
    blocklines = block.splitlines()
    # Prepend spaces transformation
    line_break_char = '\\n'
    prepend_spaces = indentation_char * indentation_spaces
    prepend_text = line_break_char + prepend_spaces
    block = prepend_text.join(blocklines)
    block = prepend_spaces + block

    return block


def present(src, block, marker, ins_aft, ins_bef, encoding, force):
    """Replace a block with the matching regex pattern.
    Insert a block before/after the matching pattern.
    Insert a block at BOF/EOF.

    Parameters
    ----------
    src : str
        The z/OS USS file or data set to modify.
    block : str
        The block to insert/replace into the src.
    marker : str
        The block will be inserted/updated with the markers.
    ins_aft : str
        Insert the block after matching '*regex*' pattern or EOF.
        choices:
            - EOF
            - '*regex*'
    ins_bef : str
        Insert the block before matching '*regex*' pattern or BOF.
        choices:
            - BOF
            - '*regex*'
    encoding : str
        Encoding of the src.
    force : bool
        If not empty passes True option to dmod cmd.

    Returns
    -------
    str
        Information in JSON format. keys:
        cmd {str} -- dmod shell command.
        found {int} -- Number of matching regex pattern.
        changed {bool} -- Indicates if the destination was modified.
    """
    return datasets.blockinfile(src, True, block=block, marker=marker, insert_after=ins_aft,
                                insert_before=ins_bef, encoding=encoding, force=force, as_json=True)


def absent(src, marker, encoding, force):
    """Delete blocks with matching regex pattern.

    Parameter
    ---------
    src : str
        The z/OS USS file or data set to modify.
    marker : str
        Identifies the block to be removed.
    encoding : str
        Encoding of the src.
    force : bool
        If not empty passes the value True option to dmod cmd.

    Returns
    -------
    str
        Information in JSON format. keys:
        cmd {str} -- dmod shell command.
        found {int} -- Number of matching regex pattern.
        changed {bool} -- Indicates if the destination was modified.
    """
    return datasets.blockinfile(src, False, marker=marker, encoding=encoding, force=force, as_json=True)


def main():
    """Run the zos_blockinfile module core functions.

    Raises
    ------
    fail_json
        Parameter verification failed.
    fail_json
        Block is required with state=present.
    fail_json
        Marker should have {mark}.
    fail_json
        src does NOT exist.
    fail_json
        Data set type is NOT supported.
    fail_json
        Unable to allocate backup.
    fail_json
        ZOAU dmod return content is NOT in json format.
    """
    module = AnsibleModule(
        argument_spec=dict(
            src=dict(
                type='str',
                required=True,
                aliases=['path', 'destfile', 'name']
            ),
            state=dict(
                type='str',
                default='present',
                choices=['absent', 'present']
            ),
            marker=dict(
                type='str',
                default='# {mark} ANSIBLE MANAGED BLOCK'
            ),
            block=dict(
                type='str',
                default='',
                aliases=['content']
            ),
            insertafter=dict(
                type='str',
                aliases=['after'],
            ),
            insertbefore=dict(
                type='str',
                aliases=['before'],
            ),
            marker_begin=dict(
                type='str',
                default='BEGIN'
            ),
            marker_end=dict(
                type='str',
                default='END'
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
            encoding=dict(
                type='str',
                default='IBM-1047'
            ),
            tmp_hlq=dict(
                type='str',
                required=False,
                default=None
            ),
            force=dict(
                type='bool',
                default=False
            ),
            indentation=dict(
                type='int',
                required=False,
                default=0
            )
        ),
        mutually_exclusive=[['insertbefore', 'insertafter']],
    )
    validate_dependencies(module)

    arg_defs = dict(
        src=dict(arg_type='data_set_or_path', aliases=['path', 'destfile', 'name'], required=True),
        state=dict(arg_type='str', default='present', choices=['absent', 'present']),
        marker=dict(arg_type='str', default='# {mark} ANSIBLE MANAGED BLOCK', required=False),
        block=dict(arg_type='str', default='', aliases=['content'], required=False),
        insertafter=dict(arg_type='str', required=False, aliases=['after'],),
        insertbefore=dict(arg_type='str', required=False, aliases=['before'],),
        marker_begin=dict(arg_type='str', default='BEGIN', required=False),
        marker_end=dict(arg_type='str', default='END', required=False),
        encoding=dict(arg_type='str', default='IBM-1047', required=False),
        force=dict(arg_type='bool', default=False, required=False),
        backup=dict(arg_type='bool', default=False, required=False),
        backup_name=dict(arg_type='data_set_or_path', required=False, default=None),
        tmp_hlq=dict(type='qualifier_or_empty', required=False, default=None),
        mutually_exclusive=[['insertbefore', 'insertafter']],
        indentation=dict(arg_type='int', default=0, required=False)
    )
    result = dict(
        changed=False,
        cmd='',
        found=0,
        stdout='',
        stdout_lines=[],
        stderr='',
        stderr_lines=[],
        rc=0,
    )
    try:
        parser = better_arg_parser.BetterArgParser(arg_defs)
        parsed_args = parser.parse_args(module.params)
    except ValueError as err:
        module.fail_json(msg="Parameter verification failed", stderr=str(err))

    # Initialize logging module
    module_verbosity_level = module._verbosity
    SingletonLogger().get_logger(module_verbosity_level)

    backup = parsed_args.get('backup')
    if parsed_args.get('backup_name') and backup:
        backup = parsed_args.get('backup_name')
    src = parsed_args.get('src')
    ins_aft = parsed_args.get('insertafter')
    ins_bef = parsed_args.get('insertbefore')
    encoding = parsed_args.get('encoding')
    block = parsed_args.get('block')
    marker = parsed_args.get('marker')
    marker_begin = parsed_args.get('marker_begin')
    marker_end = parsed_args.get('marker_end')
    tmphlq = parsed_args.get('tmp_hlq')
    force = parsed_args.get('force')
    state = parsed_args.get('state')
    indentation = parsed_args.get('indentation')

    if not block and state == 'present':
        module.fail_json(msg='block is required with state=present')
    if not marker:
        marker = '# {mark} ANSIBLE MANAGED BLOCK'
    if "{mark}" not in marker:
        module.fail_json(msg='marker should have {mark}')
    # make sure the default encoding is set if empty string was passed
    if not encoding:
        encoding = "IBM-1047"
    if not ins_aft and not ins_bef and state == 'present':
        ins_aft = "EOF"
    if not marker_begin:
        marker_begin = 'BEGIN'
    if not marker_end:
        marker_end = 'END'
    if marker_begin == marker_end:
        module.fail_json(msg='marker_begin and marker_end must be different.')
    marker = "{0}\\n{1}\\n{2}".format(marker_begin, marker_end, marker)
    block = transformBlock(block, ' ', indentation)
    # analysis the file type
    if "/" not in src:
        dataset = data_set.MVSDataSet(
            name=src
        )
        src = dataset.name

    if data_set.DataSet.is_gds_relative_name(src):
        module.fail_json(msg="{0} does not exist".format(src))

    ds_utils = data_set.DataSetUtils(src, tmphlq=tmphlq)
    if not ds_utils.exists():
        message = "{0} does NOT exist".format(str(src))
        module.fail_json(msg=message)
    file_type = ds_utils.ds_type()

    if file_type != "USS":
        if file_type not in DS_TYPE:
            message = "{0} data set type is NOT supported".format(str(file_type))
            module.fail_json(msg=message)

    return_content = None
    if backup:
        # backup can be True(bool) or none-zero length string. string indicates that backup_name was provided.
        # setting backup to None if backup_name wasn't provided. if backup=None, Backup module will use
        # pre-defined naming scheme and return the created destination name.
        if isinstance(backup, bool):
            backup = None
        try:
            if file_type == "USS":
                result['backup_name'] = Backup.uss_file_backup(src, backup_name=backup, compress=False)
            else:
                result['backup_name'] = Backup.mvs_file_backup(dsn=src, bk_dsn=backup, tmphlq=tmphlq)
        except Exception as err:
            module.fail_json(msg="Unable to allocate backup {0} destination: {1}".format(backup, str(err)))
    # state=present, insert/replace a block with matching regex pattern
    # state=absent, delete blocks with matching regex pattern
    if parsed_args.get('state') == 'present':
        return_content = present(src, block, marker, ins_aft, ins_bef, encoding, force)
    else:
        return_content = absent(src, marker, encoding, force)
    # ZOAU 1.3.0 generate false positive working with double quotes (") the call generate distinct return when using and not
    stdout = return_content.stdout_response
    stderr = return_content.stderr_response
    rc = return_content.rc
    stdout = stdout.replace('/d', '\\\\d')
    try:
        # Try to extract information from stdout
        # The triple double quotes is required for special characters (/_) been scape
        ret = json.loads("""{0}""".format(stdout))
    except Exception:
        result.update(
            dict(
                msg="ZOAU dmod return content is NOT in json format",
                stdout=str(stdout),
                stdout_lines=stdout.splitlines(),
                stderr=str(stderr),
                stderr_lines=stderr.splitlines(),
                rc=rc
            )
        )
        module.fail_json(**result)

    result['cmd'] = ret['data']['commands']
    result['changed'] = ret['data']['changed']
    result['found'] = ret['data']['found']
    result['stderr'] = str(stderr)
    result['rc'] = rc
    module.exit_json(**result)


if __name__ == '__main__':
    main()
