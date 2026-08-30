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


DOCUMENTATION = r"""
---
module: zos_lineinfile
version_added: '1.2.0'
author:
  - "Behnam (@balkajbaf)"
short_description: Manage textual data on z/OS
description:
  - Manage lines in z/OS UNIX System Services (USS) files,
    PS (sequential data set), PDS, PDSE, or member of a PDS or PDSE.
  - This module ensures a particular line is in a USS file or data set, or
    replace an existing line using a back-referenced regular expression.
  - This is primarily useful when you want to change a single line in a USS
    file or data set only.
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
  regexp:
    description:
      - The regular expression to look for in every line of the USS file
        or data set.
      - For C(state=present), the pattern to replace if found. Only the
        last line found will be replaced.
      - For C(state=absent), the pattern of the line(s) to remove.
      - If the regular expression is not matched, the line will be
        added to the USS file or data set in keeping with C(insertbefore) or
        C(insertafter) settings.
      - When modifying a line the regexp should typically match both
        the initial state of the line as well as its state after replacement by
        C(line) to ensure idempotence.
    type: str
    required: false
  state:
    description:
      - Whether the line should be inserted/replaced(present) or removed(absent).
    type: str
    choices:
      - absent
      - present
    default: present
  line:
    description:
      - The line to insert/replace into the USS file or data set.
      - Required for C(state=present).
      - If C(backrefs) is set, may contain backreferences that will get
        expanded with the C(regexp) capture groups if the regexp matches.
    required: false
    type: str
  backrefs:
    description:
      - Used with C(state=present).
      - If set, C(line) can contain backreferences (both positional and named)
        that will get populated if the C(regexp) matches.
      - This parameter changes the operation of the module slightly;
        C(insertbefore) and C(insertafter) will be ignored, and if the
        C(regexp) does not match anywhere in the USS file or data set, the USS file
        or data set will be left unchanged.
      - If the C(regexp) does match, the last matching line will be replaced by
        the expanded line parameter.
    required: false
    type: bool
    default: no
  insertafter:
    description:
      - Used with C(state=present).
      - If specified, the line will be inserted after the last match of
        specified regular expression.
      - If the first match is required, use(firstmatch=yes).
      - A special value is available; C(EOF) for inserting the line at the end
        of the USS file or data set.
      - If the specified regular expression has no matches, EOF will be used
        instead.
      - If C(insertbefore) is set, default value C(EOF) will be ignored.
      - If regular expressions are passed to both C(regexp) and C(insertafter),
        C(insertafter) is only honored if no match for C(regexp) is found.
      - May not be used with C(backrefs) or C(insertbefore).
      - Choices are EOF or '*regex*'
      - Default is EOF
    required: false
    type: str
    aliases: [ after ]
  insertbefore:
    description:
      - Used with C(state=present).
      - If specified, the line will be inserted before the last match of
        specified regular expression.
      - If the first match is required, use C(firstmatch=yes).
      - A value is available; C(BOF) for inserting the line at the beginning of
        the USS file or data set.
      - If the specified regular expression has no matches, the line will be
        inserted at the end of the USS file or data set.
      - If regular expressions are passed to both C(regexp) and
        C(insertbefore), C(insertbefore) is only honored if no match for
        C(regexp) is found.
      - May not be used with C(backrefs) or C(insertafter).
      - Choices are BOF or '*regex*'
    required: false
    type: str
    aliases: [ before ]
  backup:
    description:
      - Creates a backup file or backup data set for I(src), including the
        timestamp information to ensure that you retrieve the original file.
      - I(backup_name) can be used to specify a backup file name
        if I(backup=true).
      - The backup file name will be return on either success or failure
        of module execution such that data can be retrieved.
      - Use generation data set (GDS) relative positive name SOME.CREATION(+1)
    required: false
    type: bool
    default: false
  backup_name:
    description:
      - Specify the USS file name or data set name for the destination backup.
      - If the source I(src) is a USS file or path, the backup_name must be a file
        or path name, and the USS file or path must be an absolute path name.
      - If the source is an MVS data set, the backup_name must be an MVS
        data set name.
      - If the backup_name is not provided, the default backup_name will
        be used. If the source is a USS file or path, the name of the backup
        file will be the source file or path name appended with a
        timestamp, e.g. C(/path/file_name.2020-04-23-08-32-29-bak.tar).
      - If the source is an MVS data set, it will be a data set with a random
        name generated by calling the ZOAU API. The MVS backup data set
        recovery can be done by renaming it.
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
  firstmatch:
    description:
      - Used with C(insertafter) or C(insertbefore).
      - If set, C(insertafter) and C(insertbefore) will work with the first
        line that matches the given regular expression.
    required: false
    type: bool
    default: no
  encoding:
    description:
      - The character set of the source I(src). L(zos_lineinfile,./zos_lineinfile.html)
        requires to be provided with correct encoding to read the content
        of USS file or data set. If this parameter is not provided, this
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
  - It is the playbook author or user's responsibility to avoid files
    that should not be encoded, such as binary files. A user is described
    as the remote user, configured either for the playbook or playbook
    tasks, who can also obtain escalated privileges to execute as root
    or another user.
  - All data sets are always assumed to be cataloged. If an uncataloged data set
    needs to be encoded, it should be cataloged first.
  - For supported character sets used to encode data, refer to the
    L(documentation,https://ibm.github.io/z_ansible_collections_doc/ibm_zos_core/docs/source/resources/character_set.html).
"""

EXAMPLES = r"""
- name: Ensure value of a variable in the sequential data set
  zos_lineinfile:
    src: SOME.DATA.SET
    regexp: '^VAR='
    line: VAR="some value"

- name: Remove all comments in the USS file
  zos_lineinfile:
    src: /tmp/src/somefile
    state: absent
    regexp: '^#'

- name: Ensure the https port is 8080
  zos_lineinfile:
    src: /tmp/src/somefile
    regexp: '^Listen '
    insertafter: '^#Listen '
    line: Listen 8080

- name: Ensure we have our own comment added to the partitioned data set member
  zos_lineinfile:
    src: SOME.PARTITIONED.DATA.SET(DATA)
    regexp: '#^VAR='
    insertbefore: '^VAR='
    line: '# VAR default value'

- name: Ensure the user working directory for liberty is set as needed
  zos_lineinfile:
    src: /tmp/src/somefile
    regexp: '^(.*)User(\d+)m(.*)$'
    line: '\1APPUser\3'
    backrefs: true

- name: Add a line to a member while a task is in execution
  zos_lineinfile:
    src: SOME.PARTITIONED.DATA.SET(DATA)
    insertafter: EOF
    line: 'Should be a working test now'
    force: true

- name: Add a line to a gds
  zos_lineinfile:
    src: SOME.CREATION(-2)
    insertafter: EOF
    line: 'Should be a working test now'

- name: Add a line to dataset and backup in a new generation of gds
  zos_lineinfile:
    src: SOME.CREATION.TEST
    insertafter: EOF
    backup: true
    backup_name: CREATION.GDS(+1)
    line: 'Should be a working test now'
"""

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
  description: constructed dsed shell cmd based on the parameters
  returned: success
  type: str
  sample: dsedhelper -d -en IBM-1047 /^PATH=/a\\PATH=/dir/bin:$PATH/$ /etc/profile
msg:
  description: The module messages
  returned: failure
  type: str
  sample: Parameter verification failed
stdout:
  description: The stdout from ZOAU dsed command.
  returned: always
  type: str
stderr:
  description: The error messages from ZOAU dsed
  returned: always
  type: str
  sample: BGYSC1311E Iconv error, cannot open converter from ISO-88955-1 to IBM-1047
stdout_lines:
    description: List of strings containing individual lines from stdout.
    returned: always
    type: list
stderr_lines:
    description: List of strings containing individual lines from stderr.
    returned: always
    type: list
backup_name:
    description: Name of the backup file or data set that was created.
    returned: if backup=true
    type: str
    sample: /path/to/file.txt.2015-02-03@04:15~
"""
import json
import traceback
from ansible.module_utils.basic import AnsibleModule
from ansible_collections.ibm.ibm_zos_core.plugins.module_utils import (
    better_arg_parser, data_set, backup as Backup)
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


# supported data set types
DS_TYPE = ['PS', 'PO']


def present(src, line, regexp, ins_aft, ins_bef, encoding, first_match, backrefs, force):
    """Replace a line with the matching regex pattern.
    Insert a line before/after the matching pattern.
    Insert a line at BOF/EOF.

    Parameters
    ----------
    src : str
        The z/OS USS file or data set to modify.
    line : str
        The line to insert/replace into the src.
    regexp : str
        The regular expression to look for in every line of the src.
        If regexp matches, ins_aft/ins_bef will be ignored.
    ins_aft : str
        Insert the line after matching '*regex*' pattern or EOF.
        choices:
          - EOF
          - '*regex*'
    ins_bef : str
        Insert the line before matching '*regex*' pattern or BOF.
        choices:
          - BOF
          - '*regex*'
    encoding : str
        Encoding of the src.
    first_match : bool
        Take the first matching regex pattern.
    backrefs : bool
        Back reference.
    force : bool
        force for modify a member part of a task in execution.

    Returns
    -------
    str
        Information in JSON format. keys:
        cmd {str} -- dsed shell command
        found {int} -- Number of matching regex pattern
        changed {bool} -- Indicates if the source was modified.
    """
    return datasets.lineinfile(
        src,
        line,
        regex=regexp,
        insert_after=ins_aft,
        insert_before=ins_bef,
        encoding=encoding,
        first_match=first_match,
        backref=backrefs,
        state=True,
        debug=True,
        force=force,
    )


def absent(src, line, regexp, encoding, force):
    """Delete lines with matching regex pattern.

    Parameters
    ----------
    src : str
        The z/OS USS file or data set to modify.
    line : str
        The line to be deleted in the src. If line matches,
        regexp will be ignored.
    regexp : str
        The regular expression to look for in every line of the src.
    encoding : str
        Encoding of the src.
    force : bool
        Force for modify a member part of a task in execution.

    Returns
    -------
    str
        Information in JSON format. keys:
        cmd {str} -- dsed shell command
        found {int} -- Number of matching regex pattern
        changed {bool} -- Indicates if the source was modified.
    """
    return datasets.lineinfile(src, line, regex=regexp, encoding=encoding, state=False, debug=True, force=force)


def execute_dsed(src, state, encoding, module, line=False, first_match=False, force=False, backrefs=False, regex=None, ins_bef=None, ins_aft=None):
    """Execute in terminal dsed command directly

    Parameters
    ----------
    src : str
        The z/OS USS file or data set to modify.
    state : bool
        Determine if will add or delete the line.
    encoding : str
        Encoding of the src.
    module : obj
        Object to execute the command.
    line : str
        The line to insert/replace into the src.
    regex : str
        The regular expression to look for in every line of the src.
        If regexp matches, ins_aft/ins_bef will be ignored.
    ins_aft : str
        Insert the line after matching '*regex*' pattern or EOF.
        choices:
          - EOF
          - '*regex*'
    ins_bef : str
        Insert the line before matching '*regex*' pattern or BOF.
        choices:
          - BOF
          - '*regex*'
    first_match : bool
        Take the first matching regex pattern.
    backrefs : bool
        Back reference.
    force : bool
        force for modify a member part of a task in execution.

    Returns
    -------
    int
        RC of the execution of the command.
    cmd
        Command executed.
    stdout
        Stdout of the command execution.
    """
    options = ""
    force = " -f " if force else ""
    backrefs = " -r " if backrefs else ""
    encoding = " -c {0} ".format(encoding)
    match = "1" if first_match else "$"

    if state:
        if regex:
            if ins_aft:
                if ins_aft == "EOF" or ins_aft == "eof":
                    options += f' -s -e "/{regex}/c\\{line}/{match}" -e "$ a\\{line}" "{src}" '
                else:
                    options += f' -s -e "/{regex}/c\\{line}/{match}" -e "/{ins_aft}/a\\{line}/{match}" -e "$ a\\{line}" "{src}" '

            elif ins_bef:
                if ins_bef == "BOF" or ins_aft == "bof":
                    options += f' -s -e "/{regex}/c\\{line}/{match}" -e "1 i\\{line}" "{src}" '
                else:
                    options += f' -s -e "/{regex}/c\\{line}/{match}" -e "/{ins_bef}/i\\{line}/{match}" -e "$ a\\{line}" "{src}" '
            else:
                options += f' "/{regex}/c\\{line}/{match}" "{src}" '
        else:
            if ins_aft:
                if ins_aft == "EOF" or ins_aft == "eof":
                    options += f' "$ a\\{line}" "{src}" '
                else:
                    options += f' -s -e "/{ins_aft}/a\\{line}/{match}" -e "$ a\\{line}" "{src}" '
            elif ins_bef:
                if ins_bef == "BOF" or ins_aft == "bof":
                    options += f' "1 i\\{line}" "{src}" '
                else:
                    options += f' -s -e "/{ins_bef}/i\\{line}/{match}" -e "$ a\\{line}" "{src}" '
            else:
                raise ValueError("Incorrect parameters required regex and/or ins_aft or ins_bef")
    else:
        if regex:
            if line:
                options += f'-s -e "/{regex}/d" -e "/{line}/d" "{src}" '
            else:
                options += f'"/{line}/d" "{src}" '
        else:
            options += f'"/{line}/d" "{src}" '

    cmd = "dsed {0}{1}{2}{3}".format(force, backrefs, encoding, options)

    rc, stdout, stderr = module.run_command(cmd, errors='replace')
    cmd = clean_command_output(cmd)
    return rc, cmd, stdout


def clean_command_output(cmd):
    """Deletes escaped characters from the str.

    Parameters
    ----------
    cmd : str
        Command to clean any escaped characters.

    Returns
    -------
    str
        Command without escaped character.
    """
    cmd = cmd.replace('/c\\\\', '')
    cmd = cmd.replace('/a\\\\', '', )
    cmd = cmd.replace('/i\\\\', '', )
    cmd = cmd.replace('$ a\\\\', '', )
    cmd = cmd.replace('1 i\\\\', '', )
    cmd = cmd.replace('/c\\', '')
    cmd = cmd.replace('/a\\', '')
    cmd = cmd.replace('/i\\', '')
    cmd = cmd.replace('$ a\\', '')
    cmd = cmd.replace('1 i\\', '')
    cmd = cmd.replace('/d', '')
    cmd = cmd.replace('\\\\d', '')
    cmd = cmd.replace('\\n', '\n')
    cmd = cmd.replace('\\"', '"')
    return cmd


def check_special_characters(src):
    """Verify if the string contains special characters
    such as $ @ # -.

    Parameters
    ----------
    string : str
        Given string.

    Returns
    -------
    bool
        If the string match any special character.
    """
    special_characters = ['$', '@', '#', '-']
    return any(character in special_characters for character in src)


def quotedString(string):
    """Add escape if string was quoted.

    Parameters
    ----------
    string : str
        Given string.

    Returns
    -------
    str
        The string with the quote marks replaced.
    """
    # add escape if string was quoted
    if not isinstance(string, str):
        return string
    return string.replace('"', '\\\"')


def main():
    """Initialize the module.

    Raises
    ------
    fail_json
        Parameter verification failed.
    fail_json
        regexp is required with backrefs=true.
    fail_json
        line is required with state=present.
    fail_json
        One of line or regexp is required with state=absent.
    fail_json
        Source does not exist.
    fail_json
        Data set type is NOT supported.
    fail_json
        Creating backup has failed.
    fail_json
        dsed return content is NOT in json format.
    """
    module_args = dict(
        src=dict(
            type='str',
            aliases=['path', 'destfile', 'name'],
            required=True
        ),
        state=dict(
            type='str',
            default='present',
            choices=['absent', 'present']
        ),
        regexp=dict(type='str'),
        line=dict(type='str'),
        insertafter=dict(
            type='str',
            aliases=['after']
        ),
        insertbefore=dict(
            type='str',
            aliases=['before']
        ),
        backrefs=dict(type='bool', default=False),
        backup=dict(type='bool', default=False),
        backup_name=dict(type='str', required=False, default=None),
        firstmatch=dict(type='bool', default=False),
        encoding=dict(type='str', default="IBM-1047"),
        tmp_hlq=dict(type='str', required=False, default=None),
        force=dict(type='bool', required=False, default=False)
    )
    module = AnsibleModule(
        argument_spec=module_args,
        supports_check_mode=True
    )
    validate_dependencies(module)
    result = dict(changed=False, cmd='', found=0)

    arg_defs = dict(
        src=dict(arg_type="data_set_or_path", aliases=['path', 'destfile', 'name'], required=True),
        state=dict(arg_type="str", default='present', choices=['absent', 'present']),
        regexp=dict(arg_type="str", required=False),
        line=dict(arg_type="str", required=False),
        insertafter=dict(arg_type="str", required=False, aliases=['after']),
        insertbefore=dict(arg_type="str", required=False, aliases=['before']),
        encoding=dict(arg_type="str", default="IBM-1047", required=False),
        backup=dict(arg_type="bool", default=False, required=False),
        backup_name=dict(arg_type="data_set_or_path", required=False, default=None),
        firstmatch=dict(arg_type="bool", required=False, default=False),
        backrefs=dict(arg_type="bool", dependencies=['regexp'], required=False, default=False),
        tmp_hlq=dict(type='qualifier_or_empty', required=False, default=None),
        force=dict(arg_type='bool', required=False, default=False),
        mutually_exclusive=[["insertbefore", "insertafter"]],)

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
    backrefs = parsed_args.get('backrefs')
    src = parsed_args.get('src')
    firstmatch = parsed_args.get('firstmatch')
    regexp = parsed_args.get('regexp')
    line = parsed_args.get('line')
    ins_aft = parsed_args.get('insertafter')
    ins_bef = parsed_args.get('insertbefore')
    encoding = parsed_args.get('encoding')
    tmphlq = parsed_args.get('tmp_hlq')
    force = parsed_args.get('force')

    if parsed_args.get('state') == 'present':
        if backrefs and regexp is None:
            module.fail_json(msg='regexp is required with backrefs=true')
        if line is None:
            module.fail_json(msg='line is required with state=present')
        # set the default to EOF, if regexp/insertafter/insertbefore are None
        if regexp is None and ins_aft is None and ins_bef is None:
            ins_aft = "EOF"
    else:
        if regexp is None and line is None:
            module.fail_json(msg='one of line or regexp is required with state=absent')

    is_gds = False
    has_special_chars = False
    dmod_exec = False
    rc = 0
    stdout = ''
    stderr = ''
    cmd = ''
    changed = False
    return_content = None

    result = dict(
        changed=False,
        cmd='',
        found=0,
        stdout='',
        stdout_lines=[],
        stderr='',
        stderr_lines=[],
        rc=0,
        backup_name='',
    )

    # analysis the file type
    if "/" not in src:
        dataset = data_set.MVSDataSet(
            name=src
        )
        src = dataset.name
        is_gds = dataset.is_gds_active

    if data_set.DataSet.is_gds_relative_name(src) and is_gds is False:
        module.fail_json(msg="{0} does not exist".format(src))

    ds_utils = data_set.DataSetUtils(src, tmphlq=tmphlq)

    # Check if dest/src exists
    if not ds_utils.exists():
        module.fail_json(msg="{0} does not exist".format(src))

    file_type = ds_utils.ds_type()
    if file_type != "USS":
        has_special_chars = check_special_characters(src)
        if file_type not in DS_TYPE:
            message = "{0} data set type is NOT supported".format(str(file_type))
            module.fail_json(msg=message)

    dmod_exec = has_special_chars or is_gds
    # make sure the default encoding is set if null was passed
    if not encoding:
        encoding = "IBM-1047"
    if backup:
        if isinstance(backup, bool):
            backup = None
        try:
            if file_type == "USS":
                result['backup_name'] = Backup.uss_file_backup(src, backup_name=backup, compress=False)
            else:
                result['backup_name'] = Backup.mvs_file_backup(dsn=src, bk_dsn=backup, tmphlq=tmphlq)
        except Exception:
            module.fail_json(msg="creating backup has failed")
    # state=present, insert/replace a line with matching regex pattern
    # state=absent, delete lines with matching regex pattern
    if parsed_args.get('state') == 'present':
        if dmod_exec:
            rc, cmd, stdout = execute_dsed(src, state=True, encoding=encoding, module=module, line=line, first_match=firstmatch,
                                           force=force, backrefs=backrefs, regex=regexp, ins_bef=ins_bef, ins_aft=ins_aft)
            stderr = 'Failed to insert new entry' if rc != 0 else ""
            changed = True if rc == 0 else False
        else:
            return_content = present(src, quotedString(line), quotedString(regexp), quotedString(ins_aft), quotedString(ins_bef), encoding, firstmatch,
                                     backrefs, force)
    else:
        if dmod_exec:
            rc, cmd, stdout = execute_dsed(src, state=False, encoding=encoding, module=module, line=line, first_match=firstmatch, force=force,
                                           backrefs=backrefs, regex=regexp, ins_bef=ins_bef, ins_aft=ins_aft)
            stderr = 'Failed to insert new entry' if rc != 0 else ""
            changed = True if rc == 0 else False
        else:
            return_content = absent(src, quotedString(line), quotedString(regexp), encoding, force)
    if not dmod_exec:
        stdout = return_content.stdout_response
        stderr = return_content.stderr_response
        rc = return_content.rc
        stdout = stdout.replace('/c\\', '/c\\\\')
        stdout = stdout.replace('/a\\', '/a\\\\')
        stdout = stdout.replace('/i\\', '/i\\\\')
        stdout = stdout.replace('$ a\\', '$ a\\\\')
        stdout = stdout.replace('1 i\\', '1 i\\\\')
        stdout = stdout.replace('/d', '\\\\d')
        if line:
            stdout = stdout.replace(line, quotedString(line))
        if regexp:
            stdout = stdout.replace(regexp, quotedString(regexp))
        if ins_aft:
            stdout = stdout.replace(ins_aft, quotedString(ins_aft))
        if ins_bef:
            stdout = stdout.replace(ins_bef, quotedString(ins_bef))
        try:
            # Attempt to parse stdout content directly as JSON
            ret = json.loads(stdout)
        except json.JSONDecodeError:
            try:
                # If parsing fails, clean up possible wrapping quotes
                if (stdout.startswith("'") and stdout.endswith("'")) or (stdout.startswith('"') and stdout.endswith('"')):
                    cleaned_stdout = stdout[1:-1]
                else:
                    cleaned_stdout = stdout
                # Replace escaped backslashes with single backslashes
                cleaned_stdout = cleaned_stdout.replace('\\\\', '\\')
                # Further clean up: escape any single backslashes not followed by a valid JSON escape character
                import re
                cleaned_stdout = re.sub(r'\\([^"\\/bfnrtu])', r'\\\\\1', cleaned_stdout)
                # Try parsing the cleaned string as JSON
                ret = json.loads(cleaned_stdout)
            except Exception as e:
                # If still failing, report failure with useful debug info
                messageDict = dict(
                    msg=f"Failed while parsing command output {str(e)} ",
                    stdout=str(stdout),
                    stderr=str(stderr),
                    rc=rc
                )
                if result.get('backup_name'):
                    messageDict['backup_name'] = result['backup_name']
                module.fail_json(**messageDict)
        # If the parsed JSON has a 'cmd' key, clean its string for safe use
        if 'cmd' in ret:
            ret['cmd'] = ret['cmd'].replace('\\"', '"').replace('\\\\', '\\')
            result['cmd'] = ret['cmd']
        changed = ret.get('changed', False)
        result['found'] = ret.get('found', 0)
        result['stdout'] = stdout
    result['changed'] = changed
    result['rc'] = rc
    result['cmd'] = cmd
    result['stdout'] = str(stdout)
    result['stderr'] = str(stderr)
    result['stdout_lines'] = result['stdout'].splitlines()
    result['stderr_lines'] = result['stderr'].splitlines()
    module.exit_json(**result)


if __name__ == '__main__':
    main()
