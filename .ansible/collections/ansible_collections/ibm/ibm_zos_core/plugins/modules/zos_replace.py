#!/usr/bin/python
# -*- coding: utf-8 -*-

# Copyright (c) IBM Corporation 2025, 2026
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
module: zos_replace
version_added: '1.15.0'
short_description: Replace all instances of a pattern within a file or data set.
description:
  - The module L(zos_replace.,/zos_replace.html) can replace all instances of a pattern in the contents of a data set.
author:
  - "Marcel Gutierrez (@AndreMarcel99)"
options:
  after:
    description:
      - A regular expression that, if specified, determines which content will be replaced or removed B(after) the match.
      - Option I(after) is the start position from where the module will seek to match the I(regexp) pattern.
        When a pattern is matched, occurrences are substituted with the value set for I(replace).
      - If option I(after) is not set, the module will search from the beginning of the I(target).
      - Option I(after) is a regular expression as described in the L(Python library,https://docs.python.org/3/library/re.html).
      - Option I(after) can be used in combination with I(before).
        When combined with I(before), patterns are replaced or removed from I(after) until the value set for I(before).
      - Option I(after) can be interpreted as a literal string instead of a regular expression by setting option I(literal=after).
    required: false
    default: ''
    type: str
  backup:
    description:
      - Specifies whether a backup of the destination should be created before
        editing the source I(target).
      - When set to C(true), the module creates a backup file or data set.
      - The backup file name will be returned if I(backup) is C(true)
        on either success or failure of module execution such that data can be retrieved.
    required: false
    type: bool
    default: false
  backup_name:
    description:
      - Specify the USS file name or data set name for the destination backup.
      - If I(src) is a USS file or path, backup_name must be a file
        or path name, and it must be an absolute path name.
      - If the source is an MVS data set, I(backup_name) must be an MVS
        data set name, and the data set must B(not) be preallocated.
      - If it is a Generation Data Set (GDS), use a relative positive name, e.g., I(SOME.CREATION(+1)).
      - If I(backup_name) is not provided, a default name will
        be used. If the source is a USS file or path, the name of the backup
        file will be the source file or path name appended with a
        timestamp, e.g. C(/path/file_name.2020-04-23-08-32-29-bak.tar).
      - If I(src) is a seq data set and backup_name is not provided, the data set
        will be backed up to seq data set with a randomly generated name.
      - If I(src) is a data set member and backup_name is not provided, the data set
        member will be backed up to the same partitioned data set with a randomly generated
        member name.
      - If I(src) is a Generation Data Set (GDS) and backup_name is not provided, backup
        will be a sequential data set.
    required: false
    type: str
  before:
    description:
      - A regular expression that if, specified, determines which content will be replaced or removed B(before) the match.
      - Option I(before) is the end position from where the module will seek to match the I(regexp) pattern.
        When a pattern is matched, occurrences are substituted with the value set for I(replace).
      - If option I(before) is not set, the module will search to the end of the I(target).
      - Option I(before) is a regular expression as described in the L(Python library,https://docs.python.org/3/library/re.html).
      - Option I(before) can be used in combination with I(after).
        When combined with I(after), patterns are replaced or removed from I(after) until the value set for I(before).
      - Option I(before) can be interpreted as a literal string instead of a regular expression by setting option I(literal=before).
    required: false
    default: ''
    type: str
  encoding:
    description:
      - The character set for data in the I(target). Module L(zos_replace,./zos_replace.html)
        requires the encoding to correctly read the content
        of a USS file or data set. If this parameter is not provided, this
        module assumes that USS file or data set is encoded in IBM-1047.
      - Supported character sets rely on the charset conversion utility (iconv)
        version; the most common character sets are supported.
    required: false
    type: str
    default: IBM-1047
  literal:
    description:
      - If specified, it enables the module to interpret options I(after), I(before) and I(regexp) as a literal rather than a regular expression.
      - Option I(literal) uses any combination of V(after), V(before) and V(regexp).
      - To interpret one option as a literal, use I(literal=regexp), I(literal=after) or I(literal=before).
      - To interpret multiple options as a literal, use a list such as C(['after', 'before']) or C(['regex', 'after', 'before'])
    required: false
    default: []
    type: raw
  target:
    description:
      - The location can be a UNIX System Services (USS) file,
        PS (sequential data set), PDS, PDSE, member of a PDS or PDSE.
      - The USS file must be an absolute pathname.
      - It is possible to use a generation data set (GDS) relative name of generation already
        created. e.g. I(SOME.CREATION(-1)).
    required: true
    type: str
    aliases: [ src, path, destfile ]
  tmp_hlq:
    description:
      - Override the default High Level Qualifier (HLQ) for temporary and backup
        data sets.
      - The default HLQ is the Ansible user used to execute the module and if
        that is not available, then the value of C(TMPHLQ) is used.
    required: false
    type: str
  regexp:
    description:
      - The regular expression to look for in the contents of the file.
    required: true
    type: str
  replace:
    description:
      - The string to replace I(regexp) matches with.
      - If not set, matches are removed entirely.
    required: false
    type: str
    default: ''

notes:
  - For supported character sets used to encode data, refer to the
    L(documentation,https://ibm.github.io/z_ansible_collections_doc/ibm_zos_core/docs/source/resources/character_set.html).
  - Whitespaces at the end of line will be ignored in order for regex to work.
"""

EXAMPLES = r"""
- name: Replace 'profile/' pattern in USS file via blank substitution.
  zos_replace:
    target: /tmp/src/somefile
    regexp: 'profile\/'

- name: Replace regexp match with blank after line match in USS file.
  zos_replace:
    target: "/tmp/source"
    regexp: '^MOUNTPOINT*'
    after: export ZOAU_ROOT

- name: Replace a specific line with special character on a dataset after a line, treating the text specified
    for regexp as a literal string and after as regular expression.
  zos_replace:
    target: SAMPLE.SOURCE
    regexp: //*LIB  DD UNIT=SYS,SPACE=(TRK,(1,1)),VOL=SER=vvvvvv
    replace: //*LIB  DD UNIT=SYS,SPACE=(CYL,(1,1))
    after: '^\$source base \([^\s]+\)'
    literal: regexp

- name: Replace a specific line with special character on a dataset after a line, treating the text specified
    for regexp and after as regular expression.
  zos_replace:
    target: SAMPLE.SOURCE
    regexp: '\ \*\*LIB\ \ DD\ UNIT=SYS,SPACE=\(TRK,\(1,1\)\),VOL=SER=vvvvvv'
    replace: //*LIB  DD UNIT=SYS,SPACE=(CYL,(1,1))
    after: '^\$source base \([^\s]+\)'
    literal: regexp

- name: Replace a specific line before a specific sentence with backup, treating the text specified for regexp and before as literal strings.
  zos_replace:
    target: SAMPLE.SOURCE
    backup: true
    regexp: //SYSPRINT DD SYSOUT=*
    before: SAMPLES OUTPUT SYSIN *=$DSN
    literal:
      - regexp
      - before

- name: Replace a specific line before a specific sentence with backup, treating the text specified for regexp and before as regular expression.
  zos_replace:
    target: SAMPLE.SOURCE
    backup: true
    regexp: '\ //SYSPRINT\ DD\ SYSOUT=\*'
    before: '\ SAMPLES OUTPUT SYSIN\ \*\=\$DSN'

- name: Replace 'var' with 'vars' between matched lines after and before with backup.
  zos_replace:
    target: SAMPLE.DATASET
    tmp_hlq: ANSIBLE
    backup: true
    backup_name: BACKUP.DATASET
    regexp: var
    replace: vars
    after: ^/tmp/source*
    before: ^   if*

- name: Replace lines on a GDS and generate a backup on the same GDG.
  zos_replace:
    target: SOURCE.GDG(0)
    regexp: ^(IEE132I|IEA989I|IEA888I|IEF196I|IEA000I)\s.*
    after: ^IEE133I PENDING *
    before: ^IEE252I DEVICE *
    backup: true
    backup_name: "SOURCE.GDG(+1)"

- name: Delete 'SYSTEM' calls via backref between matched lines in a PDS member.
  zos_replace:
    target: PDS.SOURCE(MEM)
    regexp: '^(.*?SYSTEM.*?)SYSTEM(.*)'
    replace: '\1\2'
    after: IEE133I PENDING *
    before: IEF456I JOB12345 *
"""

RETURN = r"""
backup_name:
    description: Name of the backup file or data set that was created.
    returned: if backup=true
    type: str
    sample: "/path/to/file.txt.2015-02-03@04:15"
changed:
    description:
        Indicates if the source was modified.
    returned: always
    type: bool
    sample: True
found:
    description: Number of matches found
    returned: success
    type: int
    sample: 5
msg:
    description: A string with a generic or error message relayed to the user.
    returned: failure
    type: str
    sample: Parameter verification failed
replaced:
    description: Fragment of the file that was changed
    returned: always
    type: str
    sample: IEE134I TRACE DISABLED - MONITORING STOPPED
target:
    description: The data set name or USS path that was modified.
    returned: always
    type: str
    sample: ANSIBLE.USER.TEXT
"""

import os
import re
import io
import codecs
import shutil
import tempfile
import traceback
from ansible.module_utils.basic import AnsibleModule

from ansible_collections.ibm.ibm_zos_core.plugins.module_utils import (
    better_arg_parser,
    data_set,
    backup as Backup
)

from ansible_collections.ibm.ibm_zos_core.plugins.module_utils.import_handler import (
    ZOAUImportError,
)
from ansible_collections.ibm.ibm_zos_core.plugins.module_utils.ansible_module import (
    AnsibleModuleHelper,
)
from ansible_collections.ibm.ibm_zos_core.plugins.module_utils.dependency_checker import (
    validate_dependencies,
)
from ansible_collections.ibm.ibm_zos_core.plugins.module_utils.log import SingletonLogger

try:
    from zoautil_py import zoau_io, datasets
except Exception:
    zoau_io = ZOAUImportError(traceback.format_exc())
    datasets = ZOAUImportError(traceback.format_exc())


def resolve_src_name(module, name, result, tmp_hlq):
    """Function to resolve and validate the existence of the dataset or uss file.

    Parameters
    ----------
        module : object
            Ansible object to execute commands.
        name : str
            Name of the src
        result : object
            Group of vars to display on module fail.
        tmp_hlq : str
            String to resolve data source name.

    Returns
    ----------
        str: Name of the src.

    """
    if "/" in name:
        if os.path.exists(name):
            if os.path.isfile(name):
                return name
            else:
                module.fail_json(rc=256, msg=f"Path {name} is a directory, please specify a file path.", **result)
        else:
            module.fail_json(rc=257, msg=f"USS path {name} does not exist.", **result)
    else:
        try:
            data_set_obj = data_set.MVSDataSet(name=name)
            name = data_set_obj.name
            if not data_set_obj.is_gds_active:
                is_an_alias, base_name = data_set.DataSet.get_name_if_data_set_is_alias(name=name, tmp_hlq=tmp_hlq)
                if is_an_alias:
                    name = base_name
        except Exception:
            message_dict = dict(msg=f"Unable to resolve name of data set {name}.")
            module.fail_json(**message_dict, **result)

        if data_set.DataSet.is_gds_relative_name(name):
            module.fail_json(msg="{0} does not exist".format(name), **result)

        ds_utils = data_set.DataSetUtils(name)
        if not ds_utils.exists():
            module.fail_json(msg=f"{name} does NOT exist.", **result)

        return name


def replace_text(content, regexp, replace, literal=False):
    """Function received the text to work on it to find the regexp
    expected to be replaced.

    Parameters
    ----------
        content : list
            The partial or complete text to be modified.
        regexp : str
            The str that will be search to be replaced.
        replace : str
            The str to replace on the text.
        literal : bool
            Variable to know if is a regex or not.
    Returns
    ----------
        list: the new array of lines with the content replaced
        times_replaced : the number of substitutions made
    """
    module = AnsibleModuleHelper(argument_spec={})
    full_text = "\n".join(content)

    if literal:
        regexp = re.escape(regexp)
        pattern = re.compile(regexp)
    else:
        try:
            pattern = re.compile(regexp, re.MULTILINE)
        except Exception as e:
            module.fail_json(msg=f"Unable to compile regex {regexp}. {e}")

    try:
        modified_text, times_replaced = re.subn(pattern, replace, full_text, 0)
    except Exception as e:
        module.fail_json(msg=f"Bad use of regex {pattern} with replace {replace}. {e}")

    modified_list = modified_text.split("\n")
    modified_list = [line for line in modified_list if line.strip() or line.lstrip()]

    return modified_list, times_replaced


def merge_text(original, replace, begin, end):
    """Function to generate the new full text with the replaced
        text inside of it.
    Args
    ----------
        original : list
            Tue full original text on list
        replace : list
            The fraction of text that was replaced
        begin : int
            Position of the list where start the replace of array
        end : int
            Position of the list where end the replace of array

    Returns
    ----------
        list : The full text on list mode
    """
    replace = [line for line in replace if line.strip() or line.lstrip()]
    if len(replace) != 0:
        # Case for after exist and before dont
        if begin != 0 and end == len(original):
            head_content = original[:begin]
            head_content.extend(replace)
            return head_content

        # Case for before exist and after dont
        elif end != len(original) and begin == 0:
            tail_content = original[end:]
            replace.extend(tail_content)
            return replace

        # Case before and after exists
        else:
            head_content = original[:begin]
            tail_content = original[end:]
            head_content.extend(replace)
            head_content.extend(tail_content)
            return head_content
    else:
        # Case for after exist and before dont
        if begin != 0 and end == len(original):
            head_content = original[:begin]
            return head_content

        # Case for before exist and after dont
        elif end != len(original) and begin == 0:
            tail_content = original[end:]
            return tail_content

        # Case before and after exists
        else:
            head_content = original[:begin]
            tail_content = original[end:]
            head_content.extend(tail_content)
            return head_content


def search_bf_af(text, literal, before, after):
    """Function to get the boundaries of the text that should be searched and replaced.

    Args
    ----------
        text : list
            Text of the file as a list.
        literal : list
            List of values ('before' or 'after') that indicate when to disable regexes.
        before : str
            Str or regex to search where to end the section to replace the text
        after : str
            Str or regex to search where to begin the section to replace the text

    Returns
    ----------
        begin_block_code: Position of the list where to start search
        end_block_code: Position of the list where to end search
        match: If there were a match of any value of before or after
    """
    module = AnsibleModuleHelper(argument_spec={})
    lit_af = False
    lit_bf = False

    if literal:
        lit_af = True if "after" in literal else False
        lit_bf = True if "before" in literal else False

    if bool(before):
        if lit_bf:
            pattern_before = re.escape(before)
            pattern_end = re.compile(pattern_before) if before else before
        else:
            pattern_before = u''
            pattern_before = u'(?P<subsection>.*)%s' % before
            try:
                pattern_end = re.compile(pattern_before, re.DOTALL)
            except Exception as e:
                module.fail_json(msg=f"Unable to compile regex {before}. {e}")

    if bool(after):
        if lit_af:
            pattern_after = re.escape(after)
            pattern_begin = re.compile(pattern_after) if after else after
        else:
            pattern_after = u''
            pattern_after = u'%s(?P<subsection>.*)' % after
            try:
                pattern_begin = re.compile(pattern_after, re.DOTALL) if after else after
            except Exception as e:
                module.fail_json(msg=f"Unable to compile regex {after}. {e}")

    begin_block_code = 0
    end_block_code = len(text)

    line_counter = 0
    search_after = bool(after)
    search_before = False if search_after else True
    match = False

    for line in text:
        if search_after:
            if pattern_begin.match(line) is not None:
                begin_block_code = line_counter + 1
                match = True
                if not before:
                    break
                else:
                    search_before = True
                    search_after = False

        if search_before:
            if pattern_end.match(line) is not None:
                end_block_code = line_counter
                match = True
                break

        line_counter += 1

    return begin_block_code, end_block_code, match


def open_file(file, encoding, uss):
    """
    Args
    ----------
        file : str
            Name of the file to extract the text.
        encoding : str
            Type of encoding to decode the text.
        uss : bool
            How to open the file to extract the text.

    Returns
    ----------
        list : The list with the content of the file.
    """
    decode_list = []

    if uss:
        with io.open(file, "rb") as content_file:
            content = codecs.decode(content_file.read(), encoding)
        decode_list = content.splitlines()
    else:
        with zoau_io.RecordIO(f"//'{file}'") as dataset_read:
            dataset_content = dataset_read.readrecords()
        # As in ZOAU 1.4.0 on reading dataset we are getting extra whitespace so we need rstrip for regex to work.
        decode_list = [codecs.decode(record, encoding).rstrip() for record in dataset_content]

    return decode_list


def replace_func(file, regexp, replace, module, uss, literal, encoding="cp1047", after=None, before=None):
    """Function to extract from the uss a fragment or the full text to be replaced and replace the content.

    Args
    ----------
        file : str
            Uss path.
        regexp : str
            Regex expression to search.
        replace : str
            Regex expression or text to replace.
        module : obj
            Object of Ansible to access to the facilities
        uss : bool
            Variable to indicate the type of file to open for the module. Either uss or data set.
        literal : list
            List of values to be used as string disabling regex option.
        encoding : str, optional
            Encoding to use en decoding content.
            Defaults to "cp1047".
        after : str, optional
            Str or regex to search where to start the section to replace on the text.
            Defaults to "".
        before : str, optional
            Str or regex to search where to end the section to replace on the text.
            Defaults to "".

    Returns
    ----------
        list : List with the new text with the replace expected.
    """
    decode_list = open_file(file, encoding, uss)

    is_regex = False

    if literal:
        is_regex = True if "regexp" in literal else False

    if not after and not before:
        new_full_text, replaced = replace_text(content=decode_list, regexp=regexp, replace=replace, literal=is_regex)
        return new_full_text, replaced, new_full_text

    begin_block_code, end_block_code, match = search_bf_af(text=decode_list, literal=literal, before=before, after=after)

    if not match:
        module.fail_json(msg="Pattern for before/after params did not match the given file.")

    new_text, replaced = replace_text(content=decode_list[begin_block_code:end_block_code],
                                      regexp=regexp, replace=replace, literal=is_regex)
    full_new_text = merge_text(original=decode_list, replace=new_text, begin=begin_block_code, end=end_block_code)
    return full_new_text, replaced, new_text


def run_module():
    module = AnsibleModule(
        argument_spec=dict(
            after=dict(type='str', default=''),
            backup=dict(type='bool', default=False, required=False),
            backup_name=dict(type='str', default=None, required=False),
            before=dict(type='str', default=''),
            encoding=dict(type='str', default='IBM-1047', required=False),
            target=dict(type="str", required=True, aliases=['src', 'path', 'destfile']),
            tmp_hlq=dict(type='str', required=False, default=None),
            literal=dict(type="raw", required=False, default=[]),
            regexp=dict(type="str", required=True),
            replace=dict(type='str', default=""),
        ),
        supports_check_mode=False
    )
    validate_dependencies(module)
    args_def = dict(
        after=dict(type='str', default=''),
        backup=dict(type='bool', default=False, required=False),
        backup_name=dict(type='data_set_or_path', default=None, required=False),
        before=dict(type='str', default=''),
        encoding=dict(type='str', default='IBM-1047', required=False),
        target=dict(type="data_set_or_path", required=True, aliases=['src', 'path', 'destfile']),
        tmp_hlq=dict(type='qualifier_or_empty', required=False, default=None),
        literal=dict(type=literals, required=False, default=[]),
        regexp=dict(type="str", required=True),
        replace=dict(type='str', default=""),
    )

    try:
        parser = better_arg_parser.BetterArgParser(args_def)
        parsed_args = parser.parse_args(module.params)
        module.params = parsed_args
    except ValueError as err:
        module.fail_json(
            msg='Parameter verification failed.',
            stderr=str(err)
        )

    # Initialize logging module
    module_verbosity_level = module._verbosity
    SingletonLogger().get_logger(module_verbosity_level)

    src = module.params.get("target")

    result = dict()
    changed = False
    result["target"] = src
    result["changed"] = changed

    tmp_hlq = parsed_args.get('tmp_hlq')
    src = resolve_src_name(module=module, name=src, result=result, tmp_hlq=tmp_hlq)
    uss = True if "/" in src else False
    after = module.params.get("after")
    before = module.params.get("before")
    regexp = module.params.get("regexp")
    replace = module.params.get("replace")
    encoding = module.params.get("encoding")
    if not encoding or encoding == "IBM-1047":
        encoding = "cp1047"
    backup = module.params.get("backup")
    backup_name = parsed_args.get('backup_name')
    literal = module.params.get("literal")

    if literal:
        if "after" in literal and not after:
            module.fail_json(msg="Use of literal requires the use of the after option too.", **result)
        if "before" in literal and not before:
            module.fail_json(msg="Use of literal requires the use of the before option too.", **result)

    if backup:
        try:
            if uss:
                result['backup_name'] = Backup.uss_file_backup(src, backup_name=backup_name, compress=False)
            else:
                backup_ds = Backup.mvs_file_backup(dsn=src, bk_dsn=backup_name, tmphlq=tmp_hlq)
                result['backup_name'] = backup_ds
                if data_set.DataSet.is_gds_relative_name(backup_ds):
                    bk_up_obj = data_set.MVSDataSet(name=backup_ds)
                    result['backup_name'] = bk_up_obj.name
        except Exception as err:
            module.fail_json(msg=f"Unable to allocate backup {backup} destination: {str(err)}.", **result)

    if uss:
        full_text, replaced, fragment = replace_func(file=src, regexp=regexp, replace=replace, module=module, uss=uss,
                                                     encoding=encoding, after=after, before=before, literal=literal)
        tmp_file = tempfile.NamedTemporaryFile(delete=False)
        tmp_file = tmp_file.name

        if replaced > 0:
            try:
                content = [line.rstrip() for line in full_text]
                full_text = "\n".join(content)
                with open(tmp_file, 'w', encoding=encoding) as f:
                    f.write(full_text)
            except Exception as e:
                os.remove(tmp_file)
                module.fail_json(
                    msg=f"Unable to write on data set {src}. {e}",
                )

            try:
                f = open(src, 'r+', encoding=encoding)
                f.truncate(0)
                shutil.copyfile(tmp_file, src)
            finally:
                os.remove(tmp_file)

    else:
        full_text, replaced, fragment = replace_func(file=src, regexp=regexp, replace=replace, module=module, uss=uss,
                                                     encoding=encoding, after=after, before=before, literal=literal)
        if replaced > 0:
            try:
                # zoau_io.zopen on mode w allow delete all the content inside the dataset allowing to write the new one
                with zoau_io.zopen(f"//'{src}'", "w", encoding, recfm="*"):
                    pass
                content = [line.rstrip() for line in full_text]
                full_text = "\n".join(content)
                rc_write = datasets.write(dataset_name=src, content=full_text)
                if rc_write is not None:
                    raise Exception(f"There was an error executing datasets.write rc: {rc_write}")
            except Exception as e:
                module.fail_json(
                    msg=f"Unable to write on data set {src}. {e}",
                    **result
                )

    if replaced > 0:
        changed = True
        result["replaced"] = fragment
    result["found"] = replaced
    result["changed"] = changed

    module.exit_json(**result)


def literals(contents, dependencies):
    """Validate literal arguments.
        This was created for the literal option type of data to accept any
        given combination of arguments because choices only allows one.

    Parameters
    ----------
        contents : Union[str, list[str]]
                 The contents provided for the literal argument.
        dependencies : dict
                     Any arguments this argument is dependent on.

    Raises
    -------
        ValueError : str
                   When invalid argument provided.

    Returns
    -------
        contents : list[str]
                 The contents returned as a list of not required
    """
    allowed_values = {"after", "before", "regexp"}
    if not contents:
        return None
    if contents == []:
        return None
    if not isinstance(contents, list):
        contents = [contents]
    for val in contents:
        if val not in allowed_values:
            raise ValueError(f'Invalid argument "{val}" for type "literal".')
    return contents


def main():
    run_module()


if __name__ == '__main__':
    main()
