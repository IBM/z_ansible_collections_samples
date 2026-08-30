#!/usr/bin/python
# -*- coding: utf-8 -*-
# Copyright (c) IBM Corporation 2019, 2020
# Apache License, Version 2.0 (see https://opensource.org/licenses/Apache-2.0)

from __future__ import (absolute_import, division, print_function)

__metaclass__ = type


DOCUMENTATION = r'''
---
module: ims_dbd_gen
short_description: Generate IMS DBD
version_added: "2.9"
description:
  - This ims_dbd_gen module generates IMS database descriptor (DBD) resource(s) to define a database so that it can be used by IMS application programs.
  - A database descriptor (DBD) is a DL/I control block describing the database, segments, fields, indexes and relationships.
  - Generating a DBD is a two step process that assembles the DBD source and binds it into a database definition.
author:
  Seema Phalke (@sphalke)
  Dipti Gandhi (@ddgandhi)
options:
  src:
    description:
      - The src field can reference a PDS, PDSE member, sequential data set, or UNIX System Services file path.
      - If a PDS is specified, all members within the PDS will be treated as individual DBD source members to be processed.
    type: str
    required: false
  location:
    description:
      - The DBD source location. Supported options are DATA_SET or USS. The default is DATA_SET.
      - The DATA_SET option can be used for a PDS, PDSE, or sequential data set.
    type: str
    required: false
    default: DATA_SET
    choices:
      - DATA_SET
      - USS
  replace:
    description:
      - When 'replace' is 'true', an existing DBD member matching the name in the
      - input DBD source will be overwitten.
    type: bool
    default: true
    required: false
  member_list:
    description:
      - A list of member names if the source specified is a data set.
      - Optionally, proceeding the source_member, a colon with a target name for
        the generated DBD member can be specified. If no target name is
        specified, source_name will be used as the target name.
      - If 'member_list' is empty and location is set to 'DATA_SET' or
        not specified, then src is expected to be a sequential data set.
      - Elements are of the list are str or dict with single key-value
        pair
    type: list
    required: false
  dbd_name:
      description:
        - Target name of the generated DBD member.
        - This parameter is only required and applies if src is a sequential data set.
      type: str
      required: false
  batch:
      description:
        - Batch can be used to perform multiple operations in a single module call.
        - Expects a list of the location(s) of the IMS Database Descriptor (DBD) source to be compiled.
        - The source can reference a PDS or PDSE member, sequential data set or UNIX System Services file path.
      type: list
      elements: dict
      required: false
      suboptions:
        src:
          description:
            - The src field can reference a PDS, PDSE member, sequential data set, or UNIX System Services file path.
            - If a PDS is specified, all members within the PDS will be treated as individual DBD source members to be processed.
          type: str
          required: true
        location:
          description:
            - The DBD source location. Supported options are DATA_SET or USS. The default is DATA_SET.
            - The DATA_SET option can be used for a PDS, PDSE, or sequential data set.
          type: str
          required: true
          default: DATA_SET
          choices:
            - DATA_SET
            - USS
        replace:
          description:
            - When 'replace' is 'true', an existing DBD member matching the name in the
              input DBD source will be overwitten.
          type: bool
          default: true
          required: false
        member_list:
          description:
            - A list of member names if the source specified is a data set.
            - Optionally, proceeding the source_member, a colon with a target name for
              the generated DBD member can be specified. If no target name is
              specified, source_name will be used as the target name.
            - If 'member_list' is empty and location is set to 'DATA_SET' or
              not specified, then src is expected to be a sequential data set.
            - Elements are of the list are str or dict with single key-value
              pair
          type: list
          required: false
        dbd_name:
          description:
            - Target name of the generated DBD member.
            - This parameter is only required and applies if src is a sequential data set.
          type: str
          required: false
  sys_lib:
    description:
      - A list of required macro libraries that are needed to compile the DBD source. These libraries will
        be used as the sys_lib at compile time.
    type: list
    required: true
  dest:
    description:
      - The target output DBDLIB partitioned data set where the DBD members will be generated to.
    type: str
    required: true
notes:
  - Currently ims_dbd_gen does not support copying symbolic links from both local to
    remote and remote to remote.

'''

EXAMPLES = r'''
- name: Basic example of IMS DBDGEN module with a single USS source.
  ims_dbd_gen:
    src: /tmp/src/somefile
    location: USS
    'replace': true
    dest: SOME.PARTITIONED.DATA.SET.DBDLIB
    sys_lib:
      - SOME.DATA.SET.SDFSMAC
      - SYS1.MACLIB
- name: Basic example of IMS DBDGEN module with a single sequential data set.source.
  ims_dbd_gen:
    src: SOME.DATA.SET.DBD
    'replace': true
    dest: SOME.PARTITIONED.DATA.SET.DBDLIB
    dbd_name: exampleDBD
    sys_lib:
      - SOME.DATA.SET.SDFSMAC
      - SYS1.MACLIB
- name: Basic example of IMS DBDGEN module with a single PDS source.
  ims_dbd_gen:
    src: SOME.DATA.SET.DBD.SRC
    'replace': true
    member_list:
      - 'DEDBJN21': 'DBD1'
      - 'DEDBJN21': 'DBD2'
      - 'DEDBJNV1': 'DBD3'
    dest: SOME.PARTITIONED.DATA.SET.DBDLIB
    sys_lib:
      - SOME.DATA.SET.SDFSMAC
      - SYS1.MACLIB
- name: Basic example of IMS DBDGEN module with a batch input uniform source type.
  ims_dbd_gen:
    batch:
      -
        src: /tmp/src/somefile1
        location: USS
        'replace': true
      -
        src: /tmp/src/somefile2
        location: USS
        'replace': true
    dest: SOME.PARTITIONED.DATA.SET.DBDLIB
    sys_lib:
      - SOME.DATA.SET.SDFSMAC
      - SYS1.MACLIB
- name: Basic example of IMS DBDGEN module with a batch input varied source type.
  ims_dbd_gen:
    batch:
      -
        src: /tmp/src/somefile
        location: USS
        'replace': true
      -
        src: SOME.DATA.SET.DBD.SRC
        location: DATA_SET
        member_list: [DSMEMBR1, DSMEMBR2 : target2, DSMEMBR3]
      -
        src: SOME.DATA.SET.DBD.SRC
        member_list: [DSMEMBR4 : target4]
        'replace': true
      -
        src: SOME.DATA.SET.DBD.SEQ
        location: DATA_SET
        dbd_name: SEQ
    dest: SOME.PARTITIONED.DATA.SET.DBDLIB
    sys_lib:
      - SOME.DATA.SET.SDFSMAC
      - SYS1.MACLIB
'''

RETURN = r'''
batch_result:
    description:
        List of output for each DBDGEN run on each element in the list of input source if input is batch.
    type: list
    returned: on batch call
    elements: dict
    contains:
        return_text:
          description:
            Status message.
          returned: always
          type: str
          sample: Invalid input source list being passed without content.
        src:
          description:
            input dbd src name processed.
          returned: always
          type: str
msg:
    description:
        The message of the DBDGEN execution result.
    returned: always
    type: str
    sample: DBDGEN execution was successful.
rc:
    description:
      Module return code (0 for success)
    returned: always
    type: int
    sample: 0
stderr:
    description:
      Module standard error
    returned: failure
    type: str
    sample: Output data set for DDNAME has invalid record format.
stdout:
    description:
      Module standard output
    returned: success
    type: str
    sample: DBDGEN execution was successful
'''

from ansible.module_utils.basic import AnsibleModule
from ansible_collections.ibm.ibm_zos_ims.plugins.module_utils.ims_gen_utils import \
    data_set_exists, data_set_member_exists, file_exists, run_gen_file, run_gen_data_set, execute_gen_command  # pylint: disable=import-error


def run_module():
    # define available arguments/parameters a user can pass to the module
    module_args = dict(

        src=dict(type='str', required=False),
        location=dict(type='str', default='DATA_SET', choices=['DATA_SET', 'USS']),
        replace=dict(type='bool', required=False, default=True),

        # TODO member_list is required if location is 'DATA_SET'
        member_list=dict(type='list', required=False),
        # member_list=dict(type='list', elements='str', required=False),

        dbd_name=dict(type='str', required=False),

        batch=dict(
            type='list',
            required=False,
            elements='dict',
            options=dict(
                src=dict(type='str', required=False),
                location=dict(type='str', default='DATA_SET', choices=['DATA_SET', 'USS']),
                replace=dict(type='bool', required=False, default=True),

                # TODO member_list is required if location is 'DATA_SET'
                member_list=dict(type='list', required=False),
                dbd_name=dict(type='str', required=False))),

        sys_lib=dict(type='list', required=True),
        dest=dict(type='str', required=True))

    # TODO - enforce batch and single source params are mutually exclusive

    result = dict(changed=False)

    module = AnsibleModule(
        argument_spec=module_args,
        supports_check_mode=False)

    # TODO - enforce batch and single source params are mutually exclusive
    # TODO - use BetterArgParser

    run_command = module.run_command

    # TODO move src handling (list vs singulars) logic to utils
    if module.params['batch']:
        dbd_src_list = module.params['batch']
    else:
        dbd_src_list = [{
            'src': module.params['src'],
            'location': module.params['location'],
            'replace': module.params['replace'],
            'member_list': module.params['member_list'],
            'dbd_name': module.params['dbd_name']}]

    dest = module.params['dest']
    sys_lib_list = module.params['sys_lib']

    # validate destination on target
    rc = data_set_exists(dest, run_command)
    if not rc:
        return_text = 'Destination data set does not exist or is not catalogued.'
        result['rc'] = 1
        module.fail_json(msg=return_text, **result)

    # process sources and run gen utility.
    batch_result = []
    for source in dbd_src_list:
        src, return_code, return_text, failed = execute_gen_command(source, dest, sys_lib_list, run_command, module, result)

        # populate batch result list with srcs returned from execute
        if failed:
            result['rc'] = return_code
            batch_result.append({'src': src, 'return_text': return_text})
            if module.params['batch']:
                result['batch_result'] = batch_result
                module.fail_json(msg='FAILURE - DBDGEN execution unsuccessful.', **result)
            else:
                msg = batch_result[0]['return_text']
                module.fail_json(msg=msg, **result)
        else:
            batch_result.append({'src': src, 'return_text': 'success'})

        # Update changed
        result['changed'] = True

    # If size of batch_results is 1, then single src case, else batch case
    if module.params['batch']:
        result['batch_result'] = batch_result
    # Module execution successful.
    result['msg'] = 'DBDGEN execution was successful.'
    result['rc'] = 0
    module.exit_json(**result)  # success


def main():
    run_module()


if __name__ == '__main__':
    main()
