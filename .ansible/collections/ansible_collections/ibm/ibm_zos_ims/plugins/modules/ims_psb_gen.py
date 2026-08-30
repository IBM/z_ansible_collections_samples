#!/usr/bin/python
# -*- coding: utf-8 -*-
# Copyright (c) IBM Corporation 2019, 2020
# Apache License, Version 2.0 (see https://opensource.org/licenses/Apache-2.0)

from __future__ import (absolute_import, division, print_function)

__metaclass__ = type


DOCUMENTATION = r'''
---
module: ims_psb_gen
short_description: Generate IMS PSB
version_added: "2.9"
description:
    - The Program Specification Block (PSB) Generation utility places the created PSB in the PSB library so that it can be used by IMS application programs.
author:
    Ketan Kelkar (@ketan-kelkar)
    Omar Elbarmawi (@oelbarmawi)
options:
    src:
        description:
            - The src field can reference a PDS, PDSE member, sequential data set, or UNIX System Services file path.
            - If a PDS is specified, all members within the PDS will be treated as individual PSB source members to be processed.
        type: str
        required: false
    location:
        description:
            - The PSB source location, Supported options are DATA_SET or USS. The default is DATA_SET.
            - The DATA_SET option can be used for a PDS, PDSE, or sequential data set.
        type: str
        required: false
        default: DATA_SET
        choices:
            - DATA_SET
            - USS
    replace:
        description:
            - When 'replace' is 'true', an existing PSB member matching the name in the
                input PSB source will be overwitten.
        type: bool
        default: true
        required: false
    member_list:
        description:
            - A list of member names if the source specified is a data set.
            - Optionally, proceeding the source_member, a colon with a target name for
                the generated PSB member can be specified. If no target name is
                specified, source_name will be used as the target name.
            - If 'member_list' is empty and location is set to 'DATA_SET' or
                not specified, then src is expected to be a sequential data set.
            - Elements are of the list are str or dict with single key-value
                pair
        type: list
        required: false
    psb_name:
        description:
            - Target name of the generated PSB member.
            - This parameter is only required and applies if src is a sequential data set.
        type: str
        required: false
    batch:
        description:
            - Batch can be used to perform multiple operations in a single module call.
            - Expects a list of the location(s) of the IMS Program Specification Block (PSB) source to be compiled.
            - The source can reference a PDS or PDSE member, sequential data set or UNIX System Services file path.
        type: list
        elements: dict
        required: false
        suboptions:
            src:
                description:
                    - The src field can reference a PDS, PDSE member, sequential data set, or UNIX System Services file path.
                    - If a PDS is specified, all members within the PDS will be treated as individual PSB source members to be processed.
                type: str
                required: true
            location:
                description:
                    - The PSB source location, Supported options are DATA_SET or USS. The default is DATA_SET.
                    - The DATA_SET option can be used for a PDS, PDSE, or sequential data set.
                type: str
                default: DATA_SET
                choices:
                    - DATA_SET
                    - USS
            replace:
                description:
                    - When 'replace' is 'true', an existing PSB member matching the name in the input PSB source will be overwitten.
                type: bool
                default: true
                required: false
            member_list:
                description:
                    - A list of member names if the source specified is a data set.
                    - Optionally, proceeding the source_member, a colon with a target name for
                        the generated PSB member can be specified. If no target name is
                        specified, source_name will be used as the target name.
                    - If 'member_list' is empty and location is set to 'DATA_SET' or
                        not specified, then src is expected to be a sequential data set.
                    - Elements are of the list are str or dict with single
                        key-value
                type: list
                required: false
            psb_name:
                description:
                    - Target name of the generated PSB member.
                    - This parameter is only required and applies if src is a sequential data set.
                type: str
                required: false
    sys_lib:
        description:
            - A list of required macro libraries that are needed to compile the PSB source. These libraries will
                be used as the sys_lib at compile time.
        type: list
        required: true
    dest:
        description:
            - The target output PSBLIB partitioned data set in which the PSB members will be generated.
        type: str
        required: true
notes:
    - Currently ims_psb_gen does not support copying symbolic links from both local to
        remote and remote to remote.

'''


EXAMPLES = r"""
---
- name: Basic example of IMS PSBGEN module with single data set
  ims_psb_gen:
    src: /tmp/src/somefile
    location: USS
    replace: true
    dest: SOME.DATA.SET.PSBLIB
    sys_lib:
    - SOME.DATA.SET.SDFSMAC
    - SYS1.MACLIB

- name: Basic example of IMS PSBGEN module
  ims_psb_gen:
    batch:
    -
      src: /tmp/psbgen02
      location: USS
      replace: true
    -
      src: OMVSADM.IMSTESTU.ANSIBLE.PSB.SRC
      location: DATA_SET
      member_list: [PSBGENL : TARGET1, PSBGENL : TARGET2]
    -
      src: OMVSADM.IMSTESTU.ANSIBLE.PSB.SRC
      member_list: [PSBGENL, PSBGENL]
      replace: true
    -
      src: OMVSADM.IMSTESTU.ANSIBLE.PSB.SRC
      member_list:
      - 'COGPSBL': 'TARGET3'
      - 'COGPSBL2': 'TARGET4'
      replace: true
    -
      src: OMVSADM.IMSTESTU.ANSIBLE.PSB.SQ
      location: DATA_SET
      psb_name: SEQ
    dest: IMSBANK.IMS1.PSBLIB
    sys_lib:
    - IMSBLD.I15RTSMM.SDFSMAC
    - SYS1.MACLIB
"""

RETURN = r"""
---
batch_result:
        description:
                List of output for each PSBGEN run on each element in the list of input source if input is batch.
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
                        input psb src name processed.
                    returned: always
                    type: str
msg:
        description:
            The message of the PSBGEN execution result.
        returned: always
        type: str
        sample: PSBGEN execution was successful.
rc:
        description:
            Module return code (0 for success)
        returned: always
        type: int
        sample: 0
stderr:
        description:
            Module standard error.
        returned: failure
        type: str
        sample: Output data set for DDNAME has invalid record format.
stdout:
        description:
            Module standard output.
        returned: success
        type: str
        sample: PSBGEN execution was successful.
"""

from ansible.module_utils.basic import AnsibleModule
from ansible_collections.ibm.ibm_zos_ims.plugins.module_utils.ims_gen_utils \
    import data_set_exists, data_set_member_exists, file_exists, run_gen_file, \
    run_gen_data_set, execute_gen_command  # pylint: disable=import-error


def run_module():
    module_args = dict(
        src=dict(type='str', required=False),
        location=dict(type='str', default='DATA_SET', choices=['DATA_SET', 'USS']),
        replace=dict(type='bool', required=False, default=True),

        # TODO member_list is required if location is 'DATA_SET'
        member_list=dict(type='list', required=False),

        psb_name=dict(type='str', required=False),

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

                psb_name=dict(type='str', required=False),

            )
        ),

        sys_lib=dict(type='list', required=True),
        dest=dict(type='str', required=True)
    )

    # TODO - enforce batch and single source params are mutually exclusive
    # TODO - use BetterArgParser

    result = dict(
        changed=False,
        # original_message='',
        # message=''
    )

    module = AnsibleModule(
        argument_spec=module_args,
        supports_check_mode=False
    )

    run_command = module.run_command

    # # we are not supporting check mode.
    # if module.check_mode:
    #   module.exit_json(**result)

    # TODO move src handling (list vs singulars) logic to utils
    if module.params['batch']:
        psb_src_list = module.params['batch']
    else:
        psb_src_list = [{
            'src': module.params['src'],
            'location': module.params['location'],
            'replace': module.params['replace'],
            'member_list': module.params['member_list'],
            'psb_name': module.params['psb_name']
        }]

    dest = module.params['dest']
    sys_lib_list = module.params['sys_lib']

    # validate destination on target
    rc = data_set_exists(dest, run_command)
    if not rc:
        return_text = 'Destination data set does not exist or is not catalogued'
        # result['ims_output'].append({
        #     'return_code': 1,
        #     'std_error': 'error validating destination: '+dest,
        #     'return_text': return_text
        # })
        # module.fail_json(
        #     msg='Failed to validate destination dataset', **result)
        result['rc'] = 1
        module.fail_json(msg=return_text, **result)

    # process sources and run gen utility.
    batch_result = []
    for source in psb_src_list:
        src, return_code, return_text, failed = execute_gen_command(source, dest, sys_lib_list, run_command, module, result)

        # result['ims_output'].append({
        #     'return_code': 0,
        #     'src': src,
        #     'return_text': return_text
        # })

        # populate batch result list with srcs returned from execute
        if failed:
            result['rc'] = return_code
            batch_result.append({'src': src, 'return_text': return_text})
            if module.params['batch']:
                result['batch_result'] = batch_result
                module.fail_json(msg='FAILURE - PSBGEN execution unsuccessful', **result)
            else:
                msg = batch_result[0]['return_text']
                module.fail_json(msg=msg, **result)
        else:
            # batch_result.append({'src':src})
            batch_result.append({'src': src, 'return_text': 'success'})  # TODO

        # update changed
        result['changed'] = True

    if module.params['batch']:
        result['batch_result'] = batch_result

    # module execution successful.
    result['msg'] = 'PSBGEN execution was successful.'
    result['rc'] = 0
    module.exit_json(**result)


def main():
    run_module()


if __name__ == '__main__':
    main()
