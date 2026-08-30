#!/usr/bin/python
# -*- coding: utf-8 -*-
# Copyright (c) IBM Corporation 2019, 2020
# Apache License, Version 2.0 (see https://opensource.org/licenses/Apache-2.0)
from __future__ import (absolute_import, division, print_function)

__metaclass__ = type


DOCUMENTATION = r'''
---

module: ims_dbrc

short_description: Submit IMS DBRC Commands
version_added: "2.9"

description:
  - Use Database Recovery Control (DBRC) to record and manage information that is
    stored in a set of VSAM data sets that are collectively called the Recovery
    Control (RECON) data set.
  - Based on this information, you can use DBRC to advise IMS about how to proceed
    with certain IMS actions.
author:
  - Omar Elbarmawi (@oelbarmawi)
options:
  command:
    description:
      - This is the well-formatted DBRC command to submit.
    type: list
    required: true
  dbd_lib:
    description:
      - The data set that contains the descriptions for the databases that are under the control of DBRC.
    type: str
    required: false
  dynamic_allocation_dataset:
    description:
      - The dynamic allocation data set that will be used to complete the DBRC execution.
      - Required if `recon1`, `recon2`, and `recon3` are not specified.
    type: str
    required: false
  genjcl_input_dataset:
    description:
      - The PDS, which contains the skeletal JCL members used by the DBRC utility to generate JCL.
      - Equivalent to the JCLPDS control statement.
    type: str
    required: false
  genjcl_output_dataset:
    description:
      - The data set which is to receive the generated JCL. It is required only for the GENJCL commands.
      - Equivalent to the JCLOUT control statement.
    type: str
    required: false
  max_rc:
    description:
      - The maximum acceptable return code allowed for the module to complete succesfully.
    type: int
    required: false
  recon1:
    description:
      - The RECON1 data set that will be used to complete the DBRC execution.
      - Required if `dynamic_allocation_dataset` is not specified.
    type: str
    required: false
  recon2:
    description:
      - The RECON2 data set that will be used to complete the DBRC execution.
      - Required if `dynamic_allocation_dataset` is not specified.
    type: str
    required: false
  recon3:
    description:
      - The RECON3 data set that will be used to complete the DBRC execution.
      - Required if `dynamic_allocation_dataset` is not specified.
    type: str
    required: false
  steplib:
    description:
      - List of STEPLIB datasets that contain the IMS nucleus and the required action modules.
    type: list
    required: true
notes:
  - The I(steplib) parameter can also be specified in the target inventory's environment_vars.
  - The I(steplib) input parameter to the module will take precedence over the value specified in the environment_vars.
'''

EXAMPLES = '''
- name: Sample DBRC Single Command
  ims_dbrc:
    command: LIST.RECON STATUS
    steplib:
      - IMSTESTU.IMS1501.MARKER
      - IMSTESTL.IMS1.EXITLIB
      - IMSTESTG.IMS15R.TSTRES
      - IMSBLD.IMS15R.USERLIB
      - IMSBLD.I15RTSMM.CRESLIB
    dbd_lib: IMSTESTL.IMS1.DBDLIB
    genjcl_input_dataset: IMSTESTL.IMS1.GENJCL
    genjcl_output_dataset: IMSTESTL.IMS1.JCLOUT
    dynamic_allocation_dataset: IMSTESTL.IMS1.DYNALLOC

- name: Sample DBRC Multiple Commands with dynamic_allocation_dataset Specified
  ims_dbrc:
    command:
      - LIST.RECON STATUS
      - LIST.DB ALL
      - LIST.DBDS DBD(CUSTOMER)
    steplib:
      - IMSTESTU.IMS1501.MARKER
      - IMSTESTL.IMS1.EXITLIB
      - IMSTESTG.IMS15R.TSTRES
      - IMSBLD.IMS15R.USERLIB
      - IMSBLD.I15RTSMM.CRESLIB
    dbd_lib: IMSTESTL.IMS1.DBDLIB
    genjcl_input_dataset: IMSTESTL.IMS1.GENJCL
    genjcl_output_dataset: IMSTESTL.IMS1.JCLOUT
    dynamic_allocation_dataset: IMSTESTL.IMS1.SDFSRESL

- name: Sample DBRC Multiple Commands with RECON specified
  ims_dbrc:
    command:
      - LIST.RECON STATUS
      - INIT.DB DBD(TESTDB)
      - DELETE.DB DBD(TESTDB)
    steplib:
      - IMSTESTU.IMS1501.MARKER
      - IMSTESTL.IMS1.EXITLIB
      - IMSTESTG.IMS15R.TSTRES
      - IMSBLD.IMS15R.USERLIB
      - IMSBLD.I15RTSMM.CRESLIB
    dbd_lib: IMSTESTL.IMS1.DBDLIB
    genjcl_input_dataset: IMSTESTL.IMS1.GENJCL
    genjcl_output_dataset: IMSTESTL.IMS1.JCLOUT
    recon1: IMSTESTL.IMS1.RECON1
    recon2: IMSTESTL.IMS1.RECON2
    recon3: IMSTESTL.IMS1.RECON3
'''

RETURN = '''
dbrc_output:
  description:
    The output provided by the specified DBRC Command(s).
  type: list
  returned: sometimes
  contains:
    command:
      description:
        The original submitted command that corresponds to the output.
      returned: always
      type: str
    messages:
      description:
        Compiled list of messages returned from the DBRC output.
      returned: always
      type: list
    output:
      description:
        Parsed DBRC output that maps each field to its corresponding value.
      returned: always
      type: dict
msg:
  description:
    The output message that the `ims_dbrc` module generates.
  type: str
  returned: always
rc:
  description:
    The return code returned by the DBRC module.
  type: int
  returned: always
unformatted_output:
  description:
    Unformatted output response from the all of the submitted DBRC commands.
  type: list
  returned: always
'''

from ansible.module_utils.basic import AnsibleModule
from ansible_collections.ibm.ibm_zos_ims.plugins.module_utils.dbrc import dbrc  # pylint: disable=import-error
from ansible_collections.ibm.ibm_zos_ims.plugins.module_utils.ims_module_error_messages import DBRCErrorMessages as em  # pylint: disable=import-error
from ansible.module_utils.basic import AnsibleModule, env_fallback, AnsibleFallbackNotFound
import re


def get_max_rc(raw_max_rc):
    if raw_max_rc:
        try:
            return int(raw_max_rc)
        except Exception:
            result['msg'] = em.INVALID_MAX_RC
            module.fail_json(**result)
    else:
        return None


def get_step_lib_from_environment_var():
    try:
        step_lib_list = list(filter(None, re.split("[\\s,]+", env_fallback('STEPLIB'))))
        result['steplibber'] = step_lib_list
        return step_lib_list
    except AnsibleFallbackNotFound:
        return None


def run_module():
    global module, result
    module_args = dict(
        command=dict(type='list', required=True),
        dbd_lib=dict(type='str', required=False),
        dynamic_allocation_dataset=dict(type='str', required=False),
        genjcl_input_dataset=dict(type='str', required=False),
        genjcl_output_dataset=dict(type='str', required=False),
        max_rc=dict(type='int', required=False),
        recon1=dict(type='str', required=False),
        recon2=dict(type='str', required=False),
        recon3=dict(type='str', required=False),
        steplib=dict(type='list', required=True)
    )

    result = dict(
        changed=False,
        msg='',
        rc='',
        failed=True,
        dbrc_output={},
        unformatted_output=[]
    )

    module = AnsibleModule(
        argument_spec=module_args,
        supports_check_mode=True
    )

    max_rc = get_max_rc(module.params['max_rc'])
    combined_step_lib = module.params['steplib']
    environ_step_lib = get_step_lib_from_environment_var()
    if environ_step_lib:
        combined_step_lib = module.params['steplib'] + environ_step_lib
    try:
        response = dbrc.dbrc(
            commands=module.params['command'],
            steplib=combined_step_lib,
            dbd_lib=module.params['dbd_lib'],
            dynamic_allocation_dataset=module.params['dynamic_allocation_dataset'],
            genjcl_input_dataset=module.params['genjcl_input_dataset'],
            genjcl_output_dataset=module.params['genjcl_output_dataset'],
            recon1=module.params['recon1'],
            recon2=module.params['recon2'],
            recon3=module.params['recon3']).execute()

        result['dbrc_output'] = response['dbrc_fields']
        result['unformatted_output'] = response['original_output']
        result['failed'] = response['failure_detected']
        result['rc'] = response['rc']
        result['changed'] = response['changed']

        if max_rc and response['rc']:
            if int(response['rc']) <= max_rc:
                result['msg'] = response['error'] if response['error'] else em.SUCCESS_MSG
                module.exit_json(**result)

        if not result['dbrc_output']:
            if response['rc'] and int(response['rc']) > 4:
                result['msg'] = response['error'] if response['error'] else em.FAILURE_MSG
            else:
                result['msg'] = em.EMPTY_OUTPUT_MSG

            if response['error']:
                print("An error occurred:", response['error'])

            result['changed'] = False
            module.fail_json(**result)

        elif result['failed']:
            result['msg'] = em.FAILURE_MSG
            module.fail_json(**result)

        else:
            result['msg'] = em.SUCCESS_MSG

    except Exception as e:
        result['msg'] = repr(e)
        module.fail_json(**result)

    finally:
        # _delete_data_set(sysin_data_set)
        pass

    module.exit_json(**result)


def main():
    run_module()


if __name__ == '__main__':
    main()
