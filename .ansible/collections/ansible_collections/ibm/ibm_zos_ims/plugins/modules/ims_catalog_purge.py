#!/usr/bin/python
# -*- coding: utf-8 -*-
# Copyright (c) IBM Corporation 2020
# LICENSE: [GNU General Public License version 3](https://opensource.org/licenses/GPL-3.0)

from __future__ import (absolute_import, division, print_function)

__metaclass__ = type


DOCUMENTATION = r'''
---

module: ims_catalog_purge
short_description: Purge records from the IMS Catalog
version_added: "2.9"
description:
  - The IMS Catalog Purge  utility DFS3PU10 removes the segments that represent a DBD or PSB instance,
    all instances of a DBD version, or an entire DBD or PSB record from the IMS catalog. You can also analyze
    the catalog and generate delete statements for ACBs eligible for deletion, as well as update the ACB retention
    criteria.

author:
  - Jerry Li (@th365thli)
options:
  mode:
    description:
      - Determines which mode the utility runs in. ANALYSIS mode generates delete statements based on the
        retention criteria and places them in the SYSUT1 data set. PURGE mode executes delete statements in the
        SYSUT1 data set. BOTH mode performs ANALYSIS and PURGE mode consecutively.
    type: str
    required: true
    choices:
      - PURGE
      - BOTH
      - ANALYSIS
  online_batch:
    description:
      - Indicates if this utility is to be run in a BMP region.
    type: bool
    required: false
    default: false
  ims_id:
    description:
      - The identifier of the IMS system on which the job is to be run.
      - Required if online_batch is true.
    type: str
    required: false
  dbrc:
    description:
      - Indicates if the IMS Database Recovery Control facility is enabled.
    type: bool
    required: false
  irlm_id:
    description:
      - The IRLM ID if IRLM is enabled. Cannot be specified when online_batch is true.
    type: str
    required: false
  reslib:
    description:
      - Points to an authorized library that contains the IMS SVC modules.
    type: list
    required: false
  buffer_pool_param_dataset:
    description:
      - Defines the buffer pool parameters data set. This option is required if you are running the utility as a DLI.
    type: str
    required: false
  primary_log_dataset:
    description:
      - Defines the primary IMS log data set. This option is required if you are running the utility as a DLI.
    type: dict
    required: true
    suboptions:
      dataset_name:
        description:
          - Describes the name of the data set.
        type: str
        required: true
      disposition:
        description:
          - The status of the data set.
        type: str
        required: false
        choices:
          - NEW
          - OLD
          - SHR
          - EXCL
      record_format:
        description:
          - The record format.
        type: str
        required: false
        choices:
          - FB
          - VB
          - FBA
          - VBA
          - U
      record_length:
        description:
          - The logical record length in bytes.
        type: int
        required: false
      block_size:
        description:
          - The block size.
        type: int
        required: false
      primary:
        description:
          - The amount of primary space to allocate for the data set.
        type: int
        required: false
      primary_unit:
        description:
          - The unit of size to use when specifying primary space.
        type: str
        required: false
      secondary:
        description:
          - The amount of secondary space to allocate for the data set.
        type: int
        required: false
      secondary_unit:
        description:
          - The unit of size to use when specifying secondary space.
        type: str
        required: false
      normal_disposition:
        description:
          - Data set action after normal termination.
        type: str
        required: false
        choices:
          - DELETE
          - KEEP
          - CATLG
          - UNCATLG
      abnormal_disposition:
        description:
          - Data set action after abnormal termination.
        type: str
        required: false
        choices:
          - DELETE
          - KEEP
          - CATLG
          - UNCATLG
      type:
        description:
          - The type of data set.
        type: str
        required: false
        choices:
          - SEQ
          - BASIC
          - LARGE
          - PDS
          - PDSE
          - LIBRARY
          - LDS
          - RRDS
          - ESDS
          - KSDS
      volumes:
        description:
          - A list of volume serials. When providing multiple volumes, processing will begin with
            the first volume in the provided list. Offline volumes are not considered.
        type: list
        required: false
        elements: str
      storage_class:
        description:
          - The storage class for an SMS-managed data set. Not valid for data sets that are not
            SMS-managed.
        type: str
        required: false
      management_class:
        description:
          - The management class for an SMS-managed data set. Not valid for data sets that are not
            SMS-managed.
        type: str
        required: false
      data_class:
        description:
          - The data class for an SMS-managed data set. Not valid for data sets that are not
            SMS-managed.
        type: str
        required: false
  psb_lib:
    description:
      - Defines the IMS.PSBLIB data set.
    type: list
    elements: str
    required: true
  dbd_lib:
    description:
      - Defines the IMS.DBDLIB data sets.
    type: list
    elements: str
    required: true
  proclib:
    description:
      - Defines the IMS.PROCLIB data set that contains the DFSDFxxx member. The DFSDFxxx member
        defines various attributes of the IMS catalog that are required by the utility.
    type: list
    required: true
  steplib:
    description:
      - Points to IMS.SDFSRESL, which contains the IMS nucleus and required IMS modules.
      - The steplib parameter can also be specified in the target inventory's environment_vars.
      - The steplib input parameter to the module will take precedence over the value specified in the environment_vars.
    type: list
    required: False
  delete_dbd_by_version:
    description:
      - Delete DBD instances based on the specified name and version. If ANALYSIS mode is specified, it will
        generate DELETE DBD statements in the SYSUT1 data set along with any other delete statements based off the
        retention criteria. If PURGE or BOTH mode is specified, it will write the delete statements to the SYSUT1 data set
        and then execute them.
    type: list
    required: false
    elements: dict
    suboptions:
      member_name:
        description:
          - The 8 character name of the DBD that you are deleting a version from.
        type: str
        required: true
      version_number:
        description:
          - The version number of the DBD that you are deleting. The value must match the version number
            that is specified on the DBVER keyword in the DBD generation statement of the version that
            you are deleting.
        type: int
        required: true
  update_retention_criteria:
    description:
      - Use this statement to set the retention criteria for DBD or PSB records in the catalog database.
        You can submit any number of update statements. You cannot specify this option if PURGE mode is
        selected. If used with any other mode options, it will update the retention criteria first.
    type: list
    required: false
    elements: dict
    suboptions:
      resource:
        description:
          - Specifies whether a DBD or PSB should be updated.
        choices:
          - DBD
          - PSB
        type: str
        required: true
      member_name:
        description:
          - The 8 character IMS name of the DBD or PSB resource. Wildcards are supported.
        type: str
        required: true
      instances:
        description:
          - The number of instances of a DBD or PSB that must be retained in the DBD or PSB record.
        type: int
        required: true
      days:
        description:
          - The number of days that an instance of a DBD or PSB must be retained before it can be purged.
        type: int
        required: false
  delete:
    description:
      - Specifies a DBD or PSB instance or an entire DBD or PSB record to delete from the IMS catalog database.
      - This option must be used with PURGE mode and overrides any retention criteria, hence you can remove any
        DBD or PSB that would not otherwise be eligible for deletion.
    type: list
    required: false
    elements: dict
    suboptions:
      resource:
        description:
          - Specify whether you want to delete a DBD or PSB.
        type: str
        required: true
        choices:
          - DBD
          - PSB
      member_name:
        description:
          - The 8 character IMS name of the DBD or PSB resource. Wildcards are supported.
        type: str
        required: true
      time_stamp:
        description:
          - The ACB timestamp that identifies the specific DBD or PSB instance to purge.
        type: int
        required: true
  managed_acbs:
    description:
      - Specifies whether deleting DBD and PSB instances from the IMS catalog causes the corresponding DBD and PSB
        instances in the IMS directory to be deleted. If 'analysis_mode' is true, the DBD and PSB instances
        will not be deleted from the IMS directory.
    type: bool
    required: false
  resource_chkp_freq:
    description:
      - Specifies the number of resource instances to be deleted or updated between checkpoints. Can be a 1 to 8 digit
        numeric value between 1 to 99999999. The default value is 200.
    type: int
    required: false
  sysut1:
    description:
      - The data set where delete statements are written to. Written either by the purge utility when specifying ANALYSIS or BOTH mode,
        or by the user when specifying PURGE mode.
    type: dict
    required: true
    suboptions:
      dataset_name:
        description:
          - Describes the name of the data set.
        type: str
        required: true
      disposition:
        description:
          - The status of the data set.
        type: str
        required: false
        choices:
          - NEW
          - OLD
          - SHR
          - EXCL
      block_size:
        description:
          - The block size.
        type: int
        required: false
      primary:
        description:
          - The amount of primary space to allocate for the data set.
        type: int
        required: false
      primary_unit:
        description:
          - The unit of size to use when specifying primary space.
        type: str
        required: false
      secondary:
        description:
          - The amount of secondary space to allocate for the data set.
        type: int
        required: false
      secondary_unit:
        description:
          - The unit of size to use when specifying secondary space.
        type: str
        required: false
      normal_disposition:
        description:
          - Data set action after normal termination.
        type: str
        required: false
        choices:
          - DELETE
          - KEEP
          - CATLG
          - UNCATLG
      abnormal_disposition:
        description:
          - Data set action after abnormal termination.
        type: str
        required: false
        choices:
          - DELETE
          - KEEP
          - CATLG
          - UNCATLG
      type:
        description:
          - The type of the data set.
        type: str
        required: false
        choices:
          - SEQ
          - BASIC
          - LARGE
          - PDS
          - PDSE
          - LIBRARY
          - LDS
          - RRDS
          - ESDS
          - KSDS
      volumes:
        description:
          - A list of volume serials. When providing multiple volumes, processing will begin with
            the first volume in the provided list. Offline volumes are not considered.
        type: list
        required: false
        elements: str
      storage_class:
        description:
          - The storage class for an SMS-managed data set. Not valid for data sets that are not
            SMS-managed.
        type: str
        required: false
      management_class:
        description:
          - The management class for an SMS-managed data set. Not valid for data sets that are not
            SMS-managed.
        type: str
        required: false
      data_class:
        description:
          - The data class for an SMS-managed data set. Not valid for data sets that are not
            SMS-managed.
        type: str
        required: false
notes:
  - The I(steplib) parameter can also be specified in the target inventory's environment_vars.
  - The I(steplib) input parameter to the module will take precedence over the value specified in the environment_vars.
  - If only the I(steplib) parameter is specified, then only the I(steplib) concatenation will be used to resolve the IMS RESLIB data set.
  - Specifying only I(reslib) without I(steplib) is not supported.
'''

EXAMPLES = r'''
- name: Purge the IMS Catalog of DBDs beginning with 'DB'
  ims_catalog_purge:
    reslib:
      - SOME.IMS.SDFSRESL
    steplib:
      - SOME.IMS.SDFSRESL
    proclib:
      - SOME.IMS.PROCLIB
    dbd_lib:
      - SOME.IMS.DBDLIB
    psb_lib:
      - SOME.IMS.PSBLIB
    buffer_pool_param_dataset: "SOME.IMS1.PROCLIB(DFSVSMHP)"
    primary_log_dataset:
      dataset_name: SOME.IMS.LOG1
    mode: PURGE
    delete:
      - resource: DBD
        member_name: 'AUTODB'
        time_stamp: 500

- name: Purge the IMS Catalog and the IMS Directory of DBDs beginning with 'DB'
  ims_catalog_purge:
    reslib:
      - SOME.IMS.SDFSRESL
    steplib:
      - SOME.IMS.SDFSRESL
    proclib:
      - SOME.IMS.PROCLIB
    dbd_lib:
      - SOME.IMS.DBDLIB
    psb_lib:
      - SOME.IMS.PSBLIB
    buffer_pool_param_dataset: "SOME.IMS1.PROCLIB(DFSVSMHP)"
    primary_log_dataset:
      dataset_name: SOME.IMS.LOG1
    mode: PURGE
    delete:
      - resource: DBD
        member_name: AUTODB
        time_stamp: 500
    managed_acbs: true

- name: Analyze the IMS Catalog and generate delete statements for resources eligible for deletion
  ims_catalog_purge:
    reslib:
      - SOME.IMS.SDFSRESL
    steplib:
      - SOME.IMS.SDFSRESL
    proclib:
      - SOME.IMS.PROCLIB
    dbd_lib:
      - SOME.IMS.DBDLIB
    psb_lib:
      - SOME.IMS.PSBLIB
    buffer_pool_param_dataset: "SOME.IMS1.PROCLIB(DFSVSMHP)"
    primary_log_dataset:
      dataset_name: SOME.IMS.LOG1
    mode: ANALYSIS

- name: Update resource retention criteria for resources in the IMS Catalog while running as BMP
  ims_catalog_purge:
    online_batch: True
    ims_id: IMS1
    reslib:
      - SOME.IMS.SDFSRESL
    steplib:
      - SOME.IMS.SDFSRESL
    proclib:
      - SOME.IMS.PROCLIB
    dbd_lib:
      - SOME.IMS.DBDLIB
    psb_lib:
      - SOME.IMS.PSBLIB
    buffer_pool_param_dataset: "SOME.IMS1.PROCLIB(DFSVSMHP)"
    primary_log_dataset:
      dataset_name: SOME.IMS.LOG1
    mode: ANALYSIS
    update_retention_criteria:
      - resource: DBD
        member_name: AUTODB
        instances: 0
        days: 0
      - resource: PSB
        member_name: DBF000
        instances: 0
        days: 0
'''

RETURN = r'''
content:
  description: The standard output returned running the IMS Catalog Purge utility.
  type: str
  returned: always
  sample: DFS4810I ALL OF THE MEMBER INSTANCES THAT ARE REFERENCED IN THE SYSUT1 DATA SET WERE DELETED FROM THE IMS CATALOG.
rc:
  description: The return code from the IMS Catalog Purge utility.
  type: str
  returned: sometimes
  sample: '0'
stderr:
  description: The standard error output returned from running the IMS Catalog Purge utility.
  type: str
  returned: sometimes
  sample:  12.27.08 STC00143  +DFS671I OMVSADM8.STEP1. - FOR THIS EXECUTION, DBRC IS SET TO NO     IMS1
msg:
  description: Messages returned from the IMS Catalog Purge module.
  type: str
  returned: sometimes
  sample: You must specify a buffer pool parameter data set when running as DLI.
'''

from ansible.module_utils.basic import AnsibleModule
from ansible_collections.ibm.ibm_zos_ims.plugins.module_utils.catalog import catalog  # pylint: disable=import-error
from ansible_collections.ibm.ibm_zos_ims.plugins.module_utils.catalog_parser import catalog_parser  # pylint: disable=import-error

module = None


def run_module():
    module_args = dict(
        online_batch=dict(type="bool", required=False),
        ims_id=dict(type="str", required=False),
        dbrc=dict(type="bool", required=False),
        irlm_id=dict(type="str", required=False),
        reslib=dict(type="list", required=False),
        buffer_pool_param_dataset=dict(type="str", required=False),
        primary_log_dataset=dict(type="dict", required=False),
        psb_lib=dict(type="list", required=False),
        dbd_lib=dict(type="list", required=False),
        proclib=dict(type="list", required=False),
        steplib=dict(type="list", required=False),
        mode=dict(type="str", required=True, choices=['PURGE', 'BOTH', 'ANALYSIS']),
        delete_dbd_by_version=dict(type="list", required=False),
        sysut1=dict(type="dict", required=False),
        update_retention_criteria=dict(type="list", required=False),
        delete=dict(type="list", required=False),
        managed_acbs=dict(type="bool", required=False),
        resource_chkp_freq=dict(type="int", required=False)
    )

    global module
    module = AnsibleModule(
        argument_spec=module_args,
        supports_check_mode=True
    )

    result = {}
    result["changed"] = False

    parsed_args = catalog_parser.catalog_parser(module, module.params, result).validate_purge_input()
    response = catalog.catalog(module, result, parsed_args).execute_catalog_purge()

    if "DFS4518I" in result['content']:
        result["changed"] = True

    module.exit_json(**response)


def main():
    run_module()


if __name__ == '__main__':
    main()
