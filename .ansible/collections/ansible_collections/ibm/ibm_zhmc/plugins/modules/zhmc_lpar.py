#!/usr/bin/python
# Copyright 2022 IBM Corp. All Rights Reserved.
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#    http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.

from __future__ import (absolute_import, division, print_function)
__metaclass__ = type

# For information on the format of the ANSIBLE_METADATA, DOCUMENTATION,
# EXAMPLES, and RETURN strings, see
# http://docs.ansible.com/ansible/dev_guide/developing_modules_documenting.html

ANSIBLE_METADATA = {
    'metadata_version': '1.1',
    'status': ['stableinterface'],
    'supported_by': 'community',
    'shipped_by': 'other',
    'other_repo_url': 'https://github.com/zhmcclient/zhmc-ansible-modules'
}

DOCUMENTATION = """
---
module: zhmc_lpar
version_added: "2.9.0"
short_description: Manage LPARs
description:
  - Gather facts about an LPAR of a CPC (Z system) in classic mode.
  - Update modifiable properties of an active LPAR.
  - Activate an LPAR and update its properties.
  - Load an LPAR and update its properties.
  - Deactivate an LPAR using the 'Deactivate Logical Partition' operation.
  - Initialize for load using the 'Reset Clear' or 'Reset Normal' operations.
author:
  - Andreas Maier (@andy-maier)
requirements:
  - Access to the WS API of the HMC of the targeted CPC (see :term:`HMC API`).
  - The targeted CPC must be in the classic operational mode.
  - "The HMC userid must have the following HMC permissions:"
  - Object-access permission to the target LPAR and its parent CPC.
  - If the 'next-activation-profile-name' property is to be updated, task
    permission for the 'Change Object Options' task or the 'Customize/Delete
    Activation Profiles' task.
  - If any of the 'zaware-...' properties is to be updated, task permission for
    the 'Firmware Details' task.
  - If any of the numbers of allocated or reserved cores is is to be updated,
    task permission for the 'Logical Processor Add' task.
  - If the LPAR needs to be activated, task permission for the 'Activate' task.
  - If the LPAR needs to be deactivated, task permission for the 'Deactivate'
    task.
options:
  hmc_host:
    description:
      - The hostname or IP address of the HMC.
    type: str
    required: true
  hmc_auth:
    description:
      - The authentication credentials for the HMC.
    type: dict
    required: true
    suboptions:
      userid:
        description:
          - The userid (username) for authenticating with the HMC.
        type: str
        required: true
      password:
        description:
          - The password for authenticating with the HMC.
        type: str
        required: true
      ca_certs:
        description:
          - Path name of certificate file or certificate directory to be used
            for verifying the HMC certificate. If null (default), the path name
            in the 'REQUESTS_CA_BUNDLE' environment variable or the path name
            in the 'CURL_CA_BUNDLE' environment variable is used, or if neither
            of these variables is set, the certificates in the Mozilla CA
            Certificate List provided by the 'certifi' Python package are used
            for verifying the HMC certificate.
        type: str
        required: false
        default: null
      verify:
        description:
          - If True (default), verify the HMC certificate as specified in the
            C(ca_certs) parameter. If False, ignore what is specified in the
            C(ca_certs) parameter and do not verify the HMC certificate.
        type: bool
        required: false
        default: true
  cpc_name:
    description:
      - The name of the CPC with the target LPAR.
    type: str
    required: true
  name:
    description:
      - The name of the target LPAR.
    type: str
    required: true
  state:
    description:
      - "The desired state for the LPAR:"
      - "* C(inactive): Ensures that the LPAR is inactive (i.e. status
         'not-activated'), unless the LPAR is currently operating and the
         C(force) parameter was not set to True. Properties cannot be updated.
         The LPAR is deactivated if needed."
      - "* C(reset_clear): Initialize the LPAR for loading by performing a
         'Reset Clear' operation (clearing its pending interruptions,
         resetting its channel subsystem, resetting its processors, clearing
         its memory), unless the LPAR is currently loaded (i.e. status is
         'operating' or 'acceptable') and the C(force) parameter was not set to
         True. Properties cannot be updated. After successful execution of the
         'Reset Normal' operation, the LPAR will be inactive (i.e. status
         'not-activated')."
      - "* C(reset_normal): Initialize the LPAR for loading by performing a
         'Reset Normal' operation (clearing its pending interruptions,
         resetting its channel subsystem, resetting its processors), unless the
         LPAR is currently loaded (i.e. status is 'operating' or 'acceptable')
         and the C(force) parameter was not set to True. Properties cannot be
         updated. After successful execution of the 'Reset Normal' operation,
         the LPAR  will be inactive (i.e. status 'not-activated')."
      - "* C(active): Ensures that the LPAR is at least active (i.e. status
         is 'not-operating', 'operating' or 'acceptable'), and then ensures
         that the LPAR properties have the specified values. The LPAR is
         activated if needed. If auto-load is set in the activation profile,
         the LPAR will also be loaded."
      - "* C(loaded): Ensures that the LPAR is loaded (i.e. status is
         'operating' or 'acceptable'), and then ensures that the LPAR properties
         have the specified values. The LPAR is first activated if needed, and
         then loaded if needed."
      - "* C(set): Ensures that the LPAR properties have the specified
         values. Requires that the LPAR is at least active (i.e. status is
         'not-operating', 'operating' or 'acceptable') but does not activate
         the LPAR if that is not the case."
      - "* C(facts): Returns the current LPAR properties."
      - "In all cases, the LPAR must exist."
    type: str
    required: true
    choices: ['inactive', 'reset_clear', 'reset_normal', 'active', 'loaded',
              'set', 'facts']
  activation_profile_name:
    description:
      - "The name of the image or load activation profile to be used when the
         LPAR needs to be activated, for C(state=active) and C(state=loaded)."
      - "Default: The image or load activation profile specified in the
         'next-activation-profile-name' property of the LPAR is used when the
         LPAR needs to be activated."
      - "If the LPAR was already active, the C(force) parameter determines what
         happens."
      - "This parameter is not allowed for the other C(state) values."
    type: str
    required: false
    default: null
  force:
    description:
      - "Controls whether operations that change the LPAR status are performed
        when the LPAR is currently loaded (i.e. status 'operating' or
        'acceptable'):"
      - "If True, such operations are performed regardless of the current LPAR
        status."
      - "If False, such operations are performed only if the LPAR is not
        currently loaded, and are rejected otherwise."
    type: bool
    required: false
    default: false
  os_ipl_token:
    description:
      - "Setting this parameter for C(state=reset_clear) or
        C(state=reset_normal) requests that the corresponding HMC operations
        only be performed if the provided value matches the current value of
        the 'os-ipl-token' property of the LPAR, and be rejected otherwise.
        Note that the 'os-ipl-token' property of the LPAR is set by the
        operating system and is set only by some operating systems, such as
        z/OS. This parameter is ignored for other C(state) values."
    type: str
    required: false
    default: null
  properties:
    description:
      - "Dictionary with new values for the LPAR properties, for
         C(state=active), C(state=loaded) and C(state=set). Key is the property
         name with underscores instead of hyphens, and value is the property
         value in YAML syntax. Integer properties may also be provided as
         decimal strings."
      - "The possible input properties in this dictionary are the properties
         defined as writeable in the data model for LPAR resources
         (where the property names contain underscores instead of hyphens)."
      - "Properties omitted in this dictionary will not be updated."
      - "This parameter is not allowed for the other C(state) values."
    type: dict
    required: false
    default: null
  log_file:
    description:
      - "File path of a log file to which the logic flow of this module as well
         as interactions with the HMC are logged. If null, logging will be
         propagated to the Python root logger."
    type: str
    required: false
    default: null
  _faked_session:
    description:
      - "An internal parameter used for testing the module."
    required: false
    type: raw
    default: null
"""

EXAMPLES = """
---
# Note: The following examples assume that some variables named 'my_*' are set.

- name: Ensure the LPAR is inactive
  zhmc_lpar:
    hmc_host: "{{ my_hmc_host }}"
    hmc_auth: "{{ my_hmc_auth }}"
    cpc_name: "{{ my_cpc_name }}"
    name: "{{ my_lpar_name }}"
    state: inactive
  register: lpar1

- name: Ensure the LPAR is active (using the default image profile when it needs to be activated), and then set the CP sharing weight to 20
  zhmc_lpar:
    hmc_host: "{{ my_hmc_host }}"
    hmc_auth: "{{ my_hmc_auth }}"
    cpc_name: "{{ my_cpc_name }}"
    name: "{{ my_lpar_name }}"
    state: active
    properties:
      initial_processing_weight: 20
  register: lpar1

- name: Ensure the LPAR is active (using image profile LPAR2 when it needs to be activated)
  zhmc_lpar:
    hmc_host: "{{ my_hmc_host }}"
    hmc_auth: "{{ my_hmc_auth }}"
    cpc_name: "{{ my_cpc_name }}"
    name: "{{ my_lpar_name }}"
    state: active
    activation_profile_name: LPAR2
  register: lpar1

- name: Ensure the LPAR is loaded (using the default image profile when it needs to be activated)
  zhmc_lpar:
    hmc_host: "{{ my_hmc_host }}"
    hmc_auth: "{{ my_hmc_auth }}"
    cpc_name: "{{ my_cpc_name }}"
    name: "{{ my_lpar_name }}"
    state: loaded
  register: lpar1

- name: Ensure the LPAR is initialized for loading, clearing its memory
  zhmc_lpar:
    hmc_host: "{{ my_hmc_host }}"
    hmc_auth: "{{ my_hmc_auth }}"
    cpc_name: "{{ my_cpc_name }}"
    name: "{{ my_lpar_name }}"
    state: reset_clear
  register: lpar1

- name: Ensure the LPAR is initialized for loading, not clearing its memory
  zhmc_lpar:
    hmc_host: "{{ my_hmc_host }}"
    hmc_auth: "{{ my_hmc_auth }}"
    cpc_name: "{{ my_cpc_name }}"
    name: "{{ my_lpar_name }}"
    state: reset_normal
  register: lpar1

- name: Ensure the CP sharing weight of the LPAR is 30
  zhmc_lpar:
    hmc_host: "{{ my_hmc_host }}"
    hmc_auth: "{{ my_hmc_auth }}"
    cpc_name: "{{ my_cpc_name }}"
    name: "{{ my_lpar_name }}"
    state: set
    properties:
      initial_processing_weight: 30
  register: lpar1

- name: Gather facts about the LPAR
  zhmc_lpar:
    hmc_host: "{{ my_hmc_host }}"
    hmc_auth: "{{ my_hmc_auth }}"
    cpc_name: "{{ my_cpc_name }}"
    name: "{{ my_lpar_name }}"
    state: facts
  register: lpar1

"""

RETURN = """
changed:
  description: Indicates if any change has been made by the module.
    For C(state=facts), always will be false.
  returned: always
  type: bool
msg:
  description: An error message that describes the failure.
  returned: failure
  type: str
lpar:
  description:
    - "The resource properties of the LPAR, after any specified updates have
       been applied."
    - "Note that the returned properties may show different values than the ones
       that were specified as input for the update. For example, memory
       properties may be rounded up, hexadecimal strings may be shown with a
       different representation format, and other properties may change as a
       result of updating some properties. For details, see the data model of
       the 'Logical Partition' object in the :term:`HMC API` book."
  returned: success
  type: dict
  contains:
    name:
      description: "LPAR name"
      type: str
    "{property}":
      description: "Additional properties of the LPAR, as described in
        the data model of the 'Logical Partition' object in the
        :term:`HMC API` book.
        The property names have hyphens (-) as described in that book."
  sample:
    {
        "absolute-aap-capping": {
            "type": "none"
        },
        "absolute-cbp-capping": {
            "type": "none"
        },
        "absolute-cf-capping": {
            "type": "none"
        },
        "absolute-ifl-capping": {
            "type": "none"
        },
        "absolute-processing-capping": {
            "type": "none"
        },
        "absolute-ziip-capping": {
            "type": "none"
        },
        "acceptable-status": [
            "operating"
        ],
        "activation-mode": "ssc",
        "additional-status": "",
        "class": "logical-partition",
        "cluster-name": "",
        "current-aap-processing-weight": null,
        "current-aap-processing-weight-capped": null,
        "current-cbp-processing-weight": null,
        "current-cbp-processing-weight-capped": null,
        "current-cf-processing-weight": null,
        "current-cf-processing-weight-capped": null,
        "current-ifl-processing-weight": null,
        "current-ifl-processing-weight-capped": null,
        "current-processing-weight": 10,
        "current-processing-weight-capped": false,
        "current-vfm-storage": 0,
        "current-ziip-processing-weight": null,
        "current-ziip-processing-weight-capped": null,
        "defined-capacity": 0,
        "description": "LPAR Image",
        "group-profile-capacity": null,
        "group-profile-uri": null,
        "has-operating-system-messages": true,
        "has-unacceptable-status": false,
        "initial-aap-processing-weight": null,
        "initial-aap-processing-weight-capped": null,
        "initial-cbp-processing-weight": null,
        "initial-cbp-processing-weight-capped": null,
        "initial-cf-processing-weight": null,
        "initial-cf-processing-weight-capped": null,
        "initial-ifl-processing-weight": null,
        "initial-ifl-processing-weight-capped": null,
        "initial-processing-weight": 10,
        "initial-processing-weight-capped": false,
        "initial-vfm-storage": 0,
        "initial-ziip-processing-weight": null,
        "initial-ziip-processing-weight-capped": null,
        "is-locked": false,
        "last-used-activation-profile": "ANGEL",
        "last-used-boot-record-logical-block-address": "0",
        "last-used-disk-partition-id": 0,
        "last-used-load-address": "00000",
        "last-used-load-parameter": "",
        "last-used-logical-unit-number": "0",
        "last-used-operating-system-specific-load-parameters": "",
        "last-used-world-wide-port-name": "0",
        "maximum-aap-processing-weight": null,
        "maximum-cbp-processing-weight": null,
        "maximum-cf-processing-weight": null,
        "maximum-ifl-processing-weight": null,
        "maximum-processing-weight": 0,
        "maximum-vfm-storage": 0,
        "maximum-ziip-processing-weight": null,
        "minimum-aap-processing-weight": null,
        "minimum-cbp-processing-weight": null,
        "minimum-cf-processing-weight": null,
        "minimum-ifl-processing-weight": null,
        "minimum-processing-weight": 0,
        "minimum-ziip-processing-weight": null,
        "name": "ANGEL",
        "next-activation-profile-name": "ANGEL",
        "object-id": "10fa8489-4e06-3601-9170-eee82e26937c",
        "object-uri": "/api/logical-partitions/10fa8489-4e06-3601-9170-eee82e26937c",
        "os-ipl-token": "0000000000000000",
        "os-level": "1.0.0",
        "os-name": "INSTALL",
        "os-type": "SSC",
        "parent": "/api/cpcs/4f01576a-c3f6-3224-a951-b1bf361886a4",
        "partition-identifier": "33",
        "partition-number": "2f",
        "program-status-word-information": [
            {
                "cpid": "00",
                "psw": "0706C00180000000000000000070E050"
            },
            {
                "cpid": "01",
                "psw": "0706C00180000000000000000070E050"
            },
            {
                "cpid": "02",
                "psw": "0706C00180000000000000000070E050"
            },
            {
                "cpid": "03",
                "psw": "0706C00180000000000000000070E050"
            },
            {
                "cpid": "04",
                "psw": "0706C00180000000000000000070E050"
            },
            {
                "cpid": "05",
                "psw": "0706C00180000000000000000070E050"
            },
            {
                "cpid": "06",
                "psw": "0706C00180000000000000000070E050"
            },
            {
                "cpid": "07",
                "psw": "0706C00180000000000000000070E050"
            },
            {
                "cpid": "08",
                "psw": "0706C00180000000000000000070E050"
            },
            {
                "cpid": "09",
                "psw": "0706C00180000000000000000070E050"
            }
        ],
        "ssc-dns-info": null,
        "ssc-gateway-info": null,
        "ssc-host-name": null,
        "ssc-master-userid": null,
        "ssc-network-info": null,
        "status": "operating",
        "storage-central-allocation": [
            {
                "current": 8192,
                "gap": 102400,
                "initial": 8192,
                "maximum": 8192,
                "origin": 127322112,
                "storage-element-type": "central"
            }
        ],
        "storage-expanded-allocation": [],
        "sysplex-name": null,
        "workload-manager-enabled": false
    }
"""

import logging  # noqa: E402
import traceback  # noqa: E402
from ansible.module_utils.basic import AnsibleModule  # noqa: E402

from ..module_utils.common import log_init, Error, ParameterError, \
    StatusError, ensure_lpar_inactive, ensure_lpar_active, ensure_lpar_loaded, \
    get_hmc_auth, get_session, to_unicode, process_normal_property, \
    missing_required_lib, common_fail_on_import_errors  # noqa: E402

try:
    import requests.packages.urllib3
    IMP_URLLIB3_ERR = None
except ImportError:
    IMP_URLLIB3_ERR = traceback.format_exc()

try:
    import zhmcclient
    IMP_ZHMCCLIENT_ERR = None
except ImportError:
    IMP_ZHMCCLIENT_ERR = traceback.format_exc()

# Python logger name for this module
LOGGER_NAME = 'zhmc_lpar'

LOGGER = logging.getLogger(LOGGER_NAME)

# Defaults for module input parameters
DEFAULT_ACTIVATION_PROFILE_NAME = None
DEFAULT_FORCE = False

# Dictionary of properties of LPAR resources, in this format:
#   name: (allowed, create, update, update_while_active, eq_func, type_cast)
# where:
#   name: Name of the property according to the data model, with hyphens
#     replaced by underscores (this is how it is or would be specified in
#     the 'properties' module parameter).
#   allowed: Indicates whether it is allowed in the 'properties' module
#     parameter.
#   create: Always False because LPARs cannot be created - still defined for
#     consistency with other modules.
#   update: Indicates whether it can be specified for the "Update Logical
#     Partition Properties" operation (at all).
#   update_while_active: Indicates whether it can be specified for the "Update
#     Logical Partition Properties" operation while the LPAR is active. None
#     means "not applicable" (i.e. update=False).
#   eq_func: Equality test function for two values of the property; None means
#     to use Python equality.
#   type_cast: Type cast function for an input value of the property; None
#     means to use it directly. This can be used for example to convert
#     integers provided as strings by Ansible back into integers (that is a
#     current deficiency of Ansible).
ZHMC_LPAR_PROPERTIES = {

    # create-only properties: None - LPARs cannot be created

    # update-only properties:
    'acceptable_status': (True, False, True, True, None, None),
    'next_activation_profile_name': (True, False, True, True, None, None),
    'initial_processing_weight': (True, False, True, True, None, int),
    'initial_processing_weight_capped': (True, False, True, True, None, None),
    'minimum_processing_weight': (True, False, True, True, None, int),
    'maximum_processing_weight': (True, False, True, True, None, int),
    'workload_manager_enabled': (True, False, True, True, None, None),
    'absolute_processing_capping': (True, False, True, True, None, None),  # absolute_capping object
    'defined_capacity': (True, False, True, True, None, int),
    'number_general_purpose_cores': (True, False, True, True, None, int),
    'number_reserved_general_purpose_cores': (True, False, True, True, None, int),
    'number_icf_cores': (True, False, True, True, None, int),
    'number_reserved_icf_cores': (True, False, True, True, None, int),
    'number_ifl_cores': (True, False, True, True, None, int),
    'number_reserved_ifl_cores': (True, False, True, True, None, int),
    'number_ziip_cores': (True, False, True, True, None, int),
    'number_reserved_ziip_cores': (True, False, True, True, None, int),
    'number_aap_cores': (True, False, True, True, None, int),
    'number_reserved_aap_cores': (True, False, True, True, None, int),
    'initial_aap_processing_weight': (True, False, True, True, None, int),
    'initial_aap_processing_weight_capped': (True, False, True, True, None, None),
    'minimum_aap_processing_weight': (True, False, True, True, None, int),
    'maximum_aap_processing_weight': (True, False, True, True, None, int),
    'absolute_aap_capping': (True, False, True, True, None, None),  # absolute_capping object
    'initial_ifl_processing_weight': (True, False, True, True, None, int),
    'initial_ifl_processing_weight_capped': (True, False, True, True, None, None),
    'minimum_ifl_processing_weight': (True, False, True, True, None, int),
    'maximum_ifl_processing_weight': (True, False, True, True, None, int),
    'absolute_ifl_capping': (True, False, True, True, None, None),  # absolute_capping object
    'initial_cbp_processing_weight': (True, False, True, True, None, int),
    'initial_cbp_processing_weight_capped': (True, False, True, True, None, None),
    'minimum_cbp_processing_weight': (True, False, True, True, None, int),
    'maximum_cbp_processing_weight': (True, False, True, True, None, int),
    'absolute_cbp_capping': (True, False, True, True, None, None),  # absolute_capping object
    'initial_ziip_processing_weight': (True, False, True, True, None, int),
    'initial_ziip_processing_weight_capped': (True, False, True, True, None, None),
    'minimum_ziip_processing_weight': (True, False, True, True, None, int),
    'maximum_ziip_processing_weight': (True, False, True, True, None, int),
    'absolute_ziip_capping': (True, False, True, True, None, None),  # absolute_capping object
    'initial_cf_processing_weight': (True, False, True, True, None, int),
    'initial_cf_processing_weight_capped': (True, False, True, True, None, None),
    'minimum_cf_processing_weight': (True, False, True, True, None, int),
    'maximum_cf_processing_weight': (True, False, True, True, None, int),
    'absolute_cf_capping': (True, False, True, True, None, None),  # absolute_capping object
    'zaware_hostname': (True, False, True, True, None, None),
    'zaware_master_userid': (True, False, True, True, None, None),
    'zaware_master_pw': (True, False, True, True, None, None),
    'zaware_network_info': (True, False, True, True, None, None),  # Array of zaware_network objects
    'zaware_gateway_info': (True, False, True, True, None, None),  # ip_info object
    'zaware_dns_info': (True, False, True, True, None, None),  # Array of ip_info objects

    # read_only properties:
    'object_uri': (False, False, False, None, None, None),
    'object_id': (False, False, False, None, None, None),
    'parent': (False, False, False, None, None, None),
    'class': (False, False, False, None, None, None),
    'name': (False, False, False, None, None, None),
    'description': (False, False, False, None, None, None),
    'is_locked': (False, False, False, None, None, None),
    'effective_properties_apply': (False, False, False, None, None, None),
    'status': (False, False, False, None, None, None),
    'os_name': (False, False, False, None, None, None),
    'os_type': (False, False, False, None, None, None),
    'os_version': (False, False, False, None, None, None),
    'sysplex_name': (False, False, False, None, None, None),
    'is_sub_capacity_boost_active': (False, False, False, None, None, None),
    'is_secure_execution_enabled': (False, False, False, None, None, None),
    'is_ziip_capacity_boost_active': (False, False, False, None, None, None),
    'has_operating_system_messages': (False, False, False, None, None, None),
    'has_important_unviewed_operating_system_messages': (False, False, False, None, None, None),
    'activation_mode': (False, False, False, None, None, None),
    'last_used_activation_profile': (False, False, False, None, None, None),
    'last_used_load_type': (False, False, False, None, None, None),
    'last_used_load_address': (False, False, False, None, None, None),
    'last_used_load_parameter': (False, False, False, None, None, None),
    'last_used_secure_boot': (False, False, False, None, None, None),
    'last_used_world_wide_port_name': (False, False, False, None, None, None),
    'last_used_logical_unit_number': (False, False, False, None, None, None),
    'last_used_disk_partition_id': (False, False, False, None, None, None),
    'last_used_operating_system_specific_load_parameters': (False, False, False, None, None, None),
    'last_used_boot_record_logical_block_address': (False, False, False, None, None, None),
    'current_processing_weight': (False, False, False, None, None, None),
    'current_processing_weight_capped': (False, False, False, None, None, None),
    'processor_usage': (False, False, False, None, None, None),
    'number_general_purpose_processors': (False, False, False, None, None, None),
    'number_reserved_general_purpose_processors': (False, False, False, None, None, None),
    'number_icf_processors': (False, False, False, None, None, None),
    'number_reserved_icf_processors': (False, False, False, None, None, None),
    'number_ifl_processors': (False, False, False, None, None, None),
    'number_reserved_ifl_processors': (False, False, False, None, None, None),
    'number_ziip_processors': (False, False, False, None, None, None),
    'number_reserved_ziip_processors': (False, False, False, None, None, None),
    'number_aap_processors': (False, False, False, None, None, None),
    'number_reserved_aap_processors': (False, False, False, None, None, None),
    'current_ifl_processing_weight': (False, False, False, None, None, None),
    'current_ifl_processing_weight_capped': (False, False, False, None, None, None),
    'current_cbp_processing_weight': (False, False, False, None, None, None),
    'current_cbp_processing_weight_capped': (False, False, False, None, None, None),
    'current_ziip_processing_weight': (False, False, False, None, None, None),
    'current_ziip_processing_weight_capped': (False, False, False, None, None, None),
    'current_cf_processing_weight': (False, False, False, None, None, None),
    'current_cf_processing_weight_capped': (False, False, False, None, None, None),
    'program_status_word_information': (False, False, False, None, None, None),  # Array of psw_description objects
    'initial_vfm_storage': (False, False, False, None, None, None),
    'maximum_vfm_storage': (False, False, False, None, None, None),
    'current_vfm_storage': (False, False, False, None, None, None),
    'os_ipl_token': (False, False, False, None, None, None),
    'group_profile_capacity': (False, False, False, None, None, None),
    'group_profile_uri': (False, False, False, None, None, None),
    # ssc_* properties are not supported; they were removed starting with z14.
    'storage_central_allocation': (False, False, False, None, None, None),  # Array of central_storage_allocation objects
    'storage_expanded_allocation': (False, False, False, None, None, None),  # Array of expanded_storage_allocation objects
    'target_name': (False, False, False, None, None, None),
    'request_origin': (False, False, False, None, None, None),
}


def process_properties(cpc, lpar, params):
    """
    Process the properties specified in the 'properties' module parameter,
    and return a dictionarys (update_props) that contains the properties that
    can be updated. If the resource exists, the input property values are
    compared with the existing resource property values and the returned set
    of properties is the minimal set of properties that need to be changed.

    - Underscores in the property names are translated into hyphens.
    - The presence of read-only properties, invalid properties (i.e. not
      defined in the data model for LPARs), and properties that are not
      allowed because of restrictions or because they are auto-created from
      an artificial property is surfaced by raising ParameterError.
    - The properties resulting from handling artificial properties are
      added to the returned dictionaries.

    Parameters:

      cpc (zhmcclient.Cpc): CPC with the LPAR to be updated.

      lpar (zhmcclient.Lpar): LPAR to be updated with the full
        set of current properties, or `None` if it did not previously exist.

      params (dict): Module input parameters.

    Returns:
      update_props: dict of properties for zhmcclient.Lpar.update_properties()

    Raises:
      ParameterError: An issue with the module parameters.
    """
    create_props = {}
    update_props = {}
    stop = False

    # handle 'name' property
    # We looked up the LPAR by name, so we will never have to update
    # the LPAR name
    lpar_name = to_unicode(params['name'])

    # handle the other properties
    input_props = params.get('properties', {})
    if input_props is None:
        input_props = {}
    for prop_name in input_props:

        if prop_name not in ZHMC_LPAR_PROPERTIES:
            raise ParameterError(
                "Property {0!r} is not defined in the data model for "
                "LPARs.".format(prop_name))

        allowed, create, update, update_while_active, eq_func, type_cast = \
            ZHMC_LPAR_PROPERTIES[prop_name]

        if not allowed:
            raise ParameterError(
                "Property {0!r} is not allowed in the 'properties' module "
                "parameter.".format(prop_name))

        else:
            # Process a normal (= non-artificial) property
            _create_props, _update_props, _stop = process_normal_property(
                prop_name, ZHMC_LPAR_PROPERTIES, input_props, lpar)
            create_props.update(_create_props)
            update_props.update(_update_props)
            if _stop:
                stop = True

    if create_props:
        raise AssertionError(
            "create_props not empty for LPAR {0!r}".format(lpar_name))

    if stop:
        raise AssertionError(
            "stop set for LPAR {0!r}".format(lpar_name))

    return update_props


def add_artificial_properties(lpar_properties, lpar):
    """
    Add artificial properties to the lpar_properties dict.

    Upon return, the lpar_properties dict has been extended by these
    artificial properties:

    None - This module does not add any artificial properties.
    """
    pass


def ensure_inactive(params, check_mode):
    """
    Ensure that the LPAR is inactive. No properties are updated.

    Raises:
      ParameterError: An issue with the module parameters.
      StatusError: An issue with the LPAR status.
      zhmcclient.Error: Any zhmcclient exception can happen.
    """

    host = params['hmc_host']
    userid, password, ca_certs, verify = get_hmc_auth(params['hmc_auth'])
    cpc_name = params['cpc_name']
    lpar_name = params['name']
    _faked_session = params.get('_faked_session', None)

    properties = params.get('properties', None)
    if properties:
        raise ParameterError(
            "Properties must not be specified for state=inactive with "
            "LPAR {0!r}.".format(lpar_name))

    changed = False
    result = {}

    session = get_session(
        _faked_session, host, userid, password, ca_certs, verify)
    try:
        client = zhmcclient.Client(session)
        cpc = client.cpcs.find(name=cpc_name)
        lpar = cpc.lpars.find(name=lpar_name)
        # The default exception handling is sufficient for the above.

        # If we got here, the LPAR exists.

        # Deactivate the LPAR.
        changed |= ensure_lpar_inactive(LOGGER, lpar, check_mode)

        return changed, result

    finally:
        session.logoff()


def perform_reset_clear(params, check_mode):
    """
    Perform the 'Reset Clear' HMC operation.
    No properties can be updated.

    Raises:
      ParameterError: An issue with the module parameters.
      StatusError: An issue with the LPAR status.
      zhmcclient.Error: Any zhmcclient exception can happen.
    """

    host = params['hmc_host']
    userid, password, ca_certs, verify = get_hmc_auth(params['hmc_auth'])
    cpc_name = params['cpc_name']
    lpar_name = params['name']
    force = params['force']
    os_ipl_token = params['os_ipl_token']
    _faked_session = params.get('_faked_session', None)

    properties = params.get('properties', None)
    if properties:
        raise ParameterError(
            "Properties must not be specified for state=reset_clear with "
            "LPAR {0!r}.".format(lpar_name))

    changed = False
    result = {}

    session = get_session(
        _faked_session, host, userid, password, ca_certs, verify)
    try:
        client = zhmcclient.Client(session)
        cpc = client.cpcs.find(name=cpc_name)
        lpar = cpc.lpars.find(name=lpar_name)
        # The default exception handling is sufficient for the above.

        # If we got here, the LPAR exists.

        # Perform the 'Reset Clear' operation on the LPAR.
        if not check_mode:
            lpar.reset_clear(force=force, os_ipl_token=os_ipl_token)
        changed = True
        result = {}

        return changed, result

    finally:
        session.logoff()


def perform_reset_normal(params, check_mode):
    """
    Perform the 'Reset Normal' HMC operation.
    No properties can be updated.

    Raises:
      ParameterError: An issue with the module parameters.
      StatusError: An issue with the LPAR status.
      zhmcclient.Error: Any zhmcclient exception can happen.
    """

    host = params['hmc_host']
    userid, password, ca_certs, verify = get_hmc_auth(params['hmc_auth'])
    cpc_name = params['cpc_name']
    lpar_name = params['name']
    force = params['force']
    os_ipl_token = params['os_ipl_token']
    _faked_session = params.get('_faked_session', None)

    properties = params.get('properties', None)
    if properties:
        raise ParameterError(
            "Properties must not be specified for state=reset_normal with "
            "LPAR {0!r}.".format(lpar_name))

    changed = False
    result = {}

    session = get_session(
        _faked_session, host, userid, password, ca_certs, verify)
    try:
        client = zhmcclient.Client(session)
        cpc = client.cpcs.find(name=cpc_name)
        lpar = cpc.lpars.find(name=lpar_name)
        # The default exception handling is sufficient for the above.

        # If we got here, the LPAR exists.

        # Perform the 'Reset Clear' operation on the LPAR.
        if not check_mode:
            lpar.reset_normal(force=force, os_ipl_token=os_ipl_token)
        changed = True
        result = {}

        return changed, result

    finally:
        session.logoff()


def ensure_active(params, check_mode):
    """
    Ensure that the LPAR is active and has the specified properties.

    If the LPAR has auto-load set, it will continue to become loaded.
    If the LPAR was already loaded, it remains loaded.

    Raises:
      ParameterError: An issue with the module parameters.
      StatusError: An issue with the LPAR status.
      zhmcclient.Error: Any zhmcclient exception can happen.
    """

    host = params['hmc_host']
    userid, password, ca_certs, verify = get_hmc_auth(params['hmc_auth'])
    cpc_name = params['cpc_name']
    lpar_name = params['name']
    activation_profile_name = params.get(
        'activation_profile_name', DEFAULT_ACTIVATION_PROFILE_NAME)
    force = params.get('force', DEFAULT_FORCE)
    _faked_session = params.get('_faked_session', None)

    changed = False
    result = {}

    session = get_session(
        _faked_session, host, userid, password, ca_certs, verify)
    try:
        client = zhmcclient.Client(session)
        cpc = client.cpcs.find(name=cpc_name)
        lpar = cpc.lpars.find(name=lpar_name)
        # The default exception handling is sufficient for the above.

        # If we got here, the LPAR exists.

        # Bring the LPAR into the active status.
        changed |= ensure_lpar_active(
            LOGGER, lpar, check_mode,
            activation_profile_name=activation_profile_name,
            force=force)

        # Update the properties of the LPAR.
        lpar.pull_full_properties()
        lpar_properties = dict(lpar.properties)
        update_props = process_properties(cpc, lpar, params)
        if update_props:
            if not check_mode:
                lpar.update_properties(update_props)
                # We refresh the properties after the update, in case an
                # input property value gets changed.
                lpar.pull_full_properties()
                lpar_properties = dict(lpar.properties)
            else:
                lpar_properties.update(update_props)
            changed = True

        result = lpar_properties
        add_artificial_properties(result, lpar)

        return changed, result

    finally:
        session.logoff()


def ensure_loaded(params, check_mode):
    """
    Ensure that the LPAR is loaded and has the specified properties.

    Raises:
      ParameterError: An issue with the module parameters.
      StatusError: An issue with the LPAR status.
      zhmcclient.Error: Any zhmcclient exception can happen.
    """

    host = params['hmc_host']
    userid, password, ca_certs, verify = get_hmc_auth(params['hmc_auth'])
    cpc_name = params['cpc_name']
    lpar_name = params['name']
    activation_profile_name = params.get(
        'activation_profile_name', DEFAULT_ACTIVATION_PROFILE_NAME)
    force = params.get('force', DEFAULT_FORCE)
    _faked_session = params.get('_faked_session', None)

    changed = False
    result = {}

    session = get_session(
        _faked_session, host, userid, password, ca_certs, verify)
    try:
        client = zhmcclient.Client(session)
        cpc = client.cpcs.find(name=cpc_name)
        lpar = cpc.lpars.find(name=lpar_name)
        # The default exception handling is sufficient for the above.

        # If we got here, the LPAR exists.

        # Bring the LPAR into the loaded status.
        changed |= ensure_lpar_loaded(
            LOGGER, lpar, check_mode,
            activation_profile_name=activation_profile_name,
            force=force)

        # Update the properties of the LPAR.
        lpar.pull_full_properties()
        lpar_properties = dict(lpar.properties)
        update_props = process_properties(cpc, lpar, params)
        if update_props:
            if not check_mode:
                lpar.update_properties(update_props)
                # We refresh the properties after the update, in case an
                # input property value gets changed.
                lpar.pull_full_properties()
                lpar_properties = dict(lpar.properties)
            else:
                lpar_properties.update(update_props)
            changed = True

        result = lpar_properties
        add_artificial_properties(result, lpar)

        return changed, result

    finally:
        session.logoff()


def ensure_set(params, check_mode):
    """
    Ensure that the LPAR properties have been updated, without activating
    or deactivating the LPAR.

    This requires the LPAR to be active or loaded.

    Raises:
      ParameterError: An issue with the module parameters.
      StatusError: An issue with the LPAR status.
      zhmcclient.Error: Any zhmcclient exception can happen.
    """

    host = params['hmc_host']
    userid, password, ca_certs, verify = get_hmc_auth(params['hmc_auth'])
    cpc_name = params['cpc_name']
    lpar_name = params['name']
    _faked_session = params.get('_faked_session', None)

    changed = False
    result = {}

    session = get_session(
        _faked_session, host, userid, password, ca_certs, verify)
    try:
        client = zhmcclient.Client(session)
        cpc = client.cpcs.find(name=cpc_name)
        lpar = cpc.lpars.find(name=lpar_name)
        # The default exception handling is sufficient for the above.

        # If we got here, the LPAR exists.

        # Update the properties of the LPAR.
        lpar.pull_full_properties()
        lpar_properties = dict(lpar.properties)
        update_props = process_properties(cpc, lpar, params)
        if update_props:
            if not check_mode:
                lpar.update_properties(update_props)
                # We refresh the properties after the update, in case an
                # input property value gets changed.
                lpar.pull_full_properties()
                lpar_properties = dict(lpar.properties)
            else:
                # Simulate the update behavior in check mode
                status = lpar.properties.get('status', None)
                if status not in ('not-operating', 'operating', 'acceptable'):
                    raise StatusError(
                        "LPAR {0!r} has status {1} and cannot be updated.".
                        format(lpar_name, status))
                lpar_properties.update(update_props)
            changed = True

        result = lpar_properties
        add_artificial_properties(result, lpar)

        return changed, result

    finally:
        session.logoff()


def facts(params, check_mode):
    """
    Return LPAR facts.

    Raises:
      ParameterError: An issue with the module parameters.
      zhmcclient.Error: Any zhmcclient exception can happen.
    """

    host = params['hmc_host']
    userid, password, ca_certs, verify = get_hmc_auth(params['hmc_auth'])
    cpc_name = params['cpc_name']
    lpar_name = params['name']
    _faked_session = params.get('_faked_session', None)

    properties = params.get('properties', None)
    if properties:
        raise ParameterError(
            "Properties must not be specified for state=facts with "
            "LPAR {0!r}.".format(lpar_name))

    changed = False
    result = {}

    session = get_session(
        _faked_session, host, userid, password, ca_certs, verify)
    try:
        # The default exception handling is sufficient for this code
        client = zhmcclient.Client(session)
        cpc = client.cpcs.find(name=cpc_name)

        lpar = cpc.lpars.find(name=lpar_name)
        lpar.pull_full_properties()

        result = dict(lpar.properties)
        add_artificial_properties(result, lpar)

        return changed, result

    finally:
        session.logoff()


def perform_task(params, check_mode):
    """
    Perform the task for this module, dependent on the 'state' module
    parameter.

    If check_mode is True, check whether changes would occur, but don't
    actually perform any changes.

    Raises:
      ParameterError: An issue with the module parameters.
      StatusError: An issue with the LPAR status.
      zhmcclient.Error: Any zhmcclient exception can happen.
    """
    actions = {
        'inactive': ensure_inactive,
        'reset_clear': perform_reset_clear,
        'reset_normal': perform_reset_normal,
        'active': ensure_active,
        'loaded': ensure_loaded,
        'set': ensure_set,
        'facts': facts,
    }
    return actions[params['state']](params, check_mode)


def main():

    # The following definition of module input parameters must match the
    # description of the options in the DOCUMENTATION string.
    argument_spec = dict(
        hmc_host=dict(required=True, type='str'),
        hmc_auth=dict(
            required=True,
            type='dict',
            options=dict(
                userid=dict(required=True, type='str'),
                password=dict(required=True, type='str', no_log=True),
                ca_certs=dict(required=False, type='str', default=None),
                verify=dict(required=False, type='bool', default=True),
            ),
        ),
        cpc_name=dict(required=True, type='str'),
        name=dict(required=True, type='str'),
        state=dict(
            required=True, type='str',
            choices=['inactive', 'reset_clear', 'reset_normal', 'active',
                     'loaded', 'set', 'facts']),
        activation_profile_name=dict(
            required=False, type='str',
            default=DEFAULT_ACTIVATION_PROFILE_NAME),
        force=dict(required=False, type='bool', default=DEFAULT_FORCE),
        os_ipl_token=dict(required=False, type='str', default=None),
        # Note: os_ipl_token is not a secret
        properties=dict(required=False, type='dict', default={}),
        log_file=dict(required=False, type='str', default=None),
        _faked_session=dict(required=False, type='raw'),
    )

    module = AnsibleModule(
        argument_spec=argument_spec,
        supports_check_mode=True)

    if IMP_URLLIB3_ERR is not None:
        module.fail_json(msg=missing_required_lib("requests"),
                         exception=IMP_URLLIB3_ERR)

    requests.packages.urllib3.disable_warnings()

    if IMP_ZHMCCLIENT_ERR is not None:
        module.fail_json(msg=missing_required_lib("zhmcclient"),
                         exception=IMP_ZHMCCLIENT_ERR)

    common_fail_on_import_errors(module)

    log_file = module.params['log_file']
    log_init(LOGGER_NAME, log_file)

    _params = dict(module.params)
    del _params['hmc_auth']
    LOGGER.debug("Module entry: params: %r", _params)

    try:

        changed, result = perform_task(module.params, module.check_mode)

    except (Error, zhmcclient.Error) as exc:
        # These exceptions are considered errors in the environment or in user
        # input. They have a proper message that stands on its own, so we
        # simply pass that message on and will not need a traceback.
        msg = "{0}: {1}".format(exc.__class__.__name__, exc)
        LOGGER.debug(
            "Module exit (failure): msg: %s", msg)
        module.fail_json(msg=msg)
    # Other exceptions are considered module errors and are handled by Ansible
    # by showing the traceback.

    LOGGER.debug(
        "Module exit (success): changed: %r, cpc: %r", changed, result)
    module.exit_json(changed=changed, lpar=result)


if __name__ == '__main__':
    main()
