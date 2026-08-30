#!/usr/bin/python
# Copyright 2017-2020 IBM Corp. All Rights Reserved.
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
module: zhmc_partition
version_added: "2.9.0"
short_description: Create partitions
description:
  - Gather facts about a partition of a CPC (Z system), including its HBAs,
    NICs, and virtual functions.
  - Create, update, or delete a partition. The HBAs, NICs, and virtual
   functions of the partition are managed by separate Ansible modules.
  - Start or stop a partition.
seealso:
  - module: zhmc_hba
  - module: zhmc_nic
  - module: zhmc_virtual_function
author:
  - Andreas Maier (@andy-maier)
  - Andreas Scheuring (@scheuran)
  - Juergen Leopold (@leopoldjuergen)
requirements:
  - Access to the WS API of the HMC of the targeted Z system
    (see :term:`HMC API`).
  - The targeted Z system must be in the Dynamic Partition Manager (DPM)
    operational mode.
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
      - The name of the CPC with the target partition.
    type: str
    required: true
  name:
    description:
      - The name of the target partition.
    type: str
    required: true
  state:
    description:
      - "The desired state for the partition. All states are fully idempotent
         within the limits of the properties that can be changed:"
      - "* C(absent): Ensures that the partition does not exist in the specified
         CPC."
      - "* C(stopped): Ensures that the partition exists in the specified CPC,
         has the specified properties, and is in one of the inactive statuses
         ('stopped', 'terminated', 'paused', 'reservation-error')."
      - "* C(active): Ensures that the partition exists in the specified CPC,
         has the specified properties, and is in one of the active statuses
         ('active', 'degraded')."
      - "* C(facts): Returns the partition properties and the properties of its
         child resources (HBAs, NICs, and virtual functions)."
    type: str
    required: true
    choices: ['absent', 'stopped', 'active', 'facts']
  properties:
    description:
      - "Dictionary with input properties for the partition, for
         C(state=stopped) and C(state=active). Key is the property name with
         underscores instead of hyphens, and value is the property value in
         YAML syntax. Integer properties may also be provided as decimal
         strings. Will be ignored for C(state=absent)."
      - "The possible input properties in this dictionary are the properties
         defined as writeable in the data model for Partition resources
         (where the property names contain underscores instead of hyphens),
         with the following exceptions:"
      - "* C(name): Cannot be specified because the name has already been
         specified in the C(name) module parameter."
      - "* C(type): Cannot be changed once the partition exists, because
         updating it is not supported."
      - "* C(boot_storage_device): Cannot be specified because this information
         is specified using the artificial property C(boot_storage_hba_name)."
      - "* C(boot_network_device): Cannot be specified because this information
         is specified using the artificial property C(boot_network_nic_name)."
      - "* C(boot_storage_hba_name): The name of the HBA whose URI is used to
         construct C(boot_storage_device). Specifying it requires that the
         partition exists."
      - "* C(boot_network_nic_name): The name of the NIC whose URI is used to
         construct C(boot_network_device). Specifying it requires that the
         partition exists."
      - "* C(crypto_configuration): The crypto configuration for the partition,
         in the format of the C(crypto-configuration) property of the
         partition (see :term:`HMC API` for details), with the exception that
         adapters are specified with their names in field
         C(crypto_adapter_names) instead of their URIs in field
         C(crypto_adapter_uris). If the C(crypto_adapter_names) field is null,
         all crypto adapters of the CPC will be used."
      - "Properties omitted in this dictionary will remain unchanged when the
         partition already exists, and will get the default value defined in
         the data model for partitions in the :term:`HMC API` when the partition
         is being created."
    type: dict
    required: false
    default: null
  expand_storage_groups:
    description:
      - "Boolean that controls whether the returned partition contains
         an additional artificial property 'storage-groups' that is the list
         of storage groups attached to the partition, with properties as
         described for the zhmc_storage_group module with expand=true."
    required: false
    type: bool
    default: false
  expand_crypto_adapters:
    description:
      - "Boolean that controls whether the returned partition contains
         an additional artificial property 'crypto-adapters' in its
         'crypto-configuration' property that is the list
         of crypto adapters attached to the partition, with properties as
         described for the zhmc_adapter module."
    required: false
    type: bool
    default: false
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

# Because configuring LUN masking in the SAN requires the host WWPN, and the
# host WWPN is automatically assigned and will be known only after an HBA has
# been added to the partition, the partition needs to be created in stopped
# state. Also, because the HBA has not yet been created, the boot
# configuration cannot be done yet:
- name: Ensure the partition exists and is stopped
  zhmc_partition:
    hmc_host: "{{ my_hmc_host }}"
    hmc_auth: "{{ my_hmc_auth }}"
    cpc_name: "{{ my_cpc_name }}"
    name: "{{ my_partition_name }}"
    state: stopped
    properties:
      description: "zhmc Ansible modules: Example partition 1"
      ifl_processors: 2
      initial_memory: 1024
      maximum_memory: 1024
  register: part1

# After an HBA has been added (see Ansible module zhmc_hba), and LUN masking
# has been configured in the SAN, and a bootable image is available at the
# configured LUN and target WWPN, the partition can be configured for boot
# from the FCP LUN and can be started:
- name: Configure boot device and start the partition
  zhmc_partition:
    hmc_host: "{{ my_hmc_host }}"
    hmc_auth: "{{ my_hmc_auth }}"
    cpc_name: "{{ my_cpc_name }}"
    name: "{{ my_partition_name }}"
    state: active
    properties:
      boot_device: storage-adapter
      boot_storage_device_hba_name: hba1
      boot_logical_unit_number: 00000000001
      boot_world_wide_port_name: abcdefabcdef
  register: part1

- name: Ensure the partition does not exist
  zhmc_partition:
    hmc_host: "{{ my_hmc_host }}"
    hmc_auth: "{{ my_hmc_auth }}"
    cpc_name: "{{ my_cpc_name }}"
    name: "{{ my_partition_name }}"
    state: absent

- name: Define crypto configuration
  zhmc_partition:
    hmc_host: "{{ my_hmc_host }}"
    hmc_auth: "{{ my_hmc_auth }}"
    cpc_name: "{{ my_cpc_name }}"
    name: "{{ my_partition_name }}"
    state: active
    properties:
      crypto_configuration:
        crypto_adapter_names:
          - adapter1
          - adapter2
        crypto_domain_configurations:
          - domain_index: 0
            access_mode: control-usage
          - domain_index: 1
            access_mode: control
  register: part1

- name: Gather facts about a partition
  zhmc_partition:
    hmc_host: "{{ my_hmc_host }}"
    hmc_auth: "{{ my_hmc_auth }}"
    cpc_name: "{{ my_cpc_name }}"
    name: "{{ my_partition_name }}"
    state: facts
    expand_storage_groups: true
    expand_crypto_adapters: true
  register: part1

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
partition:
  description:
    - "For C(state=absent), an empty dictionary."
    - "For C(state=stopped|active|facts), the resource properties of the
       partition after any changes, including its child resources as described
       below."
  returned: success
  type: dict
  contains:
    name:
      description: "Partition name"
      type: str
    "{property}":
      description: "Additional properties of the partition, as described in
        the data model of the 'Partition' object in the :term:`HMC API` book.
        The property names have hyphens (-) as described in that book."
    hbas:
      description: "HBAs of the partition. If the CPC does not have the
        storage-management feature enabled (ie. before z15), the list is
        empty."
      type: list
      elements: dict
      contains:
        name:
          description: "HBA name"
          type: str
        "{property}":
          description: "Additional properties of the HBA, as described in the
            data model of the 'HBA' element object of the 'Partition' object
            in the :term:`HMC API` book.
            The property names have hyphens (-) as described in that book."
    nics:
      description: "NICs of the partition."
      type: list
      elements: dict
      contains:
        name:
          description: "NIC name"
          type: str
        "{property}":
          description: "Additional properties of the NIC, as described in the
            data model of the 'NIC' element object of the 'Partition' object
            in the :term:`HMC API` book.
            The property names have hyphens (-) as described in that book."
    virtual-functions:
      description: "Virtual functions of the partition."
      type: list
      elements: dict
      contains:
        name:
          description: "Virtual function name"
          type: str
        "{property}":
          description: "Additional properties of the virtual function, as
            described in the data model of the 'Virtual Function' element
            object of the 'Partition' object in the :term:`HMC API` book.
            The property names have hyphens (-) as described in that book."
  sample:
    {
        "acceptable-status": [
            "active"
        ],
        "access-basic-counter-set": true,
        "access-basic-sampling": false,
        "access-coprocessor-group-set": false,
        "access-crypto-activity-counter-set": true,
        "access-diagnostic-sampling": false,
        "access-extended-counter-set": true,
        "access-global-performance-data": true,
        "access-problem-state-counter-set": true,
        "auto-start": false,
        "autogenerate-partition-id": true,
        "available-features-list": [
            {
                "description": "The DPM storage management approach in which
                  FCP and FICON storage resources are defined in Storage
                  Groups, which are attached to Partitions.",
                "name": "dpm-storage-management",
                "state": true
            }
        ],
        "boot-configuration-selector": 0,
        "boot-device": "none",
        "boot-ftp-host": null,
        "boot-ftp-insfile": null,
        "boot-ftp-username": null,
        "boot-iso-image-name": null,
        "boot-iso-ins-file": null,
        "boot-logical-unit-number": "",
        "boot-network-device": null,
        "boot-os-specific-parameters": "",
        "boot-record-lba": "0",
        "boot-removable-media": null,
        "boot-removable-media-type": null,
        "boot-storage-device": null,
        "boot-storage-volume": null,
        "boot-timeout": 60,
        "boot-world-wide-port-name": "",
        "class": "partition",
        "cp-absolute-processor-capping": false,
        "cp-absolute-processor-capping-value": 1.0,
        "cp-processing-weight-capped": false,
        "cp-processors": 0,
        "crypto-configuration": {
            "crypto-adapter-uris": [
                "/api/adapters/f1b97ed8-e578-11e8-a87c-00106f239c31",
            ],
            "crypto-domain-configurations": [
                {
                    "access-mode": "control-usage",
                    "domain-index": 2
                }
            ]
        },
        "current-cp-processing-weight": 1,
        "current-ifl-processing-weight": 1,
        "degraded-adapters": [],
        "description": "Colo dev partition",
        "has-unacceptable-status": false,
        "hba-uris": [],
        "hbas": [],
        "ifl-absolute-processor-capping": false,
        "ifl-absolute-processor-capping-value": 1.0,
        "ifl-processing-weight-capped": false,
        "ifl-processors": 12,
        "initial-cp-processing-weight": 100,
        "initial-ifl-processing-weight": 120,
        "initial-memory": 102400,
        "ipl-load-parameter": "",
        "is-locked": false,
        "maximum-cp-processing-weight": 999,
        "maximum-ifl-processing-weight": 999,
        "maximum-memory": 102400,
        "minimum-cp-processing-weight": 1,
        "minimum-ifl-processing-weight": 1,
        "name": "CSPF1",
        "nic-uris": [
            "/api/partitions/32323df4-f433-11ea-b67c-00106f239d19/nics/5956e97a-f433-11ea-b67c-00106f239d19"
        ],
        "nics": [
            {
                "adapter-id": "128",
                "adapter-name": "OSD_128_MGMT_NET2_30",
                "adapter-port": 0,
                "class": "nic",
                "description": "HAMGMT",
                "device-number": "0004",
                "element-id": "5956e97a-f433-11ea-b67c-00106f239d19",
                "element-uri": "/api/partitions/32323df4-f433-11ea-b67c-00106f239d19/nics/5956e97a-f433-11ea-b67c-00106f239d19",
                "mac-address": "02:d2:4d:80:b9:88",
                "name": "HAMGMT0",
                "parent": "/api/partitions/32323df4-f433-11ea-b67c-00106f239d19",
                "ssc-ip-address": null,
                "ssc-ip-address-type": null,
                "ssc-management-nic": false,
                "ssc-mask-prefix": null,
                "type": "osd",
                "virtual-switch-uri": "/api/virtual-switches/db2f0bec-e578-11e8-bd0a-00106f239c31",
                "vlan-id": null,
                "vlan-type": null
            }
        ],
        "object-id": "32323df4-f433-11ea-b67c-00106f239d19",
        "object-uri": "/api/partitions/32323df4-f433-11ea-b67c-00106f239d19",
        "os-name": "SSC",
        "os-type": "SSC",
        "os-version": "3.13.0",
        "parent": "/api/cpcs/66942455-4a14-3f99-8904-3e7ed5ca28d7",
        "partition-id": "08",
        "permit-aes-key-import-functions": true,
        "permit-cross-partition-commands": false,
        "permit-des-key-import-functions": true,
        "processor-management-enabled": false,
        "processor-mode": "shared",
        "reserve-resources": false,
        "reserved-memory": 0,
        "short-name": "CSPF1",
        "ssc-boot-selection": "appliance",
        "ssc-dns-servers": [
            "8.8.8.8"
        ],
        "ssc-host-name": "cpca-cspf1",
        "ssc-ipv4-gateway": null,
        "ssc-ipv6-gateway": null,
        "ssc-master-userid": "hmREST",
        "status": "active",
        "storage-group-uris": [
            "/api/storage-groups/4947c6d0-f433-11ea-8f73-00106f239d19"
        ],
        "threads-per-processor": 2,
        "type": "ssc",
        "virtual-function-uris": [],
        "virtual-functions": []
    }
"""

from collections import OrderedDict  # noqa: E402
import logging  # noqa: E402
import traceback  # noqa: E402
import uuid
import random
import types
from ansible.module_utils.basic import AnsibleModule  # noqa: E402
from operator import itemgetter  # noqa: E402

from ..module_utils.common import log_init, Error, ParameterError, \
    StatusError, stop_partition, start_partition, \
    wait_for_transition_completion, eq_hex, get_hmc_auth, get_session, \
    to_unicode, process_normal_property, missing_required_lib, \
    common_fail_on_import_errors  # noqa: E402

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
LOGGER_NAME = 'zhmc_partition'

LOGGER = logging.getLogger(LOGGER_NAME)


def required_boot_storage_adapter(partition_properties):
    """
    Indicates whether 'boot_storage_adapter' is a required input parameter.
    """
    boot_device = partition_properties.get('boot-device')
    return boot_device == 'storage-adapter'


def required_partition_id(partition_properties):
    """
    Indicates whether 'partition_id' is a required input parameter.
    """
    auto_generate = partition_properties.get('autogenerate-partition-id', True)
    return not auto_generate


def required_ifl_processors(partition_properties):
    """
    Indicates whether 'ifl_processors' is a required input parameter.
    """
    cp_processors = partition_properties.get('cp-processors', 0)
    return cp_processors == 0


def required_cp_processors(partition_properties):
    """
    Indicates whether 'cp_processors' is a required input parameter.
    """
    ifl_processors = partition_properties.get('ifl-processors', 0)
    return ifl_processors == 0


def required_boot_ftp(partition_properties):
    """
    Indicates whether 'boot_ftp_*' are required input parameters.
    """
    boot_device = partition_properties.get('boot-device')
    return boot_device == 'ftp'


def required_boot_removable_media(partition_properties):
    """
    Indicates whether 'boot_removable_media*' are required input parameters.
    """
    boot_device = partition_properties.get('boot-device')
    return boot_device == 'removable-media'


def required_type_ssc(partition_properties):
    """
    Indicates whether 'ssc_*' are required input parameters.
    """
    part_type = partition_properties.get('type', 'linux')
    return part_type == 'ssc'


# Marker in ZHMC_PARTITION_PROPERTIES.default to indicate special handling
SPECIAL_DEFAULT = 'special_default'

# Dictionary of properties of partition resources, in this format:
#   name: (allowed, create, update, update_while_active, eq_func, type_cast)
# where:
#   name: Name of the property according to the data model, with hyphens
#     replaced by underscores (this is how it is or would be specified in
#     the 'properties' module parameter).
#   allowed: Indicates whether it is allowed in the 'properties' module
#     parameter.
#   create: Indicates whether it can be specified for the "Create Partition"
#     operation.
#   update: Indicates whether it can be specified for the "Update Partition
#     Properties" operation (at all).
#   update_while_active: Indicates whether it can be specified for the "Update
#     Partition Properties" operation while the partition is active. None means
#     "not applicable" (i.e. update=False).
#   eq_func: Equality test function for two values of the property; None means
#     to use Python equality.
#   type_cast: Type cast function for an input value of the property; None
#     means to use it directly. This can be used for example to convert
#     integers provided as strings by Ansible back into integers (that is a
#     current deficiency of Ansible).
#   create_required: Indicates whether the property is required in the HMC
#     create operation. None for artificial properties. Can be a function that
#     returns True or False, if it depends.
#     depends whether the property is required.
#   create_default: Default value for optional create properties. None for
#     required and artificial properties. Can be a function that returns a
#     default value, if it depends.
ZHMC_PARTITION_PROPERTIES = {

    # create-only properties:
    'type': (
        True, True, False, None, None, None,
        False, 'linux'),

    # update-only properties:
    'boot_network_device': (
        # Updated via boot_network_nic_name
        False, False, True, True, None, None,
        False, None),
    'boot_network_nic_name': (
        # Artificial property
        True, False, True, True, None, to_unicode,
        None, None),
    'boot_storage_device': (
        # Updated via boot_storage_hba_name
        False, False, True, True, None, None,
        False, None),
    'boot_storage_hba_name': (
        True, False, True, True, None, to_unicode,
        None, None),  # artificial property
    'boot_storage_volume': (
        # Was added in API version 2.23 (HMC 2.14.0)
        False, False, True, True, None, None,
        False, None),  # via boot_storage_volume_name
    'boot_storage_volume_name': (
        # Artificial property
        True, False, True, True, None, to_unicode,
        None, None),
    'crypto_configuration': (
        # Contains artificial properties, type_cast ignored
        True, False, False, None, None, None,
        False, None),
    'acceptable_status': (
        # TODO: Default value
        True, False, True, True, None, None,
        False, []),
    'processor_management_enabled': (
        True, False, True, True, None, None,
        False, False),
    'ifl_absolute_processor_capping': (
        True, False, True, True, None, None,
        False, False),
    'ifl_absolute_processor_capping_value': (
        True, False, True, True, None, float,
        False, 1.0),
    'ifl_processing_weight_capped': (
        True, False, True, True, None, None,
        False, False),
    'minimum_ifl_processing_weight': (
        True, False, True, True, None, int,
        False, 1),
    'maximum_ifl_processing_weight': (
        True, False, True, True, None, int,
        False, 999),
    'initial_ifl_processing_weight': (
        True, False, True, True, None, int,
        False, 100),
    'cp_absolute_processor_capping': (
        True, False, True, True, None, None,
        False, False),
    'cp_absolute_processor_capping_value': (
        True, False, True, True, None, float,
        False, 1.0),
    'cp_processing_weight_capped': (
        True, False, True, True, None, None,
        False, False),
    'minimum_cp_processing_weight': (
        True, False, True, True, None, int,
        False, 1),
    'maximum_cp_processing_weight': (
        True, False, True, True, None, int,
        False, 999),
    'initial_cp_processing_weight': (
        True, False, True, True, None, int,
        False, 100),
    'boot_logical_unit_number': (
        True, False, True, True, eq_hex, None,
        required_boot_storage_adapter, ''),
    'boot_world_wide_port_name': (
        True, False, True, True, eq_hex, None,
        required_boot_storage_adapter, ''),
    'boot_load_parameters': (
        # Was added in API version 2.23 (HMC 2.14.0)
        True, False, True, True, None, to_unicode,
        False, ''),
    'boot_os_specific_parameters': (
        True, False, True, True, None, to_unicode,
        False, ''),
    'boot_iso_ins_file': (
        True, False, True, True, None, to_unicode,
        False, None),
    'ssc_boot_selection': (
        True, False, True, True, None, None,
        False, 'installer'),

    # create+update properties:
    'name': (
        # Note: Provided in 'name' module parm
        False, True, True, True, None, None,
        True, None),
    'description': (
        True, True, True, True, None, to_unicode,
        False, ''),
    'short_name': (
        True, True, True, False, None, None,
        False, SPECIAL_DEFAULT),
    'partition_id': (
        True, True, True, False, None, None,
        required_partition_id, SPECIAL_DEFAULT),
    'autogenerate_partition_id': (
        True, True, True, False, None, None,
        False, True),
    'ifl_processors': (
        True, True, True, True, None, int,
        required_ifl_processors, 0),
    'cp_processors': (
        True, True, True, True, None, int,
        required_cp_processors, 0),
    'processor_mode': (
        True, True, True, False, None, None,
        False, 'shared'),
    'initial_memory': (
        True, True, True, True, None, int,
        True, None),
    'maximum_memory': (
        True, True, True, False, None, int,
        True, None),
    'reserve_resources': (
        True, True, True, True, None, None,
        False, False),
    'boot_device': (
        True, True, True, True, None, None,
        False, 'none'),
    'boot_timeout': (
        True, True, True, True, None, int,
        False, 60),
    'boot_ftp_host': (
        True, True, True, True, None, to_unicode,
        required_boot_ftp, None),
    'boot_ftp_username': (
        True, True, True, True, None, to_unicode,
        required_boot_ftp, None),
    'boot_ftp_password': (
        True, True, True, True, None, to_unicode,
        required_boot_ftp, None),
    'boot_ftp_insfile': (
        True, True, True, True, None, to_unicode,
        required_boot_ftp, None),
    'boot_removable_media': (
        True, True, True, True, None, to_unicode,
        required_boot_removable_media, None),
    'boot_removable_media_type': (
        True, True, True, True, None, None,
        required_boot_removable_media, None),
    'boot_configuration_selector': (
        True, True, True, True, None, int,
        False, 0),
    'boot_record_lba': (
        True, True, True, True, None, None,
        False, 0),
    'access_global_performance_data': (
        True, True, True, True, None, None,
        False, False),
    'permit_cross_partition_commands': (
        True, True, True, True, None, None,
        False, False),
    'access_basic_counter_set': (
        True, True, True, True, None, None,
        False, False),
    'access_problem_state_counter_set': (
        True, True, True, True, None, None,
        False, False),
    'access_crypto_activity_counter_set': (
        True, True, True, True, None, None,
        False, False),
    'access_extended_counter_set': (
        True, True, True, True, None, None,
        False, False),
    'access_coprocessor_group_set': (
        True, True, True, True, None, None,
        False, False),
    'access_basic_sampling': (
        True, True, True, True, None, None,
        False, False),
    'access_diagnostic_sampling': (
        True, True, True, True, None, None,
        False, False),
    'permit_des_key_import_functions': (
        True, True, True, False, None, None,
        False, True),
    'permit_aes_key_import_functions': (
        True, True, True, False, None, None,
        False, True),
    'permit_ecc_key_import_functions': (
        # Was added in API version 3.2 (HMC 2.15.0)
        True, True, True, True, None, None,
        False, True),
    'ssc_host_name': (
        True, True, True, True, None, to_unicode,
        required_type_ssc, SPECIAL_DEFAULT),
    'ssc_ipv4_gateway': (
        True, True, True, True, None, to_unicode,
        False, None),
    'ssc_ipv6_gateway': (
        # Was added in HMC 2.14.0
        True, True, True, True, None, to_unicode,
        False, None),
    'ssc_dns_servers': (
        True, True, True, True, None, to_unicode,
        False, []),
    'ssc_master_userid': (
        True, True, True, True, None, to_unicode,
        required_type_ssc, SPECIAL_DEFAULT),
    'ssc_master_pw': (
        True, True, True, True, None, to_unicode,
        required_type_ssc, SPECIAL_DEFAULT),
    'secure_boot': (
        # Added in SE/CPC 2.15.0
        True, True, True, True, None, None,
        False, False),

    # read-only properties:
    'object_uri': (
        False, False, False, None, None, None,
        False, SPECIAL_DEFAULT),
    'object_id': (
        False, False, False, None, None, None,
        False, SPECIAL_DEFAULT),
    'parent': (
        False, False, False, None, None, None,
        False, SPECIAL_DEFAULT),
    'class': (
        False, False, False, None, None, None,
        False, 'partition'),
    'status': (
        False, False, False, None, None, None,
        False, 'stopped'),
    'has_unacceptable_status': (
        False, False, False, None, None, None,
        False, False),
    'is_locked': (
        False, False, False, None, None, None,
        False, False),
    'os_name': (
        False, False, False, None, None, None,
        False, ''),
    'os_type': (
        False, False, False, None, None, None,
        False, ''),
    'os_version': (
        False, False, False, None, None, None,
        False, ''),
    'degraded_adapters': (
        False, False, False, None, None, None,
        False, []),
    'current_ifl_processing_weight': (
        False, False, False, None, None, None,
        False, 100),
    'current_cp_processing_weight': (
        False, False, False, None, None, None,
        False, 100),
    'reserved_memory': (
        False, False, False, None, None, None,
        False, SPECIAL_DEFAULT),
    'auto_start': (
        False, False, False, None, None, None,
        False, False),
    'secure_execution': (
        # Added in SE/CPC 2.15.0
        False, False, False, None, None, None,
        False, None),
    'boot_iso_image_name': (
        # Note: Property is updated via mount/unmount operations
        False, False, False, None, None, None,
        False, None),
    'threads_per_processor': (
        False, False, False, None, None, None,
        False, SPECIAL_DEFAULT),
    'virtual_function_uris': (
        False, False, False, None, None, None,
        False, []),
    'nic_uris': (
        False, False, False, None, None, None,
        False, []),
    'hba_uris': (
        False, False, False, None, None, None,
        False, []),
    'storage_group_uris': (
        False, False, False, None, None, None,
        False, []),
    'tape_link_uris': (
        # Was added in API version 3.10 (HMC 2.15.0)
        False, False, False, None, None, None,
        False, []),
    'partition_link_uris': (
        # Was added in API version 4.1 (HMC 2.16.0)
        False, False, False, None, None, None,
        False, []),
    'available_features_list': (
        False, False, False, None, None, None,
        False, []),
}


def process_properties(cpc, partition, params):
    """
    Process the properties specified in the 'properties' module parameter,
    and return two dictionaries (create_props, update_props) that contain
    the properties that can be created, and the properties that can be updated,
    respectively. If the resource exists, the input property values are
    compared with the existing resource property values and the returned set
    of properties is the minimal set of properties that need to be changed.

    - Underscores in the property names are translated into hyphens.
    - The presence of read-only properties, invalid properties (i.e. not
      defined in the data model for partitions), and properties that are not
      allowed because of restrictions or because they are auto-created from
      an artificial property is surfaced by raising ParameterError.
    - The properties resulting from handling artificial properties are
      added to the returned dictionaries.

    Parameters:

      cpc (zhmcclient.Cpc): CPC with the partition to be updated, and
        with the adapters to be used for the partition.

      partition (zhmcclient.Partition): Partition to be updated with the full
        set of current properties, or `None` if it did not previously exist.

      params (dict): Module input parameters.

    Returns:
      tuple of (create_props, update_props, stop, crypto_changes), where:
        * create_props: dict of properties for
          zhmcclient.PartitionManager.create()
        * update_props: dict of properties for
          zhmcclient.Partition.update_properties()
        * stop (bool): Indicates whether some update properties require the
          partition to be stopped when doing the update.
        * crypto_changes (tuple): Changes to the crypto configuration if any
          (or `None` if no changes were specified), as a tuple of:
          * remove_adapters: List of Adapter objects to be removed
          * remove_domain_indexes: List of domain indexes to be removed
          * add_adapters: List of Adapter objects to be added
          * add_domain_configs: List of domain configs to be added (dict of
            'domain-index', 'access-mode')
          * change_domain_configs: List of domain configs for changing the
            access mode of existing domain indexes.

    Raises:
      ParameterError: An issue with the module parameters.
    """
    create_props = {}
    update_props = {}
    stop = False
    crypto_changes = None

    # handle 'name' property
    part_name = to_unicode(params['name'])
    create_props['name'] = part_name
    # We looked up the partition by name, so we will never have to update
    # the partition name

    # handle the other properties
    input_props = params.get('properties', {})
    if input_props is None:
        input_props = {}
    for prop_name in input_props:

        if prop_name not in ZHMC_PARTITION_PROPERTIES:
            raise ParameterError(
                "Property {0!r} is not defined in the data model for "
                "partitions.".format(prop_name))

        allowed, create, update, update_while_active, eq_func, type_cast, \
            required, default = ZHMC_PARTITION_PROPERTIES[prop_name]

        if not allowed:
            raise ParameterError(
                "Property {0!r} is not allowed in the 'properties' module "
                "parameter.".format(prop_name))

        if prop_name == 'boot_storage_hba_name':
            # Process this artificial property

            if not partition:
                raise ParameterError(
                    "Artificial property {0!r} can only be specified when the "
                    "partition previously exists.".format(prop_name))

            if partition.hbas is None:
                raise ParameterError(
                    "Artificial property {0!r} can only be specified when the "
                    "'dpm-storage-management' feature is disabled.".
                    format(prop_name))

            hba_name = input_props[prop_name]
            if type_cast:
                hba_name = type_cast(hba_name)

            try:
                hba = partition.hbas.find(name=hba_name)
            except zhmcclient.NotFound:
                raise ParameterError(
                    "Artificial property {0!r} does not name an existing HBA: "
                    "{1!r}".format(prop_name, hba_name))

            hmc_prop_name = 'boot-storage-device'
            if partition.properties.get(hmc_prop_name) != hba.uri:
                update_props[hmc_prop_name] = hba.uri
                if not update_while_active:
                    raise AssertionError()

        elif prop_name == 'boot_network_nic_name':
            # Process this artificial property

            if not partition:
                raise ParameterError(
                    "Artificial property {0!r} can only be specified when the "
                    "partition previously exists.".format(prop_name))

            nic_name = input_props[prop_name]
            if type_cast:
                nic_name = type_cast(nic_name)

            try:
                nic = partition.nics.find(name=nic_name)
            except zhmcclient.NotFound:
                raise ParameterError(
                    "Artificial property {0!r} does not name an existing NIC: "
                    "{1!r}".format(prop_name, nic_name))

            hmc_prop_name = 'boot-network-device'
            if partition.properties.get(hmc_prop_name) != nic.uri:
                update_props[hmc_prop_name] = nic.uri
                if not update_while_active:
                    raise AssertionError()

        elif prop_name == 'crypto_configuration':
            # Process this artificial property

            crypto_config = input_props[prop_name]

            if not isinstance(crypto_config, dict):
                raise ParameterError(
                    "Artificial property {0!r} is not a dictionary: {1!r}.".
                    format(prop_name, crypto_config))

            if partition:
                hmc_prop_name = 'crypto-configuration'
                current_crypto_config = partition.properties.get(hmc_prop_name)
            else:
                current_crypto_config = None

            # Determine adapter changes
            try:
                adapter_field_name = 'crypto_adapter_names'
                adapter_names = crypto_config[adapter_field_name]
            except KeyError:
                raise ParameterError(
                    "Artificial property {0!r} does not have required field "
                    "{1!r}.".format(prop_name, adapter_field_name))
            adapter_uris = set()
            adapter_dict = {}  # adapters by uri
            if adapter_names is None:
                # Default: Use all crypto adapters of the CPC
                adapters = cpc.adapters.findall(type='crypto')
                for adapter in adapters:
                    adapter_dict[adapter.uri] = adapter
                    adapter_uris.add(adapter.uri)
            else:
                for adapter_name in adapter_names:
                    try:
                        adapter = cpc.adapters.find(name=adapter_name,
                                                    type='crypto')
                    except zhmcclient.NotFound:
                        raise ParameterError(
                            "Artificial property {0!r} does not specify the "
                            "name of an existing crypto adapter in its {1!r} "
                            "field: {2!r}".
                            format(prop_name, adapter_field_name,
                                   adapter_name))
                    adapter_dict[adapter.uri] = adapter
                    adapter_uris.add(adapter.uri)
            if current_crypto_config:
                current_adapter_uris = set(
                    current_crypto_config['crypto-adapter-uris'])
            else:
                current_adapter_uris = set()
            if adapter_uris != current_adapter_uris:
                add_adapter_uris = adapter_uris - current_adapter_uris
                # Result: List of adapters to be added:
                add_adapters = [adapter_dict[uri] for uri in add_adapter_uris]
                remove_adapter_uris = current_adapter_uris - adapter_uris
                for uri in remove_adapter_uris:
                    adapter = cpc.adapters.find(**{'object-uri': uri})
                    # We assume the current crypto config lists only valid URIs
                    adapter_dict[adapter.uri] = adapter
                # Result: List of adapters to be removed:
                remove_adapters = \
                    [adapter_dict[uri] for uri in remove_adapter_uris]
            else:
                # Result: List of adapters to be added:
                add_adapters = []
                # Result: List of adapters to be removed:
                remove_adapters = []

            # Determine domain config changes.
            try:
                config_field_name = 'crypto_domain_configurations'
                domain_configs = crypto_config[config_field_name]
            except KeyError:
                raise ParameterError(
                    "Artificial property {0!r} does not have required field "
                    "{1!r}.".format(prop_name, config_field_name))
            di_field_name = 'domain_index'
            am_field_name = 'access_mode'
            domain_indexes = set()
            for dc in domain_configs:
                try:
                    # Convert to integer in case the domain index is provided
                    # as a string:
                    domain_index = int(dc[di_field_name])
                except KeyError:
                    raise ParameterError(
                        "Artificial property {0!r} does not have required "
                        "sub-field {1!r} in one of its {2!r} fields.".
                        format(prop_name, di_field_name, config_field_name))
                domain_indexes.add(domain_index)
            current_access_mode_dict = {}  # dict: acc.mode by dom.index
            if current_crypto_config:
                current_domain_configs = \
                    current_crypto_config['crypto-domain-configurations']
                di_prop_name = 'domain-index'
                am_prop_name = 'access-mode'
                for dc in current_domain_configs:
                    # Here the domain index is always an integer because it is
                    # returned from the HMC that way, so no type cast needed.
                    current_access_mode_dict[dc[di_prop_name]] = \
                        dc[am_prop_name]
            current_domain_indexes = \
                set(current_access_mode_dict)
            # Result: List of domain indexes to be removed:
            remove_domain_indexes = \
                list(current_domain_indexes - domain_indexes)
            # Building result: List of domain configs to be added:
            add_domain_configs = []
            # Building result: List of domain configs to be changed:
            change_domain_configs = []
            for dc in domain_configs:
                # Convert to integer in case the domain index is provided
                # as a string:
                domain_index = int(dc[di_field_name])
                try:
                    access_mode = dc[am_field_name]
                except KeyError:
                    raise ParameterError(
                        "Artificial property {0!r} does not have required "
                        "sub-field {1!r} in one of its {2!r} fields.".
                        format(prop_name, am_field_name, config_field_name))
                hmc_domain_config = {
                    'domain-index': domain_index,
                    'access-mode': access_mode,
                }
                if domain_index not in current_access_mode_dict:
                    # Domain is not included yet
                    add_domain_configs.append(hmc_domain_config)
                elif access_mode != current_access_mode_dict[domain_index]:
                    # Domain is included but access mode needs to be changed
                    change_domain_configs.append(hmc_domain_config)

            crypto_changes = (remove_adapters, remove_domain_indexes,
                              add_adapters, add_domain_configs,
                              change_domain_configs)

        else:
            # Process a normal (= non-artificial) property
            if prop_name == 'ssc_ipv4_gateway':
                # Undo conversion from None to empty string in Ansible
                if input_props[prop_name] == '':
                    input_props[prop_name] = None
            _create_props, _update_props, _stop = process_normal_property(
                prop_name, ZHMC_PARTITION_PROPERTIES, input_props, partition)
            create_props.update(_create_props)
            update_props.update(_update_props)
            if _stop:
                stop = True

    return create_props, update_props, stop, crypto_changes


def get_crypto_config(partition):
    """
    Return the value of the 'crypto-configuration' property of the Partition
    object, and if not set return it initialized and empty.
    """
    ret_crypto_config = {
        'crypto-adapter-uris': [],
        'crypto-domain-configurations': [],
    }
    crypto_config = partition.properties.get('crypto-configuration')
    if crypto_config:
        ret_crypto_config['crypto-adapter-uris'].extend(
            crypto_config.get('crypto-adapter-uris', []))
        ret_crypto_config['crypto-domain-configurations'].extend(
            crypto_config.get('crypto-domain-configurations', []))
    return ret_crypto_config


def change_crypto_config(partition, crypto_changes, check_mode):
    """
    Change the crypto configuration of the partition as specified.

    Returns whether the crypto configuration has or would have changed.
    """

    remove_adapters, remove_domain_indexes, \
        add_adapters, add_domain_configs, \
        change_domain_configs = crypto_changes

    changed = False

    # We process additions first, in order to avoid
    # HTTPError 409,111 (At least one 'usage' required).
    if add_adapters or add_domain_configs:
        if not check_mode:
            partition.increase_crypto_config(add_adapters,
                                             add_domain_configs)
        else:
            crypto_config = get_crypto_config(partition)
            adapter_uris = crypto_config['crypto-adapter-uris']
            domain_configs = crypto_config['crypto-domain-configurations']
            for _ad in add_adapters:
                if _ad.uri not in adapter_uris:
                    adapter_uris.append(_ad.uri)
            for dc in add_domain_configs:
                if dc not in domain_configs:
                    domain_configs.append(dc)
            partition.update_properties_local(
                {'crypto-configuration': crypto_config})
        changed = True

    if change_domain_configs:
        # We process changes that set access mode 'control-usage' first,
        # in order to avoid HTTPError 409,111 (At least one 'usage' required).
        for domain_config in sorted(change_domain_configs,
                                    key=itemgetter('access-mode'),
                                    reverse=True):
            domain_index = domain_config['domain-index']
            access_mode = domain_config['access-mode']
            if not check_mode:
                partition.change_crypto_domain_config(domain_index,
                                                      access_mode)
            else:
                crypto_config = get_crypto_config(partition)
                domain_configs = crypto_config['crypto-domain-configurations']
                for dc in domain_configs:
                    if dc['domain-index'] == domain_index:
                        dc['access-mode'] = access_mode
                partition.update_properties_local(
                    {'crypto-configuration': crypto_config})
        changed = True

    if remove_adapters or remove_domain_indexes:
        if not check_mode:
            partition.decrease_crypto_config(remove_adapters,
                                             remove_domain_indexes)
        else:
            crypto_config = get_crypto_config(partition)
            adapter_uris = crypto_config['crypto-adapter-uris']
            domain_configs = crypto_config['crypto-domain-configurations']
            for _ad in remove_adapters:
                if _ad.uri in adapter_uris:
                    adapter_uris.remove(_ad.uri)
            for remove_di in remove_domain_indexes:
                for i, dc in enumerate(domain_configs):
                    if dc['domain-index'] == remove_di:
                        del domain_configs[i]
            partition.update_properties_local(
                {'crypto-configuration': crypto_config})
        changed = True

    return changed


def add_artificial_properties(
        partition_properties, partition, expand_storage_groups,
        expand_crypto_adapters):
    """
    Add artificial properties to the partition_properties dict.

    Upon return, the partition_properties dict has been extended by these
    artificial properties:

    * 'hbas': List of Hba objects of the partition.

    * 'nics': List of Nic objects of the partition, with their properties
      and these artificial properties:

        * 'adapter-name'
        * 'adapter-port'
        * 'adapter-id'

    * 'virtual-functions': List of VirtualFunction objects of the partition.

    * 'boot-storage-volume-name': Name of the object with the URI specified
      in 'boot-storage-volume', or None.

    and if expand_storage_groups is True:

    * 'storage-groups': List of StorageGroup objects representing the
      storage groups attached to the partition, with their properties
      and these artificial properties:

        * 'candidate-adapter-ports': List of Port objects representing the
          candidate adapter ports of the storage group, with their properties
          and these artificial properties:

            - 'parent-adapter': Adapter object of the port.

        * 'storage-volumes': List of StorageVolume objects of the storage
          group.

        * 'virtual-storage-resources': List of VirtualStorageResource objects
          of the storage group.

    and if expand_crypto_adapters is True:

    * 'crypto-adapters' in 'crypto-configuration': List of Adapter objects
      representing the crypto adapters assigned to the partition.
    """
    cpc = partition.manager.cpc
    console = cpc.manager.console
    session = cpc.manager.client.session

    # Get the HBA child elements of the partition
    hbas_prop = []
    if partition.hbas is not None:
        for hba in partition.hbas.list(full_properties=True):
            hbas_prop.append(dict(hba.properties))
    partition_properties['hbas'] = hbas_prop

    # Get the NIC child elements of the partition
    nics_prop = []
    for nic in partition.nics.list(full_properties=True):
        nic_props = OrderedDict()
        nic_props.update(nic.properties)
        # Add artificial properties adapter-name/-port/-id:
        vswitch_uri = nic.prop("virtual-switch-uri", None)
        if vswitch_uri:
            # OSA, Hipersockets
            vswitch = cpc.virtual_switches.find(**{'object-uri': vswitch_uri})
            adapter_uri = vswitch.get_property('backing-adapter-uri')
            adapter_port = vswitch.get_property('port')
            adapter = cpc.adapters.find(**{'object-uri': adapter_uri})
            nic_props['adapter-name'] = adapter.name
            nic_props['adapter-port'] = adapter_port
            nic_props['adapter-id'] = adapter.get_property('adapter-id')
        else:
            # RoCE, CNA
            port_uri = nic.prop("network-adapter-port-uri", None)
            port_props = session.get(port_uri)
            adapter_uri = port_props['parent']
            adapter = cpc.adapters.find(**{'object-uri': adapter_uri})
            nic_props['adapter-name'] = adapter.name
            nic_props['adapter-port'] = port_props['index']
            nic_props['adapter-id'] = adapter.get_property('adapter-id')
        nics_prop.append(nic_props)
    partition_properties['nics'] = nics_prop

    # Get the VF child elements of the partition
    vfs_prop = []
    for vf in partition.virtual_functions.list(full_properties=True):
        vfs_prop.append(dict(vf.properties))
    partition_properties['virtual-functions'] = vfs_prop

    # Set 'boot-storage-volume-name'
    bsv_uri = partition.prop('boot-storage-volume', None)
    if bsv_uri:
        sg_uri = bsv_uri.split('/storage-volumes/')[0]
        storage_group = console.storage_groups.resource_object(sg_uri)
        bsv = storage_group.storage_volumes.find(**{'element-uri': bsv_uri})
        bsv_name = bsv.name
    else:
        bsv_name = None
    partition_properties['boot-storage-volume-name'] = bsv_name

    if expand_storage_groups:
        sgs_prop = []
        for sg_uri in partition.properties['storage-group-uris']:
            storage_group = console.storage_groups.resource_object(sg_uri)
            storage_group.pull_full_properties()
            sg_properties = dict(storage_group.properties)

            # Candidate adapter ports and their adapters (full set of props)
            caps_prop = []
            for cap in storage_group.list_candidate_adapter_ports(
                    full_properties=True):
                cap_properties = dict(cap.properties)
                adapter = cap.manager.adapter
                adapter.pull_full_properties()
                cap_properties['parent-adapter'] = dict(adapter.properties)
                caps_prop.append(cap_properties)
            sg_properties['candidate-adapter-ports'] = caps_prop

            # Storage volumes (full set of properties).
            # Note: We create the storage volumes from the
            # 'storage-volume-uris' property, because the 'List Storage
            # Volumes of a Storage Group' operation returns an empty list for
            # auto-discovered volumes.
            svs_prop = []
            sv_uris = storage_group.get_property('storage-volume-uris')
            for sv_uri in sv_uris:
                sv = storage_group.storage_volumes.resource_object(sv_uri)
                sv.pull_full_properties()
                svs_prop.append(dict(sv.properties))
            sg_properties['storage-volumes'] = svs_prop

            # Virtual storage resources (full set of properties).
            vsrs_prop = []
            vsr_uris = storage_group.get_property(
                'virtual-storage-resource-uris')
            for vsr_uri in vsr_uris:
                vsr = storage_group.virtual_storage_resources.resource_object(
                    vsr_uri)
                vsr.pull_full_properties()
                vsrs_prop.append(dict(vsr.properties))
            sg_properties['virtual-storage-resources'] = vsrs_prop

            sgs_prop.append(sg_properties)

        partition_properties['storage-groups'] = sgs_prop

    if expand_crypto_adapters:

        cc = partition_properties['crypto-configuration']
        if cc:
            # partition_properties is only a shallow copy of the
            # Partition.properties dict, so cc is still a dict within the
            # original Partition.properties dict. Therefore, we copy cc
            # since we modify it.
            cc = cc.copy()
            cas_prop = []
            for ca_uri in cc['crypto-adapter-uris']:
                ca = cpc.adapters.resource_object(ca_uri)
                ca.pull_full_properties()
                cas_prop.append(dict(ca.properties))
            cc['crypto-adapters'] = cas_prop
            partition_properties['crypto-configuration'] = cc


def create_check_mode_partition(cpc, create_props, update_props):
    """
    Create and return a fake local Partition object.

    This is used when a partition needs to be created in check mode.

    This function must be consistent with the behavior of the "Create Partition"
    operation on the HMC. HTTP errors the HMC would return are indicated by
    raising zhmcclient.HTTPError.
    """

    input_props = {}
    input_props.update(create_props)
    input_props.update(update_props)

    missing_props = []

    # Handle direct requiredness, direct defaults specified in prop defs
    for prop_name in ZHMC_PARTITION_PROPERTIES:
        prop_hmc_name = prop_name.replace('_', '-')
        prop_defs = ZHMC_PARTITION_PROPERTIES[prop_name]
        required = prop_defs[6]
        default = prop_defs[7]

        if not isinstance(required, types.FunctionType) and required and \
                prop_hmc_name not in input_props:
            missing_props.append(prop_name)

        if default != SPECIAL_DEFAULT:
            input_props.setdefault(prop_hmc_name, default)

    if missing_props:
        raise ParameterError(
            "Required partition properties missing in module input: {p}".
            format(p=', '.join(missing_props)))

    # Handle SPECIAL_DEFAULT properties
    oid = '{0}'.format(uuid.uuid4())
    uri = '/api/partitions/{0}'.format(oid)
    input_props['object-id'] = oid
    input_props['object-uri'] = uri
    input_props['parent'] = cpc.uri
    input_props['reserved-memory'] = \
        input_props['maximum-memory'] - input_props['initial-memory']

    # Note: If the partition has never been activated, 0 is returned. After the
    # initial activation, the value is controlled by the SMT setting in the OS.
    # Since we cannot simulate an OS in check mode, we always return 0.
    input_props['threads-per-processor'] = 0

    # TODO: Use a default for 'partition-id' that is guaranteed unique in CPC
    if input_props['autogenerate-partition-id']:
        input_props['partition-id'] = 'FF'

    # TODO: Use a default for 'short-name' that is guaranteed unique in CPC
    name = input_props['name']
    input_props['short-name'] = \
        '{0}{1:04X}'.format(name, random.randint(0, 16 ^ 4))

    # Handle function-based requiredness specified in prop defs
    for prop_name in ZHMC_PARTITION_PROPERTIES:
        prop_hmc_name = prop_name.replace('_', '-')
        prop_defs = ZHMC_PARTITION_PROPERTIES[prop_name]
        required = prop_defs[6]

        if isinstance(required, types.FunctionType):
            required_ = required(input_props)
            if required_ and prop_hmc_name not in input_props:
                missing_props.append(prop_name)

    if required_boot_ftp(input_props):
        if input_props['boot-ftp-host'] is None:
            missing_props.append('boot_ftp_host')
        if input_props['boot-ftp-username'] is None:
            missing_props.append('boot_ftp_username')
        if input_props['boot-ftp-password'] is None:
            missing_props.append('boot_ftp_password')
        if input_props['boot-ftp-insfile'] is None:
            missing_props.append('boot_ftp_insfile')

    if required_boot_removable_media(input_props):
        if input_props['boot-removable-media'] is None:
            missing_props.append('boot_removable_media')
        if input_props['boot-removable-media-type'] is None:
            missing_props.append('boot_removable_media_type')

    if required_boot_storage_adapter(input_props):
        if input_props['boot-logical-unit-number'] == '':
            missing_props.append('boot_logical_unit_number')
        if input_props['boot-world-wide-port-name'] == '':
            missing_props.append('boot_world_wide_port_name')

    if missing_props:
        raise ParameterError(
            "Required partition properties missing in module input: {p}".
            format(p=', '.join(missing_props)))

    partition = cpc.partitions.resource_object(oid, props=input_props)

    return partition


def ensure_active(params, check_mode):
    """
    Ensure that the partition exists, is active or degraded, and has the
    specified properties.

    Raises:
      ParameterError: An issue with the module parameters.
      StatusError: An issue with the partition status.
      zhmcclient.Error: Any zhmcclient exception can happen.
    """

    host = params['hmc_host']
    userid, password, ca_certs, verify = get_hmc_auth(params['hmc_auth'])
    cpc_name = params['cpc_name']
    partition_name = params['name']
    expand_storage_groups = params['expand_storage_groups']
    expand_crypto_adapters = params['expand_crypto_adapters']
    _faked_session = params.get('_faked_session', None)

    changed = False
    result = {}

    session = get_session(
        _faked_session, host, userid, password, ca_certs, verify)
    try:
        client = zhmcclient.Client(session)
        cpc = client.cpcs.find(name=cpc_name)
        # The default exception handling is sufficient for the above.

        try:
            partition = cpc.partitions.find(name=partition_name)
            partition.pull_full_properties()
        except zhmcclient.NotFound:
            partition = None

        if not partition:
            # It does not exist. Create it and update it if there are
            # update-only properties.
            create_props, update_props, stop, crypto_changes = \
                process_properties(cpc, partition, params)
            update2_props = {}
            for name, value in update_props.items():
                if name not in create_props:
                    update2_props[name] = value
            if not check_mode:
                partition = cpc.partitions.create(create_props)
                if update2_props:
                    partition.update_properties(update2_props)
                if crypto_changes:
                    change_crypto_config(partition, crypto_changes, check_mode)
                # Properties are refreshed further down
            else:
                # Create a Partition object locally
                partition = create_check_mode_partition(
                    cpc, create_props, update2_props)
            changed = True
        else:
            # It exists. Stop if needed due to property update requirements,
            # or wait for an updateable partition status, and update its
            # properties.
            create_props, update_props, stop, crypto_changes = \
                process_properties(cpc, partition, params)
            # Note: create_props in this case only contains 'name' and can be
            # ignored.
            if update_props:
                if not check_mode:
                    if stop:
                        stop_partition(partition, check_mode)
                    else:
                        wait_for_transition_completion(partition)
                    partition.update_properties(update_props)
                    # Properties are refreshed further down
                else:
                    # Update the local object's properties
                    partition.update_properties_local(update_props)
                changed = True
            if crypto_changes:
                changed |= change_crypto_config(partition, crypto_changes,
                                                check_mode)

        if not partition:
            raise AssertionError()

        changed |= start_partition(partition, check_mode)

        if not check_mode:

            # Properties are refreshed only when not in check mode, because
            # in check mode we have local (client-side) changes that are not
            # in the HMC.
            partition.pull_full_properties()

            status = partition.get_property('status')
            if status not in ('active', 'degraded'):
                raise StatusError(
                    "Could not get partition {0!r} into an active state, "
                    "status is: {1!r}".format(partition.name, status))

        result = dict(partition.properties)
        add_artificial_properties(
            result, partition, expand_storage_groups, expand_crypto_adapters)

        return changed, result

    finally:
        session.logoff()


def ensure_stopped(params, check_mode):
    """
    Ensure that the partition exists, is stopped, and has the specified
    properties.

    Raises:
      ParameterError: An issue with the module parameters.
      StatusError: An issue with the partition status.
      zhmcclient.Error: Any zhmcclient exception can happen.
    """

    host = params['hmc_host']
    userid, password, ca_certs, verify = get_hmc_auth(params['hmc_auth'])
    cpc_name = params['cpc_name']
    partition_name = params['name']
    expand_storage_groups = params['expand_storage_groups']
    expand_crypto_adapters = params['expand_crypto_adapters']
    _faked_session = params.get('_faked_session', None)

    changed = False
    result = {}

    session = get_session(
        _faked_session, host, userid, password, ca_certs, verify)
    try:
        client = zhmcclient.Client(session)
        cpc = client.cpcs.find(name=cpc_name)
        # The default exception handling is sufficient for the above.

        try:
            partition = cpc.partitions.find(name=partition_name)
            partition.pull_full_properties()
        except zhmcclient.NotFound:
            partition = None

        if not partition:
            # It does not exist. Create it and update it if there are
            # update-only properties.
            create_props, update_props, stop, crypto_changes = \
                process_properties(cpc, partition, params)
            update2_props = {}
            for name, value in update_props.items():
                if name not in create_props:
                    update2_props[name] = value
            if not check_mode:
                partition = cpc.partitions.create(create_props)
                if update2_props:
                    partition.update_properties(update2_props)
                # Properties are refreshed further down
            else:
                # Create a Partition object locally
                partition = create_check_mode_partition(
                    cpc, create_props, update2_props)
            changed = True
            if crypto_changes:
                change_crypto_config(partition, crypto_changes, check_mode)
        else:
            # It exists. Stop it and update its properties.
            create_props, update_props, stop, crypto_changes = \
                process_properties(cpc, partition, params)
            # Note: create_props in this case only contains 'name' and can be
            # ignored.
            changed |= stop_partition(partition, check_mode)
            if update_props:
                if not check_mode:
                    partition.update_properties(update_props)
                    # Properties are refreshed further down
                else:
                    # Update the local object's properties
                    partition.update_properties_local(update_props)
                changed = True
            if crypto_changes:
                changed |= change_crypto_config(partition, crypto_changes,
                                                check_mode)

        if not partition:
            raise AssertionError()

        if not check_mode:
            # Properties are refreshed only when not in check mode, because
            # in check mode we have local (client-side) changes that are not
            # in the HMC.
            partition.pull_full_properties()

            status = partition.get_property('status')
            if status not in ('stopped'):
                raise StatusError(
                    "Could not get partition {0!r} into a stopped state, "
                    "status is: {1!r}".format(partition.name, status))

        result = dict(partition.properties)
        add_artificial_properties(
            result, partition, expand_storage_groups, expand_crypto_adapters)

        return changed, result

    finally:
        session.logoff()


def ensure_absent(params, check_mode):
    """
    Ensure that the partition does not exist.

    Raises:
      ParameterError: An issue with the module parameters.
      StatusError: An issue with the partition status.
      zhmcclient.Error: Any zhmcclient exception can happen.
    """

    host = params['hmc_host']
    userid, password, ca_certs, verify = get_hmc_auth(params['hmc_auth'])
    cpc_name = params['cpc_name']
    partition_name = params['name']
    _faked_session = params.get('_faked_session', None)

    changed = False
    result = {}

    session = get_session(
        _faked_session, host, userid, password, ca_certs, verify)
    try:
        client = zhmcclient.Client(session)
        cpc = client.cpcs.find(name=cpc_name)
        # The default exception handling is sufficient for the above.

        try:
            partition = cpc.partitions.find(name=partition_name)
        except zhmcclient.NotFound:
            return changed, result

        if not check_mode:
            stop_partition(partition, check_mode)
            partition.delete()
        changed = True

        return changed, result

    finally:
        session.logoff()


def facts(params, check_mode):
    """
    Return partition facts.

    Raises:
      ParameterError: An issue with the module parameters.
      zhmcclient.Error: Any zhmcclient exception can happen.
    """

    host = params['hmc_host']
    userid, password, ca_certs, verify = get_hmc_auth(params['hmc_auth'])
    cpc_name = params['cpc_name']
    partition_name = params['name']
    expand_storage_groups = params['expand_storage_groups']
    expand_crypto_adapters = params['expand_crypto_adapters']
    _faked_session = params.get('_faked_session', None)

    changed = False
    result = {}

    session = get_session(
        _faked_session, host, userid, password, ca_certs, verify)
    try:
        # The default exception handling is sufficient for this code
        client = zhmcclient.Client(session)
        cpc = client.cpcs.find(name=cpc_name)

        partition = cpc.partitions.find(name=partition_name)
        partition.pull_full_properties()

        result = dict(partition.properties)
        add_artificial_properties(
            result, partition, expand_storage_groups, expand_crypto_adapters)

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
      StatusError: An issue with the partition status.
      zhmcclient.Error: Any zhmcclient exception can happen.
    """
    actions = {
        "absent": ensure_absent,
        "active": ensure_active,
        "stopped": ensure_stopped,
        "facts": facts,
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
        state=dict(required=True, type='str',
                   choices=['absent', 'stopped', 'active', 'facts']),
        properties=dict(required=False, type='dict', default={}),
        expand_storage_groups=dict(required=False, type='bool', default=False),
        expand_crypto_adapters=dict(required=False, type='bool',
                                    default=False),
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
    module.exit_json(changed=changed, partition=result)


if __name__ == '__main__':
    main()
