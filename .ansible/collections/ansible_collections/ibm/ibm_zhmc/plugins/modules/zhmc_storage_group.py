#!/usr/bin/python
# Copyright 2018-2020 IBM Corp. All Rights Reserved.
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
module: zhmc_storage_group
version_added: "2.9.0"
short_description: Create storage groups
description:
  - Gather facts about a storage group associated with a CPC (Z system),
    including its storage volumes and virtual storage resources.
  - Create, delete, or update a storage group associated with a CPC.
notes:
  - This module manages only the knowledge of the Z system about its storage,
    but does not perform any actions against the storage subsystems or
    SAN switches attached to the Z system.
  - Attachment of storage groups to and from partitions is managed by the
    Ansible module zhmc_storage_group_attachment.
author:
  - Andreas Maier (@andy-maier)
  - Andreas Scheuring (@scheuran)
  - Juergen Leopold (@leopoldjuergen)
requirements:
  - Access to the WS API of the HMC of the targeted Z system
    (see :term:`HMC API`).
  - The targeted Z system must be of generation z14 or later (to have the
    "dpm-storage-management" firmware feature) and must be in the Dynamic
    Partition Manager (DPM) operational mode.
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
      - The name of the CPC associated with the target storage group.
    type: str
    required: true
  name:
    description:
      - The name of the target storage group.
    type: str
    required: true
  state:
    description:
      - "The desired state for the storage group. All states are fully
         idempotent within the limits of the properties that can be changed,
         unless otherwise stated:"
      - "* C(absent): Ensures that the storage group does not exist. If the
         storage group is currently attached to any partitions, the module will
         fail (this is an idempotency limitation)."
      - "* C(present): Ensures that the storage group exists and is associated
         with the specified CPC, and has the specified properties. The
         attachment state of an already existing storage group to a partition
         is not changed."
      - "* C(facts): Returns the storage group properties."
    type: str
    required: true
    choices: ['absent', 'present', 'facts']
  properties:
    description:
      - "Dictionary with desired properties for the storage group.
         Used for C(state=present); ignored for C(state=absent|facts).
         Dictionary key is the property name with underscores instead
         of hyphens, and dictionary value is the property value in YAML syntax.
         Integer properties may also be provided as decimal strings."
      - "The possible input properties in this dictionary are the properties
         defined as writeable in the data model for Storage Group resources
         (where the property names contain underscores instead of hyphens),
         with the following exceptions:"
      - "* C(name): Cannot be specified because the name has already been
         specified in the C(name) module parameter."
      - "* C(type): Cannot be changed once the storage group exists."
      - "Properties omitted in this dictionary will remain unchanged when the
         storage group already exists, and will get the default value defined
         in the data model for storage groups in the :term:`HMC API` when the
         storage group is being created."
    type: dict
    required: false
    default: null
  expand:
    description:
      - "Boolean that controls whether the returned storage group contains
         additional artificial properties that expand certain URI or name
         properties to the full set of resource properties (see description of
         return values of this module)."
    type: bool
    required: false
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

- name: Gather facts about a storage group
  zhmc_storage_group:
    hmc_host: "{{ my_hmc_host }}"
    hmc_auth: "{{ my_hmc_auth }}"
    cpc_name: "{{ my_cpc_name }}"
    name: "{{ my_storage_group_name }}"
    state: facts
    expand: true
  register: sg1

- name: Ensure the storage group does not exist
  zhmc_storage_group:
    hmc_host: "{{ my_hmc_host }}"
    hmc_auth: "{{ my_hmc_auth }}"
    cpc_name: "{{ my_cpc_name }}"
    name: "{{ my_storage_group_name }}"
    state: absent

- name: Ensure the storage group exists
  zhmc_storage_group:
    hmc_host: "{{ my_hmc_host }}"
    hmc_auth: "{{ my_hmc_auth }}"
    cpc_name: "{{ my_cpc_name }}"
    name: "{{ my_storage_group_name }}"
    state: present
    expand: true
    properties:
      description: "Example storage group 1"
      type: fcp
      shared: false
      connectivity: 4
      max-partitions: 1
  register: sg1

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
storage_group:
  description:
    - "For C(state=absent), an empty dictionary."
    - "For C(state=present|facts), the resource properties of the target
       storage group after any changes, plus additional artificial properties
       as described below."
  returned: success
  type: dict
  contains:
    name:
      description: "Storage group name"
      type: str
    "{property}":
      description: "Additional properties of the storage group, as described
        in the data model of the 'Storage Group' object in the
        :term:`HMC API` book.
        The property names have hyphens (-) as described in that book."
    attached-partition-names:
      description: "Names of the partitions to which the storage group is
        attached."
      type: list
      elements: str
    candidate-adapter-ports:
      description: "Only present if C(expand=true): List of candidate storage
        adapter ports of the storage group."
      returned: "success+expand"
      type: list
      elements: dict
      contains:
        name:
          description: "Storage port name"
          type: str
        index:
          description: "Storage port index"
          type: int
        "{property}":
          description: "Additional properties of the storage port, as described
            in the data model of the 'Storage Port' element object of the
            'Adapter' object in the :term:`HMC API` book.
            The property names have hyphens (-) as described in that book."
        parent-adapter:
          description: "Storage adapter of the candidate port."
          type: dict
          contains:
            name:
              description: "Storage adapter name"
              type: str
            "{property}":
              description: "Additional properties of the storage adapter, as
                described in the data model of the 'Adapter' object in the
                :term:`HMC API` book.
                The property names have hyphens (-) as described in that book."
    storage-volumes:
      description: "Only present if C(expand=true): Storage volumes of the
        storage group."
      returned: "success+expand"
      type: list
      elements: dict
      contains:
        name:
          description: "Storage volume name"
          type: str
        "{property}":
          description: "Additional properties of the storage volume, as
            described in the data model of the 'Storage Volume' element object
            of the 'Storage Group' object in the :term:`HMC API` book.
            The property names have hyphens (-) as described in that book."
    virtual-storage-resources:
      description: "Only present if C(expand=true): Virtual storage resources
        of the storage group."
      returned: "success+expand"
      type: list
      elements: dict
      contains:
        "{property}":
          description: "Properties of the virtual storage resource, as
            described in the data model of the 'Virtual Storage Resource'
            element object of the 'Storage Group' object in the
            :term:`HMC API` book.
            The property names have hyphens (-) as described in that book."
    attached-partitions:
      description: "Only present if C(expand=true): Partitions to which the
        storage group is attached."
      returned: "success+expand"
      type: list
      elements: dict
      contains:
        "{property}":
          description: "Properties of the partition, as described in the data
            model of the 'Partition' object in the :term:`HMC API` book.
            The property names have hyphens (-) as described in that book."
  sample:
    {
        "active-connectivity": 6,
        "active-max-partitions": 1,
        "attached-partition-names": [
            "MGMT1"
        ],
        "attached-partitions": [
            {
                "acceptable-status": [
                    "active"
                ],
                "access-basic-counter-set": false,
                "access-basic-sampling": false,
                "access-coprocessor-group-set": false,
                "access-crypto-activity-counter-set": false,
                "access-diagnostic-sampling": false,
                "access-extended-counter-set": false,
                "access-global-performance-data": false,
                "access-problem-state-counter-set": false,
                "auto-start": false,
                "autogenerate-partition-id": true,
                "available-features-list": [
                    {
                        "description": "The DPM storage management approach in
                          which FCP and FICON storage resources are defined in
                          Storage Groups, which are attached to Partitions.",
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
                "crypto-configuration": { },
                "current-cp-processing-weight": 1,
                "current-ifl-processing-weight": 1,
                "degraded-adapters": [],
                "description": "Colo dev partition",
                "has-unacceptable-status": false,
                "hba-uris": [],
                "ifl-absolute-processor-capping": false,
                "ifl-absolute-processor-capping-value": 1.0,
                "ifl-processing-weight-capped": false,
                "ifl-processors": 4,
                "initial-cp-processing-weight": 100,
                "initial-ifl-processing-weight": 100,
                "initial-memory": 68608,
                "ipl-load-parameter": "",
                "is-locked": false,
                "maximum-cp-processing-weight": 999,
                "maximum-ifl-processing-weight": 999,
                "maximum-memory": 68608,
                "minimum-cp-processing-weight": 1,
                "minimum-ifl-processing-weight": 1,
                "name": "MGMT1",
                "nic-uris": [ ],
                "object-id": "009c0f4c-3588-11e9-bad3-00106f239d19",
                "object-uri": "/api/partitions/009c0f4c-3588-11e9-bad3-00106f239d19",
                "os-name": "SSC",
                "os-type": "SSC",
                "os-version": "3.13.0",
                "parent": "/api/cpcs/66942455-4a14-3f99-8904-3e7ed5ca28d7",
                "partition-id": "00",
                "permit-aes-key-import-functions": true,
                "permit-cross-partition-commands": false,
                "permit-des-key-import-functions": true,
                "processor-management-enabled": false,
                "processor-mode": "shared",
                "reserve-resources": false,
                "reserved-memory": 0,
                "short-name": "MGMT1",
                "ssc-boot-selection": "appliance",
                "ssc-dns-servers": [
                    "8.8.8.8"
                ],
                "ssc-host-name": "cpca-mgmt1",
                "ssc-ipv4-gateway": "172.16.192.1",
                "ssc-ipv6-gateway": null,
                "ssc-master-userid": "hmREST",
                "status": "active",
                "storage-group-uris": [
                    "/api/storage-groups/edd782f2-200a-11e9-a142-00106f239c31"
                ],
                "threads-per-processor": 2,
                "type": "ssc",
                "virtual-function-uris": []
            }
        ],
        "candidate-adapter-port-uris": [
            "/api/adapters/e03d413a-e578-11e8-a87c-00106f239c31/storage-ports/0"
        ],
        "candidate-adapter-ports": [
            {
                "class": "storage-port",
                "description": "",
                "element-id": "0",
                "element-uri": "/api/adapters/e03d413a-e578-11e8-a87c-00106f239c31/storage-ports/0",
                "fabric-id": "100088947155A1E9",
                "index": 0,
                "name": "Port 0",
                "parent": "/api/adapters/e03d413a-e578-11e8-a87c-00106f239c31",
                "parent-adapter": {
                    "adapter-family": "ficon",
                    "adapter-id": "124",
                    "allowed-capacity": 64,
                    "card-location": "A14B-D113-J.01",
                    "channel-path-id": "08",
                    "class": "adapter",
                    "configured-capacity": 14,
                    "description": "",
                    "detected-card-type": "ficon-express-16s-plus",
                    "maximum-total-capacity": 254,
                    "name": "FCP_124_SAN1_03",
                    "object-id": "e03d413a-e578-11e8-a87c-00106f239c31",
                    "object-uri": "/api/adapters/e03d413a-e578-11e8-a87c-00106f239c31",
                    "parent": "/api/cpcs/66942455-4a14-3f99-8904-3e7ed5ca28d7",
                    "physical-channel-status": "operating",
                    "port-count": 1,
                    "state": "online",
                    "status": "active",
                    "storage-port-uris": [
                        "/api/adapters/e03d413a-e578-11e8-a87c-00106f239c31/storage-ports/0"
                    ],
                    "type": "fcp",
                    "used-capacity": 18
                }
            }
        ],
        "class": "storage-group",
        "connectivity": 6,
        "cpc-uri": "/api/cpcs/66942455-4a14-3f99-8904-3e7ed5ca28d7",
        "description": "Storage group for partition MGMT1",
        "direct-connection-count": 0,
        "fulfillment-state": "complete",
        "max-partitions": 1,
        "name": "CPCA_SG_MGMT1",
        "object-id": "edd782f2-200a-11e9-a142-00106f239c31",
        "object-uri": "/api/storage-groups/edd782f2-200a-11e9-a142-00106f239c31",
        "parent": "/api/console",
        "shared": false,
        "storage-volume-uris": [
            "/api/storage-groups/edd782f2-200a-11e9-a142-00106f239c31/storage-volumes/f02e2632-200a-11e9-8748-00106f239c31"
        ],
        "storage-volumes": [
            {
                "active-size": 128.0,
                "class": "storage-volume",
                "description": "Boot volume",
                "element-id": "f02e2632-200a-11e9-8748-00106f239c31",
                "element-uri": "/api/storage-groups/edd782f2-200a-11e9-a142-00106f239c31/storage-volumes/f02e2632-200a-11e9-8748-00106f239c31",
                "fulfillment-state": "complete",
                "name": "MGMT1_MGMT1-boot",
                "parent": "/api/storage-groups/edd782f2-200a-11e9-a142-00106f239c31",
                "paths": [
                    {
                        "device-number": "0015",
                        "logical-unit-number": "0000000000000000",
                        "partition-uri": "/api/partitions/009c0f4c-3588-11e9-bad3-00106f239d19",
                        "target-world-wide-port-name": "5005076810260382"
                    }
                ],
                "size": 128.0,
                "usage": "boot",
                "uuid": "600507681081001D4800000000000083"
            }
        ],
        "type": "fcp",
        "unassigned-world-wide-port-names": [],
        "virtual-storage-resource-uris": [
            "/api/storage-groups/edd782f2-200a-11e9-a142-00106f239c31/virtual-storage-resources/db682456-358a-11e9-bc93-00106f239d19"
        ],
        "virtual-storage-resources": [
            {
                "adapter-port-uri": "/api/adapters/e0ea33d6-e578-11e8-a87c-00106f239c31/storage-ports/0",
                "class": "virtual-storage-resource",
                "description": "",
                "device-number": "0015",
                "element-id": "db682456-358a-11e9-bc93-00106f239d19",
                "element-uri": "/api/storage-groups/edd782f2-200a-11e9-a142-00106f239c31/virtual-storage-resources/db682456-358a-11e9-bc93-00106f239d19",
                "name": "vhba_CPCA_SG_MGMT12",
                "parent": "/api/storage-groups/edd782f2-200a-11e9-a142-00106f239c31",
                "partition-uri": "/api/partitions/009c0f4c-3588-11e9-bad3-00106f239d19",
                "world-wide-port-name": "c05076d24d80016e",
                "world-wide-port-name-info": {
                    "status": "validated",
                    "world-wide-port-name": "c05076d24d80016e"
                }
            }
        ]
    }
"""

import logging  # noqa: E402
import traceback  # noqa: E402
from ansible.module_utils.basic import AnsibleModule  # noqa: E402

from ..module_utils.common import log_init, Error, ParameterError, \
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
LOGGER_NAME = 'zhmc_storage_group'

LOGGER = logging.getLogger(LOGGER_NAME)

# Dictionary of properties of storage group resources, in this format:
#   name: (allowed, create, update, eq_func, type_cast)
# where:
#   name: Name of the property according to the data model, with hyphens
#     replaced by underscores (this is how it is or would be specified in
#     the 'properties' module parameter).
#   allowed: Indicates whether it is allowed in the 'properties' module
#     parameter.
#   create: Indicates whether it can be specified for the "Create Storage
#     Group" operation.
#   update: Indicates whether it can be specified for the "Modify Storage
#     Group Properties" operation (at all).
#   update_while_active: Indicates whether it can be specified for the "Modify
#     Storage Group Properties" operation while the storage group is attached
#     to any partition. None means "not applicable" (used for update=False).
#   eq_func: Equality test function for two values of the property; None means
#     to use Python equality.
#   type_cast: Type cast function for an input value of the property; None
#     means to use it directly. This can be used for example to convert
#     integers provided as strings by Ansible back into integers (that is a
#     current deficiency of Ansible).
ZHMC_STORAGE_GROUP_PROPERTIES = {

    # create-only properties:
    'cpc-uri': (False, True, False, None, None, None),  # auto-generated here
    'type': (True, True, False, None, None, None),

    # update-only properties: None

    # non-data model properties:
    'storage-volumes': (False, True, True, None, None, None),
    # storage-volumes is a request-info object and is not part of the data
    # model. Changes to storage volumes are performed via the
    # zhmc_storage_volume.py module.

    # create+update properties:
    'name': (False, True, True, True, None, None),
    # name: provided in 'name' module parm
    'description': (True, True, True, True, None, to_unicode),
    'shared': (True, True, True, True, None, None),
    'connectivity': (True, True, True, True, None, int),
    'max-partitions': (True, True, True, True, None, int),
    'virtual-machine-count': (True, True, True, True, None, int),
    'email-to-addresses': (True, True, True, True, None, None),
    'email-cc-addresses': (True, True, True, True, None, None),
    'email-insert': (True, True, True, True, None, None),

    # read-only properties:
    'object_uri': (False, False, False, None, None, None),
    'object_id': (False, False, False, None, None, None),
    'parent': (False, False, False, None, None, None),
    'class': (False, False, False, None, None, None),
    'fulfillment-state': (False, False, False, None, None, None),
    'storage-volume-uris': (False, False, False, None, None, None),
    # storage-volume-uris: updated via method
    'virtual-storageresource-uris': (False, False, False, None, None, None),
    'active-connectivity': (False, False, False, None, None, None),
    'active-max-partitions': (False, False, False, None, None, None),
    'candidate-adapter-port-uris': (False, False, False, None, None, None),
    # candidate-adapter-port-uris: updated via method
    'unassigned-worldwide-port-names': (False, False, False, None, None, None),
}


def process_properties(cpc, storage_group, params):
    """
    Process the properties specified in the 'properties' module parameter,
    and return two dictionaries (create_props, update_props) that contain
    the properties that can be created, and the properties that can be updated,
    respectively. If the resource exists, the input property values are
    compared with the existing resource property values and the returned set
    of properties is the minimal set of properties that need to be changed.

    - Underscores in the property names are translated into hyphens.
    - The presence of read-only properties, invalid properties (i.e. not
      defined in the data model for storage groups), and properties that are
      not allowed because of restrictions or because they are auto-created from
      an artificial property is surfaced by raising ParameterError.
    - The properties resulting from handling artificial properties are
      added to the returned dictionaries.

    Parameters:

      cpc (zhmcclient.Cpc): CPC with the partition to be updated, and
        with the adapters to be used for the partition.

      storage_group (zhmcclient.StorageGroup): Storage group object to be
        updated with the full set of current properties, or `None` if it did
        not previously exist.

      params (dict): Module input parameters.

    Returns:
      tuple of (create_props, update_props), where:
        * create_props: dict of properties for
          zhmcclient.StorageGroupManager.create()
        * update_props: dict of properties for
          zhmcclient.StorageGroup.update_properties()

    Raises:
      ParameterError: An issue with the module parameters.
    """
    create_props = {}
    update_props = {}

    # handle 'name' and 'cpc-uri' properties.
    sg_name = to_unicode(params['name'])
    if storage_group is None:
        # SG does not exist yet.
        create_props['name'] = sg_name
        create_props['cpc-uri'] = cpc.uri
    else:
        # SG does already exist.
        # We looked up the storage group by name, so we will never have to
        # update the storage group name.
        # Updates to the associated CPC are not possible.
        sg_cpc = storage_group.cpc
        if sg_cpc.uri != cpc.uri:
            raise ParameterError(
                "Storage group {0!r} is not associated with the specified "
                "CPC {1!r}, but with CPC {2!r}.".
                format(sg_name, cpc.name, sg_cpc.name))

    # handle the other properties
    input_props = params.get('properties', None)
    if input_props is None:
        input_props = {}
    for prop_name in input_props:

        if prop_name not in ZHMC_STORAGE_GROUP_PROPERTIES:
            raise ParameterError(
                "Property {0!r} is not defined in the data model for "
                "storage groups.".format(prop_name))

        allowed, create, update, update_while_active, eq_func, type_cast = \
            ZHMC_STORAGE_GROUP_PROPERTIES[prop_name]

        if not allowed:
            raise ParameterError(
                "Property {0!r} is not allowed in the 'properties' module "
                "parameter.".format(prop_name))

        # Process a normal (= non-artificial) property
        _create_props, _update_props, _stop = process_normal_property(
            prop_name, ZHMC_STORAGE_GROUP_PROPERTIES, input_props,
            storage_group)
        create_props.update(_create_props)
        update_props.update(_update_props)
        if _stop:
            raise AssertionError()

    return create_props, update_props


def add_artificial_properties(sg_properties, storage_group, expand):
    """
    Add artificial properties to the storage_group object.

    Upon return, the sg_properties dict has been extended by these properties:

    Regardless of expand:

    * 'attached-partition-names': List of Partition names to which the storage
      group is attached.

    If expand is True:

    * 'candidate-adapter-ports': List of Port objects, each of which is
      represented as its dictionary of properties.

      The Port properties are extended by these properties:

      - 'parent-adapter': Adapter object of the port, represented as its
        dictionary of properties.

    * 'storage-volumes': List of StorageVolume objects, each of which is
      represented as its dictionary of properties.

    * 'virtual-storage-resources': List of VirtualStorageResource objects,
      each of which is represented as its dictionary of properties.

    * 'attached-partitions': List of Partition objects to which the storage
      group is attached. Each Partition object is represented as a dictionary
      of its properties.
    """

    parts = storage_group.list_attached_partitions()

    # List of attached partitions (just the names)
    part_names_prop = []
    for part in parts:
        part_names_prop.append(part.get_property('name'))
    sg_properties['attached-partition-names'] = part_names_prop

    if expand:

        # Candidate adapter ports and their parent adapters (full set of props)
        caps_prop = []
        for cap in storage_group.list_candidate_adapter_ports(
                full_properties=True):
            adapter = cap.manager.adapter
            adapter.pull_full_properties()
            cap_properties = dict(cap.properties)
            cap_properties['parent-adapter'] = dict(adapter.properties)
            caps_prop.append(cap_properties)
        sg_properties['candidate-adapter-ports'] = caps_prop

        # Storage volumes (full set of properties).
        # Note: We create the storage volumes from the 'storage-volume-uris'
        # property, because the 'List Storage Volumes of a Storage Group'
        # operation returns an empty list for auto-discovered volumes.
        svs_prop = []
        sv_uris = storage_group.get_property('storage-volume-uris')
        for sv_uri in sv_uris:
            sv = storage_group.storage_volumes.resource_object(sv_uri)
            sv.pull_full_properties()
            svs_prop.append(dict(sv.properties))
        sg_properties['storage-volumes'] = svs_prop

        # Virtual storage resources (full set of properties).
        vsrs_prop = []
        vsr_uris = storage_group.get_property('virtual-storage-resource-uris')
        for vsr_uri in vsr_uris:
            vsr = storage_group.virtual_storage_resources.resource_object(
                vsr_uri)
            vsr.pull_full_properties()
            vsrs_prop.append(dict(vsr.properties))
        sg_properties['virtual-storage-resources'] = vsrs_prop

        # List of attached partitions (full set of properties).
        parts = storage_group.list_attached_partitions()
        parts_prop = []
        for part in parts:
            part.pull_full_properties()
            parts_prop.append(dict(part.properties))
        sg_properties['attached-partitions'] = parts_prop


def ensure_present(params, check_mode):
    """
    Ensure that the storage group exists and has the specified properties.

    Storage volumes are not subject of this function, they are handled by the
    zhmc_storage_volume.py module.

    Raises:
      ParameterError: An issue with the module parameters.
      zhmcclient.Error: Any zhmcclient exception can happen.
    """

    host = params['hmc_host']
    userid, password, ca_certs, verify = get_hmc_auth(params['hmc_auth'])
    cpc_name = params['cpc_name']
    storage_group_name = params['name']
    expand = params['expand']
    _faked_session = params.get('_faked_session', None)

    changed = False
    result = {}

    session = get_session(
        _faked_session, host, userid, password, ca_certs, verify)
    try:
        client = zhmcclient.Client(session)
        console = client.consoles.console
        cpc = client.cpcs.find(name=cpc_name)
        # The default exception handling is sufficient for the above.

        try:
            storage_group = console.storage_groups.find(
                name=storage_group_name)
        except zhmcclient.NotFound:
            storage_group = None

        if storage_group is None:
            # It does not exist. Create it and update it if there are
            # update-only properties.
            if not check_mode:
                create_props, update_props = \
                    process_properties(cpc, storage_group, params)
                storage_group = console.storage_groups.create(
                    create_props)
                update2_props = {}
                for name, value in update_props.items():
                    if name not in create_props:
                        update2_props[name] = value
                if update2_props:
                    storage_group.update_properties(update2_props)
                # We refresh the properties after the update, in case an
                # input property value gets changed.
                storage_group.pull_full_properties()
            else:
                # TODO: Show props in module result also in check mode.
                pass
            changed = True
        else:
            # It exists. Update its properties.
            storage_group.pull_full_properties()
            create_props, update_props = \
                process_properties(cpc, storage_group, params)
            if create_props:
                raise AssertionError("Unexpected "
                                     "create_props: %r" % create_props)
            if update_props:
                if not check_mode:
                    storage_group.update_properties(update_props)
                    # We refresh the properties after the update, in case an
                    # input property value gets changed.
                    storage_group.pull_full_properties()
                else:
                    # TODO: Show updated props in mod.result also in chk.mode
                    pass
                changed = True

        if not check_mode:
            if not storage_group:
                raise AssertionError()
            result = dict(storage_group.properties)
            add_artificial_properties(result, storage_group, expand)

        return changed, result

    finally:
        session.logoff()


def ensure_absent(params, check_mode):
    """
    Ensure that the storage group does not exist.

    Raises:
      ParameterError: An issue with the module parameters.
      zhmcclient.Error: Any zhmcclient exception can happen.
    """

    host = params['hmc_host']
    userid, password, ca_certs, verify = get_hmc_auth(params['hmc_auth'])
    cpc_name = params['cpc_name']
    storage_group_name = params['name']
    _faked_session = params.get('_faked_session', None)

    changed = False
    result = {}

    session = get_session(
        _faked_session, host, userid, password, ca_certs, verify)
    try:
        client = zhmcclient.Client(session)
        console = client.consoles.console
        cpc = client.cpcs.find(name=cpc_name)
        # The default exception handling is sufficient for the above.

        try:
            storage_group = console.storage_groups.find(
                name=storage_group_name)
        except zhmcclient.NotFound:
            return changed, result

        sg_cpc = storage_group.cpc
        if sg_cpc.uri != cpc.uri:
            raise ParameterError(
                "Storage group {0!r} is not associated with the specified "
                "CPC {1!r}, but with CPC {2!r}.".
                format(storage_group_name, cpc.name, sg_cpc.name))

        if not check_mode:
            partitions = storage_group.list_attached_partitions()
            for part in partitions:
                # This will raise HTTPError(409) if the partition is in one of
                # the transitional states ('starting', 'stopping').
                part.detach_storage_group(storage_group)
            storage_group.delete()
        changed = True

        return changed, result

    finally:
        session.logoff()


def facts(params, check_mode):
    """
    Return facts about a storage group and its storage volumes and virtual
    storage resources.

    Raises:
      ParameterError: An issue with the module parameters.
      zhmcclient.Error: Any zhmcclient exception can happen.
    """

    host = params['hmc_host']
    userid, password, ca_certs, verify = get_hmc_auth(params['hmc_auth'])
    cpc_name = params['cpc_name']
    storage_group_name = params['name']
    expand = params['expand']
    _faked_session = params.get('_faked_session', None)

    changed = False
    result = {}

    session = get_session(
        _faked_session, host, userid, password, ca_certs, verify)
    try:
        # The default exception handling is sufficient for this code
        client = zhmcclient.Client(session)
        console = client.consoles.console
        cpc = client.cpcs.find(name=cpc_name)

        storage_group = console.storage_groups.find(name=storage_group_name)
        storage_group.pull_full_properties()

        sg_cpc = storage_group.cpc
        if sg_cpc.uri != cpc.uri:
            raise ParameterError(
                "Storage group {0!r} is not associated with the specified "
                "CPC {1!r}, but with CPC {2!r}.".
                format(storage_group_name, cpc.name, sg_cpc.name))

        result = dict(storage_group.properties)
        add_artificial_properties(result, storage_group, expand)

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
      zhmcclient.Error: Any zhmcclient exception can happen.
    """
    actions = {
        "absent": ensure_absent,
        "present": ensure_present,
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
                   choices=['absent', 'present', 'facts']),
        properties=dict(required=False, type='dict', default={}),
        expand=dict(required=False, type='bool', default=False),
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
    module.exit_json(changed=changed, storage_group=result)


if __name__ == '__main__':
    main()
