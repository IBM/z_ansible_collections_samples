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
module: zhmc_storage_volume
version_added: "2.9.0"
short_description: Create storage volumes
description:
  - Gather facts about a storage volume in a storage group associated with a
    CPC (Z system).
  - Create, delete, or update a storage volume in a storage group associated
    with a CPC.
notes:
  - This module manages only the knowledge of the Z system about its storage,
    but does not perform any actions against the storage subsystems or
    SAN switches attached to the Z system.
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
      - The name of the CPC associated with the storage group containing the
        target storage volume.
    type: str
    required: true
  storage_group_name:
    description:
      - The name of the storage group containing the target storage volume.
    type: str
    required: true
  name:
    description:
      - The name of the target storage volume.
    type: str
    required: true
  state:
    description:
      - "The desired state for the storage volume. All states are fully
         idempotent within the limits of the properties that can be changed:"
      - "* C(absent): Ensures that the storage volume does not exist in the
         specified storage group."
      - "* C(present): Ensures that the storage volume exists in the specified
         storage group, and has the specified properties."
      - "* C(facts): Returns the storage volume properties."
    type: str
    required: true
    choices: ['absent', 'present', 'facts']
  properties:
    description:
      - "Dictionary with desired properties for the storage volume.
         Used for C(state=present); ignored for C(state=absent|facts).
         Dictionary key is the property name with underscores instead
         of hyphens, and dictionary value is the property value in YAML syntax.
         Integer properties may also be provided as decimal strings."
      - "The possible input properties in this dictionary are the properties
         defined as writeable in the data model for Storage Volume resources
         (where the property names contain underscores instead of hyphens),
         with the following exceptions:"
      - "* C(name): Cannot be specified because the name has already been
         specified in the C(name) module parameter."
      - "Properties omitted in this dictionary will remain unchanged when the
         storage volume already exists, and will get the default value defined
         in the data model for storage volumes in the :term:`HMC API` when the
         storage volume is being created."
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

- name: Gather facts about a storage volume
  zhmc_storage_volume:
    hmc_host: "{{ my_hmc_host }}"
    hmc_auth: "{{ my_hmc_auth }}"
    cpc_name: "{{ my_cpc_name }}"
    storage_group_name: "{{ my_storage_group_name }}"
    name: "{{ my_storage_volume_name }}"
    state: facts
  register: sv1

- name: Ensure the storage volume does not exist
  zhmc_storage_volume:
    hmc_host: "{{ my_hmc_host }}"
    hmc_auth: "{{ my_hmc_auth }}"
    cpc_name: "{{ my_cpc_name }}"
    storage_group_name: "{{ my_storage_group_name }}"
    name: "{{ my_storage_volume_name }}"
    state: absent

- name: Ensure the storage volume exists
  zhmc_storage_volume:
    hmc_host: "{{ my_hmc_host }}"
    hmc_auth: "{{ my_hmc_auth }}"
    cpc_name: "{{ my_cpc_name }}"
    storage_group_name: "{{ my_storage_group_name }}"
    name: "{{ my_storage_volume_name }}"
    state: present
    properties:
      description: "Example storage volume 1"
      size: 1
  register: sv1

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
storage_volume:
  description:
    - "For C(state=absent), an empty dictionary."
    - "For C(state=present|facts), the resource properties of the storage
       volume after any changes."
  returned: success
  type: dict
  contains:
    name:
      description: "Storage volume name"
      type: str
    type:
      description: "Type of the storage volume ('fc' or 'fcp'), as defined in
        its storage group."
      type: str
    "{property}":
      description: "Additional properties of the storage volume, as described
        in the data model of the 'Storage Volume' element object of the
        'Storage Group' object in the :term:`HMC API` book.
        The property names have hyphens (-) as described in that book."
  sample:
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
        "type": "fcp",
        "usage": "boot",
        "uuid": "600507681081001D4800000000000083"
    }
"""

import logging  # noqa: E402
import traceback  # noqa: E402
from ansible.module_utils.basic import AnsibleModule  # noqa: E402

from ..module_utils.common import log_init, Error, ParameterError, \
    eq_hex, get_hmc_auth, get_session, to_unicode, process_normal_property, \
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
LOGGER_NAME = 'zhmc_storage_volume'

LOGGER = logging.getLogger(LOGGER_NAME)

# Dictionary of properties of storage volume resources, in this format:
#   name: (allowed, create, update, update_while_active, eq_func, type_cast)
# where:
#   name: Name of the property according to the data model, with hyphens
#     replaced by underscores (this is how it is or would be specified in
#     the 'properties' module parameter).
#   allowed: Indicates whether it is allowed in the 'properties' module
#     parameter.
#   create: Indicates whether it can be specified for creating a storage volume
#     using the "Modify Storage Group Properties" operation (i.e.
#     operation="create" in "storage-volume-request-info").
#   update: Indicates whether it can be specified for modifying a storage
#     volume using the "Modify Storage Group Properties" operation (i.e.
#     operation="modify" in "storage-volume-request-info").
#   update_while_active: Indicates whether it can be specified for modifying a
#     storage volume using the "Modify Storage Group Properties" operation
#     while the storage group is attached to any partition. None means
#     "not applicable" (used for update=False).
#   eq_func: Equality test function for two values of the property; None means
#     to use Python equality.
#   type_cast: Type cast function for an input value of the property; None
#     means to use it directly. This can be used for example to convert
#     integers provided as strings by Ansible back into integers (that is a
#     current deficiency of Ansible).
ZHMC_STORAGE_VOLUME_PROPERTIES = {

    # create-only properties: None
    # update-only properties: None

    # create+update properties:
    'name': (False, True, True, True, None, None),  # provided in module parm
    'description': (True, True, True, True, None, to_unicode),
    'size': (True, True, True, True, None, float),
    'usage': (True, True, True, True, None, None),
    'model': (True, True, True, True, None, None),  # ECKD only
    'cylinders': (True, True, True, True, None, int),  # ECKD only
    'device_number': (True, True, True, True, eq_hex, int),  # ECKD only

    # read-only properties:
    'element_uri': (False, False, False, None, None, None),
    'element_id': (False, False, False, None, None, None),
    'parent': (False, False, False, None, None, None),
    'class': (False, False, False, None, None, None),
    'fulfillment_state': (False, False, False, None, None, None),
    'active_size': (False, False, False, None, None, None),
    'uuid': (False, False, False, None, None, None),
    'active_model': (False, False, False, None, None, None),
    'control_unit_uri': (False, False, False, None, None, None),
    'eckd_type': (False, False, False, None, None, None),
    'unit_address': (False, False, False, None, None, None),

    # artificial properties:
    # 'type': 'fc' or 'fcp', as defined in its storage group
}


def process_properties(cpc, storage_group, storage_volume, params):
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

      cpc (zhmcclient.Cpc): CPC associated to the storage group of the target
        storage volume.

      storage_group (zhmcclient.StorageGroup): Storage group of the target
        storage volume.

      storage_volume (zhmcclient.StorageVolume): Target storage volume if it
        currently exists, or `None` if it does not currently exist.

      params (dict): Module input parameters.

    Returns:
      tuple of (create_props, update_props), where:
        * create_props: dict of properties for
          zhmcclient.StorageVolumeManager.create()
        * update_props: dict of properties for
          zhmcclient.StorageVolume.update_properties()

    Raises:
      ParameterError: An issue with the module parameters.
    """
    create_props = {}
    update_props = {}

    # handle 'name' property.
    sv_name = to_unicode(params['name'])
    if storage_volume is None:
        # SV does not exist yet.
        create_props['name'] = sv_name
    else:
        # SV does already exist.
        # We looked up the storage volume by name, so we will never have to
        # update the storage volume name.
        pass

    # handle the other properties
    input_props = params.get('properties', None)
    if input_props is None:
        input_props = {}
    for prop_name in input_props:

        if prop_name not in ZHMC_STORAGE_VOLUME_PROPERTIES:
            raise ParameterError(
                "Property {0!r} is not defined in the data model for "
                "storage volumes.".format(prop_name))

        allowed, create, update, update_while_active, eq_func, type_cast = \
            ZHMC_STORAGE_VOLUME_PROPERTIES[prop_name]

        if not allowed:
            raise ParameterError(
                "Property {0!r} is not allowed in the 'properties' module "
                "parameter.".format(prop_name))

        # Process a normal (= non-artificial) property
        _create_props, _update_props, _stop = process_normal_property(
            prop_name, ZHMC_STORAGE_VOLUME_PROPERTIES, input_props,
            storage_volume)
        create_props.update(_create_props)
        update_props.update(_update_props)
        if _stop:
            raise AssertionError()

    return create_props, update_props


def add_artificial_properties(sv_properties, storage_volume):
    """
    Add artificial properties to the sv_properties dict.

    Upon return, the sv_properties dict has been extended by these properties:

    * 'type': Type of storage group of the volume: 'fc' (for ECKD) or 'fcp'.
    """

    storage_group = storage_volume.manager.parent

    # Type property
    type_prop = storage_group.get_property('type')
    sv_properties['type'] = type_prop


def ensure_present(params, check_mode):
    """
    Ensure that the storage volume is defined and has the specified properties.

    Raises:
      ParameterError: An issue with the module parameters.
      zhmcclient.Error: Any zhmcclient exception can happen.
    """

    host = params['hmc_host']
    userid, password, ca_certs, verify = get_hmc_auth(params['hmc_auth'])
    cpc_name = params['cpc_name']
    storage_group_name = params['storage_group_name']
    storage_volume_name = params['name']
    _faked_session = params.get('_faked_session', None)

    changed = False
    result = {}

    session = get_session(
        _faked_session, host, userid, password, ca_certs, verify)
    try:
        client = zhmcclient.Client(session)
        console = client.consoles.console
        cpc = client.cpcs.find(name=cpc_name)
        storage_group = console.storage_groups.find(name=storage_group_name)
        # The default exception handling is sufficient for the above.

        sg_cpc = storage_group.cpc
        if sg_cpc.uri != cpc.uri:
            raise ParameterError(
                "Storage group {0!r} is not associated with the specified "
                "CPC {1!r}, but with CPC {2!r}.".
                format(storage_group_name, cpc.name, sg_cpc.name))

        try:
            storage_volume = storage_group.storage_volumes.find(
                name=storage_volume_name)
        except zhmcclient.NotFound:
            storage_volume = None
        except zhmcclient.NoUniqueMatch:
            # The name of storage volumes within their storage group is not
            # enforced to be unique.
            raise

        if storage_volume is None:
            # It does not exist. Create it and update it if there are
            # update-only properties.
            if not check_mode:
                create_props, update_props = \
                    process_properties(cpc, storage_group, storage_volume,
                                       params)
                storage_volume = storage_group.storage_volumes.create(
                    create_props)
                update2_props = {}
                for name, value in update_props.items():
                    if name not in create_props:
                        update2_props[name] = value
                if update2_props:
                    storage_volume.update_properties(update2_props)
                # We refresh the properties after the update, in case an
                # input property value gets changed.
                storage_volume.pull_full_properties()
            else:
                # TODO: Show props in module result also in check mode.
                pass
            changed = True
        else:
            # It exists. Update its properties.
            storage_volume.pull_full_properties()
            create_props, update_props = \
                process_properties(cpc, storage_group, storage_volume, params)
            if create_props:
                raise AssertionError("Unexpected "
                                     "create_props: %r" % create_props)
            if update_props:
                if not check_mode:
                    storage_volume.update_properties(update_props)
                    # We refresh the properties after the update, in case an
                    # input property value gets changed.
                    storage_volume.pull_full_properties()
                else:
                    # TODO: Show updated props in mod.result also in chk.mode
                    storage_volume.pull_full_properties()
                changed = True

        if not check_mode:
            if not storage_volume:
                raise AssertionError()

        if storage_volume:
            result = dict(storage_volume.properties)
            add_artificial_properties(result, storage_volume)

        return changed, result

    finally:
        session.logoff()


def ensure_absent(params, check_mode):
    """
    Ensure that the storage volume does not exist.

    Raises:
      ParameterError: An issue with the module parameters.
      zhmcclient.Error: Any zhmcclient exception can happen.
    """

    host = params['hmc_host']
    userid, password, ca_certs, verify = get_hmc_auth(params['hmc_auth'])
    cpc_name = params['cpc_name']
    storage_group_name = params['storage_group_name']
    storage_volume_name = params['name']
    _faked_session = params.get('_faked_session', None)

    changed = False
    result = {}

    session = get_session(
        _faked_session, host, userid, password, ca_certs, verify)
    try:
        client = zhmcclient.Client(session)
        console = client.consoles.console
        cpc = client.cpcs.find(name=cpc_name)
        storage_group = console.storage_groups.find(name=storage_group_name)
        # The default exception handling is sufficient for the above.

        sg_cpc = storage_group.cpc
        if sg_cpc.uri != cpc.uri:
            raise ParameterError(
                "Storage group {0!r} is not associated with the specified "
                "CPC {1!r}, but with CPC {2!r}.".
                format(storage_group_name, cpc.name, sg_cpc.name))

        try:
            storage_volume = storage_group.storage_volumes.find(
                name=storage_volume_name)
        except zhmcclient.NotFound:
            return changed, result
        except zhmcclient.NoUniqueMatch:
            # The name of storage volumes within their storage group is not
            # enforced to be unique.
            raise

        if not check_mode:
            storage_volume.delete()
        changed = True

        return changed, result

    finally:
        session.logoff()


def facts(params, check_mode):
    """
    Return facts about a storage volume.

    Raises:
      ParameterError: An issue with the module parameters.
      zhmcclient.Error: Any zhmcclient exception can happen.
    """

    host = params['hmc_host']
    userid, password, ca_certs, verify = get_hmc_auth(params['hmc_auth'])
    cpc_name = params['cpc_name']
    storage_group_name = params['storage_group_name']
    storage_volume_name = params['name']
    _faked_session = params.get('_faked_session', None)

    changed = False
    result = {}

    session = get_session(
        _faked_session, host, userid, password, ca_certs, verify)
    try:
        client = zhmcclient.Client(session)
        console = client.consoles.console
        cpc = client.cpcs.find(name=cpc_name)
        storage_group = console.storage_groups.find(name=storage_group_name)
        # The default exception handling is sufficient for the above.

        sg_cpc = storage_group.cpc
        if sg_cpc.uri != cpc.uri:
            raise ParameterError(
                "Storage group {0!r} is not associated with the specified "
                "CPC {1!r}, but with CPC {2!r}.".
                format(storage_group_name, cpc.name, sg_cpc.name))

        try:
            storage_volume = storage_group.storage_volumes.find(
                name=storage_volume_name)
        except zhmcclient.NoUniqueMatch:
            # The name of storage volumes within their storage group is not
            # enforced to be unique.
            raise

        storage_volume.pull_full_properties()
        result = dict(storage_volume.properties)
        add_artificial_properties(result, storage_volume)

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
        storage_group_name=dict(required=True, type='str'),
        name=dict(required=True, type='str'),
        state=dict(required=True, type='str',
                   choices=['absent', 'present', 'facts']),
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
    module.exit_json(changed=changed, storage_volume=result)


if __name__ == '__main__':
    main()
