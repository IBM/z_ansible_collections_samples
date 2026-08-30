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
module: zhmc_cpc
version_added: "2.9.0"
short_description: Manage CPCs
description:
  - Deactivate a CPC (Z system).
  - Activate a CPC and update its properties.
  - Gather facts about a CPC, and for DPM operational mode, including its
    adapters, partitions and storage groups.
  - Update the properties of a CPC.
author:
  - Andreas Maier (@andy-maier)
  - Andreas Scheuring (@scheuran)
requirements:
  - Access to the WS API of the HMC of the targeted CPC (see :term:`HMC API`).
  - The targeted CPC can be in any operational mode (classic, DPM).
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
  name:
    description:
      - The name of the target CPC.
    type: str
    required: true
  state:
    description:
      - "The desired state for the CPC. All states are fully idempotent
         within the limits of the properties that can be changed:"
      - "* C(inactive): Ensures the CPC is inactive."
      - "* C(active): Ensures the CPC is active and then ensures that the CPC
         has the specified properties. The operational mode of the CPC cannot
         be changed."
      - "* C(set): Ensures that the CPC has the specified properties."
      - "* C(facts): Returns the CPC properties including its child resources."
    type: str
    choices: ['inactive', 'active', 'set', 'facts']
    required: true
  activation_profile_name:
    description:
      - "The name of the reset activation profile to be used when activating the
         CPC in the classic operational mode, for C(state=active).
         This parameter is ignored when the CPC is in classic mode and was
         already active, and when the CPC is in DPM mode."
      - "Default: The reset activation profile specified in the
         'next-activation-profile-name' property of the CPC."
      - "This parameter is not allowed for the other C(state) values."
    type: str
    required: false
  properties:
    description:
      - "Only for C(state=set) and C(state=active): New values for the
         properties of the CPC.
         Properties omitted in this dictionary will remain unchanged.
         This parameter will be ignored for other C(state) values."
      - "The parameter is a dictionary. The key of each dictionary item is the
         property name as specified in the data model for CPC resources, with
         underscores instead of hyphens. The value of each dictionary item is
         the property value (in YAML syntax). Integer properties may also be
         provided as decimal strings."
      - "The possible properties in this dictionary are the properties
         defined as writeable in the data model for CPC resources."
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

- name: Gather facts about the CPC
  zhmc_cpc:
    hmc_host: "{{ my_hmc_host }}"
    hmc_auth: "{{ my_hmc_auth }}"
    name: "{{ my_cpc_name }}"
    state: facts
  register: cpc1

- name: Ensure the CPC is inactive
  zhmc_cpc:
    hmc_host: "{{ my_hmc_host }}"
    hmc_auth: "{{ my_hmc_auth }}"
    name: "{{ my_cpc_name }}"
    state: inactive

- name: Ensure the CPC is active
  zhmc_cpc:
    hmc_host: "{{ my_hmc_host }}"
    hmc_auth: "{{ my_hmc_auth }}"
    name: "{{ my_cpc_name }}"
    state: active
  register: cpc1

- name: Ensure the CPC has the desired property values
  zhmc_cpc:
    hmc_host: "{{ my_hmc_host }}"
    hmc_auth: "{{ my_hmc_auth }}"
    name: "{{ my_cpc_name }}"
    state: set
    properties:
      acceptable_status:
       - active
      description: "This is CPC {{ my_cpc_name }}"
  register: cpc1

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
cpc:
  description: "The CPC and its adapters, partitions, and storage groups."
  returned: success
  type: dict
  contains:
    name:
      description: "CPC name"
      type: str
    "{property}":
      description: "Additional properties of the CPC, as described in the data
        model of the 'CPC' object in the :term:`HMC API` book.
        The property names have hyphens (-) as described in that book."
    adapters:
      description: "The adapters of the CPC, with a subset of their
        properties. For details, see the :term:`HMC API` book."
      type: list
      elements: dict
      contains:
        name:
          description: "Adapter name"
          type: str
        object-uri:
          description: "Canonical URI of the adapter"
          type: str
        adapter-id:
          description: "Adapter ID (PCHID)"
          type: str
        type:
          description: "Adapter type"
          type: str
        adapter-family:
          description: "Adapter family"
          type: str
        status:
          description: "Status of the adapter"
          type: str
    partitions:
      description: "The defined partitions of the CPC, with a subset of their
        properties. For details, see the :term:`HMC API` book."
      type: list
      elements: dict
      contains:
        name:
          description: "Partition name"
          type: str
        object-uri:
          description: "Canonical URI of the partition"
          type: str
        type:
          description: "Type of the partition"
          type: str
        status:
          description: "Status of the partition"
          type: str
    storage-groups:
      description: "The storage groups associated with the CPC, with a subset
        of their properties. For details, see the :term:`HMC API` book."
      type: list
      elements: dict
      contains:
        name:
          description: "Storage group name"
          type: str
        object-uri:
          description: "Canonical URI of the storage group"
          type: str
        type:
          description: "Storage group type"
          type: str
        fulfillment-status:
          description: "Fulfillment status of the storage group"
          type: str
        cpc-uri:
          description: "Canonical URI of the associated CPC"
          type: str
  sample:
    {
        "name": "CPCA",
        "{property}": "... more properties ... ",
        "adapters": [
            {
                "adapter-family": "ficon",
                "adapter-id": "120",
                "name": "FCP_120_SAN1_02",
                "object-uri": "/api/adapters/dfb2147a-e578-11e8-a87c-00106f239c31",
                "status": "active",
                "type": "fcp"
            },
            {
                "adapter-family": "osa",
                "adapter-id": "10c",
                "name": "OSM1",
                "object-uri": "/api/adapters/ddde026c-e578-11e8-a87c-00106f239c31",
                "status": "active",
                "type": "osm"
            },
        ],
        "partitions": [
            {
                "name": "PART1",
                "object-uri": "/api/partitions/c44338de-351b-11e9-9fbb-00106f239d19",
                "status": "stopped",
                "type": "linux"
            },
            {
                "name": "PART2",
                "object-uri": "/api/partitions/6a46d18a-cf79-11e9-b447-00106f239d19",
                "status": "active",
                "type": "ssc"
            },
        ],
        "storage-groups": [
            {
                "cpc-uri": "/api/cpcs/66942455-4a14-3f99-8904-3e7ed5ca28d7",
                "fulfillment-state": "complete",
                "name": "CPCA_SG_PART1",
                "object-uri": "/api/storage-groups/58e41a42-20a6-11e9-8dfc-00106f239c31",
                "type": "fcp"
            },
            {
                "cpc-uri": "/api/cpcs/66942455-4a14-3f99-8904-3e7ed5ca28d7",
                "fulfillment-state": "complete",
                "name": "CPCA_SG_PART2",
                "object-uri": "/api/storage-groups/4947c6d0-f433-11ea-8f73-00106f239d19",
                "type": "fcp"
            },
        ],
    }
"""

import logging  # noqa: E402
import traceback  # noqa: E402
from ansible.module_utils.basic import AnsibleModule  # noqa: E402

from ..module_utils.common import log_init, Error, StatusError, \
    ParameterError, \
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
LOGGER_NAME = 'zhmc_cpc'

LOGGER = logging.getLogger(LOGGER_NAME)

# Dictionary of properties of CPC resources, in this format:
#   name: (allowed, create, update, eq_func, type_cast)
# where:
#   name: Name of the property according to the data model, with hyphens
#     replaced by underscores (this is how it is or would be specified in
#     the 'properties' module parameter).
#   allowed: Indicates whether it is allowed in the 'properties' module
#     parameter.
#   create: Not applicable for CPCs.
#   update: Indicates whether it can be specified for the "Modify CPC
#     Properties" operation (at all).
#   update_while_active: Indicates whether it can be specified for the "Modify
#     CPC Properties" operation while the CPC is active. None means
#     "not applicable" (used for update=False).
#   eq_func: Equality test function for two values of the property; None means
#     to use Python equality.
#   type_cast: Type cast function for an input value of the property; None
#     means to use it directly. This can be used for example to convert
#     integers provided as strings by Ansible back into integers (that is a
#     current deficiency of Ansible).
ZHMC_CPC_PROPERTIES = {

    # update properties for any mode:
    'acceptable_status': (True, None, True, True, None, None),

    # update properties for DPM mode:
    'description': (True, None, True, True, None, to_unicode),

    # update properties for classic mode:
    'next_activation_profile_name': (True, None, True, True, None, to_unicode),
    'processor_running_time_type': (True, None, True, True, None, to_unicode),
    'processor_running_time': (True, None, True, True, None, int),
    # Following property is read-only on z14 and higher:
    'does_wait_state_end_time_slice': (True, None, True, True, None, None),

    # read-only properties (subset):
    'name': (False, None, False, None, None, None),  # provided in 'name' parm
    'object_uri': (False, None, False, None, None, None),
    'object_id': (False, None, False, None, None, None),
    'parent': (False, None, False, None, None, None),
    'class': (False, None, False, None, None, None),

    # The properties not specified here default to allow=False.
}


def process_properties(cpc, params):
    """
    Process the properties specified in the 'properties' module parameter,
    and return a dictionary (update_props) that contains the properties that
    can be updated. The input property values are compared with the existing
    resource property values and the returned set of properties is the minimal
    set of properties that need to be changed.

    - Underscores in the property names are translated into hyphens.
    - The presence of properties that cannot be updated is surfaced by raising
      ParameterError.

    Parameters:

      cpc (zhmcclient.Cpc): CPC to be updated.

      params (dict): Module input parameters.

    Returns:
      update_props: dict of properties for zhmcclient.Cpc.update_properties()

    Raises:
      ParameterError: An issue with the module parameters.
    """

    input_props = params.get('properties', None)
    if input_props is None:
        input_props = {}

    update_props = {}
    for prop_name in input_props:

        try:
            allowed, create, update, update_active, eq_func, type_cast = \
                ZHMC_CPC_PROPERTIES[prop_name]
        except KeyError:
            allowed = False

        if not allowed:
            raise ParameterError(
                "CPC property {0!r} specified in the 'properties' module "
                "parameter cannot be updated.".format(prop_name))

        # Process a normal (= non-artificial) property
        _create_props, _update_props, _stop = process_normal_property(
            prop_name, ZHMC_CPC_PROPERTIES, input_props, cpc)
        update_props.update(_update_props)
        if _create_props:
            raise AssertionError()
        if _stop:
            raise AssertionError()

    return update_props


def add_artificial_properties(cpc_properties, cpc):
    """
    Add artificial properties to the CPC properties.

    Upon return, the cpc_properties dict has been extended by these artificial
    properties:

    * 'partitions': List of partitions of the CPC, with the list subset of
      their properties.

    * 'adapters': List of adapters of the CPC, with the list subset of their
      properties.

    * 'storage-groups': List of storage groups attached to the partition, with
      the list subset of their properties.
    """
    partitions = cpc.partitions.list()
    cpc_properties['partitions'] = [dict(p.properties) for p in partitions]

    adapters = cpc.adapters.list()
    cpc_properties['adapters'] = [dict(a.properties) for a in adapters]

    storage_groups = cpc.manager.console.storage_groups.list(
        filter_args={'cpc-uri': cpc.uri})
    cpc_properties['storage-groups'] = [dict(sg.properties)
                                        for sg in storage_groups]


def ensure_active(params, check_mode):
    """
    Ensure the CPC is active, and then set the properties.

    Raises:
      ParameterError: An issue with the module parameters.
      Error: Other errors during processing.
      zhmcclient.Error: Any zhmcclient exception can happen.
    """

    # Note: Defaults specified in argument_spec will be set in params dict
    host = params['hmc_host']
    userid, password, ca_certs, verify = get_hmc_auth(params['hmc_auth'])
    cpc_name = params['name']
    activation_profile_name = params.get('activation_profile_name', None)
    _faked_session = params.get('_faked_session', None)  # No default specified

    changed = False

    session = get_session(
        _faked_session, host, userid, password, ca_certs, verify)
    try:
        client = zhmcclient.Client(session)
        cpc = client.cpcs.find(name=cpc_name)
        # The default exception handling is sufficient for the above.

        # Activate the CPC
        cpc_status = cpc.get_property('status')
        if cpc_status in ('not-operating', 'no-power'):
            # CPC is inactive
            if not check_mode:
                cpc_dpm_enabled = cpc.get_property('dpm-enabled')
                if cpc_dpm_enabled:
                    cpc.start()
                else:
                    if not activation_profile_name:
                        raise ParameterError(
                            "CPC {0!r} is in classic mode and activation "
                            "requires the 'activation_profile_name' parameter "
                            "to be specified".format(cpc_name))
                    cpc.activate(
                        activation_profile_name=activation_profile_name,
                        force=True)
            changed = True
        elif cpc_status in ('active', 'operating', 'exceptions',
                            'service-required', 'degraded', 'acceptable',
                            'service'):
            # CPC is already active
            pass
        else:
            # cpc_status in ('not-communicating', 'status-check')
            # or any new future status values.
            raise StatusError(
                "CPC {0!r} cannot be activated because it is in status {1!r}".
                format(cpc_name, cpc_status))

        # Set the properties
        cpc.pull_full_properties()
        result = dict(cpc.properties)
        update_props = process_properties(cpc, params)
        if update_props:
            if not check_mode:
                cpc.update_properties(update_props)
            # Some updates of CPC properties are not reflected in a new
            # retrieval of properties until after a few seconds (usually the
            # second retrieval).
            # Therefore, we construct the modified result based upon the input
            # changes, and not based upon newly retrieved properties.
            result.update(update_props)
            changed = True
        add_artificial_properties(result, cpc)

        return changed, result

    finally:
        session.logoff()


def ensure_inactive(params, check_mode):
    """
    Ensure the CPC is inactive. The operational mode is not changed.

    Raises:
      ParameterError: An issue with the module parameters.
      Error: Other errors during processing.
      zhmcclient.Error: Any zhmcclient exception can happen.
    """

    # Note: Defaults specified in argument_spec will be set in params dict
    host = params['hmc_host']
    userid, password, ca_certs, verify = get_hmc_auth(params['hmc_auth'])
    cpc_name = params['name']
    _faked_session = params.get('_faked_session', None)  # No default specified

    changed = False

    session = get_session(
        _faked_session, host, userid, password, ca_certs, verify)
    try:
        client = zhmcclient.Client(session)
        cpc = client.cpcs.find(name=cpc_name)
        # The default exception handling is sufficient for the above.

        # Inactivate the CPC
        cpc_status = cpc.get_property('status')
        if cpc_status in ('not-operating', 'no-power'):
            # Already inactive
            pass
        elif cpc_status in ('active', 'operating', 'exceptions',
                            'service-required', 'degraded', 'acceptable',
                            'service'):
            if not check_mode:
                cpc_dpm_enabled = cpc.get_property('dpm-enabled')
                if cpc_dpm_enabled:
                    cpc.stop()
                else:
                    cpc.deactivate(force=True)
            changed = True
        else:
            # cpc_status in ('not-communicating', 'status-check')
            # or any new future status values.
            raise StatusError(
                "CPC {0!r} cannot be deactivated because it is in status {1!r}".
                format(cpc_name, cpc_status))

        return changed, None

    finally:
        session.logoff()


def ensure_set(params, check_mode):
    """
    Identify the target CPC and ensure that the specified properties are set on
    the target CPC.

    Raises:
      ParameterError: An issue with the module parameters.
      Error: Other errors during processing.
      zhmcclient.Error: Any zhmcclient exception can happen.
    """

    # Note: Defaults specified in argument_spec will be set in params dict
    host = params['hmc_host']
    userid, password, ca_certs, verify = get_hmc_auth(params['hmc_auth'])
    cpc_name = params['name']
    _faked_session = params.get('_faked_session', None)  # No default specified

    changed = False

    session = get_session(
        _faked_session, host, userid, password, ca_certs, verify)
    try:
        client = zhmcclient.Client(session)
        cpc = client.cpcs.find(name=cpc_name)
        # The default exception handling is sufficient for the above.

        # Set the properties
        cpc.pull_full_properties()
        result = dict(cpc.properties)
        update_props = process_properties(cpc, params)
        if update_props:
            if not check_mode:
                cpc.update_properties(update_props)
            # Some updates of CPC properties are not reflected in a new
            # retrieval of properties until after a few seconds (usually the
            # second retrieval).
            # Therefore, we construct the modified result based upon the input
            # changes, and not based upon newly retrieved properties.
            result.update(update_props)
            changed = True

        add_artificial_properties(result, cpc)

        return changed, result

    finally:
        session.logoff()


def facts(params, check_mode):
    """
    Identify the target CPC and return facts about the target CPC and its
    child resources.

    Raises:
      ParameterError: An issue with the module parameters.
      zhmcclient.Error: Any zhmcclient exception can happen.
    """

    host = params['hmc_host']
    userid, password, ca_certs, verify = get_hmc_auth(params['hmc_auth'])
    cpc_name = params['name']
    _faked_session = params.get('_faked_session', None)  # No default specified

    session = get_session(
        _faked_session, host, userid, password, ca_certs, verify)
    try:
        client = zhmcclient.Client(session)
        cpc = client.cpcs.find(name=cpc_name)
        # The default exception handling is sufficient for the above.

        cpc.pull_full_properties()
        result = dict(cpc.properties)
        add_artificial_properties(result, cpc)

        return False, result

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
        "inactive": ensure_inactive,
        "active": ensure_active,
        "set": ensure_set,
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
        name=dict(required=True, type='str'),
        state=dict(required=True, type='str',
                   choices=['inactive', 'active', 'set', 'facts']),
        activation_profile_name=dict(required=False, type='str', default=None),
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
        LOGGER.debug("Module exit (failure): msg: %r", msg)
        module.fail_json(msg=msg)
    # Other exceptions are considered module errors and are handled by Ansible
    # by showing the traceback.

    LOGGER.debug("Module exit (success): changed: %s, cpc: %r",
                 changed, result)
    module.exit_json(
        changed=changed, cpc=result)


if __name__ == '__main__':
    main()
