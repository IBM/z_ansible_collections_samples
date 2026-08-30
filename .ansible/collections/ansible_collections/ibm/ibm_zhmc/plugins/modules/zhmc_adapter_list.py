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

# TODO: Add once list_permitted_adapters() is supported in zhmcclient:
# - The module works for any HMC version. On HMCs with version 2.14.0 or higher,
#   the "List Permitted Adapters" opration is used. On older HMCs, the
#   managed CPCs are listed and the adapters on each CPC.
DOCUMENTATION = """
---
module: zhmc_adapter_list
version_added: "2.9.0"
short_description: List adapters
description:
  - List adapters on a specific CPC (Z system) or on all managed CPCs.
  - CPCs in classic mode are ignored (i.e. do not lead to a failure).
  - Adapters for which the user has no object access permission are ignored
    (i.e. do not lead to a failure).
author:
  - Andreas Maier (@andy-maier)
requirements:
  - Access to the WS API of the HMC (see :term:`HMC API`).
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
      - "Name of the CPC for which the adapters are to be listed."
      - "Default: All managed CPCs."
    type: str
    required: false
    default: null
  name:
    description:
      - Regular expression pattern for the adapter name to filter the result
        list to matching adapters.
    type: str
    required: false
    default: null
  adapter_id:
    description:
      - Regular expression pattern for the adapter ID (PCHID) ('adapter-id'
        property) to filter the result list to matching adapters.
    type: str
    required: false
    default: null
  adapter_family:
    description:
      - Adapter family ('adapter-family' property) to filter the result list
        to adapters with that family.
    type: str
    required: false
    default: null
  type:
    description:
      - Adapter type ('type' property) to filter the result list
        to adapters with that type.
    type: str
    required: false
    default: null
  status:
    description:
      - Adapter status ('status' property) to filter the result list
        to adapters with that status.
    type: str
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

- name: List all permitted adapters on all managed CPCs
  zhmc_adapter_list:
    hmc_host: "{{ my_hmc_host }}"
    hmc_auth: "{{ my_hmc_auth }}"
  register: adapter_list

- name: List all permitted adapters on a CPC
  zhmc_adapter_list:
    hmc_host: "{{ my_hmc_host }}"
    hmc_auth: "{{ my_hmc_auth }}"
    cpc_name: CPCA
  register: adapter_list

- name: List the permitted FICON adapters on a CPC
  zhmc_adapter_list:
    hmc_host: "{{ my_hmc_host }}"
    hmc_auth: "{{ my_hmc_auth }}"
    cpc_name: CPCA
    adapter_family: "ficon"
  register: adapter_list

"""

RETURN = """
changed:
  description: Indicates if any change has been made by the module.
    This will always be false.
  returned: always
  type: bool
msg:
  description: An error message that describes the failure.
  returned: failure
  type: str
adapters:
  description: The list of adapters, with a subset of their properties.
    For details on the properties, see the data model of the 'Adapter' resource
    (see :term:`HMC API`)
  returned: success
  type: list
  elements: dict
  contains:
    name:
      description: "Adapter name"
      type: str
    cpc_name:
      description: "Name of the parent CPC of the adapter"
      type: str
    adapter_id:
      description: "Adapter ID (PCHID) of the adapter ('adapter-id' property)"
      type: str
    adapter_family:
      description: "Family of the adapter ('adapter-family' property)"
      type: str
    type:
      description: "Type of the adapter ('type' property)"
      type: str
    status:
      description: "The current status of the adapter ('status' property)"
      type: str
  sample:
    [
        {
            "name": "adapter1",
            "cpc_name": "CPC1",
            "adapter_id": "10c",
            "adapter_family": "osa",
            "type": "osd",
            "status": "active",
        }
    ]
"""

import logging  # noqa: E402
import traceback  # noqa: E402
from ansible.module_utils.basic import AnsibleModule  # noqa: E402

from ..module_utils.common import log_init, Error, \
    get_hmc_auth, get_session, missing_required_lib  # noqa: E402

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
LOGGER_NAME = 'zhmc_adapter_list'

LOGGER = logging.getLogger(LOGGER_NAME)


def perform_list(params):
    """
    List the adapters and return a subset of properties.

    Raises:
      ParameterError: An issue with the module parameters.
      zhmcclient.Error: Any zhmcclient exception can happen.
    """

    host = params['hmc_host']
    userid, password, ca_certs, verify = get_hmc_auth(params['hmc_auth'])
    cpc_name = params.get('cpc_name', None)
    name = params.get('name', None)
    adapter_id = params.get('adapter_id', None)
    adapter_family = params.get('adapter_family', None)
    type = params.get('type', None)
    status = params.get('status', None)

    _faked_session = params.get('_faked_session', None)  # No default specified

    session = get_session(
        _faked_session, host, userid, password, ca_certs, verify)
    try:
        client = zhmcclient.Client(session)
        console = client.consoles.console

        filter_args = {}
        if name is not None:
            filter_args['name'] = name
        if adapter_id is not None:
            filter_args['adapter-id'] = adapter_id
        if adapter_family is not None:
            filter_args['adapter-family'] = adapter_family
        if type is not None:
            filter_args['type'] = type
        if status is not None:
            filter_args['status'] = status

        # The "List Permitted Adapters" operation was added in HMC
        # version 2.14.0. The operation depends only on the HMC version and not
        # on the SE/CPC version, so it is supported e.g. for a 2.14 HMC managing
        # a z13 CPC.
        hmc_version = client.query_api_version()['hmc-version']
        hmc_version_info = [int(x) for x in hmc_version.split('.')]
        # TODO: Remove check on list_permitted_adapters() once supported
        if hmc_version_info < [2, 14, 0] or \
                not hasattr(console, 'list_permitted_adapters'):
            # List the adapters in the traditional way
            if cpc_name:
                LOGGER.debug("Listing adapters of CPC %s", cpc_name)
                cpc = client.cpcs.find(name=cpc_name)
                adapters = cpc.adapters.list(filter_args=filter_args)
            else:
                LOGGER.debug("Listing adapters of all managed CPCs")
                cpcs = client.cpcs.list()
                adapters = []
                for cpc in cpcs:
                    adapters.extend(cpc.adapters.list(filter_args=filter_args))
        else:
            # List the adapters using the new operation
            if cpc_name:
                LOGGER.debug("Listing permitted adapters of CPC %s", cpc_name)
                filter_args['cpc-name'] = cpc_name
            else:
                LOGGER.debug("Listing permitted adapters of all managed CPCs")
            adapters = console.list_permitted_adapters(filter_args=filter_args)
        # The default exception handling is sufficient for the above.

        adapter_list = []
        for adapter in adapters:
            parent_cpc = adapter.manager.cpc
            adapter_properties = {
                "name": adapter.name,
                "cpc_name": parent_cpc.name,
                "adapter_id": adapter.get_property('adapter-id'),
                "adapter_family": adapter.get_property('adapter-family'),
                "type": adapter.get_property('type'),
                "status": adapter.get_property('status'),
            }
            adapter_list.append(adapter_properties)

        return adapter_list

    finally:
        session.logoff()


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
        cpc_name=dict(required=False, type='str', default=None),
        name=dict(required=False, type='str', default=None),
        adapter_id=dict(required=False, type='str', default=None),
        adapter_family=dict(required=False, type='str', default=None),
        type=dict(required=False, type='str', default=None),
        status=dict(required=False, type='str', default=None),
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

    log_file = module.params['log_file']
    log_init(LOGGER_NAME, log_file)

    _params = dict(module.params)
    del _params['hmc_auth']
    LOGGER.debug("Module entry: params: %r", _params)

    changed = False
    try:

        result_list = perform_list(module.params)

    except (Error, zhmcclient.Error) as exc:
        # These exceptions are considered errors in the environment or in user
        # input. They have a proper message that stands on its own, so we
        # simply pass that message on and will not need a traceback.
        msg = "{0}: {1}".format(exc.__class__.__name__, exc)
        LOGGER.debug("Module exit (failure): msg: %r", msg)
        module.fail_json(msg=msg)
    # Other exceptions are considered module errors and are handled by Ansible
    # by showing the traceback.

    LOGGER.debug("Module exit (success): changed: %s, result: %r",
                 changed, result_list)
    module.exit_json(changed=changed, adapters=result_list)


if __name__ == '__main__':
    main()
