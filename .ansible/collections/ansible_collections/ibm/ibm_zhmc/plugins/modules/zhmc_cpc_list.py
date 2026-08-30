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
module: zhmc_cpc_list
version_added: "2.9.0"
short_description: List CPCs
description:
  - List CPCs (Z systems). By default, only the CPCs managed by the targeted
    HMC are listed. Optionally, unmanaged CPCs can be listed in addition.
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
  include_unmanaged_cpcs:
    description:
      - Include unmanaged CPCs in the result. The unmanaged CPCs will have
        only their name as a property. Note that managed CPCs are always
        included in the result.
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

- name: List managed CPCs
  zhmc_cpc_list:
    hmc_host: "{{ my_hmc_host }}"
    hmc_auth: "{{ my_hmc_auth }}"
  register: cpc_list

- name: List managed and unmanaged CPCs
  zhmc_cpc_list:
    hmc_host: "{{ my_hmc_host }}"
    hmc_auth: "{{ my_hmc_auth }}"
    include_unmanaged_cpcs: true
  register: cpc_list

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
cpcs:
  description: The list of CPCs, with a subset of their properties.
  returned: success
  type: list
  elements: dict
  contains:
    name:
      description: "CPC name"
      type: str
    is_managed:
      description: Indicates wehether the CPC is managed by the targeted HMC
        (true) or is unmanaged (false).
      type: bool
    status:
      description: The current status of the CPC. For details, see the
        description of the 'status' property in the data model of the 'CPC'
        resource (see :term:`HMC API`).
        Only included for managed CPCs.
      type: str
    has_unacceptable_status:
      description: Indicates whether the current status of the CPC is
        unacceptable, based on its 'acceptable-status' property.
        Only included for managed CPCs.
      type: bool
    dpm_enabled:
      description: Indicates wehether the CPC is in DPM mode (true) or in
        classic mode (false).
        Only included for managed CPCs.
      type: bool
    se_version:
      description: The SE version of the CPC, as a string 'M.N.U'.
        Only included for managed CPCs.
      type: str
  sample:
    [
        {
            "name": "CPCA",
            "is_managed": True,
            "status": "active",
            "has_unacceptable_status": False,
            "dpm_enabled": True,
            "se_version": "2.15"
        },
        {
            "name": "NewCPC",
            "is_managed": False
        }
    ]
"""

import logging  # noqa: E402
import traceback  # noqa: E402
from ansible.module_utils.basic import AnsibleModule  # noqa: E402

from ..module_utils.common import log_init, Error, get_hmc_auth, get_session, \
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
LOGGER_NAME = 'zhmc_cpc_list'

LOGGER = logging.getLogger(LOGGER_NAME)


def perform_list(params):
    """
    List the managed CPCs and return a subset of properties.

    Raises:
      ParameterError: An issue with the module parameters.
      zhmcclient.Error: Any zhmcclient exception can happen.
    """

    host = params['hmc_host']
    userid, password, ca_certs, verify = get_hmc_auth(params['hmc_auth'])
    include_unmanaged_cpcs = params.get('include_unmanaged_cpcs', False)
    _faked_session = params.get('_faked_session', None)  # No default specified

    session = get_session(
        _faked_session, host, userid, password, ca_certs, verify)
    try:
        client = zhmcclient.Client(session)

        cpc_list = []

        # List the managed CPCs
        cpcs = client.cpcs.list()
        # The default exception handling is sufficient for the above.
        for cpc in cpcs:
            cpc_properties = {
                "name": cpc.name,
                "is_managed": True,
                "status": cpc.get_property('status'),
                "has_unacceptable_status": cpc.get_property(
                    'has-unacceptable-status'),
                "dpm_enabled": cpc.get_property('dpm-enabled'),
                "se_version": cpc.get_property('se-version'),
            }
            cpc_list.append(cpc_properties)

        # List the unmanaged CPCs
        if include_unmanaged_cpcs:
            cpcs = client.consoles.console.list_unmanaged_cpcs()
            # The default exception handling is sufficient for the above.
            for cpc in cpcs:
                cpc_properties = {
                    "name": cpc.name,
                    "is_managed": False,
                }
                cpc_list.append(cpc_properties)

        return cpc_list

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
        include_unmanaged_cpcs=dict(required=False, type='bool', default=False),
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
    module.exit_json(changed=changed, cpcs=result_list)


if __name__ == '__main__':
    main()
