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
module: zhmc_storage_group_attachment
version_added: "2.9.0"
short_description: Attach storage groups to partitions
description:
  - Gather facts about the attachment of a storage group to a partition of a
    CPC (Z system).
  - Attach and detach a storage group to and from a partition.
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
      - The name of the CPC that has the partition and is associated with the
        storage group.
    type: str
    required: true
  storage_group_name:
    description:
      - The name of the storage group for the attachment.
    type: str
    required: true
  partition_name:
    description:
      - The name of the partition for the attachment.
    type: str
    required: true
  state:
    description:
      - "The desired state for the storage group attachment. All states are
         fully idempotent within the limits of the properties that can be
         changed, unless otherwise stated:"
      - "* C(detached): Ensures that the storage group is not attached to the
         partition. If the storage group is currently attached to the partition
         and the partition is currently active, the module will fail (this is
         an idempotency limitation)."
      - "* C(attached): Ensures that the storage group is attached to the
         partition."
      - "* C(facts): Returns the attachment status."
    type: str
    required: true
    choices: ['detached', 'attached', 'facts']
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

- name: Gather facts about the attachment
  zhmc_storage_group_attachment:
    hmc_host: "{{ my_hmc_host }}"
    hmc_auth: "{{ my_hmc_auth }}"
    cpc_name: "{{ my_cpc_name }}"
    storage_group_name: "{{ my_storage_group_name }}"
    partition_name: "{{ my_partition_name }}"
    state: facts
  register: sga1

- name: Ensure the storage group is attached to the partition
  zhmc_storage_group_attachment:
    hmc_host: "{{ my_hmc_host }}"
    hmc_auth: "{{ my_hmc_auth }}"
    cpc_name: "{{ my_cpc_name }}"
    storage_group_name: "{{ my_storage_group_name }}"
    partition_name: "{{ my_partition_name }}"
    state: attached

- name: "Ensure the storage group is not attached to the partition."
  zhmc_storage_group_attachment:
    hmc_host: "{{ my_hmc_host }}"
    hmc_auth: "{{ my_hmc_auth }}"
    cpc_name: "{{ my_cpc_name }}"
    storage_group_name: "{{ my_storage_group_name }}"
    partition_name: "{{ my_partition_name }}"
    state: detached

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
storage_group_attachment:
  description: "Attachment state of the storage group. If no check mode was
    requested, the attachment state after any changes is returned. If check
    mode was requested, the actual attachment state is returned."
  returned: success
  type: dict
  contains:
    attached:
      description: "Attachment state of the storage group: Indicates whether
        the storage group is attached to the partition."
      type: bool
  sample:
    {
        "attached": false
    }
"""

import logging  # noqa: E402
import traceback  # noqa: E402
from ansible.module_utils.basic import AnsibleModule  # noqa: E402

from ..module_utils.common import log_init, Error, \
    get_hmc_auth, get_session, missing_required_lib, \
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
LOGGER_NAME = 'zhmc_storage_group_attachment'

LOGGER = logging.getLogger(LOGGER_NAME)


def ensure_attached(params, check_mode):
    """
    Ensure that the storage group is attached to the partition.

    Raises:
      ParameterError: An issue with the module parameters.
      zhmcclient.Error: Any zhmcclient exception can happen.
    """

    host = params['hmc_host']
    userid, password, ca_certs, verify = get_hmc_auth(params['hmc_auth'])
    cpc_name = params['cpc_name']
    storage_group_name = params['storage_group_name']
    partition_name = params['partition_name']
    _faked_session = params.get('_faked_session', None)

    changed = False
    attached = None

    session = get_session(
        _faked_session, host, userid, password, ca_certs, verify)
    try:
        client = zhmcclient.Client(session)
        console = client.consoles.console
        cpc = client.cpcs.find(name=cpc_name)
        storage_group = console.storage_groups.find(name=storage_group_name)
        partition = cpc.partitions.find(name=partition_name)
        # The default exception handling is sufficient for the above.

        attached_partitions = storage_group.list_attached_partitions(
            name=partition_name)

        if not attached_partitions:
            # The storage group is detached from the partition
            attached = False
            if not check_mode:
                partition.attach_storage_group(storage_group)
                attached = True
            changed = True
        else:
            # The storage group is already attached to the partition
            if len(attached_partitions) != 1:
                raise AssertionError()
            if attached_partitions[0].name != partition_name:
                raise AssertionError()
            attached = True

        result = dict(attached=attached)

        return changed, result

    finally:
        session.logoff()


def ensure_detached(params, check_mode):
    """
    Ensure that the storage group is detached from the partition.

    Raises:
      ParameterError: An issue with the module parameters.
      zhmcclient.Error: Any zhmcclient exception can happen.
    """

    host = params['hmc_host']
    userid, password, ca_certs, verify = get_hmc_auth(params['hmc_auth'])
    cpc_name = params['cpc_name']
    storage_group_name = params['storage_group_name']
    partition_name = params['partition_name']
    _faked_session = params.get('_faked_session', None)

    changed = False
    attached = None

    session = get_session(
        _faked_session, host, userid, password, ca_certs, verify)
    try:
        client = zhmcclient.Client(session)
        console = client.consoles.console
        cpc = client.cpcs.find(name=cpc_name)
        storage_group = console.storage_groups.find(name=storage_group_name)
        partition = cpc.partitions.find(name=partition_name)
        # The default exception handling is sufficient for the above.

        attached_partitions = storage_group.list_attached_partitions(
            name=partition_name)

        if attached_partitions:
            # The storage group is attached to the partition
            if len(attached_partitions) != 1:
                raise AssertionError()
            if attached_partitions[0].name != partition_name:
                raise AssertionError()
            attached = True
            if not check_mode:
                partition.detach_storage_group(storage_group)
                attached = False
            changed = True
        else:
            # The storage group is already detached from the partition
            attached = False

        result = dict(attached=attached)

        return changed, result

    finally:
        session.logoff()


def facts(params, check_mode):
    """
    Return facts about the attachment of a storage group to a partition.

    Raises:
      ParameterError: An issue with the module parameters.
      zhmcclient.Error: Any zhmcclient exception can happen.
    """

    host = params['hmc_host']
    userid, password, ca_certs, verify = get_hmc_auth(params['hmc_auth'])
    cpc_name = params['cpc_name']
    storage_group_name = params['storage_group_name']
    partition_name = params['partition_name']
    _faked_session = params.get('_faked_session', None)

    changed = False
    attached = None

    session = get_session(
        _faked_session, host, userid, password, ca_certs, verify)
    try:
        client = zhmcclient.Client(session)
        console = client.consoles.console
        cpc = client.cpcs.find(name=cpc_name)
        storage_group = console.storage_groups.find(name=storage_group_name)
        cpc.partitions.find(name=partition_name)  # check existance
        # The default exception handling is sufficient for the above.

        attached_partitions = storage_group.list_attached_partitions(
            name=partition_name)

        if attached_partitions:
            # The storage group is attached to the partition
            if len(attached_partitions) != 1:
                raise AssertionError()
            if attached_partitions[0].name != partition_name:
                raise AssertionError()
            attached = True
        else:
            # The storage group is not attached to the partition
            attached = False

        result = dict(attached=attached)

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
        "detached": ensure_detached,
        "attached": ensure_attached,
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
        partition_name=dict(required=True, type='str'),
        state=dict(required=True, type='str',
                   choices=['detached', 'attached', 'facts']),
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
    module.exit_json(changed=changed, storage_group_attachment=result)


if __name__ == '__main__':
    main()
