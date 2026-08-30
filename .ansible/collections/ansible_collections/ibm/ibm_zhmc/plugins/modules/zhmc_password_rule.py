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
module: zhmc_password_rule
version_added: "2.9.0"
short_description: Create HMC password rules
description:
  - Gather facts about a password rule on an HMC of a Z system.
  - Create, delete, or update a password rule on an HMC.
author:
  - Andreas Maier (@andy-maier)
requirements:
  - Access to the WS API of the HMC of the targeted Z system
    (see :term:`HMC API`).
  - The targeted Z system can be in any operational mode (classic, DPM)
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
      - The name of the target password rule.
    type: str
    required: true
  state:
    description:
      - "The desired state for the HMC password rule. All states are fully
         idempotent within the limits of the properties that can be changed:"
      - "* C(absent): Ensures that the password rule does not exist."
      - "* C(present): Ensures that the password rule exists and has the
         specified properties."
      - "* C(facts): Returns the password rule properties."
    type: str
    required: true
    choices: ['absent', 'present', 'facts']
  properties:
    description:
      - "Dictionary with desired properties for the password rule.
         Used for C(state=present); ignored for C(state=absent|facts).
         Dictionary key is the property name with underscores instead
         of hyphens, and dictionary value is the property value in YAML syntax.
         Integer properties may also be provided as decimal strings."
      - "The possible input properties in this dictionary are the properties
         defined as writeable in the data model for Password Rule resources
         (where the property names contain underscores instead of hyphens),
         with the following exceptions:"
      - "* C(name): Cannot be specified because the name has already been
         specified in the C(name) module parameter."
      - "Properties omitted in this dictionary will remain unchanged when the
         password rule already exists, and will get the default value defined
         in the data model for password rules in the :term:`HMC API` when the
         password rule is being created."
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

- name: Gather facts about a password rule
  zhmc_password_rule:
    hmc_host: "{{ my_hmc_host }}"
    hmc_auth: "{{ my_hmc_auth }}"
    name: "{{ my_password_rule_name }}"
    state: facts
  register: rule1

- name: Ensure the password rule does not exist
  zhmc_password_rule:
    hmc_host: "{{ my_hmc_host }}"
    hmc_auth: "{{ my_hmc_auth }}"
    name: "{{ my_password_rule_name }}"
    state: absent

- name: Ensure the password rule exists and has certain properties
  zhmc_password_rule:
    hmc_host: "{{ my_hmc_host }}"
    hmc_auth: "{{ my_hmc_auth }}"
    name: "{{ my_password_rule_name }}"
    state: present
    properties:
      description: "Example password rule 1"
  register: rule1

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
password_rule:
  description:
    - "For C(state=absent), an empty dictionary."
    - "For C(state=present|facts), a
       dictionary with the resource properties of the target password rule."
  returned: success
  type: dict
  contains:
    name:
      description: "Password rule name"
      type: str
    "{property}":
      description: "Additional properties of the password rule, as described
        in the data model of the 'Password Rule' object in the :term:`HMC API`
         book. The property names have hyphens (-) as described in that book."
  sample:
    {
        "case-sensitive": false,
        "character-rules": [
            {
                "special": "allowed",
                "alphabetic": "allowed",
                "min-characters": 1,
                "numeric": "not-allowed",
                "max-characters": 1,
                "custom-character-sets": []
            },
            {
                "special": "allowed",
                "alphabetic": "required",
                "min-characters": 4,
                "numeric": "allowed",
                "max-characters": 28,
                "custom-character-sets": []
            },
            {
                "special": "allowed",
                "alphabetic": "allowed",
                "min-characters": 1,
                "numeric": "not-allowed",
                "max-characters": 1,
                "custom-character-sets": []
            }
        ],
        "class": "password-rule",
        "consecutive-characters": 2,
        "description": "Standard password rule definition",
        "element-id": "520c0138-4a7e-11e9-8bb3-bdfeb245fc36",
        "element-uri": "/api/console/password-rules/520c0138-4a7e-11e9-8bb3-bdfeb245fc36",
        "expiration": 186,
        "history-count": 4,
        "max-length": 30,
        "min-length": 6,
        "name": "Standard",
        "parent": "/api/console",
        "replication-overwrite-possible": false,
        "similarity-count": 0,
        "type": "system-defined"
    }
"""

import uuid  # noqa: E402
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
LOGGER_NAME = 'zhmc_password_rule'

LOGGER = logging.getLogger(LOGGER_NAME)

# Dictionary of properties of password rule resources, in this format:
#   name: (allowed, create, update, eq_func, type_cast)
# where:
#   name: Name of the property according to the data model, with hyphens
#     replaced by underscores (this is how it is or would be specified in
#     the 'properties' module parameter).
#   allowed: Indicates whether it is allowed in the 'properties' module
#     parameter.
#   create: Indicates whether it can be specified for the "Create Password Rule"
#    operation.
#   update: Indicates whether it can be specified for the "Modify Password Rule
#     Properties" operation (at all).
#   update_while_active: Not used for this module.
#   eq_func: Equality test function for two values of the property; None means
#     to use Python equality.
#   type_cast: Type cast function for an input value of the property; None
#     means to use it directly. This can be used for example to convert
#     integers provided as strings by Ansible back into integers (that is a
#     current deficiency of Ansible).
ZHMC_PASSWORD_RULE_PROPERTIES = {

    # create+update properties:
    'name': (False, True, True, True, None, None),
    # name: provided in 'name' module parm
    'description': (True, True, True, True, None, to_unicode),
    'expiration': (True, True, True, True, None, int),
    'min_length': (True, True, True, True, None, int),
    'max_length': (True, True, True, True, None, int),
    'consecutive_characters': (True, True, True, True, None, int),
    'similarity_count': (True, True, True, True, None, int),
    'history_count': (True, True, True, True, None, int),
    'case_sensitive': (True, True, True, True, None, bool),
    'character_rules': (True, True, True, True, None, None),
    # character_rules is an array of character-rule objects:
    # character-rule object:
    #   min_characters: Integer
    #   max_characters: Integer
    #   alphabetic: StringEnum ("allowed","not-allowed","required")
    #   numeric: StringEnum ("allowed","not-allowed","required")
    #   special: StringEnum ("allowed","not-allowed","required")
    #   custom_character_sets: Array of custom-character-set objects
    # custom-character-set object:
    #   character_set: String
    #   inclusion: StringEnum ("allowed","not-allowed","required")

    # read-only properties:
    'element_uri': (False, False, False, True, None, None),
    'element_id': (False, False, False, True, None, None),
    'parent': (False, False, False, True, None, None),
    'class': (False, False, False, True, None, None),
    'type': (False, False, False, True, None, None),
    'replication_overwrite_possible': (False, False, False, True, None, bool),
}


def process_properties(console, pwrule, params):
    """
    Process the properties specified in the 'properties' module parameter,
    and return two dictionaries (create_props, update_props) that contain
    the properties that can be created, and the properties that can be updated,
    respectively. If the resource exists, the input property values are
    compared with the existing resource property values and the returned set
    of properties is the minimal set of properties that need to be changed.

    - Underscores in the property names are translated into hyphens.
    - The presence of read-only properties, invalid properties (i.e. not
      defined in the data model for password rules), and properties that are
      not allowed because of restrictions or because they are auto-created from
      an artificial property is surfaced by raising ParameterError.

    Parameters:

      pwrule (zhmcclient.PasswordRule): Password Rule object to be updated with
        the full set of current properties, or `None` if it did not previously
        exist.

      params (dict): Module input parameters.

    Returns:
      tuple of (create_props, update_props),
      where:
        * create_props: dict of properties for
          zhmcclient.PasswordRuleManager.create()
        * update_props: dict of properties for
          zhmcclient.PasswordRule.update_properties()

    Raises:
      ParameterError: An issue with the module parameters.
    """
    create_props = {}
    update_props = {}

    # handle 'name' property
    pwrule_name = to_unicode(params['name'])
    if pwrule is None:
        # Password Rule does not exist yet.
        create_props['name'] = pwrule_name
    else:
        # Password Rule does already exist.
        # We looked up the password rule by name, so we will never have to
        # update the password rule name.
        pass

    # handle the other properties
    input_props = params.get('properties', None)
    if input_props is None:
        input_props = {}

    for prop_name in input_props:

        if prop_name not in ZHMC_PASSWORD_RULE_PROPERTIES:
            raise ParameterError(
                "Property {0!r} is not defined in the data model for "
                "password rules.".format(prop_name))

        allowed, create, update, update_while_active, eq_func, type_cast = \
            ZHMC_PASSWORD_RULE_PROPERTIES[prop_name]

        if not allowed:
            raise ParameterError(
                "Property {0!r} is not allowed in the 'properties' module "
                "parameter.".format(prop_name))

        # Process a normal (= non-artificial) property
        _create_props, _update_props, _stop = process_normal_property(
            prop_name, ZHMC_PASSWORD_RULE_PROPERTIES, input_props, pwrule)
        create_props.update(_create_props)
        update_props.update(_update_props)
        if _stop:
            raise AssertionError()
    return create_props, update_props


def create_check_mode_pwrule(console, create_props, update_props):
    """
    Create and return a fake local Password Rule object.

    This is used when a password rule needs to be created in check mode.

    This function must be consistent with the behavior of the "Create Password
    Rule" operation on the HMC. HTTP errors the HMC would return are indicated
    by raising zhmcclient.HTTPError.
    """

    input_props = {}
    input_props.update(create_props)
    input_props.update(update_props)

    # Check required input properties
    missing_props = []
    for pname in ('name',):
        if pname not in input_props:
            missing_props.append(pname)
    if missing_props:
        raise zhmcclient.HTTPError({
            'http-status': 400,
            'reason': 4,
            'message': "Required input properties missing for Create Password Rule: {p}".
            format(p=missing_props),
        })

    # Defaults for properties
    props = {
        # createable/updateable
        'description': '',
        'expiration': 0,
        'min-length': 8,
        'max-length': 256,
        'consecutive-characters': 0,
        'similarity-count': 0,
        'history-count': 0,
        'case-sensitive': False,
        'character-rules': [],
        # read-only
        'type': 'user-defined',
        'replication-overwrite-possible': True,
    }

    # Apply specified input properties on top of the defaults
    props.update(input_props)

    pwrule_oid = 'fake-{0}'.format(uuid.uuid4())
    pwrule = console.password_rules.resource_object(pwrule_oid, props=props)

    return pwrule


def ensure_present(params, check_mode):
    """
    Ensure that the password rule exists and has the specified properties.

    Raises:
      ParameterError: An issue with the module parameters.
      zhmcclient.Error: Any zhmcclient exception can happen.
    """

    host = params['hmc_host']
    userid, password, ca_certs, verify = get_hmc_auth(params['hmc_auth'])
    pwrule_name = params['name']
    _faked_session = params.get('_faked_session', None)

    changed = False
    result = {}

    session = get_session(
        _faked_session, host, userid, password, ca_certs, verify)
    try:
        client = zhmcclient.Client(session)
        console = client.consoles.console
        # The default exception handling is sufficient for the above.

        try:
            pwrule = console.password_rules.find(name=pwrule_name)
        except zhmcclient.NotFound:
            pwrule = None

        if pwrule is None:
            # It does not exist. Create it and update it if there are
            # update-only properties.
            create_props, update_props = \
                process_properties(console, pwrule, params)
            update2_props = {}
            for name, value in update_props.items():
                if name not in create_props:
                    update2_props[name] = value
            if not check_mode:
                pwrule = console.password_rules.create(create_props)
                if update2_props:
                    pwrule.update_properties(update2_props)
                # We refresh the properties after the update, in case an
                # input property value gets changed.
                pwrule.pull_full_properties()
            else:
                # Create a Password Rule object locally
                pwrule = create_check_mode_pwrule(
                    console, create_props, update2_props)
            result = dict(pwrule.properties)
            changed = True
        else:
            # It exists. Update its properties.
            pwrule.pull_full_properties()
            result = dict(pwrule.properties)
            create_props, update_props = \
                process_properties(console, pwrule, params)
            if create_props:
                raise AssertionError("Unexpected "
                                     "create_props: %r" % create_props)
            if update_props:
                LOGGER.debug(
                    "Existing password rule %r needs to get properties "
                    "updated: %r", pwrule_name, update_props)
                if not check_mode:
                    pwrule.update_properties(update_props)
                    # We refresh the properties after the update, in case an
                    # input property value gets changed.
                    pwrule.pull_full_properties()
                    result = dict(pwrule.properties)
                else:
                    # Update the local Password Rule object's properties
                    result.update(update_props)
                changed = True

        if not pwrule:
            raise AssertionError()

        return changed, result

    finally:
        session.logoff()


def ensure_absent(params, check_mode):
    """
    Ensure that the password rule does not exist.

    Raises:
      ParameterError: An issue with the module parameters.
      zhmcclient.Error: Any zhmcclient exception can happen.
    """

    host = params['hmc_host']
    userid, password, ca_certs, verify = get_hmc_auth(params['hmc_auth'])
    pwrule_name = params['name']
    _faked_session = params.get('_faked_session', None)

    changed = False
    result = {}

    session = get_session(
        _faked_session, host, userid, password, ca_certs, verify)
    try:
        client = zhmcclient.Client(session)
        console = client.consoles.console
        # The default exception handling is sufficient for the above.

        try:
            pwrule = console.password_rules.find(name=pwrule_name)
        except zhmcclient.NotFound:
            return changed, result

        if not check_mode:
            pwrule.delete()
        changed = True

        return changed, result

    finally:
        session.logoff()


def facts(params, check_mode):
    """
    Return facts about a password rule.

    Raises:
      ParameterError: An issue with the module parameters.
      zhmcclient.Error: Any zhmcclient exception can happen.
    """

    host = params['hmc_host']
    userid, password, ca_certs, verify = get_hmc_auth(params['hmc_auth'])
    pwrule_name = params['name']
    _faked_session = params.get('_faked_session', None)

    changed = False
    result = {}

    session = get_session(
        _faked_session, host, userid, password, ca_certs, verify)
    try:
        # The default exception handling is sufficient for this code
        client = zhmcclient.Client(session)
        console = client.consoles.console

        pwrule = console.password_rules.find(name=pwrule_name)
        pwrule.pull_full_properties()

        result = dict(pwrule.properties)
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
        "Module exit (success): changed: %r, password_rule: %r",
        changed, result)
    module.exit_json(changed=changed, password_rule=result)


if __name__ == '__main__':
    main()
