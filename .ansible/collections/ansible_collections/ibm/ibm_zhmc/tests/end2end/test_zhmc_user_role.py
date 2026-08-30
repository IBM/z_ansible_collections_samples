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

"""
End2end tests for zhmc_user_role module.
"""

from __future__ import (absolute_import, division, print_function)
__metaclass__ = type

import uuid
import pytest
import mock
import random
from pprint import pformat
import requests.packages.urllib3
import zhmcclient
# pylint: disable=line-too-long,unused-import
from zhmcclient.testutils import hmc_definition, hmc_session  # noqa: F401, E501
# pylint: enable=line-too-long,unused-import

from plugins.modules import zhmc_user_role
from .utils import mock_ansible_module, get_failure_msg

requests.packages.urllib3.disable_warnings()

# Print debug messages
DEBUG = False

LOG_FILE = 'zhmc_user_role.log' if DEBUG else None

# Properties in the returned user role facts that are not always present, but
# only under certain conditions. This includes artificial properties whose base
# properties are not always present. The
# comments state the condition under which the property is present.
UROLE_CONDITIONAL_PROPS = (
)


# A standard test user role, as the Ansible module input properties (i.e. using
# underscores, and limited to valid input parameters)
STD_UROLE_MODULE_INPUT_PROPS = {
    # 'name': provided in separate module input parameter
    'description': "zhmc test user role",
    'associated_system_defined_user_role_name': None,  # updated upon use
    # Note: 'is-inheritance-enabled' is not provided for now due to a defect
}


# A standard test user role, as the input properties for
# UserRoleManager.create() (i.e. using dashes, and limited to valid input
# parameters)
STD_UROLE_HMC_INPUT_PROPS = {
    # 'name': updated upon use
    'description': "zhmc test user role",
    'associated-system-defined-user-role-uri': None,  # updated upon use
    # Note: 'is-inheritance-enabled' is not provided for now due to a defect
}


def updated_copy(dict1, dict2):
    dict1c = dict1.copy()
    dict1c.update(dict2)
    return dict1c


def new_urole_name():
    urole_name = 'test_{0}'.format(uuid.uuid4())
    return urole_name


def get_module_output(mod_obj):
    """
    Return the module output as a tuple (changed, user_properties) (i.e.
    the arguments of the call to exit_json()).
    If the module failed, return None.
    """

    def func(changed, user_role):
        return changed, user_role

    if not mod_obj.exit_json.called:
        return None
    call_args = mod_obj.exit_json.call_args

    # The following makes sure we get the arguments regardless of whether they
    # were specified as positional or keyword arguments:
    return func(*call_args[0], **call_args[1])


def assert_urole_props(urole_props, exp_urole_props, where):
    """
    Assert the properties of the output object of the zhmc_user_role module
    """

    assert isinstance(urole_props, dict), where  # Dict of User role props

    # Assert presence of properties in the output
    for prop_name in zhmc_user_role.ZHMC_USER_ROLE_PROPERTIES:
        prop_name_hmc = prop_name.replace('_', '-')
        if prop_name_hmc in UROLE_CONDITIONAL_PROPS:
            continue
        where_prop = where + ", property {p!r}".format(p=prop_name)
        assert prop_name_hmc in urole_props, where_prop

    # Assert the expected property values for non-artificial properties
    artificial_prop_names = (
        'permissions',
        'associated-system-defined-user-role-name',
    )
    for prop_name in exp_urole_props:
        if prop_name in artificial_prop_names:
            continue
        exp_value = exp_urole_props[prop_name]
        act_value = urole_props[prop_name]
        where_prop = where + ", property {p!r}".format(p=prop_name)
        assert act_value == exp_value, where_prop


@pytest.mark.parametrize(
    "check_mode", [
        pytest.param(False, id="check_mode=False"),
        pytest.param(True, id="check_mode=True"),
    ]
)
@mock.patch("plugins.modules.zhmc_user_role.AnsibleModule", autospec=True)
def test_zhmc_user_role_facts(
        ansible_mod_cls, check_mode, hmc_session):  # noqa: F811, E501
    """
    Test the zhmc_user_role module with state=facts.
    """

    hd = hmc_session.hmc_definition
    hmc_host = hd.host
    hmc_auth = dict(userid=hd.userid, password=hd.password,
                    ca_certs=hd.ca_certs, verify=hd.verify)

    client = zhmcclient.Client(hmc_session)
    console = client.consoles.console

    faked_session = hmc_session if hd.mock_file else None

    # Determine a random existing user role of the desired type to test.
    uroles = console.user_roles.list()
    urole = random.choice(uroles)

    where = "user role '{u}'".format(u=urole.name)

    # Prepare module input parameters (must be all required + optional)
    params = {
        'hmc_host': hmc_host,
        'hmc_auth': hmc_auth,
        'name': urole.name,
        'state': 'facts',
        'log_file': LOG_FILE,
        '_faked_session': faked_session,
    }

    # Prepare mocks for AnsibleModule object
    mod_obj = mock_ansible_module(ansible_mod_cls, params, check_mode)

    # Exercise the code to be tested
    with pytest.raises(SystemExit) as exc_info:
        zhmc_user_role.main()
    exit_code = exc_info.value.args[0]

    # Assert module exit code
    assert exit_code == 0, \
        "{w}: Module failed with exit code {e} and message:\n{m}". \
        format(w=where, e=exit_code, m=get_failure_msg(mod_obj))

    # Assert module output
    changed, urole_props = get_module_output(mod_obj)
    assert changed is False, where
    assert_urole_props(urole_props, urole.properties, where)


UROLE_ABSENT_PRESENT_TESTCASES = [
    # The list items are tuples with the following items:
    # - desc (string): description of the testcase.
    # - initial_urole_props (dict): HMC-formatted properties for initial
    #    user role, in addition to STD_UROLE_HMC_INPUT_PROPS, or None for no
    #    initial user role.
    # - input_props (dict): 'properties' input parameter for zhmc_user_role
    #   module.
    # - exp_urole_props (dict): HMC-formatted properties for expected
    #   properties of created user role.
    # - exp_changed (bool): Boolean for expected 'changed' flag.

    (
        "Present with non-existing user role",
        None,
        'present',
        STD_UROLE_MODULE_INPUT_PROPS,
        STD_UROLE_HMC_INPUT_PROPS,
        True,
    ),
    (
        "Present with existing user role, no properties changed",
        {},
        'present',
        None,
        STD_UROLE_HMC_INPUT_PROPS,
        False,
    ),
    (
        "Present with existing user role, some properties changed",
        {
            'description': 'bla',
        },
        'present',
        STD_UROLE_MODULE_INPUT_PROPS,
        STD_UROLE_HMC_INPUT_PROPS,
        True,
    ),
    (
        "Absent with existing user role",
        {},
        'absent',
        None,
        None,
        True,
    ),
    (
        "Absent with non-existing user role",
        None,
        'absent',
        None,
        None,
        False,
    ),
]


@pytest.mark.parametrize(
    "check_mode", [
        pytest.param(False, id="check_mode=False"),
        pytest.param(True, id="check_mode=True"),
    ]
)
@pytest.mark.parametrize(
    "desc, initial_urole_props, input_state, "
    "input_props, exp_urole_props, exp_changed",
    UROLE_ABSENT_PRESENT_TESTCASES)
@mock.patch("plugins.modules.zhmc_user_role.AnsibleModule", autospec=True)
def test_zhmc_user_role_absent_present(
        ansible_mod_cls,
        desc, initial_urole_props, input_state,
        input_props, exp_urole_props, exp_changed,
        check_mode,
        hmc_session):  # noqa: F811, E501
    """
    Test the zhmc_user_role module with state=absent/present.
    """

    hd = hmc_session.hmc_definition
    hmc_host = hd.host
    hmc_auth = dict(userid=hd.userid, password=hd.password,
                    ca_certs=hd.ca_certs, verify=hd.verify)

    faked_session = hmc_session if hd.mock_file else None

    client = zhmcclient.Client(hmc_session)
    console = client.consoles.console

    # Create a user role name that does not exist
    urole_name = new_urole_name()

    where = "user role '{u}'".format(u=urole_name)

    sys_urole = console.user_roles.find(name='hmc-access-administrator-tasks')

    if input_props is not None:
        input_props2 = input_props.copy()
    else:
        input_props2 = None

    if exp_urole_props is not None:
        exp_urole_props2 = exp_urole_props.copy()
        exp_urole_props2['associated-system-defined-user-role-uri'] = \
            sys_urole.uri
    else:
        exp_urole_props2 = None

    # Create initial user role, if specified so
    if initial_urole_props is not None:
        urole_props = STD_UROLE_HMC_INPUT_PROPS.copy()
        urole_props.update(initial_urole_props)
        urole_props['name'] = urole_name
        urole_props['associated-system-defined-user-role-uri'] = sys_urole.uri

        try:
            console.user_roles.create(urole_props)
        except zhmcclient.HTTPError as exc:
            if exc.http_status == 403 and exc.reason == 1:
                # User is not permitted to create user roles
                pytest.skip("HMC user '{u}' is not permitted to create "
                            "initial test user role".
                            format(u=hd.userid))
            else:
                raise
    else:
        urole_props = None

    try:

        # Prepare module input parameters (must be all required + optional)
        params = {
            'hmc_host': hmc_host,
            'hmc_auth': hmc_auth,
            'name': urole_name,
            'state': input_state,
            'log_file': LOG_FILE,
            '_faked_session': faked_session,
        }
        if input_props2 is not None:
            if not input_props2['associated_system_defined_user_role_name']:
                input_props2['associated_system_defined_user_role_name'] = \
                    sys_urole.name
            params['properties'] = input_props2
        else:
            params['properties'] = {}

        mod_obj = mock_ansible_module(ansible_mod_cls, params, check_mode)

        # Exercise the code to be tested
        with pytest.raises(SystemExit) as exc_info:
            zhmc_user_role.main()
        exit_code = exc_info.value.args[0]

        if exit_code != 0:
            msg = get_failure_msg(mod_obj)
            if msg.startswith('HTTPError: 403,1'):
                pytest.skip("HMC user '{u}' is not permitted to create "
                            "test user role".
                            format(u=hd.userid))
            raise AssertionError(
                "{w}: Module failed with exit code {e} and message:\n{m}".
                format(w=where, e=exit_code, m=msg))

        changed, output_props = get_module_output(mod_obj)
        if changed != exp_changed:
            urole_props_sorted = \
                dict(sorted(urole_props.items(), key=lambda x: x[0])) \
                if urole_props is not None else None
            input_props_sorted = \
                dict(sorted(input_props2.items(), key=lambda x: x[0])) \
                if input_props2 is not None else None
            output_props_sorted = \
                dict(sorted(output_props.items(), key=lambda x: x[0])) \
                if output_props is not None else None
            raise AssertionError(
                "Unexpected change flag returned: actual: {0}, expected: {1}\n"
                "Initial user role properties:\n{2}\n"
                "Module input properties:\n{3}\n"
                "Resulting user role properties:\n{4}".
                format(changed, exp_changed,
                       pformat(urole_props_sorted, indent=2),
                       pformat(input_props_sorted, indent=2),
                       pformat(output_props_sorted, indent=2)))
        if input_state == 'present':
            assert_urole_props(output_props, exp_urole_props2, where)

    finally:
        # Delete user role, if it exists
        try:
            # We invalidate the name cache of our client, because the user role
            # was possibly deleted by the Ansible module and not through our
            # client instance.
            console.user_roles.invalidate_cache()
            urole = console.user_roles.find_by_name(urole_name)
        except zhmcclient.NotFound:
            urole = None
        if urole:
            urole.delete()
