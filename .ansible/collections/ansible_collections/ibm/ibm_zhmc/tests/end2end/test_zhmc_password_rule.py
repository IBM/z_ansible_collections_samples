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
End2end tests for zhmc_password_rule module.
"""

from __future__ import (absolute_import, division, print_function)
__metaclass__ = type

import uuid
import pytest
import mock
import random
import requests.packages.urllib3
from collections import OrderedDict
from pprint import pformat
import zhmcclient
# pylint: disable=line-too-long,unused-import
from zhmcclient.testutils import hmc_definition, hmc_session  # noqa: F401, E501
# pylint: enable=line-too-long,unused-import

from plugins.modules import zhmc_password_rule
from .utils import mock_ansible_module, get_failure_msg

requests.packages.urllib3.disable_warnings()

# Print debug messages
DEBUG = False

LOG_FILE = 'zhmc_password_rule.log' if DEBUG else None

# Properties in the returned password rule facts that are not always present, but
# only under certain conditions. This includes artificial properties whose base
# properties are not always present. The
# comments state the condition under which the property is present.
PWRULE_CONDITIONAL_PROPS = (
)


# A standard test password rule, as specified for the 'properties' module input
# parm
STD_PWRULE_INPUT_PROPERTIES = {
    # 'name': provided in separate module input parameter
    'description': "zhmc test password rule",
    'expiration': 90,
    'min_length': 16,
    'max_length': 32,
    'consecutive_characters': 0,
    'similarity_count': 2,
    'history_count': 2,
    'case_sensitive': True,
    'character_rules': [],
}


# A standard test password rule consistent with STD_PWRULE_INPUT_PROPERTIES, but
# specified with HMC properties.
STD_PWRULE_PROPERTIES = {
    # 'name': updated upon use
    # 'default-group-uri': no default group
    'description': STD_PWRULE_INPUT_PROPERTIES['description'],
    'expiration': STD_PWRULE_INPUT_PROPERTIES['expiration'],
    'min-length': STD_PWRULE_INPUT_PROPERTIES['min_length'],
    'max-length': STD_PWRULE_INPUT_PROPERTIES['max_length'],
    'consecutive-characters': STD_PWRULE_INPUT_PROPERTIES['consecutive_characters'],
    'similarity-count': STD_PWRULE_INPUT_PROPERTIES['similarity_count'],
    'history-count': STD_PWRULE_INPUT_PROPERTIES['history_count'],
    'character-rules': STD_PWRULE_INPUT_PROPERTIES['character_rules'],
}


def updated_copy(dict1, dict2):
    dict1c = dict1.copy()
    dict1c.update(dict2)
    return dict1c


def new_pwrule_name():
    pwrule_name = 'test_{0}'.format(uuid.uuid4())
    return pwrule_name


def get_module_output(mod_obj):
    """
    Return the module output as a tuple (changed, user_properties) (i.e.
    the arguments of the call to exit_json()).
    If the module failed, return None.
    """

    def func(changed, password_rule):
        return changed, password_rule

    if not mod_obj.exit_json.called:
        return None
    call_args = mod_obj.exit_json.call_args

    # The following makes sure we get the arguments regardless of whether they
    # were specified as positional or keyword arguments:
    return func(*call_args[0], **call_args[1])


def assert_pwrule_props(pwrule_props, where):
    """
    Assert the output object of the zhmc_password_rule module
    """
    assert isinstance(pwrule_props, dict), where  # Dict of Password rule props

    # Assert presence of normal properties in the output
    for prop_name in zhmc_password_rule.ZHMC_PASSWORD_RULE_PROPERTIES:
        prop_name_hmc = prop_name.replace('_', '-')
        if prop_name_hmc in PWRULE_CONDITIONAL_PROPS:
            continue
        assert prop_name_hmc in pwrule_props, where


@pytest.mark.parametrize(
    "check_mode", [
        pytest.param(False, id="check_mode=False"),
        pytest.param(True, id="check_mode=True"),
    ]
)
@mock.patch("plugins.modules.zhmc_password_rule.AnsibleModule", autospec=True)
def test_zhmc_password_rule_facts(
        ansible_mod_cls, check_mode, hmc_session):  # noqa: F811, E501
    """
    Test the zhmc_password_rule module with state=facts.
    """

    hd = hmc_session.hmc_definition
    hmc_host = hd.host
    hmc_auth = dict(userid=hd.userid, password=hd.password,
                    ca_certs=hd.ca_certs, verify=hd.verify)

    client = zhmcclient.Client(hmc_session)
    console = client.consoles.console

    faked_session = hmc_session if hd.mock_file else None

    # Determine a random existing password rule of the desired type to test.
    pwrules = console.password_rules.list()
    pwrule = random.choice(pwrules)

    where = "password rule '{u}'".format(u=pwrule.name)

    # Prepare module input parameters (must be all required + optional)
    params = {
        'hmc_host': hmc_host,
        'hmc_auth': hmc_auth,
        'name': pwrule.name,
        'state': 'facts',
        'properties': {},
        'log_file': LOG_FILE,
        '_faked_session': faked_session,
    }

    # Prepare mocks for AnsibleModule object
    mod_obj = mock_ansible_module(ansible_mod_cls, params, check_mode)

    # Exercise the code to be tested
    with pytest.raises(SystemExit) as exc_info:
        zhmc_password_rule.main()
    exit_code = exc_info.value.args[0]

    # Assert module exit code
    assert exit_code == 0, \
        "{w}: Module failed with exit code {e} and message:\n{m}". \
        format(w=where, e=exit_code, m=get_failure_msg(mod_obj))

    # Assert module output
    changed, pwrule_props = get_module_output(mod_obj)
    assert changed is False, where
    assert_pwrule_props(pwrule_props, where)


PWRULE_ABSENT_PRESENT_TESTCASES = [
    # The list items are tuples with the following items:
    # - desc (string): description of the testcase.
    # - initial_pwrule_props (dict): HMC-formatted properties for initial
    #    password rule, in addition to STD_PWRULE_PROPERTIES, or None for no
    #    initial password rule.
    # - input_props (dict): 'properties' input parameter for zhmc_password_rule
    #   module.
    # - exp_pwrule_props (dict): HMC-formatted properties for expected
    #   properties of created password rule.
    # - exp_changed (bool): Boolean for expected 'changed' flag.

    (
        "Present with non-existing password rule",
        None,
        'present',
        STD_PWRULE_INPUT_PROPERTIES,
        STD_PWRULE_PROPERTIES,
        True,
    ),
    (
        "Present with existing password rule, no properties changed",
        {},
        'present',
        STD_PWRULE_INPUT_PROPERTIES,
        STD_PWRULE_PROPERTIES,
        True,  # due to password
    ),
    (
        "Present with existing password rule, some properties changed",
        {
            'session-timeout': 30,
        },
        'present',
        STD_PWRULE_INPUT_PROPERTIES,
        STD_PWRULE_PROPERTIES,
        True,
    ),
    (
        "Absent with existing password rule",
        {},
        'absent',
        None,
        None,
        True,
    ),
    (
        "Absent with non-existing password rule",
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
    "desc, initial_pwrule_props, input_state, "
    "input_props, exp_pwrule_props, exp_changed",
    PWRULE_ABSENT_PRESENT_TESTCASES)
@mock.patch("plugins.modules.zhmc_password_rule.AnsibleModule", autospec=True)
def test_zhmc_password_rule_absent_present(
        ansible_mod_cls,
        desc, initial_pwrule_props, input_state,
        input_props, exp_pwrule_props, exp_changed,
        check_mode,
        hmc_session):  # noqa: F811, E501
    """
    Test the zhmc_password_rule module with state=absent/present.
    """

    hd = hmc_session.hmc_definition
    hmc_host = hd.host
    hmc_auth = dict(userid=hd.userid, password=hd.password,
                    ca_certs=hd.ca_certs, verify=hd.verify)

    faked_session = hmc_session if hd.mock_file else None

    client = zhmcclient.Client(hmc_session)
    console = client.consoles.console

    # Create a password rule name that does not exist
    pwrule_name = new_pwrule_name()

    where = "password rule '{u}'".format(u=pwrule_name)

    # Create initial password rule, if specified so
    if initial_pwrule_props is not None:
        pwrule_props = STD_PWRULE_PROPERTIES.copy()
        pwrule_props.update(initial_pwrule_props)
        pwrule_props['name'] = pwrule_name
        try:
            console.password_rules.create(pwrule_props)
        except zhmcclient.HTTPError as exc:
            if exc.http_status == 403 and exc.reason == 1:
                # User is not permitted to create password rules
                pytest.skip("HMC user '{u}' is not permitted to create "
                            "initial test password rule".
                            format(u=hd.userid))
    else:
        pwrule_props = None

    try:

        # Prepare module input parameters (must be all required + optional)
        params = {
            'hmc_host': hmc_host,
            'hmc_auth': hmc_auth,
            'name': pwrule_name,
            'state': input_state,
            'log_file': LOG_FILE,
            '_faked_session': faked_session,
        }
        if input_props is not None:
            params['properties'] = input_props
        else:
            params['properties'] = {}

        mod_obj = mock_ansible_module(ansible_mod_cls, params, check_mode)

        # Exercise the code to be tested
        with pytest.raises(SystemExit) as exc_info:
            zhmc_password_rule.main()
        exit_code = exc_info.value.args[0]

        assert exit_code == 0, \
            "{w}: Module failed with exit code {e} and message:\n{m}". \
            format(w=where, e=exit_code, m=get_failure_msg(mod_obj))

        changed, output_props = get_module_output(mod_obj)
        if changed != exp_changed:
            pwrule_props_sorted = \
                OrderedDict(sorted(pwrule_props.items(), key=lambda x: x[0])) \
                if pwrule_props is not None else None
            input_props_sorted = \
                OrderedDict(sorted(input_props.items(), key=lambda x: x[0])) \
                if input_props is not None else None
            output_props_sorted = \
                OrderedDict(sorted(output_props.items(), key=lambda x: x[0])) \
                if output_props is not None else None
            raise AssertionError(
                "Unexpected change flag returned: actual: {0}, expected: {1}\n"
                "Initial password rule properties:\n{2}\n"
                "Module input properties:\n{3}\n"
                "Resulting password rule properties:\n{4}".
                format(changed, exp_changed,
                       pformat(pwrule_props_sorted.items(), indent=2),
                       pformat(input_props_sorted.items(), indent=2),
                       pformat(output_props_sorted.items(), indent=2)))
        if input_state == 'present':
            assert_pwrule_props(output_props, where)

    finally:
        # Delete password rule, if it exists
        try:
            # We invalidate the name cache of our client, because the password rule
            # was possibly deleted by the Ansible module and not through our
            # client instance.
            console.password_rules.invalidate_cache()
            pwrule = console.password_rules.find_by_name(pwrule_name)
        except zhmcclient.NotFound:
            pwrule = None
        if pwrule:
            pwrule.delete()
