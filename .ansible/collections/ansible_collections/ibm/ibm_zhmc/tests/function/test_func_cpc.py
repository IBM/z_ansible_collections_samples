#!/usr/bin/env python
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
Function tests for the 'zhmc_cpc' Ansible module.
"""

# pylint: disable=bad-option-value,redundant-u-string-prefix

from __future__ import (absolute_import, division, print_function)
__metaclass__ = type

import pytest
import mock
import re

from zhmcclient import Client
from zhmcclient_mock import FakedSession

from plugins.modules import zhmc_cpc

from .func_utils import mock_ansible_module

# Faked Console that is used for all tests
# (with property names as specified in HMC data model)
FAKED_CONSOLE_URI = '/api/console'
FAKED_CONSOLE_NAME = 'HMC1'
FAKED_CONSOLE_HMC_VERSION = '2.14.0'
FAKED_CONSOLE_API_VERSION = '2.20'
FAKED_CONSOLE = {
    'object-uri': FAKED_CONSOLE_URI,
    'class': 'console',
    'name': FAKED_CONSOLE_NAME,
    'description': 'Console HMC1',
    'version': FAKED_CONSOLE_HMC_VERSION,
}

# FakedSession() init arguments
FAKED_SESSION_KWARGS = dict(
    host='fake-host',
    hmc_name=FAKED_CONSOLE_NAME,
    hmc_version=FAKED_CONSOLE_HMC_VERSION,
    api_version=FAKED_CONSOLE_API_VERSION,
)

# Faked CPC in classic mode that is used for all tests
# (with property names as specified in HMC data model)
FAKED_CPC_1_NAME = 'CPC1'
FAKED_CPC_1_OID = 'fake-cpc-1'
FAKED_CPC_1_URI = '/api/cpcs/' + FAKED_CPC_1_OID
FAKED_CPC_1 = {
    'object-id': FAKED_CPC_1_OID,
    'object-uri': FAKED_CPC_1_URI,
    'class': 'cpc',
    'name': FAKED_CPC_1_NAME,
    'description': 'CPC #1 in classic mode',
    'status': 'operating',  # Will be overridden in test setup
    'acceptable-status': ['operating', 'degraded'],
    'dpm-enabled': False,
    'is-ensemble-member': False,
    'iml-mode': 'lpar',
}


# Faked CPC in DPM mode that is used for all tests
# (with property names as specified in HMC data model)
FAKED_CPC_2_NAME = 'CPC2'
FAKED_CPC_2_OID = 'fake-cpc-2'
FAKED_CPC_2_URI = '/api/cpcs/' + FAKED_CPC_2_OID
FAKED_CPC_2 = {
    'object-id': FAKED_CPC_2_OID,
    'object-uri': FAKED_CPC_2_URI,
    'class': 'cpc',
    'name': FAKED_CPC_2_NAME,
    'description': 'CPC #2 in DPM mode',
    'status': 'active',  # Will be overridden in test setup
    'acceptable-status': ['active', 'degraded'],
    'dpm-enabled': True,
    'is-ensemble-member': False,
    'iml-mode': 'lpar',
}


def get_failure_msg(mod_obj):
    """
    Return the module failure message, as a string (i.e. the 'msg' argument
    of the call to fail_json()).
    If the module succeeded, return None.
    """

    def func(msg):
        return msg

    if not mod_obj.fail_json.called:
        return None
    call_args = mod_obj.fail_json.call_args

    # The following makes sure we get the arguments regardless of whether they
    # were specified as positional or keyword arguments:
    return func(*call_args[0], **call_args[1])


def get_module_output(mod_obj):
    """
    Return the module output as a tuple (changed, cpc_properties) (i.e.
    the arguments of the call to exit_json()).
    If the module failed, return None.
    """

    def func(changed, cpc):
        return changed, cpc

    if not mod_obj.exit_json.called:
        return None
    call_args = mod_obj.exit_json.call_args

    # The following makes sure we get the arguments regardless of whether they
    # were specified as positional or keyword arguments:
    return func(*call_args[0], **call_args[1])


# Test cases for test_cpc_state()
TEST_CPC_STATE_TESTCASES = [

    # CPC in classic mode with initial status 'operating'
    dict(
        title="CPC classic mode / operating / no properties / state=inactive",
        faked_cpc=FAKED_CPC_1,
        faked_cpc_properties={
            'status': 'operating',
        },
        input_properties=None,
        input_state='inactive',
        exp_exit_code=0,
        exp_message_pattern=None,
        exp_cpc_properties={
            'status': 'not-operating',
        },
        exp_changed=True,
    ),
    dict(
        title="CPC classic mode / operating / properties (ignored) / state=inactive",
        faked_cpc=FAKED_CPC_1,
        faked_cpc_properties={
            'status': 'operating',
        },
        input_properties={
            'acceptable_status': ['operating', 'exceptions'],
        },
        input_state='inactive',
        exp_exit_code=0,
        exp_message_pattern=None,
        exp_cpc_properties={
            'status': 'not-operating',
        },
        exp_changed=True,
    ),
    dict(
        title="CPC classic mode / operating / no properties / state=active",
        faked_cpc=FAKED_CPC_1,
        faked_cpc_properties={
            'status': 'operating',
        },
        input_properties=None,
        input_state='active',
        exp_exit_code=0,
        exp_message_pattern=None,
        exp_cpc_properties={
            'status': 'operating',
        },
        exp_changed=False,
    ),
    dict(
        title="CPC classic mode / operating / no properties / state=set",
        faked_cpc=FAKED_CPC_1,
        faked_cpc_properties={
            'status': 'operating',
        },
        input_properties=None,
        input_state='set',
        exp_exit_code=0,
        exp_message_pattern=None,
        exp_cpc_properties={
            'status': 'operating',
        },
        exp_changed=False,
    ),
    dict(
        title="CPC classic mode / operating / properties / state=set",
        faked_cpc=FAKED_CPC_1,
        faked_cpc_properties={
            'status': 'operating',
        },
        input_properties={
            'acceptable_status': ['operating', 'exceptions'],
        },
        input_state='set',
        exp_exit_code=0,
        exp_message_pattern=None,
        exp_cpc_properties={
            'status': 'operating',
            'acceptable_status': ['operating', 'exceptions'],
        },
        exp_changed=True,
    ),

    # CPC in classic mode with state=set and different input properties
    dict(
        title="CPC classic mode / state=set / properties=None",
        faked_cpc=FAKED_CPC_1,
        faked_cpc_properties={
            'status': 'operating',
        },
        input_properties=None,
        input_state='set',
        exp_exit_code=0,
        exp_message_pattern=None,
        exp_cpc_properties={
            'status': 'operating',
        },
        exp_changed=False,
    ),
    dict(
        title="CPC classic mode / state=set / properties={}",
        faked_cpc=FAKED_CPC_1,
        faked_cpc_properties={
            'status': 'operating',
        },
        input_properties={},
        input_state='set',
        exp_exit_code=0,
        exp_message_pattern=None,
        exp_cpc_properties={
            'status': 'operating',
        },
        exp_changed=False,
    ),
    dict(
        title="CPC classic mode / state=set / properties={acceptable_status}",
        faked_cpc=FAKED_CPC_1,
        faked_cpc_properties={
            'status': 'operating',
        },
        input_properties={
            'acceptable_status': ['operating', 'exceptions'],
        },
        input_state='set',
        exp_exit_code=0,
        exp_message_pattern=None,
        exp_cpc_properties={
            'status': 'operating',
            'acceptable_status': ['operating', 'exceptions'],
        },
        exp_changed=True,
    ),
    dict(
        title="CPC classic mode / state=set / properties=(all modifiable)",
        faked_cpc=FAKED_CPC_1,
        faked_cpc_properties={
            'status': 'operating',
        },
        input_properties={
            'acceptable_status': ['operating', 'exceptions'],
            'next_activation_profile_name': 'faked_rp',
            'processor_running_time_type': 'user-determined',
            'processor_running_time': 42,
        },
        input_state='set',
        exp_exit_code=0,
        exp_message_pattern=None,
        exp_cpc_properties={
            'status': 'operating',
            'acceptable_status': ['operating', 'exceptions'],
            'next_activation_profile_name': 'faked_rp',
            'processor_running_time_type': 'user-determined',
            'processor_running_time': 42,
        },
        exp_changed=True,
    ),
    dict(
        title="CPC classic mode / state=set / properties=(a read-only)",
        faked_cpc=FAKED_CPC_1,
        faked_cpc_properties={
            'status': 'operating',
        },
        input_properties={
            'last_used_activation_profile': 'faked_rp',
        },
        input_state='set',
        exp_exit_code=1,
        exp_message_pattern=None,
        exp_cpc_properties={
            'status': 'operating',
        },
        exp_changed=False,
    ),
    dict(
        title="CPC classic mode / state=facts / no properties",
        faked_cpc=FAKED_CPC_1,
        faked_cpc_properties={
            'status': 'operating',
        },
        input_properties={},
        input_state='facts',
        exp_exit_code=0,
        exp_message_pattern=None,
        exp_cpc_properties={
            'name': FAKED_CPC_1_NAME,
            'status': 'operating',
        },
        exp_changed=False,
    ),
    dict(
        title="CPC classic mode / state=facts / properties=(modifiable/ignored)",
        faked_cpc=FAKED_CPC_1,
        faked_cpc_properties={
            'status': 'operating',
        },
        input_properties={
            'acceptable_status': ['operating', 'exceptions'],
        },
        input_state='facts',
        exp_exit_code=0,
        exp_message_pattern=None,
        exp_cpc_properties={
            'name': FAKED_CPC_1_NAME,
            'status': 'operating',
            'acceptable_status': FAKED_CPC_1['acceptable-status']  # unchanged
        },
        exp_changed=False,
    ),
]


class TestCpc(object):
    """
    All tests for CPCs.
    """

    def setup_method(self):
        """
        Using the zhmcclient mock support, set up a CPC in classic mode.
        """
        self.session = FakedSession(**FAKED_SESSION_KWARGS)
        self.client = Client(self.session)
        self.console = self.session.hmc.consoles.add(FAKED_CONSOLE)

    @pytest.mark.parametrize(
        "check_mode", [False, True])
    @pytest.mark.parametrize(
        "testcases", TEST_CPC_STATE_TESTCASES)
    @mock.patch("plugins.modules.zhmc_cpc.AnsibleModule", autospec=True)
    def test_cpc_state(self, ansible_mod_cls, testcases, check_mode):
        """
        Tests for different state values.
        """

        # Setup the faked CPC properties from the testcase
        faked_cpc = dict(testcases['faked_cpc'])
        faked_cpc.update(testcases['faked_cpc_properties'])

        # Get module input parameters from the testcase
        input_properties = testcases['input_properties']
        input_state = testcases['input_state']

        # Get expected values from testcase
        exp_exit_code = testcases['exp_exit_code']
        exp_message_pattern = testcases['exp_message_pattern']
        exp_cpc_properties = testcases['exp_cpc_properties']
        exp_changed = testcases['exp_changed']

        # Create the faked CPC
        self.faked_cpc = self.session.hmc.cpcs.add(faked_cpc)
        cpcs = self.client.cpcs.list()
        assert len(cpcs) == 1
        self.cpc = cpcs[0]
        self.cpc.pull_full_properties()

        # Prepare module input parameters
        params = {
            'hmc_host': 'fake-host',
            'hmc_auth': dict(userid='fake-userid',
                             password='fake-password'),
            'name': self.cpc.name,
            'state': input_state,
            'properties': input_properties,
            'log_file': None,
            '_faked_session': self.session,
        }

        # Prepare mocks for AnsibleModule object
        mod_obj = mock_ansible_module(ansible_mod_cls, params, check_mode)

        # Exercise the code to be tested
        with pytest.raises(SystemExit) as exc_info:
            zhmc_cpc.main()
        exit_code = exc_info.value.args[0]
        message = get_failure_msg(mod_obj)

        # Assert module exit code
        assert exit_code == exp_exit_code, \
            "Unexpected module exit code {0} (expected {1}), message:\n{2}". \
            format(exit_code, exp_exit_code, message)

        if exp_exit_code != 0 and exp_message_pattern:
            # Assert module failure message
            assert re.match(exp_message_pattern, message), \
                "Unexpected module failure message:\n" \
                "  {0}\n" \
                "Expected pattern:\n" \
                "  {1}". \
                format(message, exp_message_pattern)

        if exp_exit_code == 0:

            # Assert module output
            changed, cpc_props = get_module_output(mod_obj)
            assert changed == exp_changed
            if input_state == 'inactive':
                assert cpc_props is None
            else:
                if exp_cpc_properties:
                    for ansi_name, exp_value in exp_cpc_properties.items():
                        hmc_name = ansi_name.replace('_', '-')
                        assert hmc_name in cpc_props
                        assert exp_value == cpc_props[hmc_name], \
                            "Unexpected value for property {0!r}". \
                            format(hmc_name)

            # Assert the updated CPC resource
            if input_state in ('active', 'set') and not check_mode:
                if exp_cpc_properties:
                    self.cpc.pull_full_properties()
                    for ansi_name, exp_value in exp_cpc_properties.items():
                        hmc_name = ansi_name.replace('_', '-')
                        assert hmc_name in self.cpc.properties
                        assert exp_value == self.cpc.properties[hmc_name], \
                            "Unexpected value for property {0!r}". \
                            format(hmc_name)
