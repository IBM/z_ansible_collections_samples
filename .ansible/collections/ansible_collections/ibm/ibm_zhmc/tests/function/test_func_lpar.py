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
Function tests for the 'zhmc_lpar' Ansible module.
"""

# pylint: disable=bad-option-value,redundant-u-string-prefix

from __future__ import (absolute_import, division, print_function)
__metaclass__ = type

import pytest
import mock

from zhmcclient import Client
from zhmcclient_mock import FakedSession

from plugins.modules import zhmc_lpar

from .func_utils import mock_ansible_module

# FakedSession() init arguments
FAKED_SESSION_KWARGS = dict(
    host='fake-host',
    hmc_name='faked-hmc-name',
    hmc_version='2.13.1',
    api_version='1.8'
)

# Faked Console that is used for all tests
# (with property names as specified in HMC data model)
FAKED_CONSOLE_URI = '/api/console'
FAKED_CONSOLE = {
    'object-uri': FAKED_CONSOLE_URI,
    'class': 'console',
    'name': 'hmc-1',
    'description': 'Console HMC1',
    'version': '2.13.0',
}

# Faked CPC in classic mode that is used for all tests
# (with property names as specified in HMC data model)
FAKED_CPC_1_OID = 'fake-cpc-1'
FAKED_CPC_1_URI = '/api/cpcs/' + FAKED_CPC_1_OID
FAKED_CPC_1 = {
    'object-id': FAKED_CPC_1_OID,
    'object-uri': FAKED_CPC_1_URI,
    'class': 'cpc',
    'name': 'cpc-name-1',
    'description': 'CPC #1 in classic mode',
    'status': 'active',
    'dpm-enabled': False,
    'is-ensemble-member': False,
    'iml-mode': 'lpar',
}

# Faked LPAR that is used for these tests.
# The LPAR is an SSC type LPAR with properties from z14.
# The included properties are all standard properties (such as 'object-id'),
# a subset of the modifiable properties, and a subset of the read-only
# properties.
# The 'ssc*'' properties that had been removed with z14 are not included.
FAKED_LPAR_1_NAME = 'LPAR1'
FAKED_LPAR_1_PROFILE = FAKED_LPAR_1_NAME
FAKED_LPAR_1_OID = 'fake-lpar1'
FAKED_LPAR_1_URI = '/api/logical-partitions/' + FAKED_LPAR_1_OID
FAKED_LPAR_1_BASE = {
    'object-id': FAKED_LPAR_1_OID,
    'object-uri': FAKED_LPAR_1_URI,
    'parent': FAKED_CPC_1_URI,
    'class': 'logical-partition',
    'name': FAKED_LPAR_1_NAME,
    'description': 'Lpar #1',
    'acceptable-status': ['operating'],
    # Additional properties will be added by FAKED_LPAR_1_DELTA_*
}
FAKED_LPAR_1_DELTA_INACTIVE = {
    'activation-mode': 'not-set',
    'additional-status': '',
    'defined-capacity': None,
    'has-operating-system-messages': None,
    'has-unacceptable-status': True,
    'is-locked': False,
    'last-used-activation-profile': None,
    'last-used-boot-record-logical-block-address': '0',
    'last-used-disk-partition-id': 0,
    'last-used-load-address': '00000',
    'last-used-load-parameter': '',
    'last-used-logical-unit-number': '0',
    'last-used-operating-system-specific-load-parameters': '',
    'last-used-world-wide-port-name': '0',
    'next-activation-profile-name': FAKED_LPAR_1_PROFILE,
    'os-ipl-token': None,
    'os-level': None,
    'os-name': None,
    'os-type': None,
    'partition-identifier': None,
    'partition-number': None,
    'status': 'not-activated',
    'storage-central-allocation': [],
    'storage-expanded-allocation': [],

    # CPU related properties
    'absolute-processing-capping': {'type': 'none'},
    'initial-processing-weight': None,
    'initial-processing-weight-capped': None,
    'minimum-processing-weight': None,
    'maximum-processing-weight': None,
    'current-processing-weight': None,
    'current-processing-weight-capped': None,
}
FAKED_LPAR_1_DELTA_ACTIVE = {
    'activation-mode': 'ssc',
    'additional-status': '',
    'defined-capacity': 0,
    'has-operating-system-messages': True,
    'has-unacceptable-status': False,
    'is-locked': False,
    'last-used-activation-profile': FAKED_LPAR_1_PROFILE,
    'last-used-boot-record-logical-block-address': '0',
    'last-used-disk-partition-id': 0,
    'last-used-load-address': '00000',
    'last-used-load-parameter': '',
    'last-used-logical-unit-number': '0',
    'last-used-operating-system-specific-load-parameters': '',
    'last-used-world-wide-port-name': '0',
    'next-activation-profile-name': FAKED_LPAR_1_PROFILE,
    'os-ipl-token': '0000000000000000',
    'os-level': '1.0.0',
    'os-name': 'INSTALL',
    'os-type': 'SSC',
    'partition-identifier': '33',
    'partition-number': '2f',
    'status': 'not-operating',
    'storage-central-allocation': [
        {
            'current': 8192,
            'gap': 102400,
            'initial': 8192,
            'maximum': 8192,
            'origin': 127322112,
            'storage-element-type': 'central',
        },
    ],
    'storage-expanded-allocation': [],

    # CPU related properties
    'absolute-processing-capping': {'type': 'none'},
    'current-processing-weight': 10,
    'current-processing-weight-capped': False,
    'initial-processing-weight': 10,
    'initial-processing-weight-capped': False,
    'minimum-processing-weight': 0,
    'maximum-processing-weight': 999,
}
FAKED_LPAR_1_DELTA_LOADED = {
    'activation-mode': 'ssc',
    'additional-status': '',
    'defined-capacity': 0,
    'has-operating-system-messages': True,
    'has-unacceptable-status': False,
    'is-locked': False,
    'last-used-activation-profile': FAKED_LPAR_1_PROFILE,
    'last-used-boot-record-logical-block-address': '0',
    'last-used-disk-partition-id': 0,
    'last-used-load-address': '00000',
    'last-used-load-parameter': '',
    'last-used-logical-unit-number': '0',
    'last-used-operating-system-specific-load-parameters': '',
    'last-used-world-wide-port-name': '0',
    'next-activation-profile-name': FAKED_LPAR_1_PROFILE,
    'os-ipl-token': '0000000000000000',
    'os-level': '1.0.0',
    'os-name': 'INSTALL',
    'os-type': 'SSC',
    'partition-identifier': '33',
    'partition-number': '2f',
    'status': 'operating',
    'storage-central-allocation': [
        {
            'current': 8192,
            'gap': 102400,
            'initial': 8192,
            'maximum': 8192,
            'origin': 127322112,
            'storage-element-type': 'central',
        },
    ],
    'storage-expanded-allocation': [],

    # CPU related properties
    'absolute-processing-capping': {'type': 'none'},
    'current-processing-weight': 10,
    'current-processing-weight-capped': False,
    'initial-processing-weight': 10,
    'initial-processing-weight-capped': False,
    'minimum-processing-weight': 0,
    'maximum-processing-weight': 999,
}

# Translation table from 'state' module input parameter to the corresponding
# desired LPAR 'status' property value.
LPAR_STATUS_FROM_STATE = {
    'inactive': 'not-activated',
    'active': 'not-operating',
    'loaded': 'operating',
    'set': None,  # Any
    'facts': None,  # Any
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
    Return the module output as a tuple (changed, lpar_properties) (i.e.
    the arguments of the call to exit_json()).
    If the module failed, return None.
    """

    def func(changed, lpar):
        return changed, lpar

    if not mod_obj.exit_json.called:
        return None
    call_args = mod_obj.exit_json.call_args

    # The following makes sure we get the arguments regardless of whether they
    # were specified as positional or keyword arguments:
    return func(*call_args[0], **call_args[1])


class TestLpar(object):
    """
    All tests for LPARs.
    """

    def setup_method(self):
        """
        Using the zhmcclient mock support, set up a CPC in classic mode, that
        has no LPARs.
        """
        self.session = FakedSession(**FAKED_SESSION_KWARGS)
        self.client = Client(self.session)
        self.console = self.session.hmc.consoles.add(FAKED_CONSOLE)
        self.faked_cpc = self.session.hmc.cpcs.add(FAKED_CPC_1)
        cpcs = self.client.cpcs.list()
        assert len(cpcs) == 1
        self.cpc = cpcs[0]
        self.faked_crypto_adapters = []
        self.faked_crypto_adapter_names = []

    def setup_lpar(self, initial_state, additional_props=None):
        """
        Prepare the faked LPAR, on top of the CPC created by setup_method().
        """
        self.lpar_name = FAKED_LPAR_1_NAME
        lpar_props = FAKED_LPAR_1_BASE.copy()
        if initial_state == 'inactive':
            lpar_props.update(FAKED_LPAR_1_DELTA_INACTIVE)
        elif initial_state == 'active':
            lpar_props.update(FAKED_LPAR_1_DELTA_ACTIVE)
        elif initial_state == 'loaded':
            lpar_props.update(FAKED_LPAR_1_DELTA_LOADED)
        else:
            raise AssertionError(
                "Invalid initial_state={0!r}".format(initial_state))
        if additional_props:
            lpar_props.update(additional_props)
        self.faked_lpar = self.faked_cpc.lpars.add(lpar_props)
        lpars = self.cpc.lpars.list()
        assert len(lpars) == 1
        self.lpar = lpars[0]
        self.lpar.pull_full_properties()

    @pytest.mark.parametrize(
        "check_mode", [False, True])
    @pytest.mark.parametrize(
        "initial_state", ['inactive', 'active', 'loaded'])
    @pytest.mark.parametrize(
        "desired_state", ['inactive', 'active', 'loaded', 'set'])
    @pytest.mark.parametrize(
        "properties, props_changed", [
            # Note: properties is a dict of property values, with the property
            # names as keys (with underscores, as needed for the 'properties'
            # Ansible module input parameter). If a dict value is a tuple, its
            # first item is the input value, and its second item is the
            # expected value. Otherwise, the dict value is both input value and
            # expected value.

            # Special cases:
            ({}, False),

            # All modifiable properties in the subset defined for LPAR_1
            ({'acceptable_status': ['operating', 'exceptions']}, True),
            # 'next_activation_profile_name' must be same as LPAR name
            ({'defined_capacity': 1}, True),
            ({'absolute_processing_capping': {'type': 'none'}}, False),
            ({'initial_processing_weight': 50}, True),
            ({'initial_processing_weight_capped': True}, True),
            ({'minimum_processing_weight': 10}, True),
            ({'maximum_processing_weight': 200}, True),
        ])
    @mock.patch("plugins.modules.zhmc_lpar.AnsibleModule", autospec=True)
    def test_lpar_success(
            self, ansible_mod_cls, properties, props_changed, desired_state,
            initial_state, check_mode):
        """
        Tests for successful operations on LPAR, dependent on
        parametrization. The fact gathering is not tested here.
        """

        # Prepare the initial LPAR before the test is run
        self.setup_lpar(initial_state)

        # Set some expectations for this test from its parametrization#
        if initial_state == 'inactive' and desired_state == 'set' \
                and properties and props_changed:
            exp_exit_code = 1
        elif desired_state == 'inactive' and properties:
            exp_exit_code = 1
        else:
            exp_exit_code = 0

        if exp_exit_code == 0:

            if check_mode:
                exp_status = LPAR_STATUS_FROM_STATE[initial_state]
            elif desired_state == 'set':
                exp_status = LPAR_STATUS_FROM_STATE[initial_state]
            else:
                exp_status = LPAR_STATUS_FROM_STATE[desired_state]
            if initial_state == 'loaded' and desired_state == 'active':
                exp_status = 'operating'

            exp_lpar_returned = (desired_state in ('active', 'loaded', 'set'))

            if desired_state in ('active', 'loaded', 'set') and \
                    properties and props_changed:
                exp_changed = True
            elif initial_state == 'inactive' and \
                    desired_state in ('active', 'loaded'):
                exp_changed = True
            elif initial_state == 'active' and \
                    desired_state in ('inactive', 'loaded'):
                exp_changed = True
            elif initial_state == 'loaded' and \
                    desired_state in ('inactive',):
                exp_changed = True
            else:
                exp_changed = False

        input_props = {}
        exp_props = {}
        for prop_name in properties:
            hmc_prop_name = prop_name.replace('_', '-')
            value = properties[prop_name]
            if isinstance(value, tuple):
                assert len(value) == 2
                input_props[prop_name] = value[0]
                exp_props[hmc_prop_name] = value[1]
            else:
                input_props[prop_name] = value
                exp_props[hmc_prop_name] = value

        # Prepare module input parameters
        params = {
            'hmc_host': 'fake-host',
            'hmc_auth': dict(userid='fake-userid',
                             password='fake-password'),
            'cpc_name': self.cpc.name,
            'name': self.lpar_name,
            'state': desired_state,
            'activation_profile_name': None,  # TODO: Add to tests
            'properties': input_props,
            'log_file': None,
            '_faked_session': self.session,
        }

        # Prepare mocks for AnsibleModule object
        mod_obj = mock_ansible_module(ansible_mod_cls, params, check_mode)

        # Exercise the code to be tested
        with pytest.raises(SystemExit) as exc_info:
            zhmc_lpar.main()
        exit_code = exc_info.value.args[0]

        # Assert module exit code
        assert exit_code == exp_exit_code, \
            "Unexpected module exit code {0} (expected {1}), message:\n{2}". \
            format(exit_code, exp_exit_code, get_failure_msg(mod_obj))

        if exp_exit_code == 0:

            # Assert module output
            changed, lpar_props = get_module_output(mod_obj)
            assert changed == exp_changed
            if exp_lpar_returned:
                assert lpar_props != {}
                if not check_mode:
                    assert lpar_props['status'] == exp_status
                    assert lpar_props['name'] == params['name']
                    if exp_props:
                        for hmc_prop_name, exp_value in exp_props.items():
                            assert lpar_props[hmc_prop_name] == exp_value, \
                                "Property: {0}".format(hmc_prop_name)
            else:
                assert lpar_props == {}

            # Assert the LPAR resource
            if not check_mode:
                lpars = self.cpc.lpars.list()
                assert len(lpars) == 1
                lpar = lpars[0]
                lpar.pull_full_properties()
                assert lpar.properties['status'] == exp_status
                assert lpar.properties['name'] == params['name']
                if properties:
                    for hmc_prop_name, exp_value in exp_props.items():
                        assert lpar.properties[hmc_prop_name] == exp_value, \
                            "Property: {0}".format(hmc_prop_name)

    @pytest.mark.parametrize(
        "check_mode", [False, True])
    @pytest.mark.parametrize(
        "initial_state", ['inactive', 'active', 'loaded'])
    @pytest.mark.parametrize(
        "desired_state", ['facts'])
    @mock.patch("plugins.modules.zhmc_lpar.AnsibleModule",
                autospec=True)
    def test_lpar_facts_success(
            self, ansible_mod_cls, desired_state, initial_state, check_mode):
        """
        Tests for successful fact gathering on LPARs, dependent on
        parametrization.
        """

        # Prepare the initial LPAR before the test is run
        self.setup_lpar(initial_state)

        # Set some expectations for this test from its parametrization#
        exp_exit_code = 0

        # Prepare module input parameters
        params = {
            'hmc_host': 'fake-host',
            'hmc_auth': dict(userid='fake-userid',
                             password='fake-password'),
            'cpc_name': self.cpc.name,
            'name': self.lpar_name,
            'state': desired_state,
            'activation_profile_name': None,  # TODO: Add to tests
            'log_file': None,
            '_faked_session': self.session,
        }

        # Prepare mocks for AnsibleModule object
        mod_obj = mock_ansible_module(ansible_mod_cls, params, check_mode)

        # Exercise the code to be tested
        with pytest.raises(SystemExit) as exc_info:
            zhmc_lpar.main()
        exit_code = exc_info.value.args[0]

        # Assert module exit code
        assert exit_code == exp_exit_code, \
            "Unexpected module exit code {0} (expected {1}), message:\n{2}". \
            format(exit_code, exp_exit_code, get_failure_msg(mod_obj))

        # Assert module output
        changed, lpar_props = get_module_output(mod_obj)
        assert changed is False
        assert isinstance(lpar_props, dict)

        for pname in lpar_props:
            pvalue = lpar_props[pname]
            exp_value = self.lpar.properties[pname]
            assert pvalue == exp_value

# TODO: Add tests for updating read-only properties
