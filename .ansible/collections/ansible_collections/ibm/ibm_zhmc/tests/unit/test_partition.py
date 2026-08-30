#!/usr/bin/env python
# Copyright 2017-2020 IBM Corp. All Rights Reserved.
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
Unit tests for the 'zhmc_partition' Ansible module.
"""

from __future__ import (absolute_import, division, print_function)
__metaclass__ = type

import unittest
import mock
import re
import pdb
import pytest
import zhmcclient
import zhmcclient_mock

from plugins.modules import zhmc_partition
from plugins.module_utils import common as module_utils


class TestZhmcPartitionMain(unittest.TestCase):
    """
    Unit tests for the main() function.
    """

    @mock.patch("plugins.modules.zhmc_partition.perform_task",
                autospec=True)
    @mock.patch("plugins.modules.zhmc_partition.AnsibleModule",
                autospec=True)
    def test_main_success(self, ansible_mod_cls, perform_task_func):
        """
        Test main() with all required module parameters.
        """

        # Module invocation
        params = {
            'hmc_host': 'fake-host',
            'hmc_auth': dict(userid='fake-userid',
                             password='fake-password'),
            'cpc_name': 'fake-cpc-name',
            'name': 'fake-name',
            'state': 'absent',
            'expand_storage_groups': False,
            'expand_crypto_adapters': False,
            'log_file': None,
        }
        check_mode = False

        # Return values of perform_task()
        perform_task_changed = True
        perform_task_result = {}

        # Prepare mocks
        mod_obj = ansible_mod_cls.return_value
        mod_obj.params = params
        mod_obj.check_mode = check_mode
        mod_obj.fail_json.configure_mock(side_effect=SystemExit(1))
        mod_obj.exit_json.configure_mock(side_effect=SystemExit(0))
        perform_task_func.return_value = (perform_task_changed,
                                          perform_task_result)

        # Exercise code
        with self.assertRaises(SystemExit) as cm:
            zhmc_partition.main()
        exit_code = cm.exception.args[0]

        # Assert module exit code
        assert exit_code == 0

        # Assert call to AnsibleModule()
        expected_argument_spec = dict(
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
            name=dict(required=True, type='str'),
            state=dict(required=True, type='str',
                       choices=['absent', 'stopped', 'active', 'facts']),
            properties=dict(required=False, type='dict', default={}),
            expand_storage_groups=dict(required=False, type='bool',
                                       default=False),
            expand_crypto_adapters=dict(required=False, type='bool',
                                        default=False),
            log_file=dict(required=False, type='str', default=None),
            _faked_session=dict(required=False, type='raw'),
        )
        assert ansible_mod_cls.call_args == \
            mock.call(argument_spec=expected_argument_spec,
                      supports_check_mode=True)

        # Assert call to perform_task()
        assert perform_task_func.call_args == mock.call(params, check_mode)

        # Assert call to exit_json()
        assert mod_obj.exit_json.call_args == \
            mock.call(changed=perform_task_changed,
                      partition=perform_task_result)

        # Assert no call to fail_json()
        assert mod_obj.fail_json.called is False

    @mock.patch("plugins.modules.zhmc_partition.perform_task",
                autospec=True)
    @mock.patch("plugins.modules.zhmc_partition.AnsibleModule",
                autospec=True)
    def test_main_param_error(self, ansible_mod_cls, perform_task_func):
        """
        Test main() with ParameterError being raised in perform_task().
        """

        # Module invocation
        params = {
            'hmc_host': 'fake-host',
            'hmc_auth': dict(userid='fake-userid',
                             password='fake-password'),
            'cpc_name': 'fake-cpc-name',
            'name': 'fake-name',
            'state': 'absent',
            'expand_storage_groups': False,
            'expand_crypto_adapters': False,
            'log_file': None,
        }
        check_mode = False

        # Exception raised by perform_task()
        perform_task_exc = module_utils.ParameterError("fake message")

        # Prepare mocks
        mod_obj = ansible_mod_cls.return_value
        mod_obj.params = params
        mod_obj.check_mode = check_mode
        mod_obj.fail_json.configure_mock(side_effect=SystemExit(1))
        mod_obj.exit_json.configure_mock(side_effect=SystemExit(0))
        perform_task_func.mock.configure_mock(side_effect=perform_task_exc)

        # Exercise code
        with self.assertRaises(SystemExit) as cm:
            zhmc_partition.main()
        exit_code = cm.exception.args[0]

        # Assert module exit code
        assert exit_code == 1

        # Assert call to perform_task()
        assert perform_task_func.call_args == mock.call(params, check_mode)

        # Assert call to fail_json()
        assert mod_obj.fail_json.call_args == \
            mock.call(msg="ParameterError: fake message")

        # Assert no call to exit_json()
        assert mod_obj.exit_json.called is False


class TestZhmcPartitionPerformTask(unittest.TestCase):
    """
    Unit tests for the perform_task() function.
    """

    @mock.patch("plugins.modules.zhmc_partition.ensure_absent",
                autospec=True)
    @mock.patch("plugins.modules.zhmc_partition.ensure_active",
                autospec=True)
    @mock.patch("plugins.modules.zhmc_partition.ensure_stopped",
                autospec=True)
    def test_pt_active(self, ensure_stopped_func, ensure_active_func,
                       ensure_absent_func):
        """
        Test perform_task() with state 'active'.
        """

        # Prepare input arguments
        params = {
            'state': 'active',
            'log_file': None,
        }
        check_mode = True

        # Prepare return values
        changed = False
        result = {
            'fake-prop': 'fake-value',
        }

        # Prepare mocks
        ensure_active_func.return_value = (changed, result)

        # Exercise code
        actual_changed, actual_result = zhmc_partition.perform_task(
            params, check_mode)

        # Assert return values
        assert actual_changed == changed
        assert actual_result == result

        # Assert call to the desired action function
        assert ensure_active_func.call_args == mock.call(params, check_mode)

        # Assert no call to the other action functions
        assert ensure_stopped_func.called is False
        assert ensure_absent_func.called is False

    @mock.patch("plugins.modules.zhmc_partition.ensure_absent",
                autospec=True)
    @mock.patch("plugins.modules.zhmc_partition.ensure_active",
                autospec=True)
    @mock.patch("plugins.modules.zhmc_partition.ensure_stopped",
                autospec=True)
    def test_pt_stopped(self, ensure_stopped_func, ensure_active_func,
                        ensure_absent_func):
        """
        Test perform_task() with state 'stopped'.
        """

        # Prepare input arguments
        params = {
            'state': 'stopped',
            'log_file': None,
        }
        check_mode = True

        # Prepare return values
        changed = True
        result = {
            'fake-prop': 'fake-value',
        }

        # Prepare mocks
        ensure_stopped_func.return_value = (changed, result)

        # Exercise code
        actual_changed, actual_result = zhmc_partition.perform_task(
            params, check_mode)

        # Assert return values
        assert actual_changed == changed
        assert actual_result == result

        # Assert call to the desired action function
        assert ensure_stopped_func.call_args == mock.call(params, check_mode)

        # Assert no call to the other action functions
        assert ensure_active_func.called is False
        assert ensure_absent_func.called is False

    @mock.patch("plugins.modules.zhmc_partition.ensure_absent",
                autospec=True)
    @mock.patch("plugins.modules.zhmc_partition.ensure_active",
                autospec=True)
    @mock.patch("plugins.modules.zhmc_partition.ensure_stopped",
                autospec=True)
    def test_pt_absent(self, ensure_stopped_func, ensure_active_func,
                       ensure_absent_func):
        """
        Test perform_task() with state 'absent'.
        """

        # Prepare input arguments
        params = {
            'state': 'absent',
            'log_file': None,
        }
        check_mode = False

        # Prepare return values
        changed = True
        result = {
            'fake-prop': 'fake-value',
        }

        # Prepare mocks
        ensure_absent_func.return_value = (changed, result)

        # Exercise code
        actual_changed, actual_result = zhmc_partition.perform_task(
            params, check_mode)

        # Assert return values
        assert actual_changed == changed
        assert actual_result == result

        # Assert call to the desired action function
        assert ensure_absent_func.call_args == mock.call(params, check_mode)

        # Assert no call to the other action functions
        assert ensure_active_func.called is False
        assert ensure_stopped_func.called is False


# Faked CPC in DPM mode that is used for all tests
# (with property names as specified in HMC data model)
FAKED_CPC_1_OID = 'fake-cpc-1'
FAKED_CPC_1_URI = '/api/cpcs/' + FAKED_CPC_1_OID
FAKED_CPC_1 = {
    'object-id': FAKED_CPC_1_OID,
    'object-uri': FAKED_CPC_1_URI,
    'class': 'cpc',
    'name': 'cpc-name-1',
    'description': 'CPC #1 in DPM mode',
    'status': 'active',
    'dpm-enabled': True,
    'is-ensemble-member': False,
    'iml-mode': 'dpm',
}

# FakedSession() init arguments
FAKED_SESSION_KWARGS = dict(
    host='fake-host',
    hmc_name='faked-hmc-name',
    hmc_version='2.13.1',
    api_version='1.8'
)


def mocked_cpc():
    """
    Create and return a mocked Cpc object.
    """
    session = zhmcclient_mock.FakedSession(**FAKED_SESSION_KWARGS)
    client = zhmcclient.Client(session)
    session.hmc.cpcs.add(FAKED_CPC_1)
    cpcs = client.cpcs.list()
    assert len(cpcs) == 1
    cpc = cpcs[0]
    return cpc


PARTITION_CREATE_CHECK_MODE_PARTITION_TESTCASES = [
    # Testcases for test_partition_create_check_mode_partition()
    # The list items are tuples with the following items:
    # - desc (string): description of the testcase.
    # - create_props (dict): HMC-formatted properties for the create_props
    #   parameter of the function.
    # - update_props (dict): HMC-formatted properties for the update_props
    #   parameter of the function.
    # - exp_props (dict): HMC-formatted properties for expected
    #   properties of created partition.
    # - exp_exc_type: Expected exception type, or None for no exc. expected.
    # - exp_exc_pattern: Expected exception message pattern, or None for no
    #   exception expected.
    # - run: Indicates whether the test should be run, or 'pdb' for debugger.

    (
        "No input properties",
        {},
        {},
        None,
        module_utils.ParameterError,
        "Required.*properties.*missing.* "
        "(?=.* name)(?=.* initial_memory)(?=.* maximum_memory)",
        True,
    ),
    (
        "Memory-related required input properties missing",
        {
            'name': 'name1',
            'ifl-processors': 1,
        },
        {},
        None,
        module_utils.ParameterError,
        "Required.*properties.*missing.* "
        "(?=.* initial_memory)(?=.* maximum_memory)",
        True,
    ),
    (
        "Minimum set of required input properties as create_props",
        {
            'name': 'name1',
            'ifl-processors': 1,
            'initial-memory': 4096,
            'maximum-memory': 4096,
        },
        {},
        {
            'name': 'name1',
            'ifl-processors': 1,
            'initial-memory': 4096,
            'maximum-memory': 4096,
            # some defaulted values:
            'type': 'linux',
            'cp-processors': 0,
            'reserved-memory': 0,
            'threads-per-processor': 0,
        },
        None,
        None,
        True,
    ),
    (
        "SSC partition with missing required SSC-specific properties",
        {
            'name': 'name1',
            'ifl-processors': 1,
            'initial-memory': 4096,
            'maximum-memory': 4096,
            'type': 'ssc',
        },
        {},
        None,
        module_utils.ParameterError,
        "Required.*properties.*missing.* "
        "(?=.* ssc_host_name)(?=.* ssc_master_userid)(?=.* ssc_master_pw)",
        True,
    ),
    (
        "SSC partition with all required SSC-specific properties",
        {
            'name': 'name1',
            'ifl-processors': 1,
            'initial-memory': 4096,
            'maximum-memory': 8192,
            'type': 'ssc',
            'ssc-host-name': 'host1',
            'ssc-master-userid': 'user1',
            'ssc-master-pw': 'pw1',
        },
        {},
        {
            'name': 'name1',
            'ifl-processors': 1,
            'cp-processors': 0,
            'initial-memory': 4096,
            'maximum-memory': 8192,
            'reserved-memory': 4096,
            'type': 'ssc',
            'ssc-host-name': 'host1',
            'ssc-master-userid': 'user1',
            'ssc-master-pw': 'pw1',
        },
        None,
        None,
        True,
    ),
    (
        "Partition with auto-generate of partition ID disabled and missing "
        "required partition ID property",
        {
            'name': 'name1',
            'ifl-processors': 1,
            'initial-memory': 4096,
            'maximum-memory': 4096,
            'autogenerate-partition-id': False,
        },
        {},
        None,
        module_utils.ParameterError,
        "Required.*properties.*missing.* partition_id",
        True,
    ),
    (
        "Partition with auto-generate of partition ID disabled and required "
        "partition ID property",
        {
            'name': 'name1',
            'ifl-processors': 1,
            'initial-memory': 4096,
            'maximum-memory': 4096,
            'autogenerate-partition-id': False,
            'partition-id': '42',
        },
        {},
        {
            'name': 'name1',
            'ifl-processors': 1,
            'cp-processors': 0,
            'initial-memory': 4096,
            'maximum-memory': 4096,
            'autogenerate-partition-id': False,
            'partition-id': '42',
        },
        None,
        None,
        True,
    ),
    (
        "FTP boot partition with missing required FTP boot specific properties",
        {
            'name': 'name1',
            'ifl-processors': 1,
            'initial-memory': 4096,
            'maximum-memory': 4096,
            'boot-device': 'ftp',
        },
        {},
        None,
        module_utils.ParameterError,
        "Required.*properties.*missing.* "
        "(?=.* boot_ftp_host)(?=.* boot_ftp_username)(?=.* boot_ftp_password)"
        "(?=.* boot_ftp_insfile)",
        True,
    ),
    (
        "FTP boot partition with all required FTP boot specific properties",
        {
            'name': 'name1',
            'ifl-processors': 1,
            'initial-memory': 4096,
            'maximum-memory': 4096,
            'boot-device': 'ftp',
            'boot-ftp-host': 'host1',
            'boot-ftp-username': 'user1',
            'boot-ftp-password': 'pw1',
            'boot-ftp-insfile': 'ins1',
        },
        {},
        {
            'name': 'name1',
            'ifl-processors': 1,
            'cp-processors': 0,
            'initial-memory': 4096,
            'maximum-memory': 4096,
            'boot-device': 'ftp',
            'boot-ftp-host': 'host1',
            'boot-ftp-username': 'user1',
            'boot-ftp-password': 'pw1',
            'boot-ftp-insfile': 'ins1',
        },
        None,
        None,
        True,
    ),
    (
        "removable-media boot partition with missing required boot properties",
        {
            'name': 'name1',
            'ifl-processors': 1,
            'initial-memory': 4096,
            'maximum-memory': 4096,
            'boot-device': 'removable-media',
        },
        {},
        None,
        module_utils.ParameterError,
        "Required.*properties.*missing.* "
        "(?=.* boot_removable_media)(?=.* boot_removable_media_type)",
        True,
    ),
    (
        "removable-media boot partition with all required boot properties",
        {
            'name': 'name1',
            'ifl-processors': 1,
            'initial-memory': 4096,
            'maximum-memory': 4096,
            'boot-device': 'removable-media',
            'boot-removable-media': 'media1',
            'boot-removable-media-type': 'usb',
        },
        {},
        {
            'name': 'name1',
            'ifl-processors': 1,
            'cp-processors': 0,
            'initial-memory': 4096,
            'maximum-memory': 4096,
            'boot-device': 'removable-media',
            'boot-removable-media': 'media1',
            'boot-removable-media-type': 'usb',
        },
        None,
        None,
        True,
    ),
    (
        "storage-adapter boot partition with missing required boot properties",
        {
            'name': 'name1',
            'ifl-processors': 1,
            'initial-memory': 4096,
            'maximum-memory': 4096,
            'boot-device': 'storage-adapter',
        },
        {},
        None,
        module_utils.ParameterError,
        "Required.*properties.*missing.* "
        "(?=.* boot_logical_unit_number)(?=.* boot_world_wide_port_name)",
        True,
    ),
    (
        "storage-adapter boot partition with all required boot properties",
        {
            'name': 'name1',
            'ifl-processors': 1,
            'initial-memory': 4096,
            'maximum-memory': 4096,
            'boot-device': 'storage-adapter',
            'boot-logical-unit-number': '01',
            'boot-world-wide-port-name': '02',
        },
        {},
        {
            'name': 'name1',
            'ifl-processors': 1,
            'cp-processors': 0,
            'initial-memory': 4096,
            'maximum-memory': 4096,
            'boot-device': 'storage-adapter',
            'boot-logical-unit-number': '01',
            'boot-world-wide-port-name': '02',
        },
        None,
        None,
        True,
    ),
]


@pytest.mark.parametrize(
    "desc, create_props, update_props, exp_props, exp_exc_type, "
    "exp_exc_pattern, run",
    PARTITION_CREATE_CHECK_MODE_PARTITION_TESTCASES)
def test_partition_create_check_mode_partition(
        desc, create_props, update_props, exp_props, exp_exc_type,
        exp_exc_pattern, run):
    """
    Test the create_check_mode_partition() function.
    """

    cpc = mocked_cpc()

    if not run:
        pytest.skip("Testcase disabled: {0}".format(desc))

    if exp_exc_type:

        with pytest.raises(exp_exc_type) as exc_info:

            if run == 'pdb':
                pdb.set_trace()

            # The function to be tested
            part_obj = zhmc_partition.create_check_mode_partition(
                cpc, create_props, update_props)

        exc = exc_info.value
        exc_msg = str(exc)
        assert re.match(exp_exc_pattern, exc_msg), \
            "Unexpected message in exception {0}:\n" \
            "  Expctd pattern: {1}\n" \
            "  Actual message: {2}". \
            format(exc.__class__.__name__, exp_exc_pattern, exc_msg)

    else:

        if run == 'pdb':
            pdb.set_trace()

        # The function to be tested
        part_obj = zhmc_partition.create_check_mode_partition(
            cpc, create_props, update_props)

        act_props = dict(part_obj.properties)
        if exp_props:
            for prop_hmc_name in exp_props:

                assert prop_hmc_name in act_props, \
                    "Property {0} missing in result:\n" \
                    "{1}".format(prop_hmc_name, act_props)

                prop_value = act_props[prop_hmc_name]
                exp_prop_value = exp_props[prop_hmc_name]
                assert prop_value == exp_prop_value, \
                    "Unexpected value for property {0}: {1}". \
                    format(prop_hmc_name, exp_prop_value)


# The other functions of the module are tested with function tests.
