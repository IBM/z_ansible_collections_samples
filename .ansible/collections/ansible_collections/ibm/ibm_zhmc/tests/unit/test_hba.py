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
Unit tests for the 'zhmc_hba' Ansible module.
"""

from __future__ import (absolute_import, division, print_function)
__metaclass__ = type

import pytest
import mock

from plugins.modules import zhmc_hba
from plugins.module_utils import common as module_utils


class TestZhmcHbaMain(object):
    """
    Unit tests for the main() function.
    """

    @pytest.mark.parametrize(
        "check_mode", [False, True])
    @mock.patch("plugins.modules.zhmc_hba.perform_task",
                autospec=True)
    @mock.patch("plugins.modules.zhmc_hba.AnsibleModule",
                autospec=True)
    def test_main_success(
            self, ansible_mod_cls, perform_task_func, check_mode):
        """
        Test main() with all required module parameters.
        """

        # Module invocation
        params = {
            'hmc_host': 'fake-host',
            'hmc_auth': dict(userid='fake-userid',
                             password='fake-password'),
            'cpc_name': 'fake-cpc-name',
            'partition_name': 'fake-partition-name',
            'name': 'fake-hba-name',
            'state': 'absent',
            'log_file': None,
        }

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

        # Exercise the code to be tested
        with pytest.raises(SystemExit) as exc_info:
            zhmc_hba.main()
        exit_code = exc_info.value.args[0]

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
            partition_name=dict(required=True, type='str'),
            name=dict(required=True, type='str'),
            state=dict(required=True, type='str',
                       choices=['absent', 'present']),
            properties=dict(required=False, type='dict', default={}),
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
            mock.call(changed=perform_task_changed, hba=perform_task_result)

        # Assert no call to fail_json()
        assert mod_obj.fail_json.called is False

    @pytest.mark.parametrize(
        "check_mode", [False, True])
    @mock.patch("plugins.modules.zhmc_hba.perform_task",
                autospec=True)
    @mock.patch("plugins.modules.zhmc_hba.AnsibleModule",
                autospec=True)
    def test_main_param_error(
            self, ansible_mod_cls, perform_task_func, check_mode):
        """
        Test main() with ParameterError being raised in perform_task().
        """

        # Module invocation
        params = {
            'hmc_host': 'fake-host',
            'hmc_auth': dict(userid='fake-userid',
                             password='fake-password'),
            'cpc_name': 'fake-cpc-name',
            'partition_name': 'fake-partition-name',
            'name': 'fake-hba-name',
            'state': 'absent',
            'log_file': None,
        }

        # Exception raised by perform_task()
        perform_task_exc = module_utils.ParameterError("fake message")

        # Prepare mocks
        mod_obj = ansible_mod_cls.return_value
        mod_obj.params = params
        mod_obj.check_mode = check_mode
        mod_obj.fail_json.configure_mock(side_effect=SystemExit(1))
        mod_obj.exit_json.configure_mock(side_effect=SystemExit(0))
        perform_task_func.mock.configure_mock(side_effect=perform_task_exc)

        # Exercise the code to be tested
        with pytest.raises(SystemExit) as exc_info:
            zhmc_hba.main()
        exit_code = exc_info.value.args[0]

        # Assert module exit code
        assert exit_code == 1

        # Assert call to perform_task()
        assert perform_task_func.call_args == mock.call(params, check_mode)

        # Assert call to fail_json()
        assert mod_obj.fail_json.call_args == \
            mock.call(msg="ParameterError: fake message")

        # Assert no call to exit_json()
        assert mod_obj.exit_json.called is False


class TestZhmcHbaPerformTask(object):
    """
    Unit tests for the perform_task() function.
    """

    @pytest.mark.parametrize(
        "check_mode", [False, True])
    @mock.patch("plugins.modules.zhmc_hba.ensure_absent",
                autospec=True)
    @mock.patch("plugins.modules.zhmc_hba.ensure_present",
                autospec=True)
    def test_pt_present(
            self, ensure_present_func, ensure_absent_func, check_mode):
        """
        Test perform_task() with state 'present'.
        """

        # Prepare input arguments
        params = {
            'state': 'present',
            'log_file': None,
        }

        # Prepare return values
        changed = False
        result = {
            'fake-prop': 'fake-value',
        }

        # Prepare mocks
        ensure_present_func.return_value = (changed, result)

        # Exercise code
        actual_changed, actual_result = zhmc_hba.perform_task(
            params, check_mode)

        # Assert return values
        assert actual_changed == changed
        assert actual_result == result

        # Assert call to the desired action function
        assert ensure_present_func.call_args == mock.call(params, check_mode)

        # Assert no call to the other action functions
        assert ensure_absent_func.called is False

    @pytest.mark.parametrize(
        "check_mode", [False, True])
    @mock.patch("plugins.modules.zhmc_hba.ensure_absent",
                autospec=True)
    @mock.patch("plugins.modules.zhmc_hba.ensure_present",
                autospec=True)
    def test_pt_absent(
            self, ensure_present_func, ensure_absent_func, check_mode):
        """
        Test perform_task() with state 'absent'.
        """

        # Prepare input arguments
        params = {
            'state': 'absent',
            'log_file': None,
        }

        # Prepare return values
        changed = True
        result = {
            'fake-prop': 'fake-value',
        }

        # Prepare mocks
        ensure_absent_func.return_value = (changed, result)

        # Exercise code
        actual_changed, actual_result = zhmc_hba.perform_task(
            params, check_mode)

        # Assert return values
        assert actual_changed == changed
        assert actual_result == result

        # Assert call to the desired action function
        assert ensure_absent_func.call_args == mock.call(params, check_mode)

        # Assert no call to the other action functions
        assert ensure_present_func.called is False


# The other functions of the module are tested with function tests.
