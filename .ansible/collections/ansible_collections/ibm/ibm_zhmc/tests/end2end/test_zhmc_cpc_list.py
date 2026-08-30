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
End2end tests for zhmc_cpc_list module.
"""

from __future__ import (absolute_import, division, print_function)
__metaclass__ = type

import pytest
import mock
import requests.packages.urllib3
import zhmcclient
# pylint: disable=line-too-long,unused-import
from zhmcclient.testutils import hmc_definition, hmc_session  # noqa: F401, E501
# pylint: enable=line-too-long,unused-import

from plugins.modules import zhmc_cpc_list
from .utils import mock_ansible_module, get_failure_msg

requests.packages.urllib3.disable_warnings()

# Print debug messages
DEBUG = False

LOG_FILE = 'zhmc_cpc_list.log' if DEBUG else None


def get_module_output(mod_obj):
    """
    Return the module output as a tuple (changed, user_properties) (i.e.
    the arguments of the call to exit_json()).
    If the module failed, return None.
    """

    def func(changed, cpcs):
        return changed, cpcs

    if not mod_obj.exit_json.called:
        return None
    call_args = mod_obj.exit_json.call_args

    # The following makes sure we get the arguments regardless of whether they
    # were specified as positional or keyword arguments:
    return func(*call_args[0], **call_args[1])


def assert_cpc_list(
        cpc_list, include_unmanaged_cpcs, exp_cpc_dict, exp_um_cpc_dict):
    """
    Assert the output of the zhmc_cpc_list module
    """

    # Apply default for include_unmanaged_cpcs parameter
    # (explicit is better than implicit)
    if include_unmanaged_cpcs is None:
        include_unmanaged_cpcs = False

    assert isinstance(cpc_list, list)

    exp_len = len(exp_cpc_dict)
    if include_unmanaged_cpcs:
        exp_len += len(exp_um_cpc_dict)
    assert len(cpc_list) == exp_len

    for cpc_item in cpc_list:

        assert 'name' in cpc_item, \
            "Returned CPC {ri!r} does not have a 'name' property". \
            format(ri=cpc_item)
        cpc_name = cpc_item['name']

        assert 'is_managed' in cpc_item, \
            "Returned CPC {ri!r} does not have a 'is_managed' property". \
            format(ri=cpc_item)
        is_managed = cpc_item['is_managed']

        for pname, pvalue in cpc_item.items():

            if is_managed:
                assert cpc_name in exp_cpc_dict, \
                    "Result contains unexpected managed CPC: {rn!r}". \
                    format(rn=cpc_name)

                exp_cpc = exp_cpc_dict[cpc_name]

                # Handle artificial properties
                if pname == 'is_managed':
                    continue

                # Verify normal properties
                pname_hmc = pname.replace('_', '-')
                assert pname_hmc in exp_cpc.properties, \
                    "Unexpected property {pn!r} in CPC {rn!r}". \
                    format(pn=pname, rn=cpc_name)
                exp_value = exp_cpc.properties[pname_hmc]
                assert pvalue == exp_value, \
                    "Incorrect value for property {pn!r} of CPC {rn!r}". \
                    format(pn=pname, rn=cpc_name)

            else:
                assert cpc_name in exp_um_cpc_dict, \
                    "Result contains unexpected unmanaged CPC: {rn!r}". \
                    format(rn=cpc_name)


@pytest.mark.parametrize(
    "check_mode", [
        pytest.param(False, id="check_mode=False"),
        pytest.param(True, id="check_mode=True"),
    ]
)
@pytest.mark.parametrize(
    "include_unmanaged_cpcs", [
        pytest.param(False, id="include_unmanaged_cpcs=False"),
        pytest.param(True, id="include_unmanaged_cpcs=True"),
        pytest.param(None, id="include_unmanaged_cpcs=None"),
    ]
)
@mock.patch("plugins.modules.zhmc_cpc_list.AnsibleModule", autospec=True)
def test_zhmc_cpc_list(
        ansible_mod_cls, include_unmanaged_cpcs, check_mode, hmc_session):  # noqa: F811, E501
    """
    Test the zhmc_cpc_list module with managed and unmanaged CPCs.
    """

    hd = hmc_session.hmc_definition
    hmc_host = hd.host
    hmc_auth = dict(userid=hd.userid, password=hd.password,
                    ca_certs=hd.ca_certs, verify=hd.verify)

    client = zhmcclient.Client(hmc_session)

    faked_session = hmc_session if hd.mock_file else None

    # Determine the expected managed CPCs on the HMC
    exp_cpcs = client.cpcs.list()
    exp_cpcs_dict = {}
    for cpc in exp_cpcs:
        cpc.pull_full_properties()
        exp_cpcs_dict[cpc.name] = cpc

    # Determine the expected unmanaged CPCs on the HMC
    exp_um_cpcs = client.consoles.console.list_unmanaged_cpcs()
    exp_um_cpcs_dict = {}
    for cpc in exp_um_cpcs:
        exp_um_cpcs_dict[cpc.name] = cpc

    # Prepare module input parameters (must be all required + optional)
    params = {
        'hmc_host': hmc_host,
        'hmc_auth': hmc_auth,
        'log_file': LOG_FILE,
        '_faked_session': faked_session,
    }
    if include_unmanaged_cpcs is not None:
        params['include_unmanaged_cpcs'] = include_unmanaged_cpcs

    # Prepare mocks for AnsibleModule object
    mod_obj = mock_ansible_module(ansible_mod_cls, params, check_mode)

    # Exercise the code to be tested
    with pytest.raises(SystemExit) as exc_info:
        zhmc_cpc_list.main()
    exit_code = exc_info.value.args[0]

    # Assert module exit code
    assert exit_code == 0, \
        "Module failed with exit code {e} and message:\n{m}". \
        format(e=exit_code, m=get_failure_msg(mod_obj))

    # Assert module output
    changed, cpc_list = get_module_output(mod_obj)
    assert changed is False

    assert_cpc_list(
        cpc_list, include_unmanaged_cpcs, exp_cpcs_dict, exp_um_cpcs_dict)
