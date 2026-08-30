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
End2end tests for zhmc_adapter_list module.
"""

from __future__ import (absolute_import, division, print_function)
__metaclass__ = type

import pytest
import mock
import requests.packages.urllib3
import zhmcclient
# pylint: disable=line-too-long,unused-import
from zhmcclient.testutils import hmc_definition, hmc_session  # noqa: F401, E501
from zhmcclient.testutils import dpm_mode_cpcs  # noqa: F401, E501
# pylint: enable=line-too-long,unused-import

from plugins.modules import zhmc_adapter_list
from .utils import mock_ansible_module, get_failure_msg

requests.packages.urllib3.disable_warnings()

# Print debug messages
DEBUG = False

LOG_FILE = 'zhmc_adapter_list.log' if DEBUG else None


def get_module_output(mod_obj):
    """
    Return the module output as a tuple (changed, user_properties) (i.e.
    the arguments of the call to exit_json()).
    If the module failed, return None.
    """

    def func(changed, adapters):
        return changed, adapters

    if not mod_obj.exit_json.called:
        return None
    call_args = mod_obj.exit_json.call_args

    # The following makes sure we get the arguments regardless of whether they
    # were specified as positional or keyword arguments:
    return func(*call_args[0], **call_args[1])


def assert_adapter_list(adapter_list, exp_adapter_dict):
    """
    Assert the output of the zhmc_adapter_list module

    Parameters:
      adapter_list(list): Result of zhmc_adapter_list module, a list
        of adapter properties as documented (in HMC notation with dashes).
      exp_adapter_dict(dict): Expected properties for each expected result
        item. Key: tuple(CPC name, adapter name), Value: All properties
        of the adapter plus artificial properties (in HMC notation with
        dashes).
    """

    assert isinstance(adapter_list, list)

    exp_cpc_adapter_names = list(exp_adapter_dict)
    cpc_adapter_names = [(pi.get('cpc_name', None), pi.get('name', None))
                         for pi in adapter_list]
    assert set(cpc_adapter_names) == set(exp_cpc_adapter_names)

    for adapter_item in adapter_list:
        adapter_name = adapter_item.get('name', None)
        cpc_name = adapter_item.get('cpc_name', None)
        cpc_adapter_name = (cpc_name, adapter_name)

        assert adapter_name is not None, \
            "Returned adapter {pi!r} does not have a 'name' property". \
            format(pi=adapter_item)

        assert cpc_adapter_name in exp_adapter_dict, \
            "Result contains unexpected adapter {p!r} in CPC {c!r}". \
            format(p=adapter_name, c=cpc_name)

        exp_adapter_props = exp_adapter_dict[cpc_adapter_name]

        for pname, pvalue in adapter_item.items():

            # Verify normal properties
            pname_hmc = pname.replace('_', '-')
            assert pname_hmc in exp_adapter_props, \
                "Unexpected property {pn!r} in result adapter {rn!r}". \
                format(pn=pname_hmc, rn=adapter_name)
            exp_value = exp_adapter_props[pname_hmc]
            assert pvalue == exp_value, \
                "Incorrect value for property {pn!r} of result adapter " \
                "{rn!r}". \
                format(pn=pname_hmc, rn=adapter_name)


@pytest.mark.parametrize(
    "with_cpc", [
        pytest.param(False, id="with_cpc=False"),
        pytest.param(True, id="with_cpc=True"),
    ]
)
@pytest.mark.parametrize(
    "filters", [  # These are Ansible module parameters, using underscores
        pytest.param(None, id="filters(None)"),
        pytest.param({'type': 'osd'}, id="filters(type=osd)"),
        pytest.param({'adapter_family': 'osa'}, id="filters(family=osa)"),
        pytest.param({'name': '.*'}, id="filters(name=.*)"),
    ]
)
@pytest.mark.parametrize(
    "check_mode", [
        pytest.param(False, id="check_mode=False"),
        pytest.param(True, id="check_mode=True"),
    ]
)
@mock.patch("plugins.modules.zhmc_adapter_list.AnsibleModule", autospec=True)
def test_zhmc_adapter_list(
        ansible_mod_cls, check_mode, filters, with_cpc, dpm_mode_cpcs):  # noqa: F811, E501
    """
    Test the zhmc_adapter_list module with DPM mode CPCs.
    """
    if not dpm_mode_cpcs:
        pytest.skip("HMC definition does not include any CPCs in DPM mode")

    for cpc in dpm_mode_cpcs:
        assert cpc.dpm_enabled

        session = cpc.manager.session
        hd = session.hmc_definition
        hmc_host = hd.host
        hmc_auth = dict(userid=hd.userid, password=hd.password,
                        ca_certs=hd.ca_certs, verify=hd.verify)

        client = zhmcclient.Client(session)
        console = client.consoles.console

        faked_session = session if hd.mock_file else None

        # Determine the expected adapters on the HMC
        if DEBUG:
            print("Debug: Listing expected adapters")
        hmc_version = client.query_api_version()['hmc-version']
        hmc_version_info = [int(x) for x in hmc_version.split('.')]
        if filters:
            filter_args_module = dict(filters)
            filter_args_list = {}
            for fkey, fval in filters.items():
                filter_args_list[fkey.replace('_', '-')] = fval
        else:
            filter_args_module = {}
            filter_args_list = None
        # TODO: Remove check on list_permitted_adapters() once supported
        if hmc_version_info < [2, 14, 0] or \
                not hasattr(console, 'list_permitted_adapters'):
            # List the LPARs in the traditional way
            if with_cpc:
                exp_adapters = cpc.adapters.list(filter_args=filter_args_list)
            else:
                cpcs_ = client.cpcs.list()
                exp_adapters = []
                for cpc_ in cpcs_:
                    exp_adapters.extend(cpc_.adapters.list(
                        filter_args=filter_args_list))
        else:
            # List the LPARs using the new operation
            if with_cpc:
                filter_args_list['cpc-name'] = cpc.name
            exp_adapters = console.list_permitted_adapters(
                filter_args=filter_args_list)
        exp_adapter_dict = {}
        for adapter in exp_adapters:
            if DEBUG:
                print("Debug: Getting expected properties of adapter {p!r} "
                      "on CPC {c!r}".format(p=adapter.name, c=cpc.name))
            adapter.pull_full_properties()
            cpc = adapter.manager.parent
            exp_properties = {}
            exp_properties.update(adapter.properties)
            exp_properties['cpc-name'] = cpc.name
            exp_cpc_adapter_name = (cpc.name, adapter.name)
            exp_adapter_dict[exp_cpc_adapter_name] = exp_properties

        # Check that regexp is supported for the 'name' filter. This is done by
        # ensuring that the expected adapters are as expected.
        if filters == {'name': '.*'} and with_cpc:
            all_adapters = cpc.adapters.list()
            all_adapter_names = [ad.name for ad in all_adapters].sort()
            exp_adapter_names = \
                [item[1] for item in exp_adapter_dict.keys()].sort()
            assert exp_adapter_names == all_adapter_names, \
                "cpc.adapters.list() with 'name' filter does not seem to " \
                "support regular expressions"

        # Prepare module input parameters (must be all required + optional)
        params = {
            'hmc_host': hmc_host,
            'hmc_auth': hmc_auth,
            'cpc_name': cpc.name if with_cpc else None,
            'name': filter_args_module.get('name', None),
            'adapter_id': filter_args_module.get('adapter_id', None),
            'adapter_family': filter_args_module.get('adapter_family', None),
            'type': filter_args_module.get('type', None),
            'status': filter_args_module.get('status', None),
            'log_file': LOG_FILE,
            '_faked_session': faked_session,
        }

        # Prepare mocks for AnsibleModule object
        mod_obj = mock_ansible_module(ansible_mod_cls, params, check_mode)

        # Exercise the code to be tested
        with pytest.raises(SystemExit) as exc_info:
            zhmc_adapter_list.main()
        exit_code = exc_info.value.args[0]

        # Assert module exit code
        assert exit_code == 0, \
            "Module failed with exit code {e} and message:\n{m}". \
            format(e=exit_code, m=get_failure_msg(mod_obj))

        # Assert module output
        changed, adapter_list = get_module_output(mod_obj)
        assert changed is False

        assert_adapter_list(adapter_list, exp_adapter_dict)
