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
Function tests for the 'zhmc_partition' Ansible module.
"""

# pylint: disable=bad-option-value,redundant-u-string-prefix

from __future__ import (absolute_import, division, print_function)
__metaclass__ = type

import pytest
import mock
import re

from zhmcclient import Client
from zhmcclient_mock import FakedSession

from plugins.modules import zhmc_partition

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

# Faked partition that is used for these tests. Most properties are set to
# their default values. Note, we are prepping a faked partition; we are not
# passing these properties to PartitionManager.create().
FAKED_PARTITION_1_NAME = 'part-name-1'
FAKED_PARTITION_1_OID = 'fake-part-1'
FAKED_PARTITION_1_URI = '/api/partitions/' + FAKED_PARTITION_1_OID
FAKED_PARTITION_1 = {
    'object-id': FAKED_PARTITION_1_OID,
    'object-uri': FAKED_PARTITION_1_URI,
    'parent': FAKED_CPC_1_URI,
    'class': 'partition',
    'name': FAKED_PARTITION_1_NAME,
    'description': 'Partition #1',
    'short-name': 'PART1',
    'partition-id': '4F',
    'ifl-processors': 1,
    'initial-memory': 1024,
    'maximum-memory': 2048,
    'status': 'stopped',
    'acceptable-status': ['active', 'stopped'],
    'has-unacceptable-status': False,

    # The remaining properties get their default values:
    'is-locked': False,
    'type': 'linux',
    'autogenerate-partition-id': True,
    'os-name': '',
    'os-type': '',
    'os-version': '',
    'reserve-resources': False,
    'degraded-adapters': [],
    'processor-mode': 'shared',
    'cp-processors': 0,
    'ifl-absolute-processor-capping': False,
    'cp-absolute-processor-capping': False,
    'ifl-absolute-processor-capping-value': 1.0,
    'cp-absolute-processor-capping-value': 1.0,
    'ifl-processing-weight-capped': False,
    'cp-processing-weight-capped': False,
    'minimum-ifl-processing-weight': 1,
    'minimum-cp-processing-weight': 1,
    'initial-ifl-processing-weight': 100,
    'initial-cp-processing-weight': 100,
    'current-ifl-processing-weight': 42,
    'current-cp-processing-weight': 100,
    'maximum-ifl-processing-weight': 999,
    'maximum-cp-processing-weight': 999,
    'processor-management-enabled': False,
    'reserved-memory': 1024,
    'auto-start': False,
    'boot-device': 'none',
    'boot-network-device': None,
    'boot-ftp-host': None,
    'boot-ftp-username': None,
    'boot-ftp-password': None,
    'boot-ftp-insfile': None,
    'boot-removable-media': None,
    'boot-removable-media-type': None,
    'boot-timeout': 60,
    'boot-storage-volume': None,
    'boot-storage-volume-name': None,
    'boot-storage-device': None,
    'boot-logical-unit-number': '',
    'boot-world-wide-port-name': '',
    'boot-configuration-selector': 0,
    'boot-record-lba': None,
    'boot-os-specific-parameters': None,
    'boot-iso-image-name': None,
    'boot-iso-ins-file': None,
    'access-global-performance-data': False,
    'permit-cross-partition-commands': False,
    'access-basic-counter-set': False,
    'access-problem-state-counter-set': False,
    'access-crypto-activity-counter-set': False,
    'access-extended-counter-set': False,
    'access-coprocessor-group-set': False,
    'access-basic-sampling': False,
    'access-diagnostic-sampling': False,
    'permit-des-key-import-functions': True,
    'permit-aes-key-import-functions': True,
    'threads-per-processor': 0,
    'virtual-function-uris': [],
    'nic-uris': [],
    'hba-uris': [],
    'storage-group-uris': [],
    'crypto-configuration': None,

    # SSC-only properties; they are not present for type='linux'
    # 'ssc-host-name': None,
    # 'ssc-boot-selection': None,
    # 'ssc-ipv4-gateway': None,
    # 'ssc-dns-servers': None,
    # 'ssc-master-userid': None,
    # 'ssc-master-pw': None,
}

# Faked HBA that is used for these tests (for partition boot from storage).
# Most properties are set to their default values.
FAKED_HBA_1_NAME = 'hba-1'
FAKED_HBA_1_OID = 'fake-hba-1'
FAKED_HBA_1_URI = FAKED_PARTITION_1_URI + '/hbas/' + FAKED_HBA_1_OID
FAKED_HBA_1 = {
    'element-id': FAKED_HBA_1_OID,
    'element-uri': FAKED_HBA_1_URI,
    'parent': FAKED_PARTITION_1_URI,
    'class': 'hba',
    'name': FAKED_HBA_1_NAME,
    'description': 'HBA #1',
    'device_number': '012F',
    'wwpn': 'abcdef0123456789',
    'adapter-port-uri': 'faked-adapter-port-uri',
}

# Faked adapter, port and vswitch used for the OSA NIC.
FAKED_ADAPTER_1_NAME = 'osa adapter #1'
FAKED_ADAPTER_1_OID = 'fake-osa-adapter-1'
FAKED_ADAPTER_1_URI = '/api/adapters/' + FAKED_ADAPTER_1_OID
FAKED_ADAPTER_1_ID = '110'
FAKED_PORT_1_INDEX = 0
FAKED_PORT_1_NAME = 'Port #1'
FAKED_PORT_1_OID = 'fake-port-1'
FAKED_PORT_1_URI = '/api/adapters/' + FAKED_ADAPTER_1_OID + '/ports/' + \
    FAKED_PORT_1_OID
FAKED_VSWITCH_1_NAME = 'vswitch-1'
FAKED_VSWITCH_1_OID = 'fake-vswitch-1'
FAKED_VSWITCH_1_URI = '/api/virtual-switches/' + FAKED_VSWITCH_1_OID
FAKED_ADAPTER_1 = {
    'object-id': FAKED_ADAPTER_1_OID,
    'object-uri': FAKED_ADAPTER_1_URI,
    'parent': FAKED_CPC_1_URI,
    'class': 'adapter',
    'name': FAKED_ADAPTER_1_NAME,
    'description': 'OSA adapter #1',
    'type': 'osd',
    'adapter-family': 'osa',
    'port-count': 1,
    'network-port-uris': [FAKED_PORT_1_URI],
    'adapter-id': FAKED_ADAPTER_1_ID,
}
FAKED_PORT_1 = {
    'element-id': FAKED_PORT_1_OID,
    'element-uri': FAKED_PORT_1_URI,
    'parent': FAKED_ADAPTER_1_URI,
    'class': 'network-port',
    'name': FAKED_PORT_1_NAME,
    'description': 'Port #1 of OSA adapter #1',
    'index': FAKED_PORT_1_INDEX,
}
FAKED_VSWITCH_1 = {
    'object-id': FAKED_VSWITCH_1_OID,
    'object-uri': FAKED_VSWITCH_1_URI,
    'parent': FAKED_CPC_1_URI,
    'class': 'virtual-switch',
    'name': FAKED_VSWITCH_1_NAME,
    'description': 'vswitch for OSA adapter #1',
    'type': 'osd',
    'backing-adapter-uri': FAKED_ADAPTER_1_URI,
    'port': FAKED_PORT_1_INDEX,
}

# Faked OSA NIC that is used for these tests (for partition boot from storage).
# Most properties are set to their default values.
FAKED_NIC_1_NAME = 'nic-1'
FAKED_NIC_1_OID = 'fake-nic-1'
FAKED_NIC_1_URI = FAKED_PARTITION_1_URI + '/nics/' + FAKED_NIC_1_OID
FAKED_NIC_1 = {
    'element-id': FAKED_NIC_1_OID,
    'element-uri': FAKED_NIC_1_URI,
    'parent': FAKED_PARTITION_1_URI,
    'class': 'nic',
    'name': FAKED_NIC_1_NAME,
    'description': 'NIC #1',
    'device_number': '022F',
    'virtual-switch-uri': FAKED_VSWITCH_1_URI,
    'type': 'osd',
    'ssc-management-nic': False,
    'mac-address': 'fa:ce:da:dd:6e:55',
}

# Faked crypto adapters
# (with property names as specified in HMC data model)
FAKED_CRYPTO_ADAPTER_1 = {
    'object-id': 'crypto-adapter-oid-1',
    # We need object-uri for the assertions
    'object-uri': '/api/cpcs/cpc-oid-1/adapters/crypto-adapter-oid-1',
    'parent': '/api/cpcs/cpc-oid-1',
    'class': 'adapter',
    'name': 'crypto-adapter-name-1',
    'crypto-number': 1,
    'crypto-type': 'ep11-coprocessor',
    'udx-loaded': True,
    'description': 'Crypto adapter #1',
    'status': 'active',
    'type': 'crypto',
    'adapter-id': '02A',
    'adapter-family': 'crypto',
    'detected-card-type': 'crypto-express-5s',
    'card-location': 'vvvv-wwww',
    'state': 'online',
    'physical-channel-status': 'operating',
}
FAKED_CRYPTO_ADAPTER_2 = {
    'object-id': 'crypto-adapter-oid-2',
    # We need object-uri for the assertions
    'object-uri': '/api/cpcs/cpc-oid-1/adapters/crypto-adapter-oid-2',
    'parent': '/api/cpcs/cpc-oid-1',
    'class': 'adapter',
    'name': 'crypto-adapter-name-2',
    'crypto-number': 2,
    'crypto-type': 'cca-coprocessor',
    'udx-loaded': True,
    'description': 'Crypto adapter #2',
    'status': 'active',
    'type': 'crypto',
    'adapter-id': '02B',
    'adapter-family': 'crypto',
    'detected-card-type': 'crypto-express-5s',
    'card-location': 'vvvv-wwww',
    'state': 'online',
    'physical-channel-status': 'operating',
}

# Translation table from 'state' module input parameter to corresponding
# desired partition 'status' property value. 'None' means the partition
# does not exist.
PARTITION_STATUS_FROM_STATE = {
    'absent': None,
    'stopped': 'stopped',
    'active': 'active',
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
    Return the module output as a tuple (changed, partition_properties) (i.e.
    the arguments of the call to exit_json()).
    If the module failed, return None.
    """

    def func(changed, partition):
        return changed, partition

    if not mod_obj.exit_json.called:
        return None
    call_args = mod_obj.exit_json.call_args

    # The following makes sure we get the arguments regardless of whether they
    # were specified as positional or keyword arguments:
    return func(*call_args[0], **call_args[1])


CRYPTO_CONFIG_SUCCESS_TESTCASES = [
    (
        "No_change_to_empty_config",
        # adapters:
        [],
        # initial_config:
        None,
        # input_props:
        None,
        # exp_config:
        None,
        # exp_changed:
        False
    ),
    (
        "Add adapter to empty config",
        # adapters:
        [
            FAKED_CRYPTO_ADAPTER_1,
            FAKED_CRYPTO_ADAPTER_2,
        ],
        # initial_config:
        None,
        # input_props:
        dict(
            crypto_configuration=dict(
                crypto_adapter_names=[
                    FAKED_CRYPTO_ADAPTER_1['name'],
                ],
                crypto_domain_configurations=[
                ],
            ),
        ),
        # exp_config:
        {
            'crypto-adapter-uris': [
                FAKED_CRYPTO_ADAPTER_1['object-uri'],
            ],
            'crypto-domain-configurations': [
            ],
        },
        # exp_changed:
        True
    ),
    (
        "Add domain to empty config",
        # adapters:
        [
            FAKED_CRYPTO_ADAPTER_1,
            FAKED_CRYPTO_ADAPTER_2,
        ],
        # initial_config:
        None,
        # input_props:
        dict(
            crypto_configuration=dict(
                crypto_adapter_names=[
                ],
                crypto_domain_configurations=[
                    dict(domain_index=3, access_mode='control-usage'),
                ],
            ),
        ),
        # exp_config:
        {
            'crypto-adapter-uris': [
            ],
            'crypto-domain-configurations': [
                {'domain-index': 3, 'access-mode': 'control-usage'},
            ],
        },
        # exp_changed:
        True
    ),
    (
        "Add adapter+domain to empty config",
        # adapters:
        [
            FAKED_CRYPTO_ADAPTER_1,
            FAKED_CRYPTO_ADAPTER_2,
        ],
        # initial_config:
        None,
        # input_props:
        dict(
            crypto_configuration=dict(
                crypto_adapter_names=[
                    FAKED_CRYPTO_ADAPTER_1['name'],
                ],
                crypto_domain_configurations=[
                    dict(domain_index=3, access_mode='control-usage'),
                ],
            ),
        ),
        # exp_config:
        {
            'crypto-adapter-uris': [
                FAKED_CRYPTO_ADAPTER_1['object-uri'],
            ],
            'crypto-domain-configurations': [
                {'domain-index': 3, 'access-mode': 'control-usage'},
            ],
        },
        # exp_changed:
        True
    ),
    (
        "Change access mode of domain",
        # adapters:
        [
            FAKED_CRYPTO_ADAPTER_1,
            FAKED_CRYPTO_ADAPTER_2,
        ],
        # initial_config:
        {
            'crypto-adapter-uris': [
                FAKED_CRYPTO_ADAPTER_1['object-uri'],
            ],
            'crypto-domain-configurations': [
                {'domain-index': 3, 'access-mode': 'control'},
            ],
        },
        # input_props:
        dict(
            crypto_configuration=dict(
                crypto_adapter_names=[
                    FAKED_CRYPTO_ADAPTER_1['name'],
                ],
                crypto_domain_configurations=[
                    dict(domain_index=3, access_mode='control-usage'),
                ],
            ),
        ),
        # exp_config:
        {
            'crypto-adapter-uris': [
                FAKED_CRYPTO_ADAPTER_1['object-uri'],
            ],
            'crypto-domain-configurations': [
                {'domain-index': 3, 'access-mode': 'control-usage'},
            ],
        },
        # exp_changed:
        True
    ),
    (
        "No change to adapter+domain",
        # adapters:
        [
            FAKED_CRYPTO_ADAPTER_1,
            FAKED_CRYPTO_ADAPTER_2,
        ],
        # initial_config:
        {
            'crypto-adapter-uris': [
                FAKED_CRYPTO_ADAPTER_1['object-uri'],
            ],
            'crypto-domain-configurations': [
                {'domain-index': 2, 'access-mode': 'control-usage'},
            ],
        },
        # input_props:
        dict(
            crypto_configuration=dict(
                crypto_adapter_names=[
                    FAKED_CRYPTO_ADAPTER_1['name'],
                ],
                crypto_domain_configurations=[
                    dict(domain_index=2, access_mode='control-usage'),
                ],
            ),
        ),
        # exp_config:
        {
            'crypto-adapter-uris': [
                FAKED_CRYPTO_ADAPTER_1['object-uri'],
            ],
            'crypto-domain-configurations': [
                {'domain-index': 2, 'access-mode': 'control-usage'},
            ],
        },
        # exp_changed:
        False
    ),
    (
        "Add adapter to adapter+domain",
        # adapters:
        [
            FAKED_CRYPTO_ADAPTER_1,
            FAKED_CRYPTO_ADAPTER_2,
        ],
        # initial_config:
        {
            'crypto-adapter-uris': [
                FAKED_CRYPTO_ADAPTER_1['object-uri'],
            ],
            'crypto-domain-configurations': [
                {'domain-index': 2, 'access-mode': 'control-usage'},
            ],
        },
        # input_props:
        dict(
            crypto_configuration=dict(
                crypto_adapter_names=[
                    FAKED_CRYPTO_ADAPTER_1['name'],
                    FAKED_CRYPTO_ADAPTER_2['name'],
                ],
                crypto_domain_configurations=[
                    dict(domain_index=2, access_mode='control-usage'),
                ],
            ),
        ),
        # exp_config:
        {
            'crypto-adapter-uris': [
                FAKED_CRYPTO_ADAPTER_1['object-uri'],
                FAKED_CRYPTO_ADAPTER_2['object-uri'],
            ],
            'crypto-domain-configurations': [
                {'domain-index': 2, 'access-mode': 'control-usage'},
            ],
        },
        # exp_changed:
        True
    ),
    (
        "Add domain to adapter+domain",
        # adapters:
        [
            FAKED_CRYPTO_ADAPTER_1,
            FAKED_CRYPTO_ADAPTER_2,
        ],
        # initial_config:
        {
            'crypto-adapter-uris': [
                FAKED_CRYPTO_ADAPTER_1['object-uri'],
            ],
            'crypto-domain-configurations': [
                {'domain-index': 2, 'access-mode': 'control-usage'},
            ],
        },
        # input_props:
        dict(
            crypto_configuration=dict(
                crypto_adapter_names=[
                    FAKED_CRYPTO_ADAPTER_1['name'],
                ],
                crypto_domain_configurations=[
                    dict(domain_index=2, access_mode='control-usage'),
                    dict(domain_index=3, access_mode='control'),
                ],
            ),
        ),
        # exp_config:
        {
            'crypto-adapter-uris': [
                FAKED_CRYPTO_ADAPTER_1['object-uri'],
            ],
            'crypto-domain-configurations': [
                {'domain-index': 2, 'access-mode': 'control-usage'},
                {'domain-index': 3, 'access-mode': 'control'},
            ],
        },
        # exp_changed:
        True
    ),
    (
        "Add adapter+domain to adapter+domain",
        # adapters:
        [
            FAKED_CRYPTO_ADAPTER_1,
            FAKED_CRYPTO_ADAPTER_2,
        ],
        # initial_config:
        {
            'crypto-adapter-uris': [
                FAKED_CRYPTO_ADAPTER_1['object-uri'],
            ],
            'crypto-domain-configurations': [
                {'domain-index': 2, 'access-mode': 'control-usage'},
            ],
        },
        # input_props:
        dict(
            crypto_configuration=dict(
                crypto_adapter_names=[
                    FAKED_CRYPTO_ADAPTER_1['name'],
                    FAKED_CRYPTO_ADAPTER_2['name'],
                ],
                crypto_domain_configurations=[
                    dict(domain_index=2, access_mode='control-usage'),
                    dict(domain_index=3, access_mode='control'),
                ],
            ),
        ),
        # exp_config:
        {
            'crypto-adapter-uris': [
                FAKED_CRYPTO_ADAPTER_1['object-uri'],
                FAKED_CRYPTO_ADAPTER_2['object-uri'],
            ],
            'crypto-domain-configurations': [
                {'domain-index': 2, 'access-mode': 'control-usage'},
                {'domain-index': 3, 'access-mode': 'control'},
            ],
        },
        # exp_changed:
        True
    ),
    (
        "Remove adapter+domain from adapter+domain",
        # adapters:
        [
            FAKED_CRYPTO_ADAPTER_1,
            FAKED_CRYPTO_ADAPTER_2,
        ],
        # initial_config:
        {
            'crypto-adapter-uris': [
                FAKED_CRYPTO_ADAPTER_1['object-uri'],
            ],
            'crypto-domain-configurations': [
                {'domain-index': 2, 'access-mode': 'control-usage'},
            ],
        },
        # input_props:
        dict(
            crypto_configuration=dict(
                crypto_adapter_names=[
                ],
                crypto_domain_configurations=[
                ],
            ),
        ),
        # exp_config:
        {
            'crypto-adapter-uris': [
            ],
            'crypto-domain-configurations': [
            ],
        },
        # exp_changed:
        True
    ),
    (
        "Remove adapter+domain from 2 adapters + 2 domains",
        # adapters:
        [
            FAKED_CRYPTO_ADAPTER_1,
            FAKED_CRYPTO_ADAPTER_2,
        ],
        # initial_config:
        {
            'crypto-adapter-uris': [
                FAKED_CRYPTO_ADAPTER_1['object-uri'],
                FAKED_CRYPTO_ADAPTER_2['object-uri'],
            ],
            'crypto-domain-configurations': [
                {'domain-index': 2, 'access-mode': 'control-usage'},
                {'domain-index': 3, 'access-mode': 'control'},
            ],
        },
        # input_props:
        dict(
            crypto_configuration=dict(
                crypto_adapter_names=[
                    FAKED_CRYPTO_ADAPTER_1['name'],
                ],
                crypto_domain_configurations=[
                    dict(domain_index=2, access_mode='control-usage'),
                ],
            ),
        ),
        # exp_config:
        {
            'crypto-adapter-uris': [
                FAKED_CRYPTO_ADAPTER_1['object-uri'],
            ],
            'crypto-domain-configurations': [
                {'domain-index': 2, 'access-mode': 'control-usage'},
            ],
        },
        # exp_changed:
        True
    ),
    (
        "Check domain index numbers provided as strings",
        # adapters:
        [
            FAKED_CRYPTO_ADAPTER_1,
            FAKED_CRYPTO_ADAPTER_2,
        ],
        # initial_config:
        {
            'crypto-adapter-uris': [
                FAKED_CRYPTO_ADAPTER_1['object-uri'],
                FAKED_CRYPTO_ADAPTER_2['object-uri'],
            ],
            'crypto-domain-configurations': [
                {'domain-index': 2, 'access-mode': 'control-usage'},
                {'domain-index': 3, 'access-mode': 'control'},
            ],
        },
        # input_props:
        dict(
            crypto_configuration=dict(
                crypto_adapter_names=[
                    FAKED_CRYPTO_ADAPTER_1['name'],
                ],
                crypto_domain_configurations=[
                    # Here we provide the domain index as a string:
                    dict(domain_index="2", access_mode='control-usage'),
                ],
            ),
        ),
        # exp_config:
        {
            'crypto-adapter-uris': [
                FAKED_CRYPTO_ADAPTER_1['object-uri'],
            ],
            'crypto-domain-configurations': [
                {'domain-index': 2, 'access-mode': 'control-usage'},
            ],
        },
        # exp_changed:
        True
    ),
]


class TestPartition(object):
    """
    All tests for partitions.
    """

    def setup_method(self):
        """
        Using the zhmcclient mock support, set up a CPC in DPM mode, that has
        no partitions.
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

    def setup_partition(self, initial_state, additional_props=None):
        """
        Prepare the faked partition, on top of the CPC created by
        setup_method().
        """
        self.partition_name = FAKED_PARTITION_1_NAME
        if initial_state in ('stopped', 'active'):
            # Create the partition (it is in stopped state by default)
            partition_props = FAKED_PARTITION_1.copy()
            if additional_props:
                partition_props.update(additional_props)
            self.faked_partition = self.faked_cpc.partitions.add(
                partition_props)
            partitions = self.cpc.partitions.list()
            assert len(partitions) == 1
            self.partition = partitions[0]
            if initial_state == 'active':
                self.partition.start()
            self.partition.pull_full_properties()
        else:
            self.faked_partition = None
            self.partition = None

    def setup_hba(self):
        """
        Prepare the faked HBA, on top of the faked partition created by
        setup_partition().
        """
        self.hba_name = FAKED_HBA_1_NAME
        if self.partition:
            # Create the HBA
            self.faked_hba = self.faked_partition.hbas.add(FAKED_HBA_1)
            hbas = self.partition.hbas.list(full_properties=True)
            assert len(hbas) == 1
            self.hba = hbas[0]
        else:
            self.faked_hba = None
            self.hba = None

    def setup_nic(self):
        """
        Prepare the faked NIC, on top of the faked partition created by
        setup_partition().
        """
        self.faked_adapter = self.faked_cpc.adapters.add(FAKED_ADAPTER_1)
        self.faked_vswitch = self.faked_cpc.virtual_switches.add(
            FAKED_VSWITCH_1)
        self.nic_name = FAKED_NIC_1_NAME
        if self.partition:
            # Create the NIC
            self.faked_nic = self.faked_partition.nics.add(FAKED_NIC_1)
            nics = self.partition.nics.list(full_properties=True)
            assert len(nics) == 1
            self.nic = nics[0]
        else:
            self.faked_nic = None
            self.nic = None

    def setup_crypto_adapter(self, adapter_props):
        """
        Prepare a faked crypto adapter, on top of the faked CPC created by
        setup_method().
        """
        faked_adapter = self.faked_cpc.adapters.add(adapter_props)
        self.faked_crypto_adapters.append(faked_adapter)
        self.faked_crypto_adapter_names.append(
            faked_adapter.properties['name'])

    @pytest.mark.parametrize(
        "check_mode", [False, True])
    @pytest.mark.parametrize(
        "initial_state", ['stopped', 'active'])
    @pytest.mark.parametrize(
        "desired_state", ['facts'])
    @pytest.mark.parametrize(
        "expand_storage_groups", [False, True])
    @pytest.mark.parametrize(
        "expand_crypto_adapters", [False, True])
    @mock.patch("plugins.modules.zhmc_partition.AnsibleModule",
                autospec=True)
    def test_facts_success(
            self, ansible_mod_cls, expand_crypto_adapters,
            expand_storage_groups, desired_state, initial_state, check_mode):
        """
        Tests for successful fact gathering on partitions, dependent on
        parametrization.
        """

        # Prepare the initial partition before the test is run
        self.setup_partition(initial_state)
        self.setup_hba()
        self.setup_nic()

        # Prepare module input parameters
        params = {
            'hmc_host': 'fake-host',
            'hmc_auth': dict(userid='fake-userid',
                             password='fake-password'),
            'cpc_name': self.cpc.name,
            'name': self.partition_name,
            'state': desired_state,
            'expand_storage_groups': expand_storage_groups,
            'expand_crypto_adapters': expand_crypto_adapters,
            'log_file': None,
            '_faked_session': self.session,
        }

        # Prepare mocks for AnsibleModule object
        mod_obj = mock_ansible_module(ansible_mod_cls, params, check_mode)

        # Exercise the code to be tested
        with pytest.raises(SystemExit) as exc_info:
            zhmc_partition.main()
        exit_code = exc_info.value.args[0]

        # Assert module exit code
        assert exit_code == 0, \
            "Module unexpectedly failed with this message:\n{0}". \
            format(get_failure_msg(mod_obj))

        # Assert module output
        changed, part_props = get_module_output(mod_obj)
        assert changed is False
        assert isinstance(part_props, dict)
        assert 'nics' in part_props
        assert 'hbas' in part_props
        assert 'virtual-functions' in part_props
        if expand_storage_groups:
            assert 'storage-groups' in part_props
        else:
            assert 'storage-groups' not in part_props
        if part_props['crypto-configuration']:
            if expand_crypto_adapters:
                assert 'crypto-adapters' in part_props['crypto-configuration']
            else:
                assert 'crypto-adapters' not in \
                    part_props['crypto-configuration']
        for pname in part_props:
            pvalue = part_props[pname]
            if pname == 'nics':
                assert len(pvalue) == 1
                nic_props = pvalue[0]
                exp_nic_props = dict(self.nic.properties)
                exp_nic_props['adapter-name'] = FAKED_ADAPTER_1_NAME
                exp_nic_props['adapter-port'] = FAKED_PORT_1_INDEX
                exp_nic_props['adapter-id'] = FAKED_ADAPTER_1_ID
                assert nic_props == exp_nic_props
            elif pname == 'hbas':
                assert len(pvalue) == 1
                hba_props = pvalue[0]
                assert hba_props == self.hba.properties
            elif pname == 'virtual-functions':
                assert len(pvalue) == 0
            elif pname == 'storage-groups':
                assert len(pvalue) == 0  # Not set up
            else:
                if pname == 'crypto-configuration' and pvalue and \
                        'crypto-adapters' in pvalue:
                    ca_value = pvalue['crypto-adapters']
                    assert len(ca_value) == 0  # Not set up
                exp_value = self.partition.properties[pname]
                assert pvalue == exp_value

    @pytest.mark.parametrize(
        "check_mode", [False])
    @pytest.mark.parametrize(
        "initial_state", ['absent', 'stopped', 'active'])
    @pytest.mark.parametrize(
        "desired_state", ['stopped', 'active'])
    @pytest.mark.parametrize(
        "properties, test_when_created, test_when_modified", [
            # invalid properties (according to data model):
            ({None: 1}, True, True),
            ({'': 1}, True, True),
            ({'boo_invalid_prop': 1}, True, True),
            # valid properties specified with hyphens instead of underscores:
            ({'ifl-processors': 4}, True, True),
            # properties provided as module input parameter:
            ({'name': 'new-name'}, True, True),
            # create-only properties (tested only when modified):
            ({'type': 'ssc'}, False, True),
            # properties handled via their artificial properties:
            ({'boot_network_device': '/api/faked-nic-uri'}, True, True),
            ({'boot_storage_device': '/api/faked-hba-uri'}, True, True),
            # update-only properties (tested only when created):
            ({'boot_network_nic_name': 'faked-nic-name'}, True, False),
            ({'boot_storage_hba_name': 'faked-hba-name'}, True, False),
            # read-only properties:
            ({'object_uri': '/api/fake-partition-uri'}, True, True),
            ({'object_id': 'fake-oid'}, True, True),
            ({'parent': 'fake-parent'}, True, True),
            ({'class': 'fake-partition'}, True, True),
            ({'status': 'new-status'}, True, True),
            ({'has_unacceptable_status': False}, True, True),
            ({'is_locked': False}, True, True),
            ({'os_name': 'MyLinux'}, True, True),
            ({'os_type': 'Linux'}, True, True),
            ({'os_version': '3.10'}, True, True),
            ({'degraded_adapters': ''}, True, True),
            ({'current_ifl_processing_weight': 50}, True, True),
            ({'current_cp_processing_weight': 50}, True, True),
            ({'reserved_memory': 1024}, True, True),
            ({'auto_start': True}, True, True),
            ({'boot_iso_image_name': 'fake-iso-image-name'}, True, True),
            ({'threads_per_processor': 2}, True, True),
            ({'virtual_function_uris': ['/api/fake-vf-uri']}, True, True),
            ({'nic_uris': ['/api/fake-nic-uri']}, True, True),
            ({'hba_uris': ['/api/fake-hba-uri']}, True, True),
        ])
    @mock.patch("plugins.modules.zhmc_partition.AnsibleModule",
                autospec=True)
    def test_error_properties(
            self, ansible_mod_cls, properties, test_when_created,
            test_when_modified, desired_state, initial_state, check_mode):
        """
        Test a property in the 'properties' module input parameter that is
        valid according to the data model, but not allowed for some reason.

        The invalidity is detected by the Ansible module, causing a module
        failure to be indicated with a "ParameterError" failure message.
        """

        # Skip tests for properties that are not to be tested when the
        # partition is being created or is being modified.
        is_created = (initial_state in ('absent',) and
                      desired_state in ('stopped', 'active'))
        if is_created and not test_when_created:
            return
        is_modified = (initial_state in ('stopped', 'active') and
                       desired_state in ('stopped', 'active'))
        if is_modified and not test_when_modified:
            return

        # Prepare the initial partition before the test is run
        self.setup_partition(initial_state)

        # Set up required input properties:
        props = properties.copy()
        if 'ifl_processors' not in props and 'cp_processors' not in props:
            props['ifl_processors'] = 1
        if 'initial_memory' not in props:
            props['initial_memory'] = 512
        if 'maximum_memory' not in props:
            props['maximum_memory'] = 512

        # Prepare module input parameters
        params = {
            'hmc_host': 'fake-host',
            'hmc_auth': dict(userid='fake-userid',
                             password='fake-password'),
            'cpc_name': self.cpc.name,
            'name': self.partition_name,
            'state': desired_state,
            'properties': props,
            'expand_storage_groups': False,
            'expand_crypto_adapters': False,
            'log_file': None,
            '_faked_session': self.session,
        }

        # Prepare mocks for AnsibleModule object
        mod_obj = mock_ansible_module(ansible_mod_cls, params, check_mode)

        # Exercise the code to be tested
        with pytest.raises(SystemExit) as exc_info:
            zhmc_partition.main()
        exit_code = exc_info.value.args[0]

        # Assert module exit code
        assert exit_code == 1, \
            "Module unexpectedly succeeded with this output:\n" \
            "changed: {!r}, partition: {!r}". \
            format(*get_module_output(mod_obj))

        # Assert the failure message
        msg = get_failure_msg(mod_obj)
        assert msg.startswith("ParameterError:")

    @pytest.mark.parametrize(
        "check_mode", [False, True])
    @pytest.mark.parametrize(
        "initial_state", ['stopped', 'active'])
    @pytest.mark.parametrize(
        "desired_state", ['stopped', 'active'])
    @mock.patch("plugins.modules.zhmc_partition.AnsibleModule",
                autospec=True)
    def test_boot_storage_success(
            self, ansible_mod_cls, desired_state, initial_state, check_mode):
        """
        Tests for successful configuration of boot from storage.
        """

        # Prepare the initial partition and HBA before the test is run
        self.setup_partition(initial_state)
        assert self.partition
        self.setup_hba()

        # Set some expectations for this test from its parametrization
        exp_status = (PARTITION_STATUS_FROM_STATE[initial_state] if check_mode
                      else PARTITION_STATUS_FROM_STATE[desired_state])

        properties = {
            'boot_device': 'storage-adapter',
            'boot_storage_hba_name': self.hba_name,  # artif. prop.
            'boot_logical_unit_number': '0002',
            'boot_world_wide_port_name': '1023456789abcdef',
        }

        exp_properties = {
            'boot_device': 'storage-adapter',
            'boot_storage_device': self.hba.uri,  # real prop for artif. prop.
            'boot_logical_unit_number': '0002',
            'boot_world_wide_port_name': '1023456789abcdef',
        }

        # Prepare module input parameters
        params = {
            'hmc_host': 'fake-host',
            'hmc_auth': dict(userid='fake-userid',
                             password='fake-password'),
            'cpc_name': self.cpc.name,
            'name': self.partition_name,
            'state': desired_state,
            'properties': properties,
            'expand_storage_groups': False,
            'expand_crypto_adapters': False,
            'log_file': None,
            '_faked_session': self.session,
        }

        # Prepare mocks for AnsibleModule object
        mod_obj = mock_ansible_module(ansible_mod_cls, params, check_mode)

        # Exercise the code to be tested
        with pytest.raises(SystemExit) as exc_info:
            zhmc_partition.main()
        exit_code = exc_info.value.args[0]

        # Assert module exit code
        assert exit_code == 0, \
            "Module unexpectedly failed with this message:\n{0}". \
            format(get_failure_msg(mod_obj))

        # Assert module output
        changed, part_props = get_module_output(mod_obj)
        assert changed
        assert part_props != {}
        if not check_mode:
            assert part_props['status'] == exp_status
            assert part_props['name'] == params['name']
            for prop_name, exp_value in exp_properties.items():
                hmc_prop_name = prop_name.replace('_', '-')
                assert part_props[hmc_prop_name] == exp_value, \
                    "Property: {0}".format(prop_name)

        # Assert the partition resource
        if not check_mode:
            parts = self.cpc.partitions.list()
            assert len(parts) == 1
            part = parts[0]
            part.pull_full_properties()
            assert part.properties['status'] == exp_status
            assert part.properties['name'] == params['name']
            for prop_name, exp_value in exp_properties.items():
                hmc_prop_name = prop_name.replace('_', '-')
                assert part.properties[hmc_prop_name] == exp_value, \
                    "Property: {0}".format(prop_name)

    @pytest.mark.parametrize(
        "check_mode", [False, True])
    @pytest.mark.parametrize(
        "initial_state", ['stopped', 'active'])
    @pytest.mark.parametrize(
        "desired_state", ['stopped', 'active'])
    @mock.patch("plugins.modules.zhmc_partition.AnsibleModule",
                autospec=True)
    def test_boot_storage_error_hba_not_found(
            self, ansible_mod_cls, desired_state, initial_state, check_mode):
        """
        Tests for successful configuration of boot from storage.
        """

        # Prepare the initial partition and HBA before the test is run
        self.setup_partition(initial_state)
        assert self.partition
        self.setup_hba()

        properties = {
            'boot_device': 'storage-adapter',
            'boot_storage_hba_name': 'invalid-hba-name',  # artif. prop.
            'boot_logical_unit_number': '0002',
            'boot_world_wide_port_name': '1023456789abcdef',
        }

        # Prepare module input parameters
        params = {
            'hmc_host': 'fake-host',
            'hmc_auth': dict(userid='fake-userid',
                             password='fake-password'),
            'cpc_name': self.cpc.name,
            'name': self.partition_name,
            'state': desired_state,
            'properties': properties,
            'expand_storage_groups': False,
            'expand_crypto_adapters': False,
            'log_file': None,
            '_faked_session': self.session,
        }

        # Prepare mocks for AnsibleModule object
        mod_obj = mock_ansible_module(ansible_mod_cls, params, check_mode)

        # Exercise the code to be tested
        with pytest.raises(SystemExit) as exc_info:
            zhmc_partition.main()
        exit_code = exc_info.value.args[0]

        # Assert module exit code
        assert exit_code == 1, \
            "Module unexpectedly succeeded with this output:\n" \
            "changed: {!r}, partition: {!r}". \
            format(*get_module_output(mod_obj))

        # Assert the failure message
        msg = get_failure_msg(mod_obj)
        assert msg.startswith("ParameterError:")

    @pytest.mark.parametrize(
        "check_mode", [False, True])
    @pytest.mark.parametrize(
        "initial_state", ['stopped', 'active'])
    @pytest.mark.parametrize(
        "desired_state", ['stopped', 'active'])
    @mock.patch("plugins.modules.zhmc_partition.AnsibleModule",
                autospec=True)
    def test_boot_network_success(
            self, ansible_mod_cls, desired_state, initial_state, check_mode):
        """
        Tests for successful configuration of boot from network.
        """

        # Prepare the initial partition and HBA before the test is run
        self.setup_partition(initial_state)
        assert self.partition
        self.setup_nic()

        # Set some expectations for this test from its parametrization
        exp_status = (PARTITION_STATUS_FROM_STATE[initial_state] if check_mode
                      else PARTITION_STATUS_FROM_STATE[desired_state])

        properties = {
            'boot_device': 'network-adapter',
            'boot_network_nic_name': self.nic_name,  # artif. prop.
        }

        exp_properties = {
            'boot_device': 'network-adapter',
            'boot_network_device': self.nic.uri,  # real prop for artif. prop.
        }

        # Prepare module input parameters
        params = {
            'hmc_host': 'fake-host',
            'hmc_auth': dict(userid='fake-userid',
                             password='fake-password'),
            'cpc_name': self.cpc.name,
            'name': self.partition_name,
            'state': desired_state,
            'properties': properties,
            'expand_storage_groups': False,
            'expand_crypto_adapters': False,
            'log_file': None,
            '_faked_session': self.session,
        }

        # Prepare mocks for AnsibleModule object
        mod_obj = mock_ansible_module(ansible_mod_cls, params, check_mode)

        # Exercise the code to be tested
        with pytest.raises(SystemExit) as exc_info:
            zhmc_partition.main()
        exit_code = exc_info.value.args[0]

        # Assert module exit code
        assert exit_code == 0, \
            "Module unexpectedly failed with this message:\n{0}". \
            format(get_failure_msg(mod_obj))

        # Assert module output
        changed, part_props = get_module_output(mod_obj)
        assert changed
        assert part_props != {}
        if not check_mode:
            assert part_props['status'] == exp_status
            assert part_props['name'] == params['name']
            for prop_name, exp_value in exp_properties.items():
                hmc_prop_name = prop_name.replace('_', '-')
                assert part_props[hmc_prop_name] == exp_value, \
                    "Property: {0}".format(prop_name)

        # Assert the partition resource
        if not check_mode:
            parts = self.cpc.partitions.list()
            assert len(parts) == 1
            part = parts[0]
            part.pull_full_properties()
            assert part.properties['status'] == exp_status
            assert part.properties['name'] == params['name']
            for prop_name, exp_value in exp_properties.items():
                hmc_prop_name = prop_name.replace('_', '-')
                assert part.properties[hmc_prop_name] == exp_value, \
                    "Property: {0}".format(prop_name)

    @pytest.mark.parametrize(
        "check_mode", [False, True])
    @pytest.mark.parametrize(
        "initial_state", ['stopped', 'active'])
    @pytest.mark.parametrize(
        "desired_state", ['stopped', 'active'])
    @mock.patch("plugins.modules.zhmc_partition.AnsibleModule",
                autospec=True)
    def test_boot_network_error_hba_not_found(
            self, ansible_mod_cls, desired_state, initial_state, check_mode):
        """
        Tests for successful configuration of boot from network.
        """

        # Prepare the initial partition and HBA before the test is run
        self.setup_partition(initial_state)
        assert self.partition
        self.setup_nic()

        properties = {
            'boot_device': 'network-adapter',
            'boot_network_nic_name': 'invalid-nic-name',  # artif. prop.
        }

        # Prepare module input parameters
        params = {
            'hmc_host': 'fake-host',
            'hmc_auth': dict(userid='fake-userid',
                             password='fake-password'),
            'cpc_name': self.cpc.name,
            'name': self.partition_name,
            'state': desired_state,
            'properties': properties,
            'expand_storage_groups': False,
            'expand_crypto_adapters': False,
            'log_file': None,
            '_faked_session': self.session,
        }

        # Prepare mocks for AnsibleModule object
        mod_obj = mock_ansible_module(ansible_mod_cls, params, check_mode)

        # Exercise the code to be tested
        with pytest.raises(SystemExit) as exc_info:
            zhmc_partition.main()
        exit_code = exc_info.value.args[0]

        # Assert module exit code
        assert exit_code == 1, \
            "Module unexpectedly succeeded with this output:\n" \
            "changed: {0!r}, partition: {1!r}". \
            format(*get_module_output(mod_obj))

        # Assert the failure message
        msg = get_failure_msg(mod_obj)
        assert msg.startswith("ParameterError:")

    @pytest.mark.parametrize(
        "check_mode", [False, True])
    @pytest.mark.parametrize(
        # We omit initial state 'absent' due to limitations in the mock support
        # (when creating partitions, it does not populate them with all
        # properties).
        "initial_state", ['stopped', 'active'])
    @pytest.mark.parametrize(
        "desired_state", ['stopped', 'active'])
    @pytest.mark.parametrize(
        "desc, adapters, initial_config, input_props, exp_config, exp_changed",
        CRYPTO_CONFIG_SUCCESS_TESTCASES)
    @mock.patch("plugins.modules.zhmc_partition.AnsibleModule",
                autospec=True)
    def test_crypto_config_success(
            self, ansible_mod_cls, desc, adapters, initial_config, input_props,
            exp_config, exp_changed, desired_state, initial_state, check_mode):
        """
        Tests for successful crypto configuration.
        """

        # Prepare the initial partition and crypto adapters
        self.setup_partition(initial_state,
                             {'crypto-configuration': initial_config})
        for adapter_props in adapters:
            self.setup_crypto_adapter(adapter_props)

        # Set some expectations for this test from its parametrization
        exp_status = PARTITION_STATUS_FROM_STATE[desired_state]

        # Adjust expected changes - the exp_changed argument only indicates the
        # expectation for changes to the crypto config property, so we add
        # the effect of other changes.
        if desired_state != initial_state:
            exp_changed = True

        properties = input_props

        if self.partition:
            self.partition.pull_full_properties()
            exp_properties = dict(self.partition.properties)
        else:
            exp_properties = {}
        exp_properties['crypto-configuration'] = exp_config

        # Prepare module input parameters
        params = {
            'hmc_host': 'fake-host',
            'hmc_auth': dict(userid='fake-userid',
                             password='fake-password'),
            'cpc_name': self.cpc.name,
            'name': self.partition_name,
            'state': desired_state,
            'properties': properties,
            'expand_storage_groups': False,
            'expand_crypto_adapters': False,
            'log_file': None,
            '_faked_session': self.session,
        }

        # Prepare mocks for AnsibleModule object
        mod_obj = mock_ansible_module(ansible_mod_cls, params, check_mode)

        # Exercise the code to be tested
        with pytest.raises(SystemExit) as exc_info:
            zhmc_partition.main()
        exit_code = exc_info.value.args[0]

        # Assert module exit code
        assert exit_code == 0, \
            "Module unexpectedly failed with this message:\n{0}". \
            format(get_failure_msg(mod_obj))

        # Assert module output
        changed, part_props = get_module_output(mod_obj)
        assert changed == exp_changed
        assert part_props != {}
        assert part_props['status'] == exp_status
        assert part_props['name'] == params['name']
        for prop_name in exp_properties:

            # Because we built the expected properties from the initial
            # properties (adding the crypto_config property we test),
            # we need to skip the 'status' property (it would still show
            # the initial value).
            if prop_name == 'status':
                continue

            hmc_prop_name = prop_name.replace('_', '-')
            assert hmc_prop_name in part_props
            result_property = part_props[hmc_prop_name]
            exp_property = exp_properties[prop_name]
            assert result_property == exp_property, \
                "Property: {0}".format(prop_name)

        # Assert the partition resource
        if not check_mode:
            parts = self.cpc.partitions.list()
            assert len(parts) == 1
            part = parts[0]
            part.pull_full_properties()
            assert part.properties['status'] == exp_status
            assert part.properties['name'] == params['name']
            for prop_name in exp_properties:

                # Because we built the expected properties from the initial
                # properties (adding the crypto_config property we test),
                # we need to skip the 'status' property (it would still show
                # the initial value).
                if prop_name == 'status':
                    continue

                hmc_prop_name = prop_name.replace('_', '-')
                assert hmc_prop_name in part.properties
                part_property = part.properties[hmc_prop_name]
                exp_property = exp_properties[prop_name]
                assert part_property == exp_property, \
                    "Property: {0}".format(prop_name)

    @pytest.mark.parametrize(
        "check_mode", [False, True])
    @pytest.mark.parametrize(
        # We omit initial state 'absent' due to limitations in the mock support
        # (when creating partitions, it does not populate them with all
        # properties).
        "initial_state", ['stopped', 'active'])
    @pytest.mark.parametrize(
        "desired_state", ['stopped', 'active'])
    @pytest.mark.parametrize(
        "adapters, initial_config", [
            (
                [
                    FAKED_CRYPTO_ADAPTER_1,
                    FAKED_CRYPTO_ADAPTER_2,
                ],
                {
                    'crypto-adapter-uris': [
                        FAKED_CRYPTO_ADAPTER_1['object-uri'],
                    ],
                    'crypto-domain-configurations': [
                        {'domain-index': 3, 'access-mode': 'control'},
                    ],
                },
            ),
        ])
    @pytest.mark.parametrize(
        "input_props, error_msg_pattern", [
            (
                dict(
                    crypto_configuration='abc',  # error: no dictionary
                ),
                "ParameterError: .*",
            ),
            (
                dict(
                    crypto_configuration=dict(
                        # error: no crypto_adapter_names field
                        crypto_domain_configurations=[
                            dict(domain_index=3, access_mode='control-usage'),
                        ],
                    ),
                ),
                "ParameterError: .*crypto_adapter_names.*",
            ),
            (
                dict(
                    crypto_configuration=dict(
                        crypto_adapter_names=[
                            'invalid-adapter-name',  # error: not found
                        ],
                        crypto_domain_configurations=[
                            dict(domain_index=3, access_mode='control-usage'),
                        ],
                    ),
                ),
                "ParameterError: .*invalid-adapter-name.*",
            ),
            (
                dict(
                    crypto_configuration=dict(
                        crypto_adapter_names=[
                            FAKED_CRYPTO_ADAPTER_1['name'],
                        ],
                        # error: no crypto_domain_configurations field
                    ),
                ),
                "ParameterError: .*crypto_domain_configurations.*",
            ),
            (
                dict(
                    crypto_configuration=dict(
                        crypto_adapter_names=[
                            FAKED_CRYPTO_ADAPTER_1['name'],
                        ],
                        crypto_domain_configurations=[
                            dict(access_mode='control-usage'),
                            # error: no domain_index field
                        ],
                    ),
                ),
                "ParameterError: .*domain_index.*",
            ),
            (
                dict(
                    crypto_configuration=dict(
                        crypto_adapter_names=[
                            FAKED_CRYPTO_ADAPTER_1['name'],
                        ],
                        crypto_domain_configurations=[
                            dict(domain_index=3),
                            # error: no access_mode field
                        ],
                    ),
                ),
                "ParameterError: .*access_mode.*",
            ),
        ])
    @mock.patch("plugins.modules.zhmc_partition.AnsibleModule",
                autospec=True)
    def test_crypto_config_parm_errors(
            self, ansible_mod_cls, input_props, error_msg_pattern, adapters,
            initial_config, desired_state, initial_state, check_mode):
        """
        Tests for 'crypto_configuration' property with parameter errors.
        """

        # Prepare the initial partition and crypto adapters
        self.setup_partition(initial_state,
                             {'crypto-configuration': initial_config})
        for adapter_props in adapters:
            self.setup_crypto_adapter(adapter_props)

        # Prepare module input parameters
        params = {
            'hmc_host': 'fake-host',
            'hmc_auth': dict(userid='fake-userid',
                             password='fake-password'),
            'cpc_name': self.cpc.name,
            'name': self.partition_name,
            'state': desired_state,
            'properties': input_props,
            'expand_storage_groups': False,
            'expand_crypto_adapters': False,
            'log_file': None,
            '_faked_session': self.session,
        }

        # Prepare mocks for AnsibleModule object
        mod_obj = mock_ansible_module(ansible_mod_cls, params, check_mode)

        # Exercise the code to be tested
        with pytest.raises(SystemExit) as exc_info:
            zhmc_partition.main()
        exit_code = exc_info.value.args[0]

        # Assert module exit code
        assert exit_code == 1, \
            "Module unexpectedly succeeded with this output:\n" \
            "changed: {0!r}, partition: {1!r}". \
            format(*get_module_output(mod_obj))

        # Assert the failure message
        msg = get_failure_msg(mod_obj)
        pattern = r'^{0}$'.format(error_msg_pattern)
        assert re.match(pattern, msg)
