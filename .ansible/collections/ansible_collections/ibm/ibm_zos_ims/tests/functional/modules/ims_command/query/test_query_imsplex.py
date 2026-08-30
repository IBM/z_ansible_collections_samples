# -*- coding: utf-8 -*-

from __future__ import (absolute_import, division, print_function)
from ansible_collections.ibm.ibm_zos_ims.plugins.module_utils.ims_module_error_messages import ErrorMessages as em  # pylint: disable=import-error

import pytest
# from pprint import pprint

__metaclass__ = type

PLEX = "PLEX1"
SUCCESSFUL_RC = '00000000'
SUCCESSFUL_CC = '0'


test_data_positive = [
    ('QUERY IMSPLEX'),
    ('QUERY IMSPLEX NAME(C*)'),
    ('QUERY IMSPLEX TYPE(IMS)'),
    ('QUERY IMSPLEX STATUS(READY)'),
    ('QUERY IMSPLEX SHOW(ALL)'),
]


@pytest.mark.parametrize("cmd", test_data_positive)
def test_query_imsplex_positive(ansible_zos_module, cmd):
    hosts = ansible_zos_module
    response = hosts.all.ims_command(command=cmd, plex=PLEX)
    for result in response.contacted.values():
        # pprint(result);
        assert(int(result['ims_output'][0]['command_return']['ctl.rc']) == 0)
        for i in range(0, len(result['ims_output'][0]['type_2_response'])):
            a = result['ims_output'][0]['type_2_response'][i]['CC']
            assert(int(a) == 0)


test_data_negative = [
    ('QUERY IMSPLEX NAME(ZZZ)'),   # non-existent
    ('QUERY IMSPLEX NAME(AABBCCDDE)'),   # >8 characters
    ('QUERY IMSPLEX TYPE(*)'),   # wild card - not allowed
    ('QUERY IMSPLEX TYPE(ABC)'),   # other invalid option
    ('QUERY IMSPLEX STATUS(ABC)'),   # invalid option
    ('QUERY IMSPLEX SHOW(ZZZ)')   # non-existent
]


@pytest.mark.parametrize("cmd", test_data_negative)
def test_query_imsplex_negative(ansible_zos_module, cmd):
    hosts = ansible_zos_module
    response = hosts.all.ims_command(command=cmd, plex=PLEX)
    for result in response.contacted.values():
        assert result['ims_output'][0]['msg'] == em.NON_ZERO_RC_MSG


def test_batch_query_imsplex_basic(ansible_zos_module):
    hosts = ansible_zos_module
    batch_list = [{"command": "QUERY IMSPLEX", "plex": PLEX}]
    response = hosts.all.ims_command(batch=batch_list)
    for result in response.contacted.values():
        assert result['ims_output'][0]['command_return']['ctl.rc'] == SUCCESSFUL_RC
        assert 'type_2_response' in result['ims_output'][0]


def test_batch_query_imsplex_multiple(ansible_zos_module):
    hosts = ansible_zos_module
    batch_list = [
        {"command": "QUERY IMSPLEX", "plex": PLEX},
        {"command": "QUERY IMSPLEX NAME(C*)", "plex": PLEX},
        {"command": "QUERY IMSPLEX TYPE(IMS)", "plex": PLEX},
        {"command": "QUERY IMSPLEX STATUS(READY)", "plex": PLEX},
        {"command": "QUERY IMSPLEX SHOW(ALL)", "plex": PLEX}
    ]
    response = hosts.all.ims_command(batch=batch_list)
    for result in response.contacted.values():
        for output in result['ims_output']:
            assert int(output['command_return']['ctl.rc']) == 0
            for response in output['type_2_response']:
                assert int(response['CC']) == 0
