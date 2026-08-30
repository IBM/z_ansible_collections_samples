# -*- coding: utf-8 -*-

from __future__ import (absolute_import, division, print_function)
from ansible_collections.ibm.ibm_zos_ims.plugins.module_utils.ims_module_error_messages import ErrorMessages as em  # pylint: disable=import-error
from pprint import pprint
import pytest

__metaclass__ = type

PLEX = "PLEX1"
ROUTE = "IMS1"

invalid_query_params = [
    ('QUERY haha PGM NAME(A*)', PLEX, ROUTE),  # invalid attr in query pgm
    ('QUERY PGMPGMPGM SHOW(ALL)', PLEX, ROUTE),  # invalid query member
    ('QUERY', PLEX, ROUTE),  # no args specified in query command
    ('QRY TRAN NAME(A*) NAME(B*)', PLEX, ROUTE)  # invalid train tracks
]


@pytest.mark.parametrize("command, plex, route", invalid_query_params)
def test_invalid_query(ansible_zos_module, command, plex, route):
    hosts = ansible_zos_module
    response = hosts.all.ims_command(command=command, plex=plex, route=route)
    for result in response.contacted.values():
        assert result['ims_output'][0]['msg'] == em.NON_ZERO_RC_MSG


invalid_char_params = [
    ('QUERY ""$%^#"" P#$GM NAME(A*)', PLEX, ROUTE),
    ("QUERY PGM SHOW(ALL) theIMScmd '~' IMS1\nSay 'ATTMPTED INJECTION'", PLEX, ROUTE)
]


@pytest.mark.parametrize("command, plex, route", invalid_char_params)
def test_invalid_command_characters(ansible_zos_module, command, plex, route):
    hosts = ansible_zos_module
    response = hosts.all.ims_command(command=command, plex=plex, route=route)
    for result in response.contacted.values():
        pprint(result)
        msg = result['ims_output'][0]['msg']
        err = result['ims_output'][0]['err']
        assert (msg == em.NON_ZERO_RC_MSG) or (err == em.INVALID_CHAR_IN_CMD)


def test_malformed_command(ansible_zos_module):
    hosts = ansible_zos_module
    response = hosts.all.ims_command(command='QUY PGM SHOW(ALL)', plex=PLEX, route=ROUTE)
    for result in response.contacted.values():
        assert result['ims_output'][0]['msg'] == em.NON_ZERO_RC_MSG


def test_missing_command(ansible_zos_module):
    hosts = ansible_zos_module
    response = hosts.all.ims_command(plex=PLEX, route=ROUTE)
    for result in response.contacted.values():
        assert result['ims_output'][0]['msg'] == em.MISSING_COMMAND


def test_batch_missing_command(ansible_zos_module):
    hosts = ansible_zos_module
    batch_list = [{"plex": PLEX, "route": ROUTE}]
    response = hosts.all.ims_command(batch=batch_list)
    for result in response.contacted.values():
        assert em.MISSING_COMMAND.lower() in result['msg']
        assert result['changed'] is False


def test_batch_malformed_command_multiple(ansible_zos_module):
    hosts = ansible_zos_module
    # Fails on first incorrect command
    batch_list = [
        {"command": "QUERY haha PGM NAME(A*)", "plex": PLEX, "route": ROUTE},
        {"command": "QUERY PGMPGMPGM SHOW(ALL)", "plex": PLEX, "route": ROUTE},
        {"command": "QUERY", "plex": PLEX, "route": ROUTE},
        {"command": "QRY TRAN NAME(A*) NAME(B*)", "plex": PLEX, "route": ROUTE},
    ]
    response = hosts.all.ims_command(batch=batch_list)
    for result in response.contacted.values():
        pprint(result)
        assert result['changed'] is False
        for output in result['ims_output']:
            assert int(output['command_return']['ctl.rc']) != 0


def test_batch_correct_and_malformed_commands(ansible_zos_module):
    hosts = ansible_zos_module
    batch_list = [
        {"command": "QUERY PGM", "plex": PLEX, "route": ROUTE},
        {"command": "QUERY PGMPGMPGM SHOW(ALL)", "plex": PLEX, "route": ROUTE}
    ]
    response = hosts.all.ims_command(batch=batch_list)
    # First command should succeed, second command should fail. Overall change should be True
    for result in response.contacted.values():
        pprint(result)
        assert result['changed'] is True
        assert int(result['ims_output'][0]['command_return']['ctl.rc']) == 0
        assert result['ims_output'][1]['msg'] == em.NON_ZERO_RC_MSG
