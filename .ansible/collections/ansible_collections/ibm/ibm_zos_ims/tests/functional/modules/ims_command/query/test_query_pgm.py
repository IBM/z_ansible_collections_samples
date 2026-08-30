# -*- coding: utf-8 -*-

from __future__ import (absolute_import, division, print_function)
from ansible_collections.ibm.ibm_zos_ims.plugins.module_utils.ims_module_error_messages import ErrorMessages as em  # pylint: disable=import-error
from pprint import pprint
import pytest

__metaclass__ = type

SUCCESSFUL_RC = '00000000'
SUCCESSFUL_CC = '0'
PLEX = "PLEX1"
ROUTE = "IMS1"


def test_create_initial_pgm_name_starts_with_D(ansible_zos_module):
    hosts = ansible_zos_module
    response = hosts.all.ims_command(command='DELETE PGM NAME(DFANSI)', plex=PLEX, route=ROUTE)
    response = hosts.all.ims_command(command='CREATE PGM NAME(DFANSI)', plex=PLEX, route=ROUTE)
    for result in response.contacted.values():
        assert result['ims_output'][0]['command_return']['ctl.rc'] == SUCCESSFUL_RC
        for data in result['ims_output'][0]['type_2_response']:
            assert data['PgmName'][0].lower() == "d"


def test_query_pgm_basic(ansible_zos_module):
    hosts = ansible_zos_module
    response = hosts.all.ims_command(command='QUERY PGM', plex=PLEX, route=ROUTE)
    for result in response.contacted.values():
        assert result['ims_output'][0]['command_return']['ctl.rc'] == SUCCESSFUL_RC
        for data in result['ims_output'][0]['type_2_response']:
            assert data['CC'] == SUCCESSFUL_CC


def test_query_pgm_with_name_starts_with_D(ansible_zos_module):
    hosts = ansible_zos_module
    response = hosts.all.ims_command(command='QUERY PGM NAME(D*)', plex=PLEX, route=ROUTE)
    for result in response.contacted.values():
        assert result['ims_output'][0]['command_return']['ctl.rc'] == SUCCESSFUL_RC
        for data in result['ims_output'][0]['type_2_response']:
            assert data['PgmName'][0].lower() == "d"


def test_invalid_query_pgm(ansible_zos_module):
    hosts = ansible_zos_module
    response = hosts.all.ims_command(command='QUERY PGM NAME(THISISAVERYLONGNAME)', plex=PLEX, route=ROUTE)
    for result in response.contacted.values():
        assert result['ims_output'][0]['msg'] == em.NON_ZERO_RC_MSG


def test_query_pgm_with_name_and_show(ansible_zos_module):
    hosts = ansible_zos_module
    response = hosts.all.ims_command(command='QUERY PGM NAME(D*) SHOW(ALL)', plex=PLEX, route=ROUTE)
    for result in response.contacted.values():
        assert result['ims_output'][0]['command_return']['ctl.rc'] == SUCCESSFUL_RC
        for data in result['ims_output'][0]['type_2_response']:
            assert 'LTimeCreate' in data
            assert 'LDefnType' in data
            assert 'LTimeUpdate' in data


def test_query_pgm_with_name_and_show_db(ansible_zos_module):
    hosts = ansible_zos_module
    response = hosts.all.ims_command(command='QUERY PGM SHOW(DB)', plex=PLEX, route=ROUTE)
    for result in response.contacted.values():
        assert result['ims_output'][0]['command_return']['ctl.rc'] == SUCCESSFUL_RC
        for data in result['ims_output'][0]['type_2_response']:
            assert data['CC'] == SUCCESSFUL_CC


def test_query_pgm_with_name_show_all(ansible_zos_module):
    hosts = ansible_zos_module
    response = hosts.all.ims_command(command='QUERY PGM NAME(D*) SHOW(ALL)', plex=PLEX, route=ROUTE)
    for result in response.contacted.values():
        pprint(result)
        assert result['ims_output'][0]['command_return']['ctl.rc'] == SUCCESSFUL_RC
        assert 'type_2_response' in result['ims_output'][0]


def test_query_pgm_batch_basic(ansible_zos_module):
    hosts = ansible_zos_module
    batch_list = [{"command": "QUERY PGM", "plex": PLEX, "route": ROUTE}]
    response = hosts.all.ims_command(batch=batch_list)
    for result in response.contacted.values():
        pprint(result)
        assert result['ims_output'][0]['command_return']['ctl.rc'] == SUCCESSFUL_RC
        assert 'type_2_response' in result['ims_output'][0]


def test_query_pgm_batch_multiple(ansible_zos_module):
    hosts = ansible_zos_module
    batch_list = [
        {"command": "QUERY PGM", "plex": PLEX, "route": ROUTE},
        {"command": "QUERY PGM NAME(D*)", "plex": PLEX, "route": ROUTE},
        {"command": "QUERY PGM NAME(D*) SHOW(ALL)", "plex": PLEX, "route": ROUTE},
        {"command": "QUERY PGM SHOW(DB)", "plex": PLEX, "route": ROUTE}
    ]
    response = hosts.all.ims_command(batch=batch_list)
    for result in response.contacted.values():
        for output in result['ims_output']:
            pprint(result)
            assert output['command_return']['ctl.rc'] == SUCCESSFUL_RC
            assert 'type_2_response' in output


def test_delete_initial_pgm_name_starts_with_D(ansible_zos_module):
    hosts = ansible_zos_module
    response = hosts.all.ims_command(command='DELETE PGM NAME(DFANSI)', plex=PLEX, route=ROUTE)
    for result in response.contacted.values():
        assert result['ims_output'][0]['command_return']['ctl.rc'] == SUCCESSFUL_RC
