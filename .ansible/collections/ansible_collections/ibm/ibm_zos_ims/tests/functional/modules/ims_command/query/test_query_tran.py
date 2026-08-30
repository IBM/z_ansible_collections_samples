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
    response = hosts.all.ims_command(command='CREATE PGM NAME(DFANSI)', plex=PLEX, route=ROUTE)
    for result in response.contacted.values():
        assert result['ims_output'][0]['command_return']['ctl.rc'] == SUCCESSFUL_RC
        for data in result['ims_output'][0]['type_2_response']:
            assert data['PgmName'][0].lower() == "d"


def test_create_initial_tran_name_starts_with_D(ansible_zos_module):
    hosts = ansible_zos_module
    response = hosts.all.ims_command(command='CREATE TRAN NAME(ANSI) SET(PGM(DFANSI))', plex=PLEX, route=ROUTE)
    for result in response.contacted.values():
        assert result['ims_output'][0]['command_return']['ctl.rc'] == SUCCESSFUL_RC
        for data in result['ims_output'][0]['type_2_response']:
            assert data['Trancode'][0].lower() == "a"


def test_query_tran_basic(ansible_zos_module):
    hosts = ansible_zos_module
    response = hosts.all.ims_command(command='QUERY TRAN', plex=PLEX, route=ROUTE)
    for result in response.contacted.values():
        assert result['ims_output'][0]['command_return']['ctl.rc'] == SUCCESSFUL_RC
        for data in result['ims_output'][0]['type_2_response']:
            assert data['CC'] == SUCCESSFUL_CC


def test_query_tran_name(ansible_zos_module):
    hosts = ansible_zos_module
    response = hosts.all.ims_command(command='QUERY TRAN NAME(A*)', plex=PLEX, route=ROUTE)
    for result in response.contacted.values():
        assert result['ims_output'][0]['command_return']['ctl.rc'] == SUCCESSFUL_RC
        for data in result['ims_output'][0]['type_2_response']:
            assert data['CC'] == SUCCESSFUL_CC
            trancode = data['Trancode']
            assert trancode[0] == 'A'


def test_query_tran_show(ansible_zos_module):
    hosts = ansible_zos_module
    response = hosts.all.ims_command(command='QUERY TRAN SHOW(ALL)', plex=PLEX, route=ROUTE)
    for result in response.contacted.values():
        assert result['ims_output'][0]['command_return']['ctl.rc'] == SUCCESSFUL_RC
        for data in result['ims_output'][0]['type_2_response']:
            assert 'LTimeCreate' in data
            assert 'LDefnType' in data


def test_query_tran_invalid_name(ansible_zos_module):
    hosts = ansible_zos_module
    response = hosts.all.ims_command(command='QUERY TRAN NAME(012345678)', plex=PLEX, route=ROUTE)
    for result in response.contacted.values():
        assert result['ims_output'][0]['msg'] == em.NON_ZERO_RC_MSG


def test_batch_query_tran_basic(ansible_zos_module):
    hosts = ansible_zos_module
    batch_list = [{"command": "QUERY TRAN", "plex": PLEX, "route": ROUTE}]
    response = hosts.all.ims_command(batch=batch_list)
    for result in response.contacted.values():
        assert result['ims_output'][0]['command_return']['ctl.rc'] == SUCCESSFUL_RC
        assert 'type_2_response' in result['ims_output'][0]


def test_batch_query_tran_multiple(ansible_zos_module):
    hosts = ansible_zos_module
    batch_list = [
        {"command": "QUERY TRAN", "plex": PLEX, "route": ROUTE},
        {"command": "QUERY TRAN NAME(A*)", "plex": PLEX, "route": ROUTE},
        {"command": "QUERY TRAN SHOW(ALL)", "plex": PLEX, "route": ROUTE}
    ]
    response = hosts.all.ims_command(batch=batch_list)
    for result in response.contacted.values():
        for output in result['ims_output']:
            assert output['command_return']['ctl.rc'] == SUCCESSFUL_RC
            assert 'type_2_response' in output


def test_delete_tran_and_like(ansible_zos_module):
    hosts = ansible_zos_module
    delete_tran_response = hosts.all.ims_command(command='DELETE TRAN NAME(ANSI)', plex=PLEX, route=ROUTE)
    for result in delete_tran_response.contacted.values():
        assert result['ims_output'][0]['command_return']['ctl.rc'] == SUCCESSFUL_RC


def test_delete_initial_pgm(ansible_zos_module):
    hosts = ansible_zos_module
    delete_pgm_response = hosts.all.ims_command(command='DELETE PGM NAME(DFANSI)', plex=PLEX, route=ROUTE)
    for result in delete_pgm_response.contacted.values():
        assert result['ims_output'][0]['command_return']['ctl.rc'] == SUCCESSFUL_RC
