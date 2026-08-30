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


# We clean up the environment, making sure all resources are deleted
def test_clean_env(ansible_zos_module):
    hosts = ansible_zos_module
    delete_pgm_response = hosts.all.ims_command(command='DELETE PGM NAME(PYTEST)', plex=PLEX, route=ROUTE)
    for result in delete_pgm_response.contacted.values():
        assert result['ims_output'][0] is not None

    delete_tran_response = hosts.all.ims_command(command='DELETE TRAN NAME(PYTEST)', plex=PLEX, route=ROUTE)
    for result in delete_tran_response.contacted.values():
        assert result['ims_output'][0] is not None


# Create initial PGM needed to create TRANs
def test_create_initial_pgm(ansible_zos_module):
    hosts = ansible_zos_module
    create_pgm_response = hosts.all.ims_command(command='CREATE PGM NAME(PYTEST)', plex=PLEX, route=ROUTE)
    for result in create_pgm_response.contacted.values():
        assert result['ims_output'][0]['command_return']['ctl.rc'] == SUCCESSFUL_RC
        for data in result['ims_output'][0]['type_2_response']:
            assert data['CC'] == SUCCESSFUL_CC
            assert data['PgmName'] == 'PYTEST'


# Create an empty TRAN
def test_create_and_validate_tran(ansible_zos_module):
    hosts = ansible_zos_module
    create_tran_response = hosts.all.ims_command(command='CREATE TRAN NAME(PYTEST) SET(PGM(PYTEST))', plex=PLEX, route=ROUTE)
    for result in create_tran_response.contacted.values():
        assert result['ims_output'][0]['command_return']['ctl.rc'] == SUCCESSFUL_RC
        for data in result['ims_output'][0]['type_2_response']:
            assert data['CC'] == SUCCESSFUL_CC
            assert data['Trancode'] == 'PYTEST'


# Stop created TRAN and verify status
def test_stop_tran(ansible_zos_module):
    hosts = ansible_zos_module
    update_tran_response = hosts.all.ims_command(command='UPDATE TRAN NAME(PYTEST) STOP(SCHD)', plex=PLEX, route=ROUTE)
    for result in update_tran_response.contacted.values():
        assert result['ims_output'][0]['command_return']['ctl.rc'] == SUCCESSFUL_RC
        for data in result['ims_output'][0]['type_2_response']:
            assert data['CC'] == SUCCESSFUL_CC

    query_tran_response = hosts.all.ims_command(command='QUERY TRAN NAME(PYTEST) SHOW(STATUS)', plex=PLEX, route=ROUTE)
    for result in query_tran_response.contacted.values():
        assert result['ims_output'][0]['command_return']['ctl.rc'] == SUCCESSFUL_RC
        for data in result['ims_output'][0]['type_2_response']:
            assert data['CC'] == SUCCESSFUL_CC
            assert data['Trancode'] == 'PYTEST'
            assert data['LclStat'] == 'STOSCHD'


# Start create TRAN and verify status
def test_start_tran(ansible_zos_module):
    hosts = ansible_zos_module
    update_tran_response = hosts.all.ims_command(command='UPDATE TRAN NAME(PYTEST) START(SCHD)', plex=PLEX, route=ROUTE)
    for result in update_tran_response.contacted.values():
        assert result['ims_output'][0]['command_return']['ctl.rc'] == SUCCESSFUL_RC
        for data in result['ims_output'][0]['type_2_response']:
            assert data['CC'] == SUCCESSFUL_CC

    query_tran_response = hosts.all.ims_command(command='QUERY TRAN NAME(PYTEST) SHOW(STATUS)', plex=PLEX, route=ROUTE)
    for result in query_tran_response.contacted.values():
        assert result['ims_output'][0]['command_return']['ctl.rc'] == SUCCESSFUL_RC
        for data in result['ims_output'][0]['type_2_response']:
            assert data['CC'] == SUCCESSFUL_CC
            assert data['Trancode'] == 'PYTEST'
            assert data['LclStat'] == ''


def test_update_batch_tran(ansible_zos_module):
    hosts = ansible_zos_module
    batch_list = [
        {"command": "UPDATE TRAN NAME(PYTEST) START(SCHD)", "plex": PLEX, "route": ROUTE},
        {"command": "UPDATE TRAN NAME(PYTEST) STOP(SCHD)", "plex": PLEX, "route": ROUTE}
    ]
    response = hosts.all.ims_command(batch=batch_list)
    for result in response.contacted.values():
        assert result['changed'] is True
        for output in result['ims_output']:
            assert output['command_return']['ctl.rc'] == SUCCESSFUL_RC
            for data in output['type_2_response']:
                assert data['CC'] == SUCCESSFUL_CC


# Delete TRAN
def test_delete_tran_and_like(ansible_zos_module):
    hosts = ansible_zos_module
    delete_tran_response = hosts.all.ims_command(command='DELETE TRAN NAME(PYTEST)', plex=PLEX, route=ROUTE)
    for result in delete_tran_response.contacted.values():
        assert result['ims_output'][0]['command_return']['ctl.rc'] == SUCCESSFUL_RC


# Delete initial program
def test_delete_initial_pgm(ansible_zos_module):
    hosts = ansible_zos_module
    delete_pgm_response = hosts.all.ims_command(command='DELETE PGM NAME(PYTEST)', plex=PLEX, route=ROUTE)
    for result in delete_pgm_response.contacted.values():
        assert result['ims_output'][0]['command_return']['ctl.rc'] == SUCCESSFUL_RC
