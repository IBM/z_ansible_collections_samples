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

    delete_tran_response = hosts.all.ims_command(command='DELETE TRAN NAME(PYTEST, PYCODE)', plex=PLEX, route=ROUTE)
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


# Create and Delete an empty TRAN
def test_create_validate_and_delete_tran(ansible_zos_module):
    hosts = ansible_zos_module
    create_tran_response = hosts.all.ims_command(command='CREATE TRAN NAME(PYTEST) SET(PGM(PYTEST))', plex=PLEX, route=ROUTE)
    for result in create_tran_response.contacted.values():
        assert result['ims_output'][0]['command_return']['ctl.rc'] == SUCCESSFUL_RC
        for data in result['ims_output'][0]['type_2_response']:
            assert data['CC'] == SUCCESSFUL_CC
            assert data['Trancode'] == 'PYTEST'

    delete_tran_response = hosts.all.ims_command(command='DELETE TRAN NAME(PYTEST)', plex=PLEX, route=ROUTE)
    for result in delete_tran_response.contacted.values():
        assert result['ims_output'][0]['command_return']['ctl.rc'] == SUCCESSFUL_RC


# Create a TRAN with some attributes
def test_create_tran_with_attribute(ansible_zos_module):
    hosts = ansible_zos_module
    create_tran_response = hosts.all.ims_command(command='CREATE TRAN NAME(PYTEST) SET(CMTMODE(MULT), AOCMD(CMD), PGM(PYTEST))', plex=PLEX, route=ROUTE)
    for result in create_tran_response.contacted.values():
        assert result['ims_output'][0]['command_return']['ctl.rc'] == SUCCESSFUL_RC
        for data in result['ims_output'][0]['type_2_response']:
            assert data['CC'] == SUCCESSFUL_CC
            assert data['Trancode'] == 'PYTEST'


# Create another TRAN like the one with attributes
def test_create_like_tran(ansible_zos_module):
    hosts = ansible_zos_module
    create_tran_response = hosts.all.ims_command(command='CREATE TRAN NAME(PYCOPY) LIKE(RSC(PYTEST))', plex=PLEX, route=ROUTE)
    for result in create_tran_response.contacted.values():
        assert result['ims_output'][0]['command_return']['ctl.rc'] == SUCCESSFUL_RC
        for data in result['ims_output'][0]['type_2_response']:
            assert data['CC'] == SUCCESSFUL_CC
            assert data['Trancode'] == 'PYCOPY'


def test_create_validate_and_delete_tran_batch(ansible_zos_module):
    hosts = ansible_zos_module
    create_batch_list = [
        {"command": "CREATE TRAN NAME(PYTEST1) SET(PGM(PYTEST))", "plex": PLEX, "route": ROUTE},
        {"command": "CREATE TRAN NAME(PYTEST2) SET(PGM(PYTEST))", "plex": PLEX, "route": ROUTE},
        {"command": "CREATE TRAN NAME(PYTEST3) SET(PGM(PYTEST))", "plex": PLEX, "route": ROUTE},
        {"command": "CREATE TRAN NAME(PYTEST4) SET(PGM(PYTEST))", "plex": PLEX, "route": ROUTE}
    ]
    create_tran_response = hosts.all.ims_command(batch=create_batch_list, plex=PLEX, route=ROUTE)
    for result in create_tran_response.contacted.values():
        for output in result['ims_output']:
            assert output['command_return']['ctl.rc'] == SUCCESSFUL_RC
            for data in output['type_2_response']:
                assert data['CC'] == SUCCESSFUL_CC

    delete_batch_list = [
        {"command": "DELETE TRAN NAME(PYTEST1)", "plex": PLEX, "route": ROUTE},
        {"command": "DELETE TRAN NAME(PYTEST2)", "plex": PLEX, "route": ROUTE},
        {"command": "DELETE TRAN NAME(PYTEST3)", "plex": PLEX, "route": ROUTE},
        {"command": "DELETE TRAN NAME(PYTEST4)", "plex": PLEX, "route": ROUTE}
    ]
    delete_tran_response = hosts.all.ims_command(batch=delete_batch_list)
    for result in delete_tran_response.contacted.values():
        for output in result['ims_output']:
            assert output['command_return']['ctl.rc'] == SUCCESSFUL_RC


# Delete both TRANs
def test_delete_tran_and_like(ansible_zos_module):
    hosts = ansible_zos_module
    delete_tran_response = hosts.all.ims_command(command='DELETE TRAN NAME(PYCOPY, PYTEST)', plex=PLEX, route=ROUTE)
    for result in delete_tran_response.contacted.values():
        assert result['ims_output'][0]['command_return']['ctl.rc'] == SUCCESSFUL_RC


# Delete initial program
def test_delete_initial_pgm(ansible_zos_module):
    hosts = ansible_zos_module
    delete_pgm_response = hosts.all.ims_command(command='DELETE PGM NAME(PYTEST)', plex=PLEX, route=ROUTE)
    for result in delete_pgm_response.contacted.values():
        assert result['ims_output'][0]['command_return']['ctl.rc'] == SUCCESSFUL_RC
