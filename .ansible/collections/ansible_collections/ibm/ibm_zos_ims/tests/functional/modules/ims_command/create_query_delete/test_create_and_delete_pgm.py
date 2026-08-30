# -*- coding: utf-8 -*-

from __future__ import (absolute_import, division, print_function)
from pprint import pprint
import pytest

__metaclass__ = type

SUCCESSFUL_RC = '00000000'
SUCCESSFUL_CC = '0'
PLEX = "PLEX1"
ROUTE = "IMS1"

"""
Work flow for CREATE/DELETE PGM functional tests goes as follows:
1. Create a program
2. Query the program to see if has been successfully created
3. Delete the program
"""


# This will delete a resource that may have been previously allocated
def clean_env(hosts, pgm_name):
    delete_pgm_response = hosts.all.ims_command(command='DELETE PGM NAME(' + pgm_name + ')', plex=PLEX, route=ROUTE)
    for result in delete_pgm_response.contacted.values():
        assert result['ims_output'][0] is not None


def create_validate_and_delete(hosts, create_cmd, pgm_name):
    create_response = hosts.all.ims_command(command=create_cmd, plex=PLEX, route=ROUTE)
    for result in create_response.contacted.values():
        pprint(result)
        try:
            print("IMS_RETURN_CODE:", result['ims_output'][0]['command_return']['ims_member_messages'][0]['cmderr rc'])
            print("IMS_REASON_CODE:", result['ims_output'][0]['command_return']['ims_member_messages'][0]['rsn'])
        except Exception:
            pass
        assert result['ims_output'][0]['command_return']['ctl.rc'] == SUCCESSFUL_RC
        for data in result['ims_output'][0]['type_2_response']:
            assert data['CC'] == SUCCESSFUL_CC

    created_pgm = False
    query_response = hosts.all.ims_command(command='QUERY PGM NAME(' + pgm_name + ')', plex=PLEX, route=ROUTE)
    for result in query_response.contacted.values():
        for data in result['ims_output'][0]['type_2_response']:
            if data['PgmName'] == pgm_name:
                delete_response = hosts.all.ims_command(command='DELETE PGM NAME(' + pgm_name + ')', plex=PLEX, route=ROUTE)
                for result in delete_response.contacted.values():
                    assert result['ims_output'][0]['command_return']['ctl.rc'] == SUCCESSFUL_RC
                    created_pgm = True

    assert created_pgm is True


def create_and_delete_batch(hosts, create_cmd_batch, delete_cmd_batch):
    create_response = hosts.all.ims_command(batch=create_cmd_batch)
    for result in create_response.contacted.values():
        pprint(result)
        for output in result['ims_output']:
            assert output['command_return']['ctl.rc'] == SUCCESSFUL_RC
            for data in output['type_2_response']:
                assert data['CC'] == SUCCESSFUL_CC

    # created_pgm = False
    # query_response = hosts.all.ims_command(command='QUERY PGM NAME(' + pgm_name + ')', plex=PLEX, route=ROUTE)
    # for result in query_response.contacted.values():
    #     for data in result['ims_output'][0]['type_2_response']:
    #         if data['PgmName'] == pgm_name:
    delete_response = hosts.all.ims_command(batch=delete_cmd_batch)
    for result in delete_response.contacted.values():
        for output in result['ims_output']:
            assert output['command_return']['ctl.rc'] == SUCCESSFUL_RC

    # assert created_pgm == True


def test_create_pgm_basic(ansible_zos_module):
    hosts = ansible_zos_module
    clean_env(hosts, 'TEST1')
    create_validate_and_delete(hosts, 'CREATE PGM NAME(TEST1)', 'TEST1')


def test_create_pgm_with_one_attribute(ansible_zos_module):
    hosts = ansible_zos_module
    clean_env(hosts, 'TEST1')
    create_validate_and_delete(hosts, 'CRE PGM NAME(TEST1) SET(BMPTYPE(Y))', 'TEST1')


def test_create_pgm_with_attributes(ansible_zos_module):
    hosts = ansible_zos_module
    clean_env(hosts, 'TEST1')
    create_validate_and_delete(hosts, 'CREATE PGM NAME(TEST1) SET(BMPTYPE(Y), TRANSTAT(Y))', 'TEST1')


def test_create_pgm_with_more_attributes(ansible_zos_module):
    hosts = ansible_zos_module
    clean_env(hosts, 'TEST1')
    create_validate_and_delete(hosts, 'CRE PGM NAME(TEST1) SET(BMPTYPE(Y), TRANSTAT(Y), GPSB(Y), LANG(JAVA))', 'TEST1')


def test_create_and_delete_pgm_batch(ansible_zos_module):
    hosts = ansible_zos_module
    for pgm in ["TEST1", "TEST2", "TEST3", "TEST4"]:
        clean_env(hosts, pgm)
    create_batch_list = [
        {"command": "CREATE PGM NAME(TEST1)", "plex": PLEX, "route": ROUTE},
        {"command": "CRE PGM NAME(TEST2) SET(BMPTYPE(Y))", "plex": PLEX, "route": ROUTE},
        {"command": "CREATE PGM NAME(TEST3) SET(BMPTYPE(Y), TRANSTAT(Y))", "plex": PLEX, "route": ROUTE},
        {"command": "CRE PGM NAME(TEST4) SET(BMPTYPE(Y), TRANSTAT(Y), GPSB(Y), LANG(JAVA))", "plex": PLEX, "route": ROUTE}
    ]
    delete_batch_list = [
        {"command": "DELETE PGM NAME(TEST1)", "plex": PLEX, "route": ROUTE},
        {"command": "DELETE PGM NAME(TEST2)", "plex": PLEX, "route": ROUTE},
        {"command": "DELETE PGM NAME(TEST3)", "plex": PLEX, "route": ROUTE},
        {"command": "DELETE PGM NAME(TEST4)", "plex": PLEX, "route": ROUTE}
    ]
    create_and_delete_batch(hosts, create_batch_list, delete_batch_list)
