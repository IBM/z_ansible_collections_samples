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
Work flow for UPDATE PGM:
1. Create the pgm
2. Query the pgm, make sure it exists
3. Update the pgm
4. Delete the pgm
"""


def print_return_codes(result):
    pprint(result)
    try:
        print("IMS_RETURN_CODE:", result['ims_output'][0]['command_return']['ims_member_messages'][0]['cmderr rc'])
        print("IMS_REASON_CODE:", result['ims_output'][0]['command_return']['ims_member_messages'][0]['rsn'])
    except Exception:
        pass


def create_update_and_delete(hosts, update_cmd, pgm_name, second_update=None):
    clean_env(hosts, pgm_name)
    create_pgm(hosts, pgm_name)
    pgm_created = False
    query_response = hosts.all.ims_command(command='QUERY PGM NAME(' + pgm_name + ')', plex=PLEX, route=ROUTE)
    for result in query_response.contacted.values():
        for data in result['ims_output'][0]['type_2_response']:
            if data['PgmName'] == pgm_name:
                update_pgm(hosts, update_cmd)
                if second_update:
                    update_pgm(hosts, second_update)
                pgm_created = delete_pgm(hosts, pgm_name)
    assert pgm_created is True


def create_update_and_delete_batch(hosts, batch_list, pgm_name):
    clean_env(hosts, pgm_name)
    create_pgm(hosts, pgm_name)
    pgm_created = False
    query_response = hosts.all.ims_command(command='QUERY PGM NAME(' + pgm_name + ')', plex=PLEX, route=ROUTE)
    for result in query_response.contacted.values():
        for data in result['ims_output'][0]['type_2_response']:
            if data['PgmName'] == pgm_name:
                update_batch(hosts, batch_list)
                pgm_created = delete_pgm(hosts, pgm_name)
    assert pgm_created is True


def update_batch(hosts, batch_list):
    update_response = hosts.all.ims_command(batch=batch_list)
    for result in update_response.contacted.values():
        print_return_codes(result)
        for output in result['ims_output']:
            assert output['command_return']['ctl.rc'] == SUCCESSFUL_RC


def update_pgm(hosts, update_cmd):
    update_response = hosts.all.ims_command(command=update_cmd, plex=PLEX, route=ROUTE)
    for result in update_response.contacted.values():
        print_return_codes(result)
        assert result['ims_output'][0]['command_return']['ctl.rc'] == SUCCESSFUL_RC


def create_pgm(hosts, pgm_name):
    create_response = hosts.all.ims_command(command='CREATE PGM NAME(' + pgm_name + ')', plex=PLEX, route=ROUTE)
    for result in create_response.contacted.values():
        print_return_codes(result)
        assert result['ims_output'][0]['command_return']['ctl.rc'] == SUCCESSFUL_RC
        for data in result['ims_output'][0]['type_2_response']:
            assert data['CC'] == SUCCESSFUL_CC


def delete_pgm(hosts, pgm_name):
    delete_response = hosts.all.ims_command(command='DELETE PGM NAME(' + pgm_name + ')', plex=PLEX, route=ROUTE)
    for result in delete_response.contacted.values():
        assert result['ims_output'][0]['command_return']['ctl.rc'] == SUCCESSFUL_RC
        return True


def clean_env(hosts, pgm_name):
    hosts.all.ims_command(command='DELETE PGM NAME(' + pgm_name + ')', plex=PLEX, route=ROUTE)


def test_update_pgm_with_start(ansible_zos_module):
    hosts = ansible_zos_module
    create_update_and_delete(hosts, 'UPDATE PGM NAME(TEST1) START(TRACE)', 'TEST1')


def test_update_pgm_with_stop(ansible_zos_module):
    hosts = ansible_zos_module
    create_update_and_delete(hosts, 'UPDATE PGM NAME(TEST1) STOP(SCHD)', 'TEST1')


def test_update_pgm_with_start_and_stop(ansible_zos_module):
    hosts = ansible_zos_module
    create_update_and_delete(hosts, 'UPDATE PGM NAME(TEST1) START(TRACE)', 'TEST1', second_update='UPDATE PGM NAME(TEST1) STOP(TRACE)')


def test_update_pgm_with_set(ansible_zos_module):
    hosts = ansible_zos_module
    create_update_and_delete(hosts, 'UPDATE PGM NAME(TEST1) SET(BMPTYPE(Y))', 'TEST1')


def test_update_pgm_batch(ansible_zos_module):
    hosts = ansible_zos_module
    batch_list = [
        {"command": "UPDATE PGM NAME(TEST1) START(TRACE)", "plex": PLEX, "route": ROUTE},
        {"command": "UPDATE PGM NAME(TEST1) STOP(TRACE)", "plex": PLEX, "route": ROUTE},
        {"command": "UPDATE PGM NAME(TEST1) STOP(SCHD)", "plex": PLEX, "route": ROUTE},
        {"command": "UPDATE PGM NAME(TEST1) SET(BMPTYPE(Y))", "plex": PLEX, "route": ROUTE}
    ]
    create_update_and_delete_batch(hosts, batch_list, 'TEST1')
