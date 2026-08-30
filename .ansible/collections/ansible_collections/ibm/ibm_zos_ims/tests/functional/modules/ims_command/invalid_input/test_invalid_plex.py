# -*- coding: utf-8 -*-

from __future__ import (absolute_import, division, print_function)
from ansible_collections.ibm.ibm_zos_ims.plugins.module_utils.ims_module_error_messages import ErrorMessages as em  # pylint: disable=import-error
from pprint import pprint
import pytest

__metaclass__ = type

ROUTE = "IMS1"


def test_invalid_plex(ansible_zos_module):
    hosts = ansible_zos_module
    response = hosts.all.ims_command(command='QUERY PGM', plex="Thisplexdoesntexist", route=ROUTE)
    for result in response.contacted.values():
        assert result['ims_output'][0]['msg'] == em.NON_ZERO_RC_MSG


def test_invalid_characters_for_plex(ansible_zos_module):
    hosts = ansible_zos_module
    response = hosts.all.ims_command(command='QUERY PGM', plex="$Xe%^&L*(P", route=ROUTE)
    for result in response.contacted.values():
        assert result['ims_output'][0]['msg'] == em.INVALID_PLEX_MSG


def test_missing_plex(ansible_zos_module):
    hosts = ansible_zos_module
    response = hosts.all.ims_command(command='QUERY PGM', route=ROUTE)
    for result in response.contacted.values():
        assert result['ims_output'][0]['msg'] == em.MISSING_PLEX


def test_batch_missing_plex(ansible_zos_module):
    hosts = ansible_zos_module
    batch_list = [{"command": "QUERY PGM", "route": ROUTE}]
    response = hosts.all.ims_command(batch=batch_list)
    for result in response.contacted.values():
        assert em.MISSING_PLEX.lower() in result['msg']
        assert result['changed'] is False


def test_batch_malformed_plex_multiple(ansible_zos_module):
    hosts = ansible_zos_module
    batch_list = [
        {"command": "QUERY PGM", "route": ROUTE},
        {"command": "QUERY PGM", "plex": "Thisplexdoesntexist", "route": ROUTE},
        {"command": "QUERY PGM", "plex": "$Xe%^&L*(P", "route": ROUTE}
    ]
    response = hosts.all.ims_command(batch=batch_list)
    for result in response.contacted.values():
        pprint(result)
        assert em.MISSING_PLEX.lower() in result['msg']
        assert result['changed'] is False


def test_batch_correct_and_malformed_plex(ansible_zos_module):
    hosts = ansible_zos_module
    batch_list = [
        {"command": "QUERY PGM", "plex": "PLEX1", "route": ROUTE},
        {"command": "QUERY PGM", "plex": "Thisplexdoesntexist", "route": ROUTE}
    ]
    response = hosts.all.ims_command(batch=batch_list)
    for result in response.contacted.values():
        pprint(result)
        assert result['changed'] is True
        assert int(result['ims_output'][0]['command_return']['ctl.rc']) == 0
        assert result['ims_output'][1]['msg'] == em.NON_ZERO_RC_MSG
