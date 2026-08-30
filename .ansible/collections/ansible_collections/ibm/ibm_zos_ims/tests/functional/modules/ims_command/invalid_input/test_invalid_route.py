# -*- coding: utf-8 -*-

from __future__ import (absolute_import, division, print_function)
from ansible_collections.ibm.ibm_zos_ims.plugins.module_utils.ims_module_error_messages import ErrorMessages as em  # pylint: disable=import-error
from pprint import pprint
import pytest

__metaclass__ = type

PLEX = "PLEX1"
SUCCESSFUL_RC = '00000000'


def test_invalid_route(ansible_zos_module):
    hosts = ansible_zos_module
    response = hosts.all.ims_command(command='QUERY PGM', plex=PLEX, route="thisroutedoesntexist")
    for result in response.contacted.values():
        assert result['ims_output'][0]['msg'] == em.NON_ZERO_RC_MSG


def test_invalid_characters_for_route(ansible_zos_module):
    hosts = ansible_zos_module
    response = hosts.all.ims_command(command='QUERY PGM', plex=PLEX, route="r4&$o&$*te")
    for result in response.contacted.values():
        assert result['ims_output'][0]['msg'] == em.INVALID_ROUTE_MSG


def test_batch_invalid_route(ansible_zos_module):
    hosts = ansible_zos_module
    batch_list = [{"command": "QUERY PGM", "plex": PLEX, "route": "thisrouteisbad"}]
    response = hosts.all.ims_command(batch=batch_list)
    for result in response.contacted.values():
        pprint(result)
        assert result['msg'] == em.BATCH_FAILURE_MSG
        assert result['changed'] is False


def test_batch_malformed_route_multiple(ansible_zos_module):
    hosts = ansible_zos_module
    batch_list = [
        {"command": "QUERY PGM", "plex": PLEX, "route": "thisroutedoesntexist"},
        {"command": "QUERY PGM", "plex": PLEX, "route": "r4&$o&$*te"}
    ]
    response = hosts.all.ims_command(batch=batch_list)
    for result in response.contacted.values():
        pprint(result)
        assert result['msg'] == em.BATCH_FAILURE_MSG
        assert result['changed'] is False


def test_batch_correct_and_malformed_routes(ansible_zos_module):
    hosts = ansible_zos_module
    batch_list = [
        {"command": "QUERY PGM", "plex": PLEX, "route": "IMS1"},
        {"command": "QUERY PGM", "plex": PLEX, "route": "thisroutedoesntexist"}
    ]
    response = hosts.all.ims_command(batch=batch_list)
    for result in response.contacted.values():
        pprint(result)
        assert result['changed'] is True
        assert int(result['ims_output'][0]['command_return']['ctl.rc']) == 0
        assert result['ims_output'][1]['msg'] == em.NON_ZERO_RC_MSG
