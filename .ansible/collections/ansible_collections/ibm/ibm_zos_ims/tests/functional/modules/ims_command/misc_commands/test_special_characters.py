# -*- coding: utf-8 -*-

from __future__ import (absolute_import, division, print_function)
from pprint import pprint
import pytest

__metaclass__ = type

SUCCESSFUL_RC = '00000000'
SUCCESSFUL_CC = '0'
PLEX = "PLEX1"
ROUTE = "IMS1"


# Test single quotation functions properly
def test_command_with_single_quotes(ansible_zos_module):
    hosts = ansible_zos_module
    results = hosts.all.ims_command(command="RML DBRC='RECON STATUS'", plex=PLEX, route=ROUTE)
    for result in results.contacted.values():
        pprint(result)
        assert result['changed'] is True
        assert result['msg'] == "Success"


# Test double quotation functions properly
def test_command_with_double_quotes(ansible_zos_module):
    hosts = ansible_zos_module
    results = hosts.all.ims_command(command='QUEUE TRAN NAME(TRAN1) OPTION(ENQ) DATA(some"data")', plex=PLEX, route=ROUTE)
    for result in results.contacted.values():
        pprint(result)
        # Failure to parse results in the command being interpreted as a type 1 command
        assert result['ims_output'][0]['type_2_response']


def test_incomplete_command_with_double_quotes(ansible_zos_module):
    hosts = ansible_zos_module
    results = hosts.all.ims_command(command='QUEUE TRAN NAME(TRAN1) OPTION(ENQ) DATA(some"data', plex=PLEX, route=ROUTE)
    for result in results.contacted.values():
        pprint(result)
        assert result['ims_output'][0]['type_1_response']
        assert result['changed'] is False
