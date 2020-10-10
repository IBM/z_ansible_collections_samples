# -*- coding: utf-8 -*-

# Copyright (c) IBM Corporation 2019, 2020
# Apache License, Version 2.0 (see https://opensource.org/licenses/Apache-2.0)

from __future__ import absolute_import, division, print_function

__metaclass__ = type

import os
import sys
import warnings

import ansible.constants
import ansible.errors
import ansible.utils
import pytest

__metaclass__ = type


def test_zos_operator_various_command(ansible_zos_module):
    test_data = [
        ("d a", 0, True),
        ("k s", 0, True),
        ("d r,l", 0, True),
        ("d parmlib", 0, True),
        ("SEND 'list ready',NOW", 0, True)
    ]
    for item in test_data:
        command = item[0]
        expected_rc = item[1]
        changed = item[2]
        hosts = ansible_zos_module
        results = hosts.all.zos_operator(cmd=command)
        for result in results.contacted.values():
            assert result['rc'] == expected_rc
            assert result.get("changed") is changed


def test_zos_operator_invalid_command(ansible_zos_module):
    hosts = ansible_zos_module
    results = hosts.all.zos_operator(cmd='invalid,command', verbose=False, debug=False)
    for result in results.contacted.values():
        assert result.get("changed") is False
        assert result.get("exception") is not None


def test_zos_operator_positive_path(ansible_zos_module):
    hosts = ansible_zos_module
    results = hosts.all.zos_operator(cmd='d u,all', verbose=False, debug=False)
    for result in results.contacted.values():
        assert result['rc'] == 0
        assert result.get("changed") is True
        assert result.get("content") is not None


def test_zos_operator_positive_path_verbose(ansible_zos_module):
    hosts = ansible_zos_module
    results = hosts.all.zos_operator(cmd='d u,all', verbose=True, debug=False)
    for result in results.contacted.values():
        assert result['rc'] == 0
        assert result.get("changed") is True
        assert result.get("content") is not None


def test_zos_operator_positive_with_debug(ansible_zos_module):
    hosts = ansible_zos_module
    results = hosts.all.zos_operator(cmd='d u,all', verbose=False, debug=True)
    for result in results.contacted.values():
        assert result['rc'] == 0
        assert result.get("changed") is True
        assert result.get("content") is not None


def test_zos_operator_positive_with_debug_verbose(ansible_zos_module):
    hosts = ansible_zos_module
    results = hosts.all.zos_operator(cmd='d u,all', verbose=True, debug=True)
    for result in results.contacted.values():
        assert result['rc'] == 0
        assert result.get("changed") is True
        assert result.get("content") is not None
