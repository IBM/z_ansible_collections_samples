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


def test_zos_tso_command_run_help(ansible_zos_module):
    hosts = ansible_zos_module
    results = hosts.all.zos_tso_command(commands=["help"])
    for result in results.contacted.values():
        assert result.get("output")[0].get("rc") == 0
        assert result.get("changed") is True


# The happy path test
# Run a long tso command to allocate a dataset.
def test_zos_tso_command_long_command_128_chars(ansible_zos_module):
    hosts = ansible_zos_module
    command_string = [
        (
            "send 'Hello, this is a test message from zos_tso_command module. "
            "Im sending a command exceed 80 chars. Thank you.' user(omvsadm)"
        )
    ]
    results = hosts.all.zos_tso_command(commands=command_string)
    for result in results.contacted.values():
        assert result.get("output")[0].get("rc") == 0
        assert result.get("changed") is True


# The happy path test
# Run a long  tso command to allocate a dataset.
def test_zos_tso_command_long_unauth_command_116_chars(ansible_zos_module):
    hosts = ansible_zos_module
    command_string = [
        "alloc da('imstestl.ims1.temp.ps') catalog lrecl(133) blksize(13300) recfm(f b) dsorg(po) cylinders space(5,5) dir(5)"
    ]
    results = hosts.all.zos_tso_command(commands=command_string)
    for result in results.contacted.values():
        assert result.get("output")[0].get("rc") == 0
        assert result.get("changed") is True


# The positive path test
def test_zos_tso_command_auth_command_listds(ansible_zos_module):
    hosts = ansible_zos_module
    results = hosts.all.zos_tso_command(commands=["LISTDS 'imstestl.ims1.temp.ps'"])
    for result in results.contacted.values():
        assert result.get("output")[0].get("rc") == 0
        assert result.get("changed") is True


# The positive path test
# tests that single command works as well
def test_zos_tso_single_command_auth_command_listds(ansible_zos_module):
    hosts = ansible_zos_module
    results = hosts.all.zos_tso_command(commands="LISTDS 'imstestl.ims1.temp.ps'")
    for result in results.contacted.values():
        assert result.get("output")[0].get("rc") == 0
        assert result.get("changed") is True


# The positive path test
# tests that single command works as well with alias
def test_zos_tso_command_auth_command_listds_using_alias(ansible_zos_module):
    hosts = ansible_zos_module
    results = hosts.all.zos_tso_command(command=["LISTDS 'imstestl.ims1.temp.ps'"])
    for result in results.contacted.values():
        assert result.get("output")[0].get("rc") == 0
        assert result.get("changed") is True


# The positive path test
# tests that alias "command" works
def test_zos_tso_single_command_auth_command_listds_using_alias(ansible_zos_module):
    hosts = ansible_zos_module
    results = hosts.all.zos_tso_command(command="LISTDS 'imstestl.ims1.temp.ps'")
    for result in results.contacted.values():
        assert result.get("output")[0].get("rc") == 0
        assert result.get("changed") is True


# The positive path test
def test_zos_tso_command_unauth_command_listcat(ansible_zos_module):
    hosts = ansible_zos_module
    results = hosts.all.zos_tso_command(
        commands=["LISTCAT ENT('imstestl.ims1.temp.ps')"]
    )
    for result in results.contacted.values():
        assert result.get("output")[0].get("rc") == 0
        assert result.get("changed") is True


# The positive path test
def test_zos_tso_command_both_unauth_and_auth_command(ansible_zos_module):
    hosts = ansible_zos_module
    results = hosts.all.zos_tso_command(commands=["delete 'imstestl.ims1.temp.ps'"])
    for result in results.contacted.values():
        assert result.get("output")[0].get("rc") == 0
        assert result.get("changed") is True


# The failure path test
# the dataset has be deleted.
def test_zos_tso_command_valid_command_failed_as_has_been_deleted(ansible_zos_module):
    hosts = ansible_zos_module
    results = hosts.all.zos_tso_command(commands=["delete 'imstestl.ims1.temp.ps'"])
    for result in results.contacted.values():
        assert result.get("output")[0].get("rc") == 8
        assert result.get("changed") is False


# The failure test
# The input command is empty.
def test_zos_tso_command_empty_command(ansible_zos_module):
    hosts = ansible_zos_module
    results = hosts.all.zos_tso_command(commands=[""])
    for result in results.contacted.values():
        assert result.get("changed") is False


# The failure test
# The input command is no-existing command, the module return rc 255.
def test_zos_tso_command_invalid_command(ansible_zos_module):
    hosts = ansible_zos_module
    results = hosts.all.zos_tso_command(commands=["xxxxxx"])
    for result in results.contacted.values():
        assert result.get("output")[0].get("rc") == 255
        assert result.get("changed") is False


# The positive test
# The multiple commands
def test_zos_tso_command_multiple_commands(ansible_zos_module):
    hosts = ansible_zos_module
    commands_list = ["LU omvsadm", "LISTGRP"]
    results = hosts.all.zos_tso_command(commands=commands_list)
    for result in results.contacted.values():
        for item in result.get("output"):
            assert item.get("rc") == 0
        assert result.get("changed") is True
