# -*- coding: utf-8 -*-

# Copyright (c) IBM Corporation 2019, 2020
# Apache License, Version 2.0 (see https://opensource.org/licenses/Apache-2.0)

from __future__ import (absolute_import, division, print_function)
__metaclass__ = type


# from ansible.module_utils.basic import AnsibleModule
from ibm_zos_ims.plugins.module_utils.ims_module_error_messages import ErrorMessages as ims_em
import pytest
import sys
# from mock import call
import re


class ModuleFailException(BaseException):
    def __init__(self):
        pass
# class RunCommandException(ModuleFailException):
#     def __init__(self):
#         pass


class FailJsonException(ModuleFailException):
    def __init__(self, msg):
        self.msg = msg


class DummyModule(object):
    def __init__(self, msg=''):
        """
        Used in place of Ansible's module so we can easily mock the desired behavior.
        """
        self.rc = 0
        self.stdout = "dummy module"
        self.stderr = "dummy module"

    def fail_json(self, **kwargs):
        raise FailJsonException(msg=kwargs['msg'])


# Used my some mock modules, should match import directly below
IMPORT_NAME = 'ibm_zos_ims.plugins.modules.ims_command'
test_data_ims_command_structure = [
    ("query pgm show(all)", True, None),
    ("Not A Real IMS Command)", True, None),
    ("QUERY PGM AA?AA)", True, None),
    ('', False, ims_em.MISSING_COMMAND)
]


@pytest.mark.parametrize("raw_command, expected_flag, expected_msg", test_data_ims_command_structure)
def test_format_ims_command(zos_import_mocker, raw_command, expected_flag, expected_msg):
    test_result = True
    mocker, importer = zos_import_mocker
    ims_command = importer(IMPORT_NAME)
    result_flag, result_msg, unused_var = ims_command.format_ims_command(raw_command)
    assert expected_flag == result_flag and expected_msg == result_msg


test_data_plex = [
    ('aaaaa', True, None),
    ('abc!', False, ims_em.INVALID_PLEX_MSG)
]


@pytest.mark.parametrize("raw_plex, expected_flag, expected_msg", test_data_plex)
def test_format_plex(zos_import_mocker, raw_plex, expected_flag, expected_msg):
    mocker, importer = zos_import_mocker
    ims_command_module = importer(IMPORT_NAME)

    result_flag, result_msg, unused_var = ims_command_module.format_plex(raw_plex)
    assert result_flag == expected_flag and result_msg == expected_msg


test_data_route = [
    ({'a', 'b', 'c', 'd'}, True, None),
    ({'@', 'b', 'c', 'd'}, False, ims_em.INVALID_ROUTE_MSG),
    (None, True, None)  # Route is an optional parameter, default value is None.
]


@pytest.mark.parametrize("raw_route_list, expected_flag, expected_msg", test_data_route)
def test_format_route(zos_import_mocker, raw_route_list, expected_flag, expected_msg):
    test_result = True
    mocker, importer = zos_import_mocker
    ims_command_module = importer(IMPORT_NAME)

    result_flag, result_msg, unused_var = ims_command_module.format_route(raw_route_list)
    assert result_flag == expected_flag and result_msg == expected_msg


test_data_rc = [
    ({'command_return': {'ctl.rc': '000000'}}, True, None),
    ({'command_return': {'ctl.rc': '080004'}}, False, ims_em.NON_ZERO_RC_MSG),
    ({'command_return': {'ctl.rc': None}}, False, ims_em.NO_RC_MSG)
]


@pytest.mark.parametrize("output, expected_flag, expected_msg", test_data_rc)
def test_verify_return_code(zos_import_mocker, output, expected_flag, expected_msg):
    result_msg = None
    mocker, importer = zos_import_mocker
    ims_command = importer(IMPORT_NAME)

    result_flag, result_msg = ims_command.verify_return_code(output)
    assert result_flag == expected_flag and result_msg == expected_msg

# TODO: need to find a way to cause failure in module.run_command method to tests exception handling.
# test_data = [
#     # ("query pgm show(all)", '1', None, True),
#     # ("query pgm show(all)", '1', 'aaa', True),
#     # ("fail me", 1, '', False)
# ]
# @pytest.mark.parametrize("ims_command, plex, route, expected", test_data)
# def test_submit_rexx(zos_import_mocker, ims_command, plex, route, expected):

#     test_result = True
#     mocker, importer = zos_import_mocker
#     ims_command_module = importer(IMPORT_NAME)
#     module = DummyModule()

#     try:
#         result = {}
#         ims_command_module.submit_rexx(ims_command, plex, route, module, result)
#     except FailJsonException:
#         test_result = False
#     assert test_result == expected

# test_data = [
#     # (None, None),
#     # (None, 'You requested this to fail'),
#     # (None, ims_em.NO_OUTPUT_MSG),
#     # (None, ims_em.NO_RC_MSG),
#     # (None, ims_em.JSON_DECODE_ERROR_MSG),
# ]
# @pytest.mark.parametrize("abc, expected_msg", test_data)
# def test_run_module(zos_import_mocker, route, expected_msg):
#     test_msg = None
#     mocker, importer = zos_import_mocker
#     ims_command_module = importer(IMPORT_NAME)

#     try:
#         ims_command_module.run_module()
#     except FailJsonException as e:
#         test_msg = e.msg
#     assert test_msg == expected_msg
