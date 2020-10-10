# -*- coding: utf-8 -*-

# Copyright (c) IBM Corporation 2019, 2020
# Apache License, Version 2.0 (see https://opensource.org/licenses/Apache-2.0)

from __future__ import absolute_import, division, print_function

__metaclass__ = type

from ansible.module_utils.basic import AnsibleModule
import pytest
import sys
from mock import call

# Used my some mock modules, should match import directly below
IMPORT_NAME = 'ibm_zos_core.plugins.modules.zos_operator'


# * Tests for zos_operator

dummy_dict1 = {
    'verbose': False,
    'debug': True
}

dummy_dict2 = {
    'cmd': 123,
    'verbose': True,
    'debug': False
}

dummy_dict3 = {
    'cmd': 'd u,all'
}

dummy_dict4 = {
    'cmd': 'd u,all',
    'verbose': True
}

dummy_dict5 = {
    'cmd': 'd u,all',
    'debug': 123
}

dummy_return_dict1 = {
    'rc': 0,
    'message': 'good result'
}

dummy_return_dict2 = {
    'rc': 1,
    'message': None
}

test_data = [
    (dummy_dict1, False),
    (dummy_dict2, False),
    (dummy_dict3, True),
    (dummy_dict4, True),
    (dummy_dict5, False)
]


@pytest.mark.parametrize("args,expected", test_data)
def test_zos_opreator_various_args(zos_import_mocker, args, expected):
    mocker, importer = zos_import_mocker
    zos_operator = importer(IMPORT_NAME)
    passed = True
    try:
        zos_operator.parse_params(args)
    except Exception as e:
        passed = False
    assert passed == expected
