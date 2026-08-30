# -*- coding: utf-8 -*-

# Copyright (c) IBM Corporation 2019, 2020
# Apache License, Version 2.0 (see https://opensource.org/licenses/Apache-2.0)

from __future__ import (absolute_import, division, print_function)
__metaclass__ = type

# from ansible.module_utils.basic import AnsibleModule
from ibm_zos_ims.plugins.module_utils.ims_module_error_messages import ErrorMessages as ims_em

from ibm_zos_ims.plugins.module_utils.ims_gen_utils import is_valid_member_str, validate_member_list

import pytest
import sys
# from mock import call
import re

member_names_test_data = [
    ("abc", True),
    ("DEDBJN21", True),
    # legal chars
    ("#ABC", True),
    ("@def", True),
    ("$GHI", True),
    ("de-fg", False),  # cannot contain hypens
    ("MemberNamesCannotBeThisLong", False),
    ("3DBDS", False),  # cannot start with a number
    # illegal chars
    ("&ABC", False),
    ("*DEF", False)
]


@pytest.mark.parametrize("test_input, expected", member_names_test_data)
def test_is_valid_member_str(test_input, expected):
    assert is_valid_member_str(test_input) == expected


data = [
    # simple list of strings with legal member names
    (["abc", "def", "ghi", "jkl"], True, ''),
    # list of dicts with legal member names
    ([{"abc": "def"}, {"ghi": "jkl"}], True, ''),
    # list of strings and dicts with legal member names
    (["abc", {"def": "ghi"}, "jkl"], True, ''),
    # list of strings and dicts with some invalid member names
    (["abc", {"def": "ghi"}, "jkl-mno"], False, ims_em.INVALID_MEMBER_NAME + str("jkl-mno")),
    # list of strings, dicts, and list -- invalid
    (["abc", {"def": "ghi"}, "jkl", ["ABC", "DEF", "GHI"]], False, ims_em.INVALID_MEMBER_LIST_TYPE),
    # list of dicts where dict is larger than single-entry
    (["abc", {"def": "ghi", "jkl": "mno"}, "pqr"], False, ims_em.INVALID_MEMBER_LIST_TYPE),
]


@pytest.mark.parametrize("member_list, expected_flag, expected_str", data)
def test_validate_member_list(member_list, expected_flag, expected_str):
    result_flag, result_str = validate_member_list(member_list)
    assert result_flag == expected_flag and result_str == expected_str
