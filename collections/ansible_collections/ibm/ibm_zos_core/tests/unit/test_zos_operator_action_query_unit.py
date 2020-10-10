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
IMPORT_NAME = "ibm_zos_core.plugins.modules.zos_operator_action_query"


# * Tests for zos_operator_action_query

dummy_dict1 = {}

dummy_dict2 = {"system": "mv2c"}


dummy_dict3 = {"message_id": "DFH*"}

dummy_dict4_uppercase = {"message_id": "DFH*", "system": "MV28"}

dummy_dict4_lowercase = {"message_id": "DFH*", "system": "mv28"}

dummy_dict5 = {"system": "mv27", "message_id": "DFS*", "job_name": "IM5H*"}

dummy_dict_invalid_message = {"message_id": "$$#$%#"}
dummy_dict_invalid_job_name = {"job_name": "IM5H123456"}
dummy_dict_invalid_system = {"system": "mv2712345"}


test_data = [
    (dummy_dict1, True),
    (dummy_dict2, True),
    (dummy_dict3, True),
    (dummy_dict4_uppercase, True),
    (dummy_dict4_lowercase, True),
    (dummy_dict5, True),
    (dummy_dict_invalid_message, False),
    (dummy_dict_invalid_job_name, False),
    (dummy_dict_invalid_system, False),
]


@pytest.mark.parametrize("args,expected", test_data)
def test_zos_operator_action_query_various_args(zos_import_mocker, args, expected):
    mocker, importer = zos_import_mocker
    zos_operator_action_query = importer(IMPORT_NAME)
    passed = True
    try:
        zos_operator_action_query.parse_params(args)
    except Exception:
        passed = False
    assert passed == expected
