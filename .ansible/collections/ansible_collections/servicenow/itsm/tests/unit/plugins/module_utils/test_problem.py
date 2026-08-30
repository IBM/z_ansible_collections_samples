# -*- coding: utf-8 -*-
# Copyright: (c) 2022, XLAB Steampunk <steampunk@xlab.si>
#
# GNU General Public License v3.0+ (see COPYING or https://www.gnu.org/licenses/gpl-3.0.txt)

from __future__ import absolute_import, division, print_function

__metaclass__ = type

import sys

import pytest
from ansible_collections.servicenow.itsm.plugins.module_utils import errors
from ansible_collections.servicenow.itsm.plugins.module_utils.client import Response
from ansible_collections.servicenow.itsm.plugins.module_utils.problem import (
    ProblemClient,
)

pytestmark = pytest.mark.skipif(
    sys.version_info < (2, 7), reason="requires python2.7 or higher"
)


class TestProblemClient:
    @pytest.mark.parametrize(
        "in_base_api_path,out_base_api_path",
        [
            ("api/path", "/api/path/"),
            ("/api/path", "/api/path/"),
            ("", "/"),
            ("//api///path///", "/api/path/"),
        ],
    )
    def test_init(self, client, in_base_api_path, out_base_api_path):
        pc = ProblemClient(client, in_base_api_path)

        assert pc.base_api_path == out_base_api_path

    def test_update_record(self, client):
        client.patch.return_value = Response(200, '{"result": []}', {})
        pc = ProblemClient(client, "/api/path")

        data = dict(state="103")
        result = pc.update_record("PRB02", data)

        client.patch.assert_called_once_with(
            "/api/path/PRB02/new_state/103",
            dict(),
            query=dict(sysparm_exclude_reference_link=True),
        )

        assert result == []

    def test_update_record_error(self, client):
        client.patch.side_effect = errors.ServiceNowError("Something went wrong")
        pc = ProblemClient(client, "/api/path")

        data = dict(state="103")

        with pytest.raises(errors.ServiceNowError, match="Something went wrong"):
            pc.update_record("PRB02", data)

        client.patch.assert_called_once_with(
            "/api/path/PRB02/new_state/103",
            dict(),
            query=dict(sysparm_exclude_reference_link=True),
        )
