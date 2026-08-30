# -*- coding: utf-8 -*-
# # Copyright: (c) 2021, XLAB Steampunk <steampunk@xlab.si>
#
# GNU General Public License v3.0+ (see COPYING or https://www.gnu.org/licenses/gpl-3.0.txt)

from __future__ import absolute_import, division, print_function

__metaclass__ = type

import sys

import pytest
from ansible_collections.servicenow.itsm.plugins.modules import problem_task_info

pytestmark = pytest.mark.skipif(
    sys.version_info < (2, 7), reason="requires python2.7 or higher"
)


class TestRemapCaller:
    def test_remap_params(self, table_client):
        query = [
            {"assigned_to": ("=", "some.user")},
            {"duplicate_of": ("=", "PRB0000010")},
            {"impact": ("=", "low")},
        ]
        table_client.get_record.side_effect = [
            {"sys_id": "681ccaf9c0a8016400b98a06818d57c7"},
            {"sys_id": "6816f79cc0a8016401c5a33be04be441"},
        ]

        result = problem_task_info.remap_params(query, table_client)

        assert result == [
            {"assigned_to": ("=", "681ccaf9c0a8016400b98a06818d57c7")},
            {"duplicate_of": ("=", "6816f79cc0a8016401c5a33be04be441")},
            {"impact": ("=", "low")},
        ]


class TestMain:
    def test_minimal_set_of_params(self, run_main):
        params = dict(
            instance=dict(
                host="https://my.host.name", username="user", password="pass"
            ),
        )
        success, result = run_main(problem_task_info, params)

        assert success is True

    def test_all_params(self, run_main):
        params = dict(
            instance=dict(
                host="https://my.host.name", username="user", password="pass"
            ),
            sys_id="id",
            number="n",
        )
        success, result = run_main(problem_task_info, params)

        assert success is True

    def test_fail(self, run_main):
        success, result = run_main(problem_task_info)

        assert success is False
        assert "instance" in result["msg"]


class TestRun:
    def test_run(self, create_module, table_client):
        module = create_module(
            params=dict(
                instance=dict(
                    host="https://my.host.name", username="user", password="pass"
                ),
                sys_id=None,
                number="n",
                query=None,
                sysparm_query=None,
                sysparm_display_value="true",
            )
        )
        table_client.list_records.return_value = [dict(p=1), dict(q=2), dict(r=3)]

        problems = problem_task_info.run(module, table_client)

        table_client.list_records.assert_called_once_with(
            "problem_task", dict(number="n", sysparm_display_value="true")
        )
        assert problems == [dict(p=1), dict(q=2), dict(r=3)]
