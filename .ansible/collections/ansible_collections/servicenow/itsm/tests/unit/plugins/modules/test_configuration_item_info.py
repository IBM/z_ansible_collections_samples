# -*- coding: utf-8 -*-
# # Copyright: (c) 2021, XLAB Steampunk <steampunk@xlab.si>
#
# GNU General Public License v3.0+ (see COPYING or https://www.gnu.org/licenses/gpl-3.0.txt)

from __future__ import absolute_import, division, print_function

__metaclass__ = type

import sys

import pytest
from ansible_collections.servicenow.itsm.plugins.modules import configuration_item_info

pytestmark = pytest.mark.skipif(
    sys.version_info < (2, 7), reason="requires python2.7 or higher"
)


class TestRemapCaller:
    def test_remap_params(self, table_client):
        query = [
            {
                "hold_reason": ("=", "Some reason"),
                "assigned_to": ("=", "some.user"),
            }
        ]
        table_client.get_record.return_value = {
            "sys_id": "681ccaf9c0a8016400b98a06818d57c7"
        }

        result = configuration_item_info.remap_assignment(query, table_client)

        assert result == [
            {
                "hold_reason": ("=", "Some reason"),
                "assigned_to": ("=", "681ccaf9c0a8016400b98a06818d57c7"),
            }
        ]


class TestMain:
    def test_minimal_set_of_params(self, run_main):
        params = dict(
            instance=dict(
                host="https://my.host.name", username="user", password="pass"
            ),
        )
        success, result = run_main(configuration_item_info, params)

        assert success is True

    def test_all_params(self, run_main):
        params = dict(
            instance=dict(
                host="https://my.host.name", username="user", password="pass"
            ),
            sys_id="01a9ec0d3790200044e0bfc8bcbe5dc3",
            sys_class_name="cmdb_ci",
        )
        success, result = run_main(configuration_item_info, params)

        assert success is True

    @pytest.mark.parametrize(
        "sys_id_value, name_value, query_value, sysparm_query_value",
        [
            ("01a9ec0d3790200044e0bfc8bcbe5dc3", "test.name", None, None),
            ("01a9ec0d3790200044e0bfc8bcbe5dc3", None, None, "category=Hardware"),
            (None, None, [{"category": "= Hardware"}], "category=Hardware"),
            (None, "test.name", [{"category": "= Hardware"}], None),
        ],
    )
    def test_params_mutually_exclusive(
        self, sys_id_value, name_value, query_value, sysparm_query_value, run_main
    ):
        params = dict(
            instance=dict(
                host="https://my.host.name", username="user", password="pass"
            ),
            sys_id=sys_id_value,
            name=name_value,
            query=query_value,
            sysparm_query=sysparm_query_value,
            sys_class_name="cmdb_ci",
        )
        success, result = run_main(configuration_item_info, params)

        assert success is False
        assert (
            "parameters are mutually exclusive: sys_id|query|name|sysparm_query"
            in result["msg"]
        )

    def test_fail(self, run_main):
        success, result = run_main(configuration_item_info)

        assert success is False
        assert "instance" in result["msg"]


class TestRun:
    def test_run(self, create_module, table_client, attachment_client):
        module = create_module(
            params=dict(
                instance=dict(
                    host="https://my.host.name", username="user", password="pass"
                ),
                sys_id="01a9ec0d3790200044e0bfc8bcbe5dc3",
                sys_class_name="cmdb_ci",
                query=None,
                sysparm_query=None,
                name=None,
                sysparm_display_value="true",
            )
        )
        table_client.list_records.return_value = [
            dict(p=1, sys_id=1234),
            dict(q=2, sys_id=4321),
            dict(r=3, sys_id=1212),
        ]
        attachment_client.list_records.side_effect = [
            [
                {
                    "content_type": "text/plain",
                    "file_name": "sample_file",
                    "table_name": "change_request",
                    "table_sys_id": 1234,
                    "sys_id": 4444,
                },
            ],
            [],
            [],
        ]

        records = configuration_item_info.run(module, table_client, attachment_client)

        table_client.list_records.assert_called_once_with(
            "cmdb_ci",
            dict(
                sys_id="01a9ec0d3790200044e0bfc8bcbe5dc3", sysparm_display_value="true"
            ),
        )
        attachment_client.list_records.assert_any_call(
            {"table_name": "cmdb_ci", "table_sys_id": 1234}
        )
        attachment_client.list_records.assert_any_call(
            {"table_name": "cmdb_ci", "table_sys_id": 4321}
        )
        attachment_client.list_records.assert_any_call(
            {"table_name": "cmdb_ci", "table_sys_id": 1212}
        )
        assert attachment_client.list_records.call_count == 3
        assert records == [
            dict(
                p=1,
                sys_id=1234,
                attachments=[
                    {
                        "content_type": "text/plain",
                        "file_name": "sample_file",
                        "table_name": "change_request",
                        "table_sys_id": 1234,
                        "sys_id": 4444,
                    },
                ],
            ),
            dict(q=2, sys_id=4321, attachments=[]),
            dict(r=3, sys_id=1212, attachments=[]),
        ]

    @pytest.mark.parametrize(
        "sys_id_value, name_value, query_value, sysparm_query_value, query",
        [
            (
                "01a9ec0d3790200044e0bfc8bcbe5dc3",
                None,
                None,
                None,
                {
                    "sys_id": "01a9ec0d3790200044e0bfc8bcbe5dc3",
                    "sysparm_display_value": "true",
                },
            ),
            (
                None,
                "test.name",
                None,
                None,
                {"name": "test.name", "sysparm_display_value": "true"},
            ),
            (
                None,
                None,
                [{"category": "= Hardware"}],
                None,
                {"sysparm_query": "category=Hardware", "sysparm_display_value": "true"},
            ),
            (
                None,
                None,
                None,
                "category=Hardware",
                {"sysparm_query": "category=Hardware", "sysparm_display_value": "true"},
            ),
        ],
    )
    def test_run_called_with(
        self,
        create_module,
        table_client,
        attachment_client,
        sys_id_value,
        name_value,
        query_value,
        sysparm_query_value,
        query,
    ):
        module = create_module(
            params=dict(
                instance=dict(
                    host="https://my.host.name", username="user", password="pass"
                ),
                sys_id=sys_id_value,
                name=name_value,
                query=query_value,
                sysparm_query=sysparm_query_value,
                sys_class_name="cmdb_ci",
                sysparm_display_value="true",
            )
        )

        table_client.list_records.return_value = []
        attachment_client.list_records.return_value = []

        configuration_item_info.run(module, table_client, attachment_client)

        table_client.list_records.assert_called_once_with("cmdb_ci", query)
