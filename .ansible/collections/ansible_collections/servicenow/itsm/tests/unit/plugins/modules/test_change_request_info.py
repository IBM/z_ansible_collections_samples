# -*- coding: utf-8 -*-
# # Copyright: (c) 2021, XLAB Steampunk <steampunk@xlab.si>
#
# GNU General Public License v3.0+ (see COPYING or https://www.gnu.org/licenses/gpl-3.0.txt)

from __future__ import absolute_import, division, print_function

__metaclass__ = type

import sys

import pytest
from ansible_collections.servicenow.itsm.plugins.modules import change_request_info

pytestmark = pytest.mark.skipif(
    sys.version_info < (2, 7), reason="requires python2.7 or higher"
)


class TestRemapCaller:
    def test_remap_params(self, table_client):
        query = [
            {"type": ("=", "normal")},
            {"hold_reason": ("=", "Some reason")},
            {"requested_by": ("=", "some.user")},
            {"assignment_group": ("=", "Network")},
            {"template": ("=", "Some template")},
            {"impact": ("=", "low")},
        ]
        table_client.get_record.side_effect = [
            {"sys_id": "681ccaf9c0a8016400b98a06818d57c7"},
            {"sys_id": "d625dccec0a8016700a222a0f7900d06"},
            {"sys_id": "deb8544047810200e90d87e8dee490af"},
        ]

        result = change_request_info.remap_params(query, table_client)

        assert result == [
            {"chg_model": ("=", "normal")},
            {"on_hold_reason": ("=", "Some reason")},
            {"requested_by": ("=", "681ccaf9c0a8016400b98a06818d57c7")},
            {"assignment_group": ("=", "d625dccec0a8016700a222a0f7900d06")},
            {
                "std_change_producer_version": (
                    "=",
                    "deb8544047810200e90d87e8dee490af",
                )
            },
            {"impact": ("=", "low")},
        ]


class TestMain:
    def test_minimal_set_of_params(self, run_main):
        params = dict(
            instance=dict(
                host="https://my.host.name", username="user", password="pass"
            ),
        )
        success, result = run_main(change_request_info, params)

        assert success is True

    def test_all_params(self, run_main):
        params = dict(
            instance=dict(
                host="https://my.host.name", username="user", password="pass"
            ),
            sys_id="id",
            number="n",
        )
        success, result = run_main(change_request_info, params)

        assert success is True

    def test_fail(self, run_main):
        success, result = run_main(change_request_info)

        assert success is False
        assert "instance" in result["msg"]


class TestRun:
    def test_run(self, create_module, table_client, attachment_client):
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

        change_requests = change_request_info.run(
            module, table_client, attachment_client
        )

        table_client.list_records.assert_called_once_with(
            "change_request", dict(number="n", sysparm_display_value="true")
        )

        attachment_client.list_records.assert_any_call(
            {"table_name": "change_request", "table_sys_id": 1234}
        )
        attachment_client.list_records.assert_any_call(
            {"table_name": "change_request", "table_sys_id": 4321}
        )
        attachment_client.list_records.assert_any_call(
            {"table_name": "change_request", "table_sys_id": 1212}
        )
        assert attachment_client.list_records.call_count == 3
        assert change_requests == [
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
