# -*- coding: utf-8 -*-
# # Copyright: (c) 2021, XLAB Steampunk <steampunk@xlab.si>
#
# GNU General Public License v3.0+ (see COPYING or https://www.gnu.org/licenses/gpl-3.0.txt)

from __future__ import absolute_import, division, print_function

__metaclass__ = type

import sys

import pytest
from ansible_collections.servicenow.itsm.plugins.modules import incident_info

pytestmark = pytest.mark.skipif(
    sys.version_info < (2, 7), reason="requires python2.7 or higher"
)


class TestRemapCaller:
    def test_remap_caller(self, table_client):
        query = [{"caller": ("=", "some.user"), "state": ("=", "new")}]
        table_client.get_record.return_value = {
            "sys_id": "681ccaf9c0a8016400b98a06818d57c7"
        }

        result = incident_info.remap_caller(query, table_client)

        assert result == [
            {
                "caller_id": ("=", "681ccaf9c0a8016400b98a06818d57c7"),
                "state": ("=", "new"),
            }
        ]


class TestMain:
    def test_minimal_set_of_params(self, run_main):
        params = dict(
            instance=dict(
                host="https://my.host.name", username="user", password="pass"
            ),
        )
        success, result = run_main(incident_info, params)

        assert success is True

    def test_all_params(self, run_main):
        params = dict(
            instance=dict(
                host="https://my.host.name", username="user", password="pass"
            ),
            sys_id="id",
            number="INC001",
        )
        success, result = run_main(incident_info, params)

        assert success is True

    def test_fail(self, run_main):
        success, result = run_main(incident_info)

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
                number="INC001",
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

        records = incident_info.run(module, table_client, attachment_client)

        table_client.list_records.assert_called_once_with(
            "incident", dict(number="INC001", sysparm_display_value="true")
        )

        attachment_client.list_records.assert_any_call(
            {"table_name": "incident", "table_sys_id": 1234}
        )
        attachment_client.list_records.assert_any_call(
            {"table_name": "incident", "table_sys_id": 4321}
        )
        attachment_client.list_records.assert_any_call(
            {"table_name": "incident", "table_sys_id": 1212}
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
