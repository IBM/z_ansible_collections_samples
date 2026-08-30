# -*- coding: utf-8 -*-
# # Copyright: (c) 2021, XLAB Steampunk <steampunk@xlab.si>
#
# GNU General Public License v3.0+ (see COPYING or https://www.gnu.org/licenses/gpl-3.0.txt)

from __future__ import absolute_import, division, print_function

__metaclass__ = type

import sys

import pytest
from ansible_collections.servicenow.itsm.plugins.modules import change_request_task_info

pytestmark = pytest.mark.skipif(
    sys.version_info < (2, 7), reason="requires python2.7 or higher"
)


class TestRemapCaller:
    def test_remap_params_direct(self, table_client):
        query = [
            {"type": ("=", "planning")},
            {"hold_reason": ("=", "Some reason")},
            {"configuration_item_id": ("=", "4321")},
            {"change_request_id": ("=", "1234")},
            {"assigned_to": ("=", "some.user")},
            {"assignment_group": ("=", "some.group")},
            {"short_description": ("=", "Unmapped value")},
        ]
        table_client.get_record.side_effect = [
            {"sys_id": "c248952584a34ae1a851a38d7fc08fcf"},
            {"sys_id": "e361760abb09450da835b5e4f2271dcf"},
            {"sys_id": "4488052c5f5248f8b787ec9df5459c09"},
            {"sys_id": "18fee81900824bfeb78e8c9062fb8f06"},
        ]

        result = change_request_task_info.remap_params(query, table_client)

        assert result == [
            {"change_task_type": ("=", "planning")},
            {"on_hold_reason": ("=", "Some reason")},
            {"cmdb_ci": ("=", "4321")},
            {"change_request": ("=", "1234")},
            {"assigned_to": ("=", "c248952584a34ae1a851a38d7fc08fcf")},
            {"assignment_group": ("=", "e361760abb09450da835b5e4f2271dcf")},
            {"short_description": ("=", "Unmapped value")},
        ]

    def test_remap_params_full(self, table_client):
        query = [
            {"type": ("=", "planning")},
            {"hold_reason": ("=", "Some reason")},
            {"configuration_item": ("=", "config item")},
            {"change_request_number": ("=", "CR1234")},
            {"assigned_to": ("=", "some.user")},
            {"assignment_group": ("=", "some.group")},
            {"short_description": ("=", "Unmapped value")},
        ]
        table_client.get_record.side_effect = [
            {"sys_id": "c248952584a34ae1a851a38d7fc08fcf"},
            {"sys_id": "e361760abb09450da835b5e4f2271dcf"},
            {"sys_id": "4488052c5f5248f8b787ec9df5459c09"},
            {"sys_id": "18fee81900824bfeb78e8c9062fb8f06"},
        ]

        result = change_request_task_info.remap_params(query, table_client)

        assert result == [
            {"change_task_type": ("=", "planning")},
            {"on_hold_reason": ("=", "Some reason")},
            {"cmdb_ci": ("=", "c248952584a34ae1a851a38d7fc08fcf")},
            {"change_request": ("=", "e361760abb09450da835b5e4f2271dcf")},
            {"assigned_to": ("=", "4488052c5f5248f8b787ec9df5459c09")},
            {"assignment_group": ("=", "18fee81900824bfeb78e8c9062fb8f06")},
            {"short_description": ("=", "Unmapped value")},
        ]


class TestMain:
    def test_minimal_set_of_params(self, run_main):
        params = dict(
            instance=dict(
                host="https://my.host.name", username="user", password="pass"
            ),
        )
        success, result = run_main(change_request_task_info, params)

        assert success is True

    def test_all_params(self, run_main):
        params = dict(
            instance=dict(
                host="https://my.host.name", username="user", password="pass"
            ),
            sys_id="id",
            number="n",
        )
        success, result = run_main(change_request_task_info, params)

        assert success is True

    def test_fail(self, run_main):
        success, result = run_main(change_request_task_info)

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

        change_requests = change_request_task_info.run(module, table_client)

        table_client.list_records.assert_called_once_with(
            "change_task", dict(number="n", sysparm_display_value="true")
        )
        assert change_requests == [dict(p=1), dict(q=2), dict(r=3)]
