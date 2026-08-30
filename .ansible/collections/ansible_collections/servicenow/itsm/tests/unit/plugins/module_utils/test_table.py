# -*- coding: utf-8 -*-
# Copyright: (c) 2021, XLAB Steampunk <steampunk@xlab.si>
#
# GNU General Public License v3.0+ (see COPYING or https://www.gnu.org/licenses/gpl-3.0.txt)

from __future__ import absolute_import, division, print_function

__metaclass__ = type

import sys

import pytest
from ansible_collections.servicenow.itsm.plugins.module_utils import errors, table
from ansible_collections.servicenow.itsm.plugins.module_utils.client import Response

pytestmark = pytest.mark.skipif(
    sys.version_info < (2, 7), reason="requires python2.7 or higher"
)


class TestTableListRecords:
    def test_empty_response(self, client):
        client.get.return_value = Response(
            200, '{"result": []}', {"X-Total-Count": "0"}
        )
        t = table.TableClient(client)

        records = t.list_records("my_table")

        assert [] == records
        client.get.assert_called_once_with(
            "api/now/table/my_table",
            query=dict(
                sysparm_exclude_reference_link="true",
                sysparm_limit=1000,
                sysparm_offset=0,
            ),
        )

    def test_non_empty_response(self, client):
        client.get.return_value = Response(
            200, '{"result": [{"a": 3, "b": "sys_id"}]}', {"X-Total-Count": "1"}
        )
        t = table.TableClient(client)

        records = t.list_records("my_table")

        assert [dict(a=3, b="sys_id")] == records

    def test_query_passing(self, client):
        client.get.return_value = Response(
            200, '{"result": []}', {"X-Total-Count": "0"}
        )
        t = table.TableClient(client)

        t.list_records("my_table", dict(a="b"))

        client.get.assert_called_once_with(
            "api/now/table/my_table",
            query=dict(
                sysparm_exclude_reference_link="true",
                a="b",
                sysparm_limit=1000,
                sysparm_offset=0,
            ),
        )

    def test_pagination(self, client):
        client.get.side_effect = (
            Response(
                200, '{"result": [{"a": 3, "b": "sys_id"}]}', {"X-Total-Count": "2"}
            ),
            Response(
                200, '{"result": [{"a": 2, "b": "sys_ie"}]}', {"X-Total-Count": "2"}
            ),
        )
        t = table.TableClient(client, batch_size=1)

        records = t.list_records("my_table")

        assert [dict(a=3, b="sys_id"), dict(a=2, b="sys_ie")] == records
        assert 2 == len(client.get.mock_calls)
        client.get.assert_any_call(
            "api/now/table/my_table",
            query=dict(
                sysparm_exclude_reference_link="true", sysparm_limit=1, sysparm_offset=0
            ),
        )
        client.get.assert_any_call(
            "api/now/table/my_table",
            query=dict(
                sysparm_exclude_reference_link="true", sysparm_limit=1, sysparm_offset=1
            ),
        )


class TestTableGetRecord:
    def test_single_match(self, client):
        client.get.return_value = Response(
            200, '{"result": [{"a": 3, "b": "sys_id"}]}', {"X-Total-Count": "1"}
        )
        t = table.TableClient(client)

        record = t.get_record("my_table", dict(our="query"))

        assert dict(a=3, b="sys_id") == record
        client.get.assert_called_with(
            "api/now/table/my_table",
            query=dict(
                sysparm_exclude_reference_link="true",
                our="query",
                sysparm_limit=1000,
                sysparm_offset=0,
            ),
        )

    def test_multiple_matches(self, client):
        client.get.return_value = Response(
            200, '{"result": [{"a": 3}, {"b": 4}]}', {"X-Total-Count": "1"}
        )
        t = table.TableClient(client)

        with pytest.raises(errors.ServiceNowError, match="2"):
            t.get_record("my_table", dict(our="query"))

    def test_zero_matches(self, client):
        client.get.return_value = Response(
            200, '{"result": []}', {"X-Total-Count": "0"}
        )
        t = table.TableClient(client)

        assert t.get_record("my_table", dict(our="query")) is None

    def test_zero_matches_fail(self, client):
        client.get.return_value = Response(
            200, '{"result": []}', {"X-Total-Count": "0"}
        )
        t = table.TableClient(client)

        with pytest.raises(errors.ServiceNowError, match="No"):
            t.get_record("my_table", dict(our="query"), must_exist=True)


class TestTableGetRecordBySysId:
    def test_get_record_by_sys_id(self, client):
        client.get.return_value = Response(
            200, '{"result": {"a": 3, "b": "sys_id"}}', {"X-Total-Count": "1"}
        )
        t = table.TableClient(client)

        record = t.get_record_by_sys_id("my_table", "sys_id")

        assert dict(a=3, b="sys_id") == record
        client.get.assert_called_with("api/now/table/my_table/sys_id")

    def test_get_record_by_sys_id_must_exists(self, client):
        client.get.return_value = Response(
            200, '{"result": {"a": 3, "b": "sys_id"}}', {"X-Total-Count": "1"}
        )
        t = table.TableClient(client)

        record = t.get_record_by_sys_id("my_table", "sys_id", must_exist=True)

        assert dict(a=3, b="sys_id") == record
        client.get.assert_called_with("api/now/table/my_table/sys_id")

    def test_get_record_by_sys_id_no_record(self, client):
        client.get.return_value = Response(
            404, '{"error": {"message": "no record found"}}', {}
        )
        t = table.TableClient(client)

        record = t.get_record_by_sys_id("my_table", "sys_id")

        assert not record
        client.get.assert_called_with("api/now/table/my_table/sys_id")

    def test_get_record_by_sys_id_no_record_must_exists(self, client):
        client.get.return_value = Response(
            404, '{"error": {"message": "no record found"}}', {}
        )
        t = table.TableClient(client)

        with pytest.raises(errors.ServiceNowError, match="No"):
            t.get_record_by_sys_id("my_table", "sys_id", must_exist=True)


class TestTableCreateRecord:
    def test_normal_mode(self, client):
        client.post.return_value = Response(201, '{"result": {"a": 3, "b": "sys_id"}}')
        t = table.TableClient(client)

        record = t.create_record("my_table", dict(a=4), False)

        assert dict(a=3, b="sys_id") == record
        client.post.assert_called_with(
            "api/now/table/my_table",
            dict(a=4),
            query=dict(sysparm_exclude_reference_link="true"),
        )

    def test_check_mode(self, client):
        client.post.return_value = Response(201, '{"result": {"a": 3, "b": "sys_id"}}')
        t = table.TableClient(client)

        record = t.create_record("my_table", dict(a=4), True)

        assert dict(a=4) == record
        client.post.assert_not_called()


class TestTableUpdateRecord:
    def test_normal_mode(self, client):
        client.patch.return_value = Response(200, '{"result": {"a": 3, "b": "sys_id"}}')
        t = table.TableClient(client)

        record = t.update_record("my_table", dict(sys_id="id"), dict(a=4), False)

        assert dict(a=3, b="sys_id") == record
        client.patch.assert_called_with(
            "api/now/table/my_table/id",
            dict(a=4),
            query=dict(sysparm_exclude_reference_link="true"),
        )

    def test_check_mode(self, client):
        client.patch.return_value = Response(200, '{"result": {"a": 3, "b": "sys_id"}}')
        t = table.TableClient(client)

        record = t.update_record("my_table", dict(sys_id="id"), dict(a=4), True)

        assert dict(sys_id="id", a=4) == record
        client.patch.assert_not_called()


class TestTableDeleteRecord:
    def test_normal_mode(self, client):
        client.delete.return_value = Response(204, "")
        t = table.TableClient(client)

        t.delete_record("my_table", dict(sys_id="id"), False)

        client.delete.assert_called_with("api/now/table/my_table/id")

    def test_check_mode(self, client):
        client.delete.return_value = Response(204, "")
        t = table.TableClient(client)

        t.delete_record("my_table", dict(sys_id="id"), True)

        client.delete.assert_not_called()


class TestFindUser:
    def test_user_name_lookup(self, table_client):
        table_client.get_record.return_value = dict(sys_id="1234", user_name="test")

        user = table.find_user(table_client, "test")

        assert dict(sys_id="1234", user_name="test") == user


class TestFindChangeRequest:
    def test_change_request_lookup(self, table_client):
        table_client.get_record.return_value = dict(sys_id="1234", number="TST123")

        user = table.find_change_request(table_client, "TST123")

        assert dict(sys_id="1234", number="TST123") == user


class TestFindConfigurationItem:
    def test_change_request_lookup(self, table_client):
        table_client.get_record.return_value = dict(sys_id="1234", name="TST123")

        user = table.find_change_request(table_client, "TST123")

        assert dict(sys_id="1234", name="TST123") == user
