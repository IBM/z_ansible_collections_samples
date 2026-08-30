# -*- coding: utf-8 -*-
# # Copyright: (c) 2022, XLAB Steampunk <steampunk@xlab.si>
#
# GNU General Public License v3.0+ (see COPYING or https://www.gnu.org/licenses/gpl-3.0.txt)

from __future__ import absolute_import, division, print_function

__metaclass__ = type

import sys

import pytest
from ansible_collections.servicenow.itsm.plugins.modules import api

pytestmark = pytest.mark.skipif(
    sys.version_info < (2, 7), reason="requires python2.7 or higher"
)


class TestUpdateResource:
    def test_update_resource_not_present(self, create_module, table_client):
        module = create_module(
            params=dict(
                instance=dict(host="my.host.name", username="user", password="pass"),
                sys_id="my_sys_id",
                resource="incident",
                action="patch",
                data=dict(
                    state="new",
                    caller=None,
                    short_description="Test incident",
                    impact="low",
                    urgency="low",
                    number=None,
                    sys_id=None,
                    description=None,
                    close_code=None,
                    close_notes=None,
                    hold_reason=None,
                    other=None,
                    attachments=None,
                ),
            )
        )

        table_client.get_record_by_sys_id.return_value = None

        result = api.run(module, table_client)

        assert result == (False, None, dict(before=None, after=None))

    def test_present_resource_present(self, create_module, table_client):
        module = create_module(
            params=dict(
                instance=dict(host="my.host.name", username="user", password="pass"),
                resource="incident",
                sys_id="my_sys_id",
                action="patch",
                data=dict(state="new"),
            )
        )

        table_client.get_record_by_sys_id.return_value = dict(
            number="INC0000001",
            short_description="Test incident",
            impact="3",
            urgency="3",
            sys_id="1234",
            state="old",
        )

        table_client.update_record.return_value = dict(
            number="INC0000001",
            short_description="Test incident",
            impact="3",
            urgency="3",
            sys_id="1234",
            state="new",
        )

        result = api.run(module, table_client)

        assert result == (
            True,
            dict(
                number="INC0000001",
                short_description="Test incident",
                impact="3",
                urgency="3",
                sys_id="1234",
                state="new",
            ),
            dict(
                before=dict(
                    number="INC0000001",
                    short_description="Test incident",
                    impact="3",
                    urgency="3",
                    sys_id="1234",
                    state="old",
                ),
                after=dict(
                    number="INC0000001",
                    short_description="Test incident",
                    impact="3",
                    urgency="3",
                    sys_id="1234",
                    state="new",
                ),
            ),
        )


class TestCreateResource:
    def test_create_resource_no_sys_id(self, create_module, table_client):
        module = create_module(
            params=dict(
                instance=dict(host="my.host.name", username="user", password="pass"),
                resource="incident",
                action="post",
                data=dict(
                    state="new",
                    caller=None,
                    short_description="Test incident",
                    impact="low",
                    urgency="low",
                    number=None,
                    sys_id=None,
                    description=None,
                    close_code=None,
                    close_notes=None,
                    hold_reason=None,
                    other=None,
                    attachments=None,
                ),
            )
        )

        table_client.create_record.return_value = dict(
            state="1",
            number="INC0000001",
            short_description="Test incident",
            impact="3",
            urgency="3",
            sys_id="1234",
        )

        result = api.run(module, table_client)

        table_client.create_record.assert_called_once()

        assert result == (
            True,
            dict(
                state="1",
                number="INC0000001",
                short_description="Test incident",
                impact="3",
                urgency="3",
                sys_id="1234",
            ),
            dict(
                before=None,
                after=dict(
                    state="1",
                    number="INC0000001",
                    short_description="Test incident",
                    impact="3",
                    urgency="3",
                    sys_id="1234",
                ),
            ),
        )

    def test_create_resource_sys_id(self, create_module, table_client):
        module = create_module(
            params=dict(
                instance=dict(host="my.host.name", username="user", password="pass"),
                resource="incident",
                action="post",
                sys_id="my-sys-id",
            )
        )

        table_client.create_record.return_value = dict(
            state="1",
            number="INC0000001",
            short_description="Test incident",
            impact="3",
            urgency="3",
            sys_id="1234",
        )

        result = api.run(module, table_client)

        table_client.create_record.assert_called_once()

        assert result == (
            True,
            dict(
                state="1",
                number="INC0000001",
                short_description="Test incident",
                impact="3",
                urgency="3",
                sys_id="1234",
            ),
            dict(
                before=None,
                after=dict(
                    state="1",
                    number="INC0000001",
                    short_description="Test incident",
                    impact="3",
                    urgency="3",
                    sys_id="1234",
                ),
            ),
        )


class TestDeleteResource:
    def test_delete_resource_not_present(self, create_module, table_client):
        module = create_module(
            params=dict(
                instance=dict(host="my.host.name", username="user", password="pass"),
                resource="incident",
                sys_id="my_sys_id",
                action="delete",
            )
        )

        table_client.get_record_by_sys_id.return_value = None

        result = api.run(module, table_client)

        assert result == (False, None, dict(before=None, after=None))

    def test_delete_resource_present(self, create_module, table_client):
        module = create_module(
            params=dict(
                instance=dict(host="my.host.name", username="user", password="pass"),
                resource="incident",
                sys_id="my_sys_id",
                action="delete",
            )
        )

        table_client.get_record_by_sys_id.return_value = dict(
            state="1",
            number="INC0000001",
            short_description="Test incident",
            impact="3",
            urgency="3",
            sys_id="1234",
        )

        result = api.run(module, table_client)

        assert result == (
            True,
            None,
            dict(
                before=dict(
                    state="1",
                    number="INC0000001",
                    short_description="Test incident",
                    impact="3",
                    urgency="3",
                    sys_id="1234",
                ),
                after=None,
            ),
        )
