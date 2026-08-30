# -*- coding: utf-8 -*-
# # Copyright: (c) 2021, XLAB Steampunk <steampunk@xlab.si>
#
# GNU General Public License v3.0+ (see COPYING or https://www.gnu.org/licenses/gpl-3.0.txt)

from __future__ import absolute_import, division, print_function

__metaclass__ = type

import sys

import pytest
from ansible_collections.servicenow.itsm.plugins.modules import problem_task

pytestmark = pytest.mark.skipif(
    sys.version_info < (2, 7), reason="requires python2.7 or higher"
)


class TestEnsureAbsent:
    def test_delete_problem_task(self, create_module, table_client):
        module = create_module(
            params=dict(
                instance=dict(host="my.host.name", username="user", password="pass"),
                state="absent",
                number="PTASK0010001",
                sys_id=None,
            )
        )
        table_client.get_record.return_value = dict(state="157", number="PTASK0010001")

        result = problem_task.ensure_absent(module, table_client)

        table_client.delete_record.assert_called_once()
        assert result == (
            True,
            None,
            dict(before=dict(state="closed", number="PTASK0010001"), after=None),
        )

    def test_delete_problem_task_not_present(self, create_module, table_client):
        module = create_module(
            params=dict(
                instance=dict(host="my.host.name", username="user", password="pass"),
                state="absent",
                number=None,
                sys_id="1234",
            ),
        )
        table_client.get_record.return_value = None

        result = problem_task.ensure_absent(module, table_client)

        table_client.delete_record.assert_not_called()
        assert result == (False, None, dict(before=None, after=None))


class TestBuildPayload:
    def test_build_payload(self, create_module, table_client):
        module = create_module(
            params=dict(
                instance=dict(host="my.host.name", username="user", password="pass"),
                state="new",
                type="general",
                configuration_item="P1000001",
                due_date="2020-12-17 13:37:00",
                source_problem="PRB0007601",
                priority="low",
                assignment_group="network",
                assigned_to="admin",
                short_description="sd",
                description="Test problem_task",
                close_notes="closed",
                other=None,
            ),
        )
        table_client.get_record.return_value = {
            "sys_id": "681ccaf9c0a8016400b98a06818d57c7"
        }

        result = problem_task.build_payload(module, table_client)

        assert result["state"] == "new"
        assert result["type"] == "general"
        assert result["configuration_item"] == "P1000001"
        assert result["due_date"] == "2020-12-17 13:37:00"
        assert result["problem"] == "681ccaf9c0a8016400b98a06818d57c7"
        assert result["priority"] == "low"
        assert result["assignment_group"] == "681ccaf9c0a8016400b98a06818d57c7"
        assert result["assigned_to"] == "681ccaf9c0a8016400b98a06818d57c7"
        assert result["short_description"] == "sd"
        assert result["description"] == "Test problem_task"
        assert result["close_notes"] == "closed"

    def test_build_payload_with_other_option(self, create_module, table_client):
        module = create_module(
            params=dict(
                instance=dict(host="my.host.name", username="user", password="pass"),
                state="new",
                type="general",
                configuration_item=None,
                due_date=None,
                source_problem="PRB0007601",
                priority=None,
                assignment_group=None,
                assigned_to="admin",
                short_description="sd",
                description=None,
                close_notes=None,
                other=dict(notify="1"),
            ),
        )
        table_client.get_record.return_value = {
            "sys_id": "681ccaf9c0a8016400b98a06818d57c7"
        }

        result = problem_task.build_payload(module, table_client)

        assert result["state"] == "new"
        assert result["type"] == "general"
        assert result["problem"] == "681ccaf9c0a8016400b98a06818d57c7"
        assert result["assigned_to"] == "681ccaf9c0a8016400b98a06818d57c7"
        assert result["short_description"] == "sd"
        assert result["notify"] == "1"


class TestEnsurePresent:
    def test_ensure_present_create_new(self, create_module, table_client):
        module = create_module(
            params=dict(
                instance=dict(host="my.host.name", username="user", password="pass"),
                state="new",
                type="general",
                number=None,
                sys_id=None,
                configuration_item=None,
                due_date=None,
                source_problem="PRB0007601",
                priority="low",
                assignment_group=None,
                assigned_to="admin",
                short_description="sd",
                description=None,
                close_notes=None,
                other=None,
            ),
        )
        table_client.get_record.return_value = {
            "sys_id": "681ccaf9c0a8016400b98a06818d57c7"
        }
        table_client.create_record.return_value = dict(
            state="151",
            type="general",
            number="PTASK0010001",
            source_problem="PRB0007601",
            priority="4",
            assigned_to="admin",
            short_description="sd",
        )

        result = problem_task.ensure_present(module, table_client)

        table_client.create_record.assert_called_once()
        assert result == (
            True,
            dict(
                state="new",
                type="general",
                number="PTASK0010001",
                source_problem="PRB0007601",
                priority="low",
                assigned_to="admin",
                short_description="sd",
            ),
            dict(
                before=None,
                after=dict(
                    state="new",
                    type="general",
                    number="PTASK0010001",
                    source_problem="PRB0007601",
                    priority="low",
                    assigned_to="admin",
                    short_description="sd",
                ),
            ),
        )

    def test_ensure_present_nothing_to_do(self, create_module, table_client):
        module = create_module(
            params=dict(
                instance=dict(host="my.host.name", username="user", password="pass"),
                number="PTASK0010001",
                state="new",
                type="general",
                sys_id=None,
                configuration_item=None,
                due_date=None,
                source_problem=None,
                priority="low",
                assignment_group=None,
                assigned_to=None,
                short_description="sd",
                description=None,
                close_notes=None,
                other=None,
            ),
        )
        table_client.get_record.return_value = dict(
            state="151",
            number="PTASK0010001",
            short_description="sd",
            priority="4",
            type="general",
        )

        result = problem_task.ensure_present(module, table_client)

        table_client.get_record.assert_called_once()
        assert result == (
            False,
            dict(
                state="new",
                number="PTASK0010001",
                short_description="sd",
                priority="low",
                type="general",
            ),
            dict(
                before=dict(
                    state="new",
                    number="PTASK0010001",
                    short_description="sd",
                    priority="low",
                    type="general",
                ),
                after=dict(
                    state="new",
                    number="PTASK0010001",
                    short_description="sd",
                    priority="low",
                    type="general",
                ),
            ),
        )

    def test_ensure_present_update(self, mocker, create_module, table_client):
        module = create_module(
            params=dict(
                instance=dict(host="my.host.name", username="user", password="pass"),
                state="assess",
                type="general",
                number="PTASK0010001",
                sys_id=None,
                configuration_item=None,
                due_date=None,
                source_problem=None,
                priority="low",
                assignment_group=None,
                assigned_to=None,
                short_description="sd",
                description=None,
                close_notes=None,
                other=None,
            ),
        )
        payload_mocker = mocker.patch.object(problem_task, "build_payload")
        payload_mocker.return_value = dict(
            state="assess", number="PTASK0010001", short_description="sd"
        )
        table_client.get_record.return_value = dict(
            state="151", number="PTASK0010001", short_description="sd"
        )
        table_client.update_record.return_value = dict(
            state="152", number="PTASK0010001", short_description="sd"
        )

        result = problem_task.ensure_present(module, table_client)

        table_client.update_record.assert_called_once()
        assert result == (
            True,
            dict(
                state="assess",
                number="PTASK0010001",
                short_description="sd",
            ),
            dict(
                before=dict(state="new", number="PTASK0010001", short_description="sd"),
                after=dict(
                    state="assess",
                    number="PTASK0010001",
                    short_description="sd",
                ),
            ),
        )
