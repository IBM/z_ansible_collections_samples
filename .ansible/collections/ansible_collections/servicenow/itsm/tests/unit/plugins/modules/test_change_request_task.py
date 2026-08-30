# -*- coding: utf-8 -*-
# # Copyright: (c) 2021, XLAB Steampunk <steampunk@xlab.si>
#
# GNU General Public License v3.0+ (see COPYING or https://www.gnu.org/licenses/gpl-3.0.txt)

from __future__ import absolute_import, division, print_function

__metaclass__ = type

import sys

import pytest
from ansible_collections.servicenow.itsm.plugins.module_utils import errors
from ansible_collections.servicenow.itsm.plugins.modules import change_request_task

pytestmark = pytest.mark.skipif(
    sys.version_info < (2, 7), reason="requires python2.7 or higher"
)


class TestEnsureAbsent:
    def test_delete_change_request(self, create_module, table_client):
        module = create_module(
            params=dict(
                instance=dict(host="my.host.name", username="user", password="pass"),
                state="absent",
                number="CTASK0000001",
                sys_id=None,
                on_hold=None,
            )
        )
        table_client.get_record.return_value = dict(state="3", number="CTASK0000001")

        result = change_request_task.ensure_absent(module, table_client)

        table_client.delete_record.assert_called_once()
        assert result == (
            True,
            None,
            dict(before=dict(state="closed", number="CTASK0000001"), after=None),
        )

    def test_delete_change_request_not_present(self, create_module, table_client):
        module = create_module(
            params=dict(
                instance=dict(host="my.host.name", username="user", password="pass"),
                state="absent",
                number=None,
                sys_id="1234",
                on_hold=None,
            ),
        )
        table_client.get_record.return_value = None

        result = change_request_task.ensure_absent(module, table_client)

        table_client.delete_record.assert_not_called()
        assert result == (False, None, dict(before=None, after=None))


class TestValidateParams:
    VALID_PARAMS = dict(
        state="closed",
        close_code="successful",
        close_notes="Solved",
        description="dsc",
        short_description="sd",
        assignment_group=None,
        assignment_group_id=None,
        on_hold=None,
    )
    VALID_PARAMS_HOLD = dict(
        state="in_progress",
        close_code="successful",
        close_notes="Solved",
        assignment_group=None,
        assignment_group_id=None,
        description="dsc",
        short_description="sd",
        on_hold=True,
        hold_reason="Waiting",
    )

    @pytest.mark.parametrize(
        "missing_field",
        ["close_code", "close_notes", "description", "short_description"],
    )
    def test_validate_params_missing_field(self, missing_field):
        params = self.VALID_PARAMS.copy()
        params[missing_field] = None

        with pytest.raises(errors.ServiceNowError, match=missing_field):
            change_request_task.validate_params(params)

    @pytest.mark.parametrize(
        "missing_field",
        [
            "hold_reason",
        ],
    )
    def test_validate_params_missing_on_hold_field(self, missing_field):
        params = self.VALID_PARAMS_HOLD.copy()
        params[missing_field] = None

    def test_validate_params(self):
        change_request_task.validate_params(self.VALID_PARAMS)

    def test_validate_params_on_hold(self):
        change_request_task.validate_params(self.VALID_PARAMS_HOLD)


class TestEnsurePresent:
    def test_ensure_present_create_new(self, create_module, table_client):
        module = create_module(
            params=dict(
                instance=dict(host="my.host.name", username="user", password="pass"),
                configuration_item_id=None,
                configuration_item=None,
                change_request_id=None,
                change_request_number=None,
                type="planning",
                state="open",
                assigned_to=None,
                assignment_group=None,
                assignment_group_id=None,
                short_description="sd",
                description="description",
                on_hold=None,
                hold_reason=None,
                planned_start_date=None,
                planned_end_date=None,
                close_code=None,
                close_notes=None,
                other=None,
                sys_id=None,
                number=None,
            ),
        )
        table_client.create_record.return_value = dict(
            change_task_type="planning",
            state="1",
            short_description="sd",
            description="description",
            number="CTASK0000001",
        )

        result = change_request_task.ensure_present(module, table_client)

        table_client.create_record.assert_called_once()
        assert result == (
            True,
            dict(
                change_task_type="planning",
                state="open",
                short_description="sd",
                description="description",
                number="CTASK0000001",
            ),
            dict(
                before=None,
                after=dict(
                    change_task_type="planning",
                    state="open",
                    short_description="sd",
                    description="description",
                    number="CTASK0000001",
                ),
            ),
        )

    def test_ensure_present_nothing_to_do(self, create_module, table_client):
        module = create_module(
            params=dict(
                instance=dict(host="my.host.name", username="user", password="pass"),
                configuration_item_id=None,
                configuration_item=None,
                change_request_id=None,
                change_request_number=None,
                type="planning",
                state="open",
                assigned_to=None,
                assignment_group=None,
                assignment_group_id=None,
                short_description="sd",
                description="description",
                on_hold=None,
                hold_reason=None,
                planned_start_date=None,
                planned_end_date=None,
                close_code=None,
                close_notes=None,
                other=None,
                sys_id=None,
                number="CTASK0000001",
            ),
        )
        table_client.get_record.return_value = dict(
            change_task_type="planning",
            state="1",
            short_description="sd",
            description="description",
            number="CTASK0000001",
        )

        result = change_request_task.ensure_present(module, table_client)

        table_client.get_record.assert_called_once()
        assert result == (
            False,
            dict(
                change_task_type="planning",
                state="open",
                short_description="sd",
                description="description",
                number="CTASK0000001",
            ),
            dict(
                before=dict(
                    change_task_type="planning",
                    state="open",
                    short_description="sd",
                    description="description",
                    number="CTASK0000001",
                ),
                after=dict(
                    change_task_type="planning",
                    state="open",
                    short_description="sd",
                    description="description",
                    number="CTASK0000001",
                ),
            ),
        )

    def test_ensure_present_update(self, create_module, table_client):
        module = create_module(
            params=dict(
                instance=dict(host="my.host.name", username="user", password="pass"),
                configuration_item_id=None,
                configuration_item=None,
                change_request_id=None,
                change_request_number=None,
                type="planning",
                state="in_progress",
                assigned_to=None,
                assignment_group=None,
                assignment_group_id=None,
                short_description="sd",
                description="description",
                on_hold=None,
                hold_reason=None,
                planned_start_date=None,
                planned_end_date=None,
                close_code=None,
                close_notes=None,
                other=None,
                sys_id=None,
                number="CTASK0000001",
            ),
        )

        table_client.get_record.return_value = dict(
            change_task_type="planning",
            state="1",
            short_description="sd",
            description="description",
            number="CTASK0000001",
        )
        table_client.update_record.return_value = dict(
            change_task_type="planning",
            state="2",
            short_description="sd",
            description="description",
            number="CTASK0000001",
        )

        result = change_request_task.ensure_present(module, table_client)

        table_client.update_record.assert_called_once()
        assert result == (
            True,
            dict(
                change_task_type="planning",
                state="in_progress",
                short_description="sd",
                description="description",
                number="CTASK0000001",
            ),
            dict(
                before=dict(
                    change_task_type="planning",
                    state="open",
                    short_description="sd",
                    description="description",
                    number="CTASK0000001",
                ),
                after=dict(
                    change_task_type="planning",
                    state="in_progress",
                    short_description="sd",
                    description="description",
                    number="CTASK0000001",
                ),
            ),
        )


class TestBuildPayload:
    def test_build_payload(self, create_module, table_client):
        module = create_module(
            params=dict(
                instance=dict(host="my.host.name", username="user", password="pass"),
                configuration_item_id=None,
                configuration_item="config item",
                change_request_id=None,
                change_request_number="CR1234",
                type="planning",
                state="open",
                assigned_to="some.user",
                assignment_group="some.group",
                assignment_group_id=None,
                short_description="UV",
                description="Unmapped value",
                on_hold=None,
                hold_reason="Some reason",
                planned_start_date=None,
                planned_end_date=None,
                close_code=None,
                close_notes=None,
                other=None,
                sys_id=None,
                number=None,
            ),
        )
        table_client.get_record.side_effect = [
            {"sys_id": "c248952584a34ae1a851a38d7fc08fcf"},
            {"sys_id": "e361760abb09450da835b5e4f2271dcf"},
            {"sys_id": "4488052c5f5248f8b787ec9df5459c09"},
            {"sys_id": "18fee81900824bfeb78e8c9062fb8f06"},
        ]

        result = change_request_task.build_payload(module, table_client)

        assert result["change_task_type"] == "planning"
        assert result["on_hold_reason"] == "Some reason"
        assert result["cmdb_ci"] == "c248952584a34ae1a851a38d7fc08fcf"
        assert result["change_request"] == "e361760abb09450da835b5e4f2271dcf"
        assert result["assigned_to"] == "4488052c5f5248f8b787ec9df5459c09"
        assert result["assignment_group"] == "18fee81900824bfeb78e8c9062fb8f06"
        assert result["short_description"] == "UV"
        assert "type" not in result
        assert "hold_reason" not in result
        assert "configuration_item_id" not in result
        assert "configuration_item" not in result
        assert "change_request_id" not in result
        assert "change_request_number" not in result
        assert "hold_reason" not in result
        assert "hold_reason" not in result

    def test_build_payload_with_other_option(self, create_module, table_client):
        module = create_module(
            params=dict(
                instance=dict(host="my.host.name", username="user", password="pass"),
                configuration_item_id="1234",
                configuration_item=None,
                change_request_id="4321",
                change_request_number=None,
                type="planning",
                state="open",
                assigned_to="some.user",
                assignment_group="some.group",
                assignment_group_id=None,
                short_description="UV",
                description="Unmapped value",
                on_hold=None,
                hold_reason="Some reason",
                planned_start_date=None,
                planned_end_date=None,
                close_code=None,
                close_notes=None,
                other=None,
                sys_id=None,
                number=None,
            ),
        )
        table_client.get_record.side_effect = [
            {"sys_id": "c248952584a34ae1a851a38d7fc08fcf"},
            {"sys_id": "e361760abb09450da835b5e4f2271dcf"},
        ]

        result = change_request_task.build_payload(module, table_client)

        assert result["change_task_type"] == "planning"
        assert result["on_hold_reason"] == "Some reason"
        assert result["cmdb_ci"] == "1234"
        assert result["change_request"] == "4321"
        assert result["assigned_to"] == "c248952584a34ae1a851a38d7fc08fcf"
        assert result["assignment_group"] == "e361760abb09450da835b5e4f2271dcf"
        assert result["short_description"] == "UV"
        assert "type" not in result
        assert "hold_reason" not in result
        assert "configuration_item_id" not in result
        assert "configuration_item" not in result
        assert "change_request_id" not in result
        assert "change_request_number" not in result
        assert "hold_reason" not in result
        assert "hold_reason" not in result


class TestSupersetWithDateCheck:
    @pytest.mark.parametrize(
        "superset,candidate",
        [
            (dict(), dict()),
            (dict(a=1), dict()),
            (dict(a=1), dict(a=1)),
            (dict(a=1, b=2), dict(b=2)),
        ],
    )
    def test_valid_superset(self, superset, candidate):
        assert change_request_task.is_superset_with_date(superset, candidate) is True

    @pytest.mark.parametrize(
        "superset,candidate",
        [
            (dict(), dict(a=1)),  # superset is missing a key
            (dict(a=1), dict(a=2)),  # key value is different
            (dict(param="2021-07-09 08:40:33"), dict(param="2021-07-09T08:40:33")),
        ],
    )
    def test_not_a_superset(self, superset, candidate):
        assert change_request_task.is_superset_with_date(superset, candidate) is False

    @pytest.mark.parametrize(
        "record,params",
        [
            (
                dict(planned_start_date="2021-07-09T08:40:33"),
                dict(planned_start_date="2021-07-09T08:40:33"),
            ),
            (
                dict(planned_start_date="2021-07-09T08:40:33"),
                dict(planned_start_date="2021-07-09 08:40:33"),
            ),
            (
                dict(planned_start_date="2021-07-09 08:40:33"),
                dict(planned_start_date="2021-07-09T08:40:33"),
            ),
            (
                dict(planned_start_date="2021-07-09 08:40:33"),
                dict(planned_start_date="2021-07-09 08:40:33"),
            ),
            (
                dict(planned_end_date="2021-07-09T08:40:33"),
                dict(planned_end_date="2021-07-09T08:40:33"),
            ),
            (
                dict(planned_end_date="2021-07-09T08:40:33"),
                dict(planned_end_date="2021-07-09 08:40:33"),
            ),
            (
                dict(planned_end_date="2021-07-09 08:40:33"),
                dict(planned_end_date="2021-07-09T08:40:33"),
            ),
            (
                dict(planned_end_date="2021-07-09 08:40:33"),
                dict(planned_end_date="2021-07-09 08:40:33"),
            ),
        ],
    )
    def test_same_point_in_time(self, record, params):
        assert change_request_task.is_superset_with_date(record, params) is True

    @pytest.mark.parametrize(
        "record,params",
        [
            (
                dict(planned_start_date="2021-07-09 08:40:33"),
                dict(planned_start_date="2021-07-09 08:40:34"),
            ),
            (
                dict(planned_end_date="2021-07-09 08:40:33"),
                dict(planned_end_date="2021-07-09 08:40:34"),
            ),
        ],
    )
    def test_different_point_in_time(self, record, params):
        assert change_request_task.is_superset_with_date(record, params) is False

    @pytest.mark.parametrize(
        "record,params",
        [
            (
                dict(planned_start_date=None),
                dict(planned_start_date=None),
            ),
            (
                dict(planned_end_date=""),
                dict(planned_end_date=""),
            ),
            (
                dict(),
                dict(planned_start_date=None),
            ),
            (
                dict(),
                dict(planned_start_date=""),
            ),
        ],
    )
    def test_empty_superset_dates(self, record, params):
        assert change_request_task.is_superset_with_date(record, params) is True

    @pytest.mark.parametrize(
        "record,params",
        [
            (
                dict(planned_start_date=""),
                dict(planned_start_date="2021-07-09 08:40:34"),
            ),
            (
                dict(planned_start_date="2021-07-09 08:40:33"),
                dict(planned_start_date=""),
            ),
            (
                dict(planned_start_date=None),
                dict(planned_start_date="2021-07-09 08:40:34"),
            ),
            (
                dict(planned_start_date="2021-07-09 08:40:33"),
                dict(planned_start_date=None),
            ),
            (
                dict(),
                dict(planned_start_date="2021-07-09 08:40:33"),
            ),
        ],
    )
    def test_empty_not_superset_dates(self, record, params):
        assert change_request_task.is_superset_with_date(record, params) is False
