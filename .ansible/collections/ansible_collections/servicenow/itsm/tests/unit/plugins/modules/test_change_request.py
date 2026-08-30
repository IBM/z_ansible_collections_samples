# -*- coding: utf-8 -*-
# # Copyright: (c) 2021, XLAB Steampunk <steampunk@xlab.si>
#
# GNU General Public License v3.0+ (see COPYING or https://www.gnu.org/licenses/gpl-3.0.txt)

from __future__ import absolute_import, division, print_function

__metaclass__ = type

import sys

import pytest
from ansible_collections.servicenow.itsm.plugins.module_utils import errors
from ansible_collections.servicenow.itsm.plugins.modules import change_request

pytestmark = pytest.mark.skipif(
    sys.version_info < (2, 7), reason="requires python2.7 or higher"
)


class TestEnsureAbsent:
    def test_delete_change_request(
        self, create_module, table_client, attachment_client
    ):
        module = create_module(
            params=dict(
                instance=dict(host="my.host.name", username="user", password="pass"),
                state="absent",
                number="CHG0000001",
                sys_id=None,
            )
        )
        table_client.get_record.return_value = dict(
            state="3", number="CHG0000001", sys_id="1234"
        )

        result = change_request.ensure_absent(module, table_client, attachment_client)

        table_client.delete_record.assert_called_once()
        assert result == (
            True,
            None,
            dict(
                before=dict(state="closed", number="CHG0000001", sys_id="1234"),
                after=None,
            ),
        )

    def test_delete_change_request_not_present(
        self, create_module, table_client, attachment_client
    ):
        module = create_module(
            params=dict(
                instance=dict(host="my.host.name", username="user", password="pass"),
                state="absent",
                number=None,
                sys_id="1234",
            ),
        )
        table_client.get_record.return_value = None

        result = change_request.ensure_absent(module, table_client, attachment_client)

        table_client.delete_record.assert_not_called()
        assert result == (False, None, dict(before=None, after=None))


class TestValidateParams:
    VALID_PARAMS = dict(
        state="closed",
        close_code="successful",
        close_notes="Solved",
        assignment_group=None,
        assignment_group_id=None,
    )

    @pytest.mark.parametrize("missing_field", ["close_code", "close_notes"])
    def test_validate_params_missing_field(self, missing_field):
        params = self.VALID_PARAMS.copy()
        params[missing_field] = None

        with pytest.raises(errors.ServiceNowError, match=missing_field):
            change_request.validate_params(params)

    def test_validate_params(self):
        change_request.validate_params(self.VALID_PARAMS)


class TestEnsurePresent:
    def test_ensure_present_create_new(
        self, create_module, table_client, attachment_client
    ):
        module = create_module(
            params=dict(
                instance=dict(host="my.host.name", username="user", password="pass"),
                state="new",
                type="normal",
                requested_by=None,
                assignment_group=None,
                assignment_group_id=None,
                category=None,
                priority=None,
                risk="low",
                impact="low",
                number=None,
                sys_id=None,
                short_description="Test change request",
                description=None,
                close_code=None,
                close_notes=None,
                on_hold=None,
                hold_reason=None,
                template=None,
                other=None,
                attachments=None,
            ),
        )
        table_client.create_record.return_value = dict(
            state="-5",
            number="CHG0000001",
            short_description="Test change request",
            risk="4",
            impact="3",
            sys_id="1234",
        )
        attachment_client.upload_records.return_value = []
        module.sha256.return_value = ""

        result = change_request.ensure_present(module, table_client, attachment_client)

        table_client.create_record.assert_called_once()
        assert result == (
            True,
            dict(
                state="new",
                number="CHG0000001",
                short_description="Test change request",
                risk="low",
                impact="low",
                sys_id="1234",
                attachments=[],
            ),
            dict(
                before=None,
                after=dict(
                    state="new",
                    number="CHG0000001",
                    short_description="Test change request",
                    risk="low",
                    impact="low",
                    sys_id="1234",
                    attachments=[],
                ),
            ),
        )

    def test_ensure_present_nothing_to_do(
        self, create_module, table_client, attachment_client
    ):
        module = create_module(
            params=dict(
                instance=dict(host="my.host.name", username="user", password="pass"),
                state="new",
                type="normal",
                number="CHG0000001",
                requested_by=None,
                assignment_group=None,
                assignment_group_id=None,
                category=None,
                priority=None,
                risk=None,
                impact=None,
                sys_id=None,
                short_description="Test change request",
                description=None,
                close_code=None,
                close_notes=None,
                on_hold=None,
                hold_reason=None,
                template=None,
                other=None,
                attachments=None,
            ),
        )
        table_client.get_record.return_value = dict(
            state="-5",
            type="normal",
            number="CHG0000001",
            chg_model="normal",
            short_description="Test change request",
            sys_id="1234",
        )
        attachment_client.update_records.return_value = []
        attachment_client.list_records.return_value = []
        module.sha256.return_value = ""

        result = change_request.ensure_present(module, table_client, attachment_client)

        table_client.get_record.assert_called_once()
        assert result == (
            False,
            dict(
                state="new",
                type="normal",
                number="CHG0000001",
                chg_model="normal",
                short_description="Test change request",
                sys_id="1234",
                attachments=[],
            ),
            dict(
                before=dict(
                    state="new",
                    type="normal",
                    number="CHG0000001",
                    chg_model="normal",
                    short_description="Test change request",
                    sys_id="1234",
                    attachments=[],
                ),
                after=dict(
                    state="new",
                    type="normal",
                    number="CHG0000001",
                    chg_model="normal",
                    short_description="Test change request",
                    sys_id="1234",
                    attachments=[],
                ),
            ),
        )

    def test_ensure_present_update(
        self, create_module, table_client, attachment_client
    ):
        module = create_module(
            params=dict(
                instance=dict(host="my.host.name", username="user", password="pass"),
                state="assess",
                type="normal",
                number="CHG0000001",
                requested_by=None,
                assignment_group=None,
                assignment_group_id=None,
                category=None,
                priority=None,
                risk=None,
                impact=None,
                sys_id=None,
                short_description="Test change request",
                description=None,
                close_code=None,
                close_notes=None,
                on_hold=None,
                hold_reason=None,
                template=None,
                other=None,
                attachments=None,
            ),
        )

        table_client.get_record.return_value = dict(
            state="-5",
            number="CHG0000001",
            chg_model="normal",
            short_description="Test change request",
            sys_id="1234",
        )
        table_client.update_record.return_value = dict(
            state="-4",
            number="CHG0000001",
            chg_model="normal",
            short_description="Test change request",
            sys_id="1234",
        )
        attachment_client.update_records.return_value = []
        attachment_client.list_records.return_value = []
        module.sha256.return_value = ""

        result = change_request.ensure_present(module, table_client, attachment_client)

        table_client.update_record.assert_called_once()
        assert result == (
            True,
            dict(
                state="assess",
                number="CHG0000001",
                chg_model="normal",
                short_description="Test change request",
                sys_id="1234",
                attachments=[],
            ),
            dict(
                before=dict(
                    state="new",
                    number="CHG0000001",
                    chg_model="normal",
                    short_description="Test change request",
                    sys_id="1234",
                    attachments=[],
                ),
                after=dict(
                    state="assess",
                    number="CHG0000001",
                    chg_model="normal",
                    short_description="Test change request",
                    sys_id="1234",
                    attachments=[],
                ),
            ),
        )


class TestBuildPayload:
    def test_build_payload(self, create_module, table_client):
        module = create_module(
            params=dict(
                instance=dict(host="my.host.name", username="user", password="pass"),
                state="new",
                type="normal",
                template="Some template",
                requested_by="admin",
                assignment_group="Network",
                assignment_group_id=None,
                category=None,
                priority=None,
                risk="low",
                impact="low",
                number=None,
                sys_id=None,
                short_description="Test change request",
                description=None,
                close_code=None,
                close_notes=None,
                on_hold=None,
                hold_reason="Some reason",
                other=None,
            ),
        )
        table_client.get_record.side_effect = [
            {"sys_id": "681ccaf9c0a8016400b98a06818d57c7"},
            {"sys_id": "d625dccec0a8016700a222a0f7900d06"},
            {"sys_id": "deb8544047810200e90d87e8dee490af"},
        ]

        result = change_request.build_payload(module, table_client)

        assert result["state"] == "new"
        assert result["type"] == "normal"
        assert (
            result["std_change_producer_version"] == "deb8544047810200e90d87e8dee490af"
        )
        assert result["chg_model"] == "normal"
        assert result["requested_by"] == "681ccaf9c0a8016400b98a06818d57c7"
        assert result["risk"] == "low"
        assert result["impact"] == "low"
        assert result["short_description"] == "Test change request"
        assert result["on_hold_reason"] == "Some reason"
        assert result["assignment_group"] == "d625dccec0a8016700a222a0f7900d06"
        assert "assigned_to" not in result

    def test_build_payload_with_other_option(self, create_module, table_client):
        module = create_module(
            params=dict(
                instance=dict(host="my.host.name", username="user", password="pass"),
                state="new",
                type=None,
                template=None,
                requested_by=None,
                assignment_group=None,
                assignment_group_id=None,
                category=None,
                priority=None,
                risk="low",
                impact="low",
                number=None,
                sys_id=None,
                short_description="Test change request",
                description=None,
                close_code=None,
                close_notes=None,
                on_hold=None,
                hold_reason="Some reason",
                attachments=None,
                other=dict(assigned_to="some_user"),
            ),
        )

        result = change_request.build_payload(module, table_client)

        assert result["state"] == "new"
        assert "type" not in result
        assert "chg_model" not in result
        assert "requested_by" not in result
        assert result["risk"] == "low"
        assert result["impact"] == "low"
        assert result["short_description"] == "Test change request"
        assert result["on_hold_reason"] == "Some reason"
        assert "assignment_group" not in result
        assert result["assigned_to"] == "some_user"
        assert "requested_by" not in result
