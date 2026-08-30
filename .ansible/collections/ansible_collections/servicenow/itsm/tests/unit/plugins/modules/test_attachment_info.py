# -*- coding: utf-8 -*-
# # Copyright: (c) 2022, XLAB Steampunk <steampunk@xlab.si>
#
# GNU General Public License v3.0+ (see COPYING or https://www.gnu.org/licenses/gpl-3.0.txt)

from __future__ import absolute_import, division, print_function

__metaclass__ = type

import sys

import pytest
from ansible.module_utils._text import to_bytes
from ansible.module_utils.json_utils import json
from ansible_collections.servicenow.itsm.plugins.module_utils import errors
from ansible_collections.servicenow.itsm.plugins.module_utils.client import Response
from ansible_collections.servicenow.itsm.plugins.modules import attachment_info

pytestmark = pytest.mark.skipif(
    sys.version_info < (2, 7), reason="requires python2.7 or higher"
)


class TestMain:
    # minimal_set_of_params == test_all_params
    def test_all_params(self, run_main):
        params = dict(
            instance=dict(
                host="https://my.host.name", username="user", password="pass"
            ),
            sys_id="01a9ec0d3790200044e0bfc8bcbe5dc3",
            dest="tmp",
        )
        success, result = run_main(attachment_info, params)

        assert success is True

    def test_fail(self, run_main):
        success, result = run_main(attachment_info)

        assert success is False
        assert "missing required arguments" in result["msg"]

    def test_missing_dest(self, run_main):
        params = dict(
            instance=dict(
                host="https://my.host.name", username="user", password="pass"
            ),
            sys_id="01a9ec0d3790200044e0bfc8bcbe5dc3",
        )
        success, result = run_main(attachment_info, params)

        assert success is False
        assert "missing required arguments: dest" in result["msg"]

    def test_missing_sys_id(self, run_main):
        params = dict(
            instance=dict(
                host="https://my.host.name", username="user", password="pass"
            ),
            dest="tmp",
        )
        success, result = run_main(attachment_info, params)

        assert success is False
        assert "missing required arguments: sys_id" in result["msg"]


class TestRun:
    def test_run(self, create_module, attachment_client, mocker):
        module = create_module(
            params=dict(
                instance=dict(
                    host="https://my.host.name", username="user", password="pass"
                ),
                sys_id="01a9ec0d3790200044e0bfc8bcbe5dc3",
                dest="tmp",
            )
        )
        attachment_client.get_attachment.return_value = Response(
            200,
            to_bytes("binary_data"),
            {"x-attachment-metadata": '{  "size_bytes" : "1000"}'},
        )
        mocker.patch(
            "ansible_collections.servicenow.itsm.plugins.modules.attachment_info.time.time"
        ).return_value = 0

        records = attachment_info.run(module, attachment_client)

        assert records == {
            "elapsed": 0.0,
            "size": 1000,
            "status_code": 200,
            "msg": "OK",
        }
        attachment_client.get_attachment.assert_called_once_with(
            "01a9ec0d3790200044e0bfc8bcbe5dc3"
        )
        attachment_client.save_attachment.assert_called_once_with(
            to_bytes("binary_data"), "tmp"
        )

    def test_run_bad_response_keys(self, create_module, attachment_client, mocker):
        module = create_module(
            params=dict(
                instance=dict(
                    host="https://my.host.name", username="user", password="pass"
                ),
                sys_id="01a9ec0d3790200044e0bfc8bcbe5dc3",
                dest="tmp",
            )
        )
        attachment_client.get_attachment.return_value = Response(
            200,
            to_bytes("binary_data"),
            {"bad_key": '{"bad_key": "1000"}'},
        )
        mocker.patch(
            "ansible_collections.servicenow.itsm.plugins.modules.attachment_info.time.time"
        ).return_value = 0

        mocker.patch(
            "ansible_collections.servicenow.itsm.plugins.modules.attachment_info.os.path.getsize"
        ).return_value = 2000

        records = attachment_info.run(module, attachment_client)

        assert records == {
            "elapsed": 0.0,
            "size": 2000,
            "status_code": 200,
            "msg": "OK",
        }

    def test_run_404(self, create_module, attachment_client):
        module = create_module(
            params=dict(
                instance=dict(
                    host="https://my.host.name", username="user", password="pass"
                ),
                sys_id="01a9ec0d3790200044e0bfc8bcbe5dc3",
                dest="tmp",
            )
        )
        msg = dict(
            error=dict(message="No Record found", detail="Record does not exist"),
            status="failure",
        )
        attachment_client.get_attachment.return_value = Response(
            404,
            json.dumps(msg),
            dict(headers="headers"),
        )

        with pytest.raises(errors.ServiceNowError) as exc:
            attachment_info.run(module, attachment_client)

        assert "Status code: 404, Details: Record does not exist" in str(exc.value)

    def test_run_404_bad_response_keys(self, create_module, attachment_client, mocker):
        module = create_module(
            params=dict(
                instance=dict(
                    host="https://my.host.name", username="user", password="pass"
                ),
                sys_id="01a9ec0d3790200044e0bfc8bcbe5dc3",
                dest="tmp",
            )
        )
        msg = dict(
            bad_key=dict(message="No record found", bad_key="Record does not exist"),
            status="failure",
        )
        attachment_client.get_attachment.return_value = Response(
            404,
            json.dumps(msg),
            dict(headers="headers"),
        )

        with pytest.raises(errors.ServiceNowError) as exc:
            attachment_info.run(module, attachment_client)

        assert "Status code: 404, Details: Not found" in str(exc.value)
