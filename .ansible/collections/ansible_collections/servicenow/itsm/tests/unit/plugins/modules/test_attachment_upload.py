# -*- coding: utf-8 -*-
# # Copyright: (c) 2022, XLAB Steampunk <steampunk@xlab.si>
#
# GNU General Public License v3.0+ (see COPYING or https://www.gnu.org/licenses/gpl-3.0.txt)

from __future__ import absolute_import, division, print_function

__metaclass__ = type

import sys

import pytest
from ansible_collections.servicenow.itsm.plugins.modules import attachment_upload

pytestmark = pytest.mark.skipif(
    sys.version_info < (2, 7), reason="requires python2.7 or higher"
)


@pytest.fixture
def list_of_records():
    return [
        {
            "average_image_color": "",
            "chunk_size_bytes": "700000",
            "compressed": "true",
            "content_type": "text/plain",
            "download_link": "https://dev139037.service-now.com/api/now/attachment/f2d5cb9647222110afc6fa37536d4361/file",
            "file_name": "attachment_name1.txt",
            "hash": "f52a678046a6f06e5fca54b4c535b210f29cbaf1134f2b75197cf47078621902",
            "image_height": "",
            "image_width": "",
            "size_bytes": "210",
            "size_compressed": "207",
            "state": "pending",
            "sys_created_by": "admin",
            "sys_created_on": "2023-05-04 08:53:07",
            "sys_id": "f2d5cb9647222110afc6fa37536d4361",
            "sys_mod_count": "0",
            "sys_tags": "",
            "sys_updated_by": "admin",
            "sys_updated_on": "2023-05-04 08:53:07",
            "table_name": "incident",
            "table_sys_id": "01a9ec0d3790200044e0bfc8bcbe5dc3",
        },
        {
            "average_image_color": "",
            "chunk_size_bytes": "700000",
            "compressed": "true",
            "content_type": "text/plain",
            "download_link": "https://dev139037.service-now.com/api/now/attachment/f2d5cb9647222110afc6fa37536d4361/file",
            "file_name": "attachment_name2.txt",
            "hash": "6f2b0dec698566114435a23f15dcac848a40e1fd3e0eda4afe24a663dda23f2e",
            "image_height": "",
            "image_width": "",
            "size_bytes": "210",
            "size_compressed": "207",
            "state": "pending",
            "sys_created_by": "admin",
            "sys_created_on": "2023-05-04 08:53:07",
            "sys_id": "f2d5cb9647222110afc6fa37536d4361",
            "sys_mod_count": "0",
            "sys_tags": "",
            "sys_updated_by": "admin",
            "sys_updated_on": "2023-05-04 08:53:07",
            "table_name": "incident",
            "table_sys_id": "01a9ec0d3790200044e0bfc8bcbe5dc3",
        },
        {
            "average_image_color": "",
            "chunk_size_bytes": "700000",
            "compressed": "true",
            "content_type": "text/plain",
            "download_link": "https://dev139037.service-now.com/api/now/attachment/f2d5cb9647222110afc6fa37536d4361/file",
            "file_name": "attachment_name3.txt",
            "hash": "7f2b0dec698566114435a23f15dcac848a40e1fd3e0eda4afe24a663dda23f2e",
            "image_height": "",
            "image_width": "",
            "size_bytes": "210",
            "size_compressed": "207",
            "state": "pending",
            "sys_created_by": "admin",
            "sys_created_on": "2023-05-04 08:53:07",
            "sys_id": "f2d5cb9647222110afc6fa37536d4361",
            "sys_mod_count": "0",
            "sys_tags": "",
            "sys_updated_by": "admin",
            "sys_updated_on": "2023-05-04 08:53:07",
            "table_name": "incident",
            "table_sys_id": "01a9ec0d3790200044e0bfc8bcbe5dc3",
        },
    ]


class TestMain:
    def test_all_params(self, run_main):
        params = dict(
            instance=dict(
                host="https://my.host.name", username="user", password="pass"
            ),
            table_name="incident",
            table_sys_id="01a9ec0d3790200044e0bfc8bcbe5dc3",
            attachments=[
                {
                    "name": "attachment_name1.txt",
                    "path": "path1",
                    "type": "text/plain",
                },
            ],
        )
        success, result = run_main(attachment_upload, params)

        assert success is True

    def test_required(self, run_main):
        success, result = run_main(attachment_upload)

        assert success is False
        assert "missing required arguments: table_name, table_sys_id" in result["msg"]


class TestRun:
    def test_run_unchanged(
        self, create_module, attachment_client, list_of_records, mocker
    ):
        module = create_module(
            params=dict(
                instance=dict(
                    host="https://my.host.name", username="user", password="pass"
                ),
                table_name="incident",
                table_sys_id="01a9ec0d3790200044e0bfc8bcbe5dc3",
                attachments=[
                    {
                        "name": "attachment_name1.txt",
                        "path": "path1",
                        "type": "text/plain",
                    },
                ],
            )
        )
        mocker.patch(
            "ansible_collections.servicenow.itsm.plugins.modules.attachment_upload.attachment.transform_metadata_list"
        ).return_value = {
            "attachment_name1.txt": {
                "path": "path1",
                "type": "text/plain",
                "hash": "f52a678046a6f06e5fca54b4c535b210f29cbaf1134f2b75197cf47078621902",
            }
        }
        attachment_client.list_records.return_value = list_of_records

        changed, records, diff = attachment_upload.run(module, attachment_client)

        attachment_client.list_records.assert_called_with(
            dict(
                table_name=module.params["table_name"],
                table_sys_id=module.params["table_sys_id"],
            )
        )
        assert changed is False
        assert records == [list_of_records[0]]
        assert diff == dict(before=[list_of_records[0]], after=[list_of_records[0]])

    def test_run_changed(
        self, create_module, attachment_client, list_of_records, mocker
    ):
        module = create_module(
            params=dict(
                instance=dict(
                    host="https://my.host.name", username="user", password="pass"
                ),
                table_name="incident",
                table_sys_id="01a9ec0d3790200044e0bfc8bcbe5dc3",
                attachments=[
                    {
                        "name": "attachment_name1.txt",  # no changes
                        "path": "path1",
                        "type": "text/plain",
                    },
                    {
                        "name": "attachment_name2.txt",  # changes
                        "path": "path2",
                        "type": "text/markdown",
                    },
                ],
            )
        )

        metadata_dict = {
            "attachment_name1.txt": {
                "path": "path1",
                "type": "text/plain",
                "hash": "f52a678046a6f06e5fca54b4c535b210f29cbaf1134f2b75197cf47078621902",
            },
            "attachment_name2.txt": {
                "path": "path2",
                "type": "text/markdown",
                "hash": "new_hash",
            },
        }

        mocker.patch(
            "ansible_collections.servicenow.itsm.plugins.modules.attachment_upload.attachment.transform_metadata_list"
        ).return_value = metadata_dict

        attachment_client.list_records.return_value = list_of_records

        updated_attachments = [
            {
                "average_image_color": "",
                "chunk_size_bytes": "700000",
                "compressed": "true",
                "content_type": "text/plain",
                "download_link": "https://dev139037.service-now.com/api/now/attachment/f2d5cb9647222110afc6fa37536d4361/file",
                "file_name": "attachment_name2.txt",
                "hash": metadata_dict.get("attachment_name2.txt").get("hash"),
                "image_height": "",
                "image_width": "",
                "size_bytes": "210",
                "size_compressed": "207",
                "state": "pending",
                "sys_created_by": "admin",
                "sys_created_on": "2023-05-04 08:53:07",
                "sys_id": "f2d5cb9647222110afc6fa37536d4361",
                "sys_mod_count": "0",
                "sys_tags": "",
                "sys_updated_by": "admin",
                "sys_updated_on": "2023-05-04 08:53:07",
                "table_name": "incident",
                "table_sys_id": "01a9ec0d3790200044e0bfc8bcbe5dc3",
            },
        ]

        attachment_client.update_records.return_value = updated_attachments

        changed, records, diff = attachment_upload.run(module, attachment_client)

        attachment_client.update_records.assert_called_with(
            module.params["table_name"],
            module.params["table_sys_id"],
            {"attachment_name2.txt": metadata_dict["attachment_name2.txt"]},
            [list_of_records[1]],
            module.check_mode,
        )
        assert changed is True
        assert records == updated_attachments + [list_of_records[0]]
        assert diff == dict(
            before=[list_of_records[1]] + [list_of_records[0]],
            after=updated_attachments + [list_of_records[0]],
        )
