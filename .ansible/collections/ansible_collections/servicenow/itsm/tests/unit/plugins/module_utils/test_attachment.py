# -*- coding: utf-8 -*-
# Copyright: (c) 2021, XLAB Steampunk <steampunk@xlab.si>
#
# GNU General Public License v3.0+ (see COPYING or https://www.gnu.org/licenses/gpl-3.0.txt)

from __future__ import absolute_import, division, print_function

__metaclass__ = type

import sys

import pytest
from ansible.module_utils._text import to_bytes, to_text
from ansible_collections.servicenow.itsm.plugins.module_utils import attachment, errors
from ansible_collections.servicenow.itsm.plugins.module_utils.client import Response

pytestmark = pytest.mark.skipif(
    sys.version_info < (2, 7), reason="requires python2.7 or higher"
)


class TestAttachmentGetFileName:
    def test_name_specified(self):
        assert (
            attachment.get_file_name(
                {
                    "path": "some/path/file_name.txt",
                    "name": "attachment_name.txt",
                }
            )
            == "attachment_name.txt"
        )

    def test_name_none(self):
        assert (
            attachment.get_file_name(
                {
                    "path": "some/path/file_name.txt",
                    "name": None,
                }
            )
            == "file_name.txt"
        )

    def test_name_omitted(self):
        assert (
            attachment.get_file_name(
                {
                    "path": "some/path/file_name.txt",
                }
            )
            == "file_name.txt"
        )


class TestAttachmentGetFileType:
    def test_type_specified(self):
        assert (
            attachment.get_file_type(
                {
                    "path": "some/path/file_name.txt",
                    "type": "text/markdown",
                }
            )
            == "text/markdown"
        )

    def test_type_none(self):
        assert (
            attachment.get_file_type(
                {
                    "path": "some/path/file_name.txt",
                    "type": None,
                }
            )
            == "text/plain"
        )

    def test_type_omitted(self):
        assert (
            attachment.get_file_type(
                {
                    "path": "some/path/file_name.txt",
                }
            )
            == "text/plain"
        )


class TestAttachmentTransformMetadataList:
    def test_normal(self, tmp_path):
        path1 = tmp_path / "name1.txt"
        path1.write_text(u"file_contents")  # fmt: skip
        path2 = tmp_path / "name2.txt"
        path2.write_text(u"another_file_contents")  # fmt: skip

        assert attachment.transform_metadata_list(
            [
                {
                    "path": str(path1),
                    "type": "text/plain",
                },
                {
                    "path": str(path2),
                    "name": "attachment_name.txt",
                    "type": "text/markdown",
                },
            ],
            lambda x: "some_hash",
        ) == {
            "name1.txt": {
                "path": str(path1),
                "type": "text/plain",
                "hash": "some_hash",
            },
            "attachment_name.txt": {
                "path": str(path2),
                "type": "text/markdown",
                "hash": "some_hash",
            },
        }

    def test_same_file_different_name(self, tmp_path):
        path = tmp_path / "name.txt"
        path.write_text(u"file_contents")  # fmt: skip

        assert attachment.transform_metadata_list(
            [
                {
                    "path": str(path),
                    "type": "text/plain",
                },
                {
                    "path": str(path),
                    "name": "attachment_name.txt",
                    "type": "text/markdown",
                },
            ],
            lambda x: "some_hash",
        ) == {
            "name.txt": {
                "path": str(path),
                "type": "text/plain",
                "hash": "some_hash",
            },
            "attachment_name.txt": {
                "path": str(path),
                "type": "text/markdown",
                "hash": "some_hash",
            },
        }

    def test_duplicate(self, tmp_path):
        path1 = tmp_path / "name1.txt"
        path1.write_text(u"file_contents")  # fmt: skip
        path2 = tmp_path / "name2.txt"
        path2.write_text(u"another_file_contents")  # fmt: skip

        with pytest.raises(
            errors.ServiceNowError,
            match="Found the following duplicates: (.*, .*)",
        ):
            attachment.transform_metadata_list(
                [
                    {
                        "path": str(path1),
                        "name": "attachment_name.txt",
                        "type": "text/plain",
                    },
                    {
                        "path": str(path2),
                        "name": "attachment_name.txt",
                        "type": "text/markdown",
                    },
                ],
                lambda x: "some_hash",
            )


class TestAttachmentAreChanged:
    def test_unchanged(self):
        assert attachment.are_changed(
            [
                {"hash": "hash", "file_name": "attachment_name.txt"},
                {"hash": "hash", "file_name": "another_file_name.txt"},
            ],
            {
                "attachment_name.txt": {
                    "path": "some/path/file_name.txt",
                    "type": "text/markdown",
                    "hash": "hash",
                },
                "another_file_name.txt": {
                    "path": "some/path/another_file_name.txt",
                    "type": "text/plain",
                    "hash": "hash",
                },
            },
        ) == [False, False]

    def test_changed(self):
        assert attachment.are_changed(
            [
                {"hash": "oldhash", "file_name": "attachment_name.txt"},
                {"hash": "oldhash", "file_name": "another_file_name.txt"},
            ],
            {
                "attachment_name.txt": {
                    "path": "some/path/file_name.txt",
                    "type": "text/markdown",
                    "hash": "hash",
                },
                "another_file_name.txt": {
                    "path": "some/path/another_file_name.txt",
                    "type": "text/plain",
                    "hash": "hash",
                },
            },
        ) == [True, True]


class TestAttachmentListRecords:
    def test_empty_response_meta(self, client):
        client.get.return_value = Response(
            200, '{"result": []}', {"X-Total-Count": "0"}
        )
        a = attachment.AttachmentClient(client)

        records = a.list_records()

        assert [] == records
        client.get.assert_called_once_with(
            "api/now/attachment",
            query=dict(
                sysparm_limit=10000,
                sysparm_offset=0,
            ),
        )

    def test_non_empty_response_meta(self, client):
        client.get.return_value = Response(
            200, '{"result": [{"a": 3, "b": "sys_id"}]}', {"X-Total-Count": "1"}
        )
        a = attachment.AttachmentClient(client)

        records = a.list_records()

        assert [dict(a=3, b="sys_id")] == records

    def test_query_passing_meta(self, client):
        client.get.return_value = Response(
            200, '{"result": []}', {"X-Total-Count": "0"}
        )
        a = attachment.AttachmentClient(client)

        a.list_records(dict(a="b"))

        client.get.assert_called_once_with(
            "api/now/attachment",
            query=dict(
                a="b",
                sysparm_limit=10000,
                sysparm_offset=0,
            ),
        )

    def test_pagination_meta(self, client):
        client.get.side_effect = (
            Response(
                200, '{"result": [{"a": 3, "b": "sys_id"}]}', {"X-Total-Count": "2"}
            ),
            Response(
                200, '{"result": [{"a": 2, "b": "sys_ie"}]}', {"X-Total-Count": "2"}
            ),
        )
        a = attachment.AttachmentClient(client, batch_size=1)

        records = a.list_records()

        assert [dict(a=3, b="sys_id"), dict(a=2, b="sys_ie")] == records
        assert 2 == len(client.get.mock_calls)
        client.get.assert_any_call(
            "api/now/attachment",
            query=dict(sysparm_limit=1, sysparm_offset=0),
        )
        client.get.assert_any_call(
            "api/now/attachment",
            query=dict(sysparm_limit=1, sysparm_offset=1),
        )


class TestAttachmentCreateRecord:
    def test_normal_mode(self, client):
        client.request.return_value = Response(
            201, '{"result": {"a": 3, "b": "sys_id"}}'
        )
        a = attachment.AttachmentClient(client)

        record = a.create_record(
            dict(some="property"),
            "file_content",
            "text/plain",
            False,
        )

        assert dict(a=3, b="sys_id") == record
        client.request.assert_called_with(
            "POST",
            "api/now/attachment/file",
            query={"some": "property"},
            headers={"Accept": "application/json", "Content-type": "text/plain"},
            bytes="file_content",
        )

    def test_check_mode(self, client):
        client.request.return_value = Response(
            201, '{"result": {"a": 3, "b": "sys_id"}}'
        )
        a = attachment.AttachmentClient(client)

        record = a.create_record(
            dict(some="property"),
            "file_content",
            "text/plain",
            True,
        )

        assert dict(some="property") == record
        client.request.assert_not_called()


class TestAttachmentUploadRecord:
    def test_normal_mode(self, client, tmp_path):
        client.request.return_value = Response(
            201, '{"result": {"a": 3, "b": "sys_id"}}'
        )
        a = attachment.AttachmentClient(client)

        path = tmp_path / "name.txt"
        path.write_text(u"file_content")  # fmt: skip

        record = a.upload_record(
            "table",
            "1234",
            {
                "path": str(path),
                "name": "attachment_name.txt",
                "type": "text/markdown",
                "hash": "hash",
            },
            check_mode=False,
        )

        assert dict(a=3, b="sys_id") == record
        client.request.assert_called_with(
            "POST",
            "api/now/attachment/file",
            query={
                "table_name": "table",
                "table_sys_id": "1234",
                "file_name": "attachment_name.txt",
                "content_type": "text/markdown",
                "hash": "hash",
            },
            headers={"Accept": "application/json", "Content-type": "text/markdown"},
            bytes=b"file_content",
        )

    def test_check_mode(self, client, tmp_path):
        client.request.return_value = Response(
            201, '{"result": {"a": 3, "b": "sys_id"}}'
        )
        a = attachment.AttachmentClient(client)

        path = tmp_path / "name.txt"
        path.write_text(u"file_contents")  # fmt: skip

        record = a.upload_record(
            "table",
            "1234",
            {
                "path": str(path),
                "name": "attachment_name.txt",
                "type": "text/markdown",
                "hash": "hash",
            },
            check_mode=True,
        )

        assert {
            "table_name": "table",
            "table_sys_id": "1234",
            "file_name": "attachment_name.txt",
            "content_type": "text/markdown",
            "hash": "hash",
        } == record
        client.request.assert_not_called()


class TestAttachmentUploadRecords:
    def test_normal_mode(self, client, tmp_path):
        client.request.side_effect = [
            Response(201, '{"result": {"a": 3, "b": "sys_id"}}'),
            Response(201, '{"result": {"a": 4, "b": "sys_idn"}}'),
        ]
        a = attachment.AttachmentClient(client)

        path1 = tmp_path / "name1.txt"
        path1.write_text(u"file_content1")  # fmt: skip
        path2 = tmp_path / "name2.txt"
        path2.write_text(u"file_content2")  # fmt: skip

        record = a.upload_records(
            "table",
            "1234",
            {
                "attachment_name.txt": {
                    "path": str(path1),
                    "type": "text/markdown",
                    "hash": "hash",
                },
                "another_file_name.txt": {
                    "path": str(path2),
                    "type": "text/plain",
                    "hash": "hash",
                },
            },
            check_mode=False,
        )

        assert [dict(a=3, b="sys_id"), dict(a=4, b="sys_idn")] == record
        assert 2 == client.request.call_count
        client.request.assert_any_call(
            "POST",
            "api/now/attachment/file",
            query={
                "table_name": "table",
                "table_sys_id": "1234",
                "file_name": "attachment_name.txt",
                "content_type": "text/markdown",
                "hash": "hash",
            },
            headers={"Accept": "application/json", "Content-type": "text/markdown"},
            bytes=b"file_content1",
        )
        client.request.assert_any_call(
            "POST",
            "api/now/attachment/file",
            query={
                "table_name": "table",
                "table_sys_id": "1234",
                "file_name": "another_file_name.txt",
                "content_type": "text/plain",
                "hash": "hash",
            },
            headers={"Accept": "application/json", "Content-type": "text/plain"},
            bytes=b"file_content2",
        )

    def test_check_mode(self, client, tmp_path):
        client.request.return_value = Response(
            201, '{"result": {"a": 1, "sys_id": "1"}}'
        )
        a = attachment.AttachmentClient(client)

        path1 = tmp_path / "name1.txt"
        path1.write_text(u"file_content1")  # fmt: skip
        path2 = tmp_path / "name2.txt"
        path2.write_text(u"file_content2")  # fmt: skip

        record = a.upload_records(
            "table",
            "1234",
            {
                "attachment_name.txt": {
                    "path": str(path1),
                    "type": "text/markdown",
                    "hash": "hash",
                },
                "another_file_name.txt": {
                    "path": str(path2),
                    "type": "text/plain",
                    "hash": "hash",
                },
            },
            check_mode=True,
        )

        assert [
            {
                "table_name": "table",
                "table_sys_id": "1234",
                "file_name": "another_file_name.txt",
                "content_type": "text/plain",
                "hash": "hash",
            },
            {
                "table_name": "table",
                "table_sys_id": "1234",
                "file_name": "attachment_name.txt",
                "content_type": "text/markdown",
                "hash": "hash",
            },
        ] == sorted(record, key=lambda k: k["file_name"])
        client.request.assert_not_called()

    def test_missing_file(self, client):
        a = attachment.AttachmentClient(client)
        with pytest.raises(errors.ServiceNowError, match="Cannot open"):
            a.upload_records(
                "table",
                "1234",
                {
                    "attachment_name.txt": {
                        "path": "some/path/file_name.txt",
                        "type": "text/markdown",
                        "hash": "hash",
                    },
                    "another_file_name.txt": {
                        "path": "some/path/another_file_name.txt",
                        "type": "text/plain",
                        "hash": "hash",
                    },
                },
                True,
            )


class TestAttachmentDeleteRecord:
    def test_normal_mode(self, client):
        client.delete.return_value = Response(204, "")
        a = attachment.AttachmentClient(client)
        a.delete_record(dict(sys_id="1234"), False)

        client.delete.assert_called_with("api/now/attachment/1234")

    def test_normal_mode_missing(self, client):
        client.delete.side_effect = errors.UnexpectedAPIResponse(
            404, "Record not found"
        )
        a = attachment.AttachmentClient(client)

        with pytest.raises(errors.UnexpectedAPIResponse, match="not found"):
            a.delete_record(dict(sys_id="1234"), False)

    def test_check_mode(self, client):
        client.delete.return_value = Response(204, "")
        a = attachment.AttachmentClient(client)
        a.delete_record(dict(sys_id="id"), True)

        client.delete.assert_not_called()


class TestAttachmentDeleteRecords:
    def test_normal_mode(self, client):
        client.get.return_value = Response(
            200,
            '{"result": [{"a": 3, "sys_id": "1234"}, {"a": 4, "sys_id": "4321"}]}',
            {"X-Total-Count": "2"},
        )
        client.delete.return_value = Response(204, "")
        a = attachment.AttachmentClient(client)

        a.delete_attached_records("table", 5555, False)

        assert client.delete.call_count == 2
        client.delete.assert_any_call("api/now/attachment/1234")
        client.delete.assert_any_call("api/now/attachment/4321")

    def test_normal_mode_missing(self, client):
        client.get.return_value = Response(
            200,
            '{"result": [{"a": 3, "sys_id": "1234"}, {"a": 4, "sys_id": "4321"}]}',
            {"X-Total-Count": "2"},
        )
        client.delete.side_effect = errors.UnexpectedAPIResponse(
            404, "Record not found"
        )
        a = attachment.AttachmentClient(client)

        with pytest.raises(errors.UnexpectedAPIResponse, match="not found"):
            a.delete_attached_records("table", 5555, False)

    def test_check_mode(self, client):
        client.get.return_value = Response(
            200,
            '{"result": [{"a": 3, "sys_id": "1234"}, {"a": 4, "sys_id": "4321"}]}',
            {"X-Total-Count": "2"},
        )
        client.delete.return_value = Response(204, "")
        a = attachment.AttachmentClient(client)

        a.delete_attached_records("table", 5555, True)
        client.delete.assert_not_called()


class TestAttachmentUpdateRecords:
    def test_unchanged_normal_mode(self, client, tmp_path):
        a = attachment.AttachmentClient(client)

        path1 = tmp_path / "name1.txt"
        path1.write_text(u"file_content1")  # fmt: skip
        path2 = tmp_path / "name2.txt"
        path2.write_text(u"file_content2")  # fmt: skip

        changes = a.update_records(
            "table",
            "1234",
            {
                "attachment_name.txt": {
                    "path": str(path1),
                    "type": "text/markdown",
                    "hash": "hash",
                },
                "another_file_name.txt": {
                    "path": str(path2),
                    "type": "text/plain",
                    "hash": "hash",
                },
            },
            [
                {"hash": "hash", "sys_id": "1", "file_name": "attachment_name.txt"},
                {"hash": "hash", "sys_id": "2", "file_name": "another_file_name.txt"},
            ],
            False,
        )

        assert [
            {
                "hash": "hash",
                "sys_id": "2",
                "file_name": "another_file_name.txt",
            },
            {
                "hash": "hash",
                "sys_id": "1",
                "file_name": "attachment_name.txt",
            },
        ] == sorted(changes, key=lambda k: k["file_name"])

    def test_changed_normal_mode(self, client, tmp_path):
        client.request.side_effect = [
            Response(201, '{"result": {"a": 1, "sys_id": "a"}}'),
            Response(201, '{"result": {"a": 2, "sys_id": "b"}}'),
        ]
        client.delete.return_value = Response(204, "")
        a = attachment.AttachmentClient(client)

        path1 = tmp_path / "name1.txt"
        path1.write_text(u"file_content1")  # fmt: skip
        path2 = tmp_path / "name2.txt"
        path2.write_text(u"file_content2")  # fmt: skip

        changes = a.update_records(
            "table",
            "1234",
            {
                "attachment_name.txt": {
                    "path": str(path1),
                    "type": "text/markdown",
                    "hash": "hash",
                },
                "another_file_name.txt": {
                    "path": str(path2),
                    "type": "text/plain",
                    "hash": "hash",
                },
            },
            [
                {"hash": "oldhash", "sys_id": "1", "file_name": "attachment_name.txt"},
                {
                    "hash": "oldhash",
                    "sys_id": "2",
                    "file_name": "another_file_name.txt",
                },
            ],
            False,
        )

        assert [
            {
                "a": 1,
                "sys_id": "a",
            },
            {
                "a": 2,
                "sys_id": "b",
            },
        ] == changes

    def test_unchanged_check_mode(self, client, tmp_path):
        a = attachment.AttachmentClient(client)

        path1 = tmp_path / "name1.txt"
        path1.write_text(u"file_content1")  # fmt: skip
        path2 = tmp_path / "name2.txt"
        path2.write_text(u"file_content2")  # fmt: skip

        changes = a.update_records(
            "table",
            "1234",
            {
                "attachment_name.txt": {
                    "path": str(path1),
                    "type": "text/markdown",
                    "hash": "hash",
                },
                "another_file_name.txt": {
                    "path": str(path2),
                    "type": "text/plain",
                    "hash": "hash",
                },
            },
            [
                {"hash": "hash", "sys_id": "1", "file_name": "attachment_name.txt"},
                {"hash": "hash", "sys_id": "2", "file_name": "another_file_name.txt"},
            ],
            True,
        )

        assert [
            {
                "hash": "hash",
                "sys_id": "2",
                "file_name": "another_file_name.txt",
            },
            {
                "hash": "hash",
                "sys_id": "1",
                "file_name": "attachment_name.txt",
            },
        ] == sorted(changes, key=lambda k: k["file_name"])

    def test_changed_check_mode(self, client, tmp_path):
        client.request.side_effect = [
            Response(201, '{"result": {"a": 1, "sys_id": "a"}}'),
            Response(201, '{"result": {"a": 2, "sys_id": "b"}}'),
        ]
        client.delete.return_value = Response(204, "")
        a = attachment.AttachmentClient(client)

        path1 = tmp_path / "name1.txt"
        path1.write_text(u"file_content1")  # fmt: skip
        path2 = tmp_path / "name2.txt"
        path2.write_text(u"file_content2")  # fmt: skip

        record = a.update_records(
            "table",
            "1234",
            {
                "attachment_name.txt": {
                    "path": str(path1),
                    "type": "text/markdown",
                    "hash": "hash",
                },
                "another_file_name.txt": {
                    "path": str(path2),
                    "type": "text/plain",
                    "hash": "hash",
                },
            },
            [
                {"hash": "oldhash", "sys_id": "1", "file_name": "attachment_name.txt"},
                {
                    "hash": "oldhash",
                    "sys_id": "2",
                    "file_name": "another_file_name.txt",
                },
            ],
            True,
        )

        assert [
            {
                "table_name": "table",
                "table_sys_id": "1234",
                "content_type": "text/plain",
                "hash": "hash",
                "file_name": "another_file_name.txt",
            },
            {
                "table_name": "table",
                "table_sys_id": "1234",
                "content_type": "text/markdown",
                "hash": "hash",
                "file_name": "attachment_name.txt",
            },
        ] == sorted(record, key=lambda k: k["file_name"])


class TestAttachmentGetAttachment:
    def test_get_attachment(self, client):
        client.get.return_value = Response(
            200,
            to_bytes("binary_data"),
            {"headers": "headers"},
        )
        a = attachment.AttachmentClient(client)

        response = a.get_attachment("0061f0c510247200964f77ffeec6c4de")

        client.get.assert_called_once_with(
            "api/now/attachment/0061f0c510247200964f77ffeec6c4de/file"
        )
        assert response.status == 200
        assert response.data == to_bytes("binary_data")
        assert response.headers == {"headers": "headers"}


class TestAttachmentSaveAttachment:
    def test_save_attachment(self, client, tmp_path):
        path = tmp_path / "test.txt"
        a = attachment.AttachmentClient(client)

        a.save_attachment(to_bytes("test"), path)
        file = open(str(path), "r")
        b_data = file.read()

        assert to_text(b_data) == "test"

    def test_save_attachment_bad_dest(self, client, tmpdir):
        a = attachment.AttachmentClient(client)
        nonexisting_path_name = str(tmpdir.join("not", "a", "path"))

        with pytest.raises(errors.ServiceNowError) as exc:
            a.save_attachment(to_bytes("test"), nonexisting_path_name)

        assert "[Errno 2] No such file or directory" in str(exc.value)


class TestAreChangedReturnRecords:
    def test_no_records(self):
        records = []

        metadata_dict = {
            "attachment_name1.txt": {
                "path": "path1",
                "type": "text/plain",
                "hash": "f52a678046a6f06e5fca54b4c535b210f29cbaf1134f2b75197cf47078621902",
            },
            "attachment_name2.txt": {
                "path": "path2",
                "type": "text/markdown",
                "hash": "7f2b0dec698566114435a23f15dcac848a40e1fd3e0eda4afe24a663dda23f2e",
            },
        }

        update, changed, unchanged = attachment.are_changed_return_records(
            records, metadata_dict
        )

        assert changed == []
        assert unchanged == []
        assert update == metadata_dict

    def test_record_alreday_exists(self):
        records = [
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
                "table_sys_id": "7cd58f1647222110afc6fa37536d43ed",
            },
        ]

        metadata_dict = {
            "attachment_name1.txt": {
                "path": "path1",
                "type": "text/plain",
                "hash": "f52a678046a6f06e5fca54b4c535b210f29cbaf1134f2b75197cf47078621902",
            },
        }

        update, changed, unchanged = attachment.are_changed_return_records(
            records, metadata_dict
        )

        assert changed == []
        assert unchanged == records
        assert update == {}

    def test_record_alreday_exists_and_add_new(self):
        records = [
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
                "table_sys_id": "7cd58f1647222110afc6fa37536d43ed",
            },
        ]

        metadata_dict = {
            "attachment_name1.txt": {
                "path": "path1",
                "type": "text/plain",
                "hash": "f52a678046a6f06e5fca54b4c535b210f29cbaf1134f2b75197cf47078621902",
            },
            "attachment_name2.txt": {
                "path": "path2",
                "type": "text/markdown",
                "hash": "7f2b0dec698566114435a23f15dcac848a40e1fd3e0eda4afe24a663dda23f2e",
            },
        }

        update, changed, unchanged = attachment.are_changed_return_records(
            records, metadata_dict
        )

        assert changed == []
        assert unchanged == [records[0]]
        assert update == {
            "attachment_name2.txt": {
                "path": "path2",
                "type": "text/markdown",
                "hash": "7f2b0dec698566114435a23f15dcac848a40e1fd3e0eda4afe24a663dda23f2e",
            },
        }

    def test_update_record_and_add_new(self):
        records = [
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
                "table_sys_id": "7cd58f1647222110afc6fa37536d43ed",
            },
        ]

        metadata_dict = {
            "attachment_name1.txt": {
                "path": "path1",
                "type": "text/plain",
                "hash": "6f2b0dec698566114435a23f15dcac848a40e1fd3e0eda4afe24a663dda23f2e",
            },
            "attachment_name2.txt": {
                "path": "path2",
                "type": "text/markdown",
                "hash": "7f2b0dec698566114435a23f15dcac848a40e1fd3e0eda4afe24a663dda23f2e",
            },
        }

        update, changed, unchanged = attachment.are_changed_return_records(
            records, metadata_dict
        )

        assert changed == records
        assert unchanged == []
        assert update == metadata_dict

    def test_record_alreday_exists_update_record_and_add_new(self):
        records = [
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
                "table_sys_id": "7cd58f1647222110afc6fa37536d43ed",
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
                "table_sys_id": "7cd58f1647222110afc6fa37536d43ed",
            },
        ]

        metadata_dict = {
            "attachment_name1.txt": {
                "path": "path1",
                "type": "text/plain",
                "hash": "f52a678046a6f06e5fca54b4c535b210f29cbaf1134f2b75197cf47078621902",
            },
            "attachment_name2.txt": {
                "path": "path2",
                "type": "text/markdown",
                "hash": "7f2b0dec698566114435a23f15dcac848a40e1fd3e0eda4afe24a663dda23f2e",
            },
            "attachment_name3.txt": {
                "path": "path3",
                "type": "text/markdown",
                "hash": "8f2b0dec698566114435a23f15dcac848a40e1fd3e0eda4afe24a663dda23f2e",
            },
        }

        update, changed, unchanged = attachment.are_changed_return_records(
            records, metadata_dict
        )

        assert changed == [records[1]]
        assert unchanged == [records[0]]
        assert update == {
            "attachment_name2.txt": {
                "path": "path2",
                "type": "text/markdown",
                "hash": "7f2b0dec698566114435a23f15dcac848a40e1fd3e0eda4afe24a663dda23f2e",
            },
            "attachment_name3.txt": {
                "path": "path3",
                "type": "text/markdown",
                "hash": "8f2b0dec698566114435a23f15dcac848a40e1fd3e0eda4afe24a663dda23f2e",
            },
        }
