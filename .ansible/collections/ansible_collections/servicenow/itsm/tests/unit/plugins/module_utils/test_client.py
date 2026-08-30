# -*- coding: utf-8 -*-
# Copyright: (c) 2021, XLAB Steampunk <steampunk@xlab.si>
#
# GNU General Public License v3.0+ (see COPYING or https://www.gnu.org/licenses/gpl-3.0.txt)

from __future__ import absolute_import, division, print_function

__metaclass__ = type

import io
import sys

import pytest
from ansible.module_utils.common.text.converters import to_text
from ansible.module_utils.six.moves.urllib.error import HTTPError, URLError
from ansible.module_utils.six.moves.urllib.parse import parse_qs, urlparse
from ansible_collections.servicenow.itsm.plugins.module_utils import client, errors

pytestmark = pytest.mark.skipif(
    sys.version_info < (2, 7), reason="requires python2.7 or higher"
)


def custom_decoder_hook(dct):
    if "result" in dct:
        if not isinstance(dct["result"], list):
            return dct

        item = dct["result"]
        for k in item:
            if "__meta" in k:
                item.remove(k)
    return dct


class TestResponseInit:
    @pytest.mark.parametrize(
        "raw_headers,expected_headers",
        [
            (None, {}),
            ([], {}),
            ([("a", "aVal"), ("b", "bVal")], {"a": "aVal", "b": "bVal"}),
        ],
    )
    def test_headers(self, raw_headers, expected_headers):
        resp = client.Response(200, "{}", headers=raw_headers)
        assert resp.headers == expected_headers

    def test_valid_json(self):
        resp = client.Response(
            200,
            '{"a": ["b", "c"], "d": 1}',
            headers=[("Content-type", "application/json")],
        )

        assert resp.status == 200
        assert resp.headers == {"content-type": "application/json"}
        assert resp.data == '{"a": ["b", "c"], "d": 1}'
        assert resp.json == {"a": ["b", "c"], "d": 1}

    def test_invalid_json(self):
        resp = client.Response(404, "Not Found")

        assert resp.status == 404
        assert resp.headers == {}
        assert resp.data == "Not Found"
        with pytest.raises(errors.ServiceNowError, match="invalid JSON"):
            resp.json

    def test_json_is_cached(self, mocker):
        json_mock = mocker.patch.object(client, "json")
        resp = client.Response(
            200,
            '{"a": ["b", "c"], "d": 1}',
            headers=[("Content-type", "application/json")],
        )
        resp.json
        resp.json

        assert json_mock.loads.call_count == 1

    def test_custom_decoder_hook(self):
        resp = client.Response(
            200,
            '{"result": [{"__meta": {"encodedQuery": "some_query"}}]}',
            headers=[("Content-type", "application/json")],
            json_decoder_hook=custom_decoder_hook,
        )
        assert resp.json == {"result": []}

    def test_custom_decoder_hook_with_values(self):
        resp = client.Response(
            200,
            '{"result": [{"some_obj": "value"},{"__meta": {"encodedQuery": "some_query"}}]}',
            headers=[("Content-type", "application/json")],
            json_decoder_hook=custom_decoder_hook,
        )
        assert resp.json == {"result": [{"some_obj": "value"}]}


class TestClientInit:
    @pytest.mark.parametrize("host", [None, "", "invalid", "missing.schema"])
    def test_invalid_host(self, host):
        with pytest.raises(errors.ServiceNowError, match="Invalid instance host value"):
            client.Client(host, "user", "pass")

    @pytest.mark.parametrize("host", ["http://insecure.host", "https://secure.host"])
    def test_valid_host(self, host):
        client.Client(host, "user", "pass")


class TestClientAuthHeader:
    def test_basic_auth(self):
        c = client.Client("https://instance.com", "user", "pass")
        assert c.auth_header == {"Authorization": b"Basic dXNlcjpwYXNz"}

    def test_oauth(self, mocker):
        resp_mock = mocker.MagicMock()
        resp_mock.status = 200  # Used when testing on Python 3
        resp_mock.getcode.return_value = 200  # Used when testing on Python 2
        resp_mock.read.return_value = '{"access_token": "token"}'

        request_mock = mocker.patch.object(client, "Request").return_value
        request_mock.open.return_value = resp_mock

        c = client.Client(
            "https://instance.com",
            "user",
            "pass",
            client_id="id",
            client_secret="secret",
        )

        assert c.auth_header == {"Authorization": "Bearer token"}

    def test_oauth_refresh_token(self, mocker):
        resp_mock = mocker.MagicMock()
        resp_mock.status = 200  # Used when testing on Python 3
        resp_mock.getcode.return_value = 200  # Used when testing on Python 2
        resp_mock.read.return_value = '{"access_token": "token"}'

        request_mock = mocker.patch.object(client, "Request").return_value
        request_mock.open.return_value = resp_mock

        c = client.Client(
            "https://instance.com",
            refresh_token="refresh_token",
            grant_type="refresh_token",
            client_id="id",
            client_secret="secret",
        )

        assert c.refresh_token == "refresh_token"
        assert c.grant_type == "refresh_token"
        assert c.client_id == "id"
        assert c.client_secret == "secret"
        assert c.auth_header == {"Authorization": "Bearer token"}

    def test_oauth_failure(self, mocker):
        request_mock = mocker.patch.object(client, "Request").return_value
        request_mock.open.side_effect = HTTPError(
            "", 403, "Forbidden", {}, io.StringIO(to_text("Error message"))
        )

        c = client.Client(
            "https://instance.com",
            "user",
            "pass",
            client_id="id",
            client_secret="secret",
        )
        with pytest.raises(errors.UnexpectedAPIResponse, match="Error message"):
            c.auth_header

    def test_header_is_cached(self, mocker):
        raw_resp_mock = mocker.MagicMock()
        raw_resp_mock.status = 200  # Used when testing on Python 3
        raw_resp_mock.getcode.return_value = 200  # Used when testing on Python 2
        raw_resp_mock.read.return_value = '{"access_token": "token"}'

        request_mock = mocker.patch.object(client, "Request").return_value
        request_mock.open.return_value = raw_resp_mock

        c = client.Client(
            "https://instance.com",
            "user",
            "pass",
            client_id="id",
            client_secret="secret",
        )
        c.auth_header
        c.auth_header

        assert request_mock.open.call_count == 1

    def test_login_username_password(self, mocker):
        mocker.patch.object(client, "Request")
        c = client.Client("https://instance.com", username="user", password="pass")

        assert c.grant_type == "password"
        assert c.username == "user"
        assert c.password == "pass"
        assert c.auth_header == {"Authorization": b"Basic dXNlcjpwYXNz"}  # user:pass

    def test_login_access_token(self, mocker):
        mocker.patch.object(client, "Request")
        c = client.Client("https://instance.com", access_token="token")

        assert c.access_token == "token"
        assert c.auth_header == {"Authorization": "Bearer token"}


class TestClientRequest:
    def test_request_without_data_success(self, mocker):
        c = client.Client("https://instance.com", "user", "pass")
        mock_response = client.Response(
            200, '{"returned": "data"}', headers=[("Content-type", "application/json")]
        )
        request_mock = mocker.patch.object(c, "_request")
        request_mock.return_value = mock_response

        resp = c.request("GET", "api/now/some/path")

        request_mock.assert_called_once_with(
            "GET",
            "https://instance.com/api/now/some/path",
            data=None,
            headers=dict(Accept="application/json", **c.auth_header),
        )
        assert resp == mock_response

    def test_request_with_data_success(self, mocker):
        c = client.Client("https://instance.com", "user", "pass")
        mock_response = client.Response(
            200, '{"returned": "data"}', headers=[("Content-type", "application/json")]
        )
        request_mock = mocker.patch.object(c, "_request")
        request_mock.return_value = mock_response

        resp = c.request("PUT", "api/now/some/path", data={"some": "data"})

        request_mock.assert_called_once_with(
            "PUT",
            "https://instance.com/api/now/some/path",
            data='{"some":"data"}',
            headers={
                "Accept": "application/json",
                "Content-type": "application/json",
                "Authorization": c.auth_header["Authorization"],
            },
        )
        assert resp == mock_response

    def test_auth_error(self, mocker):
        request_mock = mocker.patch.object(client, "Request").return_value
        request_mock.open.side_effect = HTTPError("", 401, "Unauthorized", {}, None)

        c = client.Client("https://instance.com", "user", "pass")
        with pytest.raises(errors.AuthError):
            c.request("GET", "api/now/some/path")

    def test_http_error(self, mocker):
        request_mock = mocker.patch.object(client, "Request").return_value
        request_mock.open.side_effect = HTTPError(
            "", 404, "Not Found", {}, io.StringIO(to_text("My Error"))
        )

        c = client.Client("https://instance.com", "user", "pass")
        resp = c.request("GET", "api/now/some/path")

        assert resp.status == 404
        assert resp.data == "My Error"
        assert resp.headers == {}

    def test_url_error(self, mocker):
        request_mock = mocker.patch.object(client, "Request").return_value
        request_mock.open.side_effect = URLError("some error")

        c = client.Client("https://instance.com", "user", "pass")

        with pytest.raises(errors.ServiceNowError, match="some error"):
            c.request("GET", "api/now/some/path")

    def test_path_escaping(self, mocker):
        request_mock = mocker.patch.object(client, "Request").return_value
        raw_request = mocker.MagicMock(status=200)
        raw_request.read.return_value = "{}"

        c = client.Client("https://instance.com", "user", "pass")
        c.request("GET", "api/now/some path")

        request_mock.open.assert_called_once()
        path_arg = request_mock.open.call_args.args[1]
        assert path_arg == "https://instance.com/api/now/some%20path"

    @pytest.mark.parametrize("query", [None, {}])
    def test_path_without_query(self, mocker, query):
        request_mock = mocker.patch.object(client, "Request").return_value
        raw_request = mocker.MagicMock(status=200)
        raw_request.read.return_value = "{}"

        c = client.Client("https://instance.com", "user", "pass")
        c.request("GET", "api/now/some/path", query=query)

        request_mock.open.assert_called_once()
        path_arg = request_mock.open.call_args.args[1]
        assert path_arg == "https://instance.com/api/now/some/path"

    @pytest.mark.parametrize(
        "query",
        [
            dict(a="b"),
            dict(a="b", c=1),
            dict(a="hello world"),
        ],
    )
    def test_path_with_query(self, mocker, query):
        request_mock = mocker.patch.object(client, "Request").return_value
        raw_request = mocker.MagicMock(status=200)
        raw_request.read.return_value = "{}"

        c = client.Client("https://instance.com", "user", "pass")
        c.request("GET", "api/now/some/path", query=query)

        request_mock.open.assert_called_once()
        path_arg = request_mock.open.call_args.args[1]
        parsed_query = parse_qs(urlparse(path_arg).query)
        assert parsed_query == dict((k, [str(v)]) for k, v in query.items())

    def test_request_without_data_binary_success(self, mocker):
        c = client.Client("https://instance.com", "user", "pass")
        mock_response = client.Response(
            200, "data", headers=[("Content-type", "image/apng")]
        )
        request_mock = mocker.patch.object(c, "_request")
        request_mock.return_value = mock_response

        resp = c.request(
            "GET",
            "api/now/some/path",
            headers={"Accept": "image/apng", "Content-type": "text/plain"},
        )

        request_mock.assert_called_once_with(
            "GET",
            "https://instance.com/api/now/some/path",
            data=None,
            headers=dict(
                {"Accept": "image/apng", "Content-type": "text/plain"}, **c.auth_header
            ),
        )
        assert resp == mock_response

    def test_request_with_data_binary_success(self, mocker):
        c = client.Client("https://instance.com", "user", "pass")
        mock_response = client.Response(
            200, "some_data", headers=[("Content-type", "text/plain")]
        )
        request_mock = mocker.patch.object(c, "_request")
        request_mock.return_value = mock_response

        resp = c.request(
            "PUT",
            "api/now/some/path",
            headers={"Accept": "text/plain", "Content-type": "text/plain"},
            bytes="some_data",
        )

        request_mock.assert_called_once_with(
            "PUT",
            "https://instance.com/api/now/some/path",
            data="some_data",
            headers={
                "Accept": "text/plain",
                "Content-type": "text/plain",
                "Authorization": c.auth_header["Authorization"],
            },
        )
        assert resp == mock_response


class TestClientGet:
    def test_ok(self, mocker):
        c = client.Client("https://instance.com", "user", "pass")
        mock_response = client.Response(200, '{"incident": 1}', None)
        request_mock = mocker.patch.object(c, "request")
        request_mock.return_value = mock_response

        resp = c.get("api/now/table/incident/1")

        assert resp == mock_response
        assert resp.json == {"incident": 1}

    def test_ok_missing(self, mocker):
        c = client.Client("https://instance.com", "user", "pass")
        mock_response = client.Response(404, "Not Found", None)
        request_mock = mocker.patch.object(c, "request")
        request_mock.return_value = mock_response

        resp = c.get("api/now/table/incident/1")

        assert resp == mock_response

    def test_error(self, mocker):
        c = client.Client("https://instance.com", "user", "pass")
        request_mock = mocker.patch.object(c, "request")
        request_mock.return_value = client.Response(403, "forbidden")

        with pytest.raises(errors.UnexpectedAPIResponse, match="forbidden"):
            c.get("api/now/table/incident/1")

    def test_query(self, mocker):
        c = client.Client("https://instance.com", "user", "pass")
        request_mock = mocker.patch.object(c, "request")
        request_mock.return_value = client.Response(200, '{"incident": 1}', None)

        c.get("api/now/table/incident/1", query=dict(a="1"))

        request_mock.assert_called_with(
            "GET", "api/now/table/incident/1", query=dict(a="1")
        )


class TestClientPost:
    def test_ok(self, mocker):
        c = client.Client("https://instance.com", "user", "pass")
        mock_response = client.Response(201, '{"incident": 1}')
        request_mock = mocker.patch.object(c, "request")
        request_mock.return_value = mock_response

        resp = c.post("api/now/table/incident", {"some": "data"})

        assert resp == mock_response
        assert resp.json == {"incident": 1}

    def test_error(self, mocker):
        c = client.Client("https://instance.com", "user", "pass")
        request_mock = mocker.patch.object(c, "request")
        request_mock.return_value = client.Response(400, "bad request")

        with pytest.raises(errors.UnexpectedAPIResponse, match="bad request"):
            c.post("api/now/table/incident", {"some": "data"})

    def test_query(self, mocker):
        c = client.Client("https://instance.com", "user", "pass")
        request_mock = mocker.patch.object(c, "request")
        request_mock.return_value = client.Response(201, '{"incident": 1}')

        c.post("api/now/table/incident", {"some": "data"}, query={"b": "3"})

        request_mock.assert_called_with(
            "POST", "api/now/table/incident", data=dict(some="data"), query=dict(b="3")
        )


class TestClientPatch:
    def test_ok(self, mocker):
        c = client.Client("https://instance.com", "user", "pass")
        mock_response = client.Response(200, '{"incident": 1}')
        request_mock = mocker.patch.object(c, "request")
        request_mock.return_value = mock_response

        resp = c.patch("api/now/table/incident/1", {"some": "data"})

        assert resp == mock_response
        assert resp.json == {"incident": 1}

    def test_error(self, mocker):
        c = client.Client("https://instance.com", "user", "pass")
        request_mock = mocker.patch.object(c, "request")
        request_mock.return_value = client.Response(400, "bad request")

        with pytest.raises(errors.UnexpectedAPIResponse, match="bad request"):
            c.patch("api/now/table/incident/1", {"some": "data"})

    def test_query(self, mocker):
        c = client.Client("https://instance.com", "user", "pass")
        request_mock = mocker.patch.object(c, "request")
        request_mock.return_value = client.Response(200, '{"incident": 1}')

        c.patch("api/now/table/incident/1", {"some": "data"}, query={"g": "f"})

        request_mock.assert_called_with(
            "PATCH",
            "api/now/table/incident/1",
            data=dict(some="data"),
            query=dict(g="f"),
        )


class TestClientPut:
    def test_ok(self, mocker):
        c = client.Client("https://instance.com", "user", "pass")
        mock_response = client.Response(200, '{"incident": 1}')
        request_mock = mocker.patch.object(c, "request")
        request_mock.return_value = mock_response

        resp = c.put("api/now/table/incident/1", {"some": "data"})

        assert resp == mock_response
        assert resp.json == {"incident": 1}

    def test_error(self, mocker):
        c = client.Client("https://instance.com", "user", "pass")
        request_mock = mocker.patch.object(c, "request")
        request_mock.return_value = client.Response(400, "bad request")

        with pytest.raises(errors.UnexpectedAPIResponse, match="bad request"):
            c.put("api/now/table/incident/1", {"some": "data"})

    def test_query(self, mocker):
        c = client.Client("https://instance.com", "user", "pass")
        request_mock = mocker.patch.object(c, "request")
        request_mock.return_value = client.Response(200, '{"incident": 1}')

        c.put("api/now/table/incident/1", {"some": "data"}, query={"j": "i"})

        request_mock.assert_called_with(
            "PUT", "api/now/table/incident/1", data=dict(some="data"), query=dict(j="i")
        )


class TestClientDelete:
    def test_ok(self, mocker):
        c = client.Client("https://instance.com", "user", "pass")
        request_mock = mocker.patch.object(c, "request")
        request_mock.return_value = client.Response(204, {})

        c.delete("api/now/table/resource/1")

    def test_error(self, mocker):
        c = client.Client("https://instance.com", "user", "pass")
        request_mock = mocker.patch.object(c, "request")
        request_mock.return_value = client.Response(404, "not found")

        with pytest.raises(errors.UnexpectedAPIResponse, match="not found"):
            c.delete("api/now/table/resource/1")

    def test_query(self, mocker):
        c = client.Client("https://instance.com", "user", "pass")
        request_mock = mocker.patch.object(c, "request")
        request_mock.return_value = client.Response(204, {})

        c.delete("api/now/table/resource/1", query=dict(x="y"))

        request_mock.assert_called_with(
            "DELETE", "api/now/table/resource/1", query=dict(x="y")
        )


class TestClientValidateCerts:
    @pytest.mark.parametrize("value", [True, False])
    def test_validate_certs(self, create_module, value):
        module = create_module(
            params=dict(
                instance=dict(
                    host="https://my.host.name",
                    username="user",
                    password="pass",
                    validate_certs=value,
                ),
            )
        )
        snow_client = client.Client(**module.params["instance"])

        assert snow_client.validate_certs == value
