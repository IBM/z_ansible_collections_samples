# -*- coding: utf-8 -*-
# Copyright: (c) 2021, XLAB Steampunk <steampunk@xlab.si>
#
# GNU General Public License v3.0+ (see COPYING or https://www.gnu.org/licenses/gpl-3.0.txt)

from __future__ import absolute_import, division, print_function

__metaclass__ = type

import sys

import pytest
from ansible_collections.servicenow.itsm.plugins.module_utils import query, utils

pytestmark = pytest.mark.skipif(
    sys.version_info < (2, 7), reason="requires python2.7 or higher"
)


class TestGetOperatorAndValue:
    def test_valied_operator(self):
        result = query.get_operator_and_value("= new")

        assert result == ("=", "new")

    def test_unary_operator(self):
        result = query.get_operator_and_value("ISEMPTY")

        assert result == ("ISEMPTY", "")

    def test_invalid_operator(self):
        result = query.get_operator_and_value("== new")

        assert result == (None, None)


class TestParseQuery:
    def test_parse_query(self):
        result = query.parse_query([{"short_description": "LIKE SAP"}])

        assert result == ([{"short_description": ("LIKE", "SAP")}], [])

    def test_and_parse_query(self):
        result = query.parse_query(
            [{"description": "LIKE email", "short_description": "LIKE SAP"}]
        )

        assert result == (
            [{"description": ("LIKE", "email"), "short_description": ("LIKE", "SAP")}],
            [],
        )

    def test_or_parse_query(self):
        result = query.parse_query(
            [{"short_description": "LIKE SAP"}, {"description": "LIKE email"}]
        )

        assert result == (
            [
                {"short_description": ("LIKE", "SAP")},
                {"description": ("LIKE", "email")},
            ],
            [],
        )

    def test_and_or_parse_query(self):
        result = query.parse_query(
            [
                {"caller": "= abel.tuter", "state": "= new"},
                {"caller": "= bertie.luby", "state": "= new"},
            ]
        )

        assert result == (
            [
                {"caller": ("=", "abel.tuter"), "state": ("=", "new")},
                {"caller": ("=", "bertie.luby"), "state": ("=", "new")},
            ],
            [],
        )

    def test_error_parse_query(self):
        result = query.parse_query(
            [
                {"state": "== new"},
            ]
        )

        assert result == ([], ["Invalid condition '== new' for column 'state'."])

    def test_error_parse_unary_query(self):
        result = query.parse_query(
            [
                {"state": "ISEMPTY new"},
            ]
        )

        assert result == ([], ["Operator ISEMPTY does not take any arguments"])


class TestSerializeQuery:
    def test_serialize_query(self):
        result = query.serialize_query([{"short_description": ("LIKE", "SAP")}])

        assert result == "short_descriptionLIKESAP"

    def test_and_serialize_query(self):
        result = query.serialize_query(
            [{"description": ("LIKE", "email"), "short_description": ("LIKE", "SAP")}]
        )

        assert set(result.split("^")) == set(
            ("descriptionLIKEemail", "short_descriptionLIKESAP")
        )

    def test_or_serialize_query(self):
        result = query.serialize_query(
            [
                {"short_description": ("LIKE", "SAP")},
                {"description": ("LIKE", "email")},
            ]
        )

        assert result == "short_descriptionLIKESAP^NQdescriptionLIKEemail"

    def test_and_or_serialize_query(self):
        result = query.serialize_query(
            [
                {"caller": ("=", "abel.tuter"), "state": ("=", "new")},
                {"caller": ("=", "bertie.luby"), "state": ("=", "new")},
            ]
        )

        assert set(result.replace("^", " ").replace("NQ", " ").split()) == set(
            ("caller=abel.tuter", "state=new", "caller=bertie.luby")
        )


class TestMapQueryValues:
    def test_map_query_values(self):
        mapper = utils.PayloadMapper(dict(state=[("1", "new")]), None)

        result = query.map_query_values(
            [
                {"caller": ("=", "abel.tuter"), "state": ("=", "new")},
                {"caller": ("=", "bertie.luby"), "state": ("=", "new")},
            ],
            mapper,
        )

        assert result == [
            {"caller": ("=", "abel.tuter"), "state": ("=", "1")},
            {"caller": ("=", "bertie.luby"), "state": ("=", "1")},
        ]
