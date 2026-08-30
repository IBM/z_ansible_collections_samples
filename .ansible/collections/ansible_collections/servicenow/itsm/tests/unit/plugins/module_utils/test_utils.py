# -*- coding: utf-8 -*-
# Copyright: (c) 2021, XLAB Steampunk <steampunk@xlab.si>
#
# GNU General Public License v3.0+ (see COPYING or https://www.gnu.org/licenses/gpl-3.0.txt)

from __future__ import absolute_import, division, print_function

__metaclass__ = type

import sys

import pytest
from ansible_collections.servicenow.itsm.plugins.module_utils import utils

pytestmark = pytest.mark.skipif(
    sys.version_info < (2, 7), reason="requires python2.7 or higher"
)


class TestFilterDict:
    def test_no_field_names(self):
        assert {} == utils.filter_dict(dict(a=1))

    def test_ignoring_none_values(self):
        assert {} == utils.filter_dict(dict(a=None), "a")

    def test_selecting_a_subset_skip_none_values(self):
        assert dict(a=1, c="str") == utils.filter_dict(
            dict(a=1, b=2, c="str", d=None), "a", "c", "d"
        )


class TestIsSupertset:
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
        assert utils.is_superset(superset, candidate) is True

    @pytest.mark.parametrize(
        "superset,candidate",
        [
            (dict(), dict(a=1)),  # superset is missing a key
            (dict(a=1), dict(a=2)),  # key value is different
        ],
    )
    def test_not_a_superset(self, superset, candidate):
        assert utils.is_superset(superset, candidate) is False


class TestPayloadMapper:
    def test_to_ansible(self):
        mapper = utils.PayloadMapper(dict(a=[(1, 2), (3, 4)], b=[(5, 6)]))

        assert dict(a=4, b=6, c=7) == mapper.to_ansible(dict(a=3, b=5, c=7))

    def test_to_ansible_unknown_value_is_included(self):
        mapper = utils.PayloadMapper(dict(a=[(1, "a1")]))

        assert dict(a=2) == mapper.to_ansible(dict(a=2))

    def test_to_ansible_unknown_value_handler_is_invoked(self, mocker):
        mock_handler = mocker.Mock()
        mapper = utils.PayloadMapper(dict(a=[(1, "a1")]), mock_handler.warn)
        mapper.to_ansible(dict(a="a2"))

        mock_handler.warn.assert_called_once_with(
            "Encountered unknown value a2 while mapping field a."
        )

    def test_to_snow(self):
        mapper = utils.PayloadMapper(dict(a=[(1, 2), (3, 4)], b=[(5, 6)]))

        assert dict(a=1, b=5, c=7) == mapper.to_snow(dict(a=2, b=6, c=7))

    def test_to_snow_unknown_value_is_included(self):
        mapper = utils.PayloadMapper(dict(a=[(1, "a1")]))

        assert dict(a="a2") == mapper.to_snow(dict(a="a2"))

    def test_to_snow_unknown_value_handler_is_invoked(self, mocker):
        mock_handler = mocker.Mock()
        mapper = utils.PayloadMapper(dict(a=[(1, "a1")]), mock_handler.warn)
        mapper.to_snow(dict(a=2))

        mock_handler.warn.assert_called_once_with(
            "Encountered unknown value 2 while mapping field a."
        )

    @pytest.mark.parametrize(
        "data", [dict(), dict(a=1), dict(a=1, b=2), dict(c=3), dict(a=2, c=5)]
    )
    def test_to_ansible_is_inverse_of_to_snow(self, data):
        mapper = utils.PayloadMapper(dict(a=[(1, "a1"), (2, "a2")], b=[(2, "b2")]))

        assert data == mapper.to_snow(mapper.to_ansible(data))
