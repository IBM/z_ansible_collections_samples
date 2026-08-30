# -*- coding: utf-8 -*-
# Copyright: (c) 2021, XLAB Steampunk <steampunk@xlab.si>
#
# GNU General Public License v3.0+
# (see COPYING or https://www.gnu.org/licenses/gpl-3.0.txt)

from __future__ import absolute_import, division, print_function

__metaclass__ = type

import sys

import pytest
from ansible_collections.servicenow.itsm.plugins.module_utils import errors, validation

pytestmark = pytest.mark.skipif(
    sys.version_info < (2, 7), reason="requires python2.7 or higher"
)


class TestMissingFromParamsAndRemote:
    @pytest.mark.parametrize(
        "params,module_params,record",
        [
            (["a"], dict(a="b"), None),  # module param only
            (
                ["a"],
                dict(a="b"),
                dict(),
            ),  # module param & remote record without desired
            (["a"], dict(a="b"), dict(a="b")),  # module param & remote record
            (["a"], dict(a=None), dict(a="b")),  # remote record only
            (
                ["a", "b"],
                dict(a=None, b="d"),
                dict(a="b", b=None),
            ),  # mixed with empty str
            (["a", "b"], dict(a="", b="d"), dict(a="b", b="")),  # mixed with None
        ],
    )
    def test_nothing_missing(self, params, module_params, record):
        assert [] == validation.missing_from_params_and_remote(
            params, module_params, record
        )

    @pytest.mark.parametrize(
        "params,module_params,record",
        [
            (["a"], dict(a=None), None),
            (["a"], dict(a=None), dict()),
            (["a"], dict(a=None), dict(a="")),
        ],
    )
    def test_missing(self, params, module_params, record):
        assert ["a"] == validation.missing_from_params_and_remote(
            params, module_params, record
        )

    def test_invalid_not_a_subset(self):
        with pytest.raises(errors.ServiceNowError, match="not a subset"):
            validation.missing_from_params_and_remote(
                ["a", "b"], dict(a=1, b=2), dict(a=1)
            )

    @pytest.mark.parametrize(
        "module_params,record",
        [
            (dict(a=True), dict(a="")),  # invalid module param type
            (dict(a=None), dict(a=True)),  # invalid record field type
        ],
    )
    def test_invalid_wrong_param_value_type(self, module_params, record):
        with pytest.raises(errors.ServiceNowError, match="text or None"):
            validation.missing_from_params_and_remote(["a"], module_params, record)
