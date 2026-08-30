# -*- coding: utf-8 -*-
# Copyright: (c) 2021, XLAB Steampunk <steampunk@xlab.si>
#
# GNU General Public License v3.0+ (see COPYING or https://www.gnu.org/licenses/gpl-3.0.txt)

from __future__ import absolute_import, division, print_function

__metaclass__ = type

import sys

import pytest
from ansible_collections.servicenow.itsm.plugins.module_utils import arguments

pytestmark = pytest.mark.skipif(
    sys.version_info < (2, 7), reason="requires python2.7 or higher"
)


class TestGetSpec:
    def test_get_params_valid_parameter(self):
        result = arguments.get_spec("sys_id")

        assert result == dict(sys_id=dict(type="str"))

    def test_get_params_no_parameter(self):
        result = arguments.get_spec()

        assert result == dict()

    def test_instance_validate_certs_default_value(self):
        result = arguments.get_spec("instance")

        assert result["instance"]["options"]["validate_certs"]["default"] is True
