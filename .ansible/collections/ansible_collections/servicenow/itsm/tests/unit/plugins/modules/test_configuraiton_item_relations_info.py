# -*- coding: utf-8 -*-
# # Copyright: (c) 2024, Red Hat
#
# GNU General Public License v3.0+ (see COPYING or https://www.gnu.org/licenses/gpl-3.0.txt)

from __future__ import absolute_import, division, print_function

__metaclass__ = type

import sys

import pytest
from ansible_collections.servicenow.itsm.plugins.modules import (
    configuration_item_relations_info,
)

pytestmark = pytest.mark.skipif(
    sys.version_info < (2, 7), reason="requires python2.7 or higher"
)


class TestConfigurationInfoRelationsInfo:
    def test_add_relations(self, create_module, generic_client):
        module = create_module(
            params=dict(
                instance=dict(
                    host="https://my.host.name", username="user", password="pass"
                ),
                sysparm_display_value="true",
                sys_id="parent_id",
                classname="cmdb_ci_linux_server",
            )
        )

        generic_client.get_by_sys_id.return_value = dict(
            inbound_relations=[],
            outbound_relations=[
                dict(
                    sys_id="relation_1",
                    type=dict(value="relation_sys_id", display_value="relation_name"),
                    target=dict(value="target_id_1", display_value="target_name"),
                ),
            ],
        )

        result = configuration_item_relations_info.run(module, generic_client)

        assert result == dict(
            inbound_relations=[],
            outbound_relations=[
                dict(
                    sys_id="relation_1",
                    type=dict(value="relation_sys_id", display_value="relation_name"),
                    target=dict(value="target_id_1", display_value="target_name"),
                ),
            ],
        )
