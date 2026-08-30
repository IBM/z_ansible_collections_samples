# -*- coding: utf-8 -*-
# # Copyright: (c) 2024, Red Hat
#
# GNU General Public License v3.0+ (see COPYING or https://www.gnu.org/licenses/gpl-3.0.txt)

from __future__ import absolute_import, division, print_function

__metaclass__ = type

import sys

import pytest
from ansible_collections.servicenow.itsm.plugins.modules import (
    configuration_item_relations,
)

pytestmark = pytest.mark.skipif(
    sys.version_info < (2, 7), reason="requires python2.7 or higher"
)


class TestEnsureAbsent:
    def test_add_relations(self, create_module, generic_client):
        module = create_module(
            params=dict(
                instance=dict(
                    host="https://my.host.name", username="user", password="pass"
                ),
                state="absent",
                sysparm_display_value="true",
                parent_sys_id="parent_id",
                parent_classname="cmdb_ci_linux_server",
                name="Cools:Cooled by",
                direction="outbound",
                targets=[dict(sys_id="target_id_1", name="target")],
            )
        )

        generic_client.list_records.return_value = [dict(sys_id="relation_1")]

        generic_client.get_by_sys_id.return_value = dict(
            sys_class_name="cmdb_ci_linux_server",
            sys_id="parent_id",
            outbound_relations=[
                dict(
                    sys_id="relation_1",
                    type=dict(value="relation_sys_id", display_value="relation_name"),
                    target=dict(value="target_id_1", display_value="target_name"),
                ),
                dict(
                    sys_id="relation_2",
                    type=dict(value="relation_sys_id", display_value="relation_name"),
                    target=dict(value="target_id_2", display_value="target_name"),
                ),
            ],
        )

        generic_client.delete_record_by_sys_id.return_value = None

        result = configuration_item_relations.ensure_absent(module, generic_client)

        assert result == (
            True,
            dict(
                inbound_relations=[],
                outbound_relations=[
                    dict(
                        sys_id="relation_2",
                        type=dict(
                            value="relation_sys_id", display_value="relation_name"
                        ),
                        target=dict(value="target_id_2", display_value="target_name"),
                    ),
                ],
            ),
            dict(
                before=dict(
                    inbound_relations=[],
                    outbound_relations=[
                        dict(
                            sys_id="relation_1",
                            type=dict(
                                value="relation_sys_id", display_value="relation_name"
                            ),
                            target=dict(
                                value="target_id_1", display_value="target_name"
                            ),
                        ),
                        dict(
                            sys_id="relation_2",
                            type=dict(
                                value="relation_sys_id", display_value="relation_name"
                            ),
                            target=dict(
                                value="target_id_2", display_value="target_name"
                            ),
                        ),
                    ],
                ),
                after=dict(
                    inbound_relations=[],
                    outbound_relations=[
                        dict(
                            sys_id="relation_2",
                            type=dict(
                                value="relation_sys_id", display_value="relation_name"
                            ),
                            target=dict(
                                value="target_id_2", display_value="target_name"
                            ),
                        ),
                    ],
                ),
            ),
        )


class TestEnsurePresent:
    def test_add_relations(self, create_module, generic_client):
        module = create_module(
            params=dict(
                instance=dict(
                    host="https://my.host.name", username="user", password="pass"
                ),
                state="present",
                sysparm_display_value="true",
                parent_sys_id="parent_id",
                parent_classname="cmdb_ci_linux_server",
                name="Cools:Cooled by",
                direction="outbound",
                targets=[dict(sys_id="target_id_1", name="target")],
            )
        )

        generic_client.list_records.return_value = [dict(sys_id="relation_1")]

        generic_client.get_by_sys_id.return_value = dict(
            sys_class_name="cmdb_ci_linux_server",
            sys_id="parent_id",
            outbound_relations=[],
        )

        generic_client.create_record.return_value = dict(
            inbound_relations=[],
            outbound_relations=[
                dict(
                    sys_id="relation_1",
                    type=dict(value="relation_sys_id", display_value="relation_name"),
                    target=dict(value="target_id_1", display_value="target_name"),
                ),
            ],
        )

        result = configuration_item_relations.ensure_present(module, generic_client)

        assert result == (
            True,
            dict(
                inbound_relations=[],
                outbound_relations=[
                    dict(
                        sys_id="relation_1",
                        type=dict(
                            value="relation_sys_id", display_value="relation_name"
                        ),
                        target=dict(value="target_id_1", display_value="target_name"),
                    ),
                ],
            ),
            dict(
                before=dict(
                    inbound_relations=[],
                    outbound_relations=[],
                ),
                after=dict(
                    inbound_relations=[],
                    outbound_relations=[
                        dict(
                            sys_id="relation_1",
                            type=dict(
                                value="relation_sys_id", display_value="relation_name"
                            ),
                            target=dict(
                                value="target_id_1", display_value="target_name"
                            ),
                        ),
                    ],
                ),
            ),
        )
