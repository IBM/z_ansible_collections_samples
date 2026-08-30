# -*- coding: utf-8 -*-
# Copyright: (c) 2024, Red Hat
#
# GNU General Public License v3.0+ (see COPYING or https://www.gnu.org/licenses/gpl-3.0.txt)

from __future__ import absolute_import, division, print_function

__metaclass__ = type

import pytest
import sys

from ansible_collections.servicenow.itsm.plugins.module_utils import cmdb_relation

pytestmark = pytest.mark.skipif(
    sys.version_info < (2, 7), reason="requires python2.7 or higher"
)


class TestCmdbRelation:
    def test_valid_ci(self):
        input = dict(
            sys_id="relation_id",
            type=dict(value="relation_sys_id", display_value="relation_name"),
            target=dict(value="target_id", display_value="target_name"),
        )

        relation = cmdb_relation.CmdbRelation(input)

        assert relation.sys_id == "relation_id"
        assert relation.type_id == "relation_sys_id"
        assert relation.type_name == "relation_name"
        assert relation.target_id == "target_id"
        assert relation.target_name == "target_name"

    @pytest.mark.parametrize(
        "input",
        [
            dict(
                type=dict(value="relation_sys_id", display_value="relation_name"),
                target=dict(value="target_id", display_value="target_name"),
            ),
            dict(
                sys_id="relation_id",
                target=dict(value="target_id", display_value="target_name"),
            ),
            dict(
                sys_id="relation_id",
                type=dict(value="relation_sys_id", display_value="relation_name"),
            ),
            dict(
                sys_id="relation_id",
                type=[],
                target=dict(value="target_id", display_value="target_name"),
            ),
            dict(
                sys_id="relation_id",
                type=dict(value="relation_sys_id", display_value="relation_name"),
                target=[],
            ),
        ],
    )
    def test_invalid_ci(self, input):
        with pytest.raises(ValueError):
            cmdb_relation.CmdbRelation(input)

    def test_equal(self):
        input = dict(
            sys_id="relation_id",
            type=dict(value="relation_sys_id", display_value="relation_name"),
            target=dict(value="target_id", display_value="target_name"),
        )

        relation1 = cmdb_relation.CmdbRelation(input)
        relation2 = cmdb_relation.CmdbRelation(input)

        assert relation1 == relation2

    @pytest.mark.parametrize(
        "input1,input2",
        [
            (
                dict(
                    sys_id="relation_id",
                    type=dict(value="relation_sys_id", display_value="relation_name"),
                    target=dict(value="target_id", display_value="target_name"),
                ),
                dict(
                    sys_id="relation_id1",
                    type=dict(value="relation_sys_id", display_value="relation_name"),
                    target=dict(value="target_id", display_value="target_name"),
                ),
            ),
            (
                dict(
                    sys_id=None,
                    type=dict(value="relation_sys_id", display_value="relation_name"),
                    target=dict(value="target_id", display_value="target_name"),
                ),
                dict(
                    sys_id=None,
                    type=dict(value="relation_sys_id", display_value="relation_name"),
                    target=dict(value="another_target_id", display_value="target_name"),
                ),
            ),
            (
                dict(
                    sys_id=None,
                    type=dict(value="relation_sys_id", display_value="relation_name"),
                    target=dict(value="target_id", display_value="target_name"),
                ),
                dict(
                    sys_id=None,
                    type=dict(
                        value="another_relation_sys_id", display_value="relation_name"
                    ),
                    target=dict(value="target_id", display_value="target_name"),
                ),
            ),
        ],
    )
    def test_not_equal(self, input1, input2):
        relation1 = cmdb_relation.CmdbRelation(input1)
        relation2 = cmdb_relation.CmdbRelation(input2)

        assert relation1 != relation2

    def test_to_json(self):
        input = dict(
            sys_id="relation_id",
            type=dict(value="relation_sys_id", display_value="relation_name"),
            target=dict(value="target_id", display_value="target_name"),
        )
        json_dict = dict(
            sys_id="relation_id",
            type=dict(value="relation_sys_id", display_value="relation_name"),
            target=dict(value="target_id", display_value="target_name"),
        )

        r = cmdb_relation.CmdbRelation(input)

        assert json_dict == r.to_json()

    def test_to_payload(self):
        input = dict(
            sys_id="relation_id",
            type=dict(value="relation_sys_id", display_value="relation_name"),
            target=dict(value="target_id", display_value="target_name"),
        )
        payload = dict(
            type="relation_sys_id",
            target="target_id",
        )

        r = cmdb_relation.CmdbRelation(input)

        assert payload == r.to_payload()


class TestCmdbRelations:
    def test_init(self):
        ci = dict(
            sys_id="relation_id",
            outbound_relations=[
                dict(
                    sys_id="relation_1",
                    type=dict(value="relation_sys_id", display_value="relation_name"),
                    target=dict(value="target_id", display_value="target_name"),
                ),
                dict(
                    sys_id="relation_2",
                    type=dict(value="relation_sys_id", display_value="relation_name"),
                    target=dict(value="target_id", display_value="target_name"),
                ),
            ],
        )

        ci_relations = cmdb_relation.CmdbItemRelations(ci)

        assert len(ci_relations.relations) == 2
        for dir, r in ci_relations:
            assert dir == "outbound"
            assert r.sys_id in ("relation_1", "relation_2")

    def test_init2(self):
        ci = dict(
            sys_id="relation_id",
            outbound_relations=[
                dict(
                    sys_id="relation_1",
                    type=dict(value="relation_sys_id", display_value="relation_name"),
                    target=dict(value="target_id", display_value="target_name"),
                ),
                dict(
                    sys_id="relation_2",
                    type=dict(value="relation_sys_id", display_value="relation_name"),
                    target=dict(value="target_id", display_value="target_name"),
                ),
            ],
            inbound_relations=[
                dict(
                    sys_id="inbound_relation_1",
                    type=dict(value="relation_sys_id", display_value="relation_name"),
                    target=dict(value="target_id", display_value="target_name"),
                )
            ],
        )

        ci_relations = cmdb_relation.CmdbItemRelations(ci)

        assert len(ci_relations.relations) == 3

        # count outbound and inbound relations
        count = [0, 0]
        for dir, r in ci_relations:
            if dir == "outbound":
                count[1] += 1
            elif dir == "inbound":
                count[0] += 1
        assert count == [1, 2]

    def test_get(self):
        ci_relations = cmdb_relation.CmdbItemRelations(get_configuration_item())

        rel = ci_relations.get("outbound", "target_id")
        assert rel is not None
        assert rel.sys_id == "relation_1"

        rel1 = ci_relations.get("outbound", "unknown_id")
        assert rel1 is None

        rel2 = ci_relations.get("inbound", "inbound_target_id")
        assert rel2 is not None
        assert rel2.sys_id == "inbound_relation_1"

    def test_add(self):
        ci_relations = cmdb_relation.CmdbItemRelations(get_configuration_item())

        assert len(ci_relations.tainted) == 0

        ci_relations.add(
            "outbound",
            dict(
                sys_id="inbound_relation_2",
                type=dict(value="relation_sys_id", display_value="relation_name"),
                target=dict(value="target_id", display_value="target_name"),
            ),
        )

        assert len(ci_relations.tainted) == 1

        # add the same relation one more time.
        ci_relations.add(
            "outbound",
            dict(
                sys_id="inbound_relation_2",
                type=dict(value="relation_sys_id", display_value="relation_name"),
                target=dict(value="target_id", display_value="target_name"),
            ),
        )

        assert len(ci_relations.tainted) == 1

    def test_remove(self):
        ci_relations = cmdb_relation.CmdbItemRelations(get_configuration_item())

        assert len(ci_relations.tainted) == 0

        ci_relations.remove(
            "outbound",
            dict(
                sys_id="inbound_relation_2",
                type=dict(value="relation_sys_id", display_value="relation_name"),
                target=dict(value="target_id", display_value="target_name"),
            ),
        )

        assert len(ci_relations.tainted) == 1

        # add the same relation one more time.
        ci_relations.remove(
            "outbound",
            dict(
                sys_id="inbound_relation_2",
                type=dict(value="relation_sys_id", display_value="relation_name"),
                target=dict(value="target_id", display_value="target_name"),
            ),
        )

        assert len(ci_relations.tainted) == 1

    def test_clone(self):
        original = cmdb_relation.CmdbItemRelations(get_configuration_item())

        clone = original.clone()

        assert len(clone.relations) == 3

        for dir, rel in clone:
            found = False
            for d, r in original:
                if d == dir and r == rel:
                    found = True
            assert found

    def test_update(self):
        ci_relations = cmdb_relation.CmdbItemRelations(dict(sys_id="ci_1"))

        relation_to_add = cmdb_relation.CmdbRelation(
            dict(
                sys_id="outbound_relation_2",
                type=dict(value="relation_sys_id", display_value="relation_name"),
                target=dict(value="target_id", display_value="target_name"),
            )
        )

        ci_relations.add("outbound", relation_to_add)
        assert len(ci_relations.tainted) == 1

        client_mock = GenericClientMock(
            dict(
                sys_id="ci_1",
                outbound_relations=[
                    dict(
                        sys_id="outbound_relation_2",
                        type=dict(
                            value="relation_sys_id", display_value="relation_name"
                        ),
                        target=dict(value="target_id", display_value="target_name"),
                    )
                ],
            )
        )

        updated_ci = ci_relations.update("test_path", client_mock)

        assert updated_ci is not None
        assert "outbound_relations" in client_mock.payload
        assert client_mock.payload["outbound_relations"] == [
            dict(type="relation_sys_id", target="target_id")
        ]

    def test_update2(self):
        ci = dict(
            sys_id="relation_id",
            outbound_relations=[
                dict(
                    sys_id="relation_1",
                    type=dict(value="relation_sys_id", display_value="relation_name"),
                    target=dict(value="target_id", display_value="target_name"),
                ),
                dict(
                    sys_id="relation_2",
                    type=dict(value="relation_sys_id", display_value="relation_name"),
                    target=dict(value="target_id", display_value="target_name"),
                ),
            ],
        )

        ci_relations = cmdb_relation.CmdbItemRelations(ci)
        ci_relations.remove(
            "oubound",
            cmdb_relation.CmdbRelation(
                dict(
                    sys_id="relation_1",
                    type=dict(value="relation_sys_id", display_value="relation_name"),
                    target=dict(value="target_id", display_value="target_name"),
                )
            ),
        )

        client_mock = GenericClientMock(None)
        updated_ci = ci_relations.update("test_api", client_mock)

        assert updated_ci is not None
        assert client_mock.sys_id == "relation_1"
        assert len(updated_ci.relations) == 1


def get_configuration_item():
    return dict(
        sys_id="relation_id",
        outbound_relations=[
            dict(
                sys_id="relation_1",
                type=dict(value="relation_sys_id", display_value="relation_name"),
                target=dict(value="target_id", display_value="target_name"),
            ),
            dict(
                sys_id="relation_2",
                type=dict(value="relation_sys_id", display_value="relation_name"),
                target=dict(value="target_id", display_value="target_name"),
            ),
        ],
        inbound_relations=[
            dict(
                sys_id="inbound_relation_1",
                type=dict(value="relation_sys_id", display_value="relation_name"),
                target=dict(value="inbound_target_id", display_value="target_name"),
            )
        ],
    )


class GenericClientMock:
    def __init__(self, response_ci):
        self.response_ci = response_ci
        self.payload = None
        self.api_path = ""
        self.sys_id = ""

    def create_record(self, api_path, payload, check_mode, query=None):
        self.payload = payload
        self.api_path = api_path
        return self.response_ci

    def delete_record_by_sys_id(self, api_path, sys_id):
        self.api_path = api_path
        self.sys_id = sys_id
