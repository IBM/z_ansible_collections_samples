# -*- coding: utf-8 -*-
# Copyright: (c) 2022, XLAB Steampunk <steampunk@xlab.si>
#
# GNU General Public License v3.0+ (see COPYING or https://www.gnu.org/licenses/gpl-3.0.txt)

from __future__ import absolute_import, division, print_function

__metaclass__ = type

import sys

import pytest
from ansible_collections.servicenow.itsm.plugins.module_utils import relations

pytestmark = pytest.mark.skipif(
    sys.version_info < (2, 7), reason="requires python2.7 or higher"
)


class TestAddRelationsToRecords:
    def test_add_empty_relations_to_records(self):
        records = [dict(sys_id="s1", name="abc")]
        rels = dict()

        relations._extend_records_with_groups(records, rels)

        assert records == [dict(sys_id="s1", name="abc", relationship_groups=set())]

    def test_add_relations_to_empty_records(self):
        records = []
        rels = dict()

        relations._extend_records_with_groups(records, rels)

        assert records == []

    def test_add_relations_to_records(self):
        records = [dict(sys_id="s1", name="abc")]
        rels = dict(s1=set(("g1", "g2")), s2=set(("g3",)))

        relations._extend_records_with_groups(records, rels)

        assert records == [
            dict(sys_id="s1", name="abc", relationship_groups=set(("g1", "g2")))
        ]


class TestGroupRelations:
    def test_group_empty_relations(self):
        records = []
        rels = relations._relations_to_groups(records)
        assert rels == dict()

    def test_group_relations(self):
        records = [
            {
                "parent.sys_id": "p1",
                "child.name": "cn1",
                "child.sys_class_name": "cscn1",
                "child.sys_id": "c1",
                "parent.name": "pn1",
                "parent.sys_class_name": "pscn1",
                "type.name": "parent1::child1",
            },
            {
                "parent.sys_id": "p1",
                "child.name": "cn2",
                "child.sys_class_name": "cscn2",
                "child.sys_id": "c2",
                "parent.name": "pn1",
                "parent.sys_class_name": "pscn1",
                "type.name": "parent1::child2",
            },
        ]

        actual = relations._relations_to_groups(records)

        assert actual == dict(
            p1=set(("cn1_child1", "cn2_child2")),
            c1=set(("pn1_parent1",)),
            c2=set(("pn1_parent1",)),
        )


class TestExtractRelation:
    @pytest.mark.parametrize(
        "record,expected",
        [
            (dict(), ("", "", "", "")),
            (dict(some_key="value"), ("", "", "", "")),
            ({"parent.sys_id": "s1"}, ("s1", "", "", "")),
            ({"child.name": "cn"}, ("", "cn", "", "")),
            ({"child.sys_class_name": "cscn"}, ("", "", "cscn", "")),
            ({"type.name": "par::ch"}, ("", "", "", "ch")),
            (
                {
                    "parent.sys_id": "s1",
                    "child.name": "child_name",
                    "child.sys_class_name": "child_sys_class_name",
                    "type.name": "Parent desc::Child desc",
                },
                ("s1", "child_name", "child_sys_class_name", "Child_desc"),
            ),
        ],
    )
    def test_extract_parent_relation(self, record, expected):
        actual = relations._extract_parent_relation(record)
        assert actual == expected

    @pytest.mark.parametrize(
        "record,expected",
        [
            (dict(), ("", "", "", "")),
            (dict(some_key="value"), ("", "", "", "")),
            ({"child.sys_id": "s1"}, ("s1", "", "", "")),
            ({"parent.name": "pn"}, ("", "pn", "", "")),
            ({"parent.sys_class_name": "pscn"}, ("", "", "pscn", "")),
            ({"type.name": "par::ch"}, ("", "", "", "par")),
            (
                {
                    "child.sys_id": "s1",
                    "parent.name": "parent_name",
                    "parent.sys_class_name": "parent_sys_class_name",
                    "type.name": "Parent desc::Child desc",
                },
                ("s1", "parent_name", "parent_sys_class_name", "Parent_desc"),
            ),
        ],
    )
    def test_extract_child_relation(self, record, expected):
        actual = relations._extract_child_relation(record)
        assert actual == expected


class TestGetRelationType:
    @pytest.mark.parametrize(
        "type_name,expected",
        [
            (None, ("", "")),
            ("", ("", "")),
            ("Parent::Child", ("Parent", "Child")),
            (
                "Par ent desc ription::Child description",
                ("Par_ent_desc_ription", "Child_description"),
            ),
        ],
    )
    def test_extract_rel_ci_type_empty(self, type_name, expected):
        actual = relations._extract_ci_rel_type(type_name)
        assert actual == expected


class TestEnhanceRecordsWithRelationGroups:
    def test_enhance_empty_records_with_empty_rel_groups(self):
        records = []
        rel_records = []

        relations.enhance_records_with_rel_groups(records, rel_records)

        assert records == []

    def test_enhance_empty_records_with_rel_groups(self):
        records = []
        rel_records = [
            {
                "parent.sys_id": "s1",
                "child.name": "child_name",
                "child.sys_class_name": "child_sys_class_name",
                "type.name": "Parent desc::Child desc",
            }
        ]

        relations.enhance_records_with_rel_groups(records, rel_records)

        assert records == []

    def test_enhance_records_with_rel_groups(self):
        records = [dict(sys_id="s1"), dict(sys_id="s2")]
        rel_records = [
            {
                "parent.sys_id": "s1",
                "child.name": "child_name",
                "child.sys_class_name": "child_sys_class_name",
                "type.name": "Parent desc::Child desc",
            }
        ]

        relations.enhance_records_with_rel_groups(records, rel_records)

        assert records == [
            dict(sys_id="s1", relationship_groups=set(("child_name_Child_desc",))),
            dict(sys_id="s2", relationship_groups=set()),
        ]
