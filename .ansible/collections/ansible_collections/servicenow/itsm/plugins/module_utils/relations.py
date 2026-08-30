# -*- coding: utf-8 -*-
# Copyright: (c) 2022, XLAB Steampunk <steampunk@xlab.si>
#
# GNU General Public License v3.0+ (see COPYING or https://www.gnu.org/licenses/gpl-3.0.txt)

from __future__ import absolute_import, division, print_function

__metaclass__ = type

import re

REL_TABLE = "cmdb_rel_ci"
# sysparm_fields to be used when querying REL_TABLE. Uses dot-walking
# notation to extract fields from linked tables in a single REST API call.
# https://docs.servicenow.com/bundle/tokyo-application-development/page/integrate/inbound-rest/concept/c_RESTAPI.html
REL_FIELDS = set(
    (
        "sys_id",
        "type.name",
        "parent.sys_id",
        "parent.name",
        "parent.sys_class_name",
        "child.sys_id",
        "child.name",
        "child.sys_class_name",
    )
)

# Similar as above but for sysparm_query
REL_QUERY = None


def _extend_records_with_groups(records, groups):
    for record in records:
        sys_id = record.get("sys_id")
        sys_id_groups = groups.get(sys_id, set())
        record["relationship_groups"] = sys_id_groups

    return records


def _extract_ci_rel_type(type_name):
    # type_name is of form "Parent description::Child description".
    # Return the value of form (Parent_description, Child_description).
    type_name = type_name or "__"
    type_name = re.sub(r"\s|:", "_", type_name)
    ci_rel_type = tuple(type_name.split("__"))

    return ci_rel_type


def _extract_parent_relation(rel_record):
    sys_id = rel_record.get("parent.sys_id", "")
    ci_name = rel_record.get("child.name", "")
    ci_class = rel_record.get("child.sys_class_name", "")
    type_name = rel_record.get("type.name", "")
    ci_rel_type = _extract_ci_rel_type(type_name)[1]

    return sys_id, ci_name, ci_class, ci_rel_type


def _extract_child_relation(rel_record):
    sys_id = rel_record.get("child.sys_id", "")
    ci_name = rel_record.get("parent.name", "")
    ci_class = rel_record.get("parent.sys_class_name", "")
    type_name = rel_record.get("type.name", "")
    ci_rel_type = _extract_ci_rel_type(type_name)[0]

    return sys_id, ci_name, ci_class, ci_rel_type


def _relations_to_groups(rel_records):
    groups = dict()

    extract_relation = dict(
        parent=_extract_parent_relation, child=_extract_child_relation
    )

    for rel_record in rel_records or list():
        for target in ("child", "parent"):
            t_extr_rel = extract_relation[target]
            sys_id, ci_name, ci_class, ci_rel_type = t_extr_rel(rel_record)

            if sys_id and ci_name and ci_rel_type and ci_class:
                rel_group = "{0}_{1}".format(ci_name, ci_rel_type)

                items = groups.setdefault(sys_id, set())
                items.add(rel_group)

    return groups


def enhance_records_with_rel_groups(records, rel_records):
    groups = _relations_to_groups(rel_records)
    records = _extend_records_with_groups(records, groups)

    return records
