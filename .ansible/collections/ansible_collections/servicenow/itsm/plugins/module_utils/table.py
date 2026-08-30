# -*- coding: utf-8 -*-
# Copyright: (c) 2021, XLAB Steampunk <steampunk@xlab.si>
#
# GNU General Public License v3.0+ (see COPYING or https://www.gnu.org/licenses/gpl-3.0.txt)

from __future__ import absolute_import, division, print_function

__metaclass__ = type

import itertools
from . import snow


def _query(original=None):
    original = original or dict()
    original.setdefault("sysparm_exclude_reference_link", "true")
    return original


class TableClient(snow.SNowClient):
    def __init__(self, client, batch_size=1000):
        super(TableClient, self).__init__(client, batch_size)

    def list_records(self, table, query=None):
        return self.list(self.path(table), query)

    def get_record(self, table, query, must_exist=False):
        return self.get(self.path(table), query, must_exist)

    def get_record_by_sys_id(self, table, sys_id, must_exist=False):
        return self.get_by_sys_id(self.path(table), sys_id, must_exist)

    def create_record(self, table, payload, check_mode, query=None):
        if check_mode:
            # Approximate the result using the payload.
            return payload

        return self.client.post(self.path(table), payload, query=_query(query)).json[
            "result"
        ]

    def update_record(self, table, record, payload, check_mode, query=None):
        if check_mode:
            # Approximate the result by manually patching the existing state.
            return dict(record, **payload)

        return self.client.patch(
            self.path(table, record["sys_id"]),
            payload,
            query=_query(query),
        ).json["result"]

    def delete_record(self, table, record, check_mode):
        if not check_mode:
            self.delete(self.path(table), record["sys_id"])

    def path(self, table, *subpaths):
        return "/".join(["api/now/table", table] + list(itertools.chain(subpaths)))


def find_user(table_client, user_id):
    # TODO: Maybe add a lookup-by-email option too?
    return table_client.get_record("sys_user", dict(user_name=user_id), must_exist=True)


def find_assignment_group(table_client, assignment_name):
    return table_client.get_record(
        "sys_user_group", dict(name=assignment_name), must_exist=True
    )


def find_standard_change_template(table_client, template_name):
    return table_client.get_record(
        "std_change_producer_version",
        dict(name=template_name),
        must_exist=True,
    )


def find_change_request(table_client, change_request_number):
    return table_client.get_record(
        "change_request", dict(number=change_request_number), must_exist=True
    )


def find_configuration_item(table_client, item_name):
    return table_client.get_record("cmdb_ci", dict(name=item_name), must_exist=True)


def find_problem(table_client, problem_number):
    return table_client.get_record(
        "problem", dict(number=problem_number), must_exist=True
    )
