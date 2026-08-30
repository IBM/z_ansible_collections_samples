# -*- coding: utf-8 -*-
# Copyright: (c) 2024, Red Hat
#
# GNU General Public License v3.0+ (see COPYING or https://www.gnu.org/licenses/gpl-3.0.txt)

from __future__ import absolute_import, division, print_function

__metaclass__ = type


from . import errors


class SNowClient:
    def __init__(self, client, batch_size=1000):
        self.client = client
        self.batch_size = batch_size

    def list(self, api_path, query=None):
        base_query = self._sanitize_query(query)
        base_query["sysparm_limit"] = self.batch_size

        offset = 0
        total = 1  # Dummy value that ensures loop executes at least once
        result = []

        while offset < total:
            response = self.client.get(
                api_path,
                query=dict(base_query, sysparm_offset=offset),
            )

            result.extend(response.json["result"])
            # This is a header only for Table API.
            # When using this client for generic api, the header is not present anymore
            # and we need to find a new method to break from the loop
            # It can be removed from Table API but for it's better to keep it for now.
            if "x-total-count" in response.headers:
                total = int(response.headers["x-total-count"])
            else:
                if len(response.json["result"]) == 0:
                    break

            offset += self.batch_size

        return result

    def get(self, api_path, query, must_exist=False):
        records = self.list(api_path, query)

        if len(records) > 1:
            raise errors.ServiceNowError(
                "{0} {1} records match the {2} query.".format(
                    len(records), api_path, query
                )
            )

        if must_exist and not records:
            raise errors.ServiceNowError(
                "No {0} records match the {1} query.".format(api_path, query)
            )

        return records[0] if records else None

    def get_by_sys_id(self, api_path, sys_id, must_exist=False):
        response = self.client.get("/".join([api_path.rstrip("/"), sys_id]))

        record = response.json.get("result", None)
        if must_exist and not record:
            raise errors.ServiceNowError(
                "No {0} records match the sys_id {1}.".format(api_path, sys_id)
            )

        return record

    def create(self, api_path, payload, query=None):
        return self.client.post(
            api_path, payload, query=self._sanitize_query(query)
        ).json["result"]

    def update(self, api_path, sys_id, payload, query=None):
        return self.client.patch(
            "/".join((api_path.rstrip("/"), sys_id)),
            payload,
            query=self._sanitize_query(query),
        ).json["result"]

    def delete(self, api_path, sys_id):
        self.client.delete("/".join((api_path.rstrip("/"), sys_id)))

    def _sanitize_query(self, query):
        query = query or dict()
        query.setdefault("sysparm_exclude_reference_link", "true")
        return query
