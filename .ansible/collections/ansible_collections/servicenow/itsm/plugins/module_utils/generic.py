# -*- coding: utf-8 -*-
# Copyright: (c) 2024, Red Hat
#
# GNU General Public License v3.0+ (see COPYING or https://www.gnu.org/licenses/gpl-3.0.txt)

from __future__ import absolute_import, division, print_function

__metaclass__ = type

from . import snow
from ..module_utils.api import FIELD_SYS_ID, FIELD_ATTRIBUTES_NAME


class GenericClient(snow.SNowClient):
    def __init__(self, client, batch_size=1000):
        super(GenericClient, self).__init__(client, batch_size)

    def list_records(self, api_path, query=None):
        """
        List records from api_path.
        The method is retrieving all the records by making multiple GET requests to server.

        api_path    -- full path (ex: "api/now/cmdb/instance/cmdb_ci_linux_server"
        query       -- query in SNow format
        """
        return self.list(api_path, query)

    def get_record(self, api_path, query, must_exist=False):
        """
        Return a record matched by the query.

        api_path    -- full path (ex: "api/now/cmdb/instance/cmdb_ci_linux_server")
        query       -- query in SNow format
        must_exist  -- if true the method throws an expection if the records does not exists.
                       Returns None else.
        """

        return self.get(api_path, query, must_exist)

    def get_record_by_sys_id(self, api_path, sys_id):
        """
        Return a record by sys_id

        api_path    -- full path without the sys_id (ex: "api/now/cmdb/instance/cmdb_ci_linux_server")
        sys_id      -- id of the record
        """
        response = self.client.get("/".join((api_path.rstrip("/"), sys_id)))
        if "result" in response.json:
            return response.json["result"]

        return None

    def create_record(self, api_path, payload, check_mode, query=None):
        """
        Create a record

        api_path    -- full path (ex: "api/now/cmdb/instance/cmdb_ci_linux_server")
        payload     -- record's data as a dict
        query       -- query in SNow format
        check_mode  -- set to true if check_mode is activated
        """
        if check_mode:
            # Approximate the result using the payload.
            return payload
        return self.create(api_path, payload, query)

    def update_record(self, api_path, record, payload, check_mode, query=None):
        """
        Update a record

        api_path    -- full path (ex: "api/now/cmdb/instance/cmdb_ci_linux_server")
        payload     -- updated record's data
        query       -- query in SNow format
        check_mode  -- set to true if check_mode is activated
        """
        if check_mode:
            # Approximate the result by manually patching the existing state.
            return dict(record, **payload)
        return self.update(api_path, self.get_sys_id(record), payload, query)

    def delete_record(self, api_path, record, check_mode):
        """
        Remove the record

        api_path -- full path (ex: "api/now/cmdb/instance/cmdb_ci_linux_server")
        record: -- record's data
        query -- query in SNow format
        check_mode -- bool
        """
        if not check_mode:
            return self.delete(api_path, self.get_sys_id(record))

    def delete_record_by_sys_id(self, api_path, sys_id):
        """
        Remove the record by sys_id

        api_path -- full path (ex: "api/now/cmdb/instance/cmdb_ci_linux_server")
        record: -- record's data
        query -- query in SNow format
        check_mode -- bool
        """
        return self.delete(api_path, sys_id)

    def get_sys_id(self, record):
        """
        Returns the sys_id from the record.

        Different services have different record structures, so we need to take into
        account all the different records.
        """
        if FIELD_SYS_ID in record:
            sys_id = record.get(FIELD_SYS_ID)
            if isinstance(sys_id, dict):
                # records from Change Request API has FIELD_SYS_ID as dict
                return FIELD_SYS_ID.get("value")
            return record.get(FIELD_SYS_ID)

        # cmdb record
        if FIELD_ATTRIBUTES_NAME in record:
            return record.get(FIELD_ATTRIBUTES_NAME).get(FIELD_SYS_ID)
