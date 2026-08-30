# -*- coding: utf-8 -*-
# Copyright: 2024, Contributors to the Ansible project
#
# GNU General Public License v3.0+ (see COPYING or https://www.gnu.org/licenses/gpl-3.0.txt)

from __future__ import absolute_import, division, print_function

__metaclass__ = type

import sys

import pytest
from ansible_collections.servicenow.itsm.plugins.module_utils import service_catalog
from ansible_collections.servicenow.itsm.plugins.modules import (
    service_catalog_info,
)

pytestmark = pytest.mark.skipif(
    sys.version_info < (2, 7), reason="requires python2.7 or higher"
)


class TestModule:
    class GenericClientMock:
        def __init__(self, retured_data):
            self.retured_data = retured_data
            self.call_params = []

        def list_records(self, api, query=None):
            self.call_params.append(api)
            self.call_params.append(query)
            if "categories" in api:
                if not isinstance(self.retured_data, tuple):
                    raise ValueError(
                        "expected tuple in retured_data when looking for categories"
                    )
                return self.retured_data[1]
            if "items" in api:
                if not isinstance(self.retured_data, tuple):
                    raise ValueError(
                        "expected tuple in retured_data when looking for categories"
                    )
                return self.retured_data[2]
            if isinstance(self.retured_data, tuple):
                return self.retured_data[0]
            return self.retured_data

        def get_record_by_sys_id(self, api, sys_id):
            self.call_params.extend([api, sys_id])
            return self.retured_data

    def get_sc_client(self, data):
        generic_client = self.GenericClientMock(data)
        return service_catalog.ServiceCatalogClient(generic_client)

    def test_get_without_categories(self, create_module):
        module = create_module(
            params=dict(
                instance=dict(
                    host="https://my.host.name", username="user", password="pass"
                ),
                categories=False,
                items_info="none",
                items_query=None,
            )
        )

        data = dict(
            sys_id="1",
            description="2",
            title="3",
            has_categories="4",
            has_items="5",
        )

        records = service_catalog_info.run(module, self.get_sc_client([data]))

        assert len(records) == 1
        assert records[0] == service_catalog.Catalog(data).to_ansible()

    def test_get_by_sys_id(self, create_module):
        module = create_module(
            params=dict(
                instance=dict(
                    host="https://my.host.name", username="user", password="pass"
                ),
                sys_id="catalog_sys_id",
                categories=False,
                items_info="none",
                items_query=None,
            )
        )

        data = dict(
            sys_id="catalog_sys_id",
            description="2",
            title="3",
            has_categories="4",
            has_items="5",
        )

        records = service_catalog_info.run(module, self.get_sc_client(data))

        assert len(records) == 1
        assert records[0] == service_catalog.Catalog(data).to_ansible()

    def test_get_with_categories(self, create_module):
        module = create_module(
            params=dict(
                instance=dict(
                    host="https://my.host.name", username="user", password="pass"
                ),
                categories=True,
                items_info="none",
                items_query=None,
            )
        )

        catalog = dict(
            sys_id="1",
            description="2",
            title="3",
            has_categories="4",
            has_items="5",
        )
        category = dict(sys_id="category_sys_id")

        records = service_catalog_info.run(
            module, self.get_sc_client(([catalog], [category]))
        )

        assert len(records) == 1
        assert len(records[0]["categories"]) == 1
        assert (
            records[0]["categories"][0]
            == service_catalog.Category(category).to_ansible()
        )
        assert records[0]["sys_id"] == "1"

    def test_get_with_categories_and_items(self, create_module):
        module = create_module(
            params=dict(
                instance=dict(
                    host="https://my.host.name", username="user", password="pass"
                ),
                categories=True,
                items_info="brief",
                items_query=None,
            )
        )

        catalog = dict(
            sys_id="1",
            description="2",
            title="3",
            has_categories="4",
            has_items="5",
        )
        category = dict(sys_id="category_sys_id")
        item = dict(sys_id="item_sys_id")

        records = service_catalog_info.run(
            module, self.get_sc_client(([catalog], [category], [item]))
        )

        assert len(records) == 1
        assert len(records[0]["categories"]) == 1
        assert len(records[0]["sn_items"]) == 1
        assert (
            records[0]["categories"][0]
            == service_catalog.Category(category).to_ansible()
        )
        assert records[0]["sn_items"][0] == service_catalog.Category(item).to_ansible()
        assert records[0]["sys_id"] == "1"
