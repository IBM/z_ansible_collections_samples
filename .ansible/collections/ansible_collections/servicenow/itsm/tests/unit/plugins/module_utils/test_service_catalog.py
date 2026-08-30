# -*- coding: utf-8 -*-
# Copyright: (c) 2024, Red Hat
#
# GNU General Public License v3.0+ (see COPYING or https://www.gnu.org/licenses/gpl-3.0.txt)

from __future__ import absolute_import, division, print_function

__metaclass__ = type

import pytest
import sys

from ansible_collections.servicenow.itsm.plugins.module_utils import service_catalog

pytestmark = pytest.mark.skipif(
    sys.version_info < (2, 7), reason="requires python2.7 or higher"
)


class TestServiceCatalog:
    def test_item_content(self):
        data = (
            (service_catalog.ItemContent.BRIEF, "brief"),
            (service_catalog.ItemContent.FULL, "full"),
            (service_catalog.ItemContent.NONE, "something else"),
        )

        for d in data:
            item_content = service_catalog.ItemContent.from_str(d[1])
            assert item_content == d[0]

    def test_service_catalog_object(self):
        class TestClass(service_catalog.Catalog):
            DISPLAY_FIELDS = ["a", "b"]

        c = TestClass(dict(a="a", b="b", c="c"))

        ansible = c.to_ansible()

        assert "c" not in ansible
        assert "a" in ansible
        assert "b" in ansible
        assert ansible["a"] == "a"
        assert ansible["b"] == "b"

    def test_service_catalog_object_1(self):
        class TestClass(service_catalog.Catalog):
            DISPLAY_FIELDS = ["a", "b"]

        class AnotherClass(service_catalog.Catalog):
            DISPLAY_FIELDS = ["a1", "a2", "a3"]

        another_one = AnotherClass(dict(a1="1", a2="2", a3="3"))
        c = TestClass(dict(a=another_one, b="b", c="c"))

        ansible = c.to_ansible()

        assert "c" not in ansible
        assert "a" in ansible
        assert "b" in ansible
        assert ansible["a"] == dict(a1="1", a2="2", a3="3")
        assert ansible["b"] == "b"

    def test_catalog_display_fields(self):
        c = service_catalog.Catalog(
            dict(
                sys_id="1",
                description="2",
                title="3",
                has_categories="4",
                has_items="5",
                categories="6",
                items="7",
                intruder="8",
            )
        )

        ansible = c.to_ansible()

        assert "sys_id" in ansible
        assert "description" in ansible
        assert "title" in ansible
        assert "has_categories" in ansible
        assert "has_items" in ansible
        assert "categories" in ansible
        assert "sn_items" in ansible
        assert "intruder" not in ansible

    def test_category_display_fields(self):
        c = service_catalog.Category(
            dict(
                sys_id="1",
                description="2",
                title="3",
                subcategories="6",
                full_description="7",
                intruder="8",
            )
        )

        ansible = c.to_ansible()

        assert "sys_id" in ansible
        assert "description" in ansible
        assert "title" in ansible
        assert "subcategories" in ansible
        assert "full_description" in ansible
        assert "intruder" not in ansible

    def test_item_display_fields(self):
        c = service_catalog.Item(
            dict(
                sys_id="1",
                description="2",
                short_description="3",
                availability="4",
                mandatory_attachment="5",
                request_method="6",
                type="6",
                sys_class_name="7",
                catalogs="8",
                name="9",
                category="10",
                order="11",
                categories="12",
                variables="13",
                intruder="8",
            )
        )

        ansible = c.to_ansible()

        assert "sys_id" in ansible
        assert "description" in ansible
        assert "short_description" in ansible
        assert "availability" in ansible
        assert "mandatory_attachment" in ansible
        assert "request_method" in ansible
        assert "type" in ansible
        assert "sys_class_name" in ansible
        assert "catalogs" in ansible
        assert "name" in ansible
        assert "category" in ansible
        assert "order" in ansible
        assert "categories" in ansible
        assert "variables" in ansible
        assert "intruder" not in ansible


class TestServiceCatalogClient:
    class GenericClientMock:
        def __init__(self, retured_data):
            self.retured_data = retured_data
            self.call_params = []

        def list_records(self, api, query=None):
            self.call_params.append(api)
            self.call_params.append(query)
            return self.retured_data

        def get_record_by_sys_id(self, api, sys_id):
            self.call_params.extend([api, sys_id])
            return self.retured_data

    def test_get_catalogs(self):
        data = dict(
            sys_id="1",
            description="2",
            title="3",
            has_categories="4",
            has_items="5",
            items="7",
            intruder="8",
        )

        generic_client = self.GenericClientMock([data])
        sc_client = service_catalog.ServiceCatalogClient(generic_client)

        catalogs = sc_client.get_catalogs()

        assert isinstance(catalogs, list)
        assert isinstance(catalogs[0], service_catalog.Catalog)
        assert catalogs[0].to_ansible() == service_catalog.Catalog(data).to_ansible()
        assert generic_client.call_params[0] == "/".join(
            [service_catalog.SN_BASE_PATH, "catalogs"]
        )

    def test_get_catalog(self):
        data = dict(
            sys_id="1",
            description="2",
            title="3",
            has_categories="4",
            has_items="5",
            items="7",
            intruder="8",
        )

        generic_client = self.GenericClientMock(data)
        sc_client = service_catalog.ServiceCatalogClient(generic_client)

        catalog = sc_client.get_catalog("id")

        assert isinstance(catalog, service_catalog.Catalog)
        assert catalog.to_ansible() == service_catalog.Catalog(data).to_ansible()
        assert generic_client.call_params[0] == "/".join(
            [service_catalog.SN_BASE_PATH, "catalogs"]
        )
        assert generic_client.call_params[1] == "id"

    def test_get_categories(self):
        data = dict(
            sys_id="1",
            description="2",
            title="3",
            subcategories="6",
            full_description="7",
            intruder="8",
        )

        generic_client = self.GenericClientMock([data])
        sc_client = service_catalog.ServiceCatalogClient(generic_client)

        categories = sc_client.get_categories("catalog_id")

        assert isinstance(categories, list)
        assert categories[0].to_ansible() == service_catalog.Category(data).to_ansible()
        assert generic_client.call_params[0] == "/".join(
            [service_catalog.SN_BASE_PATH, "catalogs", "catalog_id", "categories"]
        )

    def test_get_items(self):
        data = dict(
            sys_id="1",
            description="2",
            short_description="3",
            availability="4",
            mandatory_attachment="5",
            request_method="6",
            type="6",
            sys_class_name="7",
            catalogs="8",
            name="9",
            category="10",
            order="11",
            categories="12",
            variables="13",
            intruder="8",
        )

        generic_client = self.GenericClientMock([data])
        sc_client = service_catalog.ServiceCatalogClient(generic_client)

        items = sc_client.get_items("catalog_id")

        assert isinstance(items, list)
        assert items[0].to_ansible() == service_catalog.Item(data).to_ansible()
        assert generic_client.call_params[0] == "/".join(
            [service_catalog.SN_BASE_PATH, "items"]
        )
        assert generic_client.call_params[1] == dict(sysparm_catalog="catalog_id")

    def test_get_item(self):
        data = dict(
            sys_id="1",
            description="2",
            short_description="3",
            availability="4",
            mandatory_attachment="5",
            request_method="6",
            type="6",
            sys_class_name="7",
            catalogs="8",
            name="9",
            category="10",
            order="11",
            categories="12",
            variables="13",
            intruder="8",
        )

        generic_client = self.GenericClientMock(data)
        sc_client = service_catalog.ServiceCatalogClient(generic_client)

        item = sc_client.get_item("item_id")

        assert isinstance(item, service_catalog.Item)
        assert item.to_ansible() == service_catalog.Item(data).to_ansible()
        assert generic_client.call_params[0] == "/".join(
            [service_catalog.SN_BASE_PATH, "items"]
        )
        assert generic_client.call_params[1] == "item_id"
