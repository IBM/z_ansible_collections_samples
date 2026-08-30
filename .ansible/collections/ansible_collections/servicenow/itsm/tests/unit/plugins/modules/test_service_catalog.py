# -*- coding: utf-8 -*-
# Copyright: 2024, Contributors to the Ansible project
#
# GNU General Public License v3.0+ (see COPYING or https://www.gnu.org/licenses/gpl-3.0.txt)

from __future__ import absolute_import, division, print_function
from ansible_collections.servicenow.itsm.plugins.module_utils import (
    service_catalog,
    client,
)
import json

__metaclass__ = type


class TestCartClient:
    def test_get_cart(self, mocker):
        def get_cart(*args):
            assert args[0] == "/api/sn_sc/servicecatalog/cart"
            return client.Response(200, json.dumps(dict(result=dict(key="value"))))

        client_mock = client.Client("https://my-host", "username", "password")
        mocker.patch.object(client_mock, "get", new=get_cart)

        cart_client = service_catalog.CartClient(client_mock)
        cart = cart_client.get_cart()

        assert cart == dict(key="value")

    def test_checkout_cart(self, mocker):
        def checkout_cart(*args):
            assert args[0] == "/api/sn_sc/servicecatalog/cart/checkout"
            return client.Response(200, json.dumps(dict(result=dict(key="value"))))

        client_mock = client.Client("https://my-host", "username", "password")
        mocker.patch.object(client_mock, "post", new=checkout_cart)

        cart_client = service_catalog.CartClient(client_mock)
        result = cart_client.checkout_cart()

        assert result == dict(key="value")

    def test_submit_order(self, mocker):
        def submit_order(*args):
            assert args[0] == "/api/sn_sc/servicecatalog/cart/submit_order"
            return client.Response(200, json.dumps(dict(result=dict(key="value"))))

        client_mock = client.Client("https://my-host", "username", "password")
        mocker.patch.object(client_mock, "post", new=submit_order)

        cart_client = service_catalog.CartClient(client_mock)
        result = cart_client.submit_order()

        assert result == dict(key="value")

    def test_add_to_cart(self, mocker):
        def add_to_cart(*args):
            assert "/api/sn_sc/servicecatalog/items/1234/add_to_cart" in args
            for arg in args:
                if arg != "/api/sn_sc/servicecatalog/items/1234/add_to_cart":
                    assert arg["sysparm_quantity"] == "1"
            return client.Response(200, json.dumps(dict(result=dict(key="value"))))

        client_mock = client.Client("https://my-host", "username", "password")
        mocker.patch.object(client_mock, "post", new=add_to_cart)

        cart_client = service_catalog.CartClient(client_mock)
        result = cart_client.add_to_cart(service_catalog.Item(dict(sys_id="1234")))

        assert result == dict(key="value")

    def test_order_now(self, mocker):
        def order_now(*args):
            assert "/api/sn_sc/servicecatalog/items/1234/order_now" in args
            for arg in args:
                if arg != "/api/sn_sc/servicecatalog/items/1234/order_now":
                    assert arg["sysparm_quantity"] == "1"
                    assert arg["variables"] == dict(var="item")
            return client.Response(200, json.dumps(dict(result=dict(key="value"))))

        client_mock = client.Client("https://my-host", "username", "password")
        mocker.patch.object(client_mock, "post", new=order_now)

        cart_client = service_catalog.CartClient(client_mock)
        result = cart_client.order_now(
            service_catalog.Item(dict(sys_id="1234", variables=dict(var="item")))
        )

        assert result == dict(key="value")


class TestItem:
    def test_to_payload(self):
        item = dict(
            sys_id="1234",
            quantity="3",
            also_request_for=["1234", "abcd"],
            requested_for="batman",
            variables=dict(key="value"),
        )
        item = service_catalog.Item(item)
        payload = item.to_payload()

        assert payload["sysparm_quantity"] == "3"
        assert payload["sysparm_also_request_for"] == ",".join(["1234", "abcd"])
        assert payload["sysparm_requested_for"] == "batman"
        assert payload["variables"] == dict(key="value")
