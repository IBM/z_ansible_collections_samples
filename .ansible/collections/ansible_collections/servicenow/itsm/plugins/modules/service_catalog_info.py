# -*- coding: utf-8 -*-
# Copyright: 2024, Contributors to the Ansible project
#
# GNU General Public License v3.0+ (see COPYING or https://www.gnu.org/licenses/gpl-3.0.txt)


from __future__ import absolute_import, division, print_function

__metaclass__ = type


DOCUMENTATION = r"""
module: service_catalog_info

author:
  - Cosmin Tupangiu (@tupyy)

short_description: List ServiceNow service catalogs along with categories and items

description:
  - Retrieve information about ServiceCatalogs.
  - For more information, refer to ServiceNow service catalog documentation at
    U(https://developer.servicenow.com/dev.do#!/reference/api/utah/rest/c_ServiceCatalogAPI)

version_added: 2.6.0

extends_documentation_fragment:
  - servicenow.itsm.instance
  - servicenow.itsm.sys_id.info

options:
  categories:
    description:
      - If set to V(true), the categories will be fetched from ServiceNow.
    type: bool
    default: False
  items_info:
    description:
      - List of options for fetching service catalog items.
      - Set to V(full), if the whole item will be fetched.
    type: str
    choices: [full, brief, none]
    default: none
  items_query:
    description:
      - Query for the item content.
      - For more information, please refer to
        U(https://developer.servicenow.com/dev.do#!/reference/api/utah/rest/c_ServiceCatalogAPI#servicecat-GET-items)
    type: str
"""

EXAMPLES = r"""
- name: Return all catalogs without categories but with items (brief information)
  servicenow.itsm.service_catalog_info:
    categories: false
    items_info: brief

- name: Return service catalog without categories but with items (brief information)
  servicenow.itsm.service_catalog_info:
    sys_id: "{{ service_catalog.sys_id }}"
    categories: false
    items_info: full

- name: Return service catalog with categories and with items (full information)
  servicenow.itsm.service_catalog_info:
    sys_id: "{{ service_catalog.sys_id }}"
    categories: true
    items_info: full

- name: Return service catalog with categories and with all items containing word "iPhone"
  servicenow.itsm.service_catalog_info:
    sys_id: "{{ service_catalog.sys_id }}"
    categories: true
    items_info: full
    items_query: iPhone
"""

RETURN = r"""
records:
  description:
    - List of catalogs.
  returned: success
  type: list
  sample:
    [
      {
      "categories": [
          {
              "description": "Datacenter hardware and services to the support business\n\t\t\tsystems.\n\t\t",
              "full_description": null,
              "subcategories": [
                  {
                      "sys_id": "d67c446ec0a80165000335aa37eafbc1",
                      "title": "Services"
                  }
              ],
              "sys_id": "803e95e1c3732100fca206e939ba8f2a",
              "title": "Infrastructure"
          },
          {
              "description": "Request for IT services to be performed",
              "full_description": null,
              "subcategories": [],
              "sys_id": "d67c446ec0a80165000335aa37eafbc1",
              "title": "Services"
          }
      ],
      "description": "Products and services for the IT department",
      "has_categories": true,
      "has_items": true,
      "items": [
          {
              "catalogs": [
                  {
                      "active": true,
                      "sys_id": "e0d08b13c3330100c8b837659bba8fb4",
                      "title": "Service Catalog"
                  },
                  {
                      "active": true,
                      "sys_id": "742ce428d7211100f2d224837e61036d",
                      "title": "Technical Catalog"
                  }
              ],
              "category": {
                  "sys_id": "e15706fc0a0a0aa7007fc21e1ab70c2f",
                  "title": "Can We Help You?"
              },
              "description": "<p>Some description</p>",
              "mandatory_attachment": false,
              "name": "Request Knowledge Base",
              "order": 0,
              "request_method": "",
              "short_description": "Request for a Knowledge Base",
              "sys_class_name": "sc_cat_item_producer",
              "sys_id": "81c887819f203100d8f8700c267fcfb5",
              "type": "record_producer"
          },
      ],
      "sys_id": "742ce428d7211100f2d224837e61036d",
      "title": "Technical Catalog"
      }
    ]
"""

from ..module_utils import arguments, client, errors, generic
from ..module_utils.service_catalog import ItemContent, ServiceCatalogClient
from ansible.module_utils.basic import AnsibleModule


def get_catalog_info(sc_client, catalog, with_categories, items_config):
    if with_categories:
        catalog.categories = sc_client.get_categories(catalog.sys_id)

    if items_config["info"] == ItemContent.NONE:
        return catalog

    query = None
    if items_config["query"]:
        query = dict(sysparm_text=items_config["query"])

    items = sc_client.get_items(catalog.sys_id, query)
    if items_config["info"] == ItemContent.BRIEF:
        catalog.items = items
        return catalog

    catalog.items = [sc_client.get_item(item.sys_id) for item in items]

    return catalog


def run(module, sc_client):
    items_config = dict(
        info=ItemContent.from_str(module.params["items_info"]),
        query=module.params["items_query"],
    )

    fetch_categories = module.params["categories"]

    if "sys_id" in module.params and module.params["sys_id"]:
        catalog = get_catalog_info(
            sc_client,
            sc_client.get_catalog(module.params["sys_id"]),
            fetch_categories,
            items_config,
        )
        return [catalog.to_ansible()]

    # fetch all catalogs
    catalogs = []
    for catalog in sc_client.get_catalogs():
        catalog = get_catalog_info(sc_client, catalog, fetch_categories, items_config)
        catalogs.append(catalog.to_ansible())

    return catalogs


def main():
    module_args = dict(
        arguments.get_spec(
            "instance",
            "sys_id",
        ),
        categories=dict(
            type="bool",
            default=False,
        ),
        items_info=dict(
            type="str",
            choices=["brief", "full", "none"],
            default="none",
        ),
        items_query=dict(type="str"),
    )

    module = AnsibleModule(
        argument_spec=module_args,
        supports_check_mode=True,
    )

    try:
        snow_client = client.Client(**module.params["instance"])
        generic_client = generic.GenericClient(snow_client)
        sc_client = ServiceCatalogClient(generic_client)
        records = run(module, sc_client)
        module.exit_json(changed=False, records=records)
    except errors.ServiceNowError as e:
        module.fail_json(msg=str(e))


if __name__ == "__main__":
    main()
