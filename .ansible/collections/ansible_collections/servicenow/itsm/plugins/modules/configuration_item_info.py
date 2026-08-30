#!/usr/bin/python
# -*- coding: utf-8 -*-
# Copyright: (c) 2021, XLAB Steampunk <steampunk@xlab.si>
#
# GNU General Public License v3.0+ (see COPYING or https://www.gnu.org/licenses/gpl-3.0.txt)


from __future__ import absolute_import, division, print_function

__metaclass__ = type


DOCUMENTATION = r"""
module: configuration_item_info

author:
  - Manca Bizjak (@mancabizjak)
  - Miha Dolinar (@mdolin)
  - Tadej Borovsak (@tadeboro)
  - Matej Pevec (@mysteriouswolf)
  - Polona Mihalič (@PolonaM)

short_description: List ServiceNow configuration item

description:
  - Retrieve information about the ServiceNow configuration items and also attachments related to this CI.
  - For more information, refer to the ServiceNow configuration item management documentation at
    U(https://docs.servicenow.com/bundle/tokyo-servicenow-platform/page/product/configuration-management/concept/c_ITILConfigurationManagement.html).
version_added: 1.0.0
extends_documentation_fragment:
  - servicenow.itsm.instance
  - servicenow.itsm.sys_id.info
  - servicenow.itsm.query
  - servicenow.itsm.configuration_item_mapping
  - servicenow.itsm.sysparm_display_value
seealso:
  - module: servicenow.itsm.configuration_item

options:
  name:
    description:
      - Unique identifier of the record to retrieve.
      - Mutually exclusive with C(query) and C(sys_id).
    type: str
    version_added: 2.0.0
  sys_class_name:
    description:
      - ServiceNow configuration item class.
      - The value of this parameter should point to a ServiceNow CMDB configuration
        item table, for instance C(cmdb_ci_server).
      - For a list of valid CMDB tables, refer to ServiceNow documentation on
        U(https://docs.servicenow.com/bundle/tokyo-servicenow-platform/page/product/configuration-management/reference/cmdb-tables-details.html).
      - If this parameter is unset when a configuration item info is queried,
        the default value C(cmdb_ci) will be used.
    type: str
  return_fields:
    description:
      - A list of fields to return.
      - If defined you need to add "attachments" as a field to return if you wish also get related attachments data.
      - If C(return_fields) is not defined, all fields and also attachments will be returned.
    type: list
    elements: str
    required: false
    version_added: 2.4.0
"""

EXAMPLES = r"""
- name: Retrieve all configuration items
  servicenow.itsm.configuration_item_info:
  register: result

- name: Retrieve a specific configuration item by sys_id
  servicenow.itsm.configuration_item_info:
    sys_id: 01a9ec0d3790200044e0bfc8bcbe5dc3
  register: result

- name: Retrieve a specific configuration item by sys_id with limited return fields
  servicenow.itsm.configuration_item_info:
    sys_id: 01a9ec0d3790200044e0bfc8bcbe5dc3
    return_fields:
      - name
      - sys_id
  register: result

- name: Retrieve a specific configuration item by name
  servicenow.itsm.configuration_item_info:
    name: my-configuration-item
  register: result

- name: Retrieve all hardware configuration items by using field query
  servicenow.itsm.configuration_item_info:
    query:
      - category: = Hardware
  register: result

- name: Retrieve all hardware configuration items by using field sysparm_query
  servicenow.itsm.configuration_item_info:
    sysparm_query: category=Hardware
  register: result

- name: Retrieve configuration items in hardware category assigned to abel.tuter or bertie.luby
  servicenow.itsm.configuration_item_info:
    query:
      - category: = hardware
        assigned_to: = abel.tuter
      - category: = hardware
        assigned_to: = bertie.luby

- name: Retrieve configuration items using configuration items mapping
  servicenow.itms.configuration_item_info:
    configuration_item_mapping:
      install_status:
        1: installed
      operational_status:
        1: operational
"""

RETURN = r"""
record:
  description:
    - A list of configuration item records.
    - Note that the fields of the returned records depend on the configuration
      item's I(sys_class_name).
  returned: success
  type: list
  sample:
    "asset": "05a9ec0d3790200044e0bfc8bcbe5dc2"
    "asset_tag": "P1000440"
    "assigned": "2019-02-28 08:00:00"
    "assigned_to": "8a826bf03710200044e0bfc8bcbe5d96"
    "assignment_group": ""
    "attachments":
      -  "average_image_color": ""
         "chunk_size_bytes": "700000"
         "compressed": "true"
         "content_type": "text/plain"
         "download_link": "https://www.example.com/api/now/attachment/919d34d50706301022f9ffa08c1ed047/file"
         "file_name": "sample_file1.txt"
         "hash": "6f2b0dec698566114435a23f15dcac848a40e1fd3e0eda4afe24a663dda23f2e"
         "image_height": ""
         "image_width": ""
         "size_bytes": "210"
         "size_compressed": "206"
         "state": "pending"
         "sys_created_by": "admin"
         "sys_created_on": "2021-08-17 11:18:58"
         "sys_id": "919d34d50706301022f9ffa08c1ed047"
         "sys_mod_count": "0"
         "sys_tags": ""
         "sys_updated_by": "admin"
         "sys_updated_on": "2021-08-17 11:18:58"
         "table_name": "cmdb_ci"
         "table_sys_id": "459d34d50706301022f9ffa08c1ed06a"
    "attestation_score": ""
    "attested": "false"
    "attested_by": ""
    "attested_date": ""
    "attributes": ""
    "can_print": "false"
    "category": "Hardware"
    "change_control": ""
    "checked_in": ""
    "checked_out": ""
    "comments": ""
    "company": "81fca4cbac1d55eb355b4b6db0e3c80f"
    "correlation_id": ""
    "cost": "1699.99"
    "cost_cc": "USD"
    "cost_center": "d9d01546c0a80a6403e18b82250c80a1"
    "delivery_date": "2018-07-05 07:00:00"
    "department": "a581ab703710200044e0bfc8bcbe5de8"
    "discovery_source": ""
    "dns_domain": ""
    "due": ""
    "due_in": ""
    "duplicate_of": ""
    "environment": ""
    "fault_count": "0"
    "first_discovered": ""
    "fqdn": ""
    "gl_account": ""
    "install_date": "2018-10-02 07:00:00"
    "install_status": "installed"
    "invoice_number": ""
    "ip_address": ""
    "justification": ""
    "last_discovered": ""
    "lease_id": ""
    "life_cycle_stage": ""
    "life_cycle_stage_status": ""
    "location": "8228cda2ac1d55eb7029baf443945c37"
    "mac_address": ""
    "maintenance_schedule": ""
    "managed_by": ""
    "managed_by_group": ""
    "manufacturer": "aa0a6df8c611227601cd2ed45989e0ac"
    "model_id": "0c43b858c611227501522de20c61ac75"
    "model_number": ""
    "monitor": "false"
    "name": "ThinkStation S20"
    "operational_status": "operational"
    "order_date": "2018-06-07 07:00:00"
    "owned_by": ""
    "po_number": "PO100005"
    "purchase_date": "2018-06-22"
    "schedule": ""
    "serial_number": "WCL-206-Q10853-BF"
    "short_description": ""
    "skip_sync": "false"
    "start_date": ""
    "subcategory": "Computer"
    "support_group": ""
    "supported_by": ""
    "sys_class_name": "cmdb_ci_computer"
    "sys_class_path": "/!!/!2/!("
    "sys_created_by": "admin"
    "sys_created_on": "2012-02-18 08:14:42"
    "sys_domain": "global"
    "sys_domain_path": "/"
    "sys_id": "01a9ec0d3790200044e0bfc8bcbe5dc3"
    "sys_mod_count": "6"
    "sys_tags": ""
    "sys_updated_by": "system"
    "sys_updated_on": "2021-01-16 05:50:31"
    "unverified": "false"
    "vendor": "aa0a6df8c611227601cd2ed45989e0ac"
    "warranty_expiration": "2021-10-01"
"""


from ansible.module_utils.basic import AnsibleModule

from ..module_utils import arguments, attachment, client, errors, query, table, utils
from ..module_utils.configuration_item import PAYLOAD_FIELDS_MAPPING
from ..module_utils.utils import get_mapper


def remap_assignment(query, table_client):
    query_load = []

    for item in query:
        q = dict()
        for k, v in item.items():
            if k == "assigned_to":
                user = table.find_user(table_client, v[1])
                q["assigned_to"] = (v[0], user["sys_id"])
            else:
                q[k] = v
        query_load.append(q)

    return query_load


def sysparms_query(module, table_client, mapper):
    parsed, err = query.parse_query(module.params["query"])
    if err:
        raise errors.ServiceNowError(err)

    remap_query = remap_assignment(parsed, table_client)

    return query.serialize_query(query.map_query_values(remap_query, mapper))


def run(module, table_client, attachment_client):
    cmdb_table = module.params["sys_class_name"] or "cmdb_ci"
    mapper = get_mapper(
        module,
        "configuration_item_mapping",
        PAYLOAD_FIELDS_MAPPING,
        sysparm_display_value=module.params["sysparm_display_value"],
    )

    if module.params["query"]:
        query = {
            "sysparm_query": sysparms_query(module, table_client, mapper),
            "sysparm_display_value": module.params["sysparm_display_value"],
        }

    else:
        query = utils.filter_dict(
            module.params, "sys_id", "name", "sysparm_query", "sysparm_display_value"
        )

    # default yes, only disabled if not selected in return_fields
    query_attachments = True

    if "return_fields" in module.params and module.params["return_fields"] is not None:
        query["sysparm_fields"] = ",".join(module.params["return_fields"])
        if "attachments" not in module.params["return_fields"]:
            query_attachments = False

    if query_attachments:
        return [
            dict(
                mapper.to_ansible(record),
                attachments=attachment_client.list_records(
                    dict(table_name=cmdb_table, table_sys_id=record["sys_id"]),
                ),
            )
            for record in table_client.list_records(cmdb_table, query)
        ]
    return [
        dict(
            mapper.to_ansible(record),
        )
        for record in table_client.list_records(cmdb_table, query)
    ]


def main():
    module = AnsibleModule(
        supports_check_mode=True,
        argument_spec=dict(
            arguments.get_spec(
                "instance",
                "sys_id",
                "query",
                "configuration_item_mapping",
                "sysparm_query",
                "sysparm_display_value",
            ),
            name=dict(
                type="str",
            ),
            sys_class_name=dict(
                type="str",
            ),
            return_fields=dict(
                type="list",
                elements="str",
            ),
        ),
        mutually_exclusive=[("sys_id", "query", "name", "sysparm_query")],
    )

    try:
        snow_client = client.Client(**module.params["instance"])
        table_client = table.TableClient(snow_client)
        attachment_client = attachment.AttachmentClient(snow_client)
        records = run(module, table_client, attachment_client)
        module.exit_json(changed=False, records=records)
    except errors.ServiceNowError as e:
        module.fail_json(msg=str(e))


if __name__ == "__main__":
    main()
