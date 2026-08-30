#!/usr/bin/python
# -*- coding: utf-8 -*-
# Copyright: (c) 2021, XLAB Steampunk <steampunk@xlab.si>
#
# GNU General Public License v3.0+ (see COPYING or https://www.gnu.org/licenses/gpl-3.0.txt)


from __future__ import absolute_import, division, print_function

__metaclass__ = type

DOCUMENTATION = r"""
module: configuration_item

author:
  - Manca Bizjak (@mancabizjak)
  - Miha Dolinar (@mdolin)
  - Tadej Borovsak (@tadeboro)
  - Matej Pevec (@mysteriouswolf)
  - Polona Mihalič (@PolonaM)

short_description: Manage ServiceNow configuration items

description:
  - Create, delete or update a ServiceNow configuration item.
  - Configuration items can be managed using sys_id or name.
  - Operations create and delete are idempotent on parameter C(name).
  - When C(state) is set to C(present), a record identified by C(name) is only created once. Further invocations will update the record.
  - For more information, refer to the ServiceNow configuration management documentation at
    U(https://docs.servicenow.com/bundle/tokyo-servicenow-platform/page/product/configuration-management/reference/cmdb-table-property-descriptions.html).
version_added: 1.0.0
extends_documentation_fragment:
  - servicenow.itsm.instance
  - servicenow.itsm.sys_id
  - servicenow.itsm.attachments
  - servicenow.itsm.configuration_item_mapping
seealso:
  - module: servicenow.itsm.configuration_item_info

options:
  state:
    description:
      - State of the configuration item.
    type: str
    choices: [ present, absent ]
    default: present
  name:
    description:
      - The name of the configuration item.
      - Required if the configuration item does not yet exist.
    type: str
  short_description:
    description:
      - Short description of the configuration item.
    type: str
  sys_class_name:
    description:
      - ServiceNow configuration item class.
      - The value of this parameter should point to a ServiceNow CMDB configuration
        item table, for instance C(cmdb_ci_server).
      - For a list of valid CMDB tables, refer to ServiceNow documentation on
        U(https://docs.servicenow.com/bundle/tokyo-servicenow-platform/page/product/configuration-management/reference/cmdb-tables-details.html).
      - If this parameter is unset when a new configuration item needs to be created,
        the default value C(cmdb_ci) will be used.
    type: str
  asset_tag:
    description:
      - Asset tag of the asset logically related to this configuration item.
      - Read more about the relationship between configuration items and assets at
        U(https://docs.servicenow.com/bundle/tokyo-it-asset-management/page/product/asset-management/concept/c_ManagingAssets.html).
    type: str
  install_status:
    description:
      - The functional status of the configuration item.
      - Default choices are C(implementing), C(installed), C(on_order), C(in_maintenance), C(pending_install),
        C(pending_repair), C(in_stock), C(retired), C(stolen), C(absent).
        One can override them by setting I(configuration_item_mapping.install_status).
    type: str
  operational_status:
    description:
      - The operational status of the configuration item.
      - Default choices are C(operational), C(non_operational), C(repair_in_progress), C(dr_standby), C(ready),
        C(retired), C(pipeline), C(catalog).
        One can override them by setting I(configuration_item_mapping.operational_status).
    type: str
  serial_number:
    description:
      - Serial number of the configuration item.
    type: str
  ip_address:
    description:
      - Primary IP address used by the configuration item.
    type: str
  mac_address:
    description:
      - MAC address of the configuration item.
    type: str
  category:
    description:
      - Category of the configuration item, for instance C(Hardware).
    type: str
  environment:
    description:
      - The environment to which this configuration item belongs.
      - Default choices are C(development), C(production), C(test),
        One can override them by setting I(configuration_item_mapping.environment).
    type: str
  assigned_to:
    description:
      - A person to whom this configuration item is assigned to.
      - Expected value for I(assigned_to) is user id (usually in the form of
        C(first_name.last_name)).
    type: str
  other:
    description:
      - Any of the remaining configuration parameters.
      - For the attributes of the base C(cmdb_ci) table, refer to the ServiceNow documentation on
        U(https://docs.servicenow.com/bundle/tokyo-servicenow-platform/page/product/configuration-management/reference/cmdb-table-property-descriptions.html).
      - For the attributes of configuration items specific to I(sys_class_name),
        please consult the relevant ServiceNow documentation.
    type: dict
"""

EXAMPLES = r"""
- name: Create a configuration item
  servicenow.itsm.configuration_item:
    name: HPE ProLiant BL465C G7
    short_description: HPE ProLiant Server G7
    serial_number: ECE-164-E10834-NO
    asset_tag: P1000613
    sys_class_name: cmdb_ci_server
    assigned_to: some.user
    environment: production
    category: Hardware
    attachments:
      - path: path/to/attachment.txt
    other:
      model_number: BL465C G7
  register: server

- name: Update a configuration item
  servicenow.itsm.configuration_item:
    sys_id: "{{ server.record.sys_id }}"
    install_status: in_maintenance
    operational_status: repair_in_progress
    other:
      fault_count: 1
      classification: Development

- name: Delete a configuration item
  servicenow.itsm.configuration_item:
    sys_id: "{{ server.record.sys_id }}"
    state: absent
"""

RETURN = r"""
record:
  description:
    - The configuration item record.
    - Note that the fields of the returned record depend on the configuration
      item's I(sys_class_name).
  returned: success
  type: dict
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

from ..module_utils import arguments, attachment, client, errors, table, utils
from ..module_utils.configuration_item import PAYLOAD_FIELDS_MAPPING
from ..module_utils.utils import get_mapper

DIRECT_PAYLOAD_FIELDS = (
    "name",
    "short_description",
    "sys_class_name",
    "asset_tag",
    "install_status",
    "operational_status",
    "serial_number",
    "ip_address",
    "mac_address",
    "category",
    "environment",
)


def ensure_absent(module, table_client, attachment_client):
    mapper = get_mapper(module, "configuration_item_mapping", PAYLOAD_FIELDS_MAPPING)
    query = utils.filter_dict(module.params, "sys_id", "name")
    configuration_item = table_client.get_record("cmdb_ci", query)

    if configuration_item:
        cmdb_table = configuration_item["sys_class_name"]
        if cmdb_table != "cmdb_ci":
            configuration_item = table_client.get_record(cmdb_table, query)

        attachment_client.delete_attached_records(
            cmdb_table,
            configuration_item["sys_id"],
            module.check_mode,
        )
        table_client.delete_record(cmdb_table, configuration_item, module.check_mode)

        return (
            True,
            None,
            dict(before=mapper.to_ansible(configuration_item), after=None),
        )

    return False, None, dict(before=None, after=None)


def build_payload(module, table_client):
    payload = (module.params["other"] or {}).copy()
    payload.update(utils.filter_dict(module.params, *DIRECT_PAYLOAD_FIELDS))

    if module.params["assigned_to"]:
        user = table.find_user(table_client, module.params["assigned_to"])
        payload["assigned_to"] = user["sys_id"]

    return payload


def ensure_present(module, table_client, attachment_client):
    mapper = get_mapper(module, "configuration_item_mapping", PAYLOAD_FIELDS_MAPPING)
    query_sys_id = utils.filter_dict(module.params, "sys_id")
    query_name = utils.filter_dict(module.params, "name")
    payload = build_payload(module, table_client)
    attachments = attachment.transform_metadata_list(
        module.params["attachments"], module.sha256
    )

    if not query_sys_id:
        configuration_item = table_client.get_record("cmdb_ci", query_name)
        # User did not specify existing CI, so we need to create a new one.
        if not configuration_item:
            cmdb_table = module.params["sys_class_name"] or "cmdb_ci"
            new = mapper.to_ansible(
                table_client.create_record(
                    cmdb_table, mapper.to_snow(payload), module.check_mode
                )
            )
            # When we execute in check mode, new["sys_id"] is not defined.
            # In order to give users back as much info as possible, we fake the sys_id in the
            # next call.
            new["attachments"] = attachment_client.upload_records(
                cmdb_table,
                new.get("sys_id", "N/A"),
                attachments,
                module.check_mode,
            )
            return True, new, dict(before=None, after=new)

        else:
            # Get existing record using provided name
            old = mapper.to_ansible(configuration_item)

    else:
        # Get existing record using provided sys_id
        old = mapper.to_ansible(
            table_client.get_record("cmdb_ci", query_sys_id, must_exist=True)
        )
        # Check if provided name already exists
        if query_name:
            configuration_item = table_client.get_record("cmdb_ci", query_name)
            if configuration_item:
                old2 = mapper.to_ansible(configuration_item)
                if old["sys_id"] != old2["sys_id"]:
                    raise errors.ServiceNowError(
                        "Record with the name {0} already exists.".format(
                            module.params["name"]
                        )
                    )
    # Update existing record
    cmdb_table = old["sys_class_name"]
    # If necessary, fetch the record from the table for the extended CI class
    if cmdb_table != "cmdb_ci":
        old = mapper.to_ansible(
            table_client.get_record(
                cmdb_table, query_sys_id or query_name, must_exist=True
            )
        )

    old["attachments"] = attachment_client.list_records(
        dict(table_name=cmdb_table, table_sys_id=old["sys_id"])
    )

    if utils.is_superset(old, payload) and not any(
        attachment.are_changed(old["attachments"], attachments)
    ):
        # No change in parameters we are interested in - nothing to do.
        return False, old, dict(before=old, after=old)

    new = mapper.to_ansible(
        table_client.update_record(
            cmdb_table, mapper.to_snow(old), mapper.to_snow(payload), module.check_mode
        )
    )
    new["attachments"] = attachment_client.update_records(
        cmdb_table,
        old["sys_id"],
        attachments,
        old["attachments"],
        module.check_mode,
    )

    return True, new, dict(before=old, after=new)


def run(module, table_client, attachment_client):
    if module.params["state"] == "absent":
        return ensure_absent(module, table_client, attachment_client)
    return ensure_present(module, table_client, attachment_client)


def main():
    module_args = dict(
        arguments.get_spec(
            "instance", "sys_id", "attachments", "configuration_item_mapping"
        ),
        state=dict(
            type="str",
            choices=[
                "present",
                "absent",
            ],
            default="present",
        ),
        name=dict(
            type="str",
        ),
        short_description=dict(
            type="str",
        ),
        sys_class_name=dict(
            type="str",
        ),
        asset_tag=dict(
            type="str",
        ),
        install_status=dict(
            type="str",
        ),
        operational_status=dict(
            type="str",
        ),
        serial_number=dict(
            type="str",
        ),
        ip_address=dict(
            type="str",
        ),
        mac_address=dict(
            type="str",
        ),
        category=dict(
            type="str",
        ),
        environment=dict(
            type="str",
        ),
        assigned_to=dict(
            type="str",
        ),
        other=dict(
            type="dict",
        ),
    )

    module = AnsibleModule(
        argument_spec=module_args,
        supports_check_mode=True,
        required_one_of=[
            (
                "sys_id",
                "name",
            ),
        ],
    )

    try:
        snow_client = client.Client(**module.params["instance"])
        table_client = table.TableClient(snow_client)
        attachment_client = attachment.AttachmentClient(snow_client)
        changed, record, diff = run(module, table_client, attachment_client)
        module.exit_json(changed=changed, record=record, diff=diff)
    except errors.ServiceNowError as e:
        module.fail_json(msg=str(e))


if __name__ == "__main__":
    main()
