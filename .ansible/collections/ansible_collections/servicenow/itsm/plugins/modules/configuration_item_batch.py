#!/usr/bin/python
# -*- coding: utf-8 -*-
# Copyright: (c) 2021, XLAB Steampunk <steampunk@xlab.si>
#
# GNU General Public License v3.0+ (see COPYING or https://www.gnu.org/licenses/gpl-3.0.txt)


from __future__ import absolute_import, division, print_function

__metaclass__ = type

DOCUMENTATION = r"""
module: configuration_item_batch

author:
  - Manca Bizjak (@mancabizjak)
  - Miha Dolinar (@mdolin)
  - Tadej Borovsak (@tadeboro)

short_description: Manage ServiceNow configuration items in batch mode

description:
  - Create, update ServiceNow configuration items in batch mode.
  - For more information, refer to the ServiceNow configuration management documentation at
    U(https://docs.servicenow.com/bundle/tokyo-servicenow-platform/page/product/configuration-management/reference/cmdb-table-property-descriptions.html).
version_added: 1.2.0
extends_documentation_fragment:
  - servicenow.itsm.instance

seealso:
  - module: servicenow.itsm.configuration_item
  - module: servicenow.itsm.configuration_item_info

options:
  sys_class_name:
    description:
      - Table name (configuration item type) that we would like to manipulate.
    required: true
    type: str
  id_column_set:
    description:
      - Columns that should be used to identify an existing record that we need to update.
    required: true
    type: list
    elements: str
  dataset:
    description:
      - List of dictionaries that will be used as a data source.
      - Each item in a list represents one CMDB item.
    required: true
    type: list
    elements: dict
  map:
    description:
      - Transformation instructions on how to convert input data to CMDB items.
      - Keys represent the CMDB item column names and the values are Jinja expressions
        that extract the value from the source data.
      - Data is returned as string because ServiceNow API expect this
    required: true
    type: dict
"""

EXAMPLES = r"""
- name: Update CMDB with some data
  servicenow.itsm.configuration_item_batch:
    sys_class_name: cmdb_ci_ec2_instance
    id_column_set: vm_inst_id
    dataset:
      - instance_id: 12345
        public_ip_address: 1.2.3.4
        tags:
          Name: my_name
      - instance_id: 54321
        public_ip_address: 4.3.2.1
        tags:
          Name: other_name
    map:
      vm_inst_id: instance_id
      ip_address: public_ip_address
      name: tags.Name

- name: Identify CMDB item using combination of two columns
  servicenow.itsm.configuration_item_batch:
    sys_class_name: cmdb_ci_server
    id_column_set:
      - name
      - ip_address
    dataset: "{{ input_data }}"
    map:
      name: tags.Name
      ip_address: private_ip_address
"""


RETURN = r"""
records:
  description:
    - A list of configuration item records.
    - Note that the fields of the returned records depend on the configuration
      item's I(sys_class_name).
    - Returning of values added in version 2.0.0.
  returned: success
  type: list
  sample:
    - skip_sync: 'false'
      assignment_group: ''
      managed_by: ''
      sys_updated_on: '2022-03-18 03:59:41'
      sys_class_name: cmdb_ci_computer
      manufacturer:
        link: https://www.example.com/api/now/table/core_company/b7e9e843c0a80169009a5a485bb2a2b5
        value: b7e9e843c0a80169009a5a485bb2a2b5
      sys_id: 00a96c0d3790200044e0bfc8bcbe5db4
      po_number: PO100003
      sys_updated_by: system
      due_in: ''
      checked_in: ''
      sys_class_path: "/!!/!2/!("
      sys_created_on: '2012-02-18 08:14:21'
      vendor:
        link: https://www.example.com/api/now/table/core_company/b7e9e843c0a80169009a5a485bb2a2b5
        value: b7e9e843c0a80169009a5a485bb2a2b5
      sys_domain:
        link: https://www.example.com/api/now/table/sys_user_group/global
        value: global
      company:
        link: https://www.example.com/api/now/table/core_company/81fbfe03ac1d55eb286d832de58ae1fd
        value: 81fbfe03ac1d55eb286d832de58ae1fd
      install_date: '2019-07-28 07:00:00'
      justification: ''
      department:
        link: https://www.example.com/api/now/table/cmn_department/221f79b7c6112284005d646b76ab978c
        value: 221f79b7c6112284005d646b76ab978c
      gl_account: ''
      invoice_number: ''
      sys_created_by: admin
      assigned_to:
        link: https://www.example.comapi/now/table/sys_user/92826bf03710200044e0bfc8bcbe5dbb
        value: 92826bf03710200044e0bfc8bcbe5dbb
      warranty_expiration: '2022-07-27'
      asset_tag: P1000503
      cost: '1799.99'
      sys_mod_count: '6'
      owned_by: ''
      serial_number: ABE-486-V17263-DO
      checked_out: ''
      model_id:
        link: https://www.example.com/api/now/table/cmdb_model/d501454f1b1310002502fbcd2c071334
        value: d501454f1b1310002502fbcd2c071334
      sys_domain_path: "/"
      sys_tags: ''
      cost_cc: USD
      order_date: '2019-05-13 08:00:00'
      support_group: ''
      delivery_date: '2019-06-09 08:00:00'
      install_status: '1'
      cost_center:
        link: https://www.example.com/api/now/table/cmn_cost_center/d9d0a971c0a80a641c20b13d99a48576
        value: d9d0a971c0a80a641c20b13d99a48576
      due: ''
      supported_by: ''
      name: MacBook Pro 15"
      unverified: 'false'
      assigned: '2019-11-10 07:00:00'
      location:
        link: https://www.example.com/api/now/table/cmn_location/8226baa4ac1d55eb40eb653c02649519
        value: 8226baa4ac1d55eb40eb653c02649519
      asset:
        link: https://www.example.com/api/now/table/alm_asset/04a96c0d3790200044e0bfc8bcbe5db3
        value: 04a96c0d3790200044e0bfc8bcbe5db3
      purchase_date: '2019-05-25'
      lease_id: ''
"""


from ansible.module_utils.basic import AnsibleModule

from ..module_utils import arguments, client, errors, table, utils


def update(module, table_client):
    changed = False
    cmdb_table = module.params["sys_class_name"]
    id_column_set = module.params["id_column_set"]

    results = []
    for desired in module.params["dataset"]:
        query = dict((c, desired[c]) for c in id_column_set)
        current = table_client.get_record(cmdb_table, query)

        if not current:
            result = table_client.create_record(cmdb_table, desired, module.check_mode)
            results.append(result)
            changed = True
            continue

        if utils.is_superset(current, desired):
            results.append(current)
            continue

        result = table_client.update_record(
            cmdb_table, current, desired, module.check_mode
        )

        results.append(result)
        changed = True

    return results, changed


def main():
    module_args = dict(
        arguments.get_spec("instance"),
        sys_class_name=dict(
            type="str",
            required=True,
        ),
        id_column_set=dict(
            type="list",
            elements="str",
            required=True,
        ),
        dataset=dict(
            type="list",
            elements="dict",
            required=True,
        ),
        map=dict(
            type="dict",
            required=True,
        ),
    )

    module = AnsibleModule(
        argument_spec=module_args,
        supports_check_mode=True,
    )

    if not module.params["id_column_set"]:
        module.fail_json(msg="id_column_set should not be empty")

    try:
        snow_client = client.Client(**module.params["instance"])
        table_client = table.TableClient(snow_client)
        results, changed = update(module, table_client)
        module.exit_json(changed=changed, records_raw=results)
    except errors.ServiceNowError as e:
        module.fail_json(msg=str(e))


if __name__ == "__main__":
    main()
