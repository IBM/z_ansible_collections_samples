#!/usr/bin/python
# -*- coding: utf-8 -*-
# Copyright: (c) 2024, Red Hat
#
# GNU General Public License v3.0+ (see COPYING or https://www.gnu.org/licenses/gpl-3.0.txt)


from __future__ import absolute_import, division, print_function

__metaclass__ = type

DOCUMENTATION = r"""
module: configuration_item_relations

author:
  - Cosmin Tupangiu (@tupyy)

short_description: Manage ServiceNow relations between configuration items

description:
  - Add and remove ServiceNow relations between configuration items.

version_added: 2.5.0

extends_documentation_fragment:
  - servicenow.itsm.instance
  - servicenow.itsm.sysparm_display_value

seealso:
  - module: servicenow.itsm.configuration_item
  - module: servicenow.itsm.configuration_item_info

options:
  state:
    description:
      - State of the relation.
    type: str
    choices: [ present, absent ]
    default: present
  name:
    description:
      - The name of the relation.
      - Mutually exclusive with C(sys_id).
    type: str
  direction:
    description:
      - Direction of the relation.
    type: str
    choices: [ inbound, outbound ]
    default: outbound
  parent_sys_id:
    description:
      - The sys_id of the configuration item who own the relation.
    type: str
    required: true
  parent_classname:
    description:
      - The class of the configuration item.
    type: str
    required: true
  targets:
    description:
      - List of configuration items to be associated with the parent.
    type: list
    elements: dict
    suboptions:
      name:
        description:
          - Name of the configuration item
        type: str
        required: true
      sys_id:
        description:
          - Sys_id of the configuration item
        type: str
        required: true
    required: true
"""

EXAMPLES = r"""
- name: Create relation between two ci
  servicenow.itsm.configuration_item_relations:
    name: Depends_On
    direction: outbound
    state: present
    parent_sys_id: "{{ parent_sys_id }}"
    parent_classname: cmdb_ci_linux_server
    targets:
      - name: target1
        sys_id: target1_id

- name: Remove relation
  servicenow.itsm.configuration_item_relations:
    direction: outbound
    state: absent
    parent_sys_id: "{{ parent_sys_id }}"
    parent_classname: cmdb_ci_linux_server
    targets:
      - name: target1
        sys_id: target1_id

- name: Update relation by adding one more target
  servicenow.itsm.configuration_item_relations:
    name: Depends_On
    direction: outbound
    state: present
    parent_sys_id: "{{ owner_sys_id }}"
    parent_classname: cmdb_ci_linux_server
    targets:
      - name: target1
        sys_id: target1_id
"""

RETURN = r"""
record:
  description:
    - The relations of the configuration item.
  returned: success
  type: dict
  sample:
    "inbound_relations": ""
    "outbound_relations":
      - "sys_id": "06d7f70697514210d8a379100153af3d"
        "target":
          "display_value": "PS LinuxApp01"
          "value": "3a290cc60a0a0bb400000bdb386af1cf"
        "type":
          "display_value": "Cools::Cooled By"
          "value": "015633570a0a0bc70029121512d46ede"
"""

from ..module_utils.utils import get_mapper
from ..module_utils.configuration_item import PAYLOAD_FIELDS_MAPPING
from ..module_utils import cmdb_relation as cmdb
from ..module_utils import arguments, client, errors, generic
from ansible.module_utils.basic import AnsibleModule

CMDB_INSTANCE_BASE_API_PATH = "api/now/cmdb/instance"
CMDB_RELATION_TYPE_API_PATH = "/api/now/table/cmdb_rel_type"


def ensure_present(module, generic_client):
    mapper = get_mapper(
        module,
        "configuration_item_mapping",
        PAYLOAD_FIELDS_MAPPING,
        sysparm_display_value=module.params["sysparm_display_value"],
    )

    # get the relation type sys_id
    relation_records = generic_client.list_records(
        CMDB_RELATION_TYPE_API_PATH, dict(sys_name=module.params["name"])
    )
    if len(relation_records) == 0:
        raise errors.ServiceNowError(
            "Error finding relation by sys_name {0}".format(module.params["name"])
        )

    relation_type_sys_id = relation_records[0]["sys_id"]

    parent_ci = mapper.to_ansible(
        generic_client.get_by_sys_id(
            "/".join([CMDB_INSTANCE_BASE_API_PATH, module.params["parent_classname"]]),
            module.params["parent_sys_id"],
            True,
        )
    )

    relations = cmdb.CmdbItemRelations(parent_ci)

    changed = False
    for target in module.params["targets"]:
        existing_relation = relations.get(module.params["direction"], target["sys_id"])
        if not existing_relation:
            relations.add(
                module.params["direction"],
                cmdb.CmdbRelation.from_values(
                    relation_type_sys_id,
                    module.params["name"],
                    target["sys_id"],
                    target["name"],
                ),
            )
            changed = True

    if changed:
        new_relations = relations.update(
            "/".join(
                [
                    CMDB_INSTANCE_BASE_API_PATH,
                    module.params["parent_classname"],
                    module.params["parent_sys_id"],
                    "relation",
                ]
            ),
            generic_client,
            module.check_mode,
        )

        return (
            True,
            new_relations.to_json(),
            dict(before=relations.to_json(), after=new_relations.to_json()),
        )

    return (
        False,
        relations.to_json(),
        dict(
            before=dict(record=relations.to_json()),
            after=dict(record=relations.to_json()),
        ),
    )


def ensure_absent(module, generic_client):
    mapper = get_mapper(
        module,
        "configuration_item_mapping",
        PAYLOAD_FIELDS_MAPPING,
        sysparm_display_value=module.params["sysparm_display_value"],
    )

    parent_ci = mapper.to_ansible(
        generic_client.get_by_sys_id(
            "/".join([CMDB_INSTANCE_BASE_API_PATH, module.params["parent_classname"]]),
            module.params["parent_sys_id"],
            True,
        )
    )

    relations = cmdb.CmdbItemRelations(parent_ci)

    changed = False
    for target in module.params["targets"]:
        existing_relation = relations.get(module.params["direction"], target["sys_id"])
        if existing_relation:
            relations.remove(module.params["direction"], existing_relation)
            changed = True

    if changed:
        new_relations = relations.update(
            "/".join(
                [
                    CMDB_INSTANCE_BASE_API_PATH,
                    module.params["parent_classname"],
                    module.params["parent_sys_id"],
                    "relation",
                ]
            ),
            generic_client,
            module.check_mode,
        )
        return (
            True,
            new_relations.to_json(),
            dict(before=relations.to_json(), after=new_relations.to_json()),
        )

    return (
        False,
        relations.to_json(),
        dict(
            before=relations.to_json(),
            after=relations.to_json(),
        ),
    )


def run(module, generic_client):
    if module.params["state"] == "absent":
        return ensure_absent(module, generic_client)
    return ensure_present(module, generic_client)


def main():
    module_args = dict(
        arguments.get_spec(
            "instance",
            "sysparm_display_value",
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
        direction=dict(
            type="str",
            choices=[
                "inbound",
                "outbound",
            ],
            default="outbound",
        ),
        parent_sys_id=dict(
            type="str",
            required=True,
        ),
        parent_classname=dict(
            type="str",
            required=True,
        ),
        targets=dict(
            type="list",
            elements="dict",
            options=dict(
                name=dict(
                    type="str",
                    required=True,
                ),
                sys_id=dict(
                    type="str",
                    required=True,
                ),
            ),
            required=True,
        ),
    )

    module = AnsibleModule(
        argument_spec=module_args,
        supports_check_mode=True,
        required_if=[("state", "present", ("name",))],
    )

    try:
        snow_client = client.Client(**module.params["instance"])
        generic_client = generic.GenericClient(snow_client)
        changed, record, diff = run(module, generic_client)
        module.exit_json(changed=changed, record=record, diff=diff)
    except errors.ServiceNowError as e:
        module.fail_json(msg=str(e))


if __name__ == "__main__":
    main()
