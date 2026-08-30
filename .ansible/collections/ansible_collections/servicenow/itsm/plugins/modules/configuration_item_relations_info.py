#!/usr/bin/python
# -*- coding: utf-8 -*-
# Copyright: (c) 2024, Red Hat
#
# GNU General Public License v3.0+ (see COPYING or https://www.gnu.org/licenses/gpl-3.0.txt)


from __future__ import absolute_import, division, print_function

__metaclass__ = type

DOCUMENTATION = r"""
module: configuration_item_relations_info

author:
  - Cosmin Tupangiu (@tupyy)

short_description: Retreive ServiceNow relations of configuration items

description:
  - Retreive configuration items relations

version_added: 2.5.0

extends_documentation_fragment:
  - servicenow.itsm.instance
  - servicenow.itsm.sys_id.info
  - servicenow.itsm.sysparm_display_value

options:
  classname:
    description:
      - The class of the configuration item.
    type: str
    required: true
seealso:
  - module: servicenow.itsm.configuration_item_relations

"""

EXAMPLES = r"""
- name: Retreive relations of a configuration item
  servicenow.itsm.configuration_item_relations_info:
    sys_id: "{{ configuration_item_sys_id }}"
    classname: cmdb_ci_linux_server
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

from ansible.module_utils.basic import AnsibleModule
from ..module_utils import arguments, client, errors, generic
from ..module_utils import cmdb_relation as cmdb
from ..module_utils.configuration_item import PAYLOAD_FIELDS_MAPPING
from ..module_utils.utils import get_mapper

CMDB_INSTANCE_BASE_API_PATH = "api/now/cmdb/instance"


def run(module, generic_client):
    mapper = get_mapper(
        module,
        "configuration_item_mapping",
        PAYLOAD_FIELDS_MAPPING,
        sysparm_display_value=module.params["sysparm_display_value"],
    )

    ci = mapper.to_ansible(
        generic_client.get_by_sys_id(
            "/".join([CMDB_INSTANCE_BASE_API_PATH, module.params["classname"]]),
            module.params["sys_id"],
            True,
        )
    )

    return cmdb.CmdbItemRelations(ci).to_json()


def main():
    module_args = dict(
        arguments.get_spec(
            "instance",
            "sys_id",
            "sysparm_display_value",
        ),
        classname=dict(
            type="str",
            required=True,
        ),
    )

    module = AnsibleModule(
        argument_spec=module_args,
        supports_check_mode=True,
    )

    try:
        snow_client = client.Client(**module.params["instance"])
        generic_client = generic.GenericClient(snow_client)
        records = run(module, generic_client)
        module.exit_json(changed=False, record=records)
    except errors.ServiceNowError as e:
        module.fail_json(msg=str(e))


if __name__ == "__main__":
    main()
