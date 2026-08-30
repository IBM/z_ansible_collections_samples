#!/usr/bin/python
# coding: utf-8 -*-
#
# Copyright (c) 2020 by Open Telekom Cloud, operated by T-Systems International GmbH
# GNU General Public License v3.0+ (see COPYING or https://www.gnu.org/licenses/gpl-3.0.txt)

DOCUMENTATION = '''
---
module: security_group_info
short_description: Lists security groups
extends_documentation_fragment: openstack.cloud.openstack
author: OpenStack Ansible SIG
description:
  - List security groups
options:
  description:
    description:
      - Description of the security group
    type: str
  name:
    description:
      - Name or id of the security group.
    type: str
  project_id:
    description:
      - Specifies the project id as filter criteria
    type: str
  revision_number:
    description:
      - Filter the list result by the revision number of the
      - resource.
    type: int
  tags:
    description:
      - A list of tags to filter the list result by.
      - Resources that match all tags in this list will be returned.
    type: list
    elements: str
  any_tags:
    description:
      - A list of tags to filter the list result by.
      - Resources that match any tag in this list will be returned.
    type: list
    elements: str
  not_tags:
    description:
      - A list of tags to filter the list result by.
      - Resources that match all tags in this list will be excluded.
    type: list
    elements: str
  not_any_tags:
    description:
      - A list of tags to filter the list result by.
      - Resources that match any tag in this list will be excluded.
    type: list
    elements: str

requirements: ["openstacksdk"]
'''

RETURN = '''
security_groups:
  description: List of dictionaries describing security groups.
  type: complex
  returned: On Success.
  contains:
    created_at:
      description: Creation time of the security group
      type: str
      sample: "yyyy-mm-dd hh:mm:ss"
    description:
      description: Description of the security group
      type: str
      sample: "My security group"
    id:
      description: ID of the security group
      type: str
      sample: "d90e55ba-23bd-4d97-b722-8cb6fb485d69"
    name:
      description: Name of the security group.
      type: str
      sample: "my-sg"
    project_id:
      description: Project ID where the security group is located in.
      type: str
      sample: "25d24fc8-d019-4a34-9fff-0a09fde6a567"
    security_group_rules:
      description: Specifies the security group rule list
      type: list
      sample: [
        {
          "id": "d90e55ba-23bd-4d97-b722-8cb6fb485d69",
          "direction": "ingress",
          "protocol": null,
          "ethertype": "IPv4",
          "description": null,
          "remote_group_id": "0431c9c5-1660-42e0-8a00-134bec7f03e2",
          "remote_ip_prefix": null,
          "tenant_id": "bbfe8c41dd034a07bebd592bf03b4b0c",
          "port_range_max": null,
          "port_range_min": null,
          "security_group_id": "0431c9c5-1660-42e0-8a00-134bec7f03e2"
        },
        {
          "id": "aecff4d4-9ce9-489c-86a3-803aedec65f7",
          "direction": "egress",
          "protocol": null,
          "ethertype": "IPv4",
          "description": null,
          "remote_group_id": null,
          "remote_ip_prefix": null,
          "tenant_id": "bbfe8c41dd034a07bebd592bf03b4b0c",
          "port_range_max": null,
          "port_range_min": null,
          "security_group_id": "0431c9c5-1660-42e0-8a00-134bec7f03e2"
        }
      ]
    updated_at:
      description: Update time of the security group
      type: str
      sample: "yyyy-mm-dd hh:mm:ss"
'''

EXAMPLES = '''
# Get specific security group
- openstack.cloud.security_group_info:
    cloud: "{{ cloud }}"
    name: "{{ my_sg }}"
  register: sg
# Get all security groups
- openstack.cloud.security_group_info:
    cloud: "{{ cloud }}"
  register: sg
'''

from ansible_collections.openstack.cloud.plugins.module_utils.openstack import (
    OpenStackModule)


class SecurityGroupInfoModule(OpenStackModule):
    argument_spec = dict(
        description=dict(required=False, type='str'),
        name=dict(required=False, type='str'),
        project_id=dict(required=False, type='str'),
        revision_number=dict(required=False, type='int'),
        tags=dict(required=False, type='list', elements='str'),
        any_tags=dict(required=False, type='list', elements='str'),
        not_tags=dict(required=False, type='list', elements='str'),
        not_any_tags=dict(required=False, type='list', elements='str')
    )
    module_kwargs = dict(
        supports_check_mode=True
    )

    def run(self):
        description = self.params['description']
        name = self.params['name']
        project_id = self.params['project_id']
        revision_number = self.params['revision_number']
        tags = self.params['tags']
        any_tags = self.params['any_tags']
        not_tags = self.params['not_tags']
        not_any_tags = self.params['not_any_tags']

        attrs = {}

        if description:
            attrs['description'] = description
        if project_id:
            attrs['project_id'] = project_id
        if revision_number:
            attrs['revision_number'] = revision_number
        if tags:
            attrs['tags'] = ','.join(tags)
        if any_tags:
            attrs['any_tags'] = ','.join(any_tags)
        if not_tags:
            attrs['not_tags'] = ','.join(not_tags)
        if not_any_tags:
            attrs['not_any_tags'] = ','.join(not_any_tags)

        attrs = self.check_versioned(**attrs)
        result = self.conn.network.security_groups(**attrs)
        result = [item if isinstance(item, dict) else item.to_dict() for item in result]
        if name:
            result = [item for item in result if name in (item['id'], item['name'])]
        self.results.update({'security_groups': result})


def main():
    module = SecurityGroupInfoModule()
    module()


if __name__ == "__main__":
    main()
