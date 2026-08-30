#!/usr/bin/python
# coding: utf-8 -*-

# Copyright (c) 2020, Sagi Shnaidman <sshnaidm@redhat.com>
# GNU General Public License v3.0+ (see COPYING or https://www.gnu.org/licenses/gpl-3.0.txt)

DOCUMENTATION = '''
---
module: stack_info
short_description: Retrive information about Heat stacks
author: OpenStack Ansible SIG
description:
  - Get information about Heat stack in openstack
options:
  name:
    description:
    - Name of the stack as a string.
    type: str
    required: false
  status:
    description:
    - Value of the status of the stack so that you can filter on "available" for example
    type: str
    required: false
  project_id:
    description:
      - Project ID to be used as filter
    type: str
    required: false
  owner_id:
    description:
      - Owner (parent) of the stack to be used as a filter
    type: str
    required: false

requirements:
  - "python >= 3.6"
  - "openstacksdk"

extends_documentation_fragment:
  - openstack.cloud.openstack
'''

RETURN = '''
stacks:
    description: List of dictionaries describing stacks.
    type: list
    elements: dict
    returned: always.
    contains:
        id:
            description: Unique UUID.
            type: str
            sample: "39007a7e-ee4f-4d13-8283-b4da2e037c69"
        status:
            description: Stack status.
            type: str

'''

EXAMPLES = '''
# Get backups.
- openstack.cloud.stack_info:
  register: stack

- openstack.cloud.stack_info:
    name: my_stack
  register: stack
'''

from ansible_collections.openstack.cloud.plugins.module_utils.openstack import OpenStackModule


class StackInfoModule(OpenStackModule):
    module_min_sdk_version = '0.53.0'

    argument_spec = dict(
        name=dict(required=False, type='str'),
        status=dict(required=False, type='str'),
        project_id=dict(required=False, type='str'),
        owner_id=dict(required=False, type='str')
    )
    module_kwargs = dict(
        supports_check_mode=True
    )

    def run(self):
        data = []
        attrs = {}

        for param in ['name', 'status', 'project_id', 'owner_id']:
            if self.params[param]:
                attrs[param] = self.params[param]

        for raw in self.conn.orchestration.stacks(**attrs):
            dt = raw.to_dict()
            dt.pop('location')
            data.append(dt)

        self.exit_json(
            changed=False,
            stacks=data
        )


def main():
    module = StackInfoModule()
    module()


if __name__ == '__main__':
    main()
