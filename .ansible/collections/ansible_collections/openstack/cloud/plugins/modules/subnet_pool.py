#!/usr/bin/python
# coding: utf-8 -*-
#
# Copyright (c) 2021 by Uemit Seren <uemit.seren@gmail.com>
# GNU General Public License v3.0+ (see COPYING or https://www.gnu.org/licenses/gpl-3.0.txt)

DOCUMENTATION = '''
---
module: subnet_pool
short_description: Create or delete subnet pools from OpenStack
author: OpenStack Ansible SIG
description:
   - Create or Delete subnet pools from OpenStack.
options:
   state:
     description:
        - Indicate desired state of the resource
     choices: ['present', 'absent']
     default: present
     type: str
   name:
     description:
        - Name to be give to the subnet pool
     required: true
     type: str
   project:
     description:
        - Unique name or ID of the project.
     type: str
   prefixes:
     description:
        - Set subnet pool prefixes (in CIDR notation)
     type: list
     elements: str
   minimum_prefix_length:
     description:
        - The minimum prefix length that can be allocated from the subnet pool.
     required: False
     type: int
   maximum_prefix_length:
     description:
        - The maximum prefix length that can be allocated from the subnet pool.
     required: False
     type: int
   default_prefix_length:
     description:
        - The length of the prefix to allocate when the cidr or prefixlen attributes
          are omitted when creating a subnet
     type: int
     required: False
   address_scope:
     description:
        - Set address scope (ID or name) associated with the subnet pool
     type: str
     required: False
   is_default:
     description:
        - Whether this subnet pool is by default
     type: bool
     default: 'no'
   description:
     description: The subnet pool description
     type: str
     required: False
   default_quota:
     description:
        - A per-project quota on the prefix space that can be allocated
          from the subnet pool for project subnets
     required: False
     type: int
   shared:
     description:
        - Whether this subnet pool is shared or not.
     type: bool
     default: 'no'
   extra_specs:
     description:
        - Dictionary with extra key/value pairs passed to the API
     required: false
     default: {}
     type: dict
requirements:
    - "python >= 3.6"
    - "openstacksdk"

extends_documentation_fragment:
- openstack.cloud.openstack
'''

EXAMPLES = '''
# Create an subnet pool.
- openstack.cloud.subnet_pool:
    cloud: mycloud
    state: present
    name: my_subnet_pool
    prefixes:
        - 10.10.10.0/24

# Create a subnet pool for a given project.
- openstack.cloud.subnet_pool:
    cloud: mycloud
    state: present
    name: my_subnet_pool
    project: myproj
    prefixes:
        - 10.10.10.0/24

# Create a shared and default subnet pool in existing address scope
- openstack.cloud.subnet_pool:
    cloud: mycloud
    state: present
    name: my_subnet_pool
    address_scope: my_adress_scope
    is_default: True
    default_quota: 10
    maximum_prefix_length: 32
    minimum_prefix_length: 8
    default_prefix_length: 24
    shared: True
    prefixes:
        - 10.10.10.0/8

# Delete subnet poool.
- openstack.cloud.subnet_pool:
    cloud: mycloud
    state: absent
    name: my_subnet_pool
'''

RETURN = '''
subnet_pool:
    description: Dictionary describing the subnet pool.
    returned: On success when I(state) is 'present'
    type: complex
    contains:
        id:
            description: Subnet Pool ID.
            type: str
            sample: "474acfe5-be34-494c-b339-50f06aa143e4"
        name:
            description: Subnet Pool name.
            type: str
            sample: "my_subnet_pool"
        project_id:
            description: The ID of the project.
            type: str
            sample: "861174b82b43463c9edc5202aadc60ef"
        ip_version:
            description: The IP version of the subnet pool 4 or 6.
            type: int
            sample: 4
        is_shared:
            description: Indicates whether this subnet pool is shared across all projects.
            type: bool
            sample: false
        is_default:
            description: Indicates whether this is the default subnet pool.
            type: bool
            sample: false
        address_scope_id:
            description: The address scope ID.
            type: str
            sample: "861174b82b43463c9edc5202aadc60ef"
        created_at:
            description: Timestamp when the subnet pool was created.
            type: str
            sample: ""
        default_prefix_length:
            description:
                - The length of the prefix to allocate when the cidr or prefixlen
                  attributes are omitted when creating a subnet
            type: int
            sample: 32
        default_quota:
            description:
                - The per-project quota on the prefix space that can be allocated
                  from the subnet pool for project subnets.
            type: int
            sample: 22
        description:
            description: The subnet pool description.
            type: str
            sample: "My test subnet pool."
        maximum_prefix_length:
            description: The maximum prefix length that can be allocated from the subnet pool.
            type: int
            sample: 22
        minimum_prefix_length:
            description: The minimum prefix length that can be allocated from the subnet pool.
            type: int
            sample: 8
        prefixes:
            description: A list of subnet prefixes that are assigned to the subnet pool.
            type: list
            sample: ['10.10.20.0/24', '10.20.10.0/24']
        revision_number:
            description: Revision number of the subnet pool.
            type: int
            sample: 5
        updated_at:
            description: Timestamp when the subnet pool was last updated.
            type: str
            sample:
'''

from ansible_collections.openstack.cloud.plugins.module_utils.openstack import OpenStackModule


class SubnetPoolModule(OpenStackModule):
    argument_spec = dict(
        state=dict(default='present', choices=['absent', 'present']),
        name=dict(required=True),
        shared=dict(default=False, type='bool'),
        minimum_prefix_length=dict(default=None, type='int'),
        maximum_prefix_length=dict(default=None, type='int'),
        default_prefix_length=dict(default=None, type='int'),
        description=dict(default=None, type='str'),
        default_quota=dict(default=None, type='int'),
        prefixes=dict(type='list', elements='str'),
        is_default=dict(default=False, type='bool'),
        address_scope=dict(default=None),
        project=dict(default=None),
        extra_specs=dict(type='dict', default=dict())
    )

    def _needs_update(self, subnet_pool):
        """Check for differences in the updatable values.

        NOTE: We don't currently allow name updates.
        """
        compare_simple = ['is_default',
                          'minimum_prefix_length',
                          'maximum_prefix_length',
                          'default_prefix_length',
                          'description',
                          'default_quota']
        compare_list = ['prefixes']

        for key in compare_simple:
            if self.params[key] is not None and self.params[key] != subnet_pool[key]:
                return True
        for key in compare_list:
            if (
                self.params[key] is not None
                and set(self.params[key]) != set(subnet_pool[key])
            ):
                return True

        return False

    def _system_state_change(self, subnet_pool, filters=None):
        """Check if the system state would be changed."""
        state = self.params['state']
        if state == 'absent' and subnet_pool:
            return True
        if state == 'present':
            if not subnet_pool:
                return True
            return self._needs_update(subnet_pool, filters)
        return False

    def _compose_subnet_pool_args(self):
        subnet_pool_kwargs = {}
        optional_parameters = ['name',
                               'minimum_prefix_length',
                               'maximum_prefix_length',
                               'default_prefix_length',
                               'description',
                               'is_default',
                               'default_quota',
                               'prefixes']

        for optional_param in optional_parameters:
            if self.params[optional_param] is not None:
                subnet_pool_kwargs[optional_param] = self.params[optional_param]

        return subnet_pool_kwargs

    def run(self):

        state = self.params['state']
        name = self.params['name']
        project = self.params['project']
        address_scope = self.params['address_scope']
        extra_specs = self.params['extra_specs']

        if project is not None:
            proj = self.conn.get_project(project)
            if proj is None:
                self.fail(msg='Project %s could not be found' % project)
            project_id = proj['id']
        else:
            project_id = self.conn.current_project_id

        address_scope_id = None
        if address_scope is not None:
            address_scope = self.conn.network.find_address_scope(name_or_id=address_scope)
            if address_scope is None:
                self.fail(msg='AddressScope %s could not be found' % address_scope)
            address_scope_id = address_scope['id']
        subnet_pool = self.conn.network.find_subnet_pool(name_or_id=name)
        if self.ansible.check_mode:
            self.exit_json(
                changed=self._system_state_change(subnet_pool)
            )

        if state == 'present':
            changed = False

            if not subnet_pool:
                kwargs = self._compose_subnet_pool_args()
                kwargs['address_scope_id'] = address_scope_id
                kwargs['project_id'] = project_id
                kwargs['is_shared'] = self.params['shared']
                dup_args = set(kwargs.keys()) & set(extra_specs.keys())
                if dup_args:
                    raise ValueError('Duplicate key(s) {0} in extra_specs'
                                     .format(list(dup_args)))
                kwargs = dict(kwargs, **extra_specs)
                subnet_pool = self.conn.network.create_subnet_pool(**kwargs)
                changed = True
            else:
                if self._needs_update(subnet_pool):
                    kwargs = self._compose_subnet_pool_args()
                    subnet_pool = self.conn.network.update_subnet_pool(subnet_pool['id'], **kwargs)
                    changed = True
                else:
                    changed = False
            self.exit_json(changed=changed, subnet_pool=subnet_pool, id=subnet_pool['id'])

        elif state == 'absent':
            if not subnet_pool:
                self.exit(changed=False)
            else:
                self.conn.network.delete_subnet_pool(subnet_pool['id'])
                self.exit_json(changed=True)


def main():
    module = SubnetPoolModule()
    module()


if __name__ == '__main__':
    main()
