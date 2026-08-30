#!/usr/bin/python
# coding: utf-8 -*-
#
# Copyright (c) 2021 by Uemit Seren <uemit.seren@gmail.com>
# GNU General Public License v3.0+ (see COPYING or https://www.gnu.org/licenses/gpl-3.0.txt)

DOCUMENTATION = '''
---
module: address_scope
short_description: Create or delete address scopes from OpenStack
author: OpenStack Ansible SIG
description:
   - Create or Delete address scopes from OpenStack.
options:
   state:
     description:
        - Indicate desired state of the resource
     choices: ['present', 'absent']
     default: present
     type: str
   name:
     description:
        - Name to be give to the address scope
     required: true
     type: str
   project:
     description:
        - Unique name or ID of the project.
     type: str
   ip_version:
     description:
        - The IP version of the subnet 4 or 6
     default: '4'
     type: str
     choices: ['4', '6']
   shared:
     description:
        - Whether this address scope is shared or not.
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
# Create an IPv4 address scope.
- openstack.cloud.address_scope:
    cloud: mycloud
    state: present
    name: my_adress_scope

# Create a shared IPv6 address scope for a given project.
- openstack.cloud.address_scope:
    cloud: mycloud
    state: present
    ip_version: 6
    name: ipv6_address_scope
    project: myproj

# Delete address scope.
- openstack.cloud.address_scope:
    cloud: mycloud
    state: absent
    name: my_adress_scope
'''

RETURN = '''
address_scope:
    description: Dictionary describing the address scope.
    returned: On success when I(state) is 'present'
    type: complex
    contains:
        id:
            description: Address Scope ID.
            type: str
            sample: "474acfe5-be34-494c-b339-50f06aa143e4"
        name:
            description: Address Scope name.
            type: str
            sample: "my_address_scope"
        tenant_id:
            description: The tenant ID.
            type: str
            sample: "861174b82b43463c9edc5202aadc60ef"
        ip_version:
            description: The IP version of the subnet 4 or 6.
            type: str
            sample: "4"
        is_shared:
            description: Indicates whether this address scope is shared across all tenants.
            type: bool
            sample: false

'''

from ansible_collections.openstack.cloud.plugins.module_utils.openstack import OpenStackModule


class AddressScopeModule(OpenStackModule):
    argument_spec = dict(
        state=dict(default='present', choices=['absent', 'present']),
        name=dict(required=True),
        shared=dict(default=False, type='bool'),
        ip_version=dict(type='str', default='4', choices=['4', '6']),
        project=dict(default=None),
        extra_specs=dict(type='dict', default=dict())
    )

    def _needs_update(self, address_scope, filters=None):
        """Decide if the given address_scope needs an update.
        """
        ip_version = int(self.params['ip_version'])
        if address_scope['is_shared'] != self.params['shared']:
            return True
        if ip_version and address_scope['ip_version'] != ip_version:
            self.fail_json(msg='Cannot update ip_version in existing address scope')
        return False

    def _system_state_change(self, address_scope, filters=None):
        """Check if the system state would be changed."""
        state = self.params['state']
        if state == 'absent' and address_scope:
            return True
        if state == 'present':
            if not address_scope:
                return True
            return self._needs_update(address_scope, filters)
        return False

    def run(self):

        state = self.params['state']
        name = self.params['name']
        shared = self.params['shared']
        ip_version = self.params['ip_version']
        project = self.params['project']
        extra_specs = self.params['extra_specs']

        if project is not None:
            proj = self.conn.get_project(project)
            if proj is None:
                self.fail(msg='Project %s could not be found' % project)
            project_id = proj['id']
        else:
            project_id = self.conn.current_project_id

        address_scope = self.conn.network.find_address_scope(name_or_id=name)
        if self.ansible.check_mode:
            self.exit_json(
                changed=self._system_state_change(address_scope)
            )

        if state == 'present':
            changed = False

            if not address_scope:
                kwargs = dict(
                    name=name,
                    ip_version=ip_version,
                    is_shared=shared,
                    tenant_id=project_id)
                dup_args = set(kwargs.keys()) & set(extra_specs.keys())
                if dup_args:
                    raise ValueError('Duplicate key(s) {0} in extra_specs'
                                     .format(list(dup_args)))
                kwargs = dict(kwargs, **extra_specs)
                address_scope = self.conn.network.create_address_scope(**kwargs)
                changed = True
            else:
                if self._needs_update(address_scope):
                    address_scope = self.conn.network.update_address_scope(address_scope['id'], is_shared=shared)
                    changed = True
                else:
                    changed = False
            self.exit_json(changed=changed, address_scope=address_scope, id=address_scope['id'])

        elif state == 'absent':
            if not address_scope:
                self.exit(changed=False)
            else:
                self.conn.network.delete_address_scope(address_scope['id'])
                self.exit_json(changed=True)


def main():
    module = AddressScopeModule()
    module()


if __name__ == '__main__':
    main()
