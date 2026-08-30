#!/usr/bin/python
# Copyright (c) 2016 IBM
# GNU General Public License v3.0+ (see COPYING or https://www.gnu.org/licenses/gpl-3.0.txt)

DOCUMENTATION = '''
---
module: identity_role
short_description: Manage OpenStack Identity Roles
author: OpenStack Ansible SIG
description:
    - Manage OpenStack Identity Roles.
options:
   name:
     description:
        - Role Name
     required: true
     type: str
   state:
     description:
       - Should the resource be present or absent.
     choices: [present, absent]
     default: present
     type: str
requirements:
    - "python >= 3.6"
    - "openstacksdk"

extends_documentation_fragment:
- openstack.cloud.openstack
'''

EXAMPLES = '''
# Create a role named "demo"
- openstack.cloud.identity_role:
    cloud: mycloud
    state: present
    name: demo

# Delete the role named "demo"
- openstack.cloud.identity_role:
    cloud: mycloud
    state: absent
    name: demo
'''

RETURN = '''
role:
    description: Dictionary describing the role.
    returned: On success when I(state) is 'present'.
    type: complex
    contains:
        domain_id:
            description: Domain to which the role belongs
            type: str
            sample: default
        id:
            description: Unique role ID.
            type: str
            sample: "677bfab34c844a01b88a217aa12ec4c2"
        name:
            description: Role name.
            type: str
            sample: "demo"
'''

from ansible_collections.openstack.cloud.plugins.module_utils.openstack import OpenStackModule


class IdentityRoleModule(OpenStackModule):
    argument_spec = dict(
        name=dict(required=True),
        state=dict(default='present', choices=['absent', 'present']),
    )

    module_kwargs = dict(
        supports_check_mode=True
    )

    def _system_state_change(self, state, role):
        if state == 'present' and not role:
            return True
        if state == 'absent' and role:
            return True
        return False

    def run(self):
        name = self.params.get('name')
        state = self.params.get('state')

        role = self.conn.get_role(name)

        if self.ansible.check_mode:
            self.exit_json(changed=self._system_state_change(state, role))

        changed = False
        if state == 'present':
            if role is None:
                role = self.conn.create_role(name=name)
                changed = True
            self.exit_json(changed=changed, role=role)
        elif state == 'absent' and role is not None:
            self.conn.identity.delete_role(role['id'])
            changed = True
        self.exit_json(changed=changed)


def main():
    module = IdentityRoleModule()
    module()


if __name__ == '__main__':
    main()
