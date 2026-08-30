#!/usr/bin/python
# Copyright (c) 2016 IBM
# GNU General Public License v3.0+ (see COPYING or https://www.gnu.org/licenses/gpl-3.0.txt)

DOCUMENTATION = '''
---
module: identity_group
short_description: Manage OpenStack Identity Groups
author: OpenStack Ansible SIG
description:
    - Manage OpenStack Identity Groups. Groups can be created, deleted or
      updated. Only the I(description) value can be updated.
options:
   name:
     description:
        - Group name
     required: true
     type: str
   description:
     description:
        - Group description
     type: str
   domain_id:
     description:
        - Domain id to create the group in if the cloud supports domains.
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
# Create a group named "demo"
- openstack.cloud.identity_group:
    cloud: mycloud
    state: present
    name: demo
    description: "Demo Group"
    domain_id: demoid

# Update the description on existing "demo" group
- openstack.cloud.identity_group:
    cloud: mycloud
    state: present
    name: demo
    description: "Something else"
    domain_id: demoid

# Delete group named "demo"
- openstack.cloud.identity_group:
    cloud: mycloud
    state: absent
    name: demo
'''

RETURN = '''
group:
    description: Dictionary describing the group.
    returned: On success when I(state) is 'present'.
    type: complex
    contains:
        id:
            description: Unique group ID
            type: str
            sample: "ee6156ff04c645f481a6738311aea0b0"
        name:
            description: Group name
            type: str
            sample: "demo"
        description:
            description: Group description
            type: str
            sample: "Demo Group"
        domain_id:
            description: Domain for the group
            type: str
            sample: "default"
'''

from ansible_collections.openstack.cloud.plugins.module_utils.openstack import OpenStackModule


class IdentityGroupModule(OpenStackModule):
    argument_spec = dict(
        name=dict(required=True),
        description=dict(required=False, default=None),
        domain_id=dict(required=False, default=None),
        state=dict(default='present', choices=['absent', 'present']),
    )

    module_kwargs = dict(
        supports_check_mode=True
    )

    def _system_state_change(self, state, description, group):
        if state == 'present' and not group:
            return True
        if state == 'present' and description is not None and group.description != description:
            return True
        if state == 'absent' and group:
            return True
        return False

    def run(self):
        name = self.params.get('name')
        description = self.params.get('description')
        state = self.params.get('state')

        domain_id = self.params.pop('domain_id')

        if domain_id:
            group = self.conn.get_group(name, filters={'domain_id': domain_id})
        else:
            group = self.conn.get_group(name)

        if self.ansible.check_mode:
            self.exit_json(changed=self._system_state_change(state, description, group))

        if state == 'present':
            if group is None:
                group = self.conn.create_group(
                    name=name, description=description, domain=domain_id)
                changed = True
            else:
                if description is not None and group.description != description:
                    group = self.conn.update_group(
                        group.id, description=description)
                    changed = True
                else:
                    changed = False
            self.exit_json(changed=changed, group=group)

        elif state == 'absent':
            if group is None:
                changed = False
            else:
                self.conn.delete_group(group.id)
                changed = True
            self.exit_json(changed=changed)


def main():
    module = IdentityGroupModule()
    module()


if __name__ == '__main__':
    main()
