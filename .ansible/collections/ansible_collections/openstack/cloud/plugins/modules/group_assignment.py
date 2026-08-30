#!/usr/bin/python
# Copyright (c) 2015 Hewlett-Packard Development Company, L.P.
# GNU General Public License v3.0+ (see COPYING or https://www.gnu.org/licenses/gpl-3.0.txt)

DOCUMENTATION = '''
---
module: group_assignment
short_description: Associate OpenStack Identity users and groups
author: OpenStack Ansible SIG
description:
    - Add and remove users from groups
options:
   user:
     description:
        - Name or id for the user
     required: true
     type: str
   group:
     description:
        - Name or id for the group.
     required: true
     type: str
   state:
     description:
       - Should the user be present or absent in the group
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
# Add the demo user to the demo group
- openstack.cloud.group_assignment:
  cloud: mycloud
  user: demo
  group: demo
'''

from ansible_collections.openstack.cloud.plugins.module_utils.openstack import OpenStackModule


class IdentityGroupAssignment(OpenStackModule):
    argument_spec = dict(
        user=dict(required=True),
        group=dict(required=True),
        state=dict(default='present', choices=['absent', 'present']),
    )

    module_kwargs = dict(
        supports_check_mode=True
    )

    def _system_state_change(self, state, in_group):
        if state == 'present' and not in_group:
            return True
        if state == 'absent' and in_group:
            return True
        return False

    def run(self):
        user = self.params['user']
        group = self.params['group']
        state = self.params['state']

        in_group = self.conn.is_user_in_group(user, group)

        if self.ansible.check_mode:
            self.exit_json(changed=self._system_state_change(state, in_group))

        changed = False
        if state == 'present':
            if not in_group:
                self.conn.add_user_to_group(user, group)
                changed = True

        elif state == 'absent':
            if in_group:
                self.conn.remove_user_from_group(user, group)
                changed = True

        self.exit_json(changed=changed)


def main():
    module = IdentityGroupAssignment()
    module()


if __name__ == '__main__':
    main()
