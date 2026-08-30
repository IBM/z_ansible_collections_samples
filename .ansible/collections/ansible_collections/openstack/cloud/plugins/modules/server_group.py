#!/usr/bin/python

# Copyright (c) 2016 Catalyst IT Limited
# GNU General Public License v3.0+ (see COPYING or https://www.gnu.org/licenses/gpl-3.0.txt)

DOCUMENTATION = '''
---
module: server_group
short_description: Manage OpenStack server groups
author: OpenStack Ansible SIG
description:
   - Add or remove server groups from OpenStack.
options:
   state:
     description:
        - Indicate desired state of the resource. When I(state) is 'present',
          then I(policies) is required.
     choices: ['present', 'absent']
     required: false
     default: present
     type: str
   name:
     description:
        - Server group name.
     required: true
     type: str
   policies:
     description:
        - A list of one or more policy names to associate with the server
          group. The list must contain at least one policy name. The current
          valid policy names are anti-affinity, affinity, soft-anti-affinity
          and soft-affinity.
     required: false
     type: list
     elements: str
requirements:
    - "python >= 3.6"
    - "openstacksdk"

extends_documentation_fragment:
- openstack.cloud.openstack
'''

EXAMPLES = '''
# Create a server group with 'affinity' policy.
- openstack.cloud.server_group:
    state: present
    auth:
      auth_url: https://identity.example.com
      username: admin
      password: admin
      project_name: admin
    name: my_server_group
    policies:
      - affinity

# Delete 'my_server_group' server group.
- openstack.cloud.server_group:
    state: absent
    auth:
      auth_url: https://identity.example.com
      username: admin
      password: admin
      project_name: admin
    name: my_server_group
'''

RETURN = '''
id:
    description: Unique UUID.
    returned: success
    type: str
name:
    description: The name of the server group.
    returned: success
    type: str
policies:
    description: A list of one or more policy names of the server group.
    returned: success
    type: list
members:
    description: A list of members in the server group.
    returned: success
    type: list
metadata:
    description: Metadata key and value pairs.
    returned: success
    type: dict
project_id:
    description: The project ID who owns the server group.
    returned: success
    type: str
user_id:
    description: The user ID who owns the server group.
    returned: success
    type: str
'''

from ansible_collections.openstack.cloud.plugins.module_utils.openstack import OpenStackModule


class ServerGroupModule(OpenStackModule):
    argument_spec = dict(
        name=dict(required=True),
        policies=dict(required=False, type='list', elements='str'),
        state=dict(default='present', choices=['absent', 'present']),
    )

    module_kwargs = dict(
        supports_check_mode=True,
    )

    def _system_state_change(self, state, server_group):
        if state == 'present' and not server_group:
            return True
        if state == 'absent' and server_group:
            return True

        return False

    def run(self):
        name = self.params['name']
        policies = self.params['policies']
        state = self.params['state']

        server_group = self.conn.get_server_group(name)

        if self.ansible.check_mode:
            self.exit_json(
                changed=self._system_state_change(state, server_group)
            )

        changed = False
        if state == 'present':
            if not server_group:
                if not policies:
                    self.fail_json(
                        msg="Parameter 'policies' is required in Server Group "
                            "Create"
                    )
                server_group = self.conn.create_server_group(name, policies)
                changed = True

            self.exit_json(
                changed=changed,
                id=server_group['id'],
                server_group=server_group
            )
        if state == 'absent':
            if server_group:
                self.conn.delete_server_group(server_group['id'])
                changed = True
            self.exit_json(changed=changed)


def main():
    module = ServerGroupModule()
    module()


if __name__ == '__main__':
    main()
