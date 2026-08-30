#!/usr/bin/python

# This module is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# This software is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this software.  If not, see <http://www.gnu.org/licenses/>.

DOCUMENTATION = '''
---
module: project_access
short_description: Manage OpenStack compute flavors access
author: OpenStack Ansible SIG
description:
    - Add or remove flavor, volume_type or other resources access
      from OpenStack.
options:
  state:
    description:
      - Indicate desired state of the resource.
    choices: ['present', 'absent']
    required: false
    default: present
    type: str
  target_project_id:
    description:
      - Project id.
    required: true
    type: str
  resource_type:
    description:
      - The resource type (eg. nova_flavor, cinder_volume_type).
    required: true
    type: str
  resource_name:
    description:
      - The resource name (eg. tiny).
    required: true
    type: str
requirements:
    - "python >= 3.6"
    - "openstacksdk"


extends_documentation_fragment:
- openstack.cloud.openstack
'''

EXAMPLES = '''
- name: "Enable access to tiny flavor to your tenant."
  openstack.cloud.project_access:
    cloud: mycloud
    state: present
    target_project_id: f0f1f2f3f4f5f67f8f9e0e1
    resource_name: tiny
    resource_type: nova_flavor


- name: "Disable access to the given flavor to project"
  openstack.cloud.project_access:
    cloud: mycloud
    state: absent
    target_project_id: f0f1f2f3f4f5f67f8f9e0e1
    resource_name: tiny
    resource_type: nova_flavor
'''

RETURN = '''
flavor:
    description: Dictionary describing the flavor.
    returned: On success when I(state) is 'present'
    type: complex
    contains:
        id:
            description: Flavor ID.
            returned: success
            type: str
            sample: "515256b8-7027-4d73-aa54-4e30a4a4a339"
        name:
            description: Flavor name.
            returned: success
            type: str
            sample: "tiny"

'''

from ansible_collections.openstack.cloud.plugins.module_utils.openstack import OpenStackModule


class IdentityProjectAccess(OpenStackModule):
    argument_spec = dict(
        state=dict(required=False, default='present',
                   choices=['absent', 'present']),
        target_project_id=dict(required=True, type='str'),
        resource_type=dict(required=True, type='str'),
        resource_name=dict(required=True, type='str'),
    )

    module_kwargs = dict(
        supports_check_mode=True,
        required_if=[
            ('state', 'present', ['target_project_id'])
        ]
    )

    def run(self):
        state = self.params['state']
        resource_name = self.params['resource_name']
        resource_type = self.params['resource_type']
        target_project_id = self.params['target_project_id']

        if resource_type == 'nova_flavor':
            # returns Munch({'NAME_ATTR': 'name',
            # 'tenant_id': u'37e55da59ec842649d84230f3a24eed5',
            # 'HUMAN_ID': False,
            # 'flavor_id': u'6d4d37b9-0480-4a8c-b8c9-f77deaad73f9',
            #  'request_ids': [], 'human_id': None}),
            _get_resource = self.conn.get_flavor
            _list_resource_access = self.conn.list_flavor_access
            _add_resource_access = self.conn.add_flavor_access
            _remove_resource_access = self.conn.remove_flavor_access
        elif resource_type == 'cinder_volume_type':
            # returns [Munch({
            # 'project_id': u'178cdb9955b047eea7afbe582038dc94',
            #  'properties': {'request_ids': [], 'NAME_ATTR': 'name',
            #  'human_id': None,
            # 'HUMAN_ID': False},
            #  'id': u'd5573023-b290-42c8-b232-7c5ca493667f'}),
            _get_resource = self.conn.get_volume_type
            _list_resource_access = self.conn.get_volume_type_access
            _add_resource_access = self.conn.add_volume_type_access
            _remove_resource_access = self.conn.remove_volume_type_access
        else:
            self.exit_json(
                changed=False,
                resource_name=resource_name,
                resource_type=resource_type,
                error="Not implemented.")

        resource = _get_resource(resource_name)
        if not resource:
            self.exit_json(
                changed=False,
                resource_name=resource_name,
                resource_type=resource_type,
                error="Not found.")
        resource_id = getattr(resource, 'id', resource['id'])
        # _list_resource_access returns a list of dicts containing 'project_id'
        acls = _list_resource_access(resource_id)

        if not all(acl.get('project_id') for acl in acls):
            self.exit_json(
                changed=False,
                resource_name=resource_name,
                resource_type=resource_type,
                error="Missing project_id in resource output.")
        allowed_tenants = [acl['project_id'] for acl in acls]

        changed_access = any((
            state == 'present' and target_project_id not in allowed_tenants,
            state == 'absent' and target_project_id in allowed_tenants
        ))
        if self.ansible.check_mode or not changed_access:
            self.exit_json(
                changed=changed_access, resource=resource, id=resource_id)

        if state == 'present':
            _add_resource_access(
                resource_id, target_project_id
            )
        elif state == 'absent':
            _remove_resource_access(
                resource_id, target_project_id
            )

        self.exit_json(
            changed=True, resource=resource, id=resource_id)


def main():
    module = IdentityProjectAccess()
    module()


if __name__ == '__main__':
    main()
