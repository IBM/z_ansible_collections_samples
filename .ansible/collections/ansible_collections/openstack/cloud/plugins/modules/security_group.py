#!/usr/bin/python

# Copyright (c) 2015 Hewlett-Packard Development Company, L.P.
# Copyright (c) 2013, Benno Joy <benno@ansible.com>
# GNU General Public License v3.0+ (see COPYING or https://www.gnu.org/licenses/gpl-3.0.txt)

DOCUMENTATION = '''
---
module: security_group
short_description: Add/Delete security groups from an OpenStack cloud.
author: OpenStack Ansible SIG
description:
   - Add or Remove security groups from an OpenStack cloud.
options:
   name:
     description:
        - Name that has to be given to the security group. This module
          requires that security group names be unique.
     required: true
     type: str
   description:
     description:
        - Long description of the purpose of the security group
     type: str
   state:
     description:
       - Should the resource be present or absent.
     choices: [present, absent]
     default: present
     type: str
   project:
     description:
        - Unique name or ID of the project.
     required: false
     type: str
requirements:
    - "python >= 3.6"
    - "openstacksdk"

extends_documentation_fragment:
- openstack.cloud.openstack
'''

EXAMPLES = '''
# Create a security group
- openstack.cloud.security_group:
    cloud: mordred
    state: present
    name: foo
    description: security group for foo servers

# Update the existing 'foo' security group description
- openstack.cloud.security_group:
    cloud: mordred
    state: present
    name: foo
    description: updated description for the foo security group

# Create a security group for a given project
- openstack.cloud.security_group:
    cloud: mordred
    state: present
    name: foo
    project: myproj
'''

from ansible_collections.openstack.cloud.plugins.module_utils.openstack import OpenStackModule


class SecurityGroupModule(OpenStackModule):

    argument_spec = dict(
        name=dict(required=True),
        description=dict(default=''),
        state=dict(default='present', choices=['absent', 'present']),
        project=dict(default=None),
    )

    def _needs_update(self, secgroup):
        """Check for differences in the updatable values.

        NOTE: We don't currently allow name updates.
        """
        if secgroup['description'] != self.params['description']:
            return True
        return False

    def _system_state_change(self, secgroup):
        state = self.params['state']
        if state == 'present':
            if not secgroup:
                return True
            return self._needs_update(secgroup)
        if state == 'absent' and secgroup:
            return True
        return False

    def run(self):

        name = self.params['name']
        state = self.params['state']
        description = self.params['description']
        project = self.params['project']

        if project is not None:
            proj = self.conn.get_project(project)
            if proj is None:
                self.fail_json(msg='Project %s could not be found' % project)
            project_id = proj['id']
        else:
            project_id = self.conn.current_project_id

        if project_id:
            filters = {'tenant_id': project_id}
        else:
            filters = None

        secgroup = self.conn.get_security_group(name, filters=filters)

        if self.ansible.check_mode:
            self.exit(changed=self._system_state_change(secgroup))

        changed = False
        if state == 'present':
            if not secgroup:
                kwargs = {}
                if project_id:
                    kwargs['project_id'] = project_id
                secgroup = self.conn.create_security_group(name, description,
                                                           **kwargs)
                changed = True
            else:
                if self._needs_update(secgroup):
                    secgroup = self.conn.update_security_group(
                        secgroup['id'], description=description)
                    changed = True
            self.exit(
                changed=changed, id=secgroup['id'], secgroup=secgroup)

        if state == 'absent':
            if secgroup:
                self.conn.delete_security_group(secgroup['id'])
                changed = True
            self.exit(changed=changed)


def main():
    module = SecurityGroupModule()
    module()


if __name__ == '__main__':
    main()
