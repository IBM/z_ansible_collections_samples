#!/usr/bin/python
# Copyright (c) 2015 IBM Corporation
# GNU General Public License v3.0+ (see COPYING or https://www.gnu.org/licenses/gpl-3.0.txt)

DOCUMENTATION = '''
---
module: project
short_description: Manage OpenStack Projects
author: OpenStack Ansible SIG
description:
    - Manage OpenStack Projects. Projects can be created,
      updated or deleted using this module. A project will be updated
      if I(name) matches an existing project and I(state) is present.
      The value for I(name) cannot be updated without deleting and
      re-creating the project.
options:
   name:
     description:
        - Name for the project
     required: true
     type: str
   description:
     description:
        - Description for the project
     type: str
   domain_id:
     description:
        - Domain id to create the project in if the cloud supports domains.
     aliases: ['domain']
     type: str
   enabled:
     description:
        - Is the project enabled
     type: bool
     default: 'yes'
   properties:
     description:
        - Additional properties to be associated with this project. Requires
          openstacksdk>0.45.
     type: dict
     required: false
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
# Create a project
- openstack.cloud.project:
    cloud: mycloud
    endpoint_type: admin
    state: present
    name: demoproject
    description: demodescription
    domain_id: demoid
    enabled: True
    properties:
      internal_alias: demo_project

# Delete a project
- openstack.cloud.project:
    cloud: mycloud
    endpoint_type: admin
    state: absent
    name: demoproject
'''


RETURN = '''
project:
    description: Dictionary describing the project.
    returned: On success when I(state) is 'present'
    type: complex
    contains:
        id:
            description: Project ID
            type: str
            sample: "f59382db809c43139982ca4189404650"
        name:
            description: Project name
            type: str
            sample: "demoproject"
        description:
            description: Project description
            type: str
            sample: "demodescription"
        enabled:
            description: Boolean to indicate if project is enabled
            type: bool
            sample: True
'''

from ansible_collections.openstack.cloud.plugins.module_utils.openstack import OpenStackModule


class IdentityProjectModule(OpenStackModule):
    argument_spec = dict(
        name=dict(required=True),
        description=dict(required=False),
        domain_id=dict(required=False, aliases=['domain']),
        properties=dict(required=False, type='dict', min_ver='0.45.1'),
        enabled=dict(default=True, type='bool'),
        state=dict(default='present', choices=['absent', 'present'])
    )
    module_kwargs = dict(
        supports_check_mode=True
    )

    def _needs_update(self, project):
        keys = ('description', 'enabled')
        for key in keys:
            if self.params[key] is not None and self.params[key] != project.get(key):
                return True

        properties = self.params['properties']
        if properties:
            project_properties = project.get('properties')
            for k, v in properties.items():
                if v is not None and (k not in project_properties or v != project_properties[k]):
                    return True

        return False

    def _system_state_change(self, project):
        state = self.params['state']
        if state == 'present':
            if project is None:
                changed = True
            else:
                if self._needs_update(project):
                    changed = True
                else:
                    changed = False

        elif state == 'absent':
            changed = project is not None

        return changed

    def run(self):
        name = self.params['name']
        description = self.params['description']
        domain = self.params['domain_id']
        enabled = self.params['enabled']
        properties = self.params['properties'] or {}
        state = self.params['state']

        if domain:
            try:
                # We assume admin is passing domain id
                dom = self.conn.get_domain(domain)['id']
                domain = dom
            except Exception:
                # If we fail, maybe admin is passing a domain name.
                # Note that domains have unique names, just like id.
                try:
                    dom = self.conn.search_domains(filters={'name': domain})[0]['id']
                    domain = dom
                except Exception:
                    # Ok, let's hope the user is non-admin and passing a sane id
                    pass

        if domain:
            project = self.conn.get_project(name, domain_id=domain)
        else:
            project = self.conn.get_project(name)

        if self.ansible.check_mode:
            self.exit_json(changed=self._system_state_change(project))

        if state == 'present':
            if project is None:
                project = self.conn.create_project(
                    name=name, description=description,
                    domain_id=domain,
                    enabled=enabled)
                changed = True

                project = self.conn.update_project(
                    project['id'],
                    description=description,
                    enabled=enabled,
                    **properties)
            else:
                if self._needs_update(project):
                    project = self.conn.update_project(
                        project['id'],
                        description=description,
                        enabled=enabled,
                        **properties)
                    changed = True
                else:
                    changed = False
            self.exit_json(changed=changed, project=project)

        elif state == 'absent':
            if project is None:
                changed = False
            else:
                self.conn.delete_project(project['id'])
                changed = True
            self.exit_json(changed=changed)


def main():
    module = IdentityProjectModule()
    module()


if __name__ == '__main__':
    main()
