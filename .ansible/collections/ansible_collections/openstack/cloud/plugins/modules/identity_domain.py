#!/usr/bin/python
# Copyright (c) 2015 Hewlett-Packard Development Company, L.P.
# GNU General Public License v3.0+ (see COPYING or https://www.gnu.org/licenses/gpl-3.0.txt)

DOCUMENTATION = '''
---
module: identity_domain
short_description: Manage OpenStack Identity Domains
author: OpenStack Ansible SIG
description:
    - Create, update, or delete OpenStack Identity domains. If a domain
      with the supplied name already exists, it will be updated with the
      new description and enabled attributes.
options:
   name:
     description:
        - Name that has to be given to the instance
     required: true
     type: str
   description:
     description:
        - Description of the domain
     type: str
   enabled:
     description:
        - Is the domain enabled
     type: bool
     default: 'yes'
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
# Create a domain
- openstack.cloud.identity_domain:
     cloud: mycloud
     state: present
     name: demo
     description: Demo Domain

# Delete a domain
- openstack.cloud.identity_domain:
     cloud: mycloud
     state: absent
     name: demo
'''

RETURN = '''
domain:
    description: Dictionary describing the domain.
    returned: On success when I(state) is 'present'
    type: complex
    contains:
        id:
            description: Domain ID.
            type: str
            sample: "474acfe5-be34-494c-b339-50f06aa143e4"
        name:
            description: Domain name.
            type: str
            sample: "demo"
        description:
            description: Domain description.
            type: str
            sample: "Demo Domain"
        enabled:
            description: Domain description.
            type: bool
            sample: True

id:
    description: The domain ID.
    returned: On success when I(state) is 'present'
    type: str
    sample: "474acfe5-be34-494c-b339-50f06aa143e4"
'''

from ansible_collections.openstack.cloud.plugins.module_utils.openstack import OpenStackModule


class IdentityDomainModule(OpenStackModule):
    argument_spec = dict(
        name=dict(required=True),
        description=dict(default=None),
        enabled=dict(default=True, type='bool'),
        state=dict(default='present', choices=['absent', 'present']),
    )

    module_kwargs = dict(
        supports_check_mode=True
    )

    def _needs_update(self, domain):
        if self.params['description'] is not None and \
           domain.description != self.params['description']:
            return True
        if domain.get(
                "is_enabled", domain.get("enabled")) != self.params['enabled']:
            return True
        return False

    def _system_state_change(self, domain):
        state = self.params['state']
        if state == 'absent' and domain:
            return True

        if state == 'present':
            if domain is None:
                return True
            return self._needs_update(domain)

        return False

    def run(self):
        name = self.params['name']
        description = self.params['description']
        enabled = self.params['enabled']
        state = self.params['state']

        domains = list(self.conn.identity.domains(name=name))

        if len(domains) > 1:
            self.fail_json(msg='Domain name %s is not unique' % name)
        elif len(domains) == 1:
            domain = domains[0]
        else:
            domain = None

        if self.ansible.check_mode:
            self.exit_json(changed=self._system_state_change(domain))

        if state == 'present':
            if domain is None:
                domain = self.conn.create_domain(
                    name=name, description=description, enabled=enabled)
                changed = True
            else:
                if self._needs_update(domain):
                    domain = self.conn.update_domain(
                        domain.id, name=name, description=description,
                        enabled=enabled)
                    changed = True
                else:
                    changed = False
            if hasattr(domain, "to_dict"):
                domain = domain.to_dict()
                domain.pop("location")
            self.exit_json(changed=changed, domain=domain, id=domain['id'])

        elif state == 'absent':
            if domain is None:
                changed = False
            else:
                self.conn.delete_domain(domain.id)
                changed = True
            self.exit_json(changed=changed)


def main():
    module = IdentityDomainModule()
    module()


if __name__ == '__main__':
    main()
