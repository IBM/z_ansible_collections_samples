#!/usr/bin/python
# coding: utf-8 -*-

# Copyright (c) 2020, Sagi Shnaidman <sshnaidm@redhat.com>
# GNU General Public License v3.0+ (see COPYING or https://www.gnu.org/licenses/gpl-3.0.txt)

DOCUMENTATION = '''
---
module: identity_role_info
short_description: Retrieve information about roles
author: OpenStack Ansible SIG
description:
  - Get information about identity roles in Openstack
options:
  domain_id:
    description:
      - Domain ID which owns the role
    type: str
    required: false
  name:
    description:
      - Name or ID of the role
    type: str
    required: false

requirements:
  - "python >= 3.6"
  - "openstacksdk"

extends_documentation_fragment:
  - openstack.cloud.openstack
'''

RETURN = '''
openstack_roles:
  description: List of identity roles
  returned: always
  type: list
  elements: dict
  contains:
    id:
      description: Unique ID for the role
      returned: success
      type: str
    name:
      description: Unique role name, within the owning domain.
      returned: success
      type: str
    domain_id:
      description: References the domain ID which owns the role.
      returned: success
      type: str
'''

EXAMPLES = '''
# Retrieve info about all roles
- openstack.cloud.identity_role_info:
    cloud: mycloud

# Retrieve info about all roles in specific domain
- openstack.cloud.identity_role_info:
    cloud: mycloud
    domain_id: some_domain_id

# Retrieve info about role 'admin'
- openstack.cloud.identity_role_info:
    cloud: mycloud
    name: admin

'''

from ansible_collections.openstack.cloud.plugins.module_utils.openstack import OpenStackModule


class IdentityRoleInfoModule(OpenStackModule):
    argument_spec = dict(
        domain_id=dict(type='str', required=False),
        name=dict(type='str', required=False),
    )

    module_kwargs = dict(
        supports_check_mode=True,
    )

    def run(self):
        params = {
            'domain_id': self.params['domain_id'],
            'name_or_id': self.params['name'],
        }
        params = {k: v for k, v in params.items() if v is not None}

        roles = self.conn.search_roles(**params)
        self.exit_json(changed=False, openstack_roles=roles)


def main():
    module = IdentityRoleInfoModule()
    module()


if __name__ == '__main__':
    main()
