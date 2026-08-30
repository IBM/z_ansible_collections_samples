#!/usr/bin/python
# Copyright (c) 2016 Hewlett-Packard Enterprise Corporation
# GNU General Public License v3.0+ (see COPYING or https://www.gnu.org/licenses/gpl-3.0.txt)

DOCUMENTATION = '''
---
module: identity_user_info
short_description: Retrieve information about one or more OpenStack users
author: OpenStack Ansible SIG
description:
    - Retrieve information about a one or more OpenStack users
    - This module was called C(openstack.cloud.identity_user_facts) before Ansible 2.9, returning C(ansible_facts).
      Note that the M(openstack.cloud.identity_user_info) module no longer returns C(ansible_facts)!
options:
   name:
     description:
        - Name or ID of the user
     type: str
   domain:
     description:
        - Name or ID of the domain containing the user if the cloud supports domains
     type: str
   filters:
     description:
        - A dictionary of meta data to use for further filtering.  Elements of
          this dictionary may be additional dictionaries.
     type: dict
requirements:
    - "python >= 3.6"
    - "openstacksdk"

extends_documentation_fragment:
- openstack.cloud.openstack
'''

EXAMPLES = '''
# Gather information about previously created users
- openstack.cloud.identity_user_info:
    cloud: awesomecloud
  register: result
- debug:
    msg: "{{ result.openstack_users }}"

# Gather information about a previously created user by name
- openstack.cloud.identity_user_info:
    cloud: awesomecloud
    name: demouser
  register: result
- debug:
    msg: "{{ result.openstack_users }}"

# Gather information about a previously created user in a specific domain
- openstack.cloud.identity_user_info:
    cloud: awesomecloud
    name: demouser
    domain: admindomain
  register: result
- debug:
    msg: "{{ result.openstack_users }}"

# Gather information about a previously created user in a specific domain with filter
- openstack.cloud.identity_user_info:
    cloud: awesomecloud
    name: demouser
    domain: admindomain
    filters:
      enabled: False
  register: result
- debug:
    msg: "{{ result.openstack_users }}"
'''


RETURN = '''
openstack_users:
    description: has all the OpenStack information about users
    returned: always
    type: list
    elements: dict
    contains:
        id:
            description: Unique UUID.
            returned: success
            type: str
        name:
            description: Username of the user.
            returned: success
            type: str
        default_project_id:
            description: Default project ID of the user
            returned: success
            type: str
        description:
            description: The description of this user
            returned: success
            type: str
        domain_id:
            description: Domain ID containing the user
            returned: success
            type: str
        email:
            description: Email of the user
            returned: success
            type: str
        enabled:
            description: Flag to indicate if the user is enabled
            returned: success
            type: bool
        username:
            description: Username with Identity API v2 (OpenStack Pike or earlier) else Null
            returned: success
            type: str
'''

from ansible_collections.openstack.cloud.plugins.module_utils.openstack import OpenStackModule


class IdentityUserInfoModule(OpenStackModule):
    argument_spec = dict(
        name=dict(required=False, default=None),
        domain=dict(required=False, default=None),
        filters=dict(required=False, type='dict', default=None),
    )
    module_kwargs = dict(
        supports_check_mode=True
    )

    deprecated_names = ('openstack.cloud.identity_user_facts')

    def run(self):
        name = self.params['name']
        domain = self.params['domain']
        filters = self.params['filters']

        args = {}
        if domain:
            dom_obj = self.conn.identity.find_domain(domain)
            if dom_obj is None:
                self.fail_json(
                    msg="Domain name or ID '{0}' does not exist".format(domain))
            args['domain_id'] = dom_obj.id

        users = self.conn.search_users(name, filters, **args)
        self.exit_json(changed=False, openstack_users=users)


def main():
    module = IdentityUserInfoModule()
    module()


if __name__ == '__main__':
    main()
