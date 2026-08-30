#!/usr/bin/python

# Copyright (c) 2015 Hewlett-Packard Development Company, L.P.
# GNU General Public License v3.0+ (see COPYING or https://www.gnu.org/licenses/gpl-3.0.txt)

DOCUMENTATION = '''
---
module: subnets_info
short_description: Retrieve information about one or more OpenStack subnets.
author: OpenStack Ansible SIG
description:
    - Retrieve information about one or more subnets from OpenStack.
    - This module was called C(openstack.cloud.subnets_facts) before Ansible 2.9, returning C(ansible_facts).
      Note that the M(openstack.cloud.subnets_info) module no longer returns C(ansible_facts)!
options:
   name:
     description:
        - Name or ID of the subnet.
        - Alias 'subnet' added in version 2.8.
     required: false
     aliases: ['subnet']
     type: str
   filters:
     description:
        - A dictionary of meta data to use for further filtering.  Elements of
          this dictionary may be additional dictionaries.
     required: false
     type: dict
requirements:
    - "python >= 3.6"
    - "openstacksdk"

extends_documentation_fragment:
- openstack.cloud.openstack
'''

EXAMPLES = '''
- name: Gather information about previously created subnets
  openstack.cloud.subnets_info:
    auth:
      auth_url: https://identity.example.com
      username: user
      password: password
      project_name: someproject
  register: result

- name: Show openstack subnets
  debug:
    msg: "{{ result.openstack_subnets }}"

- name: Gather information about a previously created subnet by name
  openstack.cloud.subnets_info:
    auth:
      auth_url: https://identity.example.com
      username: user
      password: password
      project_name: someproject
    name: subnet1
  register: result

- name: Show openstack subnets
  debug:
    msg: "{{ result.openstack_subnets }}"

- name: Gather information about a previously created subnet with filter
  # Note: name and filters parameters are not mutually exclusive
  openstack.cloud.subnets_info:
    auth:
      auth_url: https://identity.example.com
      username: user
      password: password
      project_name: someproject
    filters:
      tenant_id: 55e2ce24b2a245b09f181bf025724cbe
  register: result

- name: Show openstack subnets
  debug:
    msg: "{{ result.openstack_subnets }}"
'''

RETURN = '''
openstack_subnets:
    description: has all the openstack information about the subnets
    returned: always, but can be null
    type: complex
    contains:
        id:
            description: Unique UUID.
            returned: success
            type: str
        name:
            description: Name given to the subnet.
            returned: success
            type: str
        network_id:
            description: Network ID this subnet belongs in.
            returned: success
            type: str
        cidr:
            description: Subnet's CIDR.
            returned: success
            type: str
        gateway_ip:
            description: Subnet's gateway ip.
            returned: success
            type: str
        enable_dhcp:
            description: DHCP enable flag for this subnet.
            returned: success
            type: bool
        ip_version:
            description: IP version for this subnet.
            returned: success
            type: int
        tenant_id:
            description: Tenant id associated with this subnet.
            returned: success
            type: str
        dns_nameservers:
            description: DNS name servers for this subnet.
            returned: success
            type: list
            elements: str
        allocation_pools:
            description: Allocation pools associated with this subnet.
            returned: success
            type: list
            elements: dict
'''

from ansible_collections.openstack.cloud.plugins.module_utils.openstack import OpenStackModule


class SubnetInfoModule(OpenStackModule):

    deprecated_names = ('subnets_facts', 'openstack.cloud.subnets_facts')

    argument_spec = dict(
        name=dict(required=False, default=None, aliases=['subnet']),
        filters=dict(required=False, type='dict', default=None)
    )
    module_kwargs = dict(
        supports_check_mode=True
    )

    def run(self):
        kwargs = self.check_versioned(
            filters=self.params['filters']
        )
        if self.params['name']:
            kwargs['name_or_id'] = self.params['name']
        subnets = self.conn.search_subnets(**kwargs)

        self.exit(changed=False, openstack_subnets=subnets)


def main():
    module = SubnetInfoModule()
    module()


if __name__ == '__main__':
    main()
