#!/usr/bin/python
# -*- coding: utf-8 -*-
# Copyright (c) 2019, Bram Verschueren <verschueren.bram@gmail.com>
# GNU General Public License v3.0+ (see COPYING or https://www.gnu.org/licenses/gpl-3.0.txt)

DOCUMENTATION = '''
---
module: routers_info
short_description: Retrieve information about one or more OpenStack routers.
author: OpenStack Ansible SIG
description:
    - Retrieve information about one or more routers from OpenStack.
options:
   name:
     description:
        - Name or ID of the router
     required: false
     type: str
   filters:
     description:
        - A dictionary of meta data to use for further filtering.  Elements of
          this dictionary may be additional dictionaries.
     required: false
     type: dict
     suboptions:
       project_id:
         description:
           - Filter the list result by the ID of the project that owns the resource.
         type: str
         aliases:
           - tenant_id
       name:
         description:
           - Filter the list result by the human-readable name of the resource.
         type: str
       description:
         description:
           - Filter the list result by the human-readable description of the resource.
         type: str
       admin_state_up:
         description:
           - Filter the list result by the administrative state of the resource, which is up (true) or down (false).
         type: bool
       revision_number:
         description:
           - Filter the list result by the revision number of the resource.
         type: int
       tags:
         description:
           - A list of tags to filter the list result by. Resources that match all tags in this list will be returned.
         type: list
         elements: str
requirements:
    - "python >= 3.6"
    - "openstacksdk"
extends_documentation_fragment:
- openstack.cloud.openstack
'''

EXAMPLES = '''
- name: Gather information about routers
  openstack.cloud.routers_info:
    auth:
      auth_url: https://identity.example.com
      username: user
      password: password
      project_name: someproject
  register: result

- name: Show openstack routers
  debug:
    msg: "{{ result.openstack_routers }}"

- name: Gather information about a router by name
  openstack.cloud.routers_info:
    auth:
      auth_url: https://identity.example.com
      username: user
      password: password
      project_name: someproject
    name: router1
  register: result

- name: Show openstack routers
  debug:
    msg: "{{ result.openstack_routers }}"

- name: Gather information about a router with filter
  openstack.cloud.routers_info:
    auth:
      auth_url: https://identity.example.com
      username: user
      password: password
      project_name: someproject
    filters:
      tenant_id: bc3ea709c96849d6b81f54640400a19f
  register: result

- name: Show openstack routers
  debug:
    msg: "{{ result.openstack_routers }}"
'''

RETURN = '''
openstack_routers:
    description: has all the openstack information about the routers
    returned: always, but can be null
    type: complex
    contains:
        id:
            description: Unique UUID.
            returned: success
            type: str
        name:
            description: Name given to the router.
            returned: success
            type: str
        status:
            description: Router status.
            returned: success
            type: str
        external_gateway_info:
            description: The external gateway information of the router.
            returned: success
            type: dict
        interfaces_info:
            description: List of connected interfaces.
            returned: success
            type: list
        distributed:
            description: Indicates a distributed router.
            returned: success
            type: bool
        ha:
            description: Indicates a highly-available router.
            returned: success
            type: bool
        project_id:
            description: Project id associated with this router.
            returned: success
            type: str
        routes:
            description: The extra routes configuration for L3 router.
            returned: success
            type: list
'''

from ansible_collections.openstack.cloud.plugins.module_utils.openstack import OpenStackModule


class RouterInfoModule(OpenStackModule):

    deprecated_names = ('os_routers_info', 'openstack.cloud.os_routers_info')

    argument_spec = dict(
        name=dict(required=False, default=None),
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
        routers = self.conn.search_routers(**kwargs)

        for router in routers:
            interfaces_info = []
            for port in self.conn.list_router_interfaces(router):
                if port.device_owner != "network:router_gateway":
                    for ip_spec in port.fixed_ips:
                        int_info = {
                            'port_id': port.id,
                            'ip_address': ip_spec.get('ip_address'),
                            'subnet_id': ip_spec.get('subnet_id')
                        }
                        interfaces_info.append(int_info)
            router['interfaces_info'] = interfaces_info

        self.exit(changed=False, openstack_routers=routers)


def main():
    module = RouterInfoModule()
    module()


if __name__ == '__main__':
    main()
