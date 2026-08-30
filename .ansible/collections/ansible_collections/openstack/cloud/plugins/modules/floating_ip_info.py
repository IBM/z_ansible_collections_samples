#!/usr/bin/python
# coding: utf-8 -*-
#
# Copyright (c) 2021 by Open Telekom Cloud, operated by T-Systems International GmbH
# GNU General Public License v3.0+ (see COPYING or https://www.gnu.org/licenses/gpl-3.0.txt)

DOCUMENTATION = '''
---
module: floating_ip_info
short_description: Get information about floating ips
author: OpenStack Ansible SIG
description:
  - Get a generator of floating ips.
options:
  description:
    description:
      - The description of a floating IP.
    type: str
  fixed_ip_address:
    description:
      - The fixed IP address associated with a floating IP address.
    type: str
  floating_ip_address:
    description:
      -  The IP address of a floating IP.
    type: str
  floating_network:
    description:
      - The name or id of the network associated with a floating IP.
    type: str
  port:
    description:
      - The name or id of the port to which a floating IP is associated.
    type: str
  project_id:
    description:
      - The ID of the project a floating IP is associated with.
    type: str
  router:
    description:
      - The name or id of an associated router.
    type: str
  status:
    description:
      - The status of a floating IP, which can be ``ACTIVE``or ``DOWN``.
    choices: ['active', 'down']
    type: str
requirements:
    - "python >= 3.6"
    - "openstacksdk"

extends_documentation_fragment:
- openstack.cloud.openstack
'''

RETURN = '''
floating_ips:
  description: The floating ip objects list.
  type: complex
  returned: On Success.
  contains:
    created_at:
      description: Timestamp at which the floating IP was assigned.
      type: str
    description:
      description: The description of a floating IP.
      type: str
    dns_domain:
      description: The DNS domain.
      type: str
    dns_name:
      description: The DNS name.
      type: str
    fixed_ip_address:
      description: The fixed IP address associated with a floating IP address.
      type: str
    floating_ip_address:
      description: The IP address of a floating IP.
      type: str
    floating_network_id:
      description: The id of the network associated with a floating IP.
      type: str
    id:
      description: Id of the floating ip.
      type: str
    name:
      description: Name of the floating ip.
      type: str
    port_details:
      description: The details of the port that this floating IP associates \
        with. Present if ``fip-port-details`` extension is loaded.
      type: str
    port_id:
      description: The port ID floating ip associated with.
      type: str
    project_id:
      description: The ID of the project this floating IP is associated with.
      type: str
    qos_policy_id:
      description: The ID of the QoS policy attached to the floating IP.
      type: str
    revision_number:
      description: Revision number.
      type: str
    router_id:
      description: The id of the router floating ip associated with.
      type: str
    status:
      description: The status of a floating IP, which can be ``ACTIVE``or ``DOWN``.\
        Can be 'ACTIVE' and 'DOWN'.
      type: str
    subnet_id:
      description: The id of the subnet the floating ip associated with.
      type: str
    tags:
      description: List of tags.
      type: str
    updated_at:
      description: Timestamp at which the floating IP was last updated.
      type: str
'''

EXAMPLES = '''
# Getting all floating ips
- openstack.cloud.floating_ip_info:
  register: fips

# Getting fip by associated fixed IP address.
- openstack.cloud.floating_ip_info:
    fixed_ip_address: 192.168.10.8
  register: fip

# Getting fip by associated router.
- openstack.cloud.floating_ip_info:
    router: my-router
  register: fip
'''

from ansible_collections.openstack.cloud.plugins.module_utils.openstack import OpenStackModule


class FloatingIPInfoModule(OpenStackModule):
    argument_spec = dict(
        description=dict(required=False),
        fixed_ip_address=dict(required=False),
        floating_ip_address=dict(required=False),
        floating_network=dict(required=False),
        port=dict(required=False),
        project_id=dict(required=False),
        router=dict(required=False),
        status=dict(required=False, choices=['active', 'down']),
    )
    module_kwargs = dict(
        supports_check_mode=True
    )

    def run(self):

        description = self.params['description']
        fixed_ip_address = self.params['fixed_ip_address']
        floating_ip_address = self.params['floating_ip_address']
        floating_network = self.params['floating_network']
        port = self.params['port']
        project_id = self.params['project_id']
        router = self.params['router']
        status = self.params['status']

        query = {}
        if description:
            query['description'] = description
        if fixed_ip_address:
            query['fixed_ip_address'] = fixed_ip_address
        if floating_ip_address:
            query['floating_ip_address'] = floating_ip_address
        if floating_network:
            try:
                query['floating_network_id'] = self.conn.network.find_network(name_or_id=floating_network,
                                                                              ignore_missing=False).id
            except self.sdk.exceptions.ResourceNotFound:
                self.fail_json(msg="floating_network not found")
        if port:
            try:
                query['port_id'] = self.conn.network.find_port(name_or_id=port, ignore_missing=False).id
            except self.sdk.exceptions.ResourceNotFound:
                self.fail_json(msg="port not found")
        if project_id:
            query['project_id'] = project_id
        if router:
            try:
                query['router_id'] = self.conn.network.find_router(name_or_id=router, ignore_missing=False).id
            except self.sdk.exceptions.ResourceNotFound:
                self.fail_json(msg="router not found")
        if status:
            query['status'] = status.upper()

        ips = [ip.to_dict(computed=False) for ip in self.conn.network.ips(**query)]
        self.exit_json(changed=False, floating_ips=ips)


def main():
    module = FloatingIPInfoModule()
    module()


if __name__ == '__main__':
    main()
