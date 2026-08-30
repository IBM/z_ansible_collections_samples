#!/usr/bin/python

# Copyright (c) 2014 Hewlett-Packard Development Company, L.P.
# Copyright (c) 2013, Benno Joy <benno@ansible.com>
# GNU General Public License v3.0+ (see COPYING or https://www.gnu.org/licenses/gpl-3.0.txt)

DOCUMENTATION = '''
---
module: network
short_description: Creates/removes networks from OpenStack
author: OpenStack Ansible SIG
description:
   - Add or remove network from OpenStack.
options:
   name:
     description:
        - Name to be assigned to the network.
     required: true
     type: str
   shared:
     description:
        - Whether this network is shared or not.
     type: bool
     default: 'no'
   admin_state_up:
     description:
        - Whether the state should be marked as up or down.
     type: bool
     default: 'yes'
   external:
     description:
        - Whether this network is externally accessible.
     type: bool
     default: 'no'
   state:
     description:
        - Indicate desired state of the resource.
     choices: ['present', 'absent']
     default: present
     type: str
   provider_physical_network:
     description:
        - The physical network where this network object is implemented.
     type: str
   provider_network_type:
     description:
        - The type of physical network that maps to this network resource.
     type: str
   provider_segmentation_id:
     description:
        - An isolated segment on the physical network. The I(network_type)
          attribute defines the segmentation model. For example, if the
          I(network_type) value is vlan, this ID is a vlan identifier. If
          the I(network_type) value is gre, this ID is a gre key.
     type: int
   project:
     description:
        - Project name or ID containing the network (name admin-only)
     type: str
   port_security_enabled:
     description:
        -  Whether port security is enabled on the network or not.
           Network will use OpenStack defaults if this option is
           not utilised. Requires openstacksdk>=0.18.
     type: bool
   mtu_size:
     description:
       -  The maximum transmission unit (MTU) value to address fragmentation.
          Network will use OpenStack defaults if this option is
          not provided. Requires openstacksdk>=0.18.
     type: int
     aliases: ['mtu']
   dns_domain:
     description:
       -  The DNS domain value to set. Requires openstacksdk>=0.29.
          Network will use Openstack defaults if this option is
          not provided.
     type: str
requirements:
    - "python >= 3.6"
    - "openstacksdk"

extends_documentation_fragment:
- openstack.cloud.openstack
'''

EXAMPLES = '''
# Create an externally accessible network named 'ext_network'.
- openstack.cloud.network:
    cloud: mycloud
    state: present
    name: ext_network
    external: true
'''

RETURN = '''
network:
    description: Dictionary describing the network.
    returned: On success when I(state) is 'present'.
    type: complex
    contains:
        id:
            description: Network ID.
            type: str
            sample: "4bb4f9a5-3bd2-4562-bf6a-d17a6341bb56"
        name:
            description: Network name.
            type: str
            sample: "ext_network"
        shared:
            description: Indicates whether this network is shared across all tenants.
            type: bool
            sample: false
        status:
            description: Network status.
            type: str
            sample: "ACTIVE"
        mtu:
            description: The MTU of a network resource.
            type: int
            sample: 0
        dns_domain:
            description: The DNS domain of a network resource.
            type: str
            sample: "sample.openstack.org."
        admin_state_up:
            description: The administrative state of the network.
            type: bool
            sample: true
        port_security_enabled:
            description: The port security status
            type: bool
            sample: true
        router:external:
            description: Indicates whether this network is externally accessible.
            type: bool
            sample: true
        tenant_id:
            description: The tenant ID.
            type: str
            sample: "06820f94b9f54b119636be2728d216fc"
        subnets:
            description: The associated subnets.
            type: list
            sample: []
        "provider:physical_network":
            description: The physical network where this network object is implemented.
            type: str
            sample: my_vlan_net
        "provider:network_type":
            description: The type of physical network that maps to this network resource.
            type: str
            sample: vlan
        "provider:segmentation_id":
            description: An isolated segment on the physical network.
            type: str
            sample: 101
'''

from ansible_collections.openstack.cloud.plugins.module_utils.openstack import OpenStackModule


class NetworkModule(OpenStackModule):

    argument_spec = dict(
        name=dict(required=True),
        shared=dict(default=False, type='bool'),
        admin_state_up=dict(default=True, type='bool'),
        external=dict(default=False, type='bool'),
        provider_physical_network=dict(required=False),
        provider_network_type=dict(required=False),
        provider_segmentation_id=dict(required=False, type='int'),
        state=dict(default='present', choices=['absent', 'present']),
        project=dict(default=None),
        port_security_enabled=dict(type='bool', min_ver='0.18.0'),
        mtu_size=dict(required=False, type='int', min_ver='0.18.0', aliases=['mtu']),
        dns_domain=dict(required=False, min_ver='0.29.0')
    )

    def run(self):

        state = self.params['state']
        name = self.params['name']
        shared = self.params['shared']
        admin_state_up = self.params['admin_state_up']
        external = self.params['external']
        provider_physical_network = self.params['provider_physical_network']
        provider_network_type = self.params['provider_network_type']
        provider_segmentation_id = self.params['provider_segmentation_id']
        project = self.params['project']

        kwargs = self.check_versioned(
            mtu_size=self.params['mtu_size'], port_security_enabled=self.params['port_security_enabled'],
            dns_domain=self.params['dns_domain']
        )

        if project is not None:
            proj = self.conn.get_project(project)
            if proj is None:
                self.fail_json(msg='Project %s could not be found' % project)
            project_id = proj['id']
            filters = {'tenant_id': project_id}
        else:
            project_id = None
            filters = None
        net = self.conn.get_network(name, filters=filters)

        if state == 'present':
            if not net:
                provider = {}
                if provider_physical_network:
                    provider['physical_network'] = provider_physical_network
                if provider_network_type:
                    provider['network_type'] = provider_network_type
                if provider_segmentation_id:
                    provider['segmentation_id'] = provider_segmentation_id

                if project_id is not None:
                    net = self.conn.create_network(name, shared, admin_state_up,
                                                   external, provider, project_id,
                                                   **kwargs)
                else:
                    net = self.conn.create_network(name, shared, admin_state_up,
                                                   external, provider,
                                                   **kwargs)
                changed = True
            else:
                changed = False
            self.exit(changed=changed, network=net, id=net['id'])

        elif state == 'absent':
            if not net:
                self.exit(changed=False)
            else:
                self.conn.delete_network(name)
                self.exit(changed=True)


def main():
    module = NetworkModule()
    module()


if __name__ == '__main__':
    main()
