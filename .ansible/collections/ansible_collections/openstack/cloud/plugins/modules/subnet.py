#!/usr/bin/python
# coding: utf-8 -*-

# (c) 2013, Benno Joy <benno@ansible.com>
# GNU General Public License v3.0+ (see COPYING or https://www.gnu.org/licenses/gpl-3.0.txt)

DOCUMENTATION = '''
---
module: subnet
short_description: Add/Remove subnet to an OpenStack network
author: OpenStack Ansible SIG
description:
   - Add or Remove a subnet to an OpenStack network
options:
   state:
     description:
        - Indicate desired state of the resource
     choices: ['present', 'absent']
     default: present
     type: str
   network_name:
     description:
        - Name of the network to which the subnet should be attached
        - Required when I(state) is 'present'
     type: str
   name:
     description:
       - The name of the subnet that should be created. Although Neutron
         allows for non-unique subnet names, this module enforces subnet
         name uniqueness.
     required: true
     type: str
   cidr:
     description:
        - The CIDR representation of the subnet that should be assigned to
          the subnet. Required when I(state) is 'present' and a subnetpool
          is not specified.
     type: str
   ip_version:
     description:
        - The IP version of the subnet 4 or 6
     default: '4'
     type: str
     choices: ['4', '6']
   enable_dhcp:
     description:
        - Whether DHCP should be enabled for this subnet.
     type: bool
     default: 'yes'
   gateway_ip:
     description:
        - The ip that would be assigned to the gateway for this subnet
     type: str
   no_gateway_ip:
     description:
        - The gateway IP would not be assigned for this subnet
     type: bool
     default: 'no'
   dns_nameservers:
     description:
        - List of DNS nameservers for this subnet.
     type: list
     elements: str
   allocation_pool_start:
     description:
        - From the subnet pool the starting address from which the IP should
          be allocated.
     type: str
   allocation_pool_end:
     description:
        - From the subnet pool the last IP that should be assigned to the
          virtual machines.
     type: str
   host_routes:
     description:
        - A list of host route dictionaries for the subnet.
     type: list
     elements: dict
     suboptions:
        destination:
           description: The destination network (CIDR).
           type: str
           required: true
        nexthop:
           description: The next hop (aka gateway) for the I(destination).
           type: str
           required: true
   ipv6_ra_mode:
     description:
        - IPv6 router advertisement mode
     choices: ['dhcpv6-stateful', 'dhcpv6-stateless', 'slaac']
     type: str
   ipv6_address_mode:
     description:
        - IPv6 address mode
     choices: ['dhcpv6-stateful', 'dhcpv6-stateless', 'slaac']
     type: str
   use_default_subnetpool:
     description:
        - Use the default subnetpool for I(ip_version) to obtain a CIDR.
     type: bool
     default: 'no'
   project:
     description:
        - Project name or ID containing the subnet (name admin-only)
     type: str
   extra_specs:
     description:
        - Dictionary with extra key/value pairs passed to the API
     required: false
     default: {}
     type: dict
requirements:
    - "python >= 3.6"
    - "openstacksdk"

extends_documentation_fragment:
- openstack.cloud.openstack
'''

EXAMPLES = '''
# Create a new (or update an existing) subnet on the specified network
- openstack.cloud.subnet:
    state: present
    network_name: network1
    name: net1subnet
    cidr: 192.168.0.0/24
    dns_nameservers:
       - 8.8.8.7
       - 8.8.8.8
    host_routes:
       - destination: 0.0.0.0/0
         nexthop: 12.34.56.78
       - destination: 192.168.0.0/24
         nexthop: 192.168.0.1

# Delete a subnet
- openstack.cloud.subnet:
    state: absent
    name: net1subnet

# Create an ipv6 stateless subnet
- openstack.cloud.subnet:
    state: present
    name: intv6
    network_name: internal
    ip_version: 6
    cidr: 2db8:1::/64
    dns_nameservers:
        - 2001:4860:4860::8888
        - 2001:4860:4860::8844
    ipv6_ra_mode: dhcpv6-stateless
    ipv6_address_mode: dhcpv6-stateless
'''

from ansible_collections.openstack.cloud.plugins.module_utils.openstack import OpenStackModule


class SubnetModule(OpenStackModule):
    ipv6_mode_choices = ['dhcpv6-stateful', 'dhcpv6-stateless', 'slaac']
    argument_spec = dict(
        name=dict(type='str', required=True),
        network_name=dict(type='str'),
        cidr=dict(type='str'),
        ip_version=dict(type='str', default='4', choices=['4', '6']),
        enable_dhcp=dict(type='bool', default=True),
        gateway_ip=dict(type='str'),
        no_gateway_ip=dict(type='bool', default=False),
        dns_nameservers=dict(type='list', default=None, elements='str'),
        allocation_pool_start=dict(type='str'),
        allocation_pool_end=dict(type='str'),
        host_routes=dict(type='list', default=None, elements='dict'),
        ipv6_ra_mode=dict(type='str', choices=ipv6_mode_choices),
        ipv6_address_mode=dict(type='str', choices=ipv6_mode_choices),
        use_default_subnetpool=dict(type='bool', default=False),
        extra_specs=dict(type='dict', default=dict()),
        state=dict(type='str', default='present', choices=['absent', 'present']),
        project=dict(type='str'),
    )

    module_kwargs = dict(
        supports_check_mode=True,
        required_together=[['allocation_pool_end', 'allocation_pool_start']]
    )

    def _can_update(self, subnet, filters=None):
        """Check for differences in non-updatable values"""
        network_name = self.params['network_name']
        ip_version = int(self.params['ip_version'])
        ipv6_ra_mode = self.params['ipv6_ra_mode']
        ipv6_a_mode = self.params['ipv6_address_mode']

        if network_name:
            network = self.conn.get_network(network_name, filters)
            if network:
                netid = network['id']
                if netid != subnet['network_id']:
                    self.fail_json(msg='Cannot update network_name in existing subnet')
            else:
                self.fail_json(msg='No network found for %s' % network_name)

        if ip_version and subnet['ip_version'] != ip_version:
            self.fail_json(msg='Cannot update ip_version in existing subnet')
        if ipv6_ra_mode and subnet.get('ipv6_ra_mode', None) != ipv6_ra_mode:
            self.fail_json(msg='Cannot update ipv6_ra_mode in existing subnet')
        if ipv6_a_mode and subnet.get('ipv6_address_mode', None) != ipv6_a_mode:
            self.fail_json(msg='Cannot update ipv6_address_mode in existing subnet')

    def _needs_update(self, subnet, filters=None):
        """Check for differences in the updatable values."""

        # First check if we are trying to update something we're not allowed to
        self._can_update(subnet, filters)

        # now check for the things we are allowed to update
        enable_dhcp = self.params['enable_dhcp']
        subnet_name = self.params['name']
        pool_start = self.params['allocation_pool_start']
        pool_end = self.params['allocation_pool_end']
        gateway_ip = self.params['gateway_ip']
        no_gateway_ip = self.params['no_gateway_ip']
        dns = self.params['dns_nameservers']
        host_routes = self.params['host_routes']
        if pool_start and pool_end:
            pool = dict(start=pool_start, end=pool_end)
        else:
            pool = None

        changes = dict()
        if subnet['enable_dhcp'] != enable_dhcp:
            changes['enable_dhcp'] = enable_dhcp
        if subnet_name and subnet['name'] != subnet_name:
            changes['subnet_name'] = subnet_name
        if pool and (not subnet['allocation_pools'] or subnet['allocation_pools'] != [pool]):
            changes['allocation_pools'] = [pool]
        if gateway_ip and subnet['gateway_ip'] != gateway_ip:
            changes['gateway_ip'] = gateway_ip
        if dns and sorted(subnet['dns_nameservers']) != sorted(dns):
            changes['dns_nameservers'] = dns
        if host_routes:
            curr_hr = sorted(subnet['host_routes'], key=lambda t: t.keys())
            new_hr = sorted(host_routes, key=lambda t: t.keys())
            if curr_hr != new_hr:
                changes['host_routes'] = host_routes
        if no_gateway_ip and subnet['gateway_ip']:
            changes['disable_gateway_ip'] = no_gateway_ip
        return changes

    def _system_state_change(self, subnet, filters=None):
        state = self.params['state']
        if state == 'present':
            if not subnet:
                return True
            return bool(self._needs_update(subnet, filters))
        if state == 'absent' and subnet:
            return True
        return False

    def run(self):

        state = self.params['state']
        network_name = self.params['network_name']
        cidr = self.params['cidr']
        ip_version = self.params['ip_version']
        enable_dhcp = self.params['enable_dhcp']
        subnet_name = self.params['name']
        gateway_ip = self.params['gateway_ip']
        no_gateway_ip = self.params['no_gateway_ip']
        dns = self.params['dns_nameservers']
        pool_start = self.params['allocation_pool_start']
        pool_end = self.params['allocation_pool_end']
        host_routes = self.params['host_routes']
        ipv6_ra_mode = self.params['ipv6_ra_mode']
        ipv6_a_mode = self.params['ipv6_address_mode']
        use_default_subnetpool = self.params['use_default_subnetpool']
        project = self.params.pop('project')
        extra_specs = self.params['extra_specs']

        # Check for required parameters when state == 'present'
        if state == 'present':
            if not self.params['network_name']:
                self.fail(msg='network_name required with present state')
            if (
                not self.params['cidr']
                and not use_default_subnetpool
                and not extra_specs.get('subnetpool_id', False)
            ):
                self.fail(msg='cidr or use_default_subnetpool or '
                          'subnetpool_id required with present state')

        if pool_start and pool_end:
            pool = [dict(start=pool_start, end=pool_end)]
        else:
            pool = None

        if no_gateway_ip and gateway_ip:
            self.fail_json(msg='no_gateway_ip is not allowed with gateway_ip')

        if project is not None:
            proj = self.conn.get_project(project)
            if proj is None:
                self.fail_json(msg='Project %s could not be found' % project)
            project_id = proj['id']
            filters = {'tenant_id': project_id}
        else:
            project_id = None
            filters = None

        subnet = self.conn.get_subnet(subnet_name, filters=filters)

        if self.ansible.check_mode:
            self.exit_json(changed=self._system_state_change(subnet, filters))

        if state == 'present':
            if not subnet:
                kwargs = dict(
                    cidr=cidr,
                    ip_version=ip_version,
                    enable_dhcp=enable_dhcp,
                    subnet_name=subnet_name,
                    gateway_ip=gateway_ip,
                    disable_gateway_ip=no_gateway_ip,
                    dns_nameservers=dns,
                    allocation_pools=pool,
                    host_routes=host_routes,
                    ipv6_ra_mode=ipv6_ra_mode,
                    ipv6_address_mode=ipv6_a_mode,
                    tenant_id=project_id)
                dup_args = set(kwargs.keys()) & set(extra_specs.keys())
                if dup_args:
                    raise ValueError('Duplicate key(s) {0} in extra_specs'
                                     .format(list(dup_args)))
                if use_default_subnetpool:
                    kwargs['use_default_subnetpool'] = use_default_subnetpool
                kwargs = dict(kwargs, **extra_specs)
                subnet = self.conn.create_subnet(network_name, **kwargs)
                changed = True
            else:
                changes = self._needs_update(subnet, filters)
                if changes:
                    subnet = self.conn.update_subnet(subnet['id'], **changes)
                    changed = True
                else:
                    changed = False
            self.exit_json(changed=changed,
                           subnet=subnet,
                           id=subnet['id'])

        elif state == 'absent':
            if not subnet:
                changed = False
            else:
                changed = True
                self.conn.delete_subnet(subnet_name)
            self.exit_json(changed=changed)


def main():
    module = SubnetModule()
    module()


if __name__ == '__main__':
    main()
