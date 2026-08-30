#!/usr/bin/python
#
# Copyright: Ansible Project
# GNU General Public License v3.0+ (see COPYING or https://www.gnu.org/licenses/gpl-3.0.txt)

DOCUMENTATION = '''
---
module: router
short_description: Create or delete routers from OpenStack
author: OpenStack Ansible SIG
description:
   - Create or Delete routers from OpenStack. Although Neutron allows
     routers to share the same name, this module enforces name uniqueness
     to be more user friendly.
options:
   state:
     description:
        - Indicate desired state of the resource
     choices: ['present', 'absent']
     default: present
     type: str
   name:
     description:
        - Name to be give to the router
     required: true
     type: str
   admin_state_up:
     description:
        - Desired admin state of the created or existing router.
     type: bool
     default: 'yes'
   enable_snat:
     description:
        - Enable Source NAT (SNAT) attribute.
     type: bool
   network:
     description:
        - Unique name or ID of the external gateway network.
        - required I(interfaces) or I(enable_snat) are provided.
     type: str
   project:
     description:
        - Unique name or ID of the project.
     type: str
   external_fixed_ips:
     description:
        - The IP address parameters for the external gateway network. Each
          is a dictionary with the subnet name or ID (subnet) and the IP
          address to assign on the subnet (ip). If no IP is specified,
          one is automatically assigned from that subnet.
     type: list
     elements: dict
     suboptions:
        ip:
           description: The fixed IP address to attempt to allocate.
           required: true
           type: str
        subnet:
           description: The subnet to attach the IP address to.
           type: str
   interfaces:
     description:
        - List of subnets to attach to the router internal interface. Default
          gateway associated with the subnet will be automatically attached
          with the router's internal interface.
          In order to provide an ip address different from the default
          gateway,parameters are passed as dictionary with keys as network
          name or ID (I(net)), subnet name or ID (I(subnet)) and the IP of
          port (I(portip)) from the network.
          User defined portip is often required when a multiple router need
          to be connected to a single subnet for which the default gateway has
          been already used.
     type: list
     elements: raw
requirements:
    - "python >= 3.6"
    - "openstacksdk"

extends_documentation_fragment:
- openstack.cloud.openstack
'''

EXAMPLES = '''
# Create a simple router, not attached to a gateway or subnets.
- openstack.cloud.router:
    cloud: mycloud
    state: present
    name: simple_router

# Create a simple router, not attached to a gateway or subnets for a given project.
- openstack.cloud.router:
    cloud: mycloud
    state: present
    name: simple_router
    project: myproj

# Creates a router attached to ext_network1 on an IPv4 subnet and one
# internal subnet interface.
- openstack.cloud.router:
    cloud: mycloud
    state: present
    name: router1
    network: ext_network1
    external_fixed_ips:
      - subnet: public-subnet
        ip: 172.24.4.2
    interfaces:
      - private-subnet

# Create another router with two internal subnet interfaces.One with user defined port
# ip and another with default gateway.
- openstack.cloud.router:
    cloud: mycloud
    state: present
    name: router2
    network: ext_network1
    interfaces:
      - net: private-net
        subnet: private-subnet
        portip: 10.1.1.10
      - project-subnet

# Create another router with two internal subnet interface.One with user defined port
# ip and and another with default gateway.
- openstack.cloud.router:
    cloud: mycloud
    state: present
    name: router2
    network: ext_network1
    interfaces:
      - net: private-net
        subnet: private-subnet
        portip: 10.1.1.10
      - project-subnet

# Create another router with two internal subnet interface. one with  user defined port
# ip and and another  with default gateway.
- openstack.cloud.router:
    cloud: mycloud
    state: present
    name: router2
    network: ext_network1
    interfaces:
      - net: private-net
        subnet: private-subnet
        portip: 10.1.1.10
      - project-subnet

# Update existing router1 external gateway to include the IPv6 subnet.
# Note that since 'interfaces' is not provided, any existing internal
# interfaces on an existing router will be left intact.
- openstack.cloud.router:
    cloud: mycloud
    state: present
    name: router1
    network: ext_network1
    external_fixed_ips:
      - subnet: public-subnet
        ip: 172.24.4.2
      - subnet: ipv6-public-subnet
        ip: 2001:db8::3

# Delete router1
- openstack.cloud.router:
    cloud: mycloud
    state: absent
    name: router1
'''

RETURN = '''
router:
    description: Dictionary describing the router.
    returned: On success when I(state) is 'present'
    type: complex
    contains:
        id:
            description: Router ID.
            type: str
            sample: "474acfe5-be34-494c-b339-50f06aa143e4"
        name:
            description: Router name.
            type: str
            sample: "router1"
        admin_state_up:
            description: Administrative state of the router.
            type: bool
            sample: true
        status:
            description: The router status.
            type: str
            sample: "ACTIVE"
        tenant_id:
            description: The tenant ID.
            type: str
            sample: "861174b82b43463c9edc5202aadc60ef"
        external_gateway_info:
            description: The external gateway parameters.
            type: dict
            sample: {
                      "enable_snat": true,
                      "external_fixed_ips": [
                         {
                           "ip_address": "10.6.6.99",
                           "subnet_id": "4272cb52-a456-4c20-8f3c-c26024ecfa81"
                         }
                       ]
                    }
        routes:
            description: The extra routes configuration for L3 router.
            type: list
'''

from ansible_collections.openstack.cloud.plugins.module_utils.openstack import OpenStackModule
import itertools


class RouterModule(OpenStackModule):
    argument_spec = dict(
        state=dict(default='present', choices=['absent', 'present']),
        name=dict(required=True),
        admin_state_up=dict(type='bool', default=True),
        enable_snat=dict(type='bool'),
        network=dict(default=None),
        interfaces=dict(type='list', default=None, elements='raw'),
        external_fixed_ips=dict(type='list', default=None, elements='dict'),
        project=dict(default=None)
    )

    def _get_subnet_ids_from_ports(self, ports):
        return [fixed_ip['subnet_id'] for fixed_ip in
                itertools.chain.from_iterable(port['fixed_ips'] for port in ports if 'fixed_ips' in port)]

    def _needs_update(self, router, net,
                      missing_port_ids,
                      requested_subnet_ids,
                      existing_subnet_ids,
                      router_ifs_cfg):
        """Decide if the given router needs an update."""
        if router['admin_state_up'] != self.params['admin_state_up']:
            return True
        if router['external_gateway_info']:
            # check if enable_snat is set in module params
            if self.params['enable_snat'] is not None:
                if router['external_gateway_info'].get('enable_snat', True) != self.params['enable_snat']:
                    return True
        if net:
            if not router['external_gateway_info']:
                return True
            elif router['external_gateway_info']['network_id'] != net['id']:
                return True

        # check if external_fixed_ip has to be added
        for external_fixed_ip in router_ifs_cfg['external_fixed_ips']:
            exists = False

            # compare the requested interface with existing, looking for an existing match
            for existing_if in router['external_gateway_info']['external_fixed_ips']:
                if existing_if['subnet_id'] == external_fixed_ip['subnet_id']:
                    if 'ip' in external_fixed_ip:
                        if existing_if['ip_address'] == external_fixed_ip['ip']:
                            # both subnet id and ip address match
                            exists = True
                            break
                    else:
                        # only the subnet was given, so ip doesn't matter
                        exists = True
                        break

            # this interface isn't present on the existing router
            if not exists:
                return True

        # check if external_fixed_ip has to be removed
        if router_ifs_cfg['external_fixed_ips']:
            for external_fixed_ip in router['external_gateway_info']['external_fixed_ips']:
                obsolete = True

                # compare the existing interface with requested, looking for an requested match
                for requested_if in router_ifs_cfg['external_fixed_ips']:
                    if external_fixed_ip['subnet_id'] == requested_if['subnet_id']:
                        if 'ip' in requested_if:
                            if external_fixed_ip['ip_address'] == requested_if['ip']:
                                # both subnet id and ip address match
                                obsolete = False
                                break
                        else:
                            # only the subnet was given, so ip doesn't matter
                            obsolete = False
                            break

                # this interface isn't present on the existing router
                if obsolete:
                    return True
        else:
            # no external fixed ips requested
            if router['external_gateway_info'] \
               and router['external_gateway_info']['external_fixed_ips'] \
               and len(router['external_gateway_info']['external_fixed_ips']) > 1:
                # but router has several external fixed ips
                return True

        # check if internal port has to be added
        if router_ifs_cfg['internal_ports_missing']:
            return True

        if missing_port_ids:
            return True

        # check if internal subnet has to be added or removed
        if set(requested_subnet_ids) != set(existing_subnet_ids):
            return True

        return False

    def _build_kwargs(self, router, net):
        kwargs = {
            'admin_state_up': self.params['admin_state_up'],
        }

        if router:
            kwargs['name_or_id'] = router['id']
        else:
            kwargs['name'] = self.params['name']

        if net:
            kwargs['ext_gateway_net_id'] = net['id']
            # can't send enable_snat unless we have a network
            if self.params.get('enable_snat') is not None:
                kwargs['enable_snat'] = self.params['enable_snat']

        if self.params['external_fixed_ips']:
            kwargs['ext_fixed_ips'] = []
            for iface in self.params['external_fixed_ips']:
                subnet = self.conn.get_subnet(iface['subnet'])
                d = {'subnet_id': subnet['id']}
                if 'ip' in iface:
                    d['ip_address'] = iface['ip']
                kwargs['ext_fixed_ips'].append(d)
        else:
            # no external fixed ips requested
            if router \
               and router['external_gateway_info'] \
               and router['external_gateway_info']['external_fixed_ips'] \
               and len(router['external_gateway_info']['external_fixed_ips']) > 1:
                # but router has several external fixed ips
                # keep first external fixed ip only
                fip = router['external_gateway_info']['external_fixed_ips'][0]
                kwargs['ext_fixed_ips'] = [fip]

        return kwargs

    def _build_router_interface_config(self, filters=None):
        external_fixed_ips = []
        internal_subnets = []
        internal_ports = []
        internal_ports_missing = []

        # Build external interface configuration
        if self.params['external_fixed_ips']:
            for iface in self.params['external_fixed_ips']:
                subnet = self.conn.get_subnet(iface['subnet'], filters)
                if not subnet:
                    self.fail(msg='subnet %s not found' % iface['subnet'])
                new_external_fixed_ip = {'subnet_name': subnet.name, 'subnet_id': subnet.id}
                if 'ip' in iface:
                    new_external_fixed_ip['ip'] = iface['ip']
                external_fixed_ips.append(new_external_fixed_ip)

        # Build internal interface configuration
        if self.params['interfaces']:
            internal_ips = []
            for iface in self.params['interfaces']:
                if isinstance(iface, str):
                    subnet = self.conn.get_subnet(iface, filters)
                    if not subnet:
                        self.fail(msg='subnet %s not found' % iface)
                    internal_subnets.append(subnet)

                elif isinstance(iface, dict):
                    subnet = self.conn.get_subnet(iface['subnet'], filters)
                    if not subnet:
                        self.fail(msg='subnet %s not found' % iface['subnet'])

                    net = self.conn.get_network(iface['net'])
                    if not net:
                        self.fail(msg='net %s not found' % iface['net'])

                    if "portip" not in iface:
                        # portip not set, add any ip from subnet
                        internal_subnets.append(subnet)
                    elif not iface['portip']:
                        # portip is set but has invalid value
                        self.fail(msg='put an ip in portip or remove it from list to assign default port to router')
                    else:
                        # portip has valid value
                        # look for ports whose fixed_ips.ip_address matchs portip
                        for existing_port in self.conn.list_ports(filters={'network_id': net.id}):
                            for fixed_ip in existing_port['fixed_ips']:
                                if iface['portip'] == fixed_ip['ip_address']:
                                    # portip exists in net already
                                    internal_ports.append(existing_port)
                                    internal_ips.append(fixed_ip['ip_address'])
                        if iface['portip'] not in internal_ips:
                            # no port with portip exists hence create a new port
                            internal_ports_missing.append({
                                'network_id': net.id,
                                'fixed_ips': [{'ip_address': iface['portip'], 'subnet_id': subnet.id}]
                            })

        return {
            'external_fixed_ips': external_fixed_ips,
            'internal_subnets': internal_subnets,
            'internal_ports': internal_ports,
            'internal_ports_missing': internal_ports_missing
        }

    def run(self):

        state = self.params['state']
        name = self.params['name']
        network = self.params['network']
        project = self.params['project']

        if self.params['external_fixed_ips'] and not network:
            self.fail(msg='network is required when supplying external_fixed_ips')

        if project is not None:
            proj = self.conn.get_project(project)
            if proj is None:
                self.fail(msg='Project %s could not be found' % project)
            project_id = proj['id']
            filters = {'tenant_id': project_id}
        else:
            project_id = None
            filters = None

        router = self.conn.get_router(name, filters=filters)
        net = None
        if network:
            net = self.conn.get_network(network)
            if not net:
                self.fail(msg='network %s not found' % network)

        # Validate and cache the subnet IDs so we can avoid duplicate checks
        # and expensive API calls.
        router_ifs_cfg = self._build_router_interface_config(filters)
        requested_subnet_ids = [subnet.id for subnet in router_ifs_cfg['internal_subnets']] + \
            self._get_subnet_ids_from_ports(router_ifs_cfg['internal_ports'])
        requested_port_ids = [i['id'] for i in router_ifs_cfg['internal_ports']]

        if router:
            router_ifs_internal = self.conn.list_router_interfaces(router, 'internal')
            existing_subnet_ids = self._get_subnet_ids_from_ports(router_ifs_internal)
            obsolete_subnet_ids = set(existing_subnet_ids) - set(requested_subnet_ids)
            existing_port_ids = [i['id'] for i in router_ifs_internal]

        else:
            router_ifs_internal = []
            existing_subnet_ids = []
            obsolete_subnet_ids = []
            existing_port_ids = []

        missing_port_ids = set(requested_port_ids) - set(existing_port_ids)

        if self.ansible.check_mode:
            # Check if the system state would be changed
            if state == 'absent' and router:
                changed = True
            elif state == 'absent' and not router:
                changed = False
            elif state == 'present' and not router:
                changed = True
            else:  # if state == 'present' and router
                changed = self._needs_update(router, net,
                                             missing_port_ids,
                                             requested_subnet_ids,
                                             existing_subnet_ids,
                                             router_ifs_cfg)
            self.exit_json(changed=changed)

        if state == 'present':
            changed = False

            if not router:
                changed = True

                kwargs = self._build_kwargs(router, net)
                if project_id:
                    kwargs['project_id'] = project_id
                router = self.conn.create_router(**kwargs)

                # add interface by subnet id, because user did not specify a port id
                for subnet in router_ifs_cfg['internal_subnets']:
                    self.conn.add_router_interface(router, subnet_id=subnet.id)

                # add interface by port id if user did specify a valid port id
                for port in router_ifs_cfg['internal_ports']:
                    self.conn.add_router_interface(router, port_id=port.id)

                # add port and interface if user did specify an ip address but port is missing yet
                for missing_internal_port in router_ifs_cfg['internal_ports_missing']:
                    p = self.conn.create_port(**missing_internal_port)
                    if p:
                        self.conn.add_router_interface(router, port_id=p.id)

            else:
                if self._needs_update(router, net,
                                      missing_port_ids,
                                      requested_subnet_ids,
                                      existing_subnet_ids,
                                      router_ifs_cfg):
                    changed = True
                    kwargs = self._build_kwargs(router, net)
                    updated_router = self.conn.update_router(**kwargs)

                    # Protect against update_router() not actually updating the router.
                    if not updated_router:
                        changed = False
                    else:
                        router = updated_router

                    # delete internal subnets i.e. ports
                    if obsolete_subnet_ids:
                        for port in router_ifs_internal:
                            if 'fixed_ips' in port:
                                for fip in port['fixed_ips']:
                                    if fip['subnet_id'] in obsolete_subnet_ids:
                                        self.conn.remove_router_interface(router, port_id=port['id'])
                                        changed = True

                    # add new internal interface by subnet id, because user did not specify a port id
                    for subnet in router_ifs_cfg['internal_subnets']:
                        if subnet.id not in existing_subnet_ids:
                            self.conn.add_router_interface(router, subnet_id=subnet.id)
                            changed = True

                    # add new internal interface by port id if user did specify a valid port id
                    for port_id in missing_port_ids:
                        self.conn.add_router_interface(router, port_id=port_id)
                        changed = True

                    # add new port and new internal interface if user did specify an ip address but port is missing yet
                    for missing_internal_port in router_ifs_cfg['internal_ports_missing']:
                        p = self.conn.create_port(**missing_internal_port)
                        if p:
                            self.conn.add_router_interface(router, port_id=p.id)
                            changed = True

            self.exit_json(changed=changed, router=router)

        elif state == 'absent':
            if not router:
                self.exit_json(changed=False)
            else:
                # We need to detach all internal interfaces on a router
                # before we will be allowed to delete it. Deletion can
                # still fail if e.g. floating ips are attached to the
                # router.
                for port in router_ifs_internal:
                    self.conn.remove_router_interface(router, port_id=port['id'])
                self.conn.delete_router(router['id'])
                self.exit_json(changed=True, router=router)


def main():
    module = RouterModule()
    module()


if __name__ == '__main__':
    main()
