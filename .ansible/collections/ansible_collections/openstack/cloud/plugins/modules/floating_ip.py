#!/usr/bin/python

# Copyright: (c) 2015, Hewlett-Packard Development Company, L.P.
# GNU General Public License v3.0+ (see COPYING or https://www.gnu.org/licenses/gpl-3.0.txt)

DOCUMENTATION = '''
---
module: floating_ip
author: OpenStack Ansible SIG
short_description: Add/Remove floating IP from an instance
description:
   - Add or Remove a floating IP to an instance.
   - Returns the floating IP when attaching only if I(wait=true).
   - When detaching a floating IP there might be a delay until an instance does not list the floating IP any more.
options:
   server:
     description:
        - The name or ID of the instance to which the IP address
          should be assigned.
     required: true
     type: str
   network:
     description:
        - The name or ID of a neutron external network or a nova pool name.
     type: str
   floating_ip_address:
     description:
        - A floating IP address to attach or to detach. When I(state) is present
          can be used to specify a IP address to attach. I(floating_ip_address)
          requires I(network) to be set.
     type: str
   reuse:
     description:
        - When I(state) is present, and I(floating_ip_address) is not present,
          this parameter can be used to specify whether we should try to reuse
          a floating IP address already allocated to the project.
     type: bool
     default: 'no'
   fixed_address:
     description:
        - To which fixed IP of server the floating IP address should be
          attached to.
     type: str
   nat_destination:
     description:
        - The name or id of a neutron private network that the fixed IP to
          attach floating IP is on
     aliases: ["fixed_network", "internal_network"]
     type: str
   wait:
     description:
        - When attaching a floating IP address, specify whether to wait for it to appear as attached.
        - Must be set to C(yes) for the module to return the value of the floating IP when attaching.
     type: bool
     default: 'no'
   timeout:
     description:
        - Time to wait for an IP address to appear as attached. See wait.
     required: false
     default: 60
     type: int
   state:
     description:
       - Should the resource be present or absent.
     choices: [present, absent]
     default: present
     type: str
   purge:
     description:
        - When I(state) is absent, indicates whether or not to delete the floating
          IP completely, or only detach it from the server. Default is to detach only.
     type: bool
     default: 'no'
requirements:
    - "python >= 3.6"
    - "openstacksdk"

extends_documentation_fragment:
- openstack.cloud.openstack
'''

EXAMPLES = '''
# Assign a floating IP to the first interface of `cattle001` from an existing
# external network or nova pool. A new floating IP from the first available
# external network is allocated to the project.
- openstack.cloud.floating_ip:
     cloud: dguerri
     server: cattle001

# Assign a new floating IP to the instance fixed ip `192.0.2.3` of
# `cattle001`. If a free floating IP is already allocated to the project, it is
# reused; if not, a new one is created.
- openstack.cloud.floating_ip:
     cloud: dguerri
     state: present
     reuse: yes
     server: cattle001
     network: ext_net
     fixed_address: 192.0.2.3
     wait: true
     timeout: 180

# Assign a new floating IP from the network `ext_net` to the instance fixed
# ip in network `private_net` of `cattle001`.
- openstack.cloud.floating_ip:
     cloud: dguerri
     state: present
     server: cattle001
     network: ext_net
     nat_destination: private_net
     wait: true
     timeout: 180

# Detach a floating IP address from a server
- openstack.cloud.floating_ip:
     cloud: dguerri
     state: absent
     floating_ip_address: 203.0.113.2
     server: cattle001
'''

from ansible_collections.openstack.cloud.plugins.module_utils.openstack import OpenStackModule
import itertools


class NetworkingFloatingIPModule(OpenStackModule):
    argument_spec = dict(
        server=dict(required=True),
        state=dict(default='present', choices=['absent', 'present']),
        network=dict(required=False, default=None),
        floating_ip_address=dict(required=False, default=None),
        reuse=dict(required=False, type='bool', default=False),
        fixed_address=dict(required=False, default=None),
        nat_destination=dict(required=False, default=None,
                             aliases=['fixed_network', 'internal_network']),
        wait=dict(required=False, type='bool', default=False),
        timeout=dict(required=False, type='int', default=60),
        purge=dict(required=False, type='bool', default=False),
    )

    module_kwargs = dict(
        required_if=[
            ['state', 'absent', ['floating_ip_address']]
        ],
        required_by=dict(
            floating_ip_address=('network',)
        )
    )

    def _get_floating_ip(self, floating_ip_address):
        f_ips = self.conn.search_floating_ips(
            filters={'floating_ip_address': floating_ip_address})

        if not f_ips:
            return None

        return f_ips[0]

    def _list_floating_ips(self, server):
        return itertools.chain.from_iterable([
            (addr['addr'] for addr in server.addresses[net] if addr['OS-EXT-IPS:type'] == 'floating')
            for net in server.addresses
        ])

    def _match_floating_ip(self, server,
                           floating_ip_address,
                           network_id,
                           fixed_address,
                           nat_destination):

        if floating_ip_address:
            return self._get_floating_ip(floating_ip_address)
        elif not fixed_address and nat_destination:
            nat_destination_name = self.conn.get_network(nat_destination)['name']
            return next(
                (self._get_floating_ip(addr['addr'])
                 for addr in server.addresses.get(nat_destination_name, [])
                 if addr['OS-EXT-IPS:type'] == 'floating'),
                None)
        else:
            # not floating_ip_address and (fixed_address or not nat_destination)

            # get any of the floating ips that matches fixed_address and/or network
            f_ip_addrs = self._list_floating_ips(server)
            f_ips = [f_ip for f_ip in self.conn.list_floating_ips() if f_ip['floating_ip_address'] in f_ip_addrs]
            return next(
                (f_ip for f_ip in f_ips
                 if ((fixed_address and f_ip.fixed_ip_address == fixed_address) or not fixed_address)
                 and ((network_id and f_ip.network == network_id) or not network_id)),
                None)

    def run(self):
        server_name_or_id = self.params['server']
        state = self.params['state']
        network = self.params['network']
        floating_ip_address = self.params['floating_ip_address']
        reuse = self.params['reuse']
        fixed_address = self.params['fixed_address']
        nat_destination = self.params['nat_destination']
        wait = self.params['wait']
        timeout = self.params['timeout']
        purge = self.params['purge']

        server = self.conn.get_server(server_name_or_id)
        if not server:
            self.fail_json(
                msg="server {0} not found".format(server_name_or_id))

        # Extract floating ips from server
        f_ip_addrs = self._list_floating_ips(server)

        # Get details about requested floating ip
        f_ip = self._get_floating_ip(floating_ip_address) if floating_ip_address else None

        if network:
            network_id = self.conn.get_network(name_or_id=network)["id"]
        else:
            network_id = None

        if state == 'present':
            if floating_ip_address and f_ip and floating_ip_address in f_ip_addrs:
                # Floating ip address has been assigned to server
                self.exit_json(changed=False, floating_ip=f_ip)

            if f_ip and f_ip['attached'] and floating_ip_address not in f_ip_addrs:
                # Requested floating ip has been attached to different server
                self.fail_json(msg="floating-ip {floating_ip_address} already has been attached to different server"
                                   .format(floating_ip_address=floating_ip_address))

            if not floating_ip_address:
                # No specific floating ip requested, i.e. if any floating ip is already assigned to server,
                # check that it matches requirements.

                if not fixed_address and nat_destination:
                    # Check if we have any floating ip on the given nat_destination network
                    nat_destination_name = self.conn.get_network(nat_destination)['name']
                    for addr in server.addresses.get(nat_destination_name, []):
                        if addr['OS-EXT-IPS:type'] == 'floating':
                            # A floating ip address has been assigned to the requested nat_destination
                            f_ip = self._get_floating_ip(addr['addr'])
                            self.exit_json(changed=False, floating_ip=f_ip)
                # else fixed_address or not nat_destination, hence an
                # analysis of all floating ips of server is required
                f_ips = [f_ip for f_ip in self.conn.list_floating_ips() if f_ip['floating_ip_address'] in f_ip_addrs]
                for f_ip in f_ips:
                    if network_id and f_ip.network != network_id:
                        # requested network does not match network of floating ip
                        continue

                    if not fixed_address and not nat_destination:
                        # any floating ip will fullfil these requirements
                        self.exit_json(changed=False, floating_ip=f_ip)

                    if fixed_address and f_ip.fixed_ip_address == fixed_address:
                        # a floating ip address has been assigned that points to the requested fixed_address
                        self.exit_json(changed=False, floating_ip=f_ip)

            if floating_ip_address and not f_ip:
                # openstacksdk's create_ip requires floating_ip_address and floating_network_id to be set
                self.conn.network.create_ip(floating_ip_address=floating_ip_address, floating_network_id=network_id)
            # Else floating ip either does not exist or has not been attached yet

            # Both floating_ip_address and network are mutually exclusive in add_ips_to_server, i.e.
            # add_ips_to_server will ignore floating_ip_address if network is set
            # Ref.: https://github.com/openstack/openstacksdk/blob/a6b0ece2821ea79330c4067100295f6bdcbe456e/openstack/cloud/_floating_ip.py#L987
            server = self.conn.add_ips_to_server(
                server=server,
                ips=floating_ip_address,
                ip_pool=network if not floating_ip_address else None,
                reuse=reuse,
                fixed_address=fixed_address,
                wait=wait,
                timeout=timeout, nat_destination=nat_destination)

            # Update the floating ip status
            f_ip = self._match_floating_ip(server, floating_ip_address, network_id, fixed_address, nat_destination)
            self.exit_json(changed=True, floating_ip=f_ip)

        elif state == 'absent':
            f_ip = self._match_floating_ip(server, floating_ip_address, network_id, fixed_address, nat_destination)
            if not f_ip:
                # Nothing to detach
                self.exit_json(changed=False)
            changed = False

            if f_ip["fixed_ip_address"]:
                self.conn.detach_ip_from_server(server_id=server['id'], floating_ip_id=f_ip['id'])
                # OpenStackSDK sets {"port_id": None} to detach a floating ip from an instance,
                # but there might be a delay until a server does not list it in addresses any more.

                # Update the floating IP status
                f_ip = self.conn.get_floating_ip(id=f_ip['id'])
                changed = True

            if purge:
                self.conn.delete_floating_ip(f_ip['id'])
                self.exit_json(changed=True)
            self.exit_json(changed=changed, floating_ip=f_ip)


def main():
    module = NetworkingFloatingIPModule()
    module()


if __name__ == '__main__':
    main()
