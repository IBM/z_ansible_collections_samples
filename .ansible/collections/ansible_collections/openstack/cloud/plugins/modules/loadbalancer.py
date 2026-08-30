#!/usr/bin/python

# Copyright (c) 2018 Catalyst Cloud Ltd.
# GNU General Public License v3.0+ (see COPYING or https://www.gnu.org/licenses/gpl-3.0.txt)

DOCUMENTATION = '''
---
module: loadbalancer
short_description: Add/Delete load balancer from OpenStack Cloud
author: OpenStack Ansible SIG
description:
  - Add or Remove load balancer from the OpenStack load-balancer
    service(Octavia). Load balancer update is not supported for now.
options:
  name:
    description:
      - The name of the load balancer.
    required: true
    type: str
  state:
    description:
      - Should the resource be present or absent.
    choices: [present, absent]
    default: present
    type: str
  flavor:
    description:
      - The flavor of the load balancer.
    type: str
  vip_network:
    description:
      - The name or id of the network for the virtual IP of the load balancer.
        One of I(vip_network), I(vip_subnet), or I(vip_port) must be specified
        for creation.
    type: str
  vip_subnet:
    description:
      - The name or id of the subnet for the virtual IP of the load balancer.
        One of I(vip_network), I(vip_subnet), or I(vip_port) must be specified
        for creation.
    type: str
  vip_port:
    description:
      - The name or id of the load balancer virtual IP port. One of
        I(vip_network), I(vip_subnet), or I(vip_port) must be specified for
        creation.
    type: str
  vip_address:
    description:
      - IP address of the load balancer virtual IP.
    type: str
  public_ip_address:
    description:
      - Public IP address associated with the VIP.
    type: str
  auto_public_ip:
    description:
      - Allocate a public IP address and associate with the VIP automatically.
    type: bool
    default: 'no'
  public_network:
    description:
      - The name or ID of a Neutron external network.
    type: str
  delete_public_ip:
    description:
      - When C(state=absent) and this option is true, any public IP address
        associated with the VIP will be deleted along with the load balancer.
    type: bool
    default: 'no'
  listeners:
    description:
      - A list of listeners that attached to the load balancer.
    suboptions:
      name:
        description:
          - The listener name or ID.
      protocol:
        description:
          - The protocol for the listener.
        default: HTTP
      protocol_port:
        description:
          - The protocol port number for the listener.
        default: 80
      allowed_cidrs:
        description:
          - A list of IPv4, IPv6 or mix of both CIDRs to be allowed access to the listener. The default is all allowed.
            When a list of CIDRs is provided, the default switches to deny all.
            Ignored on unsupported Octavia versions (less than 2.12)
        default: []
      pool:
        description:
          - The pool attached to the listener.
        suboptions:
          name:
            description:
              - The pool name or ID.
          protocol:
            description:
              - The protocol for the pool.
            default: HTTP
          lb_algorithm:
            description:
              - The load balancing algorithm for the pool.
            default: ROUND_ROBIN
          members:
            description:
              - A list of members that added to the pool.
            suboptions:
              name:
                description:
                  - The member name or ID.
              address:
                description:
                  - The IP address of the member.
              protocol_port:
                description:
                  - The protocol port number for the member.
                default: 80
              subnet:
                description:
                  - The name or ID of the subnet the member service is
                    accessible from.
    elements: dict
    type: list
  wait:
    description:
      - If the module should wait for the load balancer to be created or
        deleted.
    type: bool
    default: 'yes'
  timeout:
    description:
      - The amount of time the module should wait.
    default: 180
    type: int
requirements:
    - "python >= 3.6"
    - "openstacksdk"

extends_documentation_fragment:
- openstack.cloud.openstack
'''

RETURN = '''
id:
    description: The load balancer UUID.
    returned: On success when C(state=present)
    type: str
    sample: "39007a7e-ee4f-4d13-8283-b4da2e037c69"
loadbalancer:
    description: Dictionary describing the load balancer.
    returned: On success when C(state=present)
    type: complex
    contains:
        id:
            description: Unique UUID.
            type: str
            sample: "39007a7e-ee4f-4d13-8283-b4da2e037c69"
        name:
            description: Name given to the load balancer.
            type: str
            sample: "lingxian_test"
        vip_network_id:
            description: Network ID the load balancer virtual IP port belongs in.
            type: str
            sample: "f171db43-56fd-41cf-82d7-4e91d741762e"
        vip_subnet_id:
            description: Subnet ID the load balancer virtual IP port belongs in.
            type: str
            sample: "c53e3c70-9d62-409a-9f71-db148e7aa853"
        vip_port_id:
            description: The load balancer virtual IP port ID.
            type: str
            sample: "2061395c-1c01-47ab-b925-c91b93df9c1d"
        vip_address:
            description: The load balancer virtual IP address.
            type: str
            sample: "192.168.2.88"
        public_vip_address:
            description: The load balancer public VIP address.
            type: str
            sample: "10.17.8.254"
        provisioning_status:
            description: The provisioning status of the load balancer.
            type: str
            sample: "ACTIVE"
        operating_status:
            description: The operating status of the load balancer.
            type: str
            sample: "ONLINE"
        is_admin_state_up:
            description: The administrative state of the load balancer.
            type: bool
            sample: true
        listeners:
            description: The associated listener IDs, if any.
            type: list
            sample: [{"id": "7aa1b380-beec-459c-a8a7-3a4fb6d30645"}, {"id": "692d06b8-c4f8-4bdb-b2a3-5a263cc23ba6"}]
        pools:
            description: The associated pool IDs, if any.
            type: list
            sample: [{"id": "27b78d92-cee1-4646-b831-e3b90a7fa714"}, {"id": "befc1fb5-1992-4697-bdb9-eee330989344"}]
'''

EXAMPLES = '''
# Create a load balancer by specifying the VIP subnet.
- openstack.cloud.loadbalancer:
    auth:
      auth_url: https://identity.example.com
      username: admin
      password: passme
      project_name: admin
    state: present
    name: my_lb
    vip_subnet: my_subnet
    timeout: 150

# Create a load balancer by specifying the VIP network and the IP address.
- openstack.cloud.loadbalancer:
    auth:
      auth_url: https://identity.example.com
      username: admin
      password: passme
      project_name: admin
    state: present
    name: my_lb
    vip_network: my_network
    vip_address: 192.168.0.11

# Create a load balancer together with its sub-resources in the 'all in one'
# way. A public IP address is also allocated to the load balancer VIP.
- openstack.cloud.loadbalancer:
    auth:
      auth_url: https://identity.example.com
      username: admin
      password: passme
      project_name: admin
    name: lingxian_test
    state: present
    vip_subnet: kong_subnet
    auto_public_ip: yes
    public_network: public
    listeners:
      - name: lingxian_80
        protocol: TCP
        protocol_port: 80
        pool:
          name: lingxian_80_pool
          protocol: TCP
          members:
            - name: mywebserver1
              address: 192.168.2.81
              protocol_port: 80
              subnet: webserver_subnet
      - name: lingxian_8080
        protocol: TCP
        protocol_port: 8080
        pool:
          name: lingxian_8080-pool
          protocol: TCP
          members:
            - name: mywebserver2
              address: 192.168.2.82
              protocol_port: 8080
    wait: yes
    timeout: 600

# Delete a load balancer(and all its related resources)
- openstack.cloud.loadbalancer:
    auth:
      auth_url: https://identity.example.com
      username: admin
      password: passme
      project_name: admin
    state: absent
    name: my_lb

# Delete a load balancer(and all its related resources) together with the
# public IP address(if any) attached to it.
- openstack.cloud.loadbalancer:
    auth:
      auth_url: https://identity.example.com
      username: admin
      password: passme
      project_name: admin
    state: absent
    name: my_lb
    delete_public_ip: yes
'''

import time
from ansible_collections.openstack.cloud.plugins.module_utils.openstack import OpenStackModule


class LoadBalancerModule(OpenStackModule):

    def _wait_for_pool(self, pool, provisioning_status, operating_status, failures, interval=5):
        """Wait for pool to be in a particular provisioning and operating status."""
        timeout = self.params['timeout']  # reuse loadbalancer timeout

        total_sleep = 0
        if failures is None:
            failures = []

        while total_sleep < timeout:
            pool = self.conn.load_balancer.find_pool(name_or_id=pool.id)
            if pool:
                if pool.provisioning_status == provisioning_status and pool.operating_status == operating_status:
                    return None
                if pool.provisioning_status in failures:
                    self.fail_json(
                        msg="Pool %s transitioned to failure state %s" %
                            (pool.id, pool.provisioning_status)
                    )
            else:
                if provisioning_status == "DELETED":
                    return None
                else:
                    self.fail_json(
                        msg="Pool %s transitioned to DELETED" % pool.id
                    )

            time.sleep(interval)
            total_sleep += interval

    def _wait_for_lb(self, lb, status, failures, interval=5):
        """Wait for load balancer to be in a particular provisioning status."""
        timeout = self.params['timeout']

        total_sleep = 0
        if failures is None:
            failures = []

        while total_sleep < timeout:
            lb = self.conn.load_balancer.find_load_balancer(lb.id)

            if lb:
                if lb.provisioning_status == status:
                    return None
                if lb.provisioning_status in failures:
                    self.fail_json(
                        msg="Load Balancer %s transitioned to failure state %s" %
                            (lb.id, lb.provisioning_status)
                    )
            else:
                if status == "DELETED":
                    return None
                else:
                    self.fail_json(
                        msg="Load Balancer %s transitioned to DELETED" % lb.id
                    )

            time.sleep(interval)
            total_sleep += interval

        self.fail_json(
            msg="Timeout waiting for Load Balancer %s to transition to %s" %
                (lb.id, status)
        )

    argument_spec = dict(
        name=dict(required=True),
        flavor=dict(required=False),
        state=dict(default='present', choices=['absent', 'present']),
        vip_network=dict(required=False),
        vip_subnet=dict(required=False),
        vip_port=dict(required=False),
        vip_address=dict(required=False),
        listeners=dict(type='list', default=[], elements='dict'),
        public_ip_address=dict(required=False, default=None),
        auto_public_ip=dict(required=False, default=False, type='bool'),
        public_network=dict(required=False),
        delete_public_ip=dict(required=False, default=False, type='bool'),
    )
    module_kwargs = dict(supports_check_mode=True)

    def run(self):
        flavor = self.params['flavor']
        vip_network = self.params['vip_network']
        vip_subnet = self.params['vip_subnet']
        vip_port = self.params['vip_port']
        listeners = self.params['listeners']
        public_vip_address = self.params['public_ip_address']
        allocate_fip = self.params['auto_public_ip']
        delete_fip = self.params['delete_public_ip']
        public_network = self.params['public_network']

        vip_network_id = None
        vip_subnet_id = None
        vip_port_id = None
        flavor_id = None

        try:
            max_microversion = 1
            max_majorversion = 2
            changed = False
            lb = self.conn.load_balancer.find_load_balancer(
                name_or_id=self.params['name'])

            if self.params['state'] == 'present':
                if lb and self.ansible.check_mode:
                    self.exit_json(changed=False)
                if lb:
                    self.exit_json(changed=False)
                ver_data = self.conn.load_balancer.get_all_version_data()
                region = list(ver_data.keys())[0]
                interface_type = list(ver_data[region].keys())[0]
                versions = ver_data[region][interface_type]['load-balancer']
                for ver in versions:
                    if ver['status'] == 'CURRENT':
                        curversion = ver['version'].split(".")
                        max_majorversion = int(curversion[0])
                        max_microversion = int(curversion[1])

                if not lb:
                    if self.ansible.check_mode:
                        self.exit_json(changed=True)

                    if not (vip_network or vip_subnet or vip_port):
                        self.fail_json(
                            msg="One of vip_network, vip_subnet, or vip_port must "
                                "be specified for load balancer creation"
                        )

                    if flavor:
                        _flavor = self.conn.load_balancer.find_flavor(flavor)
                        if not _flavor:
                            self.fail_json(
                                msg='flavor %s not found' % flavor
                            )
                        flavor_id = _flavor.id

                    if vip_network:
                        network = self.conn.get_network(vip_network)
                        if not network:
                            self.fail_json(
                                msg='network %s is not found' % vip_network
                            )
                        vip_network_id = network.id
                    if vip_subnet:
                        subnet = self.conn.get_subnet(vip_subnet)
                        if not subnet:
                            self.fail_json(
                                msg='subnet %s is not found' % vip_subnet
                            )
                        vip_subnet_id = subnet.id
                    if vip_port:
                        port = self.conn.get_port(vip_port)

                        if not port:
                            self.fail_json(
                                msg='port %s is not found' % vip_port
                            )
                        vip_port_id = port.id
                    lbargs = {"name": self.params['name'],
                              "vip_network_id": vip_network_id,
                              "vip_subnet_id": vip_subnet_id,
                              "vip_port_id": vip_port_id,
                              "vip_address": self.params['vip_address']
                              }
                    if flavor_id is not None:
                        lbargs["flavor_id"] = flavor_id

                    lb = self.conn.load_balancer.create_load_balancer(**lbargs)

                    changed = True

                if not listeners and not self.params['wait']:
                    self.exit_json(
                        changed=changed,
                        loadbalancer=lb.to_dict(),
                        id=lb.id
                    )

                self._wait_for_lb(lb, "ACTIVE", ["ERROR"])

                for listener_def in listeners:
                    listener_name = listener_def.get("name")
                    pool_def = listener_def.get("pool")

                    if not listener_name:
                        self.fail_json(msg='listener name is required')

                    listener = self.conn.load_balancer.find_listener(
                        name_or_id=listener_name
                    )

                    if not listener:
                        self._wait_for_lb(lb, "ACTIVE", ["ERROR"])

                        protocol = listener_def.get("protocol", "HTTP")
                        protocol_port = listener_def.get("protocol_port", 80)
                        allowed_cidrs = listener_def.get("allowed_cidrs", [])
                        listenerargs = {"name": listener_name,
                                        "loadbalancer_id": lb.id,
                                        "protocol": protocol,
                                        "protocol_port": protocol_port
                                        }
                        if max_microversion >= 12 and max_majorversion >= 2:
                            listenerargs['allowed_cidrs'] = allowed_cidrs
                        listener = self.conn.load_balancer.create_listener(**listenerargs)
                        changed = True

                # Ensure pool in the listener.
                    if pool_def:
                        pool_name = pool_def.get("name")
                        members = pool_def.get('members', [])

                        if not pool_name:
                            self.fail_json(msg='pool name is required')

                        pool = self.conn.load_balancer.find_pool(name_or_id=pool_name)

                        if not pool:
                            self._wait_for_lb(lb, "ACTIVE", ["ERROR"])

                            protocol = pool_def.get("protocol", "HTTP")
                            lb_algorithm = pool_def.get("lb_algorithm",
                                                        "ROUND_ROBIN")

                            pool = self.conn.load_balancer.create_pool(
                                name=pool_name,
                                listener_id=listener.id,
                                protocol=protocol,
                                lb_algorithm=lb_algorithm
                            )
                            self._wait_for_pool(pool, "ACTIVE", "ONLINE", ["ERROR"])
                            changed = True

                    # Ensure members in the pool
                        for member_def in members:
                            member_name = member_def.get("name")
                            if not member_name:
                                self.fail_json(msg='member name is required')

                            member = self.conn.load_balancer.find_member(member_name,
                                                                         pool.id
                                                                         )

                            if not member:
                                self._wait_for_lb(lb, "ACTIVE", ["ERROR"])

                            address = member_def.get("address")
                            if not address:
                                self.fail_json(
                                    msg='member address for member %s is '
                                        'required' % member_name
                                )

                            subnet_id = member_def.get("subnet")
                            if subnet_id:
                                subnet = self.conn.get_subnet(subnet_id)
                                if not subnet:
                                    self.fail_json(
                                        msg='subnet %s for member %s is not '
                                            'found' % (subnet_id, member_name)
                                    )
                                subnet_id = subnet.id

                            protocol_port = member_def.get("protocol_port", 80)

                            member = self.conn.load_balancer.create_member(
                                pool,
                                name=member_name,
                                address=address,
                                protocol_port=protocol_port,
                                subnet_id=subnet_id
                            )
                            self._wait_for_pool(pool, "ACTIVE", "ONLINE", ["ERROR"])
                            changed = True

                # Associate public ip to the load balancer VIP. If
                # public_vip_address is provided, use that IP, otherwise, either
                # find an available public ip or create a new one.
                fip = None
                orig_public_ip = None
                new_public_ip = None
                if public_vip_address or allocate_fip:
                    ips = self.conn.network.ips(
                        port_id=lb.vip_port_id,
                        fixed_ip_address=lb.vip_address
                    )
                    ips = list(ips)
                    if ips:
                        orig_public_ip = ips[0]
                        new_public_ip = orig_public_ip.floating_ip_address

                if public_vip_address and public_vip_address != orig_public_ip:
                    fip = self.conn.network.find_ip(public_vip_address)

                    if not fip:
                        self.fail_json(
                            msg='Public IP %s is unavailable' % public_vip_address
                        )

                    # Release origin public ip first
                    self.conn.network.update_ip(
                        orig_public_ip,
                        fixed_ip_address=None,
                        port_id=None
                    )

                    # Associate new public ip
                    self.conn.network.update_ip(
                        fip,
                        fixed_ip_address=lb.vip_address,
                        port_id=lb.vip_port_id
                    )

                    new_public_ip = public_vip_address
                    changed = True
                elif allocate_fip and not orig_public_ip:
                    fip = self.conn.network.find_available_ip()
                    if not fip:
                        if not public_network:
                            self.fail_json(msg="Public network is not provided")

                        pub_net = self.conn.network.find_network(public_network)
                        if not pub_net:
                            self.fail_json(
                                msg='Public network %s not found' %
                                    public_network
                            )
                        fip = self.conn.network.create_ip(
                            floating_network_id=pub_net.id
                        )

                    self.conn.network.update_ip(
                        fip,
                        fixed_ip_address=lb.vip_address,
                        port_id=lb.vip_port_id
                    )

                    new_public_ip = fip.floating_ip_address
                    changed = True

                # Include public_vip_address in the result.
                lb = self.conn.load_balancer.find_load_balancer(name_or_id=lb.id)
                lb_dict = lb.to_dict()
                lb_dict.update({"public_vip_address": new_public_ip})

                self.exit_json(
                    changed=changed,
                    loadbalancer=lb_dict,
                    id=lb.id
                )
            elif self.params['state'] == 'absent':
                changed = False
                public_vip_address = None

                if lb:
                    if self.ansible.check_mode:
                        self.exit_json(changed=True)
                    if delete_fip:
                        ips = self.conn.network.ips(
                            port_id=lb.vip_port_id,
                            fixed_ip_address=lb.vip_address
                        )
                        ips = list(ips)
                        if ips:
                            public_vip_address = ips[0]

                    # Deleting load balancer with `cascade=False` does not make
                    # sense because the deletion will always fail if there are
                    # sub-resources.
                    self.conn.load_balancer.delete_load_balancer(lb, cascade=True)
                    changed = True

                    if self.params['wait']:
                        self._wait_for_lb(lb, "DELETED", ["ERROR"])

                    if delete_fip and public_vip_address:
                        self.conn.network.delete_ip(public_vip_address)
                        changed = True
                elif self.ansible.check_mode:
                    self.exit_json(changed=False)

                self.exit_json(changed=changed)
        except Exception as e:
            self.fail_json(msg=str(e))


def main():
    module = LoadBalancerModule()
    module()


if __name__ == "__main__":
    main()
