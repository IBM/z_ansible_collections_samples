#!/usr/bin/python

# Copyright (c) 2018 Catalyst Cloud Ltd.
# GNU General Public License v3.0+ (see COPYING or https://www.gnu.org/licenses/gpl-3.0.txt)

DOCUMENTATION = '''
---
module: lb_listener
short_description: Add/Delete a listener for a load balancer from OpenStack Cloud
author: OpenStack Ansible SIG
description:
   - Add or Remove a listener for a load balancer from the OpenStack load-balancer service.
options:
   name:
     description:
        - Name that has to be given to the listener
     required: true
     type: str
   state:
     description:
       - Should the resource be present or absent.
     choices: [present, absent]
     default: present
     type: str
   loadbalancer:
     description:
        - The name or id of the load balancer that this listener belongs to.
     required: true
     type: str
   protocol:
     description:
        - The protocol for the listener.
     choices: [HTTP, HTTPS, TCP, TERMINATED_HTTPS, UDP, SCTP]
     default: HTTP
     type: str
   protocol_port:
     description:
        - The protocol port number for the listener.
     default: 80
     type: int
   timeout_client_data:
     description:
        - Client inactivity timeout in milliseconds.
     default: 50000
     type: int
   timeout_member_data:
     description:
        - Member inactivity timeout in milliseconds.
     default: 50000
     type: int
   wait:
     description:
        - If the module should wait for the load balancer to be ACTIVE.
     type: bool
     default: 'yes'
   timeout:
     description:
        - The amount of time the module should wait for the load balancer to get
          into ACTIVE state.
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
    description: The listener UUID.
    returned: On success when I(state) is 'present'
    type: str
    sample: "39007a7e-ee4f-4d13-8283-b4da2e037c69"
listener:
    description: Dictionary describing the listener.
    returned: On success when I(state) is 'present'
    type: complex
    contains:
        id:
            description: Unique UUID.
            type: str
            sample: "39007a7e-ee4f-4d13-8283-b4da2e037c69"
        name:
            description: Name given to the listener.
            type: str
            sample: "test"
        description:
            description: The listener description.
            type: str
            sample: "description"
        load_balancer_id:
            description: The load balancer UUID this listener belongs to.
            type: str
            sample: "b32eef7e-d2a6-4ea4-a301-60a873f89b3b"
        loadbalancers:
            description: A list of load balancer IDs..
            type: list
            sample: [{"id": "b32eef7e-d2a6-4ea4-a301-60a873f89b3b"}]
        provisioning_status:
            description: The provisioning status of the listener.
            type: str
            sample: "ACTIVE"
        operating_status:
            description: The operating status of the listener.
            type: str
            sample: "ONLINE"
        is_admin_state_up:
            description: The administrative state of the listener.
            type: bool
            sample: true
        protocol:
            description: The protocol for the listener.
            type: str
            sample: "HTTP"
        protocol_port:
            description: The protocol port number for the listener.
            type: int
            sample: 80
        timeout_client_data:
            description: Client inactivity timeout in milliseconds.
            type: int
            sample: 50000
        timeout_member_data:
            description: Member inactivity timeout in milliseconds.
            type: int
            sample: 50000
'''

EXAMPLES = '''
# Create a listener, wait for the loadbalancer to be active.
- openstack.cloud.lb_listener:
    cloud: mycloud
    endpoint_type: admin
    state: present
    name: test-listener
    loadbalancer: test-loadbalancer
    protocol: HTTP
    protocol_port: 8080

# Create a listener, do not wait for the loadbalancer to be active.
- openstack.cloud.lb_listener:
    cloud: mycloud
    endpoint_type: admin
    state: present
    name: test-listener
    loadbalancer: test-loadbalancer
    protocol: HTTP
    protocol_port: 8080
    wait: no

# Delete a listener
- openstack.cloud.lb_listener:
    cloud: mycloud
    endpoint_type: admin
    state: absent
    name: test-listener
    loadbalancer: test-loadbalancer

# Create a listener, increase timeouts for connection persistence (for SSH for example).
- openstack.cloud.lb_listener:
    cloud: mycloud
    endpoint_type: admin
    state: present
    name: test-listener
    loadbalancer: test-loadbalancer
    protocol: TCP
    protocol_port: 22
    timeout_client_data: 1800000
    timeout_member_data: 1800000
'''

import time

from ansible_collections.openstack.cloud.plugins.module_utils.openstack import OpenStackModule


class LoadbalancerListenerModule(OpenStackModule):
    argument_spec = dict(
        name=dict(required=True),
        state=dict(default='present', choices=['absent', 'present']),
        loadbalancer=dict(required=True),
        protocol=dict(default='HTTP',
                      choices=['HTTP', 'HTTPS', 'TCP', 'TERMINATED_HTTPS', 'UDP', 'SCTP']),
        protocol_port=dict(default=80, type='int', required=False),
        timeout_client_data=dict(default=50000, type='int', required=False),
        timeout_member_data=dict(default=50000, type='int', required=False),
    )
    module_kwargs = dict()

    def _lb_wait_for_status(self, lb, status, failures, interval=5):
        """Wait for load balancer to be in a particular provisioning status."""
        timeout = self.params['timeout']

        total_sleep = 0
        if failures is None:
            failures = []

        while total_sleep < timeout:
            lb = self.conn.load_balancer.get_load_balancer(lb.id)
            if lb.provisioning_status == status:
                return None
            if lb.provisioning_status in failures:
                self.fail_json(
                    msg="Load Balancer %s transitioned to failure state %s" %
                        (lb.id, lb.provisioning_status)
                )

            time.sleep(interval)
            total_sleep += interval

        self.fail_json(
            msg="Timeout waiting for Load Balancer %s to transition to %s" %
                (lb.id, status)
        )

    def run(self):
        loadbalancer = self.params['loadbalancer']
        loadbalancer_id = None

        changed = False
        listener = self.conn.load_balancer.find_listener(
            name_or_id=self.params['name'])

        if self.params['state'] == 'present':
            if not listener:
                lb = self.conn.load_balancer.find_load_balancer(loadbalancer)
                if not lb:
                    self.fail_json(
                        msg='load balancer %s is not found' % loadbalancer
                    )
                loadbalancer_id = lb.id

                listener = self.conn.load_balancer.create_listener(
                    name=self.params['name'],
                    loadbalancer_id=loadbalancer_id,
                    protocol=self.params['protocol'],
                    protocol_port=self.params['protocol_port'],
                    timeout_client_data=self.params['timeout_client_data'],
                    timeout_member_data=self.params['timeout_member_data'],
                )
                changed = True

                if not self.params['wait']:
                    self.exit_json(
                        changed=changed, listener=listener.to_dict(),
                        id=listener.id)

            if self.params['wait']:
                # Check in case the listener already exists.
                lb = self.conn.load_balancer.find_load_balancer(loadbalancer)
                if not lb:
                    self.fail_json(
                        msg='load balancer %s is not found' % loadbalancer
                    )
                self._lb_wait_for_status(lb, "ACTIVE", ["ERROR"])

            self.exit_json(
                changed=changed, listener=listener.to_dict(), id=listener.id)
        elif self.params['state'] == 'absent':
            if not listener:
                changed = False
            else:
                self.conn.load_balancer.delete_listener(listener)
                changed = True

                if self.params['wait']:
                    # Wait for the load balancer to be active after deleting
                    # the listener.
                    lb = self.conn.load_balancer.find_load_balancer(loadbalancer)
                    if not lb:
                        self.fail_json(
                            msg='load balancer %s is not found' % loadbalancer
                        )
                    self._lb_wait_for_status(lb, "ACTIVE", ["ERROR"])

            self.exit_json(changed=changed)


def main():
    module = LoadbalancerListenerModule()
    module()


if __name__ == "__main__":
    main()
