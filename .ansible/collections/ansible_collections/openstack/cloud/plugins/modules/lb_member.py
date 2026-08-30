#!/usr/bin/python

# Copyright (c) 2018 Catalyst Cloud Ltd.
# GNU General Public License v3.0+ (see COPYING or https://www.gnu.org/licenses/gpl-3.0.txt)

DOCUMENTATION = '''
---
module: lb_member
short_description: Add/Delete a member for a pool in load balancer from OpenStack Cloud
author: OpenStack Ansible SIG
description:
   - Add or Remove a member for a pool from the OpenStack load-balancer service.
options:
   name:
     description:
        - Name that has to be given to the member
     required: true
     type: str
   state:
     description:
       - Should the resource be present or absent.
     choices: [present, absent]
     default: present
     type: str
   pool:
     description:
        - The name or id of the pool that this member belongs to.
     required: true
     type: str
   protocol_port:
     description:
        - The protocol port number for the member.
     default: 80
     type: int
   address:
     description:
        - The IP address of the member.
     type: str
   subnet_id:
     description:
        - The subnet ID the member service is accessible from.
     type: str
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
   monitor_address:
     description:
        - IP address used to monitor this member
     type: str
   monitor_port:
     description:
        - Port used to monitor this member
     type: int
requirements:
    - "python >= 3.6"
    - "openstacksdk"

extends_documentation_fragment:
- openstack.cloud.openstack
'''

RETURN = '''
id:
    description: The member UUID.
    returned: On success when I(state) is 'present'
    type: str
    sample: "39007a7e-ee4f-4d13-8283-b4da2e037c69"
member:
    description: Dictionary describing the member.
    returned: On success when I(state) is 'present'
    type: complex
    contains:
        id:
            description: Unique UUID.
            type: str
            sample: "39007a7e-ee4f-4d13-8283-b4da2e037c69"
        name:
            description: Name given to the member.
            type: str
            sample: "test"
        description:
            description: The member description.
            type: str
            sample: "description"
        provisioning_status:
            description: The provisioning status of the member.
            type: str
            sample: "ACTIVE"
        operating_status:
            description: The operating status of the member.
            type: str
            sample: "ONLINE"
        is_admin_state_up:
            description: The administrative state of the member.
            type: bool
            sample: true
        protocol_port:
            description: The protocol port number for the member.
            type: int
            sample: 80
        subnet_id:
            description: The subnet ID the member service is accessible from.
            type: str
            sample: "489247fa-9c25-11e8-9679-00224d6b7bc1"
        address:
            description: The IP address of the backend member server.
            type: str
            sample: "192.168.2.10"
'''

EXAMPLES = '''
# Create a member, wait for the member to be created.
- openstack.cloud.lb_member:
    cloud: mycloud
    endpoint_type: admin
    state: present
    name: test-member
    pool: test-pool
    address: 192.168.10.3
    protocol_port: 8080

# Delete a listener
- openstack.cloud.lb_member:
    cloud: mycloud
    endpoint_type: admin
    state: absent
    name: test-member
    pool: test-pool
'''

import time

from ansible_collections.openstack.cloud.plugins.module_utils.openstack import OpenStackModule


class LoadbalancerMemberModule(OpenStackModule):
    argument_spec = dict(
        name=dict(required=True),
        state=dict(default='present', choices=['absent', 'present']),
        pool=dict(required=True),
        address=dict(default=None),
        protocol_port=dict(default=80, type='int'),
        subnet_id=dict(default=None),
        monitor_address=dict(default=None),
        monitor_port=dict(default=None, type='int')
    )
    module_kwargs = dict()

    def _wait_for_member_status(self, pool_id, member_id, status,
                                failures, interval=5):
        timeout = self.params['timeout']

        total_sleep = 0
        if failures is None:
            failures = []

        while total_sleep < timeout:
            member = self.conn.load_balancer.get_member(member_id, pool_id)
            provisioning_status = member.provisioning_status
            if provisioning_status == status:
                return member
            if provisioning_status in failures:
                self.fail_json(
                    msg="Member %s transitioned to failure state %s" %
                        (member_id, provisioning_status)
                )

            time.sleep(interval)
            total_sleep += interval

        self.fail_json(
            msg="Timeout waiting for member %s to transition to %s" %
                (member_id, status)
        )

    def run(self):
        name = self.params['name']
        pool = self.params['pool']

        changed = False

        pool_ret = self.conn.load_balancer.find_pool(name_or_id=pool)
        if not pool_ret:
            self.fail_json(msg='pool %s is not found' % pool)

        pool_id = pool_ret.id
        member = self.conn.load_balancer.find_member(name, pool_id)

        if self.params['state'] == 'present':
            if not member:
                member = self.conn.load_balancer.create_member(
                    pool_ret,
                    address=self.params['address'],
                    name=name,
                    protocol_port=self.params['protocol_port'],
                    subnet_id=self.params['subnet_id'],
                    monitor_address=self.params['monitor_address'],
                    monitor_port=self.params['monitor_port']
                )
                changed = True

                if not self.params['wait']:
                    self.exit_json(
                        changed=changed, member=member.to_dict(), id=member.id)

            if self.params['wait']:
                member = self._wait_for_member_status(
                    pool_id, member.id, "ACTIVE", ["ERROR"])

            self.exit_json(
                changed=changed, member=member.to_dict(), id=member.id)

        elif self.params['state'] == 'absent':
            if member:
                self.conn.load_balancer.delete_member(member, pool_ret)
                changed = True

            self.exit_json(changed=changed)


def main():
    module = LoadbalancerMemberModule()
    module()


if __name__ == "__main__":
    main()
