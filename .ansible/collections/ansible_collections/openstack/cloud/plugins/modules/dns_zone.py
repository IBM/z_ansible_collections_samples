#!/usr/bin/python
# Copyright (c) 2016 Hewlett-Packard Enterprise
# GNU General Public License v3.0+ (see COPYING or https://www.gnu.org/licenses/gpl-3.0.txt)

DOCUMENTATION = '''
---
module: dns_zone
short_description: Manage OpenStack DNS zones
author: OpenStack Ansible SIG
description:
    - Manage OpenStack DNS zones. Zones can be created, deleted or
      updated. Only the I(email), I(description), I(ttl) and I(masters) values
      can be updated.
options:
   name:
     description:
        - Zone name
     required: true
     type: str
   zone_type:
     description:
        - Zone type
     choices: [primary, secondary]
     type: str
   email:
     description:
        - Email of the zone owner (only applies if zone_type is primary)
     type: str
   description:
     description:
        - Zone description
     type: str
   ttl:
     description:
        -  TTL (Time To Live) value in seconds
     type: int
   masters:
     description:
        - Master nameservers (only applies if zone_type is secondary)
     type: list
     elements: str
   state:
     description:
       - Should the resource be present or absent.
     choices: [present, absent]
     default: present
     type: str
requirements:
    - "python >= 3.6"
    - "openstacksdk"

extends_documentation_fragment:
- openstack.cloud.openstack
'''

EXAMPLES = '''
# Create a zone named "example.net"
- openstack.cloud.dns_zone:
    cloud: mycloud
    state: present
    name: example.net.
    zone_type: primary
    email: test@example.net
    description: Test zone
    ttl: 3600

# Update the TTL on existing "example.net." zone
- openstack.cloud.dns_zone:
    cloud: mycloud
    state: present
    name: example.net.
    ttl: 7200

# Delete zone named "example.net."
- openstack.cloud.dns_zone:
    cloud: mycloud
    state: absent
    name: example.net.
'''

RETURN = '''
zone:
    description: Dictionary describing the zone.
    returned: On success when I(state) is 'present'.
    type: complex
    contains:
        id:
            description: Unique zone ID
            type: str
            sample: "c1c530a3-3619-46f3-b0f6-236927b2618c"
        name:
            description: Zone name
            type: str
            sample: "example.net."
        type:
            description: Zone type
            type: str
            sample: "PRIMARY"
        email:
            description: Zone owner email
            type: str
            sample: "test@example.net"
        description:
            description: Zone description
            type: str
            sample: "Test description"
        ttl:
            description: Zone TTL value
            type: int
            sample: 3600
        masters:
            description: Zone master nameservers
            type: list
            sample: []
'''

from ansible_collections.openstack.cloud.plugins.module_utils.openstack import OpenStackModule


class DnsZoneModule(OpenStackModule):

    argument_spec = dict(
        name=dict(required=True, type='str'),
        zone_type=dict(required=False, choices=['primary', 'secondary'], type='str'),
        email=dict(required=False, type='str'),
        description=dict(required=False, type='str'),
        ttl=dict(required=False, type='int'),
        masters=dict(required=False, type='list', elements='str'),
        state=dict(default='present', choices=['absent', 'present'], type='str'),
    )

    def _system_state_change(self, state, email, description, ttl, masters, zone):
        if state == 'present':
            if not zone:
                return True
            if email is not None and zone.email != email:
                return True
            if description is not None and zone.description != description:
                return True
            if ttl is not None and zone.ttl != ttl:
                return True
            if masters is not None and zone.masters != masters:
                return True
        if state == 'absent' and zone:
            return True
        return False

    def _wait(self, timeout, zone, state):
        """Wait for a zone to reach the desired state for the given state."""

        for count in self.sdk.utils.iterate_timeout(
                timeout,
                "Timeout waiting for zone to be %s" % state):

            if (state == 'absent' and zone is None) or (state == 'present' and zone and zone.status == 'ACTIVE'):
                return

            try:
                zone = self.conn.get_zone(zone.id)
            except Exception:
                continue

            if zone and zone.status == 'ERROR':
                self.fail_json(msg="Zone reached ERROR state while waiting for it to be %s" % state)

    def run(self):

        name = self.params['name']
        state = self.params['state']
        wait = self.params['wait']
        timeout = self.params['timeout']

        zone = self.conn.get_zone(name)

        if state == 'present':

            zone_type = self.params['zone_type']
            email = self.params['email']
            description = self.params['description']
            ttl = self.params['ttl']
            masters = self.params['masters']

            kwargs = {}

            if email:
                kwargs['email'] = email
            if description:
                kwargs['description'] = description
            if ttl:
                kwargs['ttl'] = ttl
            if masters:
                kwargs['masters'] = masters

            if self.ansible.check_mode:
                self.exit_json(changed=self._system_state_change(state, email,
                                                                 description, ttl,
                                                                 masters, zone))

            if zone is None:
                zone = self.conn.create_zone(
                    name=name, zone_type=zone_type, **kwargs)
                changed = True
            else:
                if masters is None:
                    masters = []

                pre_update_zone = zone
                changed = self._system_state_change(state, email,
                                                    description, ttl,
                                                    masters, pre_update_zone)
                if changed:
                    zone = self.conn.update_zone(
                        name, **kwargs)

            if wait:
                self._wait(timeout, zone, state)

            self.exit_json(changed=changed, zone=zone)

        elif state == 'absent':
            if self.ansible.check_mode:
                self.exit_json(changed=self._system_state_change(state, None,
                                                                 None, None,
                                                                 None, zone))

            if zone is None:
                changed = False
            else:
                self.conn.delete_zone(name)
                changed = True

            if wait:
                self._wait(timeout, zone, state)

            self.exit_json(changed=changed)


def main():
    module = DnsZoneModule()
    module()


if __name__ == '__main__':
    main()
