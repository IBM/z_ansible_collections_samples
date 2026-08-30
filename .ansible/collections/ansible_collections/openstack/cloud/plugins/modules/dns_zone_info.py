#!/usr/bin/python
# coding: utf-8 -*-
#
# Copyright (c) 2021 by Open Telekom Cloud, operated by T-Systems International GmbH
# GNU General Public License v3.0+ (see COPYING or https://www.gnu.org/licenses/gpl-3.0.txt)


DOCUMENTATION = '''
---
module: dns_zone_info
short_description: Getting information about dns zones
author: OpenStack Ansible SIG
description:
    - Getting information about dns zones. Output can be filtered.
options:
   name:
     description:
        - Zone name.
     type: str
   type:
     description:
        - Zone type.
     choices: [primary, secondary]
     type: str
   email:
     description:
        - Email of the zone owner (only applies if zone_type is primary).
     type: str
   description:
     description:
        - Zone description.
     type: str
   ttl:
     description:
        -  TTL (Time To Live) value in seconds.
     type: int

requirements:
    - "python >= 3.6"
    - "openstacksdk"

extends_documentation_fragment:
- openstack.cloud.openstack
'''

EXAMPLES = '''
# Create a zone named "example.net"
- openstack.cloud.dns_zones:

'''

RETURN = '''
zone:
    description: Dictionary describing the zone.
    returned: On success when I(state) is 'present'.
    type: complex
    contains:
        action:
            description: Current action in progress on the resource.
            type: str
            sample: "CREATE"
        attributes:
            description: Key:Value pairs of information about this zone, and the pool the user would like to place \
                the zone in. This information can be used by the scheduler to place zones on the correct pool.
            type: dict
            sample: {"tier": "gold", "ha": "true"}
        created_at:
            description: Date / Time when resource was created.
            type: str
            sample: "2014-07-07T18:25:31.275934"
        description:
            description: Description for this zone.
            type: str
            sample: "This is an example zone."
        email:
            description: E-mail for the zone. Used in SOA records for the zone.
            type: str
            sample: "test@example.org"
        id:
            description: ID for the resource.
            type: int
            sample: "a86dba58-0043-4cc6-a1bb-69d5e86f3ca3"
        links:
            description: Links to the resource, and other related resources. When a response has been broken into\
                pages, we will include a next link that should be followed to retrieve all results.
            type: dict
            sample:  {"self": "https://127.0.0.1:9001/v2/zones/a86dba58-0043-4cc6-a1bb-69d5e86f3ca3"}
        masters:
            description: Mandatory for secondary zones. The servers to slave from to get DNS information.
            type: list
            sample: "[]"
        name:
            description: DNS Name for the zone.
            type: str
            sample: "test.test."
        pool_id:
            description: ID for the pool hosting this zone.
            type: str
            sample: "a86dba58-0043-4cc6-a1bb-69d5e86f3ca3"
        project_id:
            description: ID for the project that owns the resource.
            type: str
            sample: "4335d1f0-f793-11e2-b778-0800200c9a66"
        serial:
            description: Current serial number for the zone.
            type: int
            sample: 1404757531
        status:
            description: Status of the resource.
            type: str
            sample: "ACTIVE"
        ttl:
            description: TTL (Time to Live) for the zone.
            type: int
            sample: 7200
        type:
            description: Type of zone. PRIMARY is controlled by Designate, SECONDARY zones are slaved from another\
                DNS Server. Defaults to PRIMARY
            type: str
            sample: "PRIMARY"
        updated_at:
            description: Date / Time when resource last updated.
            type: str
            sample: "2014-07-07T18:25:31.275934"
'''

from ansible_collections.openstack.cloud.plugins.module_utils.openstack import OpenStackModule


class DnsZoneInfoModule(OpenStackModule):

    argument_spec = dict(
        name=dict(required=False, type='str'),
        type=dict(required=False, choices=['primary', 'secondary'], type='str'),
        email=dict(required=False, type='str'),
        description=dict(required=False, type='str'),
        ttl=dict(required=False, type='int')
    )
    module_kwargs = dict(
        supports_check_mode=True
    )

    def run(self):

        name = self.params['name']
        type = self.params['type']
        email = self.params['email']
        description = self.params['description']
        ttl = self.params['ttl']

        kwargs = {}

        if name:
            kwargs['name'] = name
        if type:
            kwargs['type'] = type
        if email:
            kwargs['email'] = email
        if description:
            kwargs['description'] = description
        if ttl:
            kwargs['ttl'] = ttl

        data = [zone.to_dict(computed=False) for zone in
                self.conn.dns.zones(**kwargs)]

        self.exit_json(zones=data, changed=False)


def main():
    module = DnsZoneInfoModule()
    module()


if __name__ == '__main__':
    main()
