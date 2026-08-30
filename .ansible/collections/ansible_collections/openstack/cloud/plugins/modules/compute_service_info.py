#!/usr/bin/python
# Copyright (c) 2016 Hewlett-Packard Enterprise Corporation
# GNU General Public License v3.0+ (see COPYING or https://www.gnu.org/licenses/gpl-3.0.txt)

DOCUMENTATION = '''
---
module: compute_service_info
short_description: Retrieve information about one or more OpenStack compute services
author: OpenStack Ansible SIG
description:
    - Retrieve information about nova compute services
options:
   binary:
     description:
        - Filter by service binary type. Requires openstacksdk>=0.53.
     type: str
   host:
     description:
        - Filter by service host. Requires openstacksdk>=0.53.
     type: str
requirements:
    - "python >= 3.6"
    - "openstacksdk"

extends_documentation_fragment:
- openstack.cloud.openstack
'''

EXAMPLES = '''
# Gather information about compute services
- openstack.cloud.compute_service_info:
    cloud: awesomecloud
    binary: "nova-compute"
    host: "localhost"
  register: result
- openstack.cloud.compute_service_info:
    cloud: awesomecloud
  register: result
- debug:
    msg: "{{ result.openstack_compute_services }}"
'''


RETURN = '''
openstack_compute_services:
    description: has all the OpenStack information about compute services
    returned: always, but can be null
    type: complex
    contains:
        id:
            description: Unique UUID.
            returned: success
            type: str
        binary:
            description: The binary name of the service.
            returned: success
            type: str
        host:
            description: The name of the host.
            returned: success
            type: str
        disabled_reason:
            description: The reason why the service is disabled
            returned: success and OpenStack SDK >= 0.53
            type: str
        disables_reason:
            description: The reason why the service is disabled
            returned: success and OpenStack SDK < 0.53
            type: str
        availability_zone:
            description: The availability zone name.
            returned: success
            type: str
        is_forced_down:
            description: If the service has been forced down or nova-compute
            returned: success
            type: bool
        name:
            description: Service name
            returned: success
            type: str
        status:
            description: The status of the service. One of enabled or disabled.
            returned: success
            type: str
        state:
            description: The state of the service. One of up or down.
            returned: success
            type: str
        update_at:
            description: The date and time when the resource was updated
            returned: success
            type: str
'''

from ansible_collections.openstack.cloud.plugins.module_utils.openstack import OpenStackModule


class ComputeServiceInfoModule(OpenStackModule):
    argument_spec = dict(
        binary=dict(required=False, default=None, min_ver='0.53.0'),
        host=dict(required=False, default=None, min_ver='0.53.0'),
    )
    module_kwargs = dict(
        supports_check_mode=True
    )

    def run(self):
        filters = self.check_versioned(binary=self.params['binary'], host=self.params['host'])
        filters = {k: v for k, v in filters.items() if v is not None}
        services = self.conn.compute.services(**filters)
        services = [service.to_dict(computed=True) for service in services]
        self.exit_json(changed=False, openstack_compute_services=services)


def main():
    module = ComputeServiceInfoModule()
    module()


if __name__ == '__main__':
    main()
