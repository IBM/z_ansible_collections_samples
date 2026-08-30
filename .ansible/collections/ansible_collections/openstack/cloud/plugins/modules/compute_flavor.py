#!/usr/bin/python

# Copyright (c) 2015 Hewlett-Packard Development Company, L.P.
# GNU General Public License v3.0+ (see COPYING or https://www.gnu.org/licenses/gpl-3.0.txt)

DOCUMENTATION = '''
---
module: compute_flavor
short_description: Manage OpenStack compute flavors
author: OpenStack Ansible SIG
description:
   - Add or remove flavors from OpenStack.
options:
   state:
     description:
        - Indicate desired state of the resource. When I(state) is 'present',
          then I(ram), I(vcpus), and I(disk) are all required. There are no
          default values for those parameters.
     choices: ['present', 'absent']
     default: present
     type: str
   name:
     description:
        - Flavor name.
     required: true
     type: str
   ram:
     description:
        - Amount of memory, in MB.
     type: int
   vcpus:
     description:
        - Number of virtual CPUs.
     type: int
   disk:
     description:
        - Size of local disk, in GB.
     default: 0
     type: int
   ephemeral:
     description:
        - Ephemeral space size, in GB.
     default: 0
     type: int
   swap:
     description:
        - Swap space size, in MB.
     default: 0
     type: int
   rxtx_factor:
     description:
        - RX/TX factor.
     default: 1.0
     type: float
   is_public:
     description:
        - Make flavor accessible to the public.
     type: bool
     default: 'yes'
   flavorid:
     description:
        - ID for the flavor. This is optional as a unique UUID will be
          assigned if a value is not specified.
     default: "auto"
     type: str
   extra_specs:
     description:
        - Metadata dictionary
     type: dict
requirements:
    - "python >= 3.6"
    - "openstacksdk"

extends_documentation_fragment:
- openstack.cloud.openstack
'''

EXAMPLES = '''
- name: "Create 'tiny' flavor with 1024MB of RAM, 1 virtual CPU, and 10GB of local disk, and 10GB of ephemeral."
  openstack.cloud.compute_flavor:
    cloud: mycloud
    state: present
    name: tiny
    ram: 1024
    vcpus: 1
    disk: 10
    ephemeral: 10

- name: "Delete 'tiny' flavor"
  openstack.cloud.compute_flavor:
    cloud: mycloud
    state: absent
    name: tiny

- name: Create flavor with metadata
  openstack.cloud.compute_flavor:
    cloud: mycloud
    state: present
    name: tiny
    ram: 1024
    vcpus: 1
    disk: 10
    extra_specs:
      "quota:disk_read_iops_sec": 5000
      "aggregate_instance_extra_specs:pinned": false
'''

RETURN = '''
flavor:
    description: Dictionary describing the flavor.
    returned: On success when I(state) is 'present'
    type: complex
    contains:
        id:
            description: Flavor ID.
            returned: success
            type: str
            sample: "515256b8-7027-4d73-aa54-4e30a4a4a339"
        name:
            description: Flavor name.
            returned: success
            type: str
            sample: "tiny"
        disk:
            description: Size of local disk, in GB.
            returned: success
            type: int
            sample: 10
        ephemeral:
            description: Ephemeral space size, in GB.
            returned: success
            type: int
            sample: 10
        ram:
            description: Amount of memory, in MB.
            returned: success
            type: int
            sample: 1024
        swap:
            description: Swap space size, in MB.
            returned: success
            type: int
            sample: 100
        vcpus:
            description: Number of virtual CPUs.
            returned: success
            type: int
            sample: 2
        is_public:
            description: Make flavor accessible to the public.
            returned: success
            type: bool
            sample: true
        extra_specs:
            description: Flavor metadata
            returned: success
            type: dict
            sample:
                "quota:disk_read_iops_sec": 5000
                "aggregate_instance_extra_specs:pinned": false
'''

from ansible_collections.openstack.cloud.plugins.module_utils.openstack import OpenStackModule


class ComputeFlavorModule(OpenStackModule):
    argument_spec = dict(
        state=dict(required=False, default='present',
                   choices=['absent', 'present']),
        name=dict(required=True),

        # required when state is 'present'
        ram=dict(required=False, type='int'),
        vcpus=dict(required=False, type='int'),

        disk=dict(required=False, default=0, type='int'),
        ephemeral=dict(required=False, default=0, type='int'),
        swap=dict(required=False, default=0, type='int'),
        rxtx_factor=dict(required=False, default=1.0, type='float'),
        is_public=dict(required=False, default=True, type='bool'),
        flavorid=dict(required=False, default="auto"),
        extra_specs=dict(required=False, default=None, type='dict'),
    )

    module_kwargs = dict(
        required_if=[
            ('state', 'present', ['ram', 'vcpus', 'disk'])
        ],
        supports_check_mode=True
    )

    def _system_state_change(self, flavor):
        state = self.params['state']
        if state == 'present' and not flavor:
            return True
        if state == 'absent' and flavor:
            return True
        return False

    def run(self):
        state = self.params['state']
        name = self.params['name']
        extra_specs = self.params['extra_specs'] or {}

        flavor = self.conn.get_flavor(name)

        if self.ansible.check_mode:
            self.exit_json(changed=self._system_state_change(flavor))

        if state == 'present':
            old_extra_specs = {}
            require_update = False

            if flavor:
                old_extra_specs = flavor['extra_specs']
                if flavor['swap'] == "":
                    flavor['swap'] = 0
                for param_key in ['ram', 'vcpus', 'disk', 'ephemeral',
                                  'swap', 'rxtx_factor', 'is_public']:
                    if self.params[param_key] != flavor[param_key]:
                        require_update = True
                        break
            flavorid = self.params['flavorid']
            if flavor and require_update:
                self.conn.delete_flavor(name)
                old_extra_specs = {}
                if flavorid == 'auto':
                    flavorid = flavor['id']
                flavor = None

            if not flavor:
                flavor = self.conn.create_flavor(
                    name=name,
                    ram=self.params['ram'],
                    vcpus=self.params['vcpus'],
                    disk=self.params['disk'],
                    flavorid=flavorid,
                    ephemeral=self.params['ephemeral'],
                    swap=self.params['swap'],
                    rxtx_factor=self.params['rxtx_factor'],
                    is_public=self.params['is_public']
                )
                changed = True
            else:
                changed = False

            new_extra_specs = dict([(k, str(v)) for k, v in extra_specs.items()])
            unset_keys = set(old_extra_specs.keys()) - set(extra_specs.keys())

            if unset_keys and not require_update:
                self.conn.unset_flavor_specs(flavor['id'], unset_keys)

            if old_extra_specs != new_extra_specs:
                self.conn.set_flavor_specs(flavor['id'], extra_specs)

            changed = (changed or old_extra_specs != new_extra_specs)

            self.exit_json(
                changed=changed, flavor=flavor, id=flavor['id'])

        elif state == 'absent':
            if flavor:
                self.conn.delete_flavor(name)
                self.exit_json(changed=True)
            self.exit_json(changed=False)


def main():
    module = ComputeFlavorModule()
    module()


if __name__ == '__main__':
    main()
