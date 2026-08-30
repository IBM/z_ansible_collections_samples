#!/usr/bin/python

# Copyright (c) 2015 IBM
# GNU General Public License v3.0+ (see COPYING or https://www.gnu.org/licenses/gpl-3.0.txt)

DOCUMENTATION = '''
---
module: compute_flavor_info
short_description: Retrieve information about one or more flavors
author: OpenStack Ansible SIG
description:
    - Retrieve information about available OpenStack instance flavors. By default,
      information about ALL flavors are retrieved. Filters can be applied to get
      information for only matching flavors. For example, you can filter on the
      amount of RAM available to the flavor, or the number of virtual CPUs
      available to the flavor, or both. When specifying multiple filters,
      *ALL* filters must match on a flavor before that flavor is returned as
      a fact.
    - This module was called C(openstack.cloud.compute_flavor_facts) before Ansible 2.9, returning C(ansible_facts).
      Note that the M(openstack.cloud.compute_flavor_info) module no longer returns C(ansible_facts)!
notes:
    - The result contains a list of unsorted flavors.
options:
   name:
     description:
       - A flavor name. Cannot be used with I(ram) or I(vcpus) or I(ephemeral).
     type: str
   ram:
     description:
       - "A string used for filtering flavors based on the amount of RAM
         (in MB) desired. This string accepts the following special values:
         'MIN' (return flavors with the minimum amount of RAM), and 'MAX'
         (return flavors with the maximum amount of RAM)."

       - "A specific amount of RAM may also be specified. Any flavors with this
         exact amount of RAM will be returned."

       - "A range of acceptable RAM may be given using a special syntax. Simply
         prefix the amount of RAM with one of these acceptable range values:
         '<', '>', '<=', '>='. These values represent less than, greater than,
         less than or equal to, and greater than or equal to, respectively."
     type: str
   vcpus:
     description:
       - A string used for filtering flavors based on the number of virtual
         CPUs desired. Format is the same as the I(ram) parameter.
     type: str
   limit:
     description:
       - Limits the number of flavors returned. All matching flavors are
         returned by default.
     type: int
   ephemeral:
     description:
       - A string used for filtering flavors based on the amount of ephemeral
         storage. Format is the same as the I(ram) parameter
     type: str
requirements:
    - "python >= 3.6"
    - "openstacksdk"

extends_documentation_fragment:
- openstack.cloud.openstack
'''

EXAMPLES = '''
# Gather information about all available flavors
- openstack.cloud.compute_flavor_info:
    cloud: mycloud
  register: result

- debug:
    msg: "{{ result.openstack_flavors }}"

# Gather information for the flavor named "xlarge-flavor"
- openstack.cloud.compute_flavor_info:
    cloud: mycloud
    name: "xlarge-flavor"

# Get all flavors that have exactly 512 MB of RAM.
- openstack.cloud.compute_flavor_info:
    cloud: mycloud
    ram: "512"

# Get all flavors that have 1024 MB or more of RAM.
- openstack.cloud.compute_flavor_info:
    cloud: mycloud
    ram: ">=1024"

# Get a single flavor that has the minimum amount of RAM. Using the 'limit'
# option will guarantee only a single flavor is returned.
- openstack.cloud.compute_flavor_info:
    cloud: mycloud
    ram: "MIN"
    limit: 1

# Get all flavors with 1024 MB of RAM or more, AND exactly 2 virtual CPUs.
- openstack.cloud.compute_flavor_info:
    cloud: mycloud
    ram: ">=1024"
    vcpus: "2"

# Get all flavors with 1024 MB of RAM or more, exactly 2 virtual CPUs, and
# less than 30gb of ephemeral storage.
- openstack.cloud.compute_flavor_info:
    cloud: mycloud
    ram: ">=1024"
    vcpus: "2"
    ephemeral: "<30"
'''


RETURN = '''
openstack_flavors:
    description: Dictionary describing the flavors.
    returned: On success.
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
        description:
            description: Description of the flavor
            returned: success
            type: str
            sample: "Small flavor"
        is_disabled:
            description: Wether the flavor is enabled or not
            returned: success
            type: bool
            sample: False
        rxtx_factor:
            description: Factor to be multiplied by the rxtx_base property of
                         the network it is attached to in order to have a
                         different bandwidth cap.
            returned: success
            type: float
            sample: 1.0
        extra_specs:
            description: Optional parameters to configure different flavors
                         options.
            returned: success
            type: dict
            sample: "{'hw_rng:allowed': True}"
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
'''

from ansible_collections.openstack.cloud.plugins.module_utils.openstack import OpenStackModule


class ComputeFlavorInfoModule(OpenStackModule):
    argument_spec = dict(
        name=dict(required=False, default=None),
        ram=dict(required=False, default=None),
        vcpus=dict(required=False, default=None),
        limit=dict(required=False, default=None, type='int'),
        ephemeral=dict(required=False, default=None),
    )
    module_kwargs = dict(
        mutually_exclusive=[
            ['name', 'ram'],
            ['name', 'vcpus'],
            ['name', 'ephemeral']
        ],
        supports_check_mode=True
    )

    deprecated_names = ('openstack.cloud.compute_flavor_facts')

    def run(self):
        name = self.params['name']
        vcpus = self.params['vcpus']
        ram = self.params['ram']
        ephemeral = self.params['ephemeral']
        limit = self.params['limit']

        filters = {}
        if vcpus:
            filters['vcpus'] = vcpus
        if ram:
            filters['ram'] = ram
        if ephemeral:
            filters['ephemeral'] = ephemeral

        if name:
            # extra_specs are exposed in the flavor representation since Rocky, so we do not
            # need get_extra_specs=True which is not available in OpenStack SDK 0.36 (Train)
            # Ref.: https://docs.openstack.org/nova/latest/reference/api-microversion-history.html
            flavor = self.conn.compute.find_flavor(name)
            flavors = [flavor] if flavor else []

        else:
            flavors = list(self.conn.compute.flavors())
            if filters:
                flavors = self.conn.range_search(flavors, filters)

        if limit is not None:
            flavors = flavors[:limit]

        # Transform entries to dict
        flavors = [flavor.to_dict(computed=True) for flavor in flavors]
        self.exit_json(changed=False, openstack_flavors=flavors)


def main():
    module = ComputeFlavorInfoModule()
    module()


if __name__ == '__main__':
    main()
