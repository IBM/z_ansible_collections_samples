#!/usr/bin/python
# coding: utf-8 -*-
# Copyright (c) 2021 by Red Hat, Inc.
# GNU General Public License v3.0+ (see COPYING or https://www.gnu.org/licenses/gpl-3.0.txt)

DOCUMENTATION = '''
module: baremetal_port_info
short_description: Retrieve information about Bare Metal ports from OpenStack
author: OpenStack Ansible SIG
description:
    - Retrieve information about Bare Metal ports from OpenStack.
options:
    uuid:
      description:
        - Name or globally unique identifier (UUID) to identify the port.
      type: str
    address:
      description:
        - Physical hardware address of this network Port, typically the
          hardware MAC address.
      type: str
    node:
      description:
        - Name or globally unique identifier (UUID) to identify a Baremetal
          Node.
      type: str
    ironic_url:
      description:
        - If noauth mode is utilized, this is required to be set to the
          endpoint URL for the Ironic API.  Use with "auth" and "auth_type"
          settings set to None.
      type: str
requirements:
    - "python >= 3.6"
    - "openstacksdk"
extends_documentation_fragment:
- openstack.cloud.openstack
'''

EXAMPLES = '''
# Gather information about all baremetal ports
- openstack.cloud.baremetal_port_info:
    cloud: devstack
  register: result
# Gather information about a baremetal port by address
- openstack.cloud.baremetal_port_info:
    cloud: devstack
    address: fa:16:3e:aa:aa:aa
  register: result
# Gather information about a baremetal port by address
- openstack.cloud.baremetal_port_info:
    cloud: devstack
    uuid: a2b6bd99-77b9-43f0-9ddc-826568e68dec
  register: result
# Gather information about a baremetal ports associated with a baremetal node
- openstack.cloud.baremetal_port_info:
    cloud: devstack
    node: bm-0
  register: result
'''

RETURN = '''
baremetal_ports:
    description: Bare Metal port list. A subset of the dictionary keys
                 listed below may be returned, depending on your cloud
                 provider.
    returned: always, but can be null
    type: list
    elements: dict
    contains:
        address:
            description: Physical hardware address of this network Port,
                         typically the hardware MAC address.
            returned: success
            type: str
        created_at:
            description: Bare Metal port created at timestamp.
            returned: success
            type: str
        extra:
            description: A set of one or more arbitrary metadata key and
                         value pairs.
            returned: success
            type: dict
        id:
            description: The UUID for the Baremetal Port resource.
            returned: success
            type: str
        internal_info:
            description: Internal metadata set and stored by the Port. This
                         field is read-only.
            returned: success
            type: dict
        is_pxe_enabled:
            description: Whether PXE is enabled or disabled on the Port.
            returned: success
            type: bool
        local_link_connection:
            description: The Port binding profile.
            returned: success
            type: dict
            contains:
              switch_id:
                description: A MAC address or an OpenFlow based datapath_id of
                             the switch.
                type: str
              port_id:
                description: Identifier of the physical port on the switch to
                             which node's port is connected to.
                type: str
              switch_info:
                description: An optional string field to be used to store any
                             vendor-specific information.
                type: str
        location:
            description: Cloud location of this resource (cloud, project,
                         region, zone)
            returned: success
            type: dict
        name:
            description: Bare Metal port name.
            returned: success
            type: str
        node_id:
            description: UUID of the Bare Metal Node this resource belongs to.
            returned: success
            type: str
        physical_network:
            description: The name of the physical network to which a port is
                         connected.
            returned: success
            type: str
        port_group_id:
            description: UUID  of the Portgroup this resource belongs to.
            returned: success
            type: str
        updated_at:
            description: Bare Metal port updated at timestamp.
            returned: success
            type: str
'''


from ansible_collections.openstack.cloud.plugins.module_utils.ironic import (
    IronicModule,
    ironic_argument_spec,
)
from ansible_collections.openstack.cloud.plugins.module_utils.openstack import (
    openstack_module_kwargs,
    openstack_cloud_from_module
)


def main():
    argument_spec = ironic_argument_spec(
        uuid=dict(required=False),
        address=dict(required=False),
        node=dict(required=False),
    )
    module_kwargs = openstack_module_kwargs()
    module_kwargs['supports_check_mode'] = True
    module = IronicModule(argument_spec, **module_kwargs)

    ports = list()
    sdk, cloud = openstack_cloud_from_module(module)
    try:
        if module.params['uuid']:
            port = cloud.baremetal.find_port(module.params['uuid'])
            if not port:
                module.fail_json(
                    msg='Baremetal port with uuid {uuid} was not found'
                        .format(uuid=module.params['uuid']))
            ports.append(port)

        elif module.params['address']:
            ports = list(
                cloud.baremetal.ports(address=module.params['address'],
                                      details=True))
            if not ports:
                module.fail_json(
                    msg='Baremetal port with address {address} was not found'
                        .format(address=module.params['address']))

        elif module.params['node']:
            machine = cloud.get_machine(module.params['node'])
            if not machine:
                module.fail_json(
                    msg='Baremetal node {node} was not found'
                        .format(node=module.params['node']))
            ports = list(
                cloud.baremetal.ports(node_uuid=machine.uuid, details=True))

        else:
            ports = list(cloud.baremetal.ports(details=True))

        # Convert ports to dictionaries and cleanup properties
        ports = [port.to_dict() for port in ports]
        for port in ports:
            # links are not useful
            port.pop('links', None)

        module.exit_json(changed=False, baremetal_ports=ports)
    except sdk.exceptions.OpenStackCloudException as e:
        module.fail_json(msg=str(e))


if __name__ == "__main__":
    main()
