#!/usr/bin/python
# coding: utf-8 -*-
# Copyright (c) 2021 by Red Hat, Inc.
# GNU General Public License v3.0+ (see COPYING or https://www.gnu.org/licenses/gpl-3.0.txt)

DOCUMENTATION = '''
module: baremetal_port
short_description: Create/Delete Bare Metal port Resources from OpenStack
author: OpenStack Ansible SIG
description:
    - Create, Update and Remove ironic ports from OpenStack.
options:
    state:
      description:
        - Indicates desired state of the resource
      choices: ['present', 'absent']
      default: present
      type: str
    uuid:
      description:
        - globally unique identifier (UUID) to be given to the resource. Will
          be auto-generated if not specified.
      type: str
    node:
      description:
        - UUID or Name of the Node this resource belongs to.
      type: str
    address:
      description:
        - Physical hardware address of this network Port, typically the
          hardware MAC address.
      type: str
    portgroup:
      description:
        - UUID or Name of the Portgroup this resource belongs to.
      type: str
    local_link_connection:
      description:
        - The Port binding profile.
      type: dict
      suboptions:
        switch_id:
          description:
            - A MAC address or an OpenFlow based datapath_id of the switch.
          type: str
        port_id:
          description:
            - Identifier of the physical port on the switch to which node's
              port is connected to.
          type: str
        switch_info:
          description:
            - An optional string field to be used to store any vendor-specific
              information.
          type: str
    is_pxe_enabled:
      description:
        - Whether PXE should be enabled or disabled on the Port.
      type: bool
    physical_network:
      description:
        - The name of the physical network to which a port is connected.
      type: str
    extra:
      description:
        - A set of one or more arbitrary metadata key and value pairs.
      type: dict
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
# Create Bare Metal port
- name: Create Bare Metal port
  openstack.cloud.baremetal_port:
    cloud: devstack
    state: present
    node: bm-0
    address: fa:16:3e:aa:aa:aa
    pxe_enabled: True
    local_link_connection:
      switch_id: 0a:1b:2c:3d:4e:5f
      port_id: Ethernet3/1
      switch_info: switch1
    extra:
      something: extra
    physical_network: datacenter
  register: result
# Delete Bare Metal port
- name: Delete Bare Metal port
  openstack.cloud.baremetal_port:
    cloud: devstack
    state: absent
    address: fa:16:3e:aa:aa:aa
  register: result
# Update Bare Metal port
- name: Update Bare Metal port
  openstack.cloud.baremetal_port:
    cloud: devstack
    state: present
    uuid: 1a85ebca-22bf-42eb-ad9e-f640789b8098
    pxe_enabled: False
    local_link_connection:
      switch_id: a0:b1:c2:d3:e4:f5
      port_id: Ethernet4/12
      switch_info: switch2
'''

RETURN = '''
id:
    description: Unique UUID of the port.
    returned: always, but can be null
    type: str
result:
    description: A short text describing the result.
    returned: success
    type: str
changes:
    description: Map showing from -> to values for properties that was changed
                 after port update.
    returned: success
    type: dict
port:
    description: A port dictionary, subset of the dictionary keys listed below
                 may be returned, depending on your cloud provider.
    returned: success
    type: complex
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
            description: A set of one or more arbitrary metadata key and value
                         pairs.
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
            description: The Port binding profile. If specified, must contain
                         switch_id (only a MAC address or an OpenFlow based
                         datapath_id of the switch are accepted in this field
                         and port_id (identifier of the physical port on the
                         switch to which node's port is connected to) fields.
                         switch_info is an optional string field to be used to
                         store any vendor-specific information.
            returned: success
            type: dict
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

_PROP_TO_ATTR_MAP = {
    'pxe_enabled': 'is_pxe_enabled',
    'address': 'address',
    'extra': 'extra',
    'local_link_connection': 'local_link_connection',
    'physical_network': 'physical_network',
    'node_uuid': 'node_id',
    'portgroup_uuid': 'port_group_id',
    'uuid': 'id',
}


def find_port(module, cloud):
    port = None
    if module.params['uuid']:
        port = cloud.baremetal.find_port(module.params['uuid'])
    elif module.params['address']:
        ports = list(cloud.baremetal.ports(address=module.params['address'],
                                           details=True))
        if ports and len(ports) == 1:
            port = ports[0]
        elif len(ports) > 1:
            module.fail_json(
                msg="Multiple ports with address {address} found. A uuid must "
                    "be defined in order to identify the correct port"
                    .format(address=module.params['address']))

    return port


def add_port(module, cloud):
    port = find_port(module, cloud)
    if port:
        update_port(module, cloud, port=port)

    if not module.params['node'] or not module.params['address']:
        module.fail_json(
            msg="A Bare Metal node (name or uuid) and an address is required "
                "to create a port")

    machine = cloud.get_machine(module.params['node'])
    if not machine:
        module.fail_json(
            msg="Bare Metal node {node} could not be found".format(
                node=module.params['node']))

    module.params['node_uuid'] = machine.id
    props = {k: module.params[k] for k in _PROP_TO_ATTR_MAP.keys()
             if k in module.params}
    port = cloud.baremetal.create_port(**props)
    port_dict = port.to_dict()
    port_dict.pop('links', None)
    module.exit_json(
        changed=True,
        result="Port successfully created",
        changes=None,
        port=port_dict,
        id=port_dict['id'])


def update_port(module, cloud, port=None):
    if not port:
        port = find_port(module, cloud)

    if module.params['node']:
        machine = cloud.get_machine(module.params['node'])
        if machine:
            module.params['node_uuid'] = machine.id

    old_props = {k: port[v] for k, v in _PROP_TO_ATTR_MAP.items()}
    new_props = {k: module.params[k] for k in _PROP_TO_ATTR_MAP.keys()
                 if k in module.params and module.params[k] is not None}
    prop_diff = {k: new_props[k] for k in _PROP_TO_ATTR_MAP.keys()
                 if k in new_props and old_props[k] != new_props[k]}

    if not prop_diff:
        port_dict = port.to_dict()
        port_dict.pop('links', None)
        module.exit_json(
            changed=False,
            result="No port update required",
            changes=None,
            port=port_dict,
            id=port_dict['id'])

    port = cloud.baremetal.update_port(port.id, **prop_diff)
    port_dict = port.to_dict()
    port_dict.pop('links', None)
    module.exit_json(
        changed=True,
        result="Port successfully updated",
        changes={k: {'to': new_props[k], 'from': old_props[k]}
                 for k in prop_diff},
        port=port_dict,
        id=port_dict['id'])


def remove_port(module, cloud):
    if not module.params['uuid'] and not module.params['address']:
        module.fail_json(
            msg="A uuid or an address value must be defined in order to "
                "remove a port.")
    if module.params['uuid']:
        port = cloud.baremetal.delete_port(module.params['uuid'])
        if not port:
            module.exit_json(
                changed=False,
                result="Port not found",
                changes=None,
                id=module.params['uuid'])
    else:
        port = find_port(module, cloud)
        if not port:
            module.exit_json(
                changed=False,
                result="Port not found",
                changes=None,
                id=None)
        port = cloud.baremetal.delete_port(port.id)

    module.exit_json(
        changed=True,
        result="Port successfully removed",
        changes=None,
        id=port.id)


def main():
    argument_spec = ironic_argument_spec(
        uuid=dict(required=False),
        node=dict(required=False),
        address=dict(required=False),
        portgroup=dict(required=False),
        local_link_connection=dict(required=False, type='dict'),
        is_pxe_enabled=dict(required=False, type='bool'),
        physical_network=dict(required=False),
        extra=dict(required=False, type='dict'),
        state=dict(required=False,
                   default='present',
                   choices=['present', 'absent'])
    )

    module_kwargs = openstack_module_kwargs()
    module = IronicModule(argument_spec, **module_kwargs)

    module.params['pxe_enabled'] = module.params.pop('is_pxe_enabled', None)

    sdk, cloud = openstack_cloud_from_module(module)
    try:
        if module.params['state'] == 'present':
            add_port(module, cloud)

        if module.params['state'] == 'absent':
            remove_port(module, cloud)

    except sdk.exceptions.OpenStackCloudException as e:
        module.fail_json(msg=str(e))


if __name__ == "__main__":
    main()
