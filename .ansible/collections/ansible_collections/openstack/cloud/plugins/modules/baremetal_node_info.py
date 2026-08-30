#!/usr/bin/python
# coding: utf-8 -*-

# Copyright (c) 2021 by Red Hat, Inc.
# GNU General Public License v3.0+ (see COPYING or https://www.gnu.org/licenses/gpl-3.0.txt)


DOCUMENTATION = '''
module: baremetal_node_info
short_description: Retrieve information about Bare Metal nodes from OpenStack
author: OpenStack Ansible SIG
description:
    - Retrieve information about Bare Metal nodes from OpenStack.
options:
    node:
      description:
        - Name or globally unique identifier (UUID) to identify the host.
      type: str
    mac:
      description:
        - Unique mac address that is used to attempt to identify the host.
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
# Gather information about all baremeal nodes
- openstack.cloud.baremetal_node_info:
    cloud: "devstack"
  register: result
- debug:
    msg: "{{ result.baremetal_nodes }}"
# Gather information about a baremeal node
- openstack.cloud.baremetal_node_info:
    cloud: "devstack"
    node: "00000000-0000-0000-0000-000000000002"
  register: result
- debug:
    msg: "{{ result.baremetal_nodes }}"
'''

RETURN = '''
baremetal_nodes:
    description: Bare Metal node list. A subset of the dictionary keys
                 listed below may be returned, depending on your cloud
                 provider.
    returned: always, but can be null
    type: complex
    contains:
        allocation_uuid:
            description: The UUID of the allocation associated with the node.
                         If not null, will be the same as instance_uuid (the
                         opposite is not always true). Unlike instance_uuid,
                         this field is read-only. Please use the Allocation API
                         to remove allocations.
            returned: success
            type: str
        automated_clean:
            description: Indicates whether the node will perform automated
                         clean or not.
            returned: success
            type: bool
        bios_interface:
            description: The bios interface to be used for this node.
            returned: success
            type: str
        boot_interface:
            description: The boot interface for a Node, e.g. "pxe".
            returned: success
            type: str
        boot_mode:
            description: The boot mode for a node, either "uefi" or "bios"
            returned: success
            type: str
        chassis_uuid:
            description: UUID of the chassis associated with this Node. May be
                         empty or None.
            returned: success
            type: str
        clean_step:
            description: The current clean step.
            returned: success
            type: str
        conductor:
            description: The conductor currently servicing a node. This field
                         is read-only.
            returned: success
            type: str
        conductor_group:
            description: The conductor group for a node. Case-insensitive
                         string up to 255 characters, containing a-z, 0-9, _,
                         -, and ..
            returned: success
            type: str
        console_enabled:
            description: Indicates whether console access is enabled or
                         disabled on this node.
            returned: success
            type: bool
        console_interface:
            description: The console interface for a node, e.g. "no-console".
            returned: success
            type: str
        created_at:
            description: Bare Metal node created at timestamp.
            returned: success
            type: str
        deploy_interface:
            description: The deploy interface for a node, e.g. "direct".
            returned: success
            type: str
        deploy_step:
            description: The current deploy step.
            returned: success
            type: str
        driver:
            description: The name of the driver.
            returned: success
            type: str
        driver_info:
            description: All the metadata required by the driver to manage this
                         Node. List of fields varies between drivers, and can
                         be retrieved from the
                         /v1/drivers/<DRIVER_NAME>/properties resource.
            returned: success
            type: dict
        driver_internal_info:
            description: Internal metadata set and stored by the Node's driver.
            returned: success
            type: dict
        extra:
            description: A set of one or more arbitrary metadata key and value
                         pairs.
            returned: success
            type: dict
        fault:
            description: The fault indicates the active fault detected by
                         ironic, typically the Node is in "maintenance mode".
                         None means no fault has been detected by ironic.
                         "power failure" indicates ironic failed to retrieve
                         power state from this node. There are other possible
                         types, e.g., "clean failure" and "rescue abort
                         failure".
            returned: success
            type: str
        id:
            description: The UUID for the resource.
            returned: success
            type: str
        inspect_interface:
            description: The interface used for node inspection.
            returned: success
            type: str
        instance_info:
            description: Information used to customize the deployed image. May
                         include root partition size, a base 64 encoded config
                         drive, and other metadata. Note that this field is
                         erased automatically when the instance is deleted
                         (this is done by requesting the Node provision state
                         be changed to DELETED).
            returned: success
            type: dict
        instance_uuid:
            description: UUID of the Nova instance associated with this Node.
            returned: success
            type: str
        last_error:
            description: Any error from the most recent (last) transaction that
                         started but failed to finish.
            returned: success
            type: str
        maintenance:
            description: Whether or not this Node is currently in "maintenance
                         mode". Setting a Node into maintenance mode removes it
                         from the available resource pool and halts some
                         internal automation. This can happen manually (eg, via
                         an API request) or automatically when Ironic detects a
                         hardware fault that prevents communication with the
                         machine.
            returned: success
            type: bool
        maintenance_reason:
            description: User-settable description of the reason why this Node
                         was placed into maintenance mode
            returned: success
            type: str
        management_interface:
            description: Interface for out-of-band node management.
            returned: success
            type: str
        name:
            description: Human-readable identifier for the Node resource. May
                         be undefined. Certain words are reserved.
            returned: success
            type: str
        network_interface:
            description: Which Network Interface provider to use when plumbing
                         the network connections for this Node.
            returned: success
            type: str
        owner:
            description: A string or UUID of the tenant who owns the object.
            returned: success
            type: str
        portgroups:
            description: List of ironic portgroups on this node.
            returned: success
            type: list
            elements: dict
            contains:
              address:
                description: Physical hardware address of this Portgroup,
                             typically the hardware MAC address.
                returned: success
                type: str
              created_at:
                description: The UTC date and time when the resource was
                             created, ISO 8601 format.
                returned: success
                type: str
              extra:
                description: A set of one or more arbitrary metadata key and
                             value pairs.
                returned: success
                type: dict
              id:
                description: The UUID for the resource.
                returned: success
                type: str
              internal_info:
                description: Internal metadata set and stored by the Portgroup.
                             This field is read-only.
                returned: success
                type: dict
              is_standalone_ports_supported:
                description: Indicates whether ports that are members of this
                             portgroup can be used as stand-alone ports.
                returned: success
                type: bool
              mode:
                description: Mode of the port group. For possible values, refer
                             to https://www.kernel.org/doc/Documentation/networking/bonding.txt.
                             If not specified in a request to create a port
                             group, it will be set to the value of the
                             [DEFAULT]default_portgroup_mode configuration
                             option. When set, can not be removed from the port
                             group.
                returned: success
                type: str
              name:
                description: Human-readable identifier for the Portgroup
                             resource. May be undefined.
                returned: success
                type: str
              node_id:
                description: UUID of the Node this resource belongs to.
                returned: success
                type: str
              ports:
                description: List of port UUID's of ports belonging to this
                             portgroup.
                returned: success
                type: list
              properties:
                description: Key/value properties related to the port group's
                             configuration.
                returned: success
                type: dict
              updated_at:
                description: The UTC date and time when the resource was
                             updated, ISO 8601 format. May be "null".
                returned: success
                type: str
        ports:
            description: List of ironic ports on this node.
            returned: success
            type: list
            elements: dict
            contains:
              address:
                description: Physical hardware address of this network Port,
                             typically the hardware MAC address.
                returned: success
                type: str
              created_at:
                description: The UTC date and time when the resource was
                             created, ISO 8601 format.
                returned: success
                type: str
              extra:
                description: A set of one or more arbitrary metadata key and
                             value pairs.
                returned: success
                type: dict
              id:
                description: The UUID for the resource.
                returned: success
                type: str
              internal_info:
                description: Internal metadata set and stored by the Port. This
                             field is read-only.
                returned: success
                type: dict
              local_link_connection:
                description: The Port binding profile. If specified, must
                             contain switch_id (only a MAC address or an
                             OpenFlow based datapath_id of the switch are
                             accepted in this field) and port_id (identifier of
                             the physical port on the switch to which node's
                             port is connected to) fields. switch_info is an
                             optional string field to be used to store any
                             vendor-specific information.
                returned: success
                type: dict
              name:
                description: The name of the resource.
                returned: success
                type: str
              node_uuid:
                description: UUID of the Node this resource belongs to.
                returned: success
                type: str
              physical_network:
                description: The name of the physical network to which a port
                             is connected. May be empty.
                returned: success
                type: str
              portgroup_uuid:
                description: UUID of the Portgroup this resource belongs to.
                returned: success
                type: str
              pxe_enabled:
                description: Indicates whether PXE is enabled or disabled on
                             the Port.
                returned: success
                type: str
              updated_at:
                description: The UTC date and time when the resource was
                             updated, ISO 8601 format. May be "null".
                returned: success
                type: str
              uuid:
                description: The UUID for the resource.
                returned: success
                type: str
        power_interface:
            description: Interface used for performing power actions on the
                         node, e.g. "ipmitool".
            returned: success
            type: str
        power_state:
            description: The current power state of this Node. Usually, "power
                         on" or "power off", but may be "None" if Ironic is
                         unable to determine the power state (eg, due to
                         hardware failure).
            returned: success
            type: str
        properties:
            description: Physical characteristics of this Node. Populated by
                         ironic-inspector during inspection. May be edited via
                         the REST API at any time.
            returned: success
            type: dict
        protected:
            description: Whether the node is protected from undeploying,
                         rebuilding and deletion.
            returned: success
            type: bool
        protected_reason:
            description: The reason the node is marked as protected.
            returned: success
            type: str
        provision_state:
            description: The current provisioning state of this Node.
            returned: success
            type: str
        raid_config:
            description: Represents the current RAID configuration of the node.
                         Introduced with the cleaning feature.
            returned: success
            type: dict
        raid_interface:
            description: Interface used for configuring RAID on this node.
            returned: success
            type: str
        rescue_interface:
            description: The interface used for node rescue, e.g. "no-rescue".
            returned: success
            type: str
        reservation:
            description: The name of an Ironic Conductor host which is holding
                         a lock on this node, if a lock is held. Usually
                         "null", but this field can be useful for debugging.
            returned: success
            type: str
        resource_class:
            description: A string which can be used by external schedulers to
                         identify this Node as a unit of a specific type of
                         resource. For more details, see
                         https://docs.openstack.org/ironic/latest/install/configure-nova-flavors.html
            returned: success
            type: str
        retired:
            description: Whether the node is retired and can hence no longer be
                         provided, i.e. move from manageable to available, and
                         will end up in manageable after cleaning (rather than
                         available).
            returned: success
            type: bool
        retired_reason:
            description: The reason the node is marked as retired.
            returned: success
            type: str
        secure_boot:
            description: Indicates whether node is currently booted with
                         secure_boot turned on.
            returned: success
            type: bool
        storage_interface:
            description: Interface used for attaching and detaching volumes on
                         this node, e.g. "cinder".
            returned: success
            type: str
        target_power_state:
            description: If a power state transition has been requested, this
                         field represents the requested (ie, "target") state,
                         either "power on" or "power off".
            returned: success
            type: str
        target_provision_state:
            description: If a provisioning action has been requested, this
                         field represents the requested (ie, "target") state.
                         Note that a Node may go through several states during
                         its transition to this target state. For instance,
                         when requesting an instance be deployed to an
                         AVAILABLE Node, the Node may go through the following
                         state change progression, AVAILABLE -> DEPLOYING ->
                         DEPLOYWAIT -> DEPLOYING -> ACTIVE
            returned: success
            type: str
        target_raid_config:
            description: Represents the requested RAID configuration of the
                         node, which will be applied when the Node next
                         transitions through the CLEANING state. Introduced
                         with the cleaning feature.
            returned: success
            type: dict
        traits:
            description: List of traits for this node.
            returned: success
            type: list
        updated_at:
            description: Bare Metal node updated at timestamp.
            returned: success
            type: str
        uuid:
            description: The UUID for the resource.
            returned: success
            type: str
        vendor_interface:
            description: Interface for vendor-specific functionality on this
                         node, e.g. "no-vendor".
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


def cleanup_node_properties(machine, cloud):
    # states are links, not useful
    machine.pop('states', None)

    for port in machine.ports:
        # links are not useful
        port.pop('links', None)
        # redundant, location is in on machine as well
        port.pop('location', None)

    for portgroup in machine.portgroups:
        # links are not useful
        portgroup.pop('links', None)
        # redundant, location is in on machine as well
        portgroup.pop('location', None)
        # links to ports are not useful, replace with list of port uuid's
        portgroup['ports'] = [x.id for x in list(
            cloud.baremetal.ports(portgroup=portgroup['id']))]


def get_ports_and_portgroups(cloud, machine):
    machine.ports = cloud.list_nics_for_machine(machine.uuid)
    machine.portgroups = [dict(x) for x in
                          list(cloud.baremetal.port_groups(node=machine.uuid,
                                                           details=True))]


def main():
    argument_spec = ironic_argument_spec(
        node=dict(required=False),
        mac=dict(required=False),
    )
    module_kwargs = openstack_module_kwargs()
    module_kwargs['supports_check_mode'] = True

    module = IronicModule(argument_spec, **module_kwargs)

    machine = None
    machines = list()

    sdk, cloud = openstack_cloud_from_module(module)
    try:
        if module.params['node']:
            machine = cloud.get_machine(module.params['node'])
        elif module.params['mac']:
            machine = cloud.get_machine_by_mac(module.params['mac'])

        # Fail if node not found
        if (module.params['node'] or module.params['mac']) and not machine:
            module.fail_json(msg='The baremetal node was not found')

        if machine:
            machines.append(machine)
        else:
            machines = cloud.list_machines()

        for machine in machines:
            get_ports_and_portgroups(cloud, machine)
            cleanup_node_properties(machine, cloud)

        module.exit_json(changed=False, baremetal_nodes=machines)
    except sdk.exceptions.OpenStackCloudException as e:
        module.fail_json(msg=str(e))


if __name__ == "__main__":
    main()
