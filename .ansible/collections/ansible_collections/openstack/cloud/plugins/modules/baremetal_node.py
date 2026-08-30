#!/usr/bin/python
# coding: utf-8 -*-

# (c) 2014, Hewlett-Packard Development Company, L.P.
# GNU General Public License v3.0+ (see COPYING or https://www.gnu.org/licenses/gpl-3.0.txt)

DOCUMENTATION = '''
---
module: baremetal_node
short_description: Create/Delete Bare Metal Resources from OpenStack
author: OpenStack Ansible SIG
description:
    - Create or Remove Ironic nodes from OpenStack.
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
          be auto-generated if not specified, and name is specified.
        - Definition of a UUID will always take precedence to a name value.
      type: str
    name:
      description:
        - unique name identifier to be given to the resource.
      type: str
    driver:
      description:
        - The name of the Ironic Driver to use with this node.
        - Required when I(state=present)
      type: str
    chassis_uuid:
      description:
        - Associate the node with a pre-defined chassis.
      type: str
    ironic_url:
      description:
        - If noauth mode is utilized, this is required to be set to the
          endpoint URL for the Ironic API.  Use with "auth" and "auth_type"
          settings set to None.
      type: str
    resource_class:
      description:
        - The specific resource type to which this node belongs.
      type: str
    bios_interface:
      description:
        - The bios interface for this node, e.g. "no-bios".
      type: str
    boot_interface:
      description:
        - The boot interface for this node, e.g. "pxe".
      type: str
    console_interface:
      description:
        - The console interface for this node, e.g. "no-console".
      type: str
    deploy_interface:
      description:
        - The deploy interface for this node, e.g. "iscsi".
      type: str
    inspect_interface:
      description:
        - The interface used for node inspection, e.g. "no-inspect".
      type: str
    management_interface:
      description:
        - The interface for out-of-band management of this node, e.g.
          "ipmitool".
      type: str
    network_interface:
      description:
        - The network interface provider to use when describing
          connections for this node.
      type: str
    power_interface:
      description:
        - The interface used to manage power actions on this node, e.g.
          "ipmitool".
      type: str
    raid_interface:
      description:
        - Interface used for configuring raid on this node.
      type: str
    rescue_interface:
      description:
        - Interface used for node rescue, e.g. "no-rescue".
      type: str
    storage_interface:
      description:
        - Interface used for attaching and detaching volumes on this node, e.g.
          "cinder".
      type: str
    vendor_interface:
      description:
        - Interface for all vendor-specific actions on this node, e.g.
          "no-vendor".
      type: str
    driver_info:
      description:
        - Information for this server's driver. Will vary based on which
          driver is in use. Any sub-field which is populated will be validated
          during creation. For compatibility reasons sub-fields `power`,
          `deploy`, `management` and `console` are flattened.
      required: true
      type: dict
    nics:
      description:
        - 'A list of network interface cards, eg, " - mac: aa:bb:cc:aa:bb:cc"'
      required: true
      type: list
      elements: dict
      suboptions:
        mac:
            description: The MAC address of the network interface card.
            type: str
            required: true
    properties:
      description:
        - Definition of the physical characteristics of this server, used for scheduling purposes
      type: dict
      suboptions:
        cpu_arch:
          description:
            - CPU architecture (x86_64, i686, ...)
          default: x86_64
        cpus:
          description:
            - Number of CPU cores this machine has
          default: 1
        ram:
          description:
            - amount of RAM this machine has, in MB
          default: 1
        disk_size:
          description:
            - size of first storage device in this machine (typically /dev/sda), in GB
          default: 1
        capabilities:
          description:
            - special capabilities for the node, such as boot_option, node_role etc
              (see U(https://docs.openstack.org/ironic/latest/install/advanced.html)
              for more information)
          default: ""
        root_device:
          description:
            - Root disk device hints for deployment.
            - See U(https://docs.openstack.org/ironic/latest/install/advanced.html#specifying-the-disk-for-deployment-root-device-hints)
              for allowed hints.
          default: ""
    skip_update_of_masked_password:
      description:
        - Allows the code that would assert changes to nodes to skip the
          update if the change is a single line consisting of the password
          field.
        - As of Kilo, by default, passwords are always masked to API
          requests, which means the logic as a result always attempts to
          re-assert the password field.
        - C(skip_update_of_driver_password) is deprecated alias and will be removed in openstack.cloud 2.0.0.
      type: bool
      aliases:
        - skip_update_of_driver_password
requirements:
    - "python >= 3.6"
    - "openstacksdk"
    - "jsonpatch"

extends_documentation_fragment:
- openstack.cloud.openstack
'''

EXAMPLES = '''
# Enroll a node with some basic properties and driver info
- openstack.cloud.baremetal_node:
    cloud: "devstack"
    driver: "pxe_ipmitool"
    uuid: "00000000-0000-0000-0000-000000000002"
    properties:
      cpus: 2
      cpu_arch: "x86_64"
      ram: 8192
      disk_size: 64
      capabilities: "boot_option:local"
      root_device:
        wwn: "0x4000cca77fc4dba1"
    nics:
      - mac: "aa:bb:cc:aa:bb:cc"
      - mac: "dd:ee:ff:dd:ee:ff"
    driver_info:
      ipmi_address: "1.2.3.4"
      ipmi_username: "admin"
      ipmi_password: "adminpass"
    chassis_uuid: "00000000-0000-0000-0000-000000000001"

'''

try:
    import jsonpatch
    HAS_JSONPATCH = True
except ImportError:
    HAS_JSONPATCH = False


from ansible_collections.openstack.cloud.plugins.module_utils.ironic import (
    IronicModule,
    ironic_argument_spec,
)
from ansible_collections.openstack.cloud.plugins.module_utils.openstack import (
    openstack_module_kwargs,
    openstack_cloud_from_module
)


_PROPERTIES = {
    'cpu_arch': 'cpu_arch',
    'cpus': 'cpus',
    'ram': 'memory_mb',
    'disk_size': 'local_gb',
    'capabilities': 'capabilities',
    'root_device': 'root_device',
}


def _parse_properties(module):
    """Convert ansible properties into native ironic values.

    Also filter out any properties that are not set.
    """
    p = module.params['properties']
    return {to_key: p[from_key] for (from_key, to_key) in _PROPERTIES.items()
            if p.get(from_key) is not None}


def _parse_driver_info(sdk, module):
    info = module.params['driver_info'].copy()
    for deprecated in ('power', 'console', 'management', 'deploy'):
        if deprecated in info:
            info.update(info.pop(deprecated))
            module.deprecate("Suboption %s of the driver_info parameter of "
                             "'openstack.cloud.baremetal_node' is deprecated"
                             % deprecated, version='2.0.0',
                             collection_name='openstack.cloud')
    return info


def _choose_id_value(module):
    if module.params['uuid']:
        return module.params['uuid']
    if module.params['name']:
        return module.params['name']
    return None


def _choose_if_password_only(module, patch):
    if len(patch) == 1:
        if 'password' in patch[0]['path'] and module.params['skip_update_of_masked_password']:
            # Return false to abort update as the password appears
            # to be the only element in the patch.
            return False
    return True


def _exit_node_not_updated(module, server):
    module.exit_json(
        changed=False,
        result="Node not updated",
        uuid=server['uuid'],
        provision_state=server['provision_state']
    )


def main():
    argument_spec = ironic_argument_spec(
        uuid=dict(required=False),
        name=dict(required=False),
        driver=dict(required=False),
        resource_class=dict(required=False),
        bios_interface=dict(required=False),
        boot_interface=dict(required=False),
        console_interface=dict(required=False),
        deploy_interface=dict(required=False),
        inspect_interface=dict(required=False),
        management_interface=dict(required=False),
        network_interface=dict(required=False),
        power_interface=dict(required=False),
        raid_interface=dict(required=False),
        rescue_interface=dict(required=False),
        storage_interface=dict(required=False),
        vendor_interface=dict(required=False),
        driver_info=dict(type='dict', required=True),
        nics=dict(type='list', required=True, elements="dict"),
        properties=dict(type='dict', default={}),
        chassis_uuid=dict(required=False),
        skip_update_of_masked_password=dict(
            required=False,
            type='bool',
            aliases=['skip_update_of_driver_password'],
            deprecated_aliases=[dict(
                name='skip_update_of_driver_password',
                version='2.0.0',
                collection_name='openstack.cloud')]
        ),
        state=dict(required=False, default='present', choices=['present', 'absent'])
    )
    module_kwargs = openstack_module_kwargs()
    module = IronicModule(argument_spec, **module_kwargs)

    if not HAS_JSONPATCH:
        module.fail_json(msg='jsonpatch is required for this module')

    node_id = _choose_id_value(module)

    sdk, cloud = openstack_cloud_from_module(module)
    try:
        server = cloud.get_machine(node_id)
        if module.params['state'] == 'present':
            if module.params['driver'] is None:
                module.fail_json(msg="A driver must be defined in order "
                                     "to set a node to present.")

            properties = _parse_properties(module)
            driver_info = _parse_driver_info(sdk, module)
            kwargs = dict(
                driver=module.params['driver'],
                properties=properties,
                driver_info=driver_info,
                name=module.params['name'],
            )
            optional_field_names = ('resource_class',
                                    'bios_interface',
                                    'boot_interface',
                                    'console_interface',
                                    'deploy_interface',
                                    'inspect_interface',
                                    'management_interface',
                                    'network_interface',
                                    'power_interface',
                                    'raid_interface',
                                    'rescue_interface',
                                    'storage_interface',
                                    'vendor_interface')
            for i in optional_field_names:
                if module.params[i]:
                    kwargs[i] = module.params[i]

            if module.params['chassis_uuid']:
                kwargs['chassis_uuid'] = module.params['chassis_uuid']

            if server is None:
                # Note(TheJulia): Add a specific UUID to the request if
                # present in order to be able to re-use kwargs for if
                # the node already exists logic, since uuid cannot be
                # updated.
                if module.params['uuid']:
                    kwargs['uuid'] = module.params['uuid']

                server = cloud.register_machine(module.params['nics'],
                                                **kwargs)
                module.exit_json(changed=True, uuid=server['uuid'],
                                 provision_state=server['provision_state'])
            else:
                # TODO(TheJulia): Presently this does not support updating
                # nics.  Support needs to be added.
                #
                # Note(TheJulia): This message should never get logged
                # however we cannot realistically proceed if neither a
                # name or uuid was supplied to begin with.
                if not node_id:
                    module.fail_json(msg="A uuid or name value "
                                         "must be defined")

                # Note(TheJulia): Constructing the configuration to compare
                # against.  The items listed in the server_config block can
                # be updated via the API.

                server_config = dict(
                    driver=server['driver'],
                    properties=server['properties'],
                    driver_info=server['driver_info'],
                    name=server['name'],
                )

                # Add the pre-existing chassis_uuid only if
                # it is present in the server configuration.
                if hasattr(server, 'chassis_uuid'):
                    server_config['chassis_uuid'] = server['chassis_uuid']

                # Note(TheJulia): If a password is defined and concealed, a
                # patch will always be generated and re-asserted.
                patch = jsonpatch.JsonPatch.from_diff(server_config, kwargs)

                if not patch:
                    _exit_node_not_updated(module, server)
                elif _choose_if_password_only(module, list(patch)):
                    # Note(TheJulia): Normally we would allow the general
                    # exception catch below, however this allows a specific
                    # message.
                    try:
                        server = cloud.patch_machine(
                            server['uuid'],
                            list(patch))
                    except Exception as e:
                        module.fail_json(msg="Failed to update node, "
                                         "Error: %s" % e.message)

                    # Enumerate out a list of changed paths.
                    change_list = []
                    for change in list(patch):
                        change_list.append(change['path'])
                    module.exit_json(changed=True,
                                     result="Node Updated",
                                     changes=change_list,
                                     uuid=server['uuid'],
                                     provision_state=server['provision_state'])

            # Return not updated by default as the conditions were not met
            # to update.
            _exit_node_not_updated(module, server)

        if module.params['state'] == 'absent':
            if not node_id:
                module.fail_json(msg="A uuid or name value must be defined "
                                     "in order to remove a node.")

            if server is not None:
                cloud.unregister_machine(module.params['nics'],
                                         server['uuid'])
                module.exit_json(changed=True, result="deleted")
            else:
                module.exit_json(changed=False, result="Server not found")

    except sdk.exceptions.OpenStackCloudException as e:
        module.fail_json(msg=str(e))


if __name__ == "__main__":
    main()
