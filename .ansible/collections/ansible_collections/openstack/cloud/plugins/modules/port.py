#!/usr/bin/python

# Copyright (c) 2015 Hewlett-Packard Development Company, L.P.
# GNU General Public License v3.0+ (see COPYING or https://www.gnu.org/licenses/gpl-3.0.txt)

DOCUMENTATION = '''
---
module: port
short_description: Add/Update/Delete ports from an OpenStack cloud.
author: OpenStack Ansible SIG
description:
   - Add, Update or Remove ports from an OpenStack cloud. A I(state) of
     'present' will ensure the port is created or updated if required.
options:
   network:
     description:
        - Network ID or name this port belongs to.
        - Required when creating a new port.
     type: str
   name:
     description:
        - Name that has to be given to the port.
     type: str
   fixed_ips:
     description:
        - Desired IP and/or subnet for this port.  Subnet is referenced by
          subnet_id and IP is referenced by ip_address.
     type: list
     elements: dict
     suboptions:
        ip_address:
           description: The fixed IP address to attempt to allocate.
           required: true
           type: str
        subnet_id:
           description: The subnet to attach the IP address to.
           type: str
   admin_state_up:
     description:
        - Sets admin state.
     type: bool
   mac_address:
     description:
        - MAC address of this port.
     type: str
   security_groups:
     description:
        - Security group(s) ID(s) or name(s) associated with the port (comma
          separated string or YAML list)
     type: list
     elements: str
   no_security_groups:
     description:
        - Do not associate a security group with this port.
     type: bool
     default: 'no'
   allowed_address_pairs:
     description:
        - "Allowed address pairs list.  Allowed address pairs are supported with
          dictionary structure.
          e.g.  allowed_address_pairs:
                  - ip_address: 10.1.0.12
                    mac_address: ab:cd:ef:12:34:56
                  - ip_address: ..."
     type: list
     elements: dict
     suboptions:
        ip_address:
           description: The IP address.
           type: str
        mac_address:
           description: The MAC address.
           type: str
   extra_dhcp_opts:
     description:
        - "Extra dhcp options to be assigned to this port. Extra options are
          supported with dictionary structure. Note that options cannot be removed
          only updated.
          e.g.  extra_dhcp_opts:
                  - opt_name: opt name1
                    opt_value: value1
                    ip_version: 4
                  - opt_name: ..."
     type: list
     elements: dict
     suboptions:
        opt_name:
           description: The name of the DHCP option to set.
           type: str
           required: true
        opt_value:
           description: The value of the DHCP option to set.
           type: str
           required: true
        ip_version:
           description: The IP version this DHCP option is for.
           type: int
           required: true
   device_owner:
     description:
        - The ID of the entity that uses this port.
     type: str
   device_id:
     description:
        - Device ID of device using this port.
     type: str
   state:
     description:
       - Should the resource be present or absent.
     choices: [present, absent]
     default: present
     type: str
   vnic_type:
     description:
       - The type of the port that should be created
     choices: [normal, direct, direct-physical, macvtap, baremetal, virtio-forwarder]
     type: str
   port_security_enabled:
     description:
       - Whether to enable or disable the port security on the network.
     type: bool
   binding_profile:
     description:
       - Binding profile dict that the port should be created with.
     type: dict
   dns_name:
     description:
       - The dns name of the port ( only with dns-integration enabled )
     type: str
   dns_domain:
     description:
       - The dns domain of the port ( only with dns-integration enabled )
     type: str
requirements:
    - "python >= 3.6"
    - "openstacksdk"

extends_documentation_fragment:
- openstack.cloud.openstack
'''

EXAMPLES = '''
# Create a port
- openstack.cloud.port:
    state: present
    auth:
      auth_url: https://identity.example.com
      username: admin
      password: admin
      project_name: admin
    name: port1
    network: foo

# Create a port with a static IP
- openstack.cloud.port:
    state: present
    auth:
      auth_url: https://identity.example.com
      username: admin
      password: admin
      project_name: admin
    name: port1
    network: foo
    fixed_ips:
      - ip_address: 10.1.0.21

# Create a port with No security groups
- openstack.cloud.port:
    state: present
    auth:
      auth_url: https://identity.example.com
      username: admin
      password: admin
      project_name: admin
    name: port1
    network: foo
    no_security_groups: True

# Update the existing 'port1' port with multiple security groups (version 1)
- openstack.cloud.port:
    state: present
    auth:
      auth_url: https://identity.example.com
      username: admin
      password: admin
      project_name: admin
    name: port1
    security_groups: 1496e8c7-4918-482a-9172-f4f00fc4a3a5,057d4bdf-6d4d-472...

# Update the existing 'port1' port with multiple security groups (version 2)
- openstack.cloud.port:
    state: present
    auth:
      auth_url: https://identity.example.com
      username: admin
      password: admin
      project_name: admin
    name: port1
    security_groups:
      - 1496e8c7-4918-482a-9172-f4f00fc4a3a5
      - 057d4bdf-6d4d-472...

# Create port of type 'direct'
- openstack.cloud.port:
    state: present
    auth:
      auth_url: https://identity.example.com
      username: admin
      password: admin
      project_name: admin
    name: port1
    network: foo
    vnic_type: direct

# Create a port with binding profile
- openstack.cloud.port:
    state: present
    auth:
      auth_url: https://identity.example.com
      username: admin
      password: admin
      project_name: admin
    name: port1
    network: foo
    binding_profile:
      "pci_slot": "0000:03:11.1"
      "physical_network": "provider"
'''

RETURN = '''
id:
    description: Unique UUID.
    returned: success
    type: str
name:
    description: Name given to the port.
    returned: success
    type: str
network_id:
    description: Network ID this port belongs in.
    returned: success
    type: str
security_groups:
    description: Security group(s) associated with this port.
    returned: success
    type: list
status:
    description: Port's status.
    returned: success
    type: str
fixed_ips:
    description: Fixed ip(s) associated with this port.
    returned: success
    type: list
tenant_id:
    description: Tenant id associated with this port.
    returned: success
    type: str
allowed_address_pairs:
    description: Allowed address pairs with this port.
    returned: success
    type: list
admin_state_up:
    description: Admin state up flag for this port.
    returned: success
    type: bool
vnic_type:
    description: Type of the created port
    returned: success
    type: str
port_security_enabled:
    description: Port security state on the network.
    returned: success
    type: bool
binding:profile:
    description: Port binded profile
    returned: success
    type: dict
'''

from ansible.module_utils.basic import missing_required_lib
from ansible_collections.openstack.cloud.plugins.module_utils.openstack import OpenStackModule

try:
    from collections import OrderedDict
    HAS_ORDEREDDICT = True
except ImportError:
    try:
        from ordereddict import OrderedDict
        HAS_ORDEREDDICT = True
    except ImportError:
        HAS_ORDEREDDICT = False


class NetworkPortModule(OpenStackModule):
    argument_spec = dict(
        network=dict(required=False),
        name=dict(required=False),
        fixed_ips=dict(type='list', default=None, elements='dict'),
        admin_state_up=dict(type='bool', default=None),
        mac_address=dict(default=None),
        security_groups=dict(default=None, type='list', elements='str'),
        no_security_groups=dict(default=False, type='bool'),
        allowed_address_pairs=dict(type='list', default=None, elements='dict'),
        extra_dhcp_opts=dict(type='list', default=None, elements='dict'),
        device_owner=dict(default=None),
        device_id=dict(default=None),
        state=dict(default='present', choices=['absent', 'present']),
        vnic_type=dict(default=None,
                       choices=['normal', 'direct', 'direct-physical',
                                'macvtap', 'baremetal', 'virtio-forwarder']),
        port_security_enabled=dict(default=None, type='bool'),
        binding_profile=dict(default=None, type='dict'),
        dns_name=dict(type='str', default=None),
        dns_domain=dict(type='str', default=None)
    )

    module_kwargs = dict(
        mutually_exclusive=[
            ['no_security_groups', 'security_groups'],
        ],
        supports_check_mode=True
    )

    def _is_dns_integration_enabled(self):
        """ Check if dns-integraton is enabled """
        for ext in self.conn.network.extensions():
            if ext.alias == 'dns-integration':
                return True
        return False

    def _needs_update(self, port):
        """Check for differences in the updatable values.

        NOTE: We don't currently allow name updates.
        """
        compare_simple = ['admin_state_up',
                          'mac_address',
                          'device_owner',
                          'device_id',
                          'binding:vnic_type',
                          'port_security_enabled',
                          'binding:profile']
        compare_dns = ['dns_name', 'dns_domain']
        compare_list_dict = ['allowed_address_pairs',
                             'extra_dhcp_opts']
        compare_list = ['security_groups']

        if self.conn.has_service('dns') and \
           self._is_dns_integration_enabled():
            for key in compare_dns:
                if self.params[key] is not None and \
                   self.params[key] != port[key]:
                    return True

        for key in compare_simple:
            if self.params[key] is not None and self.params[key] != port[key]:
                return True
        for key in compare_list:
            if (
                self.params[key] is not None
                and set(self.params[key]) != set(port[key])
            ):
                return True

        for key in compare_list_dict:
            if not self.params[key]:
                if port.get(key):
                    return True

            if self.params[key]:
                if not port.get(key):
                    return True

                # sort dicts in list
                port_ordered = [OrderedDict(sorted(d.items())) for d in port[key]]
                param_ordered = [OrderedDict(sorted(d.items())) for d in self.params[key]]

                for d in param_ordered:
                    if d not in port_ordered:
                        return True

                for d in port_ordered:
                    if d not in param_ordered:
                        return True

        # NOTE: if port was created or updated with 'no_security_groups=True',
        # subsequent updates without 'no_security_groups' flag or
        # 'no_security_groups=False' and no specified 'security_groups', will not
        # result in an update to the port where the default security group is
        # applied.
        if self.params['no_security_groups'] and port['security_groups'] != []:
            return True

        if self.params['fixed_ips'] is not None:
            for item in self.params['fixed_ips']:
                if 'ip_address' in item:
                    # if ip_address in request does not match any in existing port,
                    # update is required.
                    if not any(match['ip_address'] == item['ip_address']
                               for match in port['fixed_ips']):
                        return True
                if 'subnet_id' in item:
                    return True
            for item in port['fixed_ips']:
                # if ip_address in existing port does not match any in request,
                # update is required.
                if not any(match.get('ip_address') == item['ip_address']
                           for match in self.params['fixed_ips']):
                    return True

        return False

    def _system_state_change(self, port):
        state = self.params['state']
        if state == 'present':
            if not port:
                return True
            return self._needs_update(port)
        if state == 'absent' and port:
            return True
        return False

    def _compose_port_args(self):
        port_kwargs = {}
        optional_parameters = ['name',
                               'fixed_ips',
                               'admin_state_up',
                               'mac_address',
                               'security_groups',
                               'allowed_address_pairs',
                               'extra_dhcp_opts',
                               'device_owner',
                               'device_id',
                               'binding:vnic_type',
                               'port_security_enabled',
                               'binding:profile']

        if self.conn.has_service('dns') and \
           self._is_dns_integration_enabled():
            optional_parameters.extend(['dns_name', 'dns_domain'])

        for optional_param in optional_parameters:
            if self.params[optional_param] is not None:
                port_kwargs[optional_param] = self.params[optional_param]

        if self.params['no_security_groups']:
            port_kwargs['security_groups'] = []

        return port_kwargs

    def get_security_group_id(self, security_group_name_or_id):
        security_group = self.conn.get_security_group(security_group_name_or_id)
        if not security_group:
            self.fail_json(msg="Security group: %s, was not found"
                           % security_group_name_or_id)
        return security_group['id']

    def run(self):
        if not HAS_ORDEREDDICT:
            self.fail_json(msg=missing_required_lib('ordereddict'))

        name = self.params['name']
        state = self.params['state']

        if self.params['security_groups']:
            # translate security_groups to UUID's if names where provided
            self.params['security_groups'] = [
                self.get_security_group_id(v)
                for v in self.params['security_groups']
            ]

        # Neutron API accept 'binding:vnic_type' as an argument
        # for the port type.
        self.params['binding:vnic_type'] = self.params.pop('vnic_type')
        # Neutron API accept 'binding:profile' as an argument
        # for the port binding profile type.
        self.params['binding:profile'] = self.params.pop('binding_profile')

        port = None
        network_id = None
        if name:
            port = self.conn.get_port(name)

        if self.ansible.check_mode:
            self.exit_json(changed=self._system_state_change(port))

        changed = False
        if state == 'present':
            if not port:
                network = self.params['network']
                if not network:
                    self.fail_json(
                        msg="Parameter 'network' is required in Port Create"
                    )
                port_kwargs = self._compose_port_args()
                network_object = self.conn.get_network(network)

                if network_object:
                    network_id = network_object['id']
                else:
                    self.fail_json(
                        msg="Specified network was not found."
                    )

                port_kwargs['network_id'] = network_id
                port = self.conn.network.create_port(**port_kwargs)
                changed = True
            else:
                if self._needs_update(port):
                    port_kwargs = self._compose_port_args()
                    port = self.conn.network.update_port(port['id'],
                                                         **port_kwargs)
                    changed = True
            self.exit_json(changed=changed, id=port['id'], port=port)

        if state == 'absent':
            if port:
                self.conn.delete_port(port['id'])
                changed = True
            self.exit_json(changed=changed)


def main():
    module = NetworkPortModule()
    module()


if __name__ == '__main__':
    main()
