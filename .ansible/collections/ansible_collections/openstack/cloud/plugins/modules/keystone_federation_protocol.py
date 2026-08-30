#!/usr/bin/python
# Copyright: Ansible Project
# GNU General Public License v3.0+ (see COPYING or https://www.gnu.org/licenses/gpl-3.0.txt)

DOCUMENTATION = '''
---
module: keystone_federation_protocol
short_description: manage a federation Protocol
author: OpenStack Ansible SIG
description:
  - Manage a federation Protocol.
options:
  name:
    description:
      - The name of the Protocol.
    type: str
    required: true
    aliases: ['id']
  state:
    description:
      - Whether the protocol should be C(present) or C(absent).
    choices: ['present', 'absent']
    default: present
    type: str
  idp_id:
    description:
      - The name of the Identity Provider this Protocol is associated with.
    aliases: ['idp_name']
    required: true
    type: str
  mapping_id:
    description:
      - The name of the Mapping to use for this Protocol.'
      - Required when creating a new Protocol.
    type: str
    aliases: ['mapping_name']
requirements:
  - "python >= 3.6"
  - "openstacksdk >= 0.44"
extends_documentation_fragment:
  - openstack.cloud.openstack
'''

EXAMPLES = '''
- name: Create a protocol
  openstack.cloud.keystone_federation_protocol:
    cloud: example_cloud
    name: example_protocol
    idp_id: example_idp
    mapping_id: example_mapping

- name: Delete a protocol
  openstack.cloud.keystone_federation_protocol:
    cloud: example_cloud
    name: example_protocol
    idp_id: example_idp
    state: absent
'''

RETURN = '''
'''

from ansible_collections.openstack.cloud.plugins.module_utils.openstack import OpenStackModule


class IdentityFederationProtocolModule(OpenStackModule):
    argument_spec = dict(
        name=dict(required=True, aliases=['id']),
        state=dict(default='present', choices=['absent', 'present']),
        idp_id=dict(required=True, aliases=['idp_name']),
        mapping_id=dict(aliases=['mapping_name']),
    )
    module_kwargs = dict(
        supports_check_mode=True
    )

    def normalize_protocol(self, protocol):
        """
        Normalizes the protocol definitions so that the outputs are consistent with the
        parameters

        - "name" (parameter) == "id" (SDK)
        """
        if protocol is None:
            return None

        _protocol = protocol.to_dict()
        _protocol['name'] = protocol['id']
        # As of 0.44 SDK doesn't copy the URI parameters over, so let's add them
        _protocol['idp_id'] = protocol['idp_id']
        return _protocol

    def delete_protocol(self, protocol):
        """
        Delete an existing Protocol

        returns: the "Changed" state
        """
        if protocol is None:
            return False

        if self.ansible.check_mode:
            return True

        self.conn.identity.delete_federation_protocol(None, protocol)
        return True

    def create_protocol(self, name):
        """
        Create a new Protocol

        returns: the "Changed" state and the new protocol
        """
        if self.ansible.check_mode:
            return True, None

        idp_name = self.params.get('idp_id')
        mapping_id = self.params.get('mapping_id')

        attributes = {
            'idp_id': idp_name,
            'mapping_id': mapping_id,
        }

        protocol = self.conn.identity.create_federation_protocol(id=name, **attributes)
        return (True, protocol)

    def update_protocol(self, protocol):
        """
        Update an existing Protocol

        returns: the "Changed" state and the new protocol
        """
        mapping_id = self.params.get('mapping_id')

        attributes = {}

        if (mapping_id is not None) and (mapping_id != protocol.mapping_id):
            attributes['mapping_id'] = mapping_id

        if not attributes:
            return False, protocol

        if self.ansible.check_mode:
            return True, None

        new_protocol = self.conn.identity.update_federation_protocol(None, protocol, **attributes)
        return (True, new_protocol)

    def run(self):
        """ Module entry point """
        name = self.params.get('name')
        state = self.params.get('state')
        idp = self.params.get('idp_id')
        changed = False

        protocol = self.conn.identity.find_federation_protocol(idp, name)

        if state == 'absent':
            if protocol is not None:
                changed = self.delete_protocol(protocol)
            self.exit_json(changed=changed)

        # state == 'present'
        else:
            if protocol is None:
                if self.params.get('mapping_id') is None:
                    self.fail_json(
                        msg='A mapping_id must be passed when creating'
                        ' a protocol')
                (changed, protocol) = self.create_protocol(name)
                protocol = self.normalize_protocol(protocol)
                self.exit_json(changed=changed, protocol=protocol)

            else:
                (changed, new_protocol) = self.update_protocol(protocol)
                new_protocol = self.normalize_protocol(new_protocol)
                self.exit_json(changed=changed, protocol=new_protocol)


def main():
    module = IdentityFederationProtocolModule()
    module()


if __name__ == '__main__':
    main()
