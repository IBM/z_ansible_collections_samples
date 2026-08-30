#!/usr/bin/python
# Copyright: Ansible Project
# GNU General Public License v3.0+ (see COPYING or https://www.gnu.org/licenses/gpl-3.0.txt)

DOCUMENTATION = '''
---
module: keystone_federation_protocol_info
short_description: get information about federation Protocols
author: OpenStack Ansible SIG
description:
  - Get information about federation Protocols.
options:
  name:
    description:
      - The name of the Protocol.
    type: str
    aliases: ['id']
  idp_id:
    description:
      - The name of the Identity Provider this Protocol is associated with.
    aliases: ['idp_name']
    required: true
    type: str
requirements:
  - "python >= 3.6"
  - "openstacksdk >= 0.44"
extends_documentation_fragment:
  - openstack.cloud.openstack
'''

EXAMPLES = '''
- name: Describe a protocol
  openstack.cloud.keystone_federation_protocol_info:
    cloud: example_cloud
    name: example_protocol
    idp_id: example_idp
    mapping_name: example_mapping

- name: Describe all protocols attached to an IDP
  openstack.cloud.keystone_federation_protocol_info:
    cloud: example_cloud
    idp_id: example_idp
'''

RETURN = '''
'''

from ansible_collections.openstack.cloud.plugins.module_utils.openstack import OpenStackModule


class IdentityFederationProtocolInfoModule(OpenStackModule):
    argument_spec = dict(
        name=dict(aliases=['id']),
        idp_id=dict(required=True, aliases=['idp_name']),
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

    def run(self):
        """ Module entry point """

        name = self.params.get('name')
        idp = self.params.get('idp_id')

        if name:
            protocol = self.conn.identity.get_federation_protocol(idp, name)
            protocol = self.normalize_protocol(protocol)
            self.exit_json(changed=False, protocols=[protocol])

        else:
            protocols = list(map(self.normalize_protocol, self.conn.identity.federation_protocols(idp)))
            self.exit_json(changed=False, protocols=protocols)


def main():
    module = IdentityFederationProtocolInfoModule()
    module()


if __name__ == '__main__':
    main()
