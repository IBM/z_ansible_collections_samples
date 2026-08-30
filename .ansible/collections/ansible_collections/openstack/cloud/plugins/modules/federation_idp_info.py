#!/usr/bin/python
# Copyright: Ansible Project
# GNU General Public License v3.0+ (see COPYING or https://www.gnu.org/licenses/gpl-3.0.txt)

DOCUMENTATION = '''
---
module: federation_idp_info
short_description: Get the information about the available federation identity
                   providers
author: OpenStack Ansible SIG
description:
  - Fetch a federation identity provider.
options:
  name:
    description:
      - The name of the identity provider to fetch.
      - If I(name) is specified, the module will return failed if the identity
        provider doesn't exist.
    type: str
    aliases: ['id']
requirements:
  - "python >= 3.6"
  - "openstacksdk >= 0.44"
extends_documentation_fragment:
  - openstack.cloud.openstack
'''

EXAMPLES = '''
- name: Fetch a specific identity provider
  openstack.cloud.federation_idp_info:
    cloud: example_cloud
    name: example_provider

- name: Fetch all providers
  openstack.cloud.federation_idp_info:
    cloud: example_cloud
'''

RETURN = '''
'''

from ansible_collections.openstack.cloud.plugins.module_utils.openstack import OpenStackModule


class IdentityFederationIdpInfoModule(OpenStackModule):
    argument_spec = dict(
        name=dict(aliases=['id']),
    )
    module_kwargs = dict(
        supports_check_mode=True
    )

    def normalize_idp(self, idp):
        """
        Normalizes the IDP definitions so that the outputs are consistent with the
        parameters

        - "enabled" (parameter) == "is_enabled" (SDK)
        - "name" (parameter) == "id" (SDK)
        """
        if idp is None:
            return

        _idp = idp.to_dict()
        _idp['enabled'] = idp['is_enabled']
        _idp['name'] = idp['id']
        return _idp

    def run(self):
        """ Module entry point """

        name = self.params.get('name')

        if name:
            idp = self.normalize_idp(self.conn.identity.get_identity_provider(name))
            self.exit_json(changed=False, identity_providers=[idp])

        else:
            providers = list(map(self.normalize_idp, self.conn.identity.identity_providers()))
            self.exit_json(changed=False, identity_providers=providers)


def main():
    module = IdentityFederationIdpInfoModule()
    module()


if __name__ == '__main__':
    main()
