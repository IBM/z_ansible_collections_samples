#!/usr/bin/python
# Copyright: Ansible Project
# GNU General Public License v3.0+ (see COPYING or https://www.gnu.org/licenses/gpl-3.0.txt)

DOCUMENTATION = '''
---
module: federation_mapping_info
short_description: Get the information about the available federation mappings
author: OpenStack Ansible SIG
description:
  - Fetch a federation mapping.
options:
  name:
    description:
      - The name of the mapping to fetch.
      - If I(name) is specified, the module will return failed if the mapping
        doesn't exist.
    type: str
    aliases: ['id']
requirements:
  - "python >= 3.6"
  - "openstacksdk >= 0.44"
extends_documentation_fragment:
  - openstack.cloud.openstack
'''

EXAMPLES = '''
- name: Fetch a specific mapping
  openstack.cloud.federation_mapping_info:
    cloud: example_cloud
    name: example_mapping

- name: Fetch all mappings
  openstack.cloud.federation_mapping_info:
    cloud: example_cloud
'''

RETURN = '''
'''

from ansible_collections.openstack.cloud.plugins.module_utils.openstack import OpenStackModule


class IdentityFederationMappingInfoModule(OpenStackModule):
    argument_spec = dict(
        name=dict(aliases=['id']),
    )
    module_kwargs = dict(
        supports_check_mode=True
    )

    module_min_sdk_version = "0.44"

    def normalize_mapping(self, mapping):
        """
        Normalizes the mapping definitions so that the outputs are consistent with the
        parameters

        - "name" (parameter) == "id" (SDK)
        """
        if mapping is None:
            return None

        _mapping = mapping.to_dict()
        _mapping['name'] = mapping['id']
        return _mapping

    def run(self):
        """ Module entry point """
        name = self.params.get('name')

        if name:
            mapping = self.normalize_mapping(
                self.conn.identity.get_mapping(name))
            self.exit_json(changed=False, mappings=[mapping])
        else:
            mappings = list(map(
                self.normalize_mapping, self.conn.identity.mappings()))
            self.exit_json(changed=False, mappings=mappings)


def main():
    module = IdentityFederationMappingInfoModule()
    module()


if __name__ == '__main__':
    main()
