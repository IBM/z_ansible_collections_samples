#!/usr/bin/python
# Copyright: Ansible Project
# GNU General Public License v3.0+ (see COPYING or https://www.gnu.org/licenses/gpl-3.0.txt)

DOCUMENTATION = '''
---
module: federation_mapping
short_description: Manage a federation mapping
author: OpenStack Ansible SIG
description:
  - Manage a federation mapping.
options:
  name:
    description:
      - The name of the mapping to manage.
    required: true
    type: str
    aliases: ['id']
  state:
    description:
      - Whether the mapping should be C(present) or C(absent).
    choices: ['present', 'absent']
    default: present
    type: str
  rules:
    description:
      - The rules that comprise the mapping.  These are pairs of I(local) and
        I(remote) definitions.  For more details on how these work please see
        the OpenStack documentation
        U(https://docs.openstack.org/keystone/latest/admin/federation/mapping_combinations.html).
      - Required if I(state=present)
    type: list
    elements: dict
    suboptions:
      local:
        description:
        - Information on what local attributes will be mapped.
        required: true
        type: list
        elements: dict
      remote:
        description:
        - Information on what remote attributes will be mapped.
        required: true
        type: list
        elements: dict
requirements:
  - "python >= 3.6"
  - "openstacksdk >= 0.44"
extends_documentation_fragment:
  - openstack.cloud.openstack
'''

EXAMPLES = '''
- name: Create a new mapping
  openstack.cloud.federation_mapping:
    cloud: example_cloud
    name: example_mapping
    rules:
    - local:
      - user:
          name: '{0}'
      - group:
          id: '0cd5e9'
      remote:
      - type: UserName
      - type: orgPersonType
        any_one_of:
        - Contractor
        - SubContractor

- name: Delete a mapping
  openstack.cloud.federation_mapping:
    name: example_mapping
    state: absent
'''

RETURN = '''
'''

from ansible_collections.openstack.cloud.plugins.module_utils.openstack import OpenStackModule


class IdentityFederationMappingModule(OpenStackModule):
    argument_spec = dict(
        name=dict(required=True, aliases=['id']),
        state=dict(default='present', choices=['absent', 'present']),
        rules=dict(type='list', elements='dict', options=dict(
            local=dict(required=True, type='list', elements='dict'),
            remote=dict(required=True, type='list', elements='dict')
        )),
    )
    module_kwargs = dict(
        required_if=[('state', 'present', ['rules'])],
        supports_check_mode=True
    )

    def normalize_mapping(self, mapping):
        """
        Normalizes the mapping definitions so that the outputs are consistent with
        the parameters

        - "name" (parameter) == "id" (SDK)
        """
        if mapping is None:
            return None

        _mapping = mapping.to_dict()
        _mapping['name'] = mapping['id']
        return _mapping

    def create_mapping(self, name):
        """
        Attempt to create a Mapping

        returns: A tuple containing the "Changed" state and the created mapping
        """

        if self.ansible.check_mode:
            return (True, None)

        rules = self.params.get('rules')

        mapping = self.conn.identity.create_mapping(id=name, rules=rules)
        return (True, mapping)

    def delete_mapping(self, mapping):
        """
        Attempt to delete a Mapping

        returns: the "Changed" state
        """
        if mapping is None:
            return False

        if self.ansible.check_mode:
            return True

        self.conn.identity.delete_mapping(mapping)
        return True

    def update_mapping(self, mapping):
        """
        Attempt to delete a Mapping

        returns: The "Changed" state and the the new mapping
        """

        current_rules = mapping.rules
        new_rules = self.params.get('rules')

        # Nothing to do
        if current_rules == new_rules:
            return (False, mapping)

        if self.ansible.check_mode:
            return (True, None)

        new_mapping = self.conn.identity.update_mapping(mapping, rules=new_rules)
        return (True, new_mapping)

    def run(self):
        """ Module entry point """

        name = self.params.get('name')
        state = self.params.get('state')
        changed = False

        mapping = self.conn.identity.find_mapping(name)

        if state == 'absent':
            if mapping is not None:
                changed = self.delete_mapping(mapping)
            self.exit_json(changed=changed)

        # state == 'present'
        else:
            if len(self.params.get('rules')) < 1:
                self.fail_json(msg='At least one rule must be passed')

            if mapping is None:
                (changed, mapping) = self.create_mapping(name)
                mapping = self.normalize_mapping(mapping)
                self.exit_json(changed=changed, mapping=mapping)
            else:
                (changed, new_mapping) = self.update_mapping(mapping)
                new_mapping = self.normalize_mapping(new_mapping)
                self.exit_json(mapping=new_mapping, changed=changed)


def main():
    module = IdentityFederationMappingModule()
    module()


if __name__ == '__main__':
    main()
