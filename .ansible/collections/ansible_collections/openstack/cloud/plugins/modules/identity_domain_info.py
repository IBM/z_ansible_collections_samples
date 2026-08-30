#!/usr/bin/python
# Copyright (c) 2016 Hewlett-Packard Enterprise Corporation
# GNU General Public License v3.0+ (see COPYING or https://www.gnu.org/licenses/gpl-3.0.txt)

DOCUMENTATION = '''
---
module: identity_domain_info
short_description: Retrieve information about one or more OpenStack domains
author: OpenStack Ansible SIG
description:
    - Retrieve information about a one or more OpenStack domains
    - This module was called C(openstack.cloud.identity_domain_facts) before Ansible 2.9, returning C(ansible_facts).
      Note that the M(openstack.cloud.identity_domain_info) module no longer returns C(ansible_facts)!
options:
   name:
     description:
        - Name or ID of the domain
     type: str
   filters:
     description:
        - A dictionary of meta data to use for filtering. Elements of
          this dictionary may be additional dictionaries.
     type: dict
requirements:
    - "python >= 3.6"
    - "openstacksdk"

extends_documentation_fragment:
- openstack.cloud.openstack
'''

EXAMPLES = '''
# Gather information about previously created domain
- openstack.cloud.identity_domain_info:
    cloud: awesomecloud
  register: result
- debug:
    msg: "{{ result.openstack_domains }}"

# Gather information about a previously created domain by name
- openstack.cloud.identity_domain_info:
    cloud: awesomecloud
    name: demodomain
  register: result
- debug:
    msg: "{{ result.openstack_domains }}"

# Gather information about a previously created domain with filter
- openstack.cloud.identity_domain_info:
    cloud: awesomecloud
    name: demodomain
    filters:
      enabled: false
  register: result
- debug:
    msg: "{{ result.openstack_domains }}"
'''


RETURN = '''
openstack_domains:
    description: has all the OpenStack information about domains
    returned: always, but can be null
    type: list
    elements: dict
    contains:
        id:
            description: Unique UUID.
            returned: success
            type: str
        name:
            description: Name given to the domain.
            returned: success
            type: str
        description:
            description: Description of the domain.
            returned: success
            type: str
        enabled:
            description: Flag to indicate if the domain is enabled.
            returned: success
            type: bool
'''

from ansible_collections.openstack.cloud.plugins.module_utils.openstack import OpenStackModule


class IdentityDomainInfoModule(OpenStackModule):
    argument_spec = dict(
        name=dict(required=False, default=None),
        filters=dict(required=False, type='dict', default=None),
    )

    module_kwargs = dict(
        supports_check_mode=True
    )

    deprecated_names = ('openstack.cloud.identity_domain_facts')

    def run(self):
        name = self.params['name']
        filters = self.params['filters'] or {}

        args = {}
        if name:
            args['name_or_id'] = name
        args['filters'] = filters

        domains = self.conn.search_domains(**args)
        self.exit_json(changed=False, openstack_domains=domains)


def main():
    module = IdentityDomainInfoModule()
    module()


if __name__ == '__main__':
    main()
