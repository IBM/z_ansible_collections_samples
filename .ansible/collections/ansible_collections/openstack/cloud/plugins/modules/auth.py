#!/usr/bin/python

# Copyright (c) 2015 Hewlett-Packard Development Company, L.P.
# GNU General Public License v3.0+ (see COPYING or https://www.gnu.org/licenses/gpl-3.0.txt)

DOCUMENTATION = '''
---
module: auth
short_description: Retrieve an auth token
author: OpenStack Ansible SIG
description:
    - Retrieve an auth token from an OpenStack Cloud
requirements:
    - "python >= 3.6"
    - "openstacksdk"
extends_documentation_fragment:
- openstack.cloud.openstack
'''

EXAMPLES = '''
- name: Authenticate to the cloud and retrieve the service catalog
  openstack.cloud.auth:
    cloud: rax-dfw

- name: Show service catalog
  debug:
    var: service_catalog
'''

RETURN = '''
auth_token:
    description: Openstack API Auth Token
    returned: success
    type: str
service_catalog:
    description: A dictionary of available API endpoints
    returned: success
    type: dict
'''

from ansible_collections.openstack.cloud.plugins.module_utils.openstack import OpenStackModule


class AuthModule(OpenStackModule):
    argument_spec = dict()
    module_kwargs = dict()

    def run(self):
        self.exit_json(
            changed=False,
            ansible_facts=dict(
                auth_token=self.conn.auth_token,
                service_catalog=self.conn.service_catalog))


def main():
    module = AuthModule()
    module()


if __name__ == '__main__':
    main()
