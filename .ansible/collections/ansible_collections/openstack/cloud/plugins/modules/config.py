#!/usr/bin/python

# Copyright (c) 2015 Hewlett-Packard Development Company, L.P.
# GNU General Public License v3.0+ (see COPYING or https://www.gnu.org/licenses/gpl-3.0.txt)

DOCUMENTATION = '''
---
module: config
short_description: Get OpenStack Client config
description:
  - Get I(openstack) client config data from clouds.yaml or environment
notes:
  - Facts are placed in the C(openstack.clouds) variable.
options:
   clouds:
     description:
        - List of clouds to limit the return list to. No value means return
          information on all configured clouds
     required: false
     default: []
     type: list
     elements: str
requirements:
    - "python >= 3.6"
    - "openstacksdk"
author: OpenStack Ansible SIG
'''

EXAMPLES = '''
- name: Get list of clouds that do not support security groups
  openstack.cloud.config:

- debug:
    var: "{{ item }}"
  with_items: "{{ openstack.clouds | rejectattr('secgroup_source', 'none') | list }}"

- name: Get the information back just about the mordred cloud
  openstack.cloud.config:
    clouds:
      - mordred
'''

try:
    import openstack.config
    from openstack import exceptions
    HAS_OPENSTACKSDK = True
except ImportError:
    HAS_OPENSTACKSDK = False

from ansible.module_utils.basic import AnsibleModule


def main():
    module = AnsibleModule(argument_spec=dict(
        clouds=dict(required=False, type='list', default=[], elements='str'),
    ))

    if not HAS_OPENSTACKSDK:
        module.fail_json(msg='openstacksdk is required for this module')

    p = module.params

    try:
        config = openstack.config.OpenStackConfig()
        clouds = []
        for cloud in config.get_all_clouds():
            if not p['clouds'] or cloud.name in p['clouds']:
                cloud.config['name'] = cloud.name
                clouds.append(cloud.config)
        module.exit_json(ansible_facts=dict(openstack=dict(clouds=clouds)))
    except exceptions.ConfigException as e:
        module.fail_json(msg=str(e))


if __name__ == "__main__":
    main()
