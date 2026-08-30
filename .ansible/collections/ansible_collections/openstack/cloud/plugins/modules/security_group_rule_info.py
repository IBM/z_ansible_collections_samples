#!/usr/bin/python
# coding: utf-8 -*-
#
# Copyright (c) 2020 by Tino Schreiber (Open Telekom Cloud), operated by T-Systems International GmbH
# GNU General Public License v3.0+ (see COPYING or https://www.gnu.org/licenses/gpl-3.0.txt)

DOCUMENTATION = '''
---
module: security_group_rule_info
short_description: Querying security group rules
author: OpenStack Ansible SIG
description:
   - Querying security group rules
options:
  description:
    description:
      - Filter the list result by the human-readable description of
        the resource.
    type: str
  direction:
    description:
      - Filter the security group rule list result by the direction in
        which the security group rule is applied.
    choices: ['egress', 'ingress']
    type: str
  ethertype:
    description:
      - Filter the security group rule list result by the ethertype of
        network traffic. The value must be IPv4 or IPv6.
    choices: ['IPv4', 'IPv6']
    type: str
  port_range_min:
    description:
      - Starting port
    type: int
  port_range_max:
    description:
      - Ending port
    type: int
  project:
    description:
      - Unique name or ID of the project.
    required: false
    type: str
  protocol:
    description:
      - Filter the security group rule list result by the IP protocol.
    type: str
    choices: ['any', 'tcp', 'udp', 'icmp', '112', '132']
  remote_group:
    description:
      - Filter the security group rule list result by the name or ID of the
        remote group that associates with this security group rule.
    type: str
  remote_ip_prefix:
    description:
      - Source IP address(es) in CIDR notation (exclusive with remote_group)
    type: str
  revision_number:
    description:
      - Filter the list result by the revision number of the resource.
    type: int
  rule:
    description:
      - Filter the list result by the ID of the security group rule.
    type: str
  security_group:
    description:
      - Name or ID of the security group
    type: str

requirements:
  - "python >= 3.6"
  - "openstacksdk"

extends_documentation_fragment:
  - openstack.cloud.openstack
'''

EXAMPLES = '''
# Get all security group rules
- openstack.cloud.security_group_rule_info:
    cloud: "{{ cloud }}"
  register: sg

# Filter security group rules for port 80 and name
- openstack.cloud.security_group_rule_info:
    cloud: "{{ cloud }}"
    security_group: "{{ rule_name }}"
    protocol: tcp
    port_range_min: 80
    port_range_max: 80
    remote_ip_prefix: 0.0.0.0/0

# Filter for ICMP rules
- openstack.cloud.security_group_rule_info:
    cloud: "{{ cloud }}"
    protocol: icmp
'''

RETURN = '''
security_group_rules:
  description: List of dictionaries describing security group rules.
  type: complex
  returned: On Success.
  contains:
    id:
      description: Unique rule UUID.
      type: str
    description:
      description: Human-readable description of the resource.
      type: str
      sample: 'My description.'
    direction:
      description: The direction in which the security group rule is applied.
      type: str
      sample: 'egress'
    ethertype:
      description: One of IPv4 or IPv6.
      type: str
      sample: 'IPv4'
    port_range_min:
      description: The minimum port number in the range that is matched by
                   the security group rule.
      type: int
      sample: 8000
    port_range_max:
      description: The maximum port number in the range that is matched by
                  the security group rule.
      type: int
      sample: 8000
    project:
      description:
        - Unique ID of the project.
      type: str
      sample: '16d53a84a13b49529d2e2c3646691123'
    protocol:
      description: The protocol that is matched by the security group rule.
      type: str
      sample: 'tcp'
    remote_ip_prefix:
      description: The remote IP prefix to be associated with this security group rule.
      type: str
      sample: '0.0.0.0/0'
    security_group_id:
      description: The security group ID to associate with this security group rule.
      type: str
      sample: '729b9660-a20a-41fe-bae6-ed8fa7f69123'
'''

from ansible_collections.openstack.cloud.plugins.module_utils.openstack import (
    OpenStackModule)


class SecurityGroupRuleInfoModule(OpenStackModule):
    argument_spec = dict(
        description=dict(required=False, type='str'),
        direction=dict(required=False,
                       type='str',
                       choices=['egress', 'ingress']),
        ethertype=dict(required=False,
                       type='str',
                       choices=['IPv4', 'IPv6']),
        port_range_min=dict(required=False, type='int', min_ver="0.32.0"),
        port_range_max=dict(required=False, type='int', min_ver="0.32.0"),
        project=dict(required=False, type='str'),
        protocol=dict(required=False,
                      type='str',
                      choices=['any', 'tcp', 'udp', 'icmp', '112', '132']),
        remote_group=dict(required=False, type='str'),
        remote_ip_prefix=dict(required=False, type='str', min_ver="0.32.0"),
        revision_number=dict(required=False, type='int'),
        rule=dict(required=False, type='str'),
        security_group=dict(required=False, type='str')
    )

    module_kwargs = dict(
        mutually_exclusive=[
            ['remote_ip_prefix', 'remote_group'],
        ],
        supports_check_mode=True
    )

    def run(self):
        description = self.params['description']
        direction = self.params['direction']
        ethertype = self.params['ethertype']
        project = self.params['project']
        protocol = self.params['protocol']
        remote_group = self.params['remote_group']
        revision_number = self.params['revision_number']
        rule = self.params['rule']
        security_group = self.params['security_group']

        changed = False
        filters = self.check_versioned(
            port_range_min=self.params['port_range_min'],
            port_range_max=self.params['port_range_max'],
            remote_ip_prefix=self.params['remote_ip_prefix']
        )
        data = []

        if rule:
            sec_rule = self.conn.network.get_security_group_rule(rule)
            if sec_rule is None:
                self.exit(changed=changed, security_group_rules=[])
            self.exit(changed=changed,
                      security_group_rules=sec_rule.to_dict())
            # query parameter id is currently not supported
            # PR is open for that.
            # filters['id] = sec_rule.id
        if description:
            filters['description'] = description
        if direction:
            filters['direction'] = direction
        if ethertype:
            filters['ethertype'] = ethertype
        if project:
            proj = self.conn.get_project(project)
            if proj is None:
                self.fail_json(msg='Project %s could not be found' % project)
            filters['project_id'] = proj.id
        if protocol:
            filters['protocol'] = protocol
        if remote_group:
            filters['remote_group_id'] = remote_group
        if revision_number:
            filters['revision_number'] = revision_number
        if security_group:
            sec_grp = self.conn.network.find_security_group(
                name_or_id=security_group,
                ignore_missing=True)
            if sec_grp is None:
                self.fail_json(msg='Security group %s could not be found' % sec_grp)
            filters['security_group_id'] = sec_grp.id

        for item in self.conn.network.security_group_rules(**filters):
            item = item.to_dict()
            data.append(item)

        self.exit_json(changed=changed,
                       security_group_rules=data)


def main():
    module = SecurityGroupRuleInfoModule()
    module()


if __name__ == '__main__':
    main()
