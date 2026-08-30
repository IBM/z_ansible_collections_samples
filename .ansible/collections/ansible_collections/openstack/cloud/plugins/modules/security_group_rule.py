#!/usr/bin/python

# Copyright (c) 2015 Hewlett-Packard Development Company, L.P.
# Copyright (c) 2013, Benno Joy <benno@ansible.com>
# GNU General Public License v3.0+ (see COPYING or https://www.gnu.org/licenses/gpl-3.0.txt)

DOCUMENTATION = '''
---
module: security_group_rule
short_description: Add/Delete rule from an existing security group
author: OpenStack Ansible SIG
description:
   - Add or Remove rule from an existing security group
options:
   security_group:
      description:
        - Name or ID of the security group
      required: true
      type: str
   protocol:
      description:
        - IP protocols ANY TCP UDP ICMP and others, also number in range 0-255
      type: str
   port_range_min:
      description:
        - Starting port
      type: int
   port_range_max:
      description:
        - Ending port
      type: int
   remote_ip_prefix:
      description:
        - Source IP address(es) in CIDR notation (exclusive with remote_group)
      type: str
   remote_group:
      description:
        - Name or ID of the Security group to link (exclusive with
          remote_ip_prefix)
      type: str
   ethertype:
      description:
        - Must be IPv4 or IPv6, and addresses represented in CIDR must
          match the ingress or egress rules. Not all providers support IPv6.
      choices: ['IPv4', 'IPv6']
      default: IPv4
      type: str
   direction:
      description:
        - The direction in which the security group rule is applied. Not
          all providers support egress.
      choices: ['egress', 'ingress']
      default: ingress
      type: str
   state:
      description:
        - Should the resource be present or absent.
      choices: [present, absent]
      default: present
      type: str
   project:
     description:
        - Unique name or ID of the project.
     required: false
     type: str
   description:
     required: false
     description:
       - Description of the rule.
     type: str
requirements:
    - "python >= 3.6"
    - "openstacksdk"

extends_documentation_fragment:
- openstack.cloud.openstack
'''

EXAMPLES = '''
# Create a security group rule
- openstack.cloud.security_group_rule:
    cloud: mordred
    security_group: foo
    protocol: tcp
    port_range_min: 80
    port_range_max: 80
    remote_ip_prefix: 0.0.0.0/0

# Create a security group rule for ping
- openstack.cloud.security_group_rule:
    cloud: mordred
    security_group: foo
    protocol: icmp
    remote_ip_prefix: 0.0.0.0/0

# Another way to create the ping rule
- openstack.cloud.security_group_rule:
    cloud: mordred
    security_group: foo
    protocol: icmp
    port_range_min: -1
    port_range_max: -1
    remote_ip_prefix: 0.0.0.0/0

# Create a TCP rule covering all ports
- openstack.cloud.security_group_rule:
    cloud: mordred
    security_group: foo
    protocol: tcp
    port_range_min: 1
    port_range_max: 65535
    remote_ip_prefix: 0.0.0.0/0

# Another way to create the TCP rule above (defaults to all ports)
- openstack.cloud.security_group_rule:
    cloud: mordred
    security_group: foo
    protocol: tcp
    remote_ip_prefix: 0.0.0.0/0

# Create a rule for VRRP with numbered protocol 112
- openstack.cloud.security_group_rule:
    security_group: loadbalancer_sg
    protocol: 112
    remote_group: loadbalancer-node_sg

# Create a security group rule for a given project
- openstack.cloud.security_group_rule:
    cloud: mordred
    security_group: foo
    protocol: icmp
    remote_ip_prefix: 0.0.0.0/0
    project: myproj

# Remove the default created egress rule for IPv4
- openstack.cloud.security_group_rule:
   cloud: mordred
   security_group: foo
   protocol: any
   remote_ip_prefix: 0.0.0.0/0
'''

RETURN = '''
id:
  description: Unique rule UUID.
  type: str
  returned: state == present
direction:
  description: The direction in which the security group rule is applied.
  type: str
  sample: 'egress'
  returned: state == present
ethertype:
  description: One of IPv4 or IPv6.
  type: str
  sample: 'IPv4'
  returned: state == present
port_range_min:
  description: The minimum port number in the range that is matched by
               the security group rule.
  type: int
  sample: 8000
  returned: state == present
port_range_max:
  description: The maximum port number in the range that is matched by
               the security group rule.
  type: int
  sample: 8000
  returned: state == present
protocol:
  description: The protocol that is matched by the security group rule.
  type: str
  sample: 'tcp'
  returned: state == present
remote_ip_prefix:
  description: The remote IP prefix to be associated with this security group rule.
  type: str
  sample: '0.0.0.0/0'
  returned: state == present
security_group_id:
  description: The security group ID to associate with this security group rule.
  type: str
  returned: state == present
'''

from ansible_collections.openstack.cloud.plugins.module_utils.openstack import (
    OpenStackModule)


def _ports_match(protocol, module_min, module_max, rule_min, rule_max):
    """
    Capture the complex port matching logic.

    The port values coming in for the module might be -1 (for ICMP),
    which will work only for Nova, but this is handled by sdk. Likewise,
    they might be None, which works for Neutron, but not Nova. This too is
    handled by sdk. Since sdk will consistently return these port
    values as None, we need to convert any -1 values input to the module
    to None here for comparison.

    For TCP and UDP protocols, None values for both min and max are
    represented as the range 1-65535 for Nova, but remain None for
    Neutron. sdk returns the full range when Nova is the backend (since
    that is how Nova stores them), and None values for Neutron. If None
    values are input to the module for both values, then we need to adjust
    for comparison.
    """

    # Check if the user is supplying -1 for ICMP.
    if protocol in ['icmp', 'ipv6-icmp']:
        if module_min and int(module_min) == -1:
            module_min = None
        if module_max and int(module_max) == -1:
            module_max = None

    # Rules with 'any' protocol do not match ports
    if protocol == 'any':
        return True

    # Check if the user is supplying -1, 1 to 65535 or None values for full TPC/UDP port range.
    if protocol in ['tcp', 'udp'] or protocol is None:
        if (
            not module_min and not module_max
            or (int(module_min) in [-1, 1]
                and int(module_max) in [-1, 65535])
        ):
            if (
                not rule_min and not rule_max
                or (int(rule_min) in [-1, 1]
                    and int(rule_max) in [-1, 65535])
            ):
                # (None, None) == (1, 65535) == (-1, -1)
                return True

    # Sanity check to make sure we don't have type comparison issues.
    if module_min:
        module_min = int(module_min)
    if module_max:
        module_max = int(module_max)
    if rule_min:
        rule_min = int(rule_min)
    if rule_max:
        rule_max = int(rule_max)

    return module_min == rule_min and module_max == rule_max


class SecurityGroupRuleModule(OpenStackModule):
    deprecated_names = ('os_security_group_rule', 'openstack.cloud.os_security_group_rule')

    argument_spec = dict(
        security_group=dict(required=True),
        protocol=dict(type='str'),
        port_range_min=dict(required=False, type='int'),
        port_range_max=dict(required=False, type='int'),
        remote_ip_prefix=dict(required=False),
        remote_group=dict(required=False),
        ethertype=dict(default='IPv4',
                       choices=['IPv4', 'IPv6']),
        direction=dict(default='ingress',
                       choices=['egress', 'ingress']),
        state=dict(default='present',
                   choices=['absent', 'present']),
        description=dict(required=False, default=None),
        project=dict(default=None),
    )

    module_kwargs = dict(
        mutually_exclusive=[
            ['remote_ip_prefix', 'remote_group'],
        ]
    )

    def _find_matching_rule(self, secgroup, remotegroup):
        """
        Find a rule in the group that matches the module parameters.
        :returns: The matching rule dict, or None if no matches.
        """
        protocol = self.params['protocol']
        remote_ip_prefix = self.params['remote_ip_prefix']
        ethertype = self.params['ethertype']
        direction = self.params['direction']
        remote_group_id = remotegroup['id']

        for rule in secgroup['security_group_rules']:
            if (
                protocol == rule['protocol']
                and remote_ip_prefix == rule['remote_ip_prefix']
                and ethertype == rule['ethertype']
                and direction == rule['direction']
                and remote_group_id == rule['remote_group_id']
                and _ports_match(
                    protocol,
                    self.params['port_range_min'],
                    self.params['port_range_max'],
                    rule['port_range_min'],
                    rule['port_range_max'])
            ):
                return rule
        return None

    def _system_state_change(self, secgroup, remotegroup):
        state = self.params['state']
        if secgroup:
            rule_exists = self._find_matching_rule(secgroup, remotegroup)
        else:
            return False

        if state == 'present' and not rule_exists:
            return True
        if state == 'absent' and rule_exists:
            return True
        return False

    def run(self):

        state = self.params['state']
        security_group = self.params['security_group']
        remote_group = self.params['remote_group']
        project = self.params['project']
        changed = False

        if project is not None:
            proj = self.conn.get_project(project)
            if proj is None:
                self.fail_json(msg='Project %s could not be found' % project)
            project_id = proj['id']
        else:
            project_id = self.conn.current_project_id

        if project_id and not remote_group:
            filters = {'tenant_id': project_id}
        else:
            filters = None

        secgroup = self.conn.get_security_group(security_group, filters=filters)

        if remote_group:
            remotegroup = self.conn.get_security_group(remote_group, filters=filters)
        else:
            remotegroup = {'id': None}

        if self.ansible.check_mode:
            self.exit_json(changed=self._system_state_change(secgroup, remotegroup))

        if state == 'present':
            if self.params['protocol'] == 'any':
                self.params['protocol'] = None

            if not secgroup:
                self.fail_json(msg='Could not find security group %s' % security_group)

            rule = self._find_matching_rule(secgroup, remotegroup)
            if not rule:
                kwargs = {}
                if project_id:
                    kwargs['project_id'] = project_id
                if self.params["description"] is not None:
                    kwargs["description"] = self.params['description']
                rule = self.conn.network.create_security_group_rule(
                    security_group_id=secgroup['id'],
                    port_range_min=None if self.params['port_range_min'] == -1 else self.params['port_range_min'],
                    port_range_max=None if self.params['port_range_max'] == -1 else self.params['port_range_max'],
                    protocol=self.params['protocol'],
                    remote_ip_prefix=self.params['remote_ip_prefix'],
                    remote_group_id=remotegroup['id'],
                    direction=self.params['direction'],
                    ethertype=self.params['ethertype'],
                    **kwargs
                )
                changed = True
            self.exit_json(changed=changed, rule=rule, id=rule['id'])

        if state == 'absent' and secgroup:
            rule = self._find_matching_rule(secgroup, remotegroup)
            if rule:
                self.conn.delete_security_group_rule(rule['id'])
                changed = True

        self.exit_json(changed=changed)


def main():
    module = SecurityGroupRuleModule()
    module()


if __name__ == '__main__':
    main()
