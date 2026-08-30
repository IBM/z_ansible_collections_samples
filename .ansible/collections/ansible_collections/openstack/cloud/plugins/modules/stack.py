#!/usr/bin/python
# coding: utf-8 -*-

# (c) 2016, Mathieu Bultel <mbultel@redhat.com>
# (c) 2016, Steve Baker <sbaker@redhat.com>
# GNU General Public License v3.0+ (see COPYING or https://www.gnu.org/licenses/gpl-3.0.txt)

DOCUMENTATION = '''
---
module: stack
short_description: Add/Remove Heat Stack
author: OpenStack Ansible SIG
description:
   - Add or Remove a Stack to an OpenStack Heat
options:
    state:
      description:
        - Indicate desired state of the resource
      choices: ['present', 'absent']
      default: present
      type: str
    name:
      description:
        - Name of the stack that should be created, name could be char and digit, no space
      required: true
      type: str
    tag:
      description:
        - Tag for the stack that should be created, name could be char and digit, no space
      type: str
    template:
      description:
        - Path of the template file to use for the stack creation
      type: str
    environment:
      description:
        - List of environment files that should be used for the stack creation
      type: list
      elements: str
    parameters:
      description:
        - Dictionary of parameters for the stack creation
      type: dict
    rollback:
      description:
        - Rollback stack creation
      type: bool
      default: false
    timeout:
      description:
        - Maximum number of seconds to wait for the stack creation
      default: 3600
      type: int
requirements:
    - "python >= 3.6"
    - "openstacksdk"

extends_documentation_fragment:
- openstack.cloud.openstack
'''
EXAMPLES = '''
---
- name: create stack
  ignore_errors: True
  register: stack_create
  openstack.cloud.stack:
    name: "{{ stack_name }}"
    tag: "{{ tag_name }}"
    state: present
    template: "/path/to/my_stack.yaml"
    environment:
    - /path/to/resource-registry.yaml
    - /path/to/environment.yaml
    parameters:
        bmc_flavor: m1.medium
        bmc_image: CentOS
        key_name: default
        private_net: "{{ private_net_param }}"
        node_count: 2
        name: undercloud
        image: CentOS
        my_flavor: m1.large
        external_net: "{{ external_net_param }}"
'''

RETURN = '''
id:
    description: Stack ID.
    type: str
    sample: "97a3f543-8136-4570-920e-fd7605c989d6"
    returned: always

stack:
    description: stack info
    type: complex
    returned: always
    contains:
        action:
            description: Action, could be Create or Update.
            type: str
            sample: "CREATE"
        creation_time:
            description: Time when the action has been made.
            type: str
            sample: "2016-07-05T17:38:12Z"
        description:
            description: Description of the Stack provided in the heat template.
            type: str
            sample: "HOT template to create a new instance and networks"
        id:
            description: Stack ID.
            type: str
            sample: "97a3f543-8136-4570-920e-fd7605c989d6"
        name:
            description: Name of the Stack
            type: str
            sample: "test-stack"
        identifier:
            description: Identifier of the current Stack action.
            type: str
            sample: "test-stack/97a3f543-8136-4570-920e-fd7605c989d6"
        links:
            description: Links to the current Stack.
            type: list
            elements: dict
            sample: "[{'href': 'http://foo:8004/v1/7f6a/stacks/test-stack/97a3f543-8136-4570-920e-fd7605c989d6']"
        outputs:
            description: Output returned by the Stack.
            type: list
            elements: dict
            sample: "{'description': 'IP address of server1 in private network',
                        'output_key': 'server1_private_ip',
                        'output_value': '10.1.10.103'}"
        parameters:
            description: Parameters of the current Stack
            type: dict
            sample: "{'OS::project_id': '7f6a3a3e01164a4eb4eecb2ab7742101',
                        'OS::stack_id': '97a3f543-8136-4570-920e-fd7605c989d6',
                        'OS::stack_name': 'test-stack',
                        'stack_status': 'CREATE_COMPLETE',
                        'stack_status_reason': 'Stack CREATE completed successfully',
                        'status': 'COMPLETE',
                        'template_description': 'HOT template to create a new instance and networks',
                        'timeout_mins': 60,
                        'updated_time': null}"
'''


from ansible_collections.openstack.cloud.plugins.module_utils.openstack import OpenStackModule


class StackModule(OpenStackModule):
    argument_spec = dict(
        name=dict(required=True),
        tag=dict(required=False, default=None, min_ver='0.28.0'),
        template=dict(default=None),
        environment=dict(default=None, type='list', elements='str'),
        parameters=dict(default={}, type='dict'),
        rollback=dict(default=False, type='bool'),
        timeout=dict(default=3600, type='int'),
        state=dict(default='present', choices=['absent', 'present']),
    )

    module_kwargs = dict(
        supports_check_mode=True
    )

    def _create_stack(self, stack, parameters):
        stack = self.conn.create_stack(
            self.params['name'],
            template_file=self.params['template'],
            environment_files=self.params['environment'],
            timeout=self.params['timeout'],
            wait=True,
            rollback=self.params['rollback'],
            **parameters)

        stack = self.conn.get_stack(stack.id, None)
        if stack.stack_status == 'CREATE_COMPLETE':
            return stack
        else:
            self.fail_json(msg="Failure in creating stack: {0}".format(stack))

    def _update_stack(self, stack, parameters):
        stack = self.conn.update_stack(
            self.params['name'],
            template_file=self.params['template'],
            environment_files=self.params['environment'],
            timeout=self.params['timeout'],
            rollback=self.params['rollback'],
            wait=self.params['wait'],
            **parameters)

        if stack['stack_status'] == 'UPDATE_COMPLETE':
            return stack
        else:
            self.fail_json(msg="Failure in updating stack: %s" %
                           stack['stack_status_reason'])

    def _system_state_change(self, stack):
        state = self.params['state']
        if state == 'present':
            if not stack:
                return True
        if state == 'absent' and stack:
            return True
        return False

    def run(self):
        state = self.params['state']
        name = self.params['name']
        # Check for required parameters when state == 'present'
        if state == 'present':
            for p in ['template']:
                if not self.params[p]:
                    self.fail_json(msg='%s required with present state' % p)

        stack = self.conn.get_stack(name)

        if self.ansible.check_mode:
            self.exit_json(changed=self._system_state_change(stack))

        if state == 'present':
            parameters = self.params['parameters']
            if not stack:
                stack = self._create_stack(stack, parameters)
            else:
                stack = self._update_stack(stack, parameters)
            self.exit_json(changed=True,
                           stack=stack,
                           id=stack.id)
        elif state == 'absent':
            if not stack:
                changed = False
            else:
                changed = True
                if not self.conn.delete_stack(name, wait=self.params['wait']):
                    self.fail_json(msg='delete stack failed for stack: %s' % name)
            self.exit_json(changed=changed)


def main():
    module = StackModule()
    module()


if __name__ == '__main__':
    main()
