#!/usr/bin/python
# coding: utf-8 -*-

# Copyright (c) 2015, Jesse Keating <jlk@derpops.bike>
# GNU General Public License v3.0+ (see COPYING or https://www.gnu.org/licenses/gpl-3.0.txt)

DOCUMENTATION = '''
---
module: server_action
short_description: Perform actions on Compute Instances from OpenStack
author: OpenStack Ansible SIG
description:
    - Perform server actions on an existing compute instance from OpenStack.
        This module does not return any data other than changed true/false.
        When I(action) is 'rebuild', then I(image) parameter is required.
options:
    server:
        description:
        - Name or ID of the instance
        required: true
        type: str
    wait:
        description:
        - If the module should wait for the instance action to be performed.
        type: bool
        default: 'yes'
    timeout:
        description:
        - The amount of time the module should wait for the instance to perform
            the requested action.
        default: 180
        type: int
    action:
        description:
        - Perform the given action. The lock and unlock actions always return
            changed as the servers API does not provide lock status.
        choices: [stop, start, pause, unpause, lock, unlock, suspend, resume,
                rebuild, shelve, shelve_offload, unshelve]
        type: str
        required: true
    image:
        description:
        - Image the server should be rebuilt with
        type: str
    admin_password:
        description:
        - Admin password for server to rebuild
        type: str
    all_projects:
        description:
        - Whether to search for server in all projects or just the current
          auth scoped project.
        type: bool
        default: 'no'

requirements:
    - "python >= 3.6"
    - "openstacksdk"

extends_documentation_fragment:
- openstack.cloud.openstack
'''

EXAMPLES = '''
# Pauses a compute instance
- openstack.cloud.server_action:
      action: pause
      auth:
        auth_url: https://identity.example.com
        username: admin
        password: admin
        project_name: admin
      server: vm1
      timeout: 200
'''

from ansible_collections.openstack.cloud.plugins.module_utils.openstack import OpenStackModule

# If I(action) is set to C(shelve) then according to OpenStack's Compute API, the shelved
# server is in one of two possible states:
#
#  SHELVED:           The server is in shelved state. Depends on the shelve offload time,
#                     the server will be automatically shelved off loaded.
#  SHELVED_OFFLOADED: The shelved server is offloaded (removed from the compute host) and
#                     it needs unshelved action to be used again.
#
# But wait_for_server can only wait for a single server state. If a shelved server is offloaded
# immediately, then a exceptions.ResourceTimeout will be raised if I(action) is set to C(shelve).
# This is likely to happen because shelved_offload_time in Nova's config is set to 0 by default.
# This also applies if you boot the server from volumes.
#
# Calling C(shelve_offload) instead of C(shelve) will also fail most likely because the default
# policy does not allow C(shelve_offload) for non-admin users while C(shelve) is allowed for
# admin users and server owners.
#
# As we cannot retrieve shelved_offload_time from Nova's config, we fall back to waiting for
# one state and if that fails then we fetch the server's state and match it against the other
# valid states from _action_map.
#
# Ref.: https://docs.openstack.org/api-guide/compute/server_concepts.html

_action_map = {'stop': ['SHUTOFF'],
               'start': ['ACTIVE'],
               'pause': ['PAUSED'],
               'unpause': ['ACTIVE'],
               'lock': ['ACTIVE'],  # API doesn't show lock/unlock status
               'unlock': ['ACTIVE'],
               'suspend': ['SUSPENDED'],
               'resume': ['ACTIVE'],
               'rebuild': ['ACTIVE'],
               'shelve': ['SHELVED_OFFLOADED', 'SHELVED'],
               'shelve_offload': ['SHELVED_OFFLOADED'],
               'unshelve': ['ACTIVE']}

_admin_actions = ['pause', 'unpause', 'suspend', 'resume', 'lock', 'unlock', 'shelve_offload']


class ServerActionModule(OpenStackModule):
    deprecated_names = ('os_server_action', 'openstack.cloud.os_server_action')

    argument_spec = dict(
        server=dict(required=True, type='str'),
        action=dict(required=True, type='str',
                    choices=['stop', 'start', 'pause', 'unpause',
                             'lock', 'unlock', 'suspend', 'resume',
                             'rebuild', 'shelve', 'shelve_offload', 'unshelve']),
        image=dict(required=False, type='str'),
        admin_password=dict(required=False, type='str', no_log=True),
        all_projects=dict(required=False, type='bool', default=False),
    )
    module_kwargs = dict(
        required_if=[('action', 'rebuild', ['image'])],
        supports_check_mode=True,
    )

    def run(self):
        os_server = self._preliminary_checks()
        self._execute_server_action(os_server)
        # for some reason we don't wait for lock and unlock before exit
        if self.params['action'] not in ('lock', 'unlock'):
            if self.params['wait']:
                self._wait(os_server)
        self.exit_json(changed=True)

    def _preliminary_checks(self):
        # Using Munch object for getting information about a server
        os_server = self.conn.get_server(
            self.params['server'],
            all_projects=self.params['all_projects'],
        )
        if not os_server:
            self.fail_json(msg='Could not find server %s' % self.params['server'])
        # check mode
        if self.ansible.check_mode:
            self.exit_json(changed=self.__system_state_change(os_server))
        # examine special cases
        # lock, unlock and rebuild don't depend on state, just do it
        if self.params['action'] not in ('lock', 'unlock', 'rebuild'):
            if not self.__system_state_change(os_server):
                self.exit_json(changed=False)
        return os_server

    def _execute_server_action(self, os_server):
        if self.params['action'] == 'rebuild':
            return self._rebuild_server(os_server)
        if self.params['action'] == 'shelve_offload':
            # shelve_offload is not supported in OpenstackSDK
            return self._action(os_server, json={'shelveOffload': None})
        action_name = self.params['action'] + "_server"
        try:
            func_name = getattr(self.conn.compute, action_name)
        except AttributeError:
            self.fail_json(
                msg="Method %s wasn't found in OpenstackSDK compute" % action_name)
        func_name(os_server)

    def _rebuild_server(self, os_server):
        # rebuild should ensure images exists
        try:
            image = self.conn.get_image(self.params['image'])
        except Exception as e:
            self.fail_json(
                msg="Can't find the image %s: %s" % (self.params['image'], e))
        if not image:
            self.fail_json(msg="Image %s was not found!" % self.params['image'])
        # admin_password is required by SDK, but not required by Nova API
        if self.params['admin_password']:
            self.conn.compute.rebuild_server(
                server=os_server,
                name=os_server['name'],
                image=image['id'],
                admin_password=self.params['admin_password']
            )
        else:
            self._action(os_server, json={'rebuild': {'imageRef': image['id']}})

    def _action(self, os_server, json):
        response = self.conn.compute.post(
            '/servers/{server_id}/action'.format(server_id=os_server['id']),
            json=json)
        self.sdk.exceptions.raise_from_response(response)
        return response

    def _wait(self, os_server):
        """Wait for the server to reach the desired state for the given action."""
        # The wait_for_server function needs a Server object instead of the
        # Munch object returned by self.conn.get_server
        server = self.conn.compute.get_server(os_server['id'])
        states = _action_map[self.params['action']]

        try:
            self.conn.compute.wait_for_server(
                server,
                status=states[0],
                wait=self.params['timeout'])
        except self.sdk.exceptions.ResourceTimeout:
            # raise if there is only one valid state
            if len(states) < 2:
                raise
            # fetch current server status and compare to other valid states
            server = self.conn.compute.get_server(os_server['id'])
            if server.status not in states:
                raise

    def __system_state_change(self, os_server):
        """Check if system state would change."""
        return os_server.status not in _action_map[self.params['action']]


def main():
    module = ServerActionModule()
    module()


if __name__ == '__main__':
    main()
