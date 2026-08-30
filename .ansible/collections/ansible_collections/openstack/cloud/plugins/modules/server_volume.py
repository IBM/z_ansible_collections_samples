#!/usr/bin/python
# coding: utf-8 -*-

# Copyright (c) 2014 Hewlett-Packard Development Company, L.P.
# GNU General Public License v3.0+ (see COPYING or https://www.gnu.org/licenses/gpl-3.0.txt)

DOCUMENTATION = '''
---
module: server_volume
short_description: Attach/Detach Volumes from OpenStack VM's
author: OpenStack Ansible SIG
description:
   - Attach or Detach volumes from OpenStack VM's
options:
   state:
     description:
       - Should the resource be present or absent.
     choices: [present, absent]
     default: present
     required: false
     type: str
   server:
     description:
       - Name or ID of server you want to attach a volume to
     required: true
     type: str
   volume:
     description:
      - Name or id of volume you want to attach to a server
     required: true
     type: str
   device:
     description:
      - Device you want to attach. Defaults to auto finding a device name.
     type: str
requirements:
    - "python >= 3.6"
    - "openstacksdk"

extends_documentation_fragment:
- openstack.cloud.openstack
'''

EXAMPLES = '''
# Attaches a volume to a compute host
- name: attach a volume
  hosts: localhost
  tasks:
  - name: attach volume to host
    openstack.cloud.server_volume:
      state: present
      cloud: mordred
      server: Mysql-server
      volume: mysql-data
      device: /dev/vdb
'''

from ansible_collections.openstack.cloud.plugins.module_utils.openstack import OpenStackModule


def _system_state_change(state, device):
    """Check if system state would change."""
    if state == 'present':
        if device:
            return False
        return True
    if state == 'absent':
        if device:
            return True
        return False
    return False


class ServerVolumeModule(OpenStackModule):

    argument_spec = dict(
        server=dict(required=True),
        volume=dict(required=True),
        device=dict(default=None),  # None == auto choose device name
        state=dict(default='present', choices=['absent', 'present']),
    )

    def run(self):

        state = self.params['state']
        wait = self.params['wait']
        timeout = self.params['timeout']

        server = self.conn.get_server(self.params['server'])
        volume = self.conn.get_volume(self.params['volume'])

        if not server:
            self.fail(msg='server %s is not found' % self.params['server'])

        if not volume:
            self.fail(msg='volume %s is not found' % self.params['volume'])

        dev = self.conn.get_volume_attach_device(volume, server.id)

        if self.ansible.check_mode:
            self.exit(changed=_system_state_change(state, dev))

        if state == 'present':
            changed = False
            if not dev:
                changed = True
                self.conn.attach_volume(server, volume, self.params['device'],
                                        wait=wait, timeout=timeout)

            server = self.conn.get_server(self.params['server'])  # refresh
            volume = self.conn.get_volume(self.params['volume'])  # refresh
            hostvars = self.conn.get_openstack_vars(server)

            self.exit(
                changed=changed,
                id=volume['id'],
                attachments=volume['attachments'],
                openstack=hostvars
            )

        elif state == 'absent':
            if not dev:
                # Volume is not attached to this server
                self.exit(changed=False)

            self.conn.detach_volume(server, volume, wait=wait, timeout=timeout)
            self.exit(
                changed=True,
                result='Detached volume from server'
            )


def main():
    module = ServerVolumeModule()
    module()


if __name__ == '__main__':
    main()
