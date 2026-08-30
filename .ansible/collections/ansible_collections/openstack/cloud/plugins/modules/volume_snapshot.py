#!/usr/bin/python
# coding: utf-8 -*-

# Copyright (c) 2016, Mario Santos <mario.rf.santos@gmail.com>
# GNU General Public License v3.0+ (see COPYING or https://www.gnu.org/licenses/gpl-3.0.txt)

DOCUMENTATION = '''
---
module: volume_snapshot
short_description: Create/Delete Cinder Volume Snapshots
author: OpenStack Ansible SIG
description:
   - Create or Delete cinder block storage volume snapshots
options:
   display_name:
     description:
        - Name of the snapshot
     required: true
     aliases: ['name']
     type: str
   display_description:
     description:
       - String describing the snapshot
     aliases: ['description']
     type: str
   volume:
     description:
       - The volume name or id to create/delete the snapshot
     required: True
     type: str
   force:
     description:
       - Allows or disallows snapshot of a volume to be created when the volume
         is attached to an instance.
     type: bool
     default: 'no'
   state:
     description:
       - Should the resource be present or absent.
     choices: [present, absent]
     default: present
     type: str
requirements:
    - "python >= 3.6"
    - "openstacksdk"

extends_documentation_fragment:
- openstack.cloud.openstack
'''

EXAMPLES = '''
# Creates a snapshot on volume 'test_volume'
- name: create and delete snapshot
  hosts: localhost
  tasks:
  - name: create snapshot
    openstack.cloud.volume_snapshot:
      state: present
      cloud: mordred
      availability_zone: az2
      display_name: test_snapshot
      volume: test_volume
  - name: delete snapshot
    openstack.cloud.volume_snapshot:
      state: absent
      cloud: mordred
      availability_zone: az2
      display_name: test_snapshot
      volume: test_volume
'''

RETURN = '''
snapshot:
    description: The snapshot instance after the change
    returned: success
    type: dict
    sample:
      id: 837aca54-c0ee-47a2-bf9a-35e1b4fdac0c
      name: test_snapshot
      volume_id: ec646a7c-6a35-4857-b38b-808105a24be6
      size: 2
      status: available
      display_name: test_snapshot
'''

from ansible_collections.openstack.cloud.plugins.module_utils.openstack import OpenStackModule


class VolumeSnapshotModule(OpenStackModule):
    argument_spec = dict(
        display_name=dict(required=True, aliases=['name']),
        display_description=dict(default=None, aliases=['description']),
        volume=dict(required=True),
        force=dict(required=False, default=False, type='bool'),
        state=dict(default='present', choices=['absent', 'present']),
    )

    module_kwargs = dict(
        supports_check_mode=True
    )

    def _present_volume_snapshot(self):
        volume = self.conn.get_volume(self.params['volume'])
        snapshot = self.conn.get_volume_snapshot(
            self.params['display_name'], filters={'volume_id': volume.id})
        if not snapshot:
            snapshot = self.conn.create_volume_snapshot(
                volume.id,
                force=self.params['force'],
                wait=self.params['wait'],
                timeout=self.params['timeout'],
                name=self.params['display_name'],
                description=self.params.get('display_description')
            )
            self.exit_json(changed=True, snapshot=snapshot)
        else:
            self.exit_json(changed=False, snapshot=snapshot)

    def _absent_volume_snapshot(self):
        volume = self.conn.get_volume(self.params['volume'])
        snapshot = self.conn.get_volume_snapshot(
            self.params['display_name'], filters={'volume_id': volume.id})
        if not snapshot:
            self.exit_json(changed=False)
        else:
            self.conn.delete_volume_snapshot(
                name_or_id=snapshot.id,
                wait=self.params['wait'],
                timeout=self.params['timeout'],
            )
            self.exit_json(changed=True, snapshot_id=snapshot.id)

    def _system_state_change(self):
        volume = self.conn.get_volume(self.params['volume'])
        snapshot = self.conn.get_volume_snapshot(
            self.params['display_name'],
            filters={'volume_id': volume.id})
        state = self.params['state']

        if state == 'present':
            return snapshot is None
        if state == 'absent':
            return snapshot is not None

    def run(self):
        state = self.params['state']

        if self.conn.volume_exists(self.params['volume']):
            if self.ansible.check_mode:
                self.exit_json(changed=self._system_state_change())
            if state == 'present':
                self._present_volume_snapshot()
            if state == 'absent':
                self._absent_volume_snapshot()
        else:
            self.fail_json(
                msg="No volume with name or id '{0}' was found.".format(
                    self.params['volume']))


def main():
    module = VolumeSnapshotModule()
    module()


if __name__ == '__main__':
    main()
