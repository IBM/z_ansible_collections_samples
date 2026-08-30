#!/usr/bin/python
# coding: utf-8 -*-
#
# Copyright (c) 2020 by Open Telekom Cloud, operated by T-Systems International GmbH
# GNU General Public License v3.0+ (see COPYING or https://www.gnu.org/licenses/gpl-3.0.txt)


DOCUMENTATION = '''
---
module: volume_backup
short_description: Add/Delete Volume backup
extends_documentation_fragment: openstack.cloud.openstack
author: OpenStack Ansible SIG
description:
  - Add or Remove Volume Backup in OTC.
options:
  display_name:
    description:
      - Name that has to be given to the backup
    required: true
    type: str
    aliases: ['name']
  display_description:
    description:
      - String describing the backup
    required: false
    type: str
    aliases: ['description']
  state:
    description:
      - Should the resource be present or absent.
    choices: [present, absent]
    default: present
    type: str
  volume:
    description:
      - Name or ID of the volume. Required when state is True.
    type: str
    required: False
  snapshot:
    description: Name or ID of the Snapshot to take backup of
    type: str
  force:
    description:
      - Indicates whether to backup, even if the volume is attached.
    type: bool
    default: False
  metadata:
    description: Metadata for the backup
    type: dict
  incremental:
    description: The backup mode
    type: bool
    default: False
requirements: ["openstacksdk"]
'''

RETURN = '''
id:
    description: The Volume backup ID.
    returned: On success when C(state=present)
    type: str
    sample: "39007a7e-ee4f-4d13-8283-b4da2e037c69"
backup:
    description: Dictionary describing the Cluster.
    returned: On success when C(state=present)
    type: complex
    contains:
        id:
            description: Unique UUID.
            type: str
            sample: "39007a7e-ee4f-4d13-8283-b4da2e037c69"
        name:
            description: Name given to the load balancer.
            type: str
            sample: "elb_test"
'''

EXAMPLES = '''
- name: Create backup
  openstack.cloud.volume_backup:
    display_name: test_volume_backup
    volume: "test_volume"

- name: Create backup from snapshot
  openstack.cloud.volume_backup:
    display_name: test_volume_backup
    volume: "test_volume"
    snapshot: "test_snapshot"

- name: Delete volume backup
  openstack.cloud.volume_backup:
    display_name: test_volume_backup
    state: absent
'''

from ansible_collections.openstack.cloud.plugins.module_utils.openstack import OpenStackModule


class VolumeBackupModule(OpenStackModule):
    module_min_sdk_version = '0.49.0'

    argument_spec = dict(
        display_name=dict(required=True, aliases=['name'], type='str'),
        display_description=dict(required=False, aliases=['description'],
                                 type='str'),
        volume=dict(required=False, type='str'),
        snapshot=dict(required=False, type='str'),
        state=dict(default='present', type='str', choices=['absent', 'present']),
        force=dict(default=False, type='bool'),
        metadata=dict(required=False, type='dict'),
        incremental=dict(required=False, default=False, type='bool')
    )
    module_kwargs = dict(
        required_if=[
            ('state', 'present', ['volume'])
        ],
        supports_check_mode=True
    )

    def _create_backup(self):
        if self.ansible.check_mode:
            self.exit_json(changed=True)

        name = self.params['display_name']
        description = self.params['display_description']
        volume = self.params['volume']
        snapshot = self.params['snapshot']
        force = self.params['force']
        is_incremental = self.params['incremental']
        metadata = self.params['metadata']

        changed = False

        cloud_volume = self.conn.block_storage.find_volume(volume)
        cloud_snapshot_id = None

        attrs = {
            'name': name,
            'volume_id': cloud_volume.id,
            'force': force,
            'is_incremental': is_incremental
        }

        if snapshot:
            cloud_snapshot_id = self.conn.block_storage.find_snapshot(
                snapshot, ignore_missing=False).id
            attrs['snapshot_id'] = cloud_snapshot_id

        if metadata:
            attrs['metadata'] = metadata

        if description:
            attrs['description'] = description

        backup = self.conn.block_storage.create_backup(**attrs)
        changed = True

        if self.params['wait']:
            try:
                backup = self.conn.block_storage.wait_for_status(
                    backup,
                    status='available',
                    wait=self.params['timeout'])
                self.exit_json(
                    changed=True, volume_backup=backup.to_dict(), id=backup.id
                )
            except self.sdk.exceptions.ResourceTimeout:
                self.fail_json(
                    msg='Timeout failure waiting for backup '
                        'to complete'
                )

        self.exit_json(
            changed=changed, volume_backup=backup.to_dict(), id=backup.id
        )

    def _delete_backup(self, backup):
        if self.ansible.check_mode:
            self.exit_json(changed=True)

        if backup:
            self.conn.block_storage.delete_backup(backup)
            if self.params['wait']:
                try:
                    self.conn.block_storage.wait_for_delete(
                        backup,
                        interval=2,
                        wait=self.params['timeout'])
                except self.sdk.exceptions.ResourceTimeout:
                    self.fail_json(
                        msg='Timeout failure waiting for backup '
                            'to be deleted'
                    )

        self.exit_json(changed=True)

    def run(self):
        name = self.params['display_name']

        backup = self.conn.block_storage.find_backup(name)

        if self.params['state'] == 'present':
            if not backup:
                self._create_backup()
            else:
                # For the moment we do not support backup update, since SDK
                # doesn't support it either => do nothing
                self.exit_json(changed=False)

        elif self.params['state'] == 'absent':
            self._delete_backup(backup)


def main():
    module = VolumeBackupModule()
    module()


if __name__ == '__main__':
    main()
