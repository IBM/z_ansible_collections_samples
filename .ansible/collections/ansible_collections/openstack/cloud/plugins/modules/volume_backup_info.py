#!/usr/bin/python
# coding: utf-8 -*-
#
# Copyright (c) 2020 by Open Telekom Cloud, operated by T-Systems International GmbH
# GNU General Public License v3.0+ (see COPYING or https://www.gnu.org/licenses/gpl-3.0.txt)


DOCUMENTATION = '''
---
module: volume_backup_info
short_description: Get Backups
author: OpenStack Ansible SIG
description:
  - Get Backup info from the Openstack cloud.
options:
  name:
    description:
      - Name of the Backup.
    type: str
  volume:
    description:
      - Name of the volume.
    type: str
requirements: ["openstacksdk"]
extends_documentation_fragment:
- openstack.cloud.openstack
'''

RETURN = '''
volume_backups:
    description: List of dictionaries describing volume backups.
    type: list
    elements: dict
    returned: always.
    contains:
        availability_zone:
            description: Backup availability zone.
            type: str
        created_at:
            description: Backup creation time.
            type: str
        description:
            description: Backup desciption.
            type: str
        id:
            description: Unique UUID.
            type: str
            sample: "39007a7e-ee4f-4d13-8283-b4da2e037c69"
        is_incremental:
            description: Backup incremental property.
            type: bool
        metadata:
            description: Backup metadata.
            type: dict
        name:
            description: Backup Name.
            type: str
        snapshot_id:
            description: Snapshot ID.
            type: str
        status:
            description: Backup status.
            type: str
        updated_at:
            description: Backup update time.
            type: str
        volume_id:
            description: Volume ID.
            type: str

'''

EXAMPLES = '''
# Get backups.
- openstack.cloud.volume_backup_info:
  register: backup

- openstack.cloud.volume_backup_info:
    name: my_fake_backup
  register: backup
'''

from ansible_collections.openstack.cloud.plugins.module_utils.openstack import OpenStackModule


class VolumeBackupInfoModule(OpenStackModule):
    module_min_sdk_version = '0.49.0'

    argument_spec = dict(
        name=dict(required=False, type='str'),
        volume=dict(required=False, type='str')
    )
    module_kwargs = dict(
        supports_check_mode=True
    )

    def run(self):
        name_filter = self.params['name']
        volume = self.params['volume']

        data = []
        attrs = {}

        if name_filter:
            attrs['name'] = name_filter
        if volume:
            attrs['volume_id'] = self.conn.block_storage.find_volume(volume)

        for raw in self.conn.block_storage.backups(**attrs):
            dt = raw.to_dict()
            dt.pop('location')
            data.append(dt)

        self.exit_json(
            changed=False,
            volume_backups=data
        )


def main():
    module = VolumeBackupInfoModule()
    module()


if __name__ == '__main__':
    main()
