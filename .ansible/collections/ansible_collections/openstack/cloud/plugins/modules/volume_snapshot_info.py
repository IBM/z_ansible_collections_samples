#!/usr/bin/python
# coding: utf-8 -*-
#
# Copyright (c) 2020 by Open Telekom Cloud, operated by T-Systems International GmbH
# GNU General Public License v3.0+ (see COPYING or https://www.gnu.org/licenses/gpl-3.0.txt)


DOCUMENTATION = '''
---
module: volume_snapshot_info
short_description: Get volume snapshots
author: OpenStack Ansible SIG
description:
  - Get Volume Snapshot info from the Openstack cloud.
options:
  details:
    description: More detailed output
    type: bool
    default: True
  name:
    description:
      - Name of the Snapshot.
    type: str
  volume:
    description:
      - Name of the volume.
    type: str
  status:
    description:
      - Specifies the snapshot status.
    choices: [creating, available, error, deleting,
              error_deleting, rollbacking, backing-up]
    type: str
requirements: ["openstacksdk"]
extends_documentation_fragment:
- openstack.cloud.openstack
'''

RETURN = '''
volume_snapshots:
    description: List of dictionaries describing volume snapshots.
    type: list
    elements: dict
    returned: always.
    contains:
        created_at:
            description: Snapshot creation time.
            type: str
        description:
            description: Snapshot desciption.
            type: str
        id:
            description: Unique UUID.
            type: str
            sample: "39007a7e-ee4f-4d13-8283-b4da2e037c69"
        metadata:
            description: Snapshot metadata.
            type: dict
        name:
            description: Snapshot Name.
            type: str
        status:
            description: Snapshot status.
            type: str
        updated_at:
            description: Snapshot update time.
            type: str
        volume_id:
            description: Volume ID.
            type: str

'''

EXAMPLES = '''
# Get snapshots.
- openstack.cloud.volume_snapshot_info:
  register: snapshots

- openstack.cloud.volume_snapshotbackup_info:
    name: my_fake_snapshot
  register: snapshot
'''

from ansible_collections.openstack.cloud.plugins.module_utils.openstack import OpenStackModule


class VolumeSnapshotInfoModule(OpenStackModule):
    module_min_sdk_version = '0.49.0'

    argument_spec = dict(
        details=dict(default=True, type='bool'),
        name=dict(required=False, type='str'),
        volume=dict(required=False, type='str'),
        status=dict(required=False, type='str',
                    choices=['creating', 'available', 'error',
                             'deleting', 'error_deleting', 'rollbacking',
                             'backing-up']),
    )
    module_kwargs = dict(
        supports_check_mode=True
    )

    def run(self):

        details_filter = self.params['details']
        name_filter = self.params['name']
        volume_filter = self.params['volume']
        status_filter = self.params['status']

        data = []
        query = {}
        if name_filter:
            query['name'] = name_filter
        if volume_filter:
            query['volume_id'] = self.conn.block_storage.find_volume(volume_filter)
        if status_filter:
            query['status'] = status_filter.lower()

        for raw in self.conn.block_storage.snapshots(details_filter, **query):
            dt = raw.to_dict()
            dt.pop('location')
            data.append(dt)

        self.exit_json(
            changed=False,
            volume_snapshots=data
        )


def main():
    module = VolumeSnapshotInfoModule()
    module()


if __name__ == '__main__':
    main()
