#!/usr/bin/python
# coding: utf-8 -*-

# Copyright (c) 2020, Sagi Shnaidman <sshnaidm@redhat.com>
# GNU General Public License v3.0+ (see COPYING or https://www.gnu.org/licenses/gpl-3.0.txt)

DOCUMENTATION = '''
---
module: volume_info
short_description: Retrive information about volumes
author: Sagi Shnaidman (@sshnaidm)
description:
  - Get information about block storage in openstack
options:
  details:
    description:
    - Whether to provide additional information about volumes
    type: bool
  all_projects:
    description:
    - Whether return the volumes in all projects
    type: bool
  name:
    description:
    - Name of the volume as a string.
    type: str
    required: false
  status:
    description:
    - Value of the status of the volume so that you can filter on "available" for example
    type: str
    required: false

requirements:
  - "python >= 3.6"
  - "openstacksdk"

extends_documentation_fragment:
  - openstack.cloud.openstack
'''

RETURN = '''
volumes:
  description: Volumes in project
  returned: always
  type: list
  elements: dict
  sample:
    - attachments: []
      availability_zone: nova
      consistency_group_id: null
      created_at: '2017-11-15T10:51:19.000000'
      description: ''
      extended_replication_status: null
      host: null
      id: 103ac6ed-527f-4781-8484-7ff4467e34f5
      image_id: null
      is_bootable: true
      is_encrypted: false
      links:
      - href: https://...
        rel: self
      - href: https://...
        rel: bookmark
      location:
        cloud: cloud
        project:
          domain_id: null
          domain_name: Default
          id: cfe04702154742fc964d9403c691c76e
          name: username
        region_name: regionOne
        zone: nova
      metadata:
        readonly: 'False'
      migration_id: null
      migration_status: null
      name: ''
      project_id: cab34702154a42fc96ed9403c691c76e
      replication_driver_data: null
      replication_status: disabled
      size: 9
      snapshot_id: null
      source_volume_id: null
      status: available
      volume_image_metadata:
        checksum: a14e113deeee3a3392462f167ed28cb5
        container_format: bare
        disk_format: raw
        family: centos-7
        image_id: afcf3320-1bf8-4a9a-a24d-5abd639a6e33
        image_name: CentOS-7-x86_64-GenericCloud-1708
        latest: centos-7-latest
        min_disk: '0'
        min_ram: '0'
        official: 'True'
        official-image: 'True'
        size: '8589934592'
      volume_type: null
'''

EXAMPLES = '''
- openstack.cloud.volume_info:

- openstack.cloud.volume_info:
    name: myvolume

- openstack.cloud.volume_info:
    all_projects: true

- openstack.cloud.volume_info:
    all_projects: true
    details: true
'''

from ansible_collections.openstack.cloud.plugins.module_utils.openstack import OpenStackModule


class VolumeInfoModule(OpenStackModule):

    argument_spec = dict(
        details=dict(type='bool', required=False),
        all_projects=dict(type='bool', required=False, min_ver='0.19'),
        name=dict(type='str', required=False),
        status=dict(type='str', required=False),
    )
    module_kwargs = dict(
        supports_check_mode=True
    )

    def run(self):
        kwargs = self.check_versioned(
            details=self.params['details'],
            name=self.params['name'],
            all_projects=self.params['all_projects'],
            status=self.params['status'],
        )
        result = self.conn.block_storage.volumes(**kwargs)
        result = [vol if isinstance(vol, dict) else vol.to_dict() for vol in result]
        self.results.update({'volumes': result})


def main():
    module = VolumeInfoModule()
    module()


if __name__ == '__main__':
    main()
