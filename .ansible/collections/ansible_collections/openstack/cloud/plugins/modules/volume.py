#!/usr/bin/python

# Copyright (c) 2014 Hewlett-Packard Development Company, L.P.
# GNU General Public License v3.0+ (see COPYING or https://www.gnu.org/licenses/gpl-3.0.txt)

DOCUMENTATION = '''
---
module: volume
short_description: Create/Delete Cinder Volumes
author: OpenStack Ansible SIG
description:
   - Create or Remove cinder block storage volumes
options:
   size:
     description:
        - Size of volume in GB. This parameter is required when the
          I(state) parameter is 'present'.
     type: int
   display_name:
     description:
        - Name of volume
     required: true
     type: str
     aliases: [name]
   display_description:
     description:
       - String describing the volume
     type: str
     aliases: [description]
   volume_type:
     description:
       - Volume type for volume
     type: str
   image:
     description:
       - Image name or id for boot from volume
     type: str
   snapshot_id:
     description:
       - Volume snapshot id to create from
     type: str
   volume:
     description:
       - Volume name or id to create from
     type: str
   bootable:
     description:
       - Bootable flag for volume.
     type: bool
     default: False
   state:
     description:
       - Should the resource be present or absent.
     choices: [present, absent]
     default: present
     type: str
   scheduler_hints:
     description:
       - Scheduler hints passed to volume API in form of dict
     type: dict
   metadata:
     description:
       - Metadata for the volume
     type: dict
requirements:
    - "python >= 3.6"
    - "openstacksdk"

extends_documentation_fragment:
- openstack.cloud.openstack
'''

EXAMPLES = '''
# Creates a new volume
- name: create a volume
  hosts: localhost
  tasks:
  - name: create 40g test volume
    openstack.cloud.volume:
      state: present
      cloud: mordred
      availability_zone: az2
      size: 40
      display_name: test_volume
      scheduler_hints:
        same_host: 243e8d3c-8f47-4a61-93d6-7215c344b0c0
'''

RETURNS = '''
id:
  description: Cinder's unique ID for this volume
  returned: always
  type: str
  sample: fcc4ac1c-e249-4fe7-b458-2138bfb44c06

volume:
  description: Cinder's representation of the volume object
  returned: always
  type: dict
  sample: {'...'}
'''
from ansible_collections.openstack.cloud.plugins.module_utils.openstack import OpenStackModule


class VolumeModule(OpenStackModule):

    argument_spec = dict(
        size=dict(type='int'),
        volume_type=dict(type='str'),
        display_name=dict(required=True, aliases=['name'], type='str'),
        display_description=dict(aliases=['description'], type='str'),
        image=dict(type='str'),
        snapshot_id=dict(type='str'),
        volume=dict(type='str'),
        state=dict(default='present', choices=['absent', 'present'], type='str'),
        scheduler_hints=dict(type='dict'),
        metadata=dict(type='dict'),
        bootable=dict(type='bool', default=False)
    )

    module_kwargs = dict(
        mutually_exclusive=[
            ['image', 'snapshot_id', 'volume'],
        ],
        required_if=[
            ['state', 'present', ['size']],
        ],
    )

    def _needs_update(self, volume):
        '''
        check for differences in updatable values, at the moment
        openstacksdk only supports extending the volume size, this
        may change in the future.
        :returns: bool
        '''
        compare_simple = ['size']

        for k in compare_simple:
            if self.params[k] is not None and self.params[k] != volume.get(k):
                return True

        return False

    def _modify_volume(self, volume):
        '''
        modify volume, the only modification to an existing volume
        available at the moment is extending the size, this is
        limited by the openstacksdk and may change whenever the
        functionality is extended.
        '''
        volume = self.conn.get_volume(self.params['display_name'])
        diff = {'before': volume, 'after': ''}
        size = self.params['size']

        if size < volume.get('size'):
            self.fail_json(
                msg='Cannot shrink volumes, size: {0} < {1}'.format(size, volume.get('size'))
            )

        if not self._needs_update(volume):
            diff['after'] = volume
            self.exit_json(changed=False, id=volume['id'], volume=volume, diff=diff)

        if self.ansible.check_mode:
            diff['after'] = volume
            self.exit_json(changed=True, id=volume['id'], volume=volume, diff=diff)

        self.conn.volume.extend_volume(
            volume.id,
            size
        )
        diff['after'] = self.conn.get_volume(self.params['display_name'])
        self.exit_json(changed=True, id=volume['id'], volume=volume, diff=diff)

    def _present_volume(self):

        diff = {'before': '', 'after': ''}

        volume_args = dict(
            size=self.params['size'],
            volume_type=self.params['volume_type'],
            display_name=self.params['display_name'],
            display_description=self.params['display_description'],
            snapshot_id=self.params['snapshot_id'],
            bootable=self.params['bootable'],
            availability_zone=self.params['availability_zone'],
        )
        if self.params['image']:
            image_id = self.conn.get_image_id(self.params['image'])
            if not image_id:
                self.fail_json(msg="Failed to find image '%s'" % self.params['image'])
            volume_args['imageRef'] = image_id

        if self.params['volume']:
            volume_id = self.conn.get_volume_id(self.params['volume'])
            if not volume_id:
                self.fail_json(msg="Failed to find volume '%s'" % self.params['volume'])
            volume_args['source_volid'] = volume_id

        if self.params['scheduler_hints']:
            volume_args['scheduler_hints'] = self.params['scheduler_hints']

        if self.params['metadata']:
            volume_args['metadata'] = self.params['metadata']

        if self.ansible.check_mode:
            diff['after'] = volume_args
            self.exit_json(changed=True, id=None, volume=volume_args, diff=diff)

        volume = self.conn.create_volume(
            wait=self.params['wait'], timeout=self.params['timeout'],
            **volume_args)
        diff['after'] = volume
        self.exit_json(changed=True, id=volume['id'], volume=volume, diff=diff)

    def _absent_volume(self, volume):
        changed = False
        diff = {'before': '', 'after': ''}

        if self.conn.volume_exists(self.params['display_name']):
            volume = self.conn.get_volume(self.params['display_name'])
            diff['before'] = volume

            if self.ansible.check_mode:
                self.exit_json(changed=True, diff=diff)

            try:
                changed = self.conn.delete_volume(name_or_id=self.params['display_name'],
                                                  wait=self.params['wait'],
                                                  timeout=self.params['timeout'])
            except self.sdk.exceptions.ResourceTimeout:
                diff['after'] = volume
                self.exit_json(changed=changed, diff=diff)

        self.exit_json(changed=changed, diff=diff)

    def run(self):

        state = self.params['state']
        if self.conn.volume_exists(self.params['display_name']):
            volume = self.conn.get_volume(self.params['display_name'])
        else:
            volume = None

        if state == 'present':
            if not volume:
                self._present_volume()
            elif self._needs_update(volume):
                self._modify_volume(volume)
            else:
                self.exit_json(changed=False, id=volume['id'], volume=volume)
        if state == 'absent':
            self._absent_volume(volume)


def main():
    module = VolumeModule()
    module()


if __name__ == '__main__':
    main()
