#!/usr/bin/python

# Copyright (c) 2015 Hewlett-Packard Development Company, L.P.
# Copyright (c) 2013, Benno Joy <benno@ansible.com>
# GNU General Public License v3.0+ (see COPYING or https://www.gnu.org/licenses/gpl-3.0.txt)


# TODO(mordred): we need to support "location"(v1) and "locations"(v2)

DOCUMENTATION = '''
---
module: image
short_description: Add/Delete images from OpenStack Cloud
author: OpenStack Ansible SIG
description:
   - Add or Remove images from the OpenStack Image Repository
options:
   name:
     description:
        - The name of the image when uploading - or the name/ID of the image if deleting
     required: true
     type: str
   id:
     description:
        - The ID of the image when uploading an image
     type: str
   checksum:
     description:
        - The checksum of the image
     type: str
   disk_format:
     description:
        - The format of the disk that is getting uploaded
     default: qcow2
     choices: ['ami', 'ari', 'aki', 'vhd', 'vmdk', 'raw', 'qcow2', 'vdi', 'iso', 'vhdx', 'ploop']
     type: str
   container_format:
     description:
        - The format of the container
     default: bare
     choices: ['ami', 'aki', 'ari', 'bare', 'ovf', 'ova', 'docker']
     type: str
   project:
     description:
        - The name or ID of the project owning the image
     type: str
     aliases: ['owner']
   project_domain:
     description:
        - The domain the project owning the image belongs to
        - May be used to identify a unique project when providing a name to the project argument and multiple projects with such name exist
     type: str
   min_disk:
     description:
        - The minimum disk space (in GB) required to boot this image
     type: int
   min_ram:
     description:
        - The minimum ram (in MB) required to boot this image
     type: int
   is_public:
     description:
        - Whether the image can be accessed publicly. Note that publicizing an image requires admin role by default.
     type: bool
     default: false
   protected:
     description:
        - Prevent image from being deleted
     type: bool
     default: false
   filename:
     description:
        - The path to the file which has to be uploaded
     type: str
   ramdisk:
     description:
        - The name of an existing ramdisk image that will be associated with this image
     type: str
   kernel:
     description:
        - The name of an existing kernel image that will be associated with this image
     type: str
   properties:
     description:
        - Additional properties to be associated with this image
     default: {}
     type: dict
   state:
     description:
       - Should the resource be present or absent.
     choices: [present, absent]
     default: present
     type: str
   tags:
     description:
       - List of tags to be applied to the image
     default: []
     type: list
     elements: str
   volume:
     description:
       - ID of a volume to create an image from.
       - The volume must be in AVAILABLE state.
     type: str
requirements:
    - "python >= 3.6"
    - "openstacksdk"

extends_documentation_fragment:
- openstack.cloud.openstack
'''

EXAMPLES = '''
# Upload an image from a local file named cirros-0.3.0-x86_64-disk.img
- openstack.cloud.image:
    auth:
      auth_url: https://identity.example.com
      username: admin
      password: passme
      project_name: admin
      openstack.cloud.identity_user_domain_name: Default
      openstack.cloud.project_domain_name: Default
    name: cirros
    container_format: bare
    disk_format: qcow2
    state: present
    filename: cirros-0.3.0-x86_64-disk.img
    kernel: cirros-vmlinuz
    ramdisk: cirros-initrd
    tags:
      - custom
    properties:
      cpu_arch: x86_64
      distro: ubuntu

# Create image from volume attached to an instance
- name: create volume snapshot
  openstack.cloud.volume_snapshot:
    auth:
      "{{ auth }}"
    display_name: myvol_snapshot
    volume: myvol
    force: yes
  register: myvol_snapshot

- name: create volume from snapshot
  openstack.cloud.volume:
    auth:
      "{{ auth }}"
    size: "{{ myvol_snapshot.snapshot.size }}"
    snapshot_id: "{{ myvol_snapshot.snapshot.id }}"
    display_name: myvol_snapshot_volume
    wait: yes
  register: myvol_snapshot_volume

- name: create image from volume snapshot
  openstack.cloud.image:
    auth:
      "{{ auth }}"
    volume: "{{ myvol_snapshot_volume.volume.id }}"
    name: myvol_image
'''

from ansible_collections.openstack.cloud.plugins.module_utils.openstack import OpenStackModule


class ImageModule(OpenStackModule):

    deprecated_names = ('os_image', 'openstack.cloud.os_image')

    argument_spec = dict(
        name=dict(required=True, type='str'),
        id=dict(type='str'),
        checksum=dict(type='str'),
        disk_format=dict(default='qcow2',
                         choices=['ami', 'ari', 'aki', 'vhd', 'vmdk', 'raw', 'qcow2', 'vdi', 'iso', 'vhdx', 'ploop']),
        container_format=dict(default='bare', choices=['ami', 'aki', 'ari', 'bare', 'ovf', 'ova', 'docker']),
        project=dict(type='str', aliases=['owner']),
        project_domain=dict(type='str'),
        min_disk=dict(type='int', default=0),
        min_ram=dict(type='int', default=0),
        is_public=dict(type='bool', default=False),
        protected=dict(type='bool', default=False),
        filename=dict(type='str'),
        ramdisk=dict(type='str'),
        kernel=dict(type='str'),
        properties=dict(type='dict', default={}),
        volume=dict(type='str'),
        tags=dict(type='list', default=[], elements='str'),
        state=dict(default='present', choices=['absent', 'present']),
    )

    module_kwargs = dict(
        mutually_exclusive=[['filename', 'volume']],
    )

    def run(self):

        changed = False
        if self.params['id']:
            image = self.conn.get_image(name_or_id=self.params['id'])
        elif self.params['checksum']:
            image = self.conn.get_image(name_or_id=self.params['name'], filters={'checksum': self.params['checksum']})
        else:
            image = self.conn.get_image(name_or_id=self.params['name'])

        if self.params['state'] == 'present':
            if not image:
                kwargs = {}
                if self.params['id'] is not None:
                    kwargs['id'] = self.params['id']
                if self.params['project']:
                    project_domain = {'id': None}
                    if self.params['project_domain']:
                        project_domain = self.conn.get_domain(name_or_id=self.params['project_domain'])
                        if not project_domain or project_domain['id'] is None:
                            self.fail(msg='Project domain %s could not be found' % self.params['project_domain'])
                    project = self.conn.get_project(name_or_id=self.params['project'], domain_id=project_domain['id'])
                    if not project:
                        self.fail(msg='Project %s could not be found' % self.params['project'])
                    kwargs['owner'] = project['id']
                image = self.conn.create_image(
                    name=self.params['name'],
                    filename=self.params['filename'],
                    disk_format=self.params['disk_format'],
                    container_format=self.params['container_format'],
                    wait=self.params['wait'],
                    timeout=self.params['timeout'],
                    is_public=self.params['is_public'],
                    protected=self.params['protected'],
                    min_disk=self.params['min_disk'],
                    min_ram=self.params['min_ram'],
                    volume=self.params['volume'],
                    tags=self.params['tags'],
                    **kwargs
                )
                changed = True
                if not self.params['wait']:
                    self.exit(changed=changed, image=image, id=image.id)

            self.conn.update_image_properties(
                image=image,
                kernel=self.params['kernel'],
                ramdisk=self.params['ramdisk'],
                protected=self.params['protected'],
                **self.params['properties'])
            if self.params['tags']:
                self.conn.image.update_image(image.id, tags=self.params['tags'])
            image = self.conn.get_image(name_or_id=image.id)
            self.exit(changed=changed, image=image, id=image.id)

        elif self.params['state'] == 'absent':
            if not image:
                changed = False
            else:
                self.conn.delete_image(
                    name_or_id=self.params['name'],
                    wait=self.params['wait'],
                    timeout=self.params['timeout'])
                changed = True
            self.exit(changed=changed)


def main():
    module = ImageModule()
    module()


if __name__ == '__main__':
    main()
