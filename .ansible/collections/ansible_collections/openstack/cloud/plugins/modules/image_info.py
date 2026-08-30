#!/usr/bin/python

# Copyright (c) 2015 Hewlett-Packard Development Company, L.P.
# GNU General Public License v3.0+ (see COPYING or https://www.gnu.org/licenses/gpl-3.0.txt)

DOCUMENTATION = '''
---
module: image_info
short_description: Retrieve information about an image within OpenStack.
author: OpenStack Ansible SIG
description:
    - Retrieve information about a image image from OpenStack.
    - This module was called C(openstack.cloud.image_facts) before Ansible 2.9, returning C(ansible_facts).
      Note that the M(openstack.cloud.image_info) module no longer returns C(ansible_facts)!
options:
   image:
     description:
        - Name or ID of the image
     required: false
     type: str
   filters:
     description:
        - Dict of properties of the images used for query
     type: dict
     required: false
     aliases: ['properties']
requirements:
    - "python >= 3.6"
    - "openstacksdk"

extends_documentation_fragment:
- openstack.cloud.openstack
'''

EXAMPLES = '''
- name: Gather information about a previously created image named image1
  openstack.cloud.image_info:
    auth:
      auth_url: https://identity.example.com
      username: user
      password: password
      project_name: someproject
    image: image1
  register: result

- name: Show openstack information
  debug:
    msg: "{{ result.image }}"

# Show all available Openstack images
- name: Retrieve all available Openstack images
  openstack.cloud.image_info:
  register: result

- name: Show images
  debug:
    msg: "{{ result.image }}"

# Show images matching requested properties
- name: Retrieve images having properties with desired values
  openstack.cloud.image_facts:
    filters:
      some_property: some_value
      OtherProp: OtherVal

- name: Show images
  debug:
    msg: "{{ result.image }}"
'''

RETURN = '''
openstack_images:
    description: has all the openstack information about the image
    returned: always, but can be null
    type: complex
    contains:
        id:
            description: Unique UUID.
            returned: success
            type: str
        name:
            description: Name given to the image.
            returned: success
            type: str
        status:
            description: Image status.
            returned: success
            type: str
        created_at:
            description: Image created at timestamp.
            returned: success
            type: str
        container_format:
            description: Container format of the image.
            returned: success
            type: str
        direct_url:
            description: URL to access the image file kept in external store.
            returned: success
            type: str
        min_ram:
            description: Min amount of RAM required for this image.
            returned: success
            type: int
        disk_format:
            description: Disk format of the image.
            returned: success
            type: str
        file:
            description: The URL for the virtual machine image file.
            returned: success
            type: str
        os_hidden:
            description: Controls whether an image is displayed in the default image-list response
            returned: success
            type: bool
        locations:
            description: A list of URLs to access the image file in external store.
            returned: success
            type: str
        metadata:
            description: The location metadata.
            returned: success
            type: str
        schema:
            description: URL for the schema describing a virtual machine image.
            returned: success
            type: str
        updated_at:
            description: Image updated at timestamp.
            returned: success
            type: str
        virtual_size:
            description: The virtual size of the image.
            returned: success
            type: str
        min_disk:
            description: Min amount of disk space required for this image.
            returned: success
            type: int
        is_protected:
            description: Image protected flag.
            returned: success
            type: bool
        checksum:
            description: Checksum for the image.
            returned: success
            type: str
        owner:
            description: Owner for the image.
            returned: success
            type: str
        visibility:
            description: Indicates who has access to the image.
            returned: success
            type: str
        size:
            description: Size of the image.
            returned: success
            type: int
        tags:
            description: List of tags assigned to the image
            returned: success
            type: list
'''
from ansible_collections.openstack.cloud.plugins.module_utils.openstack import OpenStackModule


class ImageInfoModule(OpenStackModule):

    deprecated_names = ('openstack.cloud.os_image_facts', 'openstack.cloud.os_image_info')

    argument_spec = dict(
        image=dict(type='str', required=False),
        filters=dict(type='dict', required=False, aliases=['properties']),
    )
    module_kwargs = dict(
        supports_check_mode=True
    )

    def run(self):
        args = {
            'name_or_id': self.params['image'],
            'filters': self.params['filters'],
        }
        args = {k: v for k, v in args.items() if v is not None}
        images = self.conn.search_images(**args)

        # for backward compatibility
        if 'name_or_id' in args:
            image = images[0] if images else None
        else:
            image = images

        self.exit(changed=False, openstack_images=images, image=image)


def main():
    module = ImageInfoModule()
    module()


if __name__ == '__main__':
    main()
