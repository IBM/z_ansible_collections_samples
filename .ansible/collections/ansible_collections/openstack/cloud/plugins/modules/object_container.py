#!/usr/bin/python
# coding: utf-8 -*-
#
# Copyright (c) 2021 by Open Telekom Cloud, operated by T-Systems International GmbH
# GNU General Public License v3.0+ (see COPYING or https://www.gnu.org/licenses/gpl-3.0.txt)

DOCUMENTATION = '''
---
module: object_container
short_description: Manage Swift container.
author: OpenStack Ansible SIG
description:
  - Manage Swift container.
options:
  container:
    description: Name of a container in Swift.
    type: str
    required: true
  metadata:
    description:
        - Key/value pairs to be set as metadata on the container.
        - If a container doesn't exist, it will be created.
        - Both custom and system metadata can be set.
        - Custom metadata are keys and values defined by the user.
        - The system metadata keys are content_type, content_encoding, content_disposition, delete_after,\
         delete_at, is_content_type_detected
    type: dict
    required: false
  keys:
    description: Keys from 'metadata' to be deleted.
    type: list
    elements: str
    required: false
  delete_with_all_objects:
    description:
        - Whether the container should be deleted with all objects or not.
        - Without this parameter set to "true", an attempt to delete a container that contains objects will fail.
    type: bool
    default: False
    required: false
  state:
    description: Whether resource should be present or absent.
    default: 'present'
    choices: ['present', 'absent']
    type: str
requirements:
    - "python >= 3.6"
    - "openstacksdk"

extends_documentation_fragment:
- openstack.cloud.openstack
'''

RETURN = '''
container:
  description: Specifies the container.
  returned: On success when C(state=present)
  type: dict
  sample:
    {
      "bytes": 5449,
      "bytes_used": 5449,
      "content_type": null,
      "count": 1,
      "id": "otc",
      "if_none_match": null,
      "is_content_type_detected": null,
      "is_newest": null,
      "meta_temp_url_key": null,
      "meta_temp_url_key_2": null,
      "name": "otc",
      "object_count": 1,
      "read_ACL": null,
      "sync_key": null,
      "sync_to": null,
      "timestamp": null,
      "versions_location": null,
      "write_ACL": null
    }
'''

EXAMPLES = '''
# Create empty container
 - openstack.cloud.object_container:
    container: "new-container"
    state: present

# Set metadata for container
 - openstack.cloud.object_container:
    container: "new-container"
    metadata: "Cache-Control='no-cache'"

# Delete some keys from metadata of a container
 - openstack.cloud.object_container:
    container: "new-container"
    keys:
        - content_type

# Delete container
 - openstack.cloud.object_container:
    container: "new-container"
    state: absent

# Delete container and its objects
 - openstack.cloud.object_container:
    container: "new-container"
    delete_with_all_objects: true
    state: absent
'''

from ansible_collections.openstack.cloud.plugins.module_utils.openstack import OpenStackModule


class ContainerModule(OpenStackModule):

    argument_spec = dict(
        container=dict(type='str', required=True),
        metadata=dict(type='dict', required=False),
        keys=dict(type='list', required=False, elements='str', no_log=False),
        state=dict(type='str', required=False, default='present', choices=['present', 'absent']),
        delete_with_all_objects=dict(type='bool', default=False, required=False)
    )

    def create(self, container):

        data = {}
        if self._container_exist(container):
            self.exit_json(changed=False)

        container_data = self.conn.object_store.create_container(name=container).to_dict()
        container_data.pop('location')
        data['container'] = container_data
        self.exit_json(changed=True, **data)

    def delete(self, container):

        delete_with_all_objects = self.params['delete_with_all_objects']

        changed = False
        if self._container_exist(container):
            objects = []
            for raw in self.conn.object_store.objects(container):
                dt = raw.to_dict()
                dt.pop('location')
                objects.append(dt)
            if len(objects) > 0:
                if delete_with_all_objects:
                    for obj in objects:
                        self.conn.object_store.delete_object(container=container, obj=obj['id'])
                else:
                    self.fail_json(msg="Container has objects")
            self.conn.object_store.delete_container(container=container)
            changed = True

        self.exit(changed=changed)

    def set_metadata(self, container, metadata):

        data = {}

        if not self._container_exist(container):
            new_container = self.conn.object_store.create_container(name=container).to_dict()

        new_container = self.conn.object_store.set_container_metadata(container, **metadata).to_dict()
        new_container.pop('location')
        data['container'] = new_container
        self.exit(changed=True, **data)

    def delete_metadata(self, container, keys):

        if not self._container_exist(container):
            self.fail_json(msg="Container doesn't exist")

        self.conn.object_store.delete_container_metadata(container=container, keys=keys)
        self.exit(changed=True)

    def _container_exist(self, container):
        try:
            self.conn.object_store.get_container_metadata(container)
            return True
        except self.sdk.exceptions.ResourceNotFound:
            return False

    def run(self):

        container = self.params['container']
        state = self.params['state']
        metadata = self.params['metadata']
        keys = self.params['keys']

        if state == 'absent':
            self.delete(container)
        if metadata:
            self.set_metadata(container, metadata)
        if keys:
            self.delete_metadata(container, keys)

        self.create(container)


def main():
    module = ContainerModule()
    module()


if __name__ == "__main__":
    main()
