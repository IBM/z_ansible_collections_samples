#!/usr/bin/python
# -*- coding: utf-8 -*-

# Copyright (c) IBM Corporation 2021
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#     http://www.apache.org/licenses/LICENSE-2.0
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.

import os
import subprocess
import json


from ansible.module_utils.basic import AnsibleModule


def get_collection_version(meta_file):
    try:
        with open(meta_file) as f:
            return get_json_value(json.load(f), 'version')
    except (IOError, OSError):
        pass

def get_json_value(obj, key):
    """Recursively fetch values for given key from JSON ."""
    result_array = []

    def search(obj, result_array, key):
        """Recursively search for values of key in JSON tree."""
        if isinstance(obj, dict):
            for k, v in obj.items():
                if isinstance(v, (dict, list)):
                    search(v, result_array, key)
                elif k == key:
                    result_array.append(v)
        elif isinstance(obj, list):
            for item in obj:
                search(item, result_array, key)
        return result_array

    result_vals = search(obj, result_array, key)
    # In this case we know its only going to be one value returned so
    # we can just return index[0]
    return result_vals[0]

def main():
    module_args = dict(
        data=dict(type='list', required=True),
    )

    module = AnsibleModule(
        argument_spec=module_args,
        supports_check_mode=True
    )

    data = module.params['data']

    for d in data:
        if 'installed_collection_manifest' in d:
            d['installed_collection_version'] = get_collection_version(d['installed_collection_manifest'])

    # We should not remove the objects with no version because they are used
    # to determine if the controller has the correct collection dependency installed.
    # data = [i for i in data if not (i['installed_collection_version'] == None)]

    module.exit_json(changed=False,results=data)

if __name__ == '__main__':
    main()

# If we wanted to search the JSON for version using a shell command.
# cat ~/.ansible/collections/ansible_collections/ibm/ibm_zos_core/MANIFEST.json |grep version|awk -F":" '{print $2}'|  tr -d \''"\,'