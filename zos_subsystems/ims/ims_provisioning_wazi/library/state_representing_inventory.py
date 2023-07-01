# -*- coding: utf-8 -*-

from __future__ import absolute_import, division, print_function

ANSIBLE_METADATA = {
    'metadata_version': '1.1',
    'status': ['preview'],
    'supported_by': 'Blake Becker, IMS Cloud Native'
}

__metaclass__ = type

from ansible.module_utils.basic import AnsibleModule
import requests
from datetime import datetime
import json
from traceback import format_exc


class RequestHandler():
    def __init__(self, username, password, base_url):
        self.username = username
        self.password = password
        self.base_url = base_url

    def get(self, route, expected_status=200, get_response=True):
        response = requests.get(url=self.base_url + route, auth=(self.username, self.password))
        if response.status_code != expected_status:
            return
        if expected_status == 204:
            return True
        return response.json()

    def post(self, route, data=None, expected_status=201, get_response=True):
        response = requests.post(url=self.base_url + route, auth=(self.username, self.password), json=data)
        if response.status_code != expected_status:
            return
        if expected_status == 204:
            return True
        return response.json()

    def patch(self, route, data, expected_status=200, get_response=True):
        response = requests.patch(url=self.base_url + route, auth=(self.username, self.password), json=data)
        if response.status_code != expected_status:
            return
        if expected_status == 204:
            return True
        return response.json()


class TowerResource(object):
    def __init__(self, request_handler, name, identifier):
        self.request_handler = request_handler
        self.name = name
        self.identifier = identifier


class Inventory(TowerResource):
    def __init__(self, request_handler, name, identifier):
        super(Inventory, self).__init__(request_handler, name, identifier)
        self.variables = self.get_variables()

    @classmethod
    def from_name(cls, request_handler, name):
        identifier = get_field(request_handler, '/inventories/', 'id', name)
        if identifier is None:
            raise InventoryNotFound(name)
        return cls(request_handler, name, identifier)

    def get_variables(self):
        req = self.request_handler.get(route='/inventories/{0}/variable_data/'.format(str(self.identifier)))
        if req is None:
            raise VarsNotFound(self.name, self.identifier)
        return req

    def update_variables(self, variables):
        data = {'variables': json.dumps(variables)}
        res = self.request_handler.patch(route='/inventories/{0}/'.format(str(self.identifier)), data=data)
        if res is None:
            raise InventoryUpdateFail(self.name, self.identifier)
        return True


class StatefulTemplateManager():
    def __init__(self, provisioned, deprovisioned):
        self.provisioned = provisioned
        self.deprovisioned = deprovisioned

    @classmethod
    def from_dict(cls, json_data):
        templates = {
            'provisioned': {
                'workflows': [],
                'jobs': []
            },
            'deprovisioned': {
                'workflows': [],
                'jobs': []
            }
        }
        for state in templates:
            if json_data.get(state) is not None:
                for template_type in templates.get(state):
                    if json_data.get(state).get(template_type) is not None:
                        templates[state][template_type] = json_data.get(state).get(template_type)
        return cls(**templates)

    def add_template(self, name, template_type):
        inventory_type_name = template_type + 's'
        if name not in self.provisioned.get(inventory_type_name):
            self.provisioned.get(inventory_type_name).append(name)
        return name

    def flag_template(self, name, template_type):
        inventory_type_name = template_type + 's'
        if name in self.provisioned.get(inventory_type_name):
            self.provisioned.get(inventory_type_name).remove(name)
            self.deprovisioned.get(inventory_type_name).append(name)
        return name

    def remove_template(self, name, template_type):
        inventory_type_name = template_type + 's'
        if name in self.provisioned.get(inventory_type_name):
            self.provisioned.get(inventory_type_name).remove(name)
        if name in self.deprovisioned.get(inventory_type_name):
            self.deprovisioned.get(inventory_type_name).remove(name)
        return name

    def to_dict(self):
        return {'provisioned': self.provisioned, 'deprovisioned': self.deprovisioned}


def get_field(request_handler, route, field_to_get, value_to_find, field_to_search='name'):
    req = request_handler.get(route=route)
    found_field = None
    if req is None:
        return None
    for item in req.get('results'):
        if str(item.get(field_to_search)) == str(value_to_find):
            found_field = item.get(field_to_get)
            break
    return found_field


# Exception definitions
class Error(Exception):
    pass


class InventoryNotFound(Error):
    def __init__(self, inventory_name):
        self.msg = 'No ID was found for the provided inventory name "{0}". \
                    Verify the inventory exists and that provided capitalization and punctuation is correct.'.format(inventory_name)


class VarsNotFound(Error):
    def __init__(self, inventory_name, inventory_id):
        self.msg = 'No vars were found for the provided inventory name "{0}" and ID "{1}". \
                    Verify the inventory exists and provided capitalization and punctuation is correct.'.format(inventory_name, str(inventory_id))


class InventoryUpdateFail(Error):
    def __init__(self, inventory_name, inventory_id):
        self.msg = 'An attempt to update existing inventory "{0}" with ID "{1}" failed.'.format(inventory_name, str(inventory_id))


def run_module():
    module_args = dict(
        inventory_name=dict(
            type='str',
            required=True,
            aliases=['name', 'inventory']
        ),
        template_name=dict(
            type='str',
            required=True,
            aliases=['template']
        ),
        tower_username=dict(
            type='str',
            required=True,
            aliases=['tower_user']
        ),
        tower_password=dict(
            type='str',
            required=True,
            no_log=True,
            aliases=['password', 'tower_pass']
        ),
        base_url=dict(
            type='str',
            default='localhost:31180',
            aliases=['url', 'uri']
        ),
        state=dict(
            type='str',
            default='present',
            choices=['present', 'absent', 'flagged']
        ),
        template_type=dict(
            type='str',
            default='job',
            choices=['job', 'workflow'],
            aliases=['type']
        )
    )

    result = dict(
        changed=False,
        original_message='',
        message=''
    )

    module = AnsibleModule(
        argument_spec=module_args,
        supports_check_mode=True
    )

    if module.check_mode:
        return result

    try:
        # what to send in reply once we are done, remove password from return values
        original_message = {key: value for key, value in module.params.items() if key != 'tower_password'}

        api_url = module.params.get('base_url').rstrip('/') + '/api/v2'
        if not api_url.startswith('http'):
            api_url = 'http://{0}'.format(api_url)

        request_handler = RequestHandler(module.params.get('tower_username'), module.params.get('tower_password'), api_url)

        inventory = Inventory.from_name(request_handler, module.params.get('inventory_name'))

        variable_data = StatefulTemplateManager.from_dict(inventory.variables)

        template_name = module.params.get('template_name')
        template_type = module.params.get('template_type')
        if module.params.get('state') == 'flagged':
            variable_data.flag_template(template_name, template_type)
        elif module.params.get('state') == 'absent':
            variable_data.remove_template(template_name, template_type)
        else:
            variable_data.add_template(template_name, template_type)

        inventory.update_variables(variable_data.to_dict())

    except Error as e:
        module.fail_json(msg=e.msg, **result)
    except Exception as e:
        trace = format_exc()
        module.fail_json(msg='An unexpected error occurred: {0}'.format(trace), **result)

    result['changed'] = True
    result['original_message'] = original_message
    result['message'] = {'stdout': 'Inventory variables updated!', 'stderr': ''}
    result['variables'] = variable_data.to_dict()

    module.exit_json(**result)


def main():
    run_module()


if __name__ == '__main__':
    main()
