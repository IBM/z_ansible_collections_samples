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

    def get(self, route, expected_status=200):
        response = requests.get(url=self.base_url + route, auth=(self.username, self.password))
        if response.status_code != expected_status:
            return
        if expected_status == 204:
            return True
        return response.json()

    def post(self, route, data=None, expected_status=201):
        response = requests.post(url=self.base_url + route, auth=(self.username, self.password), json=data)
        if response.status_code != expected_status:
            return
        if expected_status == 204:
            return True
        return response.json()

    def patch(self, route, data, expected_status=200):
        response = requests.patch(url=self.base_url + route, auth=(self.username, self.password), json=data)
        if response.status_code != expected_status:
            return
        if expected_status == 204:
            return True
        return response.json()

    def delete(self, route, expected_status=204):
        response = requests.delete(url=self.base_url + route, auth=(self.username, self.password))
        if response.status_code != expected_status:
            return
        return True


class TowerResource(object):
    def __init__(self, request_handler, name, identifier):
        self.request_handler = request_handler
        self.name = name
        self.identifier = identifier


class User(TowerResource):
    def __init__(self, request_handler, name, identifier):
        super(User, self).__init__(request_handler, name, identifier)

    @classmethod
    def from_name(cls, request_handler, name):
        identifier = get_field(request_handler, '/users/', 'id', name, 'username')
        if identifier is None:
            raise UserNotFound(name)
        return cls(request_handler, name, identifier)

    @classmethod
    def from_id(cls, request_handler, identifier):
        name = get_field(request_handler, '/users/', 'username', identifier, 'id')
        if name is None:
            raise UserNotFound(identifier)
        return cls(request_handler, name, identifier)


class Template(TowerResource):
    def __init__(self, request_handler, name, identifier):
        super(Template, self).__init__(request_handler, name, identifier)


class Workflow(Template):
    def __init__(self, request_handler, name, identifier):
        super(Workflow, self).__init__(request_handler, name, identifier)

    @classmethod
    def from_name(cls, request_handler, name):
        identifier = get_field(request_handler, '/workflow_job_templates/', 'id', name)
        if identifier is None:
            raise WorkflowNotFound(name)
        return cls(request_handler, name, identifier)

    def can_copy(self):
        res = self.request_handler.get(route='/workflow_job_templates/{0}/copy/'.format(str(self.identifier)))
        if res is None:
            return False
        if res.get('can_copy') is True and res.get('can_copy_without_user_input') is True:
            return True
        return False

    def copy(self, new_name):
        data = {
            'name': str(new_name)
        }
        res = self.request_handler.post(route='/workflow_job_templates/{0}/copy/'.format(str(self.identifier)), data=data)
        if res is None:
            raise WorkflowCopyFail(self.name, self.identifier)
        return Workflow(self.request_handler, res.get('name'), res.get('id'))

    # TODO: add option to update or override extra_vars, current behavior is override
    def update(self, updated_name=None, updated_description=None, updated_extra_vars=None, updated_inventory=None):
        data = {}
        if updated_name:
            data['name'] = updated_name
        if updated_description:
            data['description'] = updated_description
        if updated_extra_vars:
            data['extra_vars'] = json.dumps(updated_extra_vars)
        if updated_inventory:
            data['inventory'] = updated_inventory
        if len(data) > 0:
            res = self.request_handler.patch(route='/workflow_job_templates/{0}/'.format(str(self.identifier)), data=data)
            if res is None:
                raise WorkflowUpdateFail(self.name, self.identifier)
        return self.identifier

    def delete(self):
        res = self.request_handler.delete(route='/workflow_job_templates/{0}/'.format(str(self.identifier)))
        if res is None:
            raise WorkflowDeleteFail(self.identifier)
        return self.identifier


class Role(TowerResource):
    def __init__(self, request_handler, name, identifier, container, container_identifier):
        super(Role, self).__init__(request_handler, name, identifier)
        self.container = container
        self.container_identifier = container_identifier

    @classmethod
    def from_role_name_in_workflow(cls, request_handler, workflow, name):
        identifier = get_field(request_handler, '/workflow_job_templates/' + str(workflow.identifier) + '/object_roles/', 'id', name)
        if identifier is None:
            raise RoleNotFound('Execute', workflow.name, workflow.identifier)
        return cls(request_handler, name, identifier, workflow.name, workflow.identifier)

    def add_user(self, user):
        res = self.request_handler.post(route='/roles/{0}/users/'.format(str(self.identifier)), data={'id': int(user.identifier)}, expected_status=204)
        if res is None:
            raise RoleUpdateUserFail(user.name, user.identifier, self.name, self.container, self.container_identifier)
        return self.identifier


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


def get_new_workflow_name(workflow_name, name, username):
    now = datetime.now().strftime("%B-%d-%Y %H:%M:%S")
    return '{0} {1} {2} {3}'.format(str(workflow_name), str(name), str(username), now)


def make_state_present(request_handler, module, workflow):
    user_id = module.params.get('user_id')
    username = module.params.get('username')
    if user_id is None:
        user = User.from_name(request_handler, username)
    elif username is None:
        user = User.from_id(request_handler, user_id)

    new_workflow_name = get_new_workflow_name(module.params.get('workflow_name'), module.params.get('provisioned_instance_name'), user.name)

    # ensure we have permission to copy workflow before we go any further
    if not workflow.can_copy():
        raise WorkflowCopyPermissionError()
    workflow_copy = workflow.copy(new_workflow_name)

    if module.params.get('keep_extra_vars'):
        extra_vars_text = request_handler.get('/workflow_job_templates/{0}/'.format(str(workflow_copy.identifier))).get('extra_vars')
        if extra_vars_text:
            extra_vars = json.loads(extra_vars_text)
        else:
            extra_vars = {}
        extra_vars.update(module.params.get('extra_vars'))
    else:
        extra_vars = module.params.get('extra_vars')

    extra_vars['tower_template_name'] = new_workflow_name

    workflow_copy.update(None, module.params.get('description'), extra_vars)

    role = Role.from_role_name_in_workflow(request_handler, workflow_copy, 'Execute')
    role.add_user(user)
    return new_workflow_name


def make_state_absent(request_handler, workflow):
    delete_workflow_response = workflow.delete()
    return True


# Exception definitions
class Error(Exception):
    pass


class WorkflowNotFound(Error):
    def __init__(self, workflow_name):
        self.msg = 'No ID was found for the provided workflow job template name "{0}". \
                    Verify the workflow exists and that provided capitalization and punctuation is correct.'.format(workflow_name)


class WorkflowCopyFail(Error):
    def __init__(self, workflow_name, workflow_id):
        self.msg = 'An attempt to copy workflow "{0}" with ID "{1}" failed.'.format(workflow_name, str(workflow_id))


class WorkflowCopyPermissionError(Error):
    def __init__(self):
        self.msg = 'Username provided in ansible_username does not have permission to copy this workflow OR cannot copy without user input.'


class WorkflowUpdateFail(Error):
    def __init__(self, workflow_name, workflow_id):
        self.msg = 'An attempt to update existing workflow "{0}" with ID "{1}" failed.'.format(workflow_name, str(workflow_id))


class UserNotFound(Error):
    def __init__(self, field):
        self.msg = 'No ID/username was found for the provided corresponding ID/username "{0}". \
                    Verify the username exists and that provided capitalization and punctuation is correct.'.format(str(field))


class RoleNotFound(Error):
    def __init__(self, role, role_container, role_container_id):
        self.msg = 'No ID was found for the role "{0}" in "{1}" with ID "{2}". \
                    Verify the role exists and that provided capitalization and punctuation is correct.'.format(role, role_container, str(role_container_id))


class RoleUpdateUserFail(Error):
    def __init__(self, username, user_id, role, role_container, role_container_id):
        self.msg = 'An attempt to add username "{0}" with ID "{1}" to \
                    role "{2}" in "{3}" with ID "{4}" failed.'.format(username, str(user_id), role, role_container, str(role_container_id))


class UserParamMissing(Error):
    def __init__(self):
        self.msg = 'Must specify either "username" or "user_id" parameter.'


class MissingArg(Error):
    def __init__(self, arg, state):
        self.msg = 'Missing required argument. For state "{0}" the argument {1} must be present.'.format(state, arg)


class WorkflowDeleteFail(Error):
    def __init__(self, workflow_id):
        self.msg = 'An attempt to delete existing workflow with ID "{0}" failed.'.format(str(workflow_id))


def check_for_missing_args(module):
    expected_state = module.params.get('state')
    if expected_state == 'present' or expected_state == 'copied':
        if module.params.get('username') is None and module.params.get('user_id') is None:
            raise UserParamMissing()
        if module.params.get('provisioned_instance_name') is None:
            raise MissingArg('provisioned_instance_name', 'present/copied')
    else:
        if module.params.get('workflow_name') is None:
            raise MissingArg('workflow_name', 'absent')
    return


def run_module():
    module_args = dict(
        workflow_name=dict(
            type='str',
            required=False,
            aliases=['workflow', 'name']
        ),
        description=dict(
            type='str',
            required=False,
        ),
        username=dict(
            type='str',
            required=False,
            aliases=['user']
        ),
        user_id=dict(
            type='str',
            required=False,
        ),
        provisioned_instance_name=dict(
            type='str',
            required=False,
            aliases=['instance', 'instance_name']
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
        extra_vars=dict(
            type='dict',
            default={},
            aliases=['variables']
        ),
        keep_extra_vars=dict(
            type='bool',
            required=False,
            default=True,
            aliases=['append_extra_vars', 'keep_vars']
        ),
        base_url=dict(
            type='str',
            default='localhost:31180',
            aliases=['url', 'uri']
        ),
        state=dict(
            type='str',
            default='present',
            choices=['present', 'absent', 'copied']
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

    parameter_defaults = {
        'description': 'Workflow referencing provisioned instance {0} for username {1}'.format(module.params.get('provisioned_instance_name'),
                                                                                               module.params.get('username'))
    }

    try:
        expected_state = module.params.get('state')

        check_for_missing_args(module)

        for key, value in parameter_defaults.items():
            if not module.params.get(key):
                module.params[key] = value

        # what to send in reply once we are done, remove password from return values
        # this should be taken care of with no_log=True, but good to have backup
        original_message = {key: value for key, value in module.params.items() if key != 'tower_password'}

        api_url = module.params.get('base_url').rstrip('/') + '/api/v2'
        if not api_url.startswith('http'):
            api_url = 'http://{0}'.format(api_url)

        request_handler = RequestHandler(module.params.get('tower_username'), module.params.get('tower_password'), api_url)

        nothing_to_change = False

        try:
            workflow = Workflow.from_name(request_handler, module.params.get('workflow_name'))
        # if exception is raised, workflow most likely couldn't be found
        except Exception as e:
            if expected_state == 'present' or expected_state == 'copied':
                raise e
            else:
                nothing_to_change = True

        if not nothing_to_change:
            if expected_state == 'present' or expected_state == 'copied':
                new_workflow_name = make_state_present(request_handler, module, workflow)
                result['workflow_name'] = new_workflow_name
                result['message'] = {'stdout': 'Workflow created for instance.', 'stderr': ''}
            else:
                make_state_absent(request_handler, workflow)
                result['message'] = {'stdout': 'Workflow deleted.', 'stderr': ''}
            result['changed'] = True
        else:
            result['changed'] = False
            result['message'] = {'stdout': 'Workflow already absent.', 'stderr': ''}

    except Error as e:
        module.fail_json(msg=e.msg, **result)
    except Exception as e:
        trace = format_exc()
        module.fail_json(msg='An unexpected error occurred: {0}'.format(trace), **result)

    result['original_message'] = original_message

    module.exit_json(**result)


def main():
    run_module()


if __name__ == '__main__':
    main()
