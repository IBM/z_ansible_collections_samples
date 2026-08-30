#!/usr/bin/python
# -*- coding: utf-8 -*-
# Copyright: (c) 2022, XLAB Steampunk <steampunk@xlab.si>
#
# GNU General Public License v3.0+ (see COPYING or https://www.gnu.org/licenses/gpl-3.0.txt)

from __future__ import absolute_import, division, print_function

__metaclass__ = type

DOCUMENTATION = r"""
module: api

author:
  - Tjaž Eržen (@tjazsch)
  - Jure Medvešek (@juremedvesek)

short_description: Manage ServiceNow POST, PATCH and DELETE requests
description:
  - Create, delete or update a ServiceNow record from the given resource.
  - For more information, refer to the ServiceNow REST Table API documentation at
    U(https://docs.servicenow.com/bundle/tokyo-application-development/page/integrate/inbound-rest/concept/c_RESTAPI.html).
version_added: 2.0.0
seealso:
  - module: servicenow.itsm.api_info
extends_documentation_fragment:
  - servicenow.itsm.instance
  - servicenow.itsm.sys_id
options:
  sys_id:
    description:
      - Required if I(action==patch) or I(action==delete).
    type: str
  resource:
    description:
      - The name of the table in which a record is to be created, updated or deleted from.
      - Mutually exclusive with C(api_path).
      - Require one of C(resource) or C(api_path)
    type: str
  api_path:
    version_added: "2.5.0"
    description:
      - The path of the service which a record is to be created, updated or deleted from.
      - Mutually exclusive with C(resource).
      - Require one of C(resource) or C(api_path).
    type: str
  action:
    description: The action to perform.
    type: str
    required: true
    choices:
      - post
      - patch
      - delete
  query_params:
    version_added: "2.1.0"
    description:
      - Query parameters that may be used on POST or PATCH request.
    type: dict
    default: {}
  data:
    description:
      - The data that we want to update or create the resource with.
      - Mutually exclusive with I(template).
      - Only relevant if I(action==patch) or I(action==post).
      - A Dict consists of resource's column names as keys (such as description, number, priority, and so on) and the
        patching values as values (the value we want to change the column to).
      - When updating a resource's record, if no datum is specified for a specific column, the value of that column will
        remain intact.
      - When creating a resource's record, if no datum is specified for a specific column, the default value of the
        column will be used.
    type: dict
    default: {}
  template:
    description:
      - Provide a valid YAML template definition file for creating or updating a record.
      - Provides built-in template processing capabilities as an alternative to its data parameter.
      - Mutually exclusive with I(data).
      - If template starts with C("/"), it is assumed you have specified absolute path to the file. Otherwise, it is assumed
        you have specified relative path to the file.
      - Template file needs to be present on the Ansible Controller's system. Otherwise, an error is raised.
    type: str
"""


EXAMPLES = """
- name: Create a record in table incident with specified short_description (which is read from data)
  servicenow.itsm.api:
    resource: incident
    action: post
    data:
      short_description: my-incident
  register: result

- name: Create a record in table incident with column values set in template, located in Ansible controller file system
  servicenow.itsm.api:
    resource: incident
    action: post
    template: '/testing/deployment.j2'
  register: result

- name: Update a record with given sys_id in table incident with template, located in Ansible controller file system
  servicenow.itsm.api:
    resource: incident
    action: patch
    sys_id: 46b66a40a9fe198101f243dfbc79033d
    template: '/testing/deployment.j2'
  register: result

- name: Update column short_description (specified in data) in table incident of a record with given sys_id
  servicenow.itsm.api:
    resource: incident
    action: patch
    sys_id: 46b66a40a9fe198101f243dfbc79033d
    data:
      short_description: my-incident-updated
  register: result

- name: Delete the resource the table incident with given sys_id
  servicenow.itsm.api:
    resource: incident
    action: delete
    sys_id: 46b66a40a9fe198101f243dfbc79033d
  register: result

- name: Create a record in the table sc_req_item and set short_description's value to demo-description2
  servicenow.itsm.api:
    resource: sc_req_item
    action: post
    data:
      short_description: demo-description2
  register: result

- name: Create a record in the table sc_req_item and set short_description's value to demo-description2
  servicenow.itsm.api:
    resource: sc_req_item
    action: post
    data:
      short_description: demo-description2
  register: result

- name: create user (object with encrypted fields)
  servicenow.itsm.api:
    resource: sys_user
    action: post
    query_params:
      sysparm_input_display_value: true
    data:
      user_name: "demo_username"
      user_password: "demo_password"
      first_name: "first_name"
      last_name: Demouser
      department: IT
      email: "demo_username@example.com"
      title: Demo user
  register: user

- name: Create a record in sc_req_item with column values set in template, located in Ansible controller file system
  servicenow.itsm.api:
    resource: sc_req_item
    action: post
    template: '/testing/deployment.j2'
  register: result

- name: Delete a record by sys_id from table sc_req_item
  servicenow.itsm.api:
    resource: sc_req_item
    action: delete
    sys_id: b82adae197201110949235dfe153afec
  register: result

- name: Create a record in cmdb service using api_path
  servicenow.itsm.api:
    api_path: api/now/cmdb/instance/cmdb_ci_linux_server
    action: post
    data:
      attributes:
        name: "linux99"
        firewall_status: "intranet"
    source: "ServiceNow"
"""

RETURN = """
record:
  description:
    - The created, updated or deleted record.
  returned: success
  type: dict
  sample:
    active: "true"
    activity_due: ""
    additional_assignee_list: ""
    approval: "not requested"
    approval_history: ""
    approval_set: ""
    assigned_to: ""
    assignment_group: ""
    business_duration: ""
    business_impact: ""
    business_service: ""
    business_stc: ""
    calendar_duration: ""
    calendar_stc: ""
    caller_id: ""
    category: "inquiry"
    cause: ""
    caused_by: ""
    child_incidents: "0"
    close_code: ""
    close_notes: ""
    closed_at: ""
    closed_by: ""
    cmdb_ci: ""
    comments: ""
    comments_and_work_notes: ""
    company: ""
    contact_type: ""
    contract: ""
    correlation_display: ""
    correlation_id: ""
    delivery_plan: ""
    delivery_task: ""
    description: ""
    due_date: ""
    escalation: "0"
    expected_start: ""
    follow_up: ""
    group_list: ""
    hold_reason: ""
    impact: "3"
    incident_state: "1"
    knowledge: "false"
    location: ""
    made_sla: "true"
    notify: "1"
    number: "INC0010204"
    opened_at: "2022-07-06 08:53:05"
    opened_by: "6816f79cc0a8016401c5a33be04be441"
    order: ""
    origin_id: ""
    origin_table: ""
    parent: ""
    parent_incident: ""
    priority: "5"
    problem_id: ""
    reassignment_count: "0"
    reopen_count: "0"
    reopened_by: ""
    reopened_time: ""
    resolved_at: ""
    resolved_by: ""
    rfc: ""
    route_reason: ""
    service_offering: ""
    severity: "3"
    short_description: "my-incident"
    sla_due: ""
    state: "1"
    subcategory: ""
    sys_class_name: "incident"
    sys_created_by: "admin"
    sys_created_on: "2022-07-06 08:53:05"
    sys_domain: "global"
    sys_domain_path: "/"
    sys_id: "35b5fb4197245110949235dfe153af06"
    sys_mod_count: "0"
    sys_tags: ""
    sys_updated_by: "admin"
    sys_updated_on: "2022-07-06 08:53:05"
    task_effective_number: "INC0010204"
    time_worked: ""
    universal_request: ""
    upon_approval: "proceed"
    upon_reject: "cancel"
    urgency: "3"
    user_input: ""
    watch_list: ""
    work_end: ""
    work_notes: ""
    work_notes_list: ""
    work_start: ""
"""

from ansible.module_utils.basic import AnsibleModule

from ..module_utils import arguments, client, errors, table, generic
from ..module_utils.api import (
    ACTION_DELETE,
    ACTION_PATCH,
    ACTION_POST,
    FIELD_DATA,
    FIELD_QUERY_PARAMS,
    FIELD_SYS_ID,
    FIELD_TEMPLATE,
    field_present,
    get_sys_id,
    resource_name,
)


def update_resource(module, client):
    record_old = client.get_record_by_sys_id(resource_name(module), get_sys_id(module))
    if record_old is None:
        return False, None, dict(before=None, after=None)
    record_new = client.update_record(
        resource_name(module),
        record=record_old,
        payload=module.params.get(FIELD_DATA, dict()),
        check_mode=module.check_mode,
        query=module.params.get(FIELD_QUERY_PARAMS, dict()),
    )
    return True, record_new, dict(before=record_old, after=record_new)


def create_resource(module, client):
    # At the moment, creating a resource is not idempotent (meaning: If a record with such data as specified in
    # module.params["data"] already exists, such resource will get created once again).
    new = client.create_record(
        resource_name(module),
        payload=module.params.get(FIELD_DATA, dict()),
        check_mode=module.check_mode,
        query=module.params.get(FIELD_QUERY_PARAMS, dict()),
    )
    return True, new, dict(before=None, after=new)


def delete_resource(module, client):
    record = client.get_record_by_sys_id(resource_name(module), get_sys_id(module))
    if record is None:
        return False, None, dict(before=None, after=None)
    client.delete_record(resource_name(module), record, module.check_mode)
    return True, None, dict(before=record, after=None)


def run(module, client):
    if module.params["action"] == ACTION_PATCH:  # PATCH method
        return update_resource(module, client)
    elif module.params["action"] == ACTION_POST:  # POST method
        if field_present(module, FIELD_SYS_ID):
            module.warn("For action create (post) sys_id is ignored.")
        return create_resource(module, client)
    return delete_resource(module, client)  # DELETE method


def main():
    arg_spec = dict(
        arguments.get_spec(
            "instance",
            "sys_id",  # necessary for deleting and patching a resource, not relevant if creating a resource
        ),
        resource=dict(type="str"),
        api_path=dict(type="str"),
        action=dict(
            type="str",
            required=True,
            choices=[
                ACTION_POST,  # create
                ACTION_PATCH,  # update
                ACTION_DELETE,  # delete
            ],
        ),
        query_params=dict(type="dict", default=dict()),
        data=dict(type="dict", default=dict()),
        template=dict(
            type="str",
        ),
    )

    module = AnsibleModule(
        supports_check_mode=True,
        argument_spec=arg_spec,
        mutually_exclusive=[(FIELD_DATA, FIELD_TEMPLATE), ("resource", "api_path")],
        required_one_of=[("resource", "api_path")],
        required_if=[
            ("action", "patch", ("sys_id",)),
            ("action", "delete", ("sys_id",)),
        ],
    )

    try:
        snow_client = client.Client(**module.params["instance"])

        if module.params["api_path"]:
            _client = generic.GenericClient(snow_client)
        else:
            _client = table.TableClient(snow_client)

        changed, record, diff = run(module, _client)
        module.exit_json(changed=changed, record=record, diff=diff)
    except errors.ServiceNowError as e:
        module.fail_json(msg=str(e))


if __name__ == "__main__":
    main()
