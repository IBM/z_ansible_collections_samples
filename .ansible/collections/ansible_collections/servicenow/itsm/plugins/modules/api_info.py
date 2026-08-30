#!/usr/bin/python
# -*- coding: utf-8 -*-
# Copyright: (c) 2022, XLAB Steampunk <steampunk@xlab.si>
#
# GNU General Public License v3.0+ (see COPYING or https://www.gnu.org/licenses/gpl-3.0.txt)

from __future__ import absolute_import, division, print_function

__metaclass__ = type

DOCUMENTATION = r"""
module: api_info

author:
  - Tjaž Eržen (@tjazsch)

short_description: Manage ServiceNow GET requests
description:
  - Retrieve records via ServiceNow REST Table API for an arbitrary table.
  - For more information, refer to the ServiceNow REST Table API documentation at
    U(https://docs.servicenow.com/bundle/tokyo-application-development/page/integrate/inbound-rest/concept/c_RESTAPI.html).
version_added: 2.0.0
extends_documentation_fragment:
  - servicenow.itsm.instance
  - servicenow.itsm.sys_id.info
seealso:
  - module: servicenow.itsm.api
options:
  resource:
    description:
      - The name of the table in which a record is to be created, updated or deleted from.
      - Mutually exclusive with C(api_path).
      - Require one of C(resource) or C(api_path).
    type: str
  api_path:
    version_added: "2.5.0"
    description:
      - The path of the service which a record is to be created, updated or deleted from.
      - Mutually exclusive with C(resource).
      - Require one of C(resource) or C(api_path).
    type: str
  sysparm_query:
    description:
      - An encoded query string used to filter the results.
      - List of all possible operators and a guide on how to map them to form a query may be found at
        U(https://docs.servicenow.com/bundle/tokyo-platform-user-interface/page/use/common-ui-elements/reference/r_OpAvailableFiltersQueries.html).
        and U(https://developer.servicenow.com/dev.do#!/reference/api/tokyo/rest/c_TableAPI) under 'sysparm_query'.
    type: str
  display_value:
    description:
      - Return field display values C(true), actual values C(false), or both C(all).
      - Default value is set to C(false).
    type: str
    choices: ["true", "false", "all"]
    default: 'false'
  exclude_reference_link:
    description:
      - C(true) to exclude Table API links for reference fields.
      - The default is C(false).
    type: bool
    default: False
  columns:
    description:
      - List of fields/columns to return in the response.
    type: list
    elements: str
    default: []
  query_category:
    description:
      - Name of the query category to use for queries.
    type: str
  query_no_domain:
    description:
      - If set to C(true) to access data across domains if authorized.
      - Default is set to C(false).
    type: bool
    default: False
  no_count:
    description:
      - Do not execute a select count(*) on table.
      - Default is set to C(false).
    type: bool
    default: False
"""

EXAMPLES = """
- name: Retrieve all records from table incident
  servicenow.itsm.api_info:
    resource: incident
  register: result

- name: Retrieve a record with specified sys_id from the resource incident
  servicenow.itsm.api_info:
    resource: incident
    sys_id: 471bfbc7a9fe198101e77a3e10e5d47f
  register: result

- name: Retrieve all incidents with properties specified in a query
  servicenow.itsm.api_info:
    resource: incident
    sysparm_query: numberSTARTSWITHINC^ORnumberSTARTSWITHABC^state!=7^stateBETWEEN1@4^short_descriptionISNOTEMPTY
  register: result

- name: Retrieve all incidents with properties specified in a query, filtered by a few other parameters
  servicenow.itsm.api_info:
    resource: incident
    sysparm_query: numberSTARTSWITHINC^ORnumberSTARTSWITHABC^state!=7^stateBETWEEN1@4^short_descriptionISNOTEMPTY
    display_value: "true"
    exclude_reference_link: true
    columns:
      - state
      - number
      - sys_id
    query_no_domain: true
    no_count: false
  register: result

- name: Retrieve all linux servers
  servicenow.itsm.api_info:
    api_path: api/now/cmdb/instance/cmdb_ci_linux_server
"""

RETURN = r"""
records:
  description:
    - A list of records from the specified table.
  returned: success
  type: list
  sample:
    - active: "false"
      activity_due: ""
      additional_assignee_list: ""
      approval: not requested
      approval_history: ""
      approval_set: ""
      assigned_to: 5137153cc611227c000bbd1bd8cd2007
      assignment_group: 8a4dde73c6112278017a6a4baf547aa7
      business_duration: "1970-01-20 05:38:50"
      business_service: ""
      business_stc: "1661930"
      calendar_duration: "1970-03-21 20:38:50"
      calendar_stc: "6899930"
      caller_id: 681ccaf9c0a8016400b98a06818d57c7
      category: inquiry
      caused_by: ""
      child_incidents: ""
      close_code: Solved (Work Around)
      close_notes: Gave workaround
      closed_at: "2020-07-07 23:18:40"
      closed_by: 9ee1b13dc6112271007f9d0efdb69cd0
      cmdb_ci: ""
      comments: ""
      comments_and_work_notes: ""
      company: 31bea3d53790200044e0bfc8bcbe5dec
      contact_type: phone
      contract: ""
      correlation_display: ""
      correlation_id: ""
      delivery_plan: ""
      delivery_task: ""
      description: Noticing today that any time I send an email with an attachment, it
        takes at least 20 seconds to send.
      due_date: ""
      escalation: "0"
      expected_start: ""
      follow_up: ""
      group_list: ""
      hold_reason: ""
      impact: "1"
      incident_state: "7"
      knowledge: "false"
      location: ""
      made_sla: "false"
      notify: "1"
      number: INC0000013
      opened_at: "2020-07-06 23:15:58"
      opened_by: 9ee1b13dc6112271007f9d0efdb69cd0
      order: ""
      parent: ""
      parent_incident: ""
      priority: "1"
      problem_id: ""
      reassignment_count: "2"
      reopen_count: ""
      reopened_by: ""
      reopened_time: ""
      resolved_at: "2020-09-24 19:54:48"
      resolved_by: 6816f79cc0a8016401c5a33be04be441
      rfc: ""
      route_reason: ""
      service_offering: ""
      severity: "3"
      short_description: EMAIL is slow when an attachment is involved
      sla_due: ""
      state: "7"
      subcategory: ""
      sys_class_name: incident
      sys_created_by: don.goodliffe
      sys_created_on: "2020-07-07 23:18:07"
      sys_domain: global
      sys_domain_path: /
      sys_id: 46cebb88a9fe198101aee93734f9768b
      sys_mod_count: "5"
      sys_tags: ""
      sys_updated_by: VALUE_SPECIFIED_IN_NO_LOG_PARAMETER
      sys_updated_on: "2020-09-24 19:54:48"
      task_effective_number: INC0000013
      time_worked: ""
      universal_request: ""
      upon_approval: ""
      upon_reject: ""
      urgency: "1"
      user_input: ""
      watch_list: ""
      work_end: ""
      work_notes: ""
      work_notes_list: ""
      work_start: ""
"""

from ansible.module_utils.basic import AnsibleModule

from ..module_utils import arguments, client, errors, table, utils, generic
from ..module_utils.api import (
    FIELD_COLUMNS_NAME,
    POSSIBLE_FILTER_PARAMETERS,
    resource_name,
    transform_query_to_servicenow_query,
)


def run(module, client):
    search_dict = dict(module.params)
    columns = ",".join([field.lower() for field in module.params[FIELD_COLUMNS_NAME]])
    search_dict.update(columns=columns)
    query = utils.filter_dict(search_dict, *POSSIBLE_FILTER_PARAMETERS)
    servicenow_query = transform_query_to_servicenow_query(query)
    return client.list_records(resource_name(module), servicenow_query)


def main():
    arg_spec = dict(
        arguments.get_spec("instance", "sys_id"),
        resource=dict(type="str"),
        api_path=dict(type="str"),
        sysparm_query=dict(type="str"),
        display_value=dict(
            type="str",
            choices=["true", "false", "all"],
            default="false",
            # Return field display values (true), actual values (false), or both (all) (default: false)
        ),
        exclude_reference_link=dict(
            type="bool",
            default=False,  # to enforce False when this parameter is omitted from a playbook
        ),  # True to exclude Table API links for reference columns
        columns=dict(
            type="list", default=[], elements="str"
        ),  # A comma-separated list of fields to return in the response
        query_category=dict(
            type="str"
        ),  # Name of the query category (read replica category) to use for queries
        query_no_domain=dict(
            type="bool",
            default=False,  # to enforce False when this parameter is omitted from a playbook
        ),  # True to access data across domains if authorized (default: false)
        no_count=dict(
            type="bool",
            default=False,  # to enforce False when this parameter is omitted from a playbook
        ),  # Do not execute a select count(*) on table (default: false)
    )

    module = AnsibleModule(
        supports_check_mode=True,
        argument_spec=arg_spec,
        mutually_exclusive=[("resource", "api_path")],
        required_one_of=[("resource", "api_path")],
    )

    try:
        snow_client = client.Client(**module.params["instance"])

        if module.params["api_path"]:
            _client = generic.GenericClient(snow_client)
        else:
            _client = table.TableClient(snow_client)

        records = run(module, _client)
        module.exit_json(changed=False, record=records)
    except errors.ServiceNowError as e:
        module.fail_json(msg=str(e))


if __name__ == "__main__":
    main()
