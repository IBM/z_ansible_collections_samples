#!/usr/bin/python
# -*- coding: utf-8 -*-
# Copyright: (c) 2021, XLAB Steampunk <steampunk@xlab.si>
#
# GNU General Public License v3.0+ (see COPYING or https://www.gnu.org/licenses/gpl-3.0.txt)


from __future__ import absolute_import, division, print_function

__metaclass__ = type

DOCUMENTATION = r"""
module: problem_task_info

author:
  - Manca Bizjak (@mancabizjak)
  - Miha Dolinar (@mdolin)
  - Tadej Borovsak (@tadeboro)
short_description: List ServiceNow problem tasks
description:
  - Retrieve information about ServiceNow problem tasks.
  - For more information, refer to the ServiceNow problem management documentation at
    U(https://docs.servicenow.com/bundle/tokyo-it-service-management/page/product/problem-management/concept/c_ProblemManagement.html).
version_added: 1.3.0
extends_documentation_fragment:
  - servicenow.itsm.instance
  - servicenow.itsm.sys_id.info
  - servicenow.itsm.number.info
  - servicenow.itsm.query
  - servicenow.itsm.sysparm_display_value
seealso:
  - module: servicenow.itsm.problem_task
  - module: servicenow.itsm.problem
  - module: servicenow.itsm.problem_info
"""

EXAMPLES = r"""
- name: Retrieve all problem tasks
  servicenow.itsm.problem_task_info:
  register: result

- name: Retrieve a specific problem task by its sys_id
  servicenow.itsm.problem_task_info:
    sys_id: 471bfbc7a9fe198101e77a3e10e5d47f
  register: result

- name: Retrieve problem tasks by number
  servicenow.itsm.problem_task_info:
    number: PTASK0007601
  register: result

- name: Retrieve problem tasks that do not contain SAP in its short description by using field query
  servicenow.itsm.problem_task_info:
    query:
      - short_description: NOT LIKE SAP
  register: result

- name: Retrieve problem tasks that do not contain SAP in its short description by using field sysparm_query
  servicenow.itsm.problem_task_info:
    sysparm_query: short_descriptionNOT LIKESAP
  register: result

- name: Retrieve new problem tasks assigned to abel.tuter or bertie.luby
  servicenow.itsm.problem_task_info:
    query:
      - state: = new
        assigned_to: = abel.tuter
      - state: = new
        assigned_to: = bertie.luby
"""

RETURN = r"""
records:
  description:
    - A list of problem records.
  returned: success
  type: list
  sample:
    - "active": "true"
      "activity_due": ""
      "additional_assignee_list": ""
      "approval": "not requested"
      "approval_history": ""
      "approval_set": ""
      "assigned_to": "7e3bbb173b331300ad3cc9bb34efc4a8"
      "assignment_group": ""
      "business_duration": ""
      "business_service": ""
      "calendar_duration": ""
      "cause_code": ""
      "cause_notes": ""
      "close_code": ""
      "close_notes": ""
      "closed_at": ""
      "closed_by": ""
      "cmdb_ci": "26da329f0a0a0bb400f69d8159bc753d"
      "comments": ""
      "comments_and_work_notes": ""
      "company": ""
      "contact_type": ""
      "contract": ""
      "correlation_display": ""
      "correlation_id": ""
      "delivery_plan": ""
      "delivery_task": ""
      "description": ""
      "due_date": ""
      "escalation": "0"
      "expected_start": ""
      "fix_notes": ""
      "follow_up": ""
      "group_list": ""
      "impact": "low"
      "knowledge": "false"
      "location": ""
      "made_sla": "true"
      "number": "PTASK0010005"
      "opened_at": "2020-12-17 10:21:49"
      "opened_by": "d3dbbf173b331300ad3cc9bb34efc466"
      "order": ""
      "other_reason": ""
      "parent": ""
      "priority": "2"
      "problem": "d7296d02c0a801670085e737da016e70"
      "problem_task_type": "rca"
      "reassignment_count": "0"
      "reopen_count": "1"
      "reopened_at": "2020-12-17 10:23:10"
      "reopened_by": "6816f79cc0a8016401c5a33be04be441"
      "route_reason": ""
      "service_offering": ""
      "short_description": "SAP outage, please investigate the cause"
      "sla_due": ""
      "started_at": "2020-12-17 10:23:14"
      "started_by": "6816f79cc0a8016401c5a33be04be441"
      "state": "154"
      "sys_class_name": "problem_task"
      "sys_created_by": "admin"
      "sys_created_on": "2020-12-17 10:22:25"
      "sys_domain": "global"
      "sys_domain_path": "/"
      "sys_id": "5f6bec57531063004247ddeeff7b1216"
      "sys_mod_count": "5"
      "sys_tags": ""
      "sys_updated_by": "admin"
      "sys_updated_on": "2020-12-17 10:27:14"
      "task_effective_number": "PTASK0010005"
      "time_worked": ""
      "universal_request": ""
      "upon_approval": "proceed"
      "upon_reject": "cancel"
      "urgency": "low"
      "user_input": ""
      "vendor": ""
      "watch_list": ""
      "work_end": ""
      "work_notes": ""
      "work_notes_list": ""
      "work_start": ""
      "workaround": ""
"""

from ansible.module_utils.basic import AnsibleModule

from ..module_utils import arguments, client, errors, query, table, utils
from ..module_utils.problem_task import PAYLOAD_FIELDS_MAPPING
from ..module_utils.utils import get_mapper


def remap_params(query, table_client):
    query_load = []

    for item in query:
        q = dict()
        for k, v in item.items():
            if k == "assigned_to":
                user = table.find_user(table_client, v[1])
                q["assigned_to"] = (v[0], user["sys_id"])

            elif k == "duplicate_of":
                problem = table_client.get_record(
                    "problem", query=dict(number=v[1]), must_exist=True
                )
                q["duplicate_of"] = (v[0], problem["sys_id"])

            else:
                q[k] = v

        query_load.append(q)

    return query_load


def sysparms_query(module, table_client, mapper):
    parsed, err = query.parse_query(module.params["query"])
    if err:
        raise errors.ServiceNowError(err)

    remap_query = remap_params(parsed, table_client)

    return query.serialize_query(query.map_query_values(remap_query, mapper))


def run(module, table_client):
    mapper = get_mapper(
        module,
        "problem_task_mapping",
        PAYLOAD_FIELDS_MAPPING,
        sysparm_display_value=module.params["sysparm_display_value"],
    )

    if module.params["query"]:
        query = {
            "sysparm_query": sysparms_query(module, table_client, mapper),
            "sysparm_display_value": module.params["sysparm_display_value"],
        }
    elif module.params["sysparm_query"]:
        query = {"sysparm_query": module.params["sysparm_query"]}
    else:
        query = utils.filter_dict(
            module.params, "sys_id", "number", "sysparm_display_value"
        )

    return [
        mapper.to_ansible(record)
        for record in table_client.list_records("problem_task", query)
    ]


def main():
    module = AnsibleModule(
        supports_check_mode=True,
        argument_spec=dict(
            arguments.get_spec(
                "instance",
                "sys_id",
                "number",
                "query",
                "sysparm_query",
                "sysparm_display_value",
            ),
        ),
        mutually_exclusive=[
            ("sys_id", "query"),
            ("number", "query"),
            ("sysparm_query", "query"),
            ("sys_id", "sysparm_query"),
            ("number", "sysparm_query"),
        ],
    )

    try:
        snow_client = client.Client(**module.params["instance"])
        table_client = table.TableClient(snow_client)
        records = run(module, table_client)
        module.exit_json(changed=False, records=records)
    except errors.ServiceNowError as e:
        module.fail_json(msg=str(e))


if __name__ == "__main__":
    main()
