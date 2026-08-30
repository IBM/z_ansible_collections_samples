#!/usr/bin/python
# -*- coding: utf-8 -*-
# Copyright: (c) 2021, XLAB Steampunk <steampunk@xlab.si>
#
# GNU General Public License v3.0+ (see COPYING or https://www.gnu.org/licenses/gpl-3.0.txt)


from __future__ import absolute_import, division, print_function

__metaclass__ = type


DOCUMENTATION = r"""
module: problem_task

author:
  - Manca Bizjak (@mancabizjak)
  - Miha Dolinar (@mdolin)
  - Tadej Borovsak (@tadeboro)

short_description: Manage ServiceNow problem tasks

description:
  - Create, delete or update ServiceNow problem tasks.
  - For more information, refer to the ServiceNow problem management documentation at
    U(https://docs.servicenow.com/bundle/tokyo-it-service-management/page/product/problem-management/concept/c_ProblemManagement.html).

version_added: 1.3.0

extends_documentation_fragment:
  - servicenow.itsm.instance
  - servicenow.itsm.sys_id
  - servicenow.itsm.number
  - servicenow.itsm.problem_task_mapping
seealso:
  - module: servicenow.itsm.problem_task_info
  - module: servicenow.itsm.problem
  - module: servicenow.itsm.problem_info

options:
  state:
    description:
      - State of problem tasks.
      - If I(state) value is C(new), I(short_description) parameter must be filled in.
      - Default choices are C(new), C(assess), C(work_in_progress), C(closed), C(absent).
        One can override them by setting I(problem_task_mapping.state).
    type: str
  type:
    description:
      - Read-only state. Determines whether the problem task is created specifically
        to investigate the cause of the problem or is a general task.
    choices: [ root_cause_analysis, general]
    type: str
  configuration_item:
    description:
      - Configuration item (CI) that the problem applies to. The CI class of the selected
        configuration item identifies the type of problem.
    type: str
  due_date:
    description:
      - Date within which the problem task should be completed.
    type: str
  source_problem:
    description:
      - Number of the problem for which the problem task is created.
    type: str
  priority:
    description:
      - How quickly the service desk should address the problem task.
      - Default choices are C(critical), C(high), C(moderate), C(low), C(planning).
        One can override them by setting I(problem_task_mapping.priority).
    type: str
  assignment_group:
    description:
      - Specific group to whom the problem task is assigned to.
    type: str
  assigned_to:
    description:
      - Specific problem analyst to whom the task is assigned to.
    type: str
  short_description:
    description:
      - Brief description of the problem task.
    type: str
  description:
    description:
      - Detailed description of the problem task.
    type: str
  close_code:
    description:
      - Provide information on how the change task was resolved.
      - The change task must have this parameter set prior to
        transitioning to the C(closed) state.
    choices: [ completed, canceled ]
    type: str
  close_notes:
    description:
      - Resolution notes added by the user who closed the change task.
      - The change task must have this parameter set prior to
        transitioning to the C(closed) state.
    type: str
  other:
    description:
      - Optional remaining parameters.
      - For more information on optional parameters, refer to the ServiceNow
        create problem task documentation at
        U(https://docs.servicenow.com/bundle/tokyo-it-service-management/page/product/problem-management/task/create-problem-task.html).
    type: dict
"""

EXAMPLES = r"""
- name: Create problem task
  servicenow.itsm.problem_task:
    instance:
      host: https://instance_id.service-now.com
      username: user
      password: pass

    state: new
    type: general
    source_problem: PRB0000001
    short_description: User is not receiving email
    description: User has been unable to receive email for the past 15 minutes
    priority: low

- name: Change state of the problem task
  servicenow.itsm.problem_task:
    instance:
      host: https://instance_id.service-now.com
      username: user
      password: pass

    number: PTASK0010005
    state: assess
    assigned_to: fred.luddy

- name: Close problem task
  servicenow.itsm.problem_task:
    instance:
      host: https://instance_id.service-now.com
      username: user
      password: pass

    state: closed
    number: PTASK0010005
    close_code: canceled
    close_notes: Closed

- name: Delete problem task
  servicenow.itsm.problem_task:
    instance:
      host: https://instance_id.service-now.com
      username: user
      password: pass

    state: absent
    number: PTASK0010005
"""

RETURN = r"""
record:
  description:
    - A list of problem records.
  returned: success
  type: dict
  sample:
      "active": "true"
      "activity_due": ""
      "additional_assignee_list": ""
      "approval": "not requested"
      "tranquilitybusiness_service": ""
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

from datetime import datetime

from ansible.module_utils.basic import AnsibleModule

from ..module_utils import arguments, client, errors, table, utils
from ..module_utils.problem_task import PAYLOAD_FIELDS_MAPPING
from ..module_utils.utils import get_mapper

DIRECT_PAYLOAD_FIELDS = (
    "state",
    "type",
    "configuration_item",
    "due_date",
    "source_problem",
    "priority",
    "assignment_group",
    "assigned_to",
    "short_description",
    "description",
    "close_notes",
)


def ensure_absent(module, table_client):
    mapper = get_mapper(module, "problem_task_mapping", PAYLOAD_FIELDS_MAPPING)
    query = utils.filter_dict(module.params, "sys_id", "number")
    problem_task = table_client.get_record("problem_task", query)

    if problem_task:
        table_client.delete_record("problem_task", problem_task, module.check_mode)
        return True, None, dict(before=mapper.to_ansible(problem_task), after=None)

    return False, None, dict(before=None, after=None)


def build_payload(module, table_client):
    payload = (module.params["other"] or {}).copy()
    payload.update(utils.filter_dict(module.params, *DIRECT_PAYLOAD_FIELDS))

    if module.params["configuration_item"]:
        configuration_item = table.find_configuration_item(
            table_client, module.params["configuration_item"]
        )
        payload["cmdb_ci"] = configuration_item["sys_id"]

    if module.params["source_problem"]:
        problem = table.find_problem(table_client, module.params["source_problem"])
        payload["problem"] = problem["sys_id"]

    if module.params["assignment_group"]:
        assignment_group = table.find_assignment_group(
            table_client, module.params["assignment_group"]
        )
        payload["assignment_group"] = assignment_group["sys_id"]

    if module.params["assigned_to"]:
        user = table.find_user(table_client, module.params["assigned_to"])
        payload["assigned_to"] = user["sys_id"]

    if module.params["state"] == "work_in_progress":
        started_by = table.find_user(table_client, module.params["assigned_to"])
        payload["started_by"] = started_by["sys_id"]
        payload["started_at"] = datetime.now().strftime("%Y-%m-%d %H:%M:%S")

    return payload


def ensure_present(module, table_client):
    mapper = get_mapper(module, "problem_task_mapping", PAYLOAD_FIELDS_MAPPING)
    query = utils.filter_dict(module.params, "sys_id", "number")
    payload = build_payload(module, table_client)

    if not query:
        # User did not specify existing problem, so we need to create a new one.
        new = mapper.to_ansible(
            table_client.create_record(
                "problem_task", mapper.to_snow(payload), module.check_mode
            )
        )
        return True, new, dict(before=None, after=new)

    old = mapper.to_ansible(
        table_client.get_record("problem_task", query, must_exist=True)
    )
    if utils.is_superset(old, payload):
        # No change in parameters we are interested in - nothing to do.
        return False, old, dict(before=old, after=old)

    new = mapper.to_ansible(
        table_client.update_record(
            "problem_task",
            mapper.to_snow(old),
            mapper.to_snow(payload),
            module.check_mode,
        )
    )

    return True, new, dict(before=old, after=new)


def run(module, table_client):
    if module.params["state"] == "absent":
        return ensure_absent(module, table_client)
    return ensure_present(module, table_client)


def main():
    module_args = dict(
        arguments.get_spec("instance", "sys_id", "number", "problem_task_mapping"),
        state=dict(
            type="str",
        ),
        type=dict(
            choices=[
                "root_cause_analysis",
                "general",
            ],
            type="str",
        ),
        configuration_item=dict(
            type="str",
        ),
        due_date=dict(
            type="str",
        ),
        source_problem=dict(
            type="str",
        ),
        priority=dict(
            type="str",
        ),
        assignment_group=dict(
            type="str",
        ),
        assigned_to=dict(
            type="str",
        ),
        short_description=dict(
            type="str",
        ),
        description=dict(
            type="str",
        ),
        close_code=dict(
            type="str",
            choices=["completed", "canceled"],
        ),
        close_notes=dict(
            type="str",
        ),
        other=dict(
            type="dict",
        ),
    )

    module = AnsibleModule(
        argument_spec=module_args,
        supports_check_mode=True,
        required_if=[
            ("state", "absent", ("sys_id", "number"), True),
            ("state", "new", ("short_description",)),
            ("state", "assess", ("assigned_to",)),
        ],
    )

    try:
        snow_client = client.Client(**module.params["instance"])
        table_client = table.TableClient(snow_client)
        changed, record, diff = run(module, table_client)
        module.exit_json(changed=changed, record=record, diff=diff)
    except errors.ServiceNowError as e:
        module.fail_json(msg=str(e))


if __name__ == "__main__":
    main()
