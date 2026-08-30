#!/usr/bin/python
# -*- coding: utf-8 -*-
# Copyright: (c) 2021, XLAB Steampunk <steampunk@xlab.si>
#
# GNU General Public License v3.0+ (see COPYING or https://www.gnu.org/licenses/gpl-3.0.txt)

from __future__ import absolute_import, division, print_function

__metaclass__ = type

DOCUMENTATION = r"""
module: change_request_task

author:
  - Matej Pevec (@mysteriouswolf)
  - Manca Bizjak (@mancabizjak)
  - Miha Dolinar (@mdolin)
  - Tadej Borovsak (@tadeboro)

short_description: Manage ServiceNow change request tasks

description:
  - Create, delete or update a ServiceNow change request tasks.
  - For more information, refer to the ServiceNow change management documentation at
    U(https://docs.servicenow.com/bundle/tokyo-it-service-management/page/product/change-management/concept/c_ITILChangeManagement.html).
version_added: 1.3.0
extends_documentation_fragment:
  - servicenow.itsm.instance
  - servicenow.itsm.sys_id
  - servicenow.itsm.number
  - servicenow.itsm.change_request_task_mapping
seealso:
  - module: servicenow.itsm.change_request_task_info

options:
  configuration_item:
    description:
      - The configuration item (CI) or service name that the change task applies to.
      - Note that contrary to I(configuration_item_id), configuration item names may not uniquely identify a record.
        In case there are more configuration items with the same name, the module fails and does nothing.
      - Mutually exclusive with I(configuration_item_id).
    type: str
  configuration_item_id:
    description:
      - The configuration item (CI) or service ID that the change task applies to.
      - Mutually exclusive with I(configuration_item).
    type: str
  change_request_number:
    description:
      - I(number) of the change request this task belongs to.
      - Note that contrary to I(change_request_id), change request number may not uniquely identify a record.
        In case there are more change requests with the same number, the module fails and does nothing.
      - Mutually exclusive with I(change_request_id).
    type: str
  change_request_id:
    description:
      - I(sys_id) of the change request this task belongs to.
      - Mutually exclusive with I(change_request_number).
    type: str
  type:
    description:
      - The type of change task.
      - Default workflow generates tasks in I(type) C(review).
      - If the task I(type) is C(implementation), the I(planned_start_date) and I(planned_end_date) values
        must fall within the planned start and end dates specified in the I(change_request).
    choices: [ planning, implementation, testing, review ]
    type: str
  state:
    description:
      - The state of the change request task.
      - Cannot be changed to C(pending) when I(on_hold) is C(true)
        (module fails and does nothing).
      - Default choices are C(pending), C(open), C(in_progress), C(closed), C(canceled), C(absent).
        One can override them by setting I(change_request_task.state).
    type: str
  assigned_to:
    description:
      - The user that the change task is assigned to.
    type: str
  assignment_group:
    description:
      - The name of the group that the change task is assigned to.
      - Mutually exclusive with C(assignment_group_id).
    type: str
  assignment_group_id:
    version_added: '2.4.0'
    description:
      - The id of the group that the change task is assigned to.
      - Mutually exclusive with C(assignment_group).
    type: str
  short_description:
    description:
      - A summary of the task.
      - This field has to be set either in the record or here.
    type: str
  description:
    description:
      - A detailed description of the task.
      - This field has to be set either in the record or here.
    type: str
  on_hold:
    description:
      - A change task cannot be put on hold when I(state) is C(pending), C(canceled), or C(closed)
        (module fails and does nothing).
      - Provide an On hold reason if a change task is placed on hold.
    type: bool
  hold_reason:
    description:
      - Reason why change task is on hold.
      - Required if change task's I(on_hold) value will be C(true).
    type: str
  planned_start_date:
    description:
      - The date you plan to begin working on the task.
    type: str
  planned_end_date:
    description:
      - The date the change task is planned to be completed.
    type: str
  close_code:
    description:
      - Provide information on how the change task was resolved.
      - The change task must have this parameter set prior to
        transitioning to the C(closed) state.
    choices: [ successful, successful_issues, unsuccessful ]
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
        change task documentation at
        U(https://docs.servicenow.com/bundle/tokyo-it-service-management/page/product/change-management/task/create-a-change-task.html).
    type: dict
"""

EXAMPLES = """
- name: Create a change task
  servicenow.itsm.change_request_task:
    configuration_item: Rogue Squadron Launcher
    change_request_number: CHG0000001
    type: planning
    state: open
    assigned_to: fred.luddy
    assignment_group: robot.embedded
    short_description: Implement collision avoidance
    description: "Implement collision avoidance based on the newly installed TOF sensor arrays."
    on_hold: true
    hold_reason: "Waiting for a report from the hardware team"
    planned_start_date: 2021-07-15 08:00:00
    planned_end_date: 2021-07-21 16:00:00
    other:
      approval: approved

- name: Change state of the change task
  servicenow.itsm.change_request_task:
    state: in_progress
    on_hold: false
    number: CTASK0000001

- name: Close a change task
  servicenow.itsm.change_request_task:
    state: closed
    close_code: "successful"
    close_notes: "Closed"
    number: CTASK0000001

- name: Delete a change task
  servicenow.itsm.change_request_task:
    state: absent
    number: CTASK0000001
"""

from ..module_utils.utils import get_mapper
from ..module_utils.change_request_task import PAYLOAD_FIELDS_MAPPING
from ..module_utils import arguments, client, errors, table, utils, validation
from ansible.module_utils.basic import AnsibleModule

DIRECT_PAYLOAD_FIELDS = (
    "state",
    "short_description",
    "description",
    "on_hold",
    "planned_start_date",
    "planned_end_date",
    "close_code",
    "close_notes",
)


def ensure_absent(module, table_client):
    mapper = get_mapper(module, "change_request_task_mapping", PAYLOAD_FIELDS_MAPPING)
    query = utils.filter_dict(module.params, "sys_id", "number")
    task = table_client.get_record("change_task", query)

    if task:
        table_client.delete_record("change_task", task, module.check_mode)
        return True, None, dict(before=mapper.to_ansible(task), after=None)

    return False, None, dict(before=None, after=None)


def validate_params(params, change_task=None):
    missing = []
    if params["state"] == "closed":
        missing.extend(
            validation.missing_from_params_and_remote(
                ("close_code", "close_notes"), params, change_task
            )
        )

    # Description must be set
    missing.extend(
        validation.missing_from_params_and_remote(
            ("short_description", "description"), params, change_task
        )
    )

    if missing:
        raise errors.ServiceNowError(
            "Missing required parameters {0}".format(", ".join(missing))
        )


def ensure_present(module, table_client):
    mapper = get_mapper(module, "change_request_task_mapping", PAYLOAD_FIELDS_MAPPING)
    query = utils.filter_dict(module.params, "sys_id", "number")
    payload = build_payload(module, table_client)

    if not query:
        # User did not specify an existing change task, so we need to create a new one.
        validate_params(module.params)
        new = mapper.to_ansible(
            table_client.create_record(
                "change_task", mapper.to_snow(payload), module.check_mode
            )
        )
        return True, new, dict(before=None, after=new)

    old = mapper.to_ansible(
        table_client.get_record("change_task", query, must_exist=True)
    )
    if is_superset_with_date(old, payload):
        # No change in parameters we are interested in - nothing to do.
        return False, old, dict(before=old, after=old)

    validate_params(module.params, old)
    new = mapper.to_ansible(
        table_client.update_record(
            "change_task",
            mapper.to_snow(old),
            mapper.to_snow(payload),
            module.check_mode,
        )
    )
    return True, new, dict(before=old, after=new)


def is_superset_with_date(superset, candidate):
    datetime_fields = ("planned_start_date", "planned_end_date")

    for k in datetime_fields:
        if k in candidate and (superset.get(k, "") or "").replace("T", " ") != (
            candidate[k] or ""
        ).replace("T", " "):
            return False

    return utils.is_superset(
        superset, dict((k, v) for k, v in candidate.items() if k not in datetime_fields)
    )


def build_payload(module, table_client):
    payload = (module.params["other"] or {}).copy()
    payload.update(utils.filter_dict(module.params, *DIRECT_PAYLOAD_FIELDS))

    if module.params["type"]:
        payload["change_task_type"] = module.params["type"]

    if module.params["hold_reason"]:
        payload["on_hold_reason"] = module.params["hold_reason"]

    # Map the configuration item
    if module.params["configuration_item"]:
        configuration_item = table.find_configuration_item(
            table_client, module.params["configuration_item"]
        )
        payload["cmdb_ci"] = configuration_item["sys_id"]

    if module.params["configuration_item_id"]:
        payload["cmdb_ci"] = module.params["configuration_item_id"]

    # Map the change request
    if module.params["change_request_number"]:
        configuration_item = table.find_change_request(
            table_client, module.params["change_request_number"]
        )
        payload["change_request"] = configuration_item["sys_id"]

    if module.params["change_request_id"]:
        payload["change_request"] = module.params["change_request_id"]

    # Map the assignee
    if module.params["assigned_to"]:
        user = table.find_user(table_client, module.params["assigned_to"])
        payload["assigned_to"] = user["sys_id"]

    # Map the assigned group
    if module.params["assignment_group"]:
        assignment_group = table.find_assignment_group(
            table_client, module.params["assignment_group"]
        )
        payload["assignment_group"] = assignment_group["sys_id"]

    if module.params["assignment_group_id"]:
        payload["assignment_group"] = module.params["assignment_group"]

    return payload


def run(module, table_client):
    if module.params["state"] == "absent":
        return ensure_absent(module, table_client)
    return ensure_present(module, table_client)


def main():
    module_args = dict(
        arguments.get_spec(
            "instance", "sys_id", "number", "change_request_task_mapping"
        ),
        configuration_item=dict(
            type="str",
        ),
        configuration_item_id=dict(
            type="str",
        ),
        change_request_id=dict(
            type="str",
        ),
        change_request_number=dict(
            type="str",
        ),
        type=dict(
            type="str",
            choices=[
                "planning",
                "implementation",
                "testing",
                "review",
            ],
        ),
        state=dict(
            type="str",
        ),
        assigned_to=dict(
            type="str",
        ),
        assignment_group=dict(
            type="str",
        ),
        assignment_group_id=dict(
            type="str",
        ),
        short_description=dict(
            type="str",
        ),
        description=dict(
            type="str",
        ),
        on_hold=dict(
            type="bool",
        ),
        hold_reason=dict(
            type="str",
        ),
        planned_start_date=dict(
            type="str",
        ),
        planned_end_date=dict(
            type="str",
        ),
        close_code=dict(
            type="str",
            choices=[
                "successful",
                "successful_issues",
                "unsuccessful",
            ],
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
            ("on_hold", True, ("hold_reason",)),
        ],
        mutually_exclusive=[
            ("change_request_id", "change_request_number"),
            ("configuration_item_id", "configuration_item"),
            ("assignment_group", "assignment_group_id"),
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
