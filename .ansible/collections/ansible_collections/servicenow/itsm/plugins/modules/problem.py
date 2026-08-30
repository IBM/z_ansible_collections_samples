#!/usr/bin/python
# -*- coding: utf-8 -*-
# Copyright: (c) 2021, XLAB Steampunk <steampunk@xlab.si>
#
# GNU General Public License v3.0+ (see COPYING or https://www.gnu.org/licenses/gpl-3.0.txt)


from __future__ import absolute_import, division, print_function

__metaclass__ = type


DOCUMENTATION = r"""
module: problem

author:
  - Manca Bizjak (@mancabizjak)
  - Miha Dolinar (@mdolin)
  - Tadej Borovsak (@tadeboro)
  - Matej Pevec (@mysteriouswolf)
  - Uros Pascinski (@uscinski)

short_description: Manage ServiceNow problems

description:
  - Create, delete or update a ServiceNow problem.
  - For more information, refer to the ServiceNow problem management documentation at
    U(https://docs.servicenow.com/bundle/tokyo-it-service-management/page/product/problem-management/concept/c_ProblemManagement.html).
version_added: 1.0.0
extends_documentation_fragment:
  - servicenow.itsm.instance
  - servicenow.itsm.sys_id
  - servicenow.itsm.number
  - servicenow.itsm.attachments
  - servicenow.itsm.problem_mapping
seealso:
  - module: servicenow.itsm.problem_info
  - module: servicenow.itsm.problem_task
  - module: servicenow.itsm.problem_task_info

options:
  state:
    description:
      - State of the problem.
      - If a problem does not yet exist, all states except for C(new)
        require setting of I(assigned_to) parameter.
      - Default choices are C(new), C(assess), C(root_cause_analysis), C(fix_in_progress), C(resolved), C(closed), C(absent).
        One can override them by setting I(problem_mapping.state).
    type: str
  short_description:
    description:
      - Short description of the problem that the problem-solving team should address.
      - Required if the problem does not exist yet.
    type: str
  description:
    description:
      - Detailed description of the problem.
    type: str
  impact:
    description:
      - Effect that the problem has on business.
      - Default choices are C(low), C(medium), C(high).
        One can override them by setting I(problem_mapping.impact).
    type: str
  urgency:
    description:
      - The extent to which the problem resolution can bear delay.
      - Default choices are C(low), C(medium), C(high).
        One can override them by setting I(problem_mapping.urgency).
    type: str
  assigned_to:
    description:
      - A person who will assess this problem.
      - Expected value for I(assigned_to) is user id (usually in the form of
        C(first_name.last_name)).
      - This field is required when creating new problems for all problem
        I(state)s except C(new).
    type: str
  resolution_code:
    description:
      - The reason for problem resolution.
    choices: [ fix_applied, risk_accepted, duplicate, canceled ]
    type: str
  cause_notes:
    description:
      - Provide information on what caused the problem.
      - Required if I(state) is C(in_progress).
      - Required if I(state) is C(resolved) or C(closed) and I(resolution_code)
        is C(fix_applied) or C(risk_accepted).
    type: str
  close_notes:
    description:
      - The reason for closing the problem.
      - Required if I(state) is C(resolved) or C(closed) and I(resolution_code)
        is C(risk_accepted) or C(canceled).
    type: str
  fix_notes:
    description:
      - Notes on how the problem was fixed.
      - Required if I(state) is C(in_progress).
      - Required if I(state) is C(resolved) or C(closed) and I(resolution_code)
        is C(fix_applied).
    type: str
  duplicate_of:
    description:
      - Number of the problem of which this problem is a duplicate of.
      - Required if I(state) is C(resolved) or C(closed) and I(resolution_code)
        is C(duplicate).
    type: str
  other:
    description:
      - Optional remaining parameters.
      - For more information on optional parameters, refer to the ServiceNow
        documentation on creating problems at
        U(https://docs.servicenow.com/bundle/tokyo-it-service-management/page/product/problem-management/task/create-a-problem-v2.html).
    type: dict
  base_api_path:
    description:
      - Base API path for the ServiceNow problem state management scripted API.
      - Used for managing problem state transitions.
      - Requires I(API for Red Hat Ansible Automation Platform Certified Content Collection) application to be installed from the ServiceNow Store
        U(https://store.servicenow.com/sn_appstore_store.do#!/store/application/9b33c83a1bcc5510b76a0d0fdc4bcb21/1.0.0?sl=sh).
      - Considered mostly for development and testing purposes, as in most cases the default value should be fine.
      - Starting with release I(Rome), I(ServiceNow Table API) no longer supports problem state transitions, which is worked around by using
        this server-side scripted REST API resource.
    type: str
    default: /api/x_rhtpp_ansible/problem
    version_added: 2.0.0
"""

EXAMPLES = r"""
- name: Create a problem
  servicenow.itsm.problem:
    state: new
    short_description: Issue with the network printer
    description: Since this morning, all printer jobs are stuck.
    attachments:
      - path: path/to/attachment.txt
    impact: medium
    urgency: low
    other:
      user_input: notes

- name: Assign a problem to a user for assessment
  servicenow.itsm.problem:
    number: PRB0000010
    state: assess
    assigned_to: problem.manager

- name: Mark a problem for root cause analysis
  servicenow.itsm.problem:
    number: PRB0000010
    state: root_cause_analysis

- name: Work on fixing a problem
  servicenow.itsm.problem:
    number: PRB0000010
    state: fix_in_progress
    cause_notes: I identified the issue.
    fix_notes: Fix here.


- name: Close a problem as fixed
  servicenow.itsm.problem:
    number: PRB0000010
    state: closed
    resolution_code: fix_applied
    cause_notes: I found that this doesn't work.
    fix_notes: I solved it like this.

- name: Close a problem as duplicate
  servicenow.itsm.problem:
    number: PRB0000010
    state: closed
    resolution_code: duplicate
    duplicate_of: PRB0000001

- name: Cancel a problem
  servicenow.itsm.problem:
    number: PRB0000010
    state: closed
    resolution_code: canceled
    close_notes: The problem seems to have resolved itself.

- name: Delete a problem
  servicenow.itsm.problem:
    number: PRB0000010
    state: absent
"""

RETURN = r"""
record:
  description:
    - The problem record.
  returned: success
  type: dict
  sample:
    "active": "true"
    "activity_due": ""
    "additional_assignee_list": ""
    "approval": "not requested"
    "approval_history": ""
    "approval_set": ""
    "assigned_to": "73ab3f173b331300ad3cc9bb34efc4df"
    "assignment_group": ""
    "attachments":
      -  "average_image_color": ""
         "chunk_size_bytes": "700000"
         "compressed": "true"
         "content_type": "text/plain"
         "download_link": "https://www.example.com/api/now/attachment/31cdf4d50706301022f9ffa08c1ed07f/file"
         "file_name": "sample_file1.txt"
         "hash": "6f2b0dec698566114435a23f15dcac848a40e1fd3e0eda4afe24a663dda23f2e"
         "image_height": ""
         "image_width": ""
         "size_bytes": "210"
         "size_compressed": "206"
         "state": "pending"
         "sys_created_by": "admin"
         "sys_created_on": "2021-08-17 11:19:49"
         "sys_id": "31cdf4d50706301022f9ffa08c1ed07f"
         "sys_mod_count": "0"
         "sys_tags": ""
         "sys_updated_by": "admin"
         "sys_updated_on": "2021-08-17 11:19:49"
         "table_name": "problem"
         "table_sys_id": "6dcdb4d50706301022f9ffa08c1ed0fb"
    "business_duration": ""
    "business_service": ""
    "calendar_duration": ""
    "category": "software"
    "cause_notes": ""
    "close_notes": ""
    "closed_at": ""
    "closed_by": ""
    "cmdb_ci": "27d32778c0a8000b00db970eeaa60f16"
    "comments": ""
    "comments_and_work_notes": ""
    "company": ""
    "confirmed_at": ""
    "confirmed_by": ""
    "contact_type": ""
    "contract": ""
    "correlation_display": ""
    "correlation_id": ""
    "delivery_plan": ""
    "delivery_task": ""
    "description": "Unable to send or receive emails."
    "due_date": ""
    "duplicate_of": ""
    "escalation": "0"
    "expected_start": ""
    "first_reported_by_task": ""
    "fix_communicated_at": ""
    "fix_communicated_by": ""
    "fix_notes": ""
    "follow_up": ""
    "group_list": ""
    "impact": "low"
    "knowledge": "false"
    "known_error": "false"
    "location": ""
    "made_sla": "true"
    "major_problem": "false"
    "number": "PRB0007601"
    "opened_at": "2018-08-30 08:08:39"
    "opened_by": "6816f79cc0a8016401c5a33be04be441"
    "order": ""
    "parent": ""
    "priority": "5"
    "problem_state": "new"
    "reassignment_count": "0"
    "related_incidents": "0"
    "reopen_count": "0"
    "reopened_at": ""
    "reopened_by": ""
    "resolution_code": ""
    "resolved_at": ""
    "resolved_by": ""
    "review_outcome": ""
    "rfc": ""
    "route_reason": ""
    "service_offering": ""
    "short_description": "Unable to send or receive emails."
    "sla_due": ""
    "state": "new"
    "subcategory": "email"
    "sys_class_name": "problem"
    "sys_created_by": "admin"
    "sys_created_on": "2018-08-30 08:09:05"
    "sys_domain": "global"
    "sys_domain_path": "/"
    "sys_id": "62304320731823002728660c4cf6a7e8"
    "sys_mod_count": "1"
    "sys_tags": ""
    "sys_updated_by": "admin"
    "sys_updated_on": "2018-12-12 07:16:57"
    "task_effective_number": "PRB0007601"
    "time_worked": ""
    "universal_request": ""
    "upon_approval": "proceed"
    "upon_reject": "cancel"
    "urgency": "low"
    "user_input": ""
    "watch_list": ""
    "work_end": ""
    "work_notes": ""
    "work_notes_list": ""
    "work_start": ""
    "workaround": ""
    "workaround_applied": "false"
    "workaround_communicated_at": ""
    "workaround_communicated_by": ""
"""

from ansible.module_utils.basic import AnsibleModule

from ..module_utils import (
    arguments,
    attachment,
    client,
    errors,
    table,
    utils,
    validation,
)
from ..module_utils.problem import (
    ASSESS,
    CLOSED,
    FIX,
    NEW,
    PAYLOAD_FIELDS_MAPPING,
    RCA,
    RESOLVED,
    ProblemClient,
)
from ..module_utils.utils import get_mapper

DIRECT_PAYLOAD_FIELDS = (
    "state",
    "short_description",
    "description",
    "impact",
    "urgency",
    "resolution_code",
    "fix_notes",
    "cause_notes",
    "close_notes",
)


def ensure_absent(module, table_client, attachment_client):
    mapper = get_mapper(module, "problem_mapping", PAYLOAD_FIELDS_MAPPING)
    query = utils.filter_dict(module.params, "sys_id", "number")
    problem = table_client.get_record("problem", query)

    if problem:
        attachment_client.delete_attached_records(
            "problem",
            problem["sys_id"],
            module.check_mode,
        )
        table_client.delete_record("problem", problem, module.check_mode)
        return True, None, dict(before=mapper.to_ansible(problem), after=None)

    return False, None, dict(before=None, after=None)


def build_payload(sn_params, table_client):
    sn_payload = (sn_params["other"] or {}).copy()
    sn_payload.update(utils.filter_dict(sn_params, *DIRECT_PAYLOAD_FIELDS))

    if sn_params["assigned_to"]:
        user = table.find_user(table_client, sn_params["assigned_to"])
        sn_payload["assigned_to"] = user["sys_id"]
    if sn_params["duplicate_of"]:
        problem = table_client.get_record(
            "problem",
            query=dict(number=sn_params["duplicate_of"]),
            must_exist=True,
        )
        sn_payload["duplicate_of"] = problem["sys_id"]

    return sn_payload


def validate_params(sn_params, sn_problem=None):
    # Validation is compliant with the data policies described at
    # https://community.servicenow.com/sys_attachment_java.do?sys_id=6f37cbf0dbb87708fece0b55ca961902
    # If we do not enforce this, the server refuses to advance problem state and to update supplementary fields.
    state = sn_params["state"]
    missing = []
    if state in (NEW, ASSESS, RCA, FIX, RESOLVED, CLOSED):
        missing.extend(
            validation.missing_from_params_and_remote(
                ["short_description"], sn_params, sn_problem
            )
        )
    if state in (ASSESS, RCA, FIX, RESOLVED, CLOSED):
        missing.extend(
            validation.missing_from_params_and_remote(
                ["assigned_to"], sn_params, sn_problem
            )
        )
    if state in (RESOLVED, CLOSED):
        missing.extend(
            validation.missing_from_params_and_remote(
                ["resolution_code"], sn_params, sn_problem
            )
        )
    if state == FIX:
        missing.extend(
            validation.missing_from_params_and_remote(
                ["cause_notes", "fix_notes"], sn_params, sn_problem
            )
        )
    resolution_params = dict(
        fix_applied=["cause_notes", "fix_notes"],
        risk_accepted=["cause_notes", "close_notes"],
        canceled=["close_notes"],
        duplicate=["duplicate_of"],
    )
    if sn_params["resolution_code"]:
        missing.extend(
            validation.missing_from_params_and_remote(
                resolution_params[sn_params["resolution_code"]], sn_params, sn_problem
            )
        )

    if missing:
        raise errors.ServiceNowError(
            "Missing required parameters {0}".format(", ".join(missing))
        )


def ensure_present(module, problem_client, table_client, attachment_client):
    mapper = get_mapper(module, "problem_mapping", PAYLOAD_FIELDS_MAPPING)
    query = utils.filter_dict(module.params, "sys_id", "number")
    sn_params = mapper.to_snow(module.params)
    sn_payload = build_payload(sn_params, table_client)
    attachments = attachment.transform_metadata_list(
        module.params["attachments"], module.sha256
    )

    if not query:
        # User did not specify existing problem, so we need to create a new one.
        new = mapper.to_ansible(
            table_client.create_record("problem", sn_payload, module.check_mode)
        )

        # When we execute in check mode, new["sys_id"] is not defined.
        # In order to give users back as much info as possible, we fake the sys_id in the
        # next call.
        new["attachments"] = attachment_client.upload_records(
            "problem",
            new.get("sys_id", "N/A"),
            attachments,
            module.check_mode,
        )

        return True, new, dict(before=None, after=new)

    sn_old = table_client.get_record("problem", query, must_exist=True)

    sn_old["attachments"] = attachment_client.list_records(
        dict(table_name="problem", table_sys_id=sn_old["sys_id"])
    )

    old = mapper.to_ansible(sn_old)

    if utils.is_superset(sn_old, sn_payload) and not any(
        attachment.are_changed(old["attachments"], attachments)
    ):
        # No change in parameters we are interested in -- nothing to do.
        return False, old, dict(before=old, after=old)

    validate_params(sn_params, sn_old)
    sn_new = table_client.update_record(
        "problem", sn_old, sn_payload, module.check_mode
    )

    # Was the problem state advanced?
    # Starting with release Rome, Table API no longer transitions problem
    # state. Try to advance state with the ServiceNow Store app.
    if "state" in sn_payload and sn_new["state"] != sn_payload["state"]:
        # Does not happen when check_mode is True
        sn_new = problem_client.update_record(
            sn_new["number"],
            sn_payload,
        )

    new = mapper.to_ansible(sn_new)

    new["attachments"] = attachment_client.update_records(
        "problem",
        old["sys_id"],
        attachments,
        old["attachments"],
        module.check_mode,
    )

    return True, new, dict(before=old, after=new)


def run(module, problem_client, table_client, attachment_client):
    if module.params["state"] == "absent":
        return ensure_absent(module, table_client, attachment_client)
    return ensure_present(module, problem_client, table_client, attachment_client)


def main():
    module_args = dict(
        arguments.get_spec(
            "instance", "sys_id", "number", "attachments", "problem_mapping"
        ),
        state=dict(
            type="str",
        ),
        short_description=dict(
            type="str",
        ),
        description=dict(
            type="str",
        ),
        impact=dict(
            type="str",
        ),
        urgency=dict(
            type="str",
        ),
        assigned_to=dict(
            type="str",
        ),
        resolution_code=dict(
            type="str",
            choices=[
                "fix_applied",
                "risk_accepted",
                "duplicate",
                "canceled",
            ],
        ),
        cause_notes=dict(
            type="str",
        ),
        close_notes=dict(
            type="str",
        ),
        fix_notes=dict(
            type="str",
        ),
        duplicate_of=dict(
            type="str",
        ),
        other=dict(
            type="dict",
        ),
        base_api_path=dict(
            type="str",
            default="/api/x_rhtpp_ansible/problem",
        ),
    )

    module = AnsibleModule(
        argument_spec=module_args,
        supports_check_mode=True,
        required_if=[
            ("state", "absent", ("sys_id", "number"), True),
        ],
    )

    try:
        snow_client = client.Client(**module.params["instance"])
        table_client = table.TableClient(snow_client)
        attachment_client = attachment.AttachmentClient(snow_client)
        problem_client = ProblemClient(snow_client, module.params["base_api_path"])
        changed, record, diff = run(
            module, problem_client, table_client, attachment_client
        )
        module.exit_json(changed=changed, record=record, diff=diff)
    except errors.ServiceNowError as e:
        module.fail_json(msg=str(e))


if __name__ == "__main__":
    main()
