#!/usr/bin/python
# -*- coding: utf-8 -*-
# Copyright: (c) 2021, XLAB Steampunk <steampunk@xlab.si>
#
# GNU General Public License v3.0+ (see COPYING or https://www.gnu.org/licenses/gpl-3.0.txt)


from __future__ import absolute_import, division, print_function

__metaclass__ = type

DOCUMENTATION = r"""
module: problem_info

author:
  - Manca Bizjak (@mancabizjak)
  - Miha Dolinar (@mdolin)
  - Tadej Borovsak (@tadeboro)
  - Matej Pevec (@mysteriouswolf)
short_description: List ServiceNow problems
description:
  - Retrieve information about ServiceNow problems.
  - For more information, refer to the ServiceNow problem management documentation at
    U(https://docs.servicenow.com/bundle/tokyo-it-service-management/page/product/problem-management/concept/c_ProblemManagement.html).
version_added: 1.0.0
extends_documentation_fragment:
  - servicenow.itsm.instance
  - servicenow.itsm.sys_id.info
  - servicenow.itsm.number.info
  - servicenow.itsm.query
  - servicenow.itsm.sysparm_display_value
seealso:
  - module: servicenow.itsm.problem
  - module: servicenow.itsm.problem_task
  - module: servicenow.itsm.problem_task_info
"""

EXAMPLES = r"""
- name: Retrieve all problems
  servicenow.itsm.problem_info:
  register: result

- name: Retrieve a specific problem by its sys_id
  servicenow.itsm.problem_info:
    sys_id: 471bfbc7a9fe198101e77a3e10e5d47f
  register: result

- name: Retrieve problems by number
  servicenow.itsm.problem_info:
    number: PRB0007601
  register: result

- name: Retrieve problems that do not contain SAP in its short description by using field query
  servicenow.itsm.problem_info:
    query:
      - short_description: NOT LIKE SAP
  register: result

- name: Retrieve problems that do not contain SAP in its short description by using field sysparm_query
  servicenow.itsm.problem_info:
    sysparm_query: short_descriptionNOT LIKESAP
  register: result

- name: Retrieve new problems assigned to abel.tuter or bertie.luby
  servicenow.itsm.problem_info:
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

from ..module_utils import arguments, attachment, client, errors, query, table, utils
from ..module_utils.problem import PAYLOAD_FIELDS_MAPPING
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


def run(module, table_client, attachment_client):
    mapper = get_mapper(
        module,
        "problem_mapping",
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
        dict(
            mapper.to_ansible(record),
            attachments=attachment_client.list_records(
                dict(table_name="problem", table_sys_id=record["sys_id"]),
            ),
        )
        for record in table_client.list_records("problem", query)
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
                "sysparm_display_value",
                "sysparm_query",
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
        attachment_client = attachment.AttachmentClient(snow_client)
        records = run(module, table_client, attachment_client)
        module.exit_json(changed=False, records=records)
    except errors.ServiceNowError as e:
        module.fail_json(msg=str(e))


if __name__ == "__main__":
    main()
