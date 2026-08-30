#!/usr/bin/python
# -*- coding: utf-8 -*-
# Copyright: (c) 2021, XLAB Steampunk <steampunk@xlab.si>
#
# GNU General Public License v3.0+ (see COPYING or https://www.gnu.org/licenses/gpl-3.0.txt)


from __future__ import absolute_import, division, print_function

__metaclass__ = type


DOCUMENTATION = r"""
module: incident

author:
  - Manca Bizjak (@mancabizjak)
  - Miha Dolinar (@mdolin)
  - Tadej Borovsak (@tadeboro)
  - Matej Pevec (@mysteriouswolf)

short_description: Manage ServiceNow incidents

description:
  - Create, delete or update a ServiceNow incident.
  - For more information, refer to the ServiceNow incident management documentation at
    U(https://docs.servicenow.com/bundle/tokyo-it-service-management/page/product/incident-management/concept/c_IncidentManagement.html).
version_added: 1.0.0
extends_documentation_fragment:
  - servicenow.itsm.instance
  - servicenow.itsm.sys_id
  - servicenow.itsm.number
  - servicenow.itsm.attachments
  - servicenow.itsm.incident_mapping
options:
  state:
    description:
      - State of incident.
      - If I(state) value is C(on_hold), I(on_hold_reason) parameter must be filled in.
      - Default choices are C(new), C(in_progress), C(on_hold), C(resolved), C(closed), C(canceled), C(absent).
        One can override them by setting I(incident_mapping.state).
    type: str
  hold_reason:
    description:
      - Reason why incident is on hold.
      - Required if I(state) value is C(on_hold).
      - Default choices are C(awaiting_caller), C(awaiting_change), C(awaiting_problem), C(awaiting_vendor).
        One can override them by setting I(incident_mapping.hold_reason).
    type: str
  caller:
    description:
      - A person who reported or is affected by this incident.
      - Expected value for I(caller) is user id (usually in the form of
        C(first_name.last_name)).
      - Required if the incident does not exist yet.
    type: str
  short_description:
    description:
      - Short description of the incident.
      - Required if the incident does not exist yet.
    type: str
  description:
    description:
      - Long description of the incident with some more details.
    type: str
  impact:
    description:
      - The measure of the business criticality of the affected service.
      - Default choices are C(low), C(medium), C(high).
        One can override them by setting I(incident_mapping.impact).
    type: str
  urgency:
    description:
      - The extent to which resolution of an incident can bear delay.
      - Default choices are C(low), C(medium), C(high).
        One can override them by setting I(incident_mapping.urgency).
    type: str
  close_code:
    description:
      - Provide information on how the incident was resolved.
      - Default choices are C(Solved (Work Around)), C(Solved (Permanently)),
        C(Solved Remotely (Work Around)), C(Solved Remotely (Permanently)),
        C(Not Solved (Not Reproducible)), C(Not Solved (Too Costly)),
        C(Closed/Resolved by Caller).
        One can override them by setting I(incident_mapping.close_code).
      - Choices for I(close_code) removed in 2.4.0.
    type: str
  close_notes:
    description:
      - Resolution notes added by the user who closed the incident.
    type: str
  other:
    description:
      - Optional remaining parameters.
      - For more information on optional parameters, refer to the ServiceNow
        create incident documentation at
        U(https://docs.servicenow.com/bundle/tokyo-it-service-management/page/product/incident-management/task/create-an-incident.html).
    type: dict
"""

EXAMPLES = r"""
- name: Create incident
  servicenow.itsm.incident:
    instance:
      host: https://instance_id.service-now.com
      username: user
      password: pass

    state: new
    caller: some.user
    short_description: User is not receiving email
    description: User has been unable to receive email for the past 15 minutes
    attachments:
      - path: path/to/attachment.txt
    impact: low
    urgency: low

    other:
      expected_start: 2021-02-12

- name: Change state of the incident
  servicenow.itsm.incident:
    instance:
      host: https://instance_id.service-now.com
      username: user
      password: pass

    state: in_progress
    number: INC0000001

- name: Close incident
  servicenow.itsm.incident:
    instance:
      host: https://instance_id.service-now.com
      username: user
      password: pass

    state: closed
    number: INC0000001
    close_code: "Solved (Permanently)"
    close_notes: "Closed"

- name: Delete incident
  servicenow.itsm.incident:
    instance:
      host: https://instance_id.service-now.com
      username: user
      password: pass

    state: absent
    number: INC0000001
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
from ..module_utils.incident import PAYLOAD_FIELDS_MAPPING
from ..module_utils.utils import get_mapper

DIRECT_PAYLOAD_FIELDS = (
    "state",
    "hold_reason",
    "short_description",
    "description",
    "impact",
    "urgency",
    "close_code",
    "close_notes",
)


def ensure_absent(module, table_client, attachment_client):
    mapper = get_mapper(module, "incident_mapping", PAYLOAD_FIELDS_MAPPING)
    query = utils.filter_dict(module.params, "sys_id", "number")
    incident = table_client.get_record("incident", query)

    if incident:
        attachment_client.delete_attached_records(
            "incident",
            incident["sys_id"],
            module.check_mode,
        )
        table_client.delete_record("incident", incident, module.check_mode)
        return True, None, dict(before=mapper.to_ansible(incident), after=None)

    return False, None, dict(before=None, after=None)


def build_payload(module, table_client):
    payload = (module.params["other"] or {}).copy()
    payload.update(utils.filter_dict(module.params, *DIRECT_PAYLOAD_FIELDS))

    if module.params["caller"]:
        user = table.find_user(table_client, module.params["caller"])
        payload["caller_id"] = user["sys_id"]

    return payload


def validate_params(params, incident=None):
    missing = []
    if params["state"] in ("resolved", "closed"):
        missing.extend(
            validation.missing_from_params_and_remote(
                ("close_code", "close_notes"), params, incident
            )
        )

    if missing:
        raise errors.ServiceNowError(
            "Missing required parameters {0}".format(", ".join(missing))
        )


def ensure_present(module, table_client, attachment_client):
    mapper = get_mapper(module, "incident_mapping", PAYLOAD_FIELDS_MAPPING)

    query = utils.filter_dict(module.params, "sys_id", "number")
    payload = build_payload(module, table_client)
    attachments = attachment.transform_metadata_list(
        module.params["attachments"], module.sha256
    )

    if not query:
        # User did not specify existing incident, so we need to create a new one.
        validate_params(module.params)
        new = mapper.to_ansible(
            table_client.create_record(
                "incident", mapper.to_snow(payload), module.check_mode
            )
        )

        # When we execute in check mode, new["sys_id"] is not defined.
        # In order to give users back as much info as possible, we fake the sys_id in the
        # next call.
        new["attachments"] = attachment_client.upload_records(
            "incident",
            new.get("sys_id", "N/A"),
            attachments,
            module.check_mode,
        )

        return True, new, dict(before=None, after=new)

    old = mapper.to_ansible(table_client.get_record("incident", query, must_exist=True))

    old["attachments"] = attachment_client.list_records(
        dict(table_name="incident", table_sys_id=old["sys_id"])
    )

    if utils.is_superset(old, payload) and not any(
        attachment.are_changed(old["attachments"], attachments)
    ):
        # No change in parameters we are interested in - nothing to do.
        return False, old, dict(before=old, after=old)

    validate_params(module.params, old)
    new = mapper.to_ansible(
        table_client.update_record(
            "incident", mapper.to_snow(old), mapper.to_snow(payload), module.check_mode
        )
    )
    new["attachments"] = attachment_client.update_records(
        "incident",
        old["sys_id"],
        attachments,
        old["attachments"],
        module.check_mode,
    )

    return True, new, dict(before=old, after=new)


def run(module, table_client, attachment_client):
    if module.params["state"] == "absent":
        return ensure_absent(module, table_client, attachment_client)
    return ensure_present(module, table_client, attachment_client)


def main():
    module_args = dict(
        arguments.get_spec(
            "instance", "sys_id", "number", "attachments", "incident_mapping"
        ),
        state=dict(
            type="str",
        ),
        hold_reason=dict(
            type="str",
        ),
        caller=dict(
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
        close_code=dict(
            type="str",
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
            ("state", "on_hold", ("hold_reason",)),
        ],
        # If there is no sys id or number create a new ticket
    )

    try:
        snow_client = client.Client(**module.params["instance"])
        table_client = table.TableClient(snow_client)
        attachment_client = attachment.AttachmentClient(snow_client)
        changed, record, diff = run(module, table_client, attachment_client)
        module.exit_json(changed=changed, record=record, diff=diff)
    except errors.ServiceNowError as e:
        module.fail_json(msg=str(e))


if __name__ == "__main__":
    main()
