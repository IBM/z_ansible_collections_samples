# -*- coding: utf-8 -*-
# Copyright: (c) 2021, XLAB Steampunk <steampunk@xlab.si>
#
# GNU General Public License v3.0+ (see COPYING or https://www.gnu.org/licenses/gpl-3.0.txt)

from __future__ import absolute_import, division, print_function

__metaclass__ = type

from ansible.module_utils.basic import env_fallback

INCIDENT_MAPPING_SPEC = dict(
    type="dict",
    required=False,
    options=dict(
        state=dict(type="dict"),
        hold_reason=dict(type="dict"),
        impact=dict(type="dict"),
        urgency=dict(type="dict"),
        close_code=dict(type="dict"),
    ),
)

CHANGE_REQUEST_MAPPING_SPEC = dict(
    type="dict",
    required=False,
    options=dict(
        priority=dict(type="dict"),
        risk=dict(type="dict"),
        impact=dict(type="dict"),
        urgency=dict(type="dict"),
        state=dict(type="dict"),
        category=dict(type="dict"),
    ),
)


CHANGE_REQUEST_TASK_MAPPING_SPEC = dict(
    type="dict",
    required=False,
    options=dict(
        state=dict(type="dict"),
    ),
)


CONFIGURATION_ITEM_MAPPING_SPEC = dict(
    type="dict",
    required=False,
    options=dict(
        environment=dict(type="dict"),
        install_status=dict(type="dict"),
        operational_status=dict(type="dict"),
    ),
)

PROBLEM_MAPPING_SPEC = dict(
    type="dict",
    required=False,
    options=dict(
        impact=dict(type="dict"),
        urgency=dict(type="dict"),
        problem_state=dict(type="dict"),
        state=dict(type="dict"),
    ),
)

PROBLEM_TASK_MAPPING_SPEC = dict(
    type="dict",
    required=False,
    options=dict(
        state=dict(type="dict"),
        priority=dict(type="dict"),
    ),
)

SHARED_SPECS = dict(
    instance=dict(
        type="dict",
        apply_defaults=True,
        options=dict(
            host=dict(
                type="str",
                required=True,
                fallback=(env_fallback, ["SN_HOST"]),
            ),
            username=dict(
                type="str",
                fallback=(env_fallback, ["SN_USERNAME"]),
            ),
            password=dict(
                type="str",
                no_log=True,
                fallback=(env_fallback, ["SN_PASSWORD"]),
            ),
            grant_type=dict(
                type="str",
                choices=["password", "refresh_token"],
                fallback=(env_fallback, ["SN_GRANT_TYPE"]),
            ),
            api_path=dict(
                type="str",
                default="api/now",
            ),
            client_id=dict(
                type="str",
                fallback=(env_fallback, ["SN_CLIENT_ID"]),
            ),
            client_secret=dict(
                type="str",
                no_log=True,
                fallback=(env_fallback, ["SN_CLIENT_SECRET"]),
            ),
            custom_headers=dict(type="dict"),
            refresh_token=dict(
                type="str",
                no_log=True,
                fallback=(env_fallback, ["SN_REFRESH_TOKEN"]),
            ),
            access_token=dict(
                type="str",
                no_log=True,
                fallback=(env_fallback, ["SN_ACCESS_TOKEN"]),
            ),
            timeout=dict(
                type="float",
                fallback=(env_fallback, ["SN_TIMEOUT"]),
            ),
            validate_certs=dict(
                type="bool",
                default=True,
            ),
        ),
        required_together=[("client_id", "client_secret"), ("username", "password")],
        required_one_of=[("username", "refresh_token", "access_token")],
        mutually_exclusive=[
            ("username", "refresh_token", "access_token"),
            ("client_id", "access_token"),
            ("grant_type", "access_token"),
        ],
        required_if=[
            ("grant_type", "password", ("username", "password")),
            ("grant_type", "refresh_token", ("refresh_token",)),
        ],
    ),
    sys_id=dict(type="str"),
    number=dict(type="str"),
    query=dict(type="list", elements="dict"),
    sysparm_query=dict(
        type="str",
        fallback=(env_fallback, ["SN_SYSPARM_QUERY"]),
    ),
    attachments=dict(
        type="list",
        elements="dict",
        options=dict(
            path=dict(
                type="str",
                required=True,
            ),
            name=dict(
                type="str",
            ),
            type=dict(
                type="str",
            ),
        ),
    ),
    sysparm_display_value=dict(
        type="str",
        choices=[
            "true",
            "false",
            "all",
        ],
        default="false",
    ),
    incident_mapping=INCIDENT_MAPPING_SPEC,
    change_request_mapping=CHANGE_REQUEST_MAPPING_SPEC,
    change_request_task_mapping=CHANGE_REQUEST_TASK_MAPPING_SPEC,
    configuration_item_mapping=CONFIGURATION_ITEM_MAPPING_SPEC,
    problem_mapping=PROBLEM_MAPPING_SPEC,
    problem_task_mapping=PROBLEM_TASK_MAPPING_SPEC,
)


def get_spec(*param_names):
    return dict((p, SHARED_SPECS[p]) for p in param_names)
