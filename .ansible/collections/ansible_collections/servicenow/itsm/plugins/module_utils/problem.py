# -*- coding: utf-8 -*-
# Copyright: (c) 2021, XLAB Steampunk <steampunk@xlab.si>
#
# GNU General Public License v3.0+ (see COPYING or https://www.gnu.org/licenses/gpl-3.0.txt)

from __future__ import absolute_import, division, print_function

from ansible_collections.servicenow.itsm.plugins.module_utils.utils import filter_dict

__metaclass__ = type

import re

NEW = "101"
ASSESS = "102"
RCA = "103"
FIX = "104"
RESOLVED = "106"
CLOSED = "107"

STATE_MAPPING = [
    (NEW, "new"),
    (ASSESS, "assess"),
    (RCA, "root_cause_analysis"),
    (FIX, "fix_in_progress"),
    (RESOLVED, "resolved"),
    (CLOSED, "closed"),
]

PAYLOAD_FIELDS_MAPPING = dict(
    impact=[("1", "high"), ("2", "medium"), ("3", "low")],
    urgency=[("1", "high"), ("2", "medium"), ("3", "low")],
    problem_state=STATE_MAPPING,
    state=STATE_MAPPING,
)

ALLOWED_FIELDS = [
    "assigned_to",
    "resolution_code",
    "short_description",
    "fix_notes",
    "cause_notes",
    "close_notes",
    "duplicate_of",
]


class ProblemClient:
    def __init__(self, client, base_api_path):
        self.client = client
        self.base_api_path = re.sub(r"/+", "/", "/{0}/".format(base_api_path))

    def update_record(self, problem_number, data):
        new_state = data["state"]
        path = "{0}{1}/new_state/{2}".format(
            self.base_api_path, problem_number, new_state
        )

        # Fields filtering is a restriction enforced by the store application.
        # State updates are only allowed with certain fields.
        filtered_data = filter_dict(data, *ALLOWED_FIELDS)

        return self.client.patch(
            path, filtered_data, query=dict(sysparm_exclude_reference_link=True)
        ).json["result"]
