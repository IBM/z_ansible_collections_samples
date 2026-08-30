# -*- coding: utf-8 -*-
# Copyright: (c) 2021, XLAB Steampunk <steampunk@xlab.si>
#
# GNU General Public License v3.0+ (see COPYING or https://www.gnu.org/licenses/gpl-3.0.txt)

from __future__ import absolute_import, division, print_function

__metaclass__ = type


PAYLOAD_FIELDS_MAPPING = dict(
    type=[("rca", "root_cause_analysis"), ("general", "general")],
    state=[
        ("151", "new"),
        ("152", "assess"),
        ("154", "work_in_progress"),
        ("157", "closed"),
    ],
    priority=[
        ("1", "critical"),
        ("2", "high"),
        ("3", "moderate"),
        ("4", "low"),
        ("5", "planning"),
    ],
)
