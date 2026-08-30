# -*- coding: utf-8 -*-
# Copyright: (c) 2021, XLAB Steampunk <steampunk@xlab.si>
#
# GNU General Public License v3.0+ (see COPYING or https://www.gnu.org/licenses/gpl-3.0.txt)

from __future__ import absolute_import, division, print_function

__metaclass__ = type


PAYLOAD_FIELDS_MAPPING = dict(
    environment=[
        ("", ""),  # field not set
        ("Development", "development"),
        ("Production", "production"),
        ("Test", "test"),
    ],
    install_status=[
        ("", ""),  # field not set
        ("0", "implementing"),
        ("1", "installed"),
        ("2", "on_order"),
        ("3", "in_maintenance"),
        ("4", "pending_install"),
        ("5", "pending_repair"),
        ("6", "in_stock"),
        ("7", "retired"),
        ("8", "stolen"),
        ("100", "absent"),
    ],
    operational_status=[
        ("", ""),  # field not set
        ("1", "operational"),
        ("2", "non_operational"),
        ("3", "repair_in_progress"),
        ("4", "dr_standby"),
        ("5", "ready"),
        ("6", "retired"),
        ("7", "pipeline"),
        ("8", "catalog"),
    ],
)
