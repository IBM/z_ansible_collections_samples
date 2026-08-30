# -*- coding: utf-8 -*-
# Copyright: (c) 2022, XLAB Steampunk <steampunk@xlab.si>
#
# GNU General Public License v3.0+ (see COPYING or https://www.gnu.org/licenses/gpl-3.0.txt)

from __future__ import absolute_import, division, print_function

__metaclass__ = type


class ModuleDocFragment(object):
    DOCUMENTATION = r"""
options:
  change_request_task_mapping:
    version_added: "1.3.0"
    description:
      - User mapping for I(Change request task) object, where user can override Choice Lists values for objects.
    type: dict
    suboptions:
      state:
        description:
          - The state of the change request task.
          - Cannot be changed to C(pending) when I(on_hold) is C(true)
            (module fails and does nothing).
        type: dict
"""
