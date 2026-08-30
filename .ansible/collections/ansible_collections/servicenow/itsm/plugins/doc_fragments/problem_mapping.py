# -*- coding: utf-8 -*-
# Copyright: (c) 2022, XLAB Steampunk <steampunk@xlab.si>
#
# GNU General Public License v3.0+ (see COPYING or https://www.gnu.org/licenses/gpl-3.0.txt)

from __future__ import absolute_import, division, print_function

__metaclass__ = type


class ModuleDocFragment(object):
    DOCUMENTATION = r"""
options:
  problem_mapping:
    version_added: "1.3.0"
    description:
      - User mapping for I(Problem) object, where user can override Choice Lists values for objects.
    type: dict
    suboptions:
      state:
        description:
          - State of the problem.
          - If a problem does not yet exist, all states except for C(new)
            require setting of I(assigned_to) parameter.
          - Special value that can not be overridden is C(absent), which would remove a problem from ServiceNow.
        type: dict
      problem_state:
        description:
          - State of the problem.
          - If a problem does not yet exist, all states except for C(new)
            require setting of I(assigned_to) parameter.
          - This mapping can also be edited inside Choice Lists inside ServiceNow and can differ from state mapping.
        type: dict
      impact:
        description:
          - Effect that the problem has on business.
        type: dict
      urgency:
        description:
          - The extent to which the problem resolution can bear delay.
        type: dict
"""
