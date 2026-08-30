# -*- coding: utf-8 -*-
# Copyright: (c) 2022, XLAB Steampunk <steampunk@xlab.si>
#
# GNU General Public License v3.0+ (see COPYING or https://www.gnu.org/licenses/gpl-3.0.txt)

from __future__ import absolute_import, division, print_function

__metaclass__ = type


class ModuleDocFragment(object):
    DOCUMENTATION = r"""
options:
  problem_task_mapping:
    version_added: "1.3.0"
    description:
      - User mapping for I(Problem task) object, where user can override Choice Lists values for objects.
    type: dict
    suboptions:
      state:
        description:
          - State of problem tasks.
          - If I(state) value is C(new), I(short_description) parameter must be filled in.
        type: dict
      priority:
        description:
          - How quickly the service desk should address the problem task.
        type: dict
"""
