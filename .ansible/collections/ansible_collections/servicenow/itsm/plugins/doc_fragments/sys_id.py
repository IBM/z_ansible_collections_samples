# -*- coding: utf-8 -*-
# Copyright: (c) 2021, XLAB Steampunk <steampunk@xlab.si>
#
# GNU General Public License v3.0+ (see COPYING or https://www.gnu.org/licenses/gpl-3.0.txt)

from __future__ import absolute_import, division, print_function

__metaclass__ = type


class ModuleDocFragment(object):
    DOCUMENTATION = r"""
options:
  sys_id:
    description:
      - Unique identifier of the record to operate on.
    type: str
"""

    INFO = r"""
options:
  sys_id:
    description:
      - Unique identifier of the record to retrieve.
    type: str
"""
