# -*- coding: utf-8 -*-
# Copyright: (c) 2022, XLAB Steampunk <steampunk@xlab.si>
#
# GNU General Public License v3.0+ (see COPYING or https://www.gnu.org/licenses/gpl-3.0.txt)

from __future__ import absolute_import, division, print_function

__metaclass__ = type


class ModuleDocFragment(object):
    DOCUMENTATION = r"""
options:
  sysparm_display_value:
    description:
      - Return field display values C(true), actual values C(false), or both C(all).
    type: str
    choices: ["true", "false", "all"]
    default: 'false'
    version_added: '2.0.0'
"""
