# -*- coding: utf-8 -*-
# Copyright: (c) 2022, XLAB Steampunk <steampunk@xlab.si>
#
# GNU General Public License v3.0+ (see COPYING or https://www.gnu.org/licenses/gpl-3.0.txt)

from __future__ import absolute_import, division, print_function

__metaclass__ = type


class ModuleDocFragment(object):
    DOCUMENTATION = r"""
options:
  configuration_item_mapping:
    version_added: "1.3.0"
    description:
      - User mappings for I(Configuration item) object.
      - Where mapping is not set, the default will be used.
    type: dict
    suboptions:
      environment:
        description:
          - The environment to which this configuration item belongs.
        type: dict
      install_status:
        description:
          - The functional status of the configuration item.
          - Special value that can not be overridden is C(absent), which would remove a configuration item from ServiceNow.
        type: dict
      operational_status:
        description:
          - The operational status of the configuration item.
        type: dict
"""
