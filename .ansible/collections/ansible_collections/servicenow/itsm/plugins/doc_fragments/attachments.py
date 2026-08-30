# -*- coding: utf-8 -*-
# Copyright: (c) 2021, XLAB Steampunk <steampunk@xlab.si>
#
# GNU General Public License v3.0+ (see COPYING or https://www.gnu.org/licenses/gpl-3.0.txt)

from __future__ import absolute_import, division, print_function

__metaclass__ = type


class ModuleDocFragment(object):
    DOCUMENTATION = r"""
options:
  attachments:
    version_added: 1.2.0
    description:
      - ServiceNow attachments.
    type: list
    elements: dict
    suboptions:
      path:
        description:
          - Path to the file to be uploaded.
        required: true
        type: str
      name:
        description:
          - Name of the file to be uploaded.
          - Serves as unique identifier.
          - If not specified, the module will use I(path)'s base name.
        type: str
      type:
        description:
          - MIME type of the file to be attached.
          - If not specified, the module will try to guess the file's type from its extension.
        type: str
"""
