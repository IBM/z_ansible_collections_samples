# -*- coding: utf-8 -*-

# Copyright (c) IBM Corporation 2022, 2024
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#     http://www.apache.org/licenses/LICENSE-2.0
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.

from __future__ import (absolute_import, division, print_function)
__metaclass__ = type


class ModuleDocFragment(object):

    DOCUMENTATION = r'''
options:
  use_template:
    description:
      - Whether the module should treat C(src) as a Jinja2 template and
        render it before continuing with the rest of the module.
      - Only valid when C(src) is a local file or directory.
      - All variables defined in inventory files, vars files and the playbook
        will be passed to the template engine,
        as well as L(Ansible special variables,https://docs.ansible.com/ansible/latest/reference_appendices/special_variables.html#special-variables),
        such as C(playbook_dir), C(ansible_version), etc.
      - If variables defined in different scopes share the same name, Ansible will
        apply variable precedence to them. You can see the complete precedence order
        L(in Ansible's documentation,https://docs.ansible.com/ansible/latest/playbook_guide/playbooks_variables.html#understanding-variable-precedence)
    type: bool
    default: false
  template_parameters:
    description:
      - Options to set the way Jinja2 will process templates.
      - Jinja2 already sets defaults for the markers it uses, you can find more
        information at its L(official documentation,https://jinja.palletsprojects.com/en/latest/templates/).
      - These options are ignored unless C(use_template) is true.
    required: false
    type: dict
    suboptions:
      variable_start_string:
        description:
          - Marker for the beginning of a statement to print a variable in Jinja2.
        type: str
        default: '{{'
      variable_end_string:
        description:
          - Marker for the end of a statement to print a variable in Jinja2.
        type: str
        default: '}}'
      block_start_string:
        description:
          - Marker for the beginning of a block in Jinja2.
        type: str
        default: '{%'
      block_end_string:
        description:
          - Marker for the end of a block in Jinja2.
        type: str
        default: '%}'
      comment_start_string:
        description:
          - Marker for the beginning of a comment in Jinja2.
        type: str
        default: '{#'
      comment_end_string:
        description:
          - Marker for the end of a comment in Jinja2.
        type: str
        default: '#}'
      line_statement_prefix:
        description:
          - Prefix used by Jinja2 to identify line-based statements.
        type: str
        required: false
      line_comment_prefix:
        description:
          - Prefix used by Jinja2 to identify comment lines.
        type: str
        required: false
      lstrip_blocks:
        description:
          - Whether Jinja2 should strip leading spaces from the start of a line
            to a block.
        type: bool
        default: false
      trim_blocks:
        description:
          - Whether Jinja2 should remove the first newline after a block is removed.
          - Setting this option to C(False) will result in newlines being added to
            the rendered template. This could create invalid code when working with
            JCL templates or empty records in destination data sets.
        type: bool
        default: true
      keep_trailing_newline:
        description:
          - Whether Jinja2 should keep the first trailing newline at the end of a
            template after rendering.
        type: bool
        default: false
      newline_sequence:
        description:
          - Sequence that starts a newline in a template.
        type: str
        default: "\n"
        choices:
          - "\n"
          - "\r"
          - "\r\n"
      auto_reload:
        description:
          - Whether to reload a template file when it has changed after the task
            has started.
        type: bool
        default: false
      autoescape:
        description:
          - Whether to enable autoescape of XML/HTML elements on a template.
        type: bool
        default: true
'''
