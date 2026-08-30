#!/usr/bin/python
# -*- coding: utf-8 -*-

# Copyright (c) IBM Corporation 2019, 2025
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#     http://www.apache.org/licenses/LICENSE-2.0
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.

DOCUMENTATION = r"""
---
module: zos_ping
version_added: "1.1.0"
short_description: Ping z/OS and check dependencies.
description:
  - L(zos_ping,./zos_ping.html) verifies the presence of z/OS Web Client Enablement Toolkit,
    iconv, and Python.
  - L(zos_ping,./zos_ping.html) returns C(pong) when the target host is not missing any required
    dependencies.
  - If the target host is missing optional dependencies, the L(zos_ping,./zos_ping.html) will
    return one or more warning messages.
  - If a required dependency is missing from the target host, an explanatory
    message will be returned with the module failure.
author:
  - "Vijay Katoch (@vijayka)"
  - "Blake Becker (@blakeinate)"
  - "Demetrios Dimatos (@ddimatos)"
options: {}

attributes:
  action:
    support: full
    description: Indicates this has a corresponding action plugin so some parts of the options can be executed on the controller.
  async:
    support: full
    description: Supports being used with the ``async`` keyword.
  check_mode:
    support: none
    description: Can run in check_mode and return changed status prediction without modifying target. If not supported, the action will be skipped.

notes:
    - This module is written in REXX and relies on the SCP protocol to transfer the source to
      the managed z/OS node and encode it in the managed nodes default encoding, eg IBM-1047.
      Starting with OpenSSH 9.0, it switches from SCP to use SFTP by default, meaning transfers
      are no longer treated as text and are transferred as binary preserving the source files
      encoding resulting in a module failure. If you are using OpenSSH 9.0 (ssh -V) or later,
      you can instruct SSH to use SCP by adding the entry C(scp_extra_args="-O") into the ini
      file named C(ansible.cfg).
    - For more information, review the
      L(ansible.builtin.ssh, https://docs.ansible.com/ansible/latest/collections/ansible/builtin/ssh_connection.html)
      module.
"""

EXAMPLES = r"""
- name: Ping the z/OS host and perform resource checks
  zos_ping:
  register: result
"""

RETURN = r"""
ping:
  description: Should contain the value "pong" on success.
  returned: always
  type: str
  sample: pong
warnings:
  description: List of warnings returned from stderr when performing resource checks.
  returned: failure
  type: list
  elements: str
"""
