# Copyright (c) IBM Corporation 2020
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#     http://www.apache.org/licenses/LICENSE-2.0
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.

from __future__ import absolute_import, division, print_function

__metaclass__ = type

from ansible.module_utils.basic import AnsibleModule


class AnsibleModuleHelper(AnsibleModule):
    """Wrapper for AnsibleModule object that
    allows us to use AnsibleModule methods like
    run_command() without specifying a valid argument
    spec.
    """

    def fail_json(self, **kwargs):
        if "Unsupported parameters for" in kwargs.get("msg", ""):
            return
        else:
            super().fail_json(**kwargs)
