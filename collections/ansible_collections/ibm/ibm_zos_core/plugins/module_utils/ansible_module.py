# Copyright (c) IBM Corporation 2020
# Apache License, Version 2.0 (see https://opensource.org/licenses/Apache-2.0)

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
