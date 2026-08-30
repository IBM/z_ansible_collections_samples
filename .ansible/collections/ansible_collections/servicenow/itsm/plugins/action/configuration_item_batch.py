# -*- coding: utf-8 -*-
# Copyright: (c) 2021, XLAB Steampunk <steampunk@xlab.si>
#
# GNU General Public License v3.0+ (see COPYING or https://www.gnu.org/licenses/gpl-3.0.txt)

from __future__ import absolute_import, division, print_function

__metaclass__ = type


from ansible.plugins.action import ActionBase
from ansible.utils.vars import merge_hash
from jinja2 import Environment


def validate(name, args, required, typ):
    """
    Make sure that required values are not None and that if the value is
    present, it is of the correct type.
    """
    value = args.get(name)
    messages = []
    if required and value is None:
        messages.append("{0} is required argument".format(name))
    if value is not None and not isinstance(value, typ):
        messages.append("{0} should be {1}".format(name, typ))
    return messages


class ActionModule(ActionBase):
    def run(self, _tmp=None, task_vars=None):
        self._supports_check_mode = True
        self._supports_async = True

        result = super(ActionModule, self).run(task_vars=task_vars)

        wrap_async = self._task.async_val and not self._connection.has_native_async

        try:
            err_msgs = self.validate_arguments(self._task.args)
            if err_msgs:
                return dict(result, failed=True, msg=" ".join(err_msgs))

            args = dict(
                self._task.args,
                dataset=self.build_asset(
                    self._task.args["map"], self._task.args["dataset"]
                ),
                map=dict(),
            )

            return merge_hash(
                result,
                self._execute_module(
                    module_name="servicenow.itsm.configuration_item_batch",
                    module_args=args,
                    task_vars=task_vars,
                    wrap_async=wrap_async,
                ),
            )

        finally:
            if not wrap_async:
                self._remove_tmp_path(self._connection._shell.tmpdir)

    @staticmethod
    def validate_arguments(args):
        # We only validate arguments that we use. We let the module
        # validate the rest (like auth data).
        messages = []
        messages.extend(validate("dataset", args, required=True, typ=list))
        messages.extend(validate("map", args, required=True, typ=dict))

        return messages

    @staticmethod
    def build_asset(mapping, dataset):
        cmdb_items = [{} for _i in range(len(dataset))]
        env = Environment()

        for key, template in mapping.items():
            t = env.from_string("{{" + template + "}}")

            for input, output in zip(dataset, cmdb_items):
                output[key] = t.render(**input)

        return cmdb_items
