# -*- coding: utf-8 -*-
# Copyright: (c) 2022, XLAB Steampunk <steampunk@xlab.si>
#
# GNU General Public License v3.0+ (see COPYING or https://www.gnu.org/licenses/gpl-3.0.txt)

from __future__ import absolute_import, division, print_function

__metaclass__ = type

import copy
from contextlib import contextmanager

import yaml
from ansible.errors import (
    AnsibleAction,
    AnsibleActionFail,
    AnsibleError,
    AnsibleFileNotFound,
)
from ansible.module_utils._text import to_bytes, to_text
from ansible.module_utils.common.validation import check_mutually_exclusive
from ansible.module_utils.six import iteritems
from ansible.plugins.action import ActionBase
from yaml import Loader

from ..module_utils.api import FIELD_DATA, FIELD_TEMPLATE


def get_template_args(template):
    return dict(
        newline_sequence="\n",
        # variable_start_string is the string marking the start of a print statement.
        # Default value in jinja2: "{{"
        variable_start_string=None,
        # variable_end_string is the string marking the end of a print statement.
        # Default value in jinja2: "}}"
        variable_end_string=None,
        # block_start_string is the string marking the beginning of a block.
        # Default value in jinja2: "{%"
        block_start_string=None,
        # block_end_string is the string marking the end of a block.
        # Default value in jinja2: "%}"
        block_end_string=None,
        # Determine when newlines should be removed from blocks.
        # When set to yes the first newline after a block is removed (block, not variable tag!)
        # Default value in jinja2: True
        trim_blocks=True,
        # Determine when leading spaces and tabs should be stripped.
        # Option `lstrip_blocks' was added in Jinja2 version 2.7.
        # When set to yes leading spaces and tabs are stripped from the start of a line to a block.
        # Default value in jinja2: False
        lstrip_blocks=False,
        # Path to the template (if specified in the playbook for this task)
        path=template,
    )


class ActionModule(ActionBase):
    MUTUALLY_EXCLUSIVE = [(FIELD_DATA, FIELD_TEMPLATE)]

    def run(self, _tmp=None, task_vars=None):
        self._supports_check_mode = True
        self._supports_async = True
        if task_vars is None:
            task_vars = dict()

        # result is dictionary of results from the module
        result = super(ActionModule, self).run(task_vars=task_vars)
        wrap_async = self._task.async_val and not self._connection.has_native_async
        # If the user specified template, the value of rendered template is going to override parameter 'data' in
        # original module.params. So mutual exclusiveness has to be checked before running the module inside /modules
        # on remote machine

        check_mutually_exclusive(
            self.MUTUALLY_EXCLUSIVE,
            self._task.args,
        )

        new_module_args = copy.deepcopy(self._task.args)
        template = self._task.args.get(FIELD_TEMPLATE, None)
        if template:
            # If template was specified, override the existing data field with rendered jinja template
            # with path specified in template.
            new_module_args[FIELD_DATA] = self.load_template(template, task_vars)
            new_module_args.pop(FIELD_TEMPLATE)

        # Execute the api.py module.
        module_return = self._execute_module(
            module_name="servicenow.itsm.api",
            module_args=new_module_args,
            task_vars=task_vars,
            wrap_async=wrap_async,
        )

        result.update(module_return)

        # Delete tmp path
        self._remove_tmp_path(self._connection._shell.tmpdir)

        return result

    def _set_default_env(self):
        default_environment = dict()
        for key in (
            "newline_sequence",
            "variable_start_string",
            "variable_end_string",
            "block_start_string",
            "block_end_string",
            "trim_blocks",
        ):
            if hasattr(self._templar.environment, key):
                default_environment[key] = getattr(self._templar.environment, key)
        return default_environment

    @contextmanager
    def get_template_data(self, template_path):
        try:
            # _find_needle tried to find a needle in haystack of paths, optionally using 'dirname' as a subdir.
            # This will build the ordered list of paths to search and pass them to dwim to get back the first
            # existing file found.
            source = self._find_needle("templates", template_path)
        except AnsibleError as e:
            raise AnsibleActionFail(to_text(e))

        try:
            # get_real_file will in our case return a path to a temporary decrypted file
            tmp_source = self._loader.get_real_file(source)

        except AnsibleFileNotFound as e:
            raise AnsibleActionFail(
                "could not find template=%s, %s" % (source, to_text(e))
            )
        # Makes sure that a string is a byte string
        b_tmp_source = to_bytes(tmp_source, errors="surrogate_or_strict")

        try:
            with open(b_tmp_source, "rb") as f:
                try:
                    # Makes sure that a string is a text string
                    template_data = to_text(f.read(), errors="surrogate_or_strict")
                except UnicodeError:
                    raise AnsibleActionFail(
                        "Template source files must be utf-8 encoded"
                    )
            yield template_data
        except AnsibleAction:
            raise
        except Exception as e:
            raise AnsibleActionFail("%s: %s" % (type(e).__name__, to_text(e)))
        finally:
            # Removes any temporary files created from a previous call to get_real_file
            self._loader.cleanup_tmp_file(b_tmp_source)

    def load_template(self, template, task_vars):
        template_item = get_template_args(template)
        default_environment = self._set_default_env()

        # template the source data locally & get ready to transfer
        with self.get_template_data(template_item["path"]) as template_data:
            for key, value in iteritems(template_item):
                if hasattr(self._templar.environment, key):
                    if value is not None:
                        setattr(self._templar.environment, key, value)
                    else:
                        setattr(
                            self._templar.environment,
                            key,
                            default_environment.get(key),
                        )
            self._templar.available_variables = copy.deepcopy(task_vars)
            # rendered_template's a string which is going to be dumped into dict
            rendered_template = self._templar.do_template(
                template_data,
                preserve_trailing_newlines=True,
                escape_backslashes=False,
            )

        return yaml.load(stream=rendered_template, Loader=Loader)
