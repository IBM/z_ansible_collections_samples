# Copyright (c) IBM Corporation 2019, 2020
# Apache License, Version 2.0 (see https://opensource.org/licenses/Apache-2.0)

from __future__ import absolute_import, division, print_function

__metaclass__ = type

import os
import stat
import uuid
from collections import OrderedDict

# ? should we just use yaml and accept the unordered dict?
# * oyaml is a drop-in replacement for pyyaml that preserves dict
# * ordering, this is useful in our use case since we define environment variables as
# * a list of key:value pairs. This resolves issues reading environment variables
# * that depend other environment variables defined earlier in the environment variable list.
# when using python versions >= 3.7, dict ordering is preserved by default so pyyaml can be used
from oyaml import safe_load

# TODO: Add/enhance error handling


class ZTestHelper(object):
    """ ZTestHelper provides helper methods to deal with added complexities when testing against a z/OS system. """

    def __init__(self, host, user, python_path, environment, **extra_args):
        self._host = host
        self._user = user
        self._python_path = python_path
        self._environment = environment
        self._extra_args = extra_args

    @classmethod
    def from_yaml_file(cls, path):
        """ Reads arguments from a YAML file to create an instance of ZTestHelper.  """
        testvars = {}
        with open(path, "r") as varfile:
            testvars = safe_load(varfile)
        return cls(**testvars)

    def get_inventory_info(self):
        """ Returns dictionary containing basic info needed to generate a single-host inventory file. """
        inventory_info = {
            "user": self._user,
            "inventory": "{0},".format(self._host),
        }
        inventory_info.update(self._extra_args)
        return inventory_info

    def build_interpreter_string(self):
        """ Builds wrapper to be used for python calls in pytest fixtures with needed environment variables.
        This is useful in situations where no environment variables are assumed to be set. """
        interpreter_string = ""
        for key, value in self._environment.items():
            interpreter_string += "export {0}={1} ; ".format(key, value)
        interpreter_string += self._python_path
        return interpreter_string
