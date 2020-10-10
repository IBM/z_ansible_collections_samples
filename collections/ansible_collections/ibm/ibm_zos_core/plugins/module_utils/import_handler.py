# Copyright (c) IBM Corporation 2020
# Apache License, Version 2.0 (see https://opensource.org/licenses/Apache-2.0)

from __future__ import absolute_import, division, print_function

__metaclass__ = type


class MissingZOAUImport(object):
    def __getattr__(self, name):
        def method(*args, **kwargs):
            raise ImportError(
                (
                    "ZOAU is not properly configured for Ansible. Unable to import zoautil_py. "
                    "Ensure environment variables are properly configured in Ansible for use with ZOAU."
                )
            )

        return method


class MissingImport(object):
    def __init__(self, import_name=""):
        self.import_name = import_name

    def __getattr__(self, name):
        def method(*args, **kwargs):
            raise ImportError("Import {0} was not available.".format(self.import_name))

        return method
