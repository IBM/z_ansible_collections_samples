# -*- coding: utf-8 -*-

# (c) Copyright IBM Corp. 2023,2024
# Apache License, Version 2.0 (see https://opensource.org/licenses/Apache-2.0)

# FOR INTERNAL USE IN THE COLLECTION ONLY.

from __future__ import (absolute_import, division, print_function)
__metaclass__ = type


def _execution(name, rc, stdout, stderr):  # type: (str, int, str, str) -> dict
    return {
        "name": name,
        "rc": rc,
        "stdout": stdout,
        "stderr": stderr,
    }


class MVSExecutionException(Exception):
    def __init__(self, message, executions):   # type: (str, list[_execution]) -> None
        self.message = message
        self.executions = executions
