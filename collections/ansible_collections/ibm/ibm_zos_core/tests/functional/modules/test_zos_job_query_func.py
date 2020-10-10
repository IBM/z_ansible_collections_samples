# -*- coding: utf-8 -*-

# Copyright (c) IBM Corporation 2019, 2020
# Apache License, Version 2.0 (see https://opensource.org/licenses/Apache-2.0)

from __future__ import (absolute_import, division, print_function)

__metaclass__ = type

import os
import sys
import warnings

import ansible.constants
import ansible.errors
import ansible.utils
import pytest
from pprint import pprint


def test_zos_job_query_func(ansible_zos_module):
    hosts = ansible_zos_module
    results = hosts.all.zos_job_query(job_name="*", owner="*")
    pprint(vars(results))
    for result in results.contacted.values():
        assert result.get("changed") is False
        assert result.get("jobs") is not None
