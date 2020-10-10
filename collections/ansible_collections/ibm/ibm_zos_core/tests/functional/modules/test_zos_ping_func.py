# -*- coding: utf-8 -*-

# Copyright (c) IBM Corporation 2019, 2020
# Apache License, Version 2.0 (see https://opensource.org/licenses/Apache-2.0)

from __future__ import (absolute_import, division, print_function)

__metaclass__ = type


def test_job_submit_PDS(ansible_zos_module):
    hosts = ansible_zos_module
    results = hosts.all.zos_ping()
    for result in results.contacted.values():
        assert result.get("ping") == "pong"
