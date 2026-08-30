# -*- coding: utf-8 -*-

# (c) Copyright IBM Corp. 2023,2024
# Apache License, Version 2.0 (see https://opensource.org/licenses/Apache-2.0)

# FOR INTERNAL USE IN THE COLLECTION ONLY.utils.

from __future__ import absolute_import, division, print_function

__metaclass__ = type


def _get_idcams_cmd_temp(data_set):  # type: (dict) -> dict
    defaults = {
        "CLUSTER": {
            "RECORDSIZE": "{0} {1}".format(
                RECORD_COUNT_DEFAULT,
                RECORD_SIZE_DEFAULT,
            ),
            "NONINDEXED": None,
            "CONTROLINTERVALSIZE": str(CONTROL_INTERVAL_SIZE_DEFAULT),
            "SHAREOPTIONS": "{0} {1}".format(
                SHARE_CROSSREGION,
                SHARE_CROSSSYSTEM,
            ),
        },
        "DATA": {"UNIQUE": None},
    }
    defaults.update(data_set)
    return defaults


RECORD_COUNT_DEFAULT = 4089
RECORD_SIZE_DEFAULT = 4089
CONTROL_INTERVAL_SIZE_DEFAULT = 4096
SHARE_CROSSREGION = 2
SHARE_CROSSSYSTEM = 3
