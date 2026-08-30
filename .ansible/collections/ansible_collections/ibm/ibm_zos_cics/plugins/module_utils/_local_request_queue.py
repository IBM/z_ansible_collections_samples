# -*- coding: utf-8 -*-

# (c) Copyright IBM Corp. 2023,2024
# Apache License, Version 2.0 (see https://opensource.org/licenses/Apache-2.0)

# FOR INTERNAL USE IN THE COLLECTION ONLY.

from __future__ import (absolute_import, division, print_function)
__metaclass__ = type


def _get_idcams_cmd_lrq(data_set):  # type: (dict) -> dict
    defaults = {
        "CLUSTER": {
            "RECORDSIZE": "{0} {1}".format(RECORD_COUNT_DEFAULT, RECORD_SIZE_DEFAULT),
            "INDEXED": None,
            "KEYS": "{0} {1}".format(KEY_LENGTH, KEY_OFFSET),
            "FREESPACE": "{0} {1}".format(CI_PERCENT, CA_PERCENT),
            "SHAREOPTIONS": "{0} {1}".format(SHARE_CROSSREGION, SHARE_CROSSSYSTEM),
            "REUSE": None,
            "LOG": str(LOG_OPTION)
        },
        "DATA": {
            "CONTROLINTERVALSIZE": str(CONTROL_INTERVAL_SIZE_DEFAULT)
        },
        "INDEX": {
            None
        }
    }
    defaults.update(data_set)
    return defaults


RECORD_COUNT_DEFAULT = 2232
RECORD_SIZE_DEFAULT = 2400
CONTROL_INTERVAL_SIZE_DEFAULT = 2560
KEY_LENGTH = 40
KEY_OFFSET = 0
CI_PERCENT = 0
CA_PERCENT = 10
SHARE_CROSSREGION = 2
SHARE_CROSSSYSTEM = 3
LOG_OPTION = "UNDO"
