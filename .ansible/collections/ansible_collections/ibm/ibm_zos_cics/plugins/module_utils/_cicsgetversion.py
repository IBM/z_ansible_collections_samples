# -*- coding: utf-8 -*-

# (c) Copyright IBM Corp. 2023,2024
# Apache License, Version 2.0 (see https://opensource.org/licenses/Apache-2.0)

# FOR INTERNAL USE IN THE COLLECTION ONLY.

from __future__ import (absolute_import, division, print_function)
__metaclass__ = type
try:
    from zoautil_py.datasets import read
    from zoautil_py.exceptions import ZOAUException
except ImportError as imp_exc:
    ZOAUTIL_IMPORT_ERROR = imp_exc
else:
    ZOAUTIL_IMPORT_ERROR = None


def get_dataset_member_version_record(dataset):  # type: (str) -> str
    try:
        result = read("%s.SDFHSAMP(DFH0SINX)" % dataset).split("STATUS = ", 1)[1].split(" ")[0]
        if not result or result == "":
            raise Exception("CICS version was blank")
        elif len(result) >= 10:
            raise Exception("CICS version was too long")
        else:
            return result
    except ZOAUException as e:
        raise Exception("Error reading data set for calculating CICS version - {0}".format(e))
