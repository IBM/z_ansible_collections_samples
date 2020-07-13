# Copyright (c) IBM Corporation 2020
# Apache License, Version 2.0 (see https://opensource.org/licenses/Apache-2.0)

from __future__ import absolute_import, division, print_function

__metaclass__ = type
import re
from math import ceil

BYTES_PER_TRK = 56664
BYTES_PER_CYL = BYTES_PER_TRK * 15
BYTES_PER_KB = 1024
BYTES_PER_MB = 1048576


def total_size(dls_response, unit="cyl"):
    """Estimate the total size of a list of data sets based on `dls -s` output.

    Args:
        dls_response (dict): The response of command task which runs dls
        unit (str, optional): The unit of measurement for size to return. Defaults to "trk".

    Returns:
        dict: Estimated total primary and secondary size comparable to size valid for a list of data sets.
    """
    estimated_size_in_bytes = 0
    pattern = r"^(?:(?:[A-Z$#@]{1}[A-Z0-9$#@-]{0,7})(?:[.]{1})){1,21}[A-Z$#@]{1}[A-Z0-9$#@-]{0,7}\s+[A-Z]{1,3}\s+[A-Z]\s+[0-9]+\s[A-Z0-9]{1,6}\s+([0-9]+)\s+[0-9]+$"
    for line in dls_response.get("stdout_lines", []):
        pass
        match = re.match(pattern, line, re.IGNORECASE | re.MULTILINE)
        if match:
            estimated_size_in_bytes += int(match.group(1))
    estimated_size_in_unit = bytes_to_unit(estimated_size_in_bytes, unit)
    return {
        "primary": estimated_size_in_unit,
        "secondary": ceil(estimated_size_in_unit / 2),
    }


def bytes_to_unit(number_of_bytes, unit):
    size = 0
    unit = unit.lower()
    if number_of_bytes == 0:
        number_of_bytes = 1
    if unit == "cyl":
        size = byte_to_cyl(number_of_bytes)
    elif unit == "kb" or unit == "k":
        size = byte_to_kilobyte(number_of_bytes)
    elif unit == "mb" or unit == "m":
        size = byte_to_megabyte(number_of_bytes)
    else:
        size = byte_to_trk(number_of_bytes)
    return size


def byte_to_trk(number_of_bytes):

    return ceil(number_of_bytes / BYTES_PER_TRK)


def byte_to_cyl(number_of_bytes):
    return ceil(number_of_bytes / BYTES_PER_CYL)


def byte_to_kilobyte(number_of_bytes):
    return ceil(number_of_bytes / BYTES_PER_KB)


def byte_to_megabyte(number_of_bytes):
    return ceil(number_of_bytes / BYTES_PER_MB)


class FilterModule(object):
    """ Jinja2 filters for use with WTOR response objects returned by zos_operator_action_query module. """

    def filters(self):
        filters = {
            "total_size": total_size,
        }
        return filters
