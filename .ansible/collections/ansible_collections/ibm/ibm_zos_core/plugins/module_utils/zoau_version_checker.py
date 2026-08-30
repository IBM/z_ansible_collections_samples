# -*- coding: utf-8 -*-
# Copyright (c) IBM Corporation 2022, 2025
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#     http://www.apache.org/licenses/LICENSE-2.0
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.


from __future__ import absolute_import, division, print_function

# import subprocess

try:
    from zoautil_py import ZOAU_API_VERSION
except Exception:
    ZOAU_API_VERSION = "1.2.0"

__metaclass__ = type


def is_zoau_version_higher_than(min_version_str):
    """Reports back if ZOAU version is high enough.

    Parameters
    ----------
    min_version_str : str
        The minimal desired ZOAU version '#.#.#'.

    Returns
    -------
    bool
        Whether ZOAU version found was high enough.
    """
    if is_valid_version_string(min_version_str):
        # check zoau version on system (already a list)
        system_version_list = get_zoau_version_str()

        # convert input to list format
        min_version_list = min_version_str.split('.')

        # convert list of strs to list of ints
        system_version_list = [int(i) for i in system_version_list]
        min_version_list = [int(i) for i in min_version_list]

        # compare major
        if system_version_list[0] > min_version_list[0]:
            return True
        if system_version_list[0] < min_version_list[0]:
            return False

        # majors are equal, compare minor
        if system_version_list[1] > min_version_list[1]:
            return True
        if system_version_list[1] < min_version_list[1]:
            return False

        # majors and minors are equal, compare patch
        if system_version_list[2] > min_version_list[2]:
            return True
        if system_version_list[2] < min_version_list[2]:
            return False

        # check for a 4th level if system and min_version provided it
        if len(system_version_list) < 4:
            system_version_list.append(0)
        if len(min_version_list) < 4:
            min_version_list.append(0)

        #  return result of comparison of 4th levels.
        return system_version_list[3] >= min_version_list[3]

    # invalid version string
    return False


def is_valid_version_string(version_str):
    """Reports back if string is in proper version format. Expected format is a
        series of numbers (major) followed by a dot(.) followed by another
        series of numbers (minor) followed by another dot(.) followed by a
        series of numbers (patch) i.e. "#.#.#" where '#' can be any integer.
        There is a provision for a 4th level to this eg "v1.2.0.1".

    Parameters
    ----------
    min_version_str : str
        String to be verified is in correct format.

    Returns
    -------
    bool
        Whether provided str is in correct format.
    """

    # split string into [major, minor, patch]
    version_list = version_str.split('.')

    # check each element in list isnumeric()
    for ver in version_list:
        if not ver.isnumeric():
            return False

    return True


def get_zoau_version_str():
    """Attempts to call zoaversion on target and parses out version string.

    Returns
    -------
    Union[int, int, int]
        ZOAU version found in format [#,#,#]. There is a
        provision for a 4th level eg "v1.2.0.1".
    """
    version_list = (
        ZOAU_API_VERSION.split('.')
    )

    return version_list
