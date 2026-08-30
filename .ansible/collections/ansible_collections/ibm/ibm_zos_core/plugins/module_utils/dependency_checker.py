# -*- coding: utf-8 -*-
# Copyright (c) IBM Corporation 2026
# Licensed under the Apache License, Version 2.0 (the "License");
# You may not use this file except in compliance with the License.
# You may obtain a copy at:
#     http://www.apache.org/licenses/LICENSE-2.0
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.

from __future__ import absolute_import, division, print_function
import re
import sys

from ansible_collections.ibm.ibm_zos_core.plugins.module_utils.log import SingletonLogger
from ansible_collections.ibm.ibm_zos_core.plugins.module_utils import version
logger = SingletonLogger().get_logger(verbosity=3)

__metaclass__ = type


# ------------------------------------------------------------------------------
# Compatibility Matrix by Collection Version
# ------------------------------------------------------------------------------
COMPATIBILITY_MATRIX = {
    "2.0.0":
        {"min_zoau_version": "1.4.0", "max_zoau_version": "1.5.0", "min_zos_version": "2.5"},
    # "2.1.0":
        # {"min_zoau_version": "1.4.1", "max_zoau_version": "1.5.0", "min_zos_version": "2.5"},
    # "2.2.0":
    #     {"min_zoau_version": "1.4.2", "max_zoau_version": "1.5.0", "min_zos_version": "2.5"},
}

REQUIRED_PYTHON_MAJOR_VERSION = 3


# ------------------------------------------------------------------------------
# Version conversion helper
# ------------------------------------------------------------------------------
def get_version_tuple(ver_str):
    """
    Convert version string to tuple for comparison.

    Handles formats like:
    - "1.4.2" -> (1, 4, 2)
    - "1.4.0.0-beta1" -> (1, 4, 0, 0)
    - "2.5" -> (2, 5)

    Args:
        ver_str: Version string to parse

    Returns:
        tuple: Version as tuple of integers, or (0,) on error
    """
    try:
        # Extract numeric version: digit + optional (dot + digit) repeating
        match = re.match(r'^(\d+(?:\.\d+)*)', ver_str)

        if not match:
            logger.warning("No valid version found in '%s'", ver_str)
            return (0,)

        # Split and convert to integers
        version_str = match.group(1)
        parts = tuple(int(x) for x in version_str.split('.'))

        return parts

    except (ValueError, AttributeError) as e:
        logger.warning("Error parsing version '%s': %s", ver_str, e)
        return (0,)


# ------------------------------------------------------------------------------
# Version Fetchers
# ------------------------------------------------------------------------------
def get_zoau_version_str():
    try:
        from zoautil_py import ZOAU_API_VERSION
        logger.debug("Found ZOAU version: %s", ZOAU_API_VERSION)
        return ZOAU_API_VERSION
    except ImportError:
        return None


def get_python_version_str():
    python_version = f"{sys.version_info.major}.{sys.version_info.minor}"
    logger.debug("Found python version: %s", python_version)
    return python_version


def get_zos_version_str(module):
    """
    Get z/OS version by calling 'uname -Irsv' and parsing the output.

    Expected output format: "z/OS 05.00 02"
    Where the last number (02) is the version and the middle number (05) is the release,
    resulting in z/OS version 2.5.

    Args:
        module: Ansible module object with run_command method

    Returns:
        str: z/OS version (e.g., "2.5"), or None if unable to determine.
    """
    try:
        # Execute uname -Irsv command using module.run_command
        rc, stdout, stderr = module.run_command(['uname', '-Irsv'])

        if rc != 0:
            error_msg = f"Command 'uname -Irsv' failed with return code {rc}: {stderr}"
            logger.debug(error_msg)
            return None

        output = stdout.strip()
        logger.debug("uname -Irsv output: %s", output)

        # Parse output format: "z/OS RR.MM VV"
        # Example: "z/OS 05.00 02" -> version 2.5
        # Where: VV (02) = version 2, RR (05) = release 5
        match = re.search(r'z/OS\s+(\d+)\.(\d+)\s+(\d+)', output)

        if match:
            release = int(match.group(1))  # RR (e.g., 05 → 5)
            version = int(match.group(3))  # VV (e.g., 02 → 2)

            zos_version = f"{version}.{release}"
            logger.debug("Parsed z/OS version: %s", zos_version)
            return zos_version

        logger.debug("Unable to parse z/OS version from: %s", output)
        return None

    except Exception as e:
        error_msg = f"Failed to fetch z/OS version: {e}"
        logger.debug(error_msg)
        return None


# ------------------------------------------------------------------------------
# Dependency Validation
# ------------------------------------------------------------------------------
def validate_dependencies(module):

    collection_version = version.__version__

    # Find compatibility entry
    compat_dict = COMPATIBILITY_MATRIX.get(collection_version, {})

    if not compat_dict:
        logger.debug("Discovered unknown ibm_zos_core collection version: %s.", collection_version)

    # Get version requirements
    min_zos_ver = compat_dict.get("min_zos_version")
    min_zoau_ver = compat_dict.get("min_zoau_version")
    max_zoau_ver = compat_dict.get("max_zoau_version")

    # Collect all warnings to display them together
    # ansible known issue - module.warn displays only latest warning
    warnings = []

    # --- z/OS version check ---
    current_zos_ver = get_zos_version_str(module)
    if current_zos_ver is None:
        logger.debug("Unable to retrieve z/OS version.")
    elif min_zos_ver:
        current_zos_tuple = get_version_tuple(current_zos_ver)
        min_zos_tuple = get_version_tuple(min_zos_ver)
        if current_zos_tuple < min_zos_tuple:
            msg = (
                f"Incompatible z/OS version {current_zos_ver}. "
                f"ibm_zos_core collection v{collection_version} requires z/OS {min_zos_ver} or later."
            )
            logger.warning(msg)
            warnings.append(msg)

    # --- Python version checks ---
    current_python_ver = get_python_version_str()
    current_python_tuple = get_version_tuple(current_python_ver)

    if current_python_tuple[0] != REQUIRED_PYTHON_MAJOR_VERSION:
        msg = (
            f"Incompatible Python version {current_python_ver}. "
            f"ibm_zos_core collection v{collection_version} requires Python {REQUIRED_PYTHON_MAJOR_VERSION}."
        )
        logger.warning(msg)
        warnings.append(msg)

    # --- ZOAU version checks ---
    current_zoau_ver = get_zoau_version_str()

    if current_zoau_ver is None:
        msg = (
            "Unable to import ZOAU. Verify the ZOAU installation and ensure "
            "PYTHONPATH, LIBPATH, and PATH environment variables are configured correctly."
        )
        logger.warning(msg)
        warnings.append(msg)

    if min_zoau_ver and max_zoau_ver and current_zoau_ver is not None:
        current_zoau_tuple = get_version_tuple(current_zoau_ver)
        min_zoau_tuple = get_version_tuple(min_zoau_ver)
        max_zoau_tuple = get_version_tuple(max_zoau_ver)

        if current_zoau_tuple < min_zoau_tuple or current_zoau_tuple > max_zoau_tuple:
            # Extract major.minor from min version for series reference
            min_series = '.'.join(min_zoau_ver.split('.')[:2])
            msg = (
                f"Incompatible ZOAU version {current_zoau_ver}. "
                f"ibm_zos_core collection v{collection_version} supports ZOAU {min_series}.x series "
                f"(minimum {min_zoau_ver})."
            )
            logger.warning(msg)
            warnings.append(msg)

    # Issue all warnings as a single combined message
    if warnings:
        combined_warning = "\n".join(warnings)
        module.warn(combined_warning)

    return  # do not exit, allow module to continue
