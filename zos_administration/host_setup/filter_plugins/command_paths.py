# Copyright (c) IBM Corporation 2020
# Apache License, Version 2.0 (see https://opensource.org/licenses/Apache-2.0)

from __future__ import absolute_import, division, print_function

__metaclass__ = type
import re

IBM_PYTHON_PATTERN = r"^((.*pyz)\/bin\/python([0-9]).([0-9]+))$"
ANACONDA_PYTHON_PATTERN = r"^((.*anaconda)\/bin\/python3.6)$"
ROCKET_PYTHON_PATTERN = r"^((.*python36)\/bin\/python3.6)$"


def potential_installation_paths(dependency_paths):
    """Determine which paths found on system match dependency criteria.
    Args:
        dependency_paths (dict): The response of command task which runs find
    Returns:
        dict: The most likely paths for our dependencies
    """
    all_paths = dependency_paths.get("stdout_lines", [])
    python_installs = _find_python_installs(all_paths)
    zoau_installs = _find_zoau(all_paths)
    return {"python": python_installs, "zoau": zoau_installs}


def _find_python_installs(all_paths):
    """Attempts to locate all supported python installs.
    Returned list of installs are sorted based on preferred python
    vendor.
    Args:
        all_paths (list[str]): A list of potential paths.
    Returns:
        list[dict]: A list of python interpreter strings, sorted by preferred vendor.
    """
    paths = "\n".join(all_paths)

    valid_python_versions = [
        IBM_PYTHON_PATTERN,
        ANACONDA_PYTHON_PATTERN,
        ROCKET_PYTHON_PATTERN,
    ]

    python_installs = []
    for python_version in valid_python_versions:
        match = re.findall(python_version, paths, re.MULTILINE)
        if match:
            python_installs += [
                {"interpreter": group[0], "root": group[1]} for group in match
            ]
    return python_installs


def _find_zoau(all_paths):
    """Attempts to locate all supported ZOAU installs.
    Args:
        all_paths (list[str]): A list of potential paths.
    Returns:
        list[dict]: A list of ZOAU installation paths.
    """
    paths = "\n".join(all_paths)
    zoau = r"^((.*)\/bin\/dtouch)$"
    zoau_installs = []
    match = re.findall(zoau, paths, re.MULTILINE)
    if match:
        zoau_installs = [{"root": group[1]} for group in match]
    return zoau_installs


def is_ibm_python(python_interpreter):
    """Determines if interpreter string matches format
    of IBM Python installation.
    Args:
        python_interpreter (str): The path to the python interpreter.
    Returns:
        bool: True if IBM python otherwise False
    """
    if re.match(IBM_PYTHON_PATTERN, python_interpreter):
        return True
    return False


def is_rocket_python(python_interpreter):
    """Determines if interpreter string matches format
    of Rocket Python installation.
    Args:
        python_interpreter (str): The path to the python interpreter.
    Returns:
        bool: True if Rocket python otherwise False
    """
    if re.match(ROCKET_PYTHON_PATTERN, python_interpreter):
        return True
    return False


def filter_zoau_installs(zoau_installs, build_info, minimum_zoau_version):
    """Sort and filter potential ZOAU installs based on build date
    and version.
    Args:
        zoau_installs (list[dict]): A list of found ZOAU installation paths.
        build_info (list[str]): A list of build info strings
        minimum_zoau_version (str): The minimum version of ZOAU to accept.
    Returns:
        list[dict]: A sorted and filtered list of ZOAU installation paths.
    """
    for index, zoau_install in enumerate(zoau_installs):
        zoau_install["build"] = build_info[index]
    zoau_installs.sort(key=lambda x: x.get("build"), reverse=True)
    min_version = _version_to_tuple(minimum_zoau_version)
    valid_installs = []
    for zoau_install in zoau_installs:
        if min_version <= _version_to_tuple(
            _get_version_from_build_string(zoau_install.get("build", ""))
        ):
            valid_installs.append(zoau_install)
    return valid_installs


def _get_version_from_build_string(build):
    version = "0.0.0"
    match = re.search(
        r"[0-9]+\/[0-9]+\/[0-9]+\s+[0-9]+:[0-9]+:[0-9]+\s+(?:CUT )?V([0-9]+.[0-9]+.[0-9]+)",
        build,
    )
    if match:
        version = match.group(1)
    return version


def _version_to_tuple(version):
    return tuple([int(x) for x in version.split(".")])


class FilterModule(object):
    """ Jinja2 filters for identifying, sorting and filtering dependency paths. """

    def filters(self):
        filters = {
            "potential_paths": potential_installation_paths,
            "is_ibm_python": is_ibm_python,
            "is_rocket_python": is_rocket_python,
            "filter_zoau_installs": filter_zoau_installs,
        }
        return filters