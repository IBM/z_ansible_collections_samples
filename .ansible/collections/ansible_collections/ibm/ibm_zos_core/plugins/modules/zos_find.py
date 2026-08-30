#!/usr/bin/python
# -*- coding: utf-8 -*-

# Copyright (c) IBM Corporation 2020, 2025
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#     http://www.apache.org/licenses/LICENSE-2.0
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.

from __future__ import (absolute_import, division, print_function)

__metaclass__ = type


DOCUMENTATION = r"""
---
module: zos_find
version_added: "1.3.0"
short_description: Find matching data sets
description:
  - Return a list of data sets based on specific criteria.
  - Multiple criteria can be added (AND'd) together.
  - The C(zos_find) module can only find MVS data sets. Use the
    L(find,https://docs.ansible.com/ansible/latest/modules/find_module.html)
    module to find USS files.
author:
  - "Asif Mahmud (@asifmahmud)"
  - "Demetrios Dimatos (@ddimatos)"
  - "Fernando Flores (@fernandofloresg)"
options:
  age:
    description:
      - Select data sets whose age is equal to or greater than the specified time.
      - Use a negative age to find data sets equal to or less than the specified time.
      - You can choose days, weeks, months or years by specifying the first letter of
        any of those words (e.g., "1w"). The default is days.
      - Age is determined by using the 'referenced date' of the data set.
    type: str
    required: false
  age_stamp:
    description:
      - Choose the age property against which to compare age.
      - C(creation_date) is the date the data set was created and C(ref_date) is the date
        the data set was last referenced.
      - C(ref_date) is only applicable to sequential and partitioned data sets.
    choices:
      - creation_date
      - ref_date
    default: creation_date
    type: str
    required: false
  contains:
    description:
      - A string which should be matched against the data set content or data
        set member content.
    type: str
    required: false
  excludes:
    description:
      - Data sets whose names match an excludes pattern are culled from patterns matches.
        Multiple patterns can be specified using a list.
      - The pattern can be a regular expression.
      - If the pattern is a regular expression, it must match the full data set name.
      - To exclude members, the regular expression or pattern must be enclosed in parentheses.
        This expression can be used alongside a pattern to exclude data set names.
    aliases:
      - exclude
    type: list
    elements: str
    required: false
  patterns:
    description:
      - One or more data set or member patterns.
      - The patterns restrict the list of data sets or members to be returned to those
        names that match at least one of the patterns specified. Multiple patterns
        can be specified using a list.
      - This parameter expects a list, which can be either comma separated or YAML.
      - When searching for members within a PDS/PDSE, pattern can be a regular expression.
    type: list
    elements: str
    required: true
  size:
    description:
      - Select data sets whose size is equal to or greater than the specified size.
      - Use a negative size to find files equal to or less than the specified size.
      - Unqualified values are in bytes but b, k, m, g, and t can be appended to
        specify bytes, kilobytes, megabytes, gigabytes, and terabytes, respectively.
      - Filtering by size is currently only valid for sequential and partitioned data sets.
    required: false
    type: str
  resource_type:
    description:
      - The types of resources to search.
      - C(nonvsam) refers to one of SEQ, LIBRARY (PDSE), PDS, LARGE, BASIC, EXTREQ, or EXTPREF.
      - C(cluster) refers to a VSAM cluster. The C(data) and C(index) are the data and index
        components of a VSAM cluster.
      - C(gdg) refers to Generation Data Groups. The module searches based on the GDG base name.
      - C(migrated) refers to listing migrated datasets. Only C(excludes) and C(migrated_type) options can be used along
        with this option. The module only searches based on dataset patterns.
    aliases:
      - resource_types
    choices:
      - nonvsam
      - cluster
      - data
      - index
      - gdg
      - migrated
    type: list
    elements: str
    required: false
    default: "nonvsam"
  migrated_type:
    description:
      - A migrated data set related attribute, only valid when C(resource_type=migrated).
      - If provided, will search for only those types of migrated datasets.
    aliases:
      - migrated_types
    choices:
      - nonvsam
      - cluster
      - data
      - index
    type: list
    elements: str
    required: false
    default: ["cluster", "data", "index", "nonvsam"]
  volume:
    description:
      - If provided, only the data sets allocated in the specified list of
        volumes will be searched.
    type: list
    elements: str
    required: false
    aliases:
      - volumes
  empty:
    description:
      - A GDG attribute, only valid when C(resource_type=gdg).
      - If provided, will search for data sets with I(empty) attribute set as provided.
    type: bool
    required: false
  extended:
    description:
      - A GDG attribute, only valid when C(resource_type=gdg).
      - If provided, will search for data sets with I(extended) attribute set as provided.
    type: bool
    required: false
  fifo:
    description:
      - A GDG attribute, only valid when C(resource_type=gdg).
      - If provided, will search for data sets with I(fifo) attribute set as provided.
    type: bool
    required: false
  limit:
    description:
      - A GDG attribute, only valid when C(resource_type=gdg).
      - If provided, will search for data sets with I(limit) attribute set as provided.
    type: int
    required: false
  purge:
    description:
      - A GDG attribute, only valid when C(resource_type=gdg).
      - If provided, will search for data sets with I(purge) attribute set as provided.
    type: bool
    required: false
  scratch:
    description:
      - A GDG attribute, only valid when C(resource_type=gdg).
      - If provided, will search for data sets with I(scratch) attribute set as provided.
    type: bool
    required: false

attributes:
  action:
    support: none
    description: Indicates this has a corresponding action plugin so some parts of the options can be executed on the controller.
  async:
    support: full
    description: Supports being used with the ``async`` keyword.
  check_mode:
    support: none
    description: Can run in check_mode and return changed status prediction without modifying target. If not supported, the action will be skipped.

notes:
  - Only cataloged data sets will be searched. If an uncataloged data set needs to
    be searched, it should be cataloged first. The L(zos_data_set,./zos_data_set.html) module can be
    used to catalog uncataloged data sets.
  - The L(zos_find,./zos_find.html) module currently does not support wildcards for high level qualifiers.
    For example, C(SOME.*.DATA.SET) is a valid pattern, but C(*.DATA.SET) is not.
  - If a data set pattern is specified as C(USER.*), the matching data sets will have two
    name segments such as C(USER.ABC), C(USER.XYZ) etc. If a wildcard is specified
    as C(USER.*.ABC), the matching data sets will have three name segments such as
    C(USER.XYZ.ABC), C(USER.TEST.ABC) etc.
  - The time taken to execute the module is proportional to the number of data
    sets present on the system and how large the data sets are.
  - When searching for content within data sets, only non-binary content is considered.
  - As a migrated data set's information can't be retrieved without recalling it first, other options
    besides C(excludes) and C(migrated_type) are not supported.
seealso:
- module: ibm.ibm_zos_core.zos_data_set
"""


EXAMPLES = r"""
- name: Exclude all members starting with characters 'TE' in a given list datasets patterns
  zos_find:
    excludes: '(^te.*)'
    patterns:
      - IMSTEST.TEST.*
      - IMSTEST.USER.*
      - USER.*.LIB

- name: Exclude datasets that includes 'DATA' and members starting with characters 'MEM' in a given list datasets patterns
  zos_find:
    excludes: '^.*DATA.*(^MEM.*)'
    patterns:
      - IMSTEST.*.TEST
      - IMSTEST.*.*
      - USER.*.LIB

- name: Find all data sets with HLQ 'IMS.LIB' or 'IMSTEST.LIB' that contain the word 'hello'
  zos_find:
    patterns:
      - IMS.LIB.*
      - IMSTEST.LIB.*
    contains: 'hello'
    age: 2d

- name: Search for 'rexx' in all datasets matching IBM.TSO.*.C??
  zos_find:
    patterns:
      - IBM.TSO.*.C??
    contains: 'rexx'

- name: Exclude data sets that have a low level qualifier 'TEST'
  zos_find:
    patterns: 'IMS.LIB.*'
    contains: 'hello'
    excludes: '.*TEST'

- name: Find all data sets greater than 2MB and allocated in one of the specified volumes
  zos_find:
    patterns: 'USER.*'
    size: 2m
    volumes:
      - SCR03
      - IMSSUN

- name: Find all VSAM clusters starting with the word 'USER'
  zos_find:
    patterns:
      - USER.*
    resource_type:
      - 'cluster'

- name: Find all Generation Data Groups starting with the word 'USER' and specific GDG attributes.
  zos_find:
    patterns:
      - USER.*
    resource_type:
      - 'gdg'
    limit: 30
    scratch: true
    purge: true

- name: Find all migrated and nonvsam data sets starting with the word 'USER'
  zos_find:
    patterns:
      - USER.*
    resource_type:
      - 'migrated'
    migrated_type:
      - 'nonvsam'
"""


RETURN = r"""
data_sets:
    description: All matches found with the specified criteria.
    returned: success
    type: list
    sample: [
      {
        "name": "IMS.CICS13.USERLIB",
        "members": {
            "COBU",
            "MC2CNAM",
            "TINAD"
        },
        "type": "NONVSAM"
      },
      {
        "name": "SAMPLE.DATA.SET",
        "type": "CLUSTER"
      },
      {
        "name": "SAMPLE.VSAM.DATA",
        "type": "DATA"
      }
    ]
matched:
    description: The number of matched data sets found.
    returned: success
    type: int
    sample: 49
examined:
    description: The number of data sets searched.
    returned: success
    type: int
    sample: 158
msg:
    description: Failure message returned by the module.
    returned: failure
    type: str
    sample: Error while gathering data set information
stdout:
    description: The stdout from a USS command or MVS command, if applicable.
    returned: failure
    type: str
    sample: Searching dataset IMSTESTL.COMNUC
stderr:
    description: The stderr of a USS command or MVS command, if applicable.
    returned: failure
    type: str
    sample: No such file or directory "/tmp/foo"
rc:
    description: The return code of a USS or MVS command, if applicable.
    returned: failure
    type: int
    sample: 8
"""

import re
import time
import datetime
import math
import json

from re import match as fullmatch


from ansible.module_utils.basic import AnsibleModule

from ansible_collections.ibm.ibm_zos_core.plugins.module_utils import (
    vtoc, mvs_cmd
)

from ansible_collections.ibm.ibm_zos_core.plugins.module_utils.better_arg_parser import (
    BetterArgParser
)

from ansible_collections.ibm.ibm_zos_core.plugins.module_utils.ansible_module import (
    AnsibleModuleHelper
)
from ansible_collections.ibm.ibm_zos_core.plugins.module_utils.dependency_checker import (
    validate_dependencies,
)

from ansible_collections.ibm.ibm_zos_core.plugins.module_utils.log import SingletonLogger
from shlex import quote


def content_filter(module, patterns, content):
    """ Find data sets that match any pattern in a list of patterns and
    contains the given content.

    Parameters
    ----------
    module : AnsibleModule
        The Ansible module object being used in the module.
    patterns : list[str]
        A list of data set patterns.
    content : str
        The content string to search for within matched data sets.

    Returns
    -------
    dict[ps=set, pds=dict[str, str], searched=int]
        A dictionary containing
        a set of matched "PS" data sets, a dictionary containing "PDS" data sets
        and members corresponding to each PDS, an int representing number of total
        data sets examined.

    Raises
    ------
        fail_json: Non-zero return code received while executing ZOAU shell command 'dgrep'.
    """
    filtered_data_sets = dict(ps=set(), pds=dict(), searched=0)
    for pattern in patterns:
        rc, out, err = _dgrep_wrapper(
            pattern, content=content, verbose=True, ignore_case=True
        )
        if rc > 4 and rc != 28:
            module.fail_json(
                msg="Non-zero return code received while executing ZOAU shell command 'dgrep'",
                rc=rc, stdout=out, stderr=err
            )
        for line in err.splitlines():
            if line and line.strip().startswith("BGYSC1005I"):
                filtered_data_sets['searched'] += 1

        for line in out.splitlines():
            if line:
                line = line.split()
                ds_name = line[0]
                if _ds_type(ds_name) == "PO":
                    try:
                        filtered_data_sets['pds'][ds_name].add(line[1])
                    except KeyError:
                        filtered_data_sets['pds'][ds_name] = set([line[1]])
                else:
                    filtered_data_sets['ps'].add(ds_name)
    return filtered_data_sets


def data_set_filter(module, patterns):
    """ Find data sets that match any pattern in a list of patterns.

    Parameters
    ----------
    module : AnsibleModule
        The Ansible module object being used.
    patterns : list[str]
        A list of data set patterns.

    Returns
    -------
    dict[ps=set, pds=dict[str, str], searched=int]
        A dictionary containing
        a set of matched "PS" data sets, a dictionary containing "PDS" data sets
        and members corresponding to each PDS, an int representing number of total
        data sets examined.

    Raises
    ------
    fail_json
        Non-zero return code received while executing ZOAU shell command 'dls'.
    """
    filtered_data_sets = dict(ps=set(), pds=dict(), searched=0)
    for pattern in patterns:
        rc, out, err = _dls_wrapper(pattern, list_details=True)
        if rc != 0:
            if "BGYSC1103E" in err:
                # return filtered_data_sets
                continue

            module.fail_json(
                msg="Non-zero return code received while executing ZOAU shell command 'dls'",
                rc=rc, stdout=out, stderr=err
            )
        for ds in err.splitlines():
            if ds and ds.strip().startswith("BGYSC1005I"):
                filtered_data_sets['searched'] += 1

        for line in out.splitlines():
            result = line.split()
            if result:
                if result[1] == "PO":
                    mls_rc, mls_out, mls_err = module.run_command(
                        f"mls '{result[0]}(*)'", errors='replace'
                    )
                    if mls_rc == 2:
                        filtered_data_sets["pds"][result[0]] = {}
                    else:
                        filtered_data_sets["pds"][result[0]] = \
                            set(filter(None, mls_out.splitlines()))
                else:
                    filtered_data_sets["ps"].add(result[0])
    return filtered_data_sets


def filter_members(module, members, excludes):
    """ Return all PDS/PDSE data sets whose members match any of the patterns
    in the given list of member patterns.
    Parameters
    ----------
    module : AnsibleModule
        The Ansible module object being used in the module.
    member : set
        A list of member patterns to on it.
    excludes : list
        The str value to filter members.
    Returns
    -------
    dict[str, set[str]]
        Filtered PDS/PDSE with corresponding members.
    """
    filtered_members = {
        member for member in members
        if not any(_match_regex(module, exclude, member) for exclude in excludes)
    }
    return filtered_members


def vsam_filter(module, patterns, vsam_types, age=None, excludes=None):
    """ Return all VSAM data sets that match any of the patterns
    in the given list of patterns.

    Parameters
    ----------
    module : AnsibleModule
        The Ansible module object being used.
    patterns : list[str]
        A list of data set patterns.

    Returns
    -------
    set[str]
        Matched VSAM data sets.

    Raises
    ------
    fail_json
        Non-zero return code received while executing ZOAU shell command 'vls'.
    """

    filtered_data_sets = list()
    now = time.time()
    examined = 0
    for pattern in patterns:
        request_details = age is not None
        rc, out, err = _vls_wrapper(pattern, details=request_details)
        if rc > 4:
            module.fail_json(
                msg="Non-zero return code received while executing ZOAU shell command 'vls'",
                rc=rc, stdout=out, stderr=err
            )
        for entry in out.splitlines():
            if entry:
                vsam_props = entry.split()
                vsam_name = vsam_props[0]
                vsam_ignore = False
                if excludes is not None:
                    for ex_pat in excludes:
                        if _match_regex(module, ex_pat, vsam_name):
                            vsam_ignore = True
                            break
                if not vsam_ignore:
                    vsam_type = vsam_name.split('.')[-1]
                    if vsam_type not in {"DATA", "INDEX"}:
                        examined = examined + 1
                    for type in vsam_types:
                        if _match_resource_type(type, vsam_type):
                            if age:
                                if _age_filter(vsam_props[1], now, age):
                                    filtered_data_sets.append({"name": vsam_name, "type": type})
                            else:
                                filtered_data_sets.append({"name": vsam_name, "type": type})
                            break
    return filtered_data_sets, examined


def migrated_vsam_filter(module, patterns, vsam_types, excludes):
    """ Return all migrated VSAM data sets that match any of the patterns
    in the given list of patterns.

    Parameters
    ----------
    module : AnsibleModule
        The Ansible module object being used.
    patterns : list[str]
        A list of data set patterns.

    Returns
    -------
    set[str]
        Matched VSAM data sets.

    Raises
    ------
    fail_json
        Non-zero return code received while executing ZOAU shell command 'vls'.
    """
    filtered_data_sets = list()
    examined = 0
    for pattern in patterns:
        # Fetch non-migrtated datasets
        nonmigrated_data_sets = set()
        nmrc, nmout, nmerr = _vls_wrapper(pattern)
        if nmrc > 4:
            module.fail_json(
                msg="Non-zero return code received while executing ZOAU shell command 'vls'",
                rc=nmrc, stdout=nmout, stderr=nmerr
            )
        for entry in nmout.splitlines():
            if entry:
                vsam_props = entry.split()
                vsam_name = vsam_props[0]
                for type in vsam_types:
                    vsam_type = vsam_name.split('.')[-1]
                    if _match_resource_type(type, vsam_type):
                        nonmigrated_data_sets.add(vsam_name)
        # Fetch migrated datasets
        rc, out, err = _vls_wrapper(pattern, migrated=True)
        if rc > 4:
            module.fail_json(
                msg="Non-zero return code received while executing ZOAU shell command 'vls'",
                rc=rc, stdout=out, stderr=err
            )
        # Compare migrated and non-migrated datasets and create list of only migrated datasets
        for entry in out.splitlines():
            if entry:
                vsam_props = entry.split()
                vsam_name = vsam_props[0]
                vsam_ignore = False
                if excludes is not None:
                    for ex_pat in excludes:
                        if _match_regex(module, ex_pat, vsam_name):
                            vsam_ignore = True
                            break
                if not vsam_ignore:
                    if vsam_name not in nonmigrated_data_sets:
                        vsam_type = vsam_name.split('.')[-1]
                        if vsam_type not in {"DATA", "INDEX"}:
                            examined = examined + 1
                        for type in vsam_types:
                            if _match_resource_type(type, vsam_type):
                                filtered_data_sets.append({"name": vsam_name, "type": "MIGRATED", "migrated_resource_type": type})
    return filtered_data_sets, examined


def data_set_attribute_filter(
    module, data_sets, size=None, age=None, age_stamp="creation_date"
):
    """ Filter data sets based on attributes such as age or size.

    Parameters
    ----------
    module : AnsibleModule
        The Ansible module object being used.
    data_sets : set[str]
        A set of data set names.
    size : int
        The size, in bytes, that should be used to filter data sets.
    age : int
        The age, in days, that should be used to filter data sets.

    Returns
    -------
    set[str]
        Matched data sets filtered by age and size.

    Raises
    ------
    fail_json
        Non-zero return code received while executing ZOAU shell command 'dls'.
    """
    filtered_data_sets = list()
    now = time.time()
    for ds in data_sets:
        rc, out, err = _dls_wrapper(
            ds, u_time=age is not None, size=size is not None
        )
        if rc != 0:
            # Continue when no matching datasets are found
            if "BGYSC1103E" in err:
                continue
            module.fail_json(
                msg="Non-zero return code received while executing ZOAU shell command 'dls'",
                rc=rc, stdout=out, stderr=err
            )
        out = out.strip().split()
        ds_age = out[1] if age_stamp == "ref_date" else _get_creation_date(module, ds)
        if (
            (
                age
                and size
                and _age_filter(ds_age, now, age)
                and _size_filter(int(out[6]), size)
            ) or
            (
                age and not size and _age_filter(ds_age, now, age)
            ) or
            (
                size and not age and _size_filter(int(out[6]), size)
            )
        ):
            filtered_data_sets.append(ds)
    return filtered_data_sets


def gdg_filter(module, data_sets, limit, empty, fifo, purge, scratch, extended, excludes):
    """ Filter Generation Data Groups based on their attributes.

    Parameters
    ----------
    module : AnsibleModule
        The Ansible module object being used.
    data_sets : set[str]
        A set of data set names.
    limit : int
        The limit GDG attribute that should be used to filter GDGs.
    empty : bool
        The empty GDG attribute, that should be used to filter GDGs.
    fifo : bool
        The fifo GDG attribute, that should be used to filter GDGs.
    purge : bool
        The purge GDG attribute, that should be used to filter GDGs.
    scratch : bool
        The scratch GDG attribute, that should be used to filter GDGs.
    extended : bool
        The extended GDG attribute, that should be used to filter GDGs.

    Returns
    -------
    set[str]
        Matched GDG base names.

    Raises
    ------
    fail_json
        Non-zero return code received while executing ZOAU shell command 'dls'.
    """
    filtered_data_sets = list()
    for ds in data_sets:
        rc, out, err = _dls_wrapper(ds, data_set_type='gdg', list_details=True, json=True)

        if rc != 0:
            # Continue when no matching datasets are found
            if "BGYSC1103E" in err:
                continue
            module.fail_json(
                msg="Non-zero return code received while executing ZOAU shell command 'dls'",
                rc=rc, stdout=out, stderr=err
            )
        try:
            response = json.loads(out)
            gdgs = response['data']['gdgs']
            for gdg in gdgs:
                if (
                    gdg['limit'] == (gdg['limit'] if limit is None else limit) and
                    gdg['empty'] == (gdg['empty'] if empty is None else empty) and
                    gdg['purge'] == (gdg['purge'] if purge is None else purge) and
                    gdg['fifo'] == (gdg['fifo'] if fifo is None else fifo) and
                    gdg['scratch'] == (gdg['scratch'] if scratch is None else scratch) and
                    gdg['extended'] == (gdg['extended'] if extended is None else extended)
                ):
                    if excludes is not None:
                        for ex_pat in excludes:
                            if not _match_regex(module, ex_pat, gdg['base']):
                                filtered_data_sets.append({"name": gdg['base'], "type": "GDG"})
                    else:
                        filtered_data_sets.append({"name": gdg['base'], "type": "GDG"})
        except Exception as e:
            module.fail_json(repr(e))

        return filtered_data_sets


# TODO:
# Use dls -m command output only when ZOAU adds volser/volume information also for datasets
def migrated_nonvsam_filter(module, data_sets, excludes):
    """ Filter Migrated Datasets by comparing results of dls -m and dls -l commands.

    Parameters
    ----------
    module : AnsibleModule
        The Ansible module object being used.
    data_sets : set[str]
        A set of data set names.

    Returns
    -------
    set[str]
        Matched Migrated datasets base names.

    Raises
    ------
    fail_json
        Non-zero return code received while executing ZOAU shell command 'dls'.
    """
    filtered_data_sets = list()
    for ds in data_sets:
        # Fetch active and migrated datasets
        rc, out, err = _dls_wrapper(ds, migrated=True)
        if rc != 0:
            # Continue when no matching datasets are found
            if "BGYSC1103E" in err:
                continue
            module.fail_json(
                msg="Non-zero return code received while executing ZOAU shell command 'dls'",
                rc=rc, stdout=out, stderr=err
            )
        try:
            # Fetch only active datasets
            init_filtered_data_sets = data_set_filter(
                module,
                [ds]
            )
            active_datasets = \
                init_filtered_data_sets.get("ps").union(set(init_filtered_data_sets['pds'].keys()))
            # Create a result list of datasets which are existing in migrated datasets list and
            # not existed in active datasets list
            for line in out.splitlines():
                ds = line.strip()
                if ds not in active_datasets:
                    if excludes is not None:
                        for ex_pat in excludes:
                            if not _match_regex(module, ex_pat, ds):
                                filtered_data_sets.append({"name": ds, "type": "MIGRATED", "migrated_resource_type": "NONVSAM"})
                                break
                    else:
                        filtered_data_sets.append({"name": ds, "type": "MIGRATED", "migrated_resource_type": "NONVSAM"})
        except Exception as e:
            module.fail_json(repr(e))

    return filtered_data_sets


# TODO:
# Implement volume_filter() using "vtocls" shell command from ZOAU
# when it becomes available.
def volume_filter(module, data_sets, volumes):
    """Return only the data sets that are allocated in one of the volumes from
    the list of input volumes.

    Parameters
    ----------
    module : AnsibleModule
        The Ansible module object.
    data_sets : set[str]
        A set of data sets to be filtered.
    volumes : list[str]
        A list of input volumes.

    Returns
    -------
    set[str]
        The filtered data sets.

    Raises
    ------
    fail_json
        Unable to retrieve VTOC information.
    """
    filtered_data_sets = list()
    for volume in volumes:
        vtoc_entry = vtoc.get_volume_entry(volume)
        if vtoc_entry:
            for ds in vtoc_entry:
                if ds.get('data_set_name') in data_sets:
                    filtered_data_sets.append(ds.get('data_set_name'))
        else:
            module.fail_json(
                msg="Unable to retrieve VTOC information for volume {0}".format(volume)
            )

    return filtered_data_sets


def exclude_data_sets(module, data_set_list, excludes):
    """Remove data sets that match any pattern in a list of patterns.

    Parameters
    ----------
    module : AnsibleModule
        The Ansible module object being used.
    data_set_list : set[str]
        A set of data sets to be filtered.
    excludes : list[str]
        A list of data set patterns to be excluded.

    Returns
    -------
    set[str]
        The remaining data sets that have not been excluded.
    """
    for ds in list(data_set_list):
        for ex_pat in excludes:
            if _match_regex(module, ex_pat, ds):
                data_set_list.remove(ds)
                break
    return data_set_list


def get_members_to_exclude(excludes):
    """Get from the excludes str any subject that is isndie () to get members to exclude

    Args
    ----
    excludes : str
        String of exlucions of the find operation including members

    Returns
    -------
    members_to_exclude [list]
        The patters of members to be exlude from the list
    datasets_to_exclude [list]
        The patters of datasets to be exlude from the list
    """
    members_to_exclude = []
    datasets_to_exclude = []
    for exclude in excludes:
        match = re.search(r'\(([^)]+)\)', exclude)
        if match:
            members = match.group(1)
            datasets = exclude[:match.start()] + exclude[match.end():]
            members_to_exclude.append(members)
            if datasets:
                datasets_to_exclude.append(datasets)
        else:
            if exclude:
                datasets_to_exclude.append(exclude)
    return members_to_exclude, datasets_to_exclude


def _age_filter(ds_date, now, age):
    """Determine whether a given date is older than 'age'.

    Parameters
    ----------
    ds_date : str
        The input date in the format YYYY/MM/DD.
    now : float
        The time elapsed since the last epoch.
    age : int
        The age, in days, to compare against.

    Returns
    -------
    bool
        Whether 'ds_date' is older than 'age'.
    """
    year, month, day = list(map(int, ds_date.split("/")))
    if year == "0000":
        return age >= 0

    # Seconds per day = 86400
    ds_age = datetime.datetime(year, month, day).timestamp()
    if age >= 0 and (now - ds_age) / 86400 >= abs(age):
        return True
    elif age < 0 and (now - ds_age) / 86400 <= abs(age):
        return True
    return False


def _get_creation_date(module, ds):
    """Retrieve the creation date for a given data set.

    Arguments
    ---------
    module : AnsibleModule
        The Ansible module object being used.
    ds : str
        The name of the data set.

    Returns
    -------
    str
        The data set creation date in the format "YYYY/MM/DD".

    Raises
    ------
    fail_json
        Non-zero return code received while retrieving data set age.
    """
    rc, out, err = mvs_cmd.idcams(
        "  LISTCAT ENT('{0}') HISTORY".format(ds), authorized=True
    )
    if rc != 0:
        module.fail_json(
            msg="Non-zero return code received while retrieving data set age",
            rc=rc, stderr=err, stdout=out
        )
    out = re.findall(r"CREATION-*[A-Z|0-9]*", out.strip())
    if out:
        out = out[0]
        date = "".join(re.findall(r"-[A-Z|0-9]*", out)).replace("-", "").split(".")
        days = 1 if len(date) < 2 else int(date[1])
        years = int(date[0])
        days_per_month = 30.4167
        return "{0}/{1}/{2}".format(
            years,
            math.ceil(days / days_per_month),
            math.ceil(days % days_per_month)
        )

    # If no creation data is found, return default "000/1/1"
    return "000/1/1"


def _size_filter(ds_size, size):
    """Determine whether a given size is greater than the input size.

    Parameters
    ----------
    ds_size : int
        The input size, in bytes.
    size : int
        The size, in bytes, to compare against.

    Returns
    -------
    bool
        Whether 'ds_size' is greater than 'age'.
    """
    if size >= 0 and ds_size >= abs(size):
        return True
    if size < 0 and ds_size <= abs(size):
        return True
    return False


def _match_regex(module, pattern, string):
    """Determine whether the input regex pattern matches the string.

    Parameters
    ----------
    module : AnsibleModule
        The Ansible module object being used.
    pattern : str
        The regular expression to match.
    string : str
        The string to match.

    Returns
    -------
    re.Match
        A Match object that matches the pattern to string.

    Raises
    ------
    fail_json
        Invalid regular expression.
    """
    try:
        return fullmatch(pattern, string, re.IGNORECASE)
    except re.error as err:
        module.fail_json(
            msg="Invalid regular expression '{0}'".format(pattern),
            stderr=repr(err)
        )


def _dgrep_wrapper(
    data_set_pattern,
    content,
    ignore_case=False,
    line_num=False,
    verbose=False,
    context=None
):
    """A wrapper for ZOAU 'dgrep' shell command.

    Parameters
    ----------
    data_set_pattern : str
        Data set pattern where to search for content.
    content : str
        Content to search across the data sets specified in data_set_pattern.
    ignore_case : bool
        Whether to ignore case or not.
    line_num : bool
        Whether to display line numbers.
    verbose : bool
        Extra verbosity, prints names of datasets being searched.
    context : int
        If context lines are requested, then up to <NUM> lines before and after the matching line are also printed.

    Returns
    -------
    tuple(int,str,str)
        Return code, standard output and standard error.
    """
    dgrep_cmd = "dgrep"
    if ignore_case:
        dgrep_cmd += " -i"
    if line_num:
        dgrep_cmd += " -n"
    if verbose:
        dgrep_cmd += " -v"
    if context:
        dgrep_cmd += " -C{0}".format(context)

    dgrep_cmd += " {0} {1}".format(quote(content), quote(data_set_pattern))
    return AnsibleModuleHelper(argument_spec={}).run_command(dgrep_cmd, errors='replace')


def _dls_wrapper(
    data_set_pattern,
    list_details=False,
    u_time=False,
    size=False,
    verbose=False,
    migrated=False,
    data_set_type="",
    json=False,
):
    """A wrapper for ZOAU 'dls' shell command.

    Parameters
    ----------
    data_set_pattern : str
        Data set pattern.
    list_details : bool
        Display detailed information based on the dataset type.
    u_time : bool
        Display last usage time.
    size : bool
        Display size in list.
    verbose : bool
        Display verbose information.
    migrated : bool
        Display migrated data sets.
    data_set_type : str
        Data set type to look for.
    json : bool
        Return content in json format.

    Returns
    -------
    tuple(int,str,str)
        Return code, standard output and standard error.
    """
    dls_cmd = "dls"
    if migrated:
        dls_cmd += " -m"
    else:
        if list_details:
            dls_cmd += " -l"
        if u_time:
            dls_cmd += " -u"
        if size:
            dls_cmd += " -s"
    if verbose:
        dls_cmd += " -v"
    if data_set_type:
        dls_cmd += f" -t{data_set_type}"
    if json:
        dls_cmd += " -j"

    dls_cmd += " {0}".format(quote(data_set_pattern))
    return AnsibleModuleHelper(argument_spec={}).run_command(dls_cmd, errors='replace')


def _vls_wrapper(pattern, details=False, migrated=False, verbose=False):
    """A wrapper for ZOAU 'vls' shell command.

    Parameters
    ----------
    pattern : str
        Data set pattern.
    details : bool
        Display detailed information based on the dataset type.
    verbose : bool
        Display verbose information.

    Returns
    -------
    tuple(int,str,str)
        Return code, standard output and standard error.
    """
    vls_cmd = "vls"
    if migrated:
        vls_cmd += " -m"
    else:
        if details:
            vls_cmd += " -l"
    if verbose:
        vls_cmd += " -v"

    vls_cmd += " {0}".format(quote(pattern))
    return AnsibleModuleHelper(argument_spec={}).run_command(vls_cmd, errors='replace')


def _match_resource_type(type1, type2):
    """Compare that the two types match.

    Parameters
    ----------
    type1 : str
        One of the types that are expected to match.
    type2 : str
        One of the types that are expected to match.

    Returns
    -------
    bool
        If the types match.
    """
    if type1 == type2:
        return True
    if type1 == "CLUSTER" and type2 not in ("DATA", "INDEX"):
        return True
    return False


def _ds_type(ds_name):
    """Utility function to determine the DSORG of a data set.

    Parameters
    ----------
    ds_name : str
        The name of the data set.

    Returns
    -------
    str
        The DSORG of the data set.
    """
    rc, out, err = mvs_cmd.ikjeft01(
        "  LISTDS '{0}'".format(ds_name),
        authorized=True
    )
    if rc == 0:
        search = re.search(r"(-|--)DSORG(-\s*|\s*)\n(.*)", out, re.MULTILINE)
        return search.group(3).split()[-1]
    return None


def run_module(module):
    """Initialize parameters.

    Parameters
    ----------
    module : AnsibleModule
        Ansible Module.

    Returns
    -------
    dict
        Arguments.

    Raises
    ------
    fail_json
        Failed to process age.
    fail_json
        Failed to process size.
    """
    validate_dependencies(module)

    # Initialize logging module
    module_verbosity_level = module._verbosity
    SingletonLogger().get_logger(module_verbosity_level)

    # Parameter initialization
    age = module.params.get('age')
    age_stamp = module.params.get('age_stamp')
    contains = module.params.get('contains')
    excludes = module.params.get('excludes') or module.params.get('exclude')
    patterns = module.params.get('patterns')
    size = module.params.get('size')
    resource_type = module.params.get('resource_type') or module.params.get('resource_types')
    resource_type = [type.upper() for type in resource_type]
    volume = module.params.get('volume') or module.params.get('volumes')
    limit = module.params.get('limit')
    empty = module.params.get('empty')
    scratch = module.params.get('scratch')
    purge = module.params.get('purge')
    extended = module.params.get('extended')
    migrated_type = module.params.get('migrated_type') or module.params.get('migrated_types')
    fifo = module.params.get('fifo')
    vsam_types = {"CLUSTER", "DATA", "INDEX"}
    res_args = dict(data_sets=[])
    examined_ds = 0
    filtered_resource_types = set()
    vsam_resource_types = set()
    filtered_migrated_types = set()
    vsam_migrated_types = set()

    excludes_datasets = exclude_members = []
    if excludes:
        exclude_members, excludes_datasets = get_members_to_exclude(excludes)
    for type in resource_type:
        if type in vsam_types:
            filtered_resource_types.add("VSAM")
            vsam_resource_types.add(type)
        else:
            filtered_resource_types.add(type)
    if "MIGRATED" in resource_type:
        migrated_type = [type.upper() for type in migrated_type]
        for type in migrated_type:
            if type in vsam_types:
                filtered_migrated_types.add("VSAM")
                vsam_migrated_types.add(type)
            else:
                filtered_migrated_types.add(type)
    if age:
        # convert age to days:
        m = re.match(r"^(-?\d+)(d|w|m|y)?$", age.lower())
        days_per_unit = {"d": 1, "w": 7, "m": 30, "y": 365}
        if m:
            age = int(m.group(1)) * days_per_unit.get(m.group(2), 1)
        else:
            module.fail_json(age=age, msg="failed to process age")

    if size:
        # convert size to bytes:
        m = re.match(r"^(-?\d+)(b|k|m|g|t)?$", size.lower())
        bytes_per_unit = {"b": 1, "k": 1024, "m": 1024**2, "g": 1024**3, "t": 1024**4}
        if m:
            size = int(m.group(1)) * bytes_per_unit.get(m.group(2), 1)
        else:
            module.fail_json(size=size, msg="failed to process size")
    for res_type in filtered_resource_types:
        examined = 0
        filtered_data_sets = list()
        init_filtered_data_sets = dict()
        if res_type == "MIGRATED":
            migrated_data_sets = list()
            for mtype in filtered_migrated_types:
                if mtype == "NONVSAM":
                    migrated_data_sets = migrated_nonvsam_filter(module, patterns, excludes)
                elif mtype == "VSAM":
                    migrated_data_sets, examined = migrated_vsam_filter(module, patterns, vsam_migrated_types, excludes)
                filtered_data_sets = filtered_data_sets + migrated_data_sets
        if res_type == "NONVSAM":
            if contains:
                init_filtered_data_sets = content_filter(
                    module,
                    patterns,
                    contains
                )
            else:
                init_filtered_data_sets = data_set_filter(
                    module,
                    patterns
                )
            filtered_data_sets = \
                list(init_filtered_data_sets.get("ps").union(set(init_filtered_data_sets['pds'].keys())))
            if len(excludes_datasets) > 0:
                filtered_data_sets = exclude_data_sets(module, filtered_data_sets, excludes_datasets)
            # Filter data sets by age or size
            if size or age:
                filtered_data_sets = data_set_attribute_filter(
                    module, filtered_data_sets, size=size, age=age, age_stamp=age_stamp
                )
            # Filter data sets by volume
            if volume:
                filtered_data_sets = volume_filter(module, filtered_data_sets, volume)
            examined = init_filtered_data_sets.get("searched")
        elif res_type == "VSAM":
            filtered_data_sets, examined = vsam_filter(module, patterns, vsam_resource_types, age=age, excludes=excludes)
        elif res_type == "GDG":
            filtered_data_sets = gdg_filter(module, patterns, limit, empty, fifo, purge, scratch, extended, excludes)
        if filtered_data_sets:
            for ds in filtered_data_sets:
                if ds:
                    if res_type == "NONVSAM":
                        members = init_filtered_data_sets['pds'].get(ds)
                        if members:
                            if len(exclude_members) > 0:
                                members = filter_members(module, members, exclude_members)
                            res_args['data_sets'].append(
                                dict(name=ds, members=members, type=res_type)
                            )
                        else:
                            res_args['data_sets'].append(dict(name=ds, type=res_type))
                    else:
                        res_args['data_sets'].append(ds)
        examined_ds = examined_ds + examined
    res_args['examined'] = examined_ds
    res_args['matched'] = len(res_args['data_sets'])
    return res_args


def main():
    """Initialize module when it's run as main.

    Raises
    ------
    fail_json
        Parameter verification failed.
    """
    module = AnsibleModule(
        argument_spec=dict(
            age=dict(type="str", required=False),
            age_stamp=dict(
                type="str",
                required=False,
                choices=["creation_date", "ref_date"],
                default="creation_date"
            ),
            contains=dict(type="str", required=False),
            excludes=dict(
                type="list",
                elements="str",
                required=False,
                aliases=["exclude"]
            ),
            patterns=dict(
                type="list",
                elements="str",
                required=True
            ),
            size=dict(type="str", required=False),
            resource_type=dict(
                type="list",
                elements="str",
                required=False,
                default=["nonvsam"],
                choices=["cluster", "data", "index", "nonvsam", "gdg", "migrated"],
                aliases=["resource_types"]
            ),
            migrated_type=dict(
                type="list",
                required=False,
                elements="str",
                default=["cluster", "data", "index", "nonvsam"],
                choices=["cluster", "data", "index", "nonvsam"],
                aliases=["migrated_types"]
            ),
            volume=dict(
                type="list",
                elements="str",
                required=False,
                aliases=["volumes"]
            ),
            limit=dict(type="int", required=False),
            empty=dict(type="bool", required=False),
            purge=dict(type="bool", required=False),
            scratch=dict(type="bool", required=False),
            extended=dict(type="bool", required=False),
            fifo=dict(type="bool", required=False),
        )
    )

    arg_def = dict(
        age=dict(arg_type="str", required=False),
        age_stamp=dict(
            arg_type="str",
            required=False,
            choices=["creation_date", "ref_date"],
            default="creation_date"
        ),
        contains=dict(arg_type="str", required=False),
        excludes=dict(arg_type="list", required=False, aliases=["exclude"]),
        patterns=dict(arg_type="list", required=True),
        size=dict(arg_type="str", required=False),
        resource_type=dict(
            arg_type="list",
            required=False,
            default=["nonvsam"],
            aliases=["resource_types"]
        ),
        migrated_type=dict(
            arg_type="list",
            required=False,
            default=["cluster", "data", "index", "nonvsam"],
            aliases=["migrated_types"]
        ),
        volume=dict(arg_type="list", required=False, aliases=["volumes"]),
        limit=dict(type="int", required=False),
        empty=dict(type="bool", required=False),
        purge=dict(type="bool", required=False),
        scratch=dict(type="bool", required=False),
        extended=dict(type="bool", required=False),
        fifo=dict(type="bool", required=False),
    )
    try:
        BetterArgParser(arg_def).parse_args(module.params)
    except ValueError as err:
        module.fail_json(
            msg="Parameter verification failed", stderr=str(err)
        )
    module.exit_json(**run_module(module))


if __name__ == '__main__':
    main()
