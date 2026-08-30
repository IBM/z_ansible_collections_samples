# -*- coding: utf-8 -*-

# (c) Copyright IBM Corp. 2024
# Apache License, Version 2.0 (see https://opensource.org/licenses/Apache-2.0)

from __future__ import (absolute_import, division, print_function)
__metaclass__ = type


class ModuleDocFragment(object):

    DOCUMENTATION = r"""
options:
  space_primary:
    description:
      - The size of the primary space allocated to the global catalog data set.
        Note that this is just the value; the unit is specified with O(space_type).
      - This option takes effect only when the global catalog data set is being created.
        If the global catalog data set already exists, the option has no effect.
    type: int
    required: false
    default: 5
  space_secondary:
    description:
      - The size of the secondary space allocated to the global catalog data set.
        Note that this is just the value; the unit is specified with O(space_type).
      - This option takes effect only when the global catalog data set is being created.
        If the global catalog data set already exists, the option has no effect.
    type: int
    required: false
    default: 1
  space_type:
    description:
      - The unit portion of the global catalog data set size. Note that this is
        just the unit; the value for the primary space is specified with O(space_primary) and
        the value for the secondary space is specified with O(space_secondary).
      - This option takes effect only when the global catalog data set is being created.
        If the global catalog data set already exists, the option has no effect.
      - The size can be specified in megabytes (V(M)), kilobytes (V(K)),
        records (V(REC)), cylinders (V(CYL)), or tracks (V(TRK)).
    required: false
    type: str
    choices:
      - M
      - K
      - REC
      - CYL
      - TRK
    default: M
  volumes:
    description:
      - The volume(s) where the data set is created. Use a string to define a singular volume or a list of strings for multiple volumes.
    type: raw
    required: false
  region_data_sets:
    description:
      - The location of the region data sets to be created by using a template, for example,
        C(REGIONS.ABCD0001.<< data_set_name >>).
      - If you want to use a data set that already exists, ensure that the data set is a global catalog data set.
    type: dict
    required: true
    suboptions:
      template:
        description:
          - The base location of the region data sets with a template.
        required: false
        type: str
      dfhgcd:
        description:
          - Overrides the templated location for the global catalog data set.
        required: false
        type: dict
        suboptions:
          dsn:
            description:
              - The data set name of the global catalog to override the template.
            type: str
            required: false
  cics_data_sets:
    description:
      - The name of the C(SDFHLOAD) library of the CICS installation, for example, C(CICSTS61.CICS.SDFHLOAD).
      - This module uses the C(DFHRMUTL) utility internally, which is found in the C(SDFHLOAD) library.
    type: dict
    required: true
    suboptions:
      template:
        description:
          - The templated location of the C(SDFHLOAD) library.
        required: false
        type: str
      sdfhload:
        description:
          - The location of the C(SDFHLOAD) library. If O(cics_data_sets.template) is provided, this value overrides the template.
        type: str
        required: false
  state:
    description:
      - The intended state for the global catalog data set, which the module aims to achieve.
      - Specify V(absent) to remove the global catalog data set entirely, if it exists.
      - Specify V(initial) to set the autostart override record to C(AUTOINIT).
        If the specified global catalog data set does not already exist, the module creates the data set.
      - Specify V(cold) to set the autostart override record of an existing global catalog to C(AUTOCOLD).
        If the specified global catalog data set does not already exist, the operation fails.
      - Specify V(warm) to set the autostart override record of an existing global catalog to C(AUTOASIS),
        undoing any previous setting of C(AUTOINIT) or C(AUTOCOLD). The module verifies whether the specified
        data set exists and whether it contains any records. If either condition is not met, the operation fails.
    choices:
      - "absent"
      - "initial"
      - "cold"
      - "warm"
    required: true
    type: str
  """
