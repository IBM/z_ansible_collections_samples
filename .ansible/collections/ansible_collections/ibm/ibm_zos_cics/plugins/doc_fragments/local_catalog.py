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
      - The size of the primary space allocated to the local catalog data set.
        Note that this is just the value; the unit is specified with O(space_type).
      - This option takes effect only when the local catalog data set is being created.
        If the local catalog data set already exists, the option has no effect.
    type: int
    required: false
    default: 200
  space_secondary:
    description:
      - The size of the secondary space allocated to the local catalog data set.
        Note that this is just the value; the unit is specified with O(space_type).
      - This option takes effect only when the local catalog data set is being created.
        If the local catalog data set already exists, the option has no effect.
    type: int
    required: false
    default: 5
  space_type:
    description:
      - The unit portion of the local catalog data set size. Note that this is
        just the unit; the value for the primary space is specified with O(space_primary) and
        the value for the secondary space is specified with O(space_secondary).
      - This option takes effect only when the local catalog data set is being created.
        If the local catalog data set already exists, the option has no effect.
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
    default: REC
  volumes:
    description:
      - The volume(s) where the data set is created. Use a string to define a singular volume or a list of strings for multiple volumes.
    type: raw
    required: false
  region_data_sets:
    description:
      - The location of the region data sets to be created by using a template, for example,
        C(REGIONS.ABCD0001.<< data_set_name >>).
      - If you want to use a data set that already exists, ensure that the data set is a local catalog data set.
    type: dict
    required: true
    suboptions:
      template:
        description:
          - The base location of the region data sets with a template.
        required: false
        type: str
      dfhlcd:
        description:
          - Overrides the templated location for the local catalog data set.
        required: false
        type: dict
        suboptions:
          dsn:
            description:
              - The data set name of the local catalog to override the template.
            type: str
            required: false
  cics_data_sets:
    description:
      - The name of the C(SDFHLOAD) library of the CICS installation, for example, C(CICSTS61.CICS.SDFHLOAD).
      - This module uses the C(DFHCCUTL) utility internally, which is found in the C(SDFHLOAD) library.
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
      - The intended state for the local catalog, which the module aims to achieve.
      - Specify V(absent) to remove the local catalog data set entirely, if it already exists.
      - Specify V(initial) to create the local catalog data set if it does not exist,
        or empty this existing local catalog of all records.
      - Specify V(warm) to retain an existing local catalog in its current state.
        The module verifies whether the specified data set exists and whether it contains any records.
        If both conditions are met, the module leaves the data set as is.
        If the data set does not exist or if it is empty, the operation fails.
    choices:
      - "initial"
      - "absent"
      - "warm"
    required: true
    type: str
  """
