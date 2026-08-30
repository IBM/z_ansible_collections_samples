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
      - The size of the primary space allocated to the auxiliary temporary storage data set.
        Note that this is just the value; the unit is specified with O(space_type).
      - This option takes effect only when the auxiliary temporary storage data set is being created.
        If the data set already exists, the option has no effect.
    type: int
    required: false
    default: 200
  space_secondary:
    description:
      - The size of the secondary space allocated to the auxiliary temporary storage data set.
        Note that this is just the value; the unit is specified with O(space_type).
      - This option takes effect only when the auxiliary temporary storage data set is being created.
        If the data set already exists, the option has no effect.
    type: int
    required: false
    default: 10
  space_type:
    description:
      - The unit portion of the auxiliary temporary storage data set size. Note that this is
        just the unit; the value for the primary space is specified with O(space_primary) and the value
        for the secondary space is specified with O(space_secondary).
      - This option takes effect only when the auxiliary temporary storage data set is being created.
        If the data set already exists, the option has no effect.
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
      - If you want to use a data set that already exists, ensure that the data set is an auxiliary temporary storage data set.
    type: dict
    required: true
    suboptions:
      template:
        description:
          - The base location of the region data sets with a template.
        required: false
        type: str
      dfhtemp:
        description:
          - Overrides the templated location for the auxiliary temporary storage data set.
        required: false
        type: dict
        suboptions:
          dsn:
            description:
              - The data set name of the auxiliary temporary storage to override the template.
            type: str
            required: false
  state:
    description:
      - The intended state for the auxiliary temporary storage data set, which the module aims to
        achieve.
      - Specify V(absent) to remove the auxiliary temporary storage data set entirely, if it
        already exists.
      - Specify V(initial) to create the auxiliary temporary storage data set, if it does not exist.
        If the specified data set exists but is empty, the module leaves the data set as is.
        If the specified data set exists and has contents, the module deletes the data set and then creates a new, empty data set.
      - Specify V(warm) to retain an existing auxiliary temporary storage data set in its current state.
        The module checks whether the specified data set exists, and if it does, leaves the data set as is.
        If the data set does not exist, the operation fails.
    choices:
      - "initial"
      - "absent"
      - "warm"
    required: true
    type: str
"""
