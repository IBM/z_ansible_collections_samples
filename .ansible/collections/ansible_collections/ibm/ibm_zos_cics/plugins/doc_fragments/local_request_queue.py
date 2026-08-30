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
      - The size of the primary space allocated to the local request queue data set.
        Note that this is just the value; the unit is specified with O(space_type).
      - This option takes effect when the local request queue data set is being created.
        If the data set already exists, the option has no effect.
    type: int
    required: false
    default: 4
  space_secondary:
    description:
      - The size of the secondary space allocated to the local request queue data set.
        Note that this is just the value; the unit is specified with O(space_type).
      - This option takes effect when the local request queue data set is being created.
        If the data set already exists, the option has no effect.
    type: int
    required: false
    default: 1
  space_type:
    description:
      - The unit portion of the local request queue data set size. Note that this is
        just the unit; the value for the primary space is specified with O(space_primary) and
        the value for the secondary space is specified with O(space_secondary).
      - This option takes effect only when the local request queue data set is being created.
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
      - If you want to use a data set that already exists, ensure that the data set is a local request queue data set.
    type: dict
    required: true
    suboptions:
      template:
        description:
          - The base location of the region data sets with a template.
        required: false
        type: str
      dfhlrq:
        description:
          - Overrides the templated location for the local request queue data set.
        required: false
        type: dict
        suboptions:
          dsn:
            description:
              - The data set name of the local request queue to override the template.
            type: str
            required: false
  state:
    description:
      - The intended state for the local request queue, which the module aims to achieve.
      - Specify V(absent) to remove the local request queue data set entirely, if it exists.
      - Specify V(initial) to create the local request queue data set if it does not exist,
        or empty this existing local request queue of all records.
      - Specify V(warm) to retain an existing local request queue data set in its current state.
        The module checks whether the specified data set exists, and if it does, leaves the data set as is.
        If the data set does not exist, the operation fails.
    choices:
      - "initial"
      - "absent"
      - "warm"
    required: true
    type: str
  """
