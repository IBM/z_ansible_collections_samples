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
      - The size of the primary space allocated to the auxiliary trace data set.
        Note that this is just the value; the unit is specified with O(space_type).
      - This option takes effect only when the auxiliary trace data set is being created.
        If the data set already exists, the option has no effect.
    type: int
    required: false
    default: 20
  space_secondary:
    description:
      - The size of the secondary space allocated to the auxiliary trace data set.
        Note that this is just the value; the unit is specified with O(space_type).
      - This option takes effect only when the auxiliary trace data set is being created.
        If the data set already exists, the option has no effect.
    type: int
    required: false
    default: 4
  space_type:
    description:
      - The unit portion of the auxiliary trace data set size. Note that this is
        just the unit; the value for the primary space is specified with O(space_primary) and
        the value for the secondary space is specified with O(space_secondary).
      - This option takes effect only when the auxiliary trace data set is being created.
        If the data set already exists, the option has no effect.
      - The size can be specified in megabytes (V(M)), kilobytes (V(K)),
        cylinders (V(CYL)), or tracks (V(TRK)).
    required: false
    type: str
    choices:
      - M
      - K
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
      - If you want to use a data set that already exists, ensure that the data set is an auxiliary trace data set.
    type: dict
    required: true
    suboptions:
      template:
        description:
          - The base location of the region data sets with a template.
        required: false
        type: str
      dfhauxt:
        description:
          - Overrides the templated location for the DFHAUXT data set.
        required: false
        type: dict
        suboptions:
          dsn:
            description:
              - The data set name of DFHAUXT to override the template.
            type: str
            required: false
      dfhbuxt:
        description:
          - Overrides the templated location for the DFHBUXT data set.
        required: false
        type: dict
        suboptions:
          dsn:
            description:
              - The data set name of DFHBUXT to override the template.
            type: str
            required: false
  destination:
    description:
      - Identify which one of the auxiliary trace data sets is the target of the operation.
        If the value is left blank, A is implied, but you can specify A or B.
      - Specify V(A) to create or delete the A data set.
      - Specify V(B) to create or delete the B data set. This MUST be set for the creation of the B data set.
    choices:
      - "A"
      - "B"
    type: str
    required: false
    default: "A"
  state:
    description:
      - The intended state for the auxiliary trace data set, which the module aims to achieve.
      - Specify V(absent) to remove the auxiliary trace data set data set entirely, if it exists.
      - Specify V(initial) to create the auxiliary trace data set if it does not exist.
        If the specified data set exists but is empty, the module leaves the data set as is.
        If the specified data set exists and has contents, the module deletes the data set and then creates a new, empty one.
      - Specify V(warm) to retain an existing auxiliary trace data set in its current state.
        The module checks whether the specified data set exists, and if it does, leaves the data set as is.
        If the data set does not exist, the operation fails.
    choices:
      - "initial"
      - "absent"
      - "warm"
    required: true
    type: str
  """
