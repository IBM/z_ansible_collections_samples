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
      - The size of the primary space allocated to the CSD.
        Note that this is just the value; the unit is specified with O(space_type).
      - This option takes effect only when the CSD is being created.
        If the CSD already exists, the option has no effect.
    type: int
    required: false
    default: 4
  space_secondary:
    description:
      - The size of the secondary space allocated to the CSD.
        Note that this is just the value; the unit is specified with O(space_type).
      - This option takes effect only when the CSD is being created.
        If the CSD already exists, the option has no effect.
    type: int
    required: false
    default: 1
  space_type:
    description:
      - The unit portion of the CSD size. Note that this is
        just the unit; the value for the primary space is specified with O(space_primary)
        and the value for the secondary space is specified with O(space_secondary).
      - This option takes effect only when the CSD is being created.
        If the CSD already exists, the option has no effect.
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
    type: dict
    required: true
    suboptions:
      template:
        description:
          - The base location of the region data sets with a template.
        required: false
        type: str
      dfhcsd:
        description:
          - Overrides the templated location for the CSD.
        required: false
        type: dict
        suboptions:
          dsn:
            description:
              - The data set name of the CSD to override the template.
            type: str
            required: false
  cics_data_sets:
    description:
      - The name of the C(SDFHLOAD) library of the CICS installation, for example, C(CICSTS61.CICS.SDFHLOAD).
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
      - The intended state for the CSD, which the module aims to achieve.
      - Specify V(absent) to remove the CSD entirely, if it already exists.
      - Specify V(initial) to create the CSD if it does not already exist, and initialize it by using DFHCSDUP.
      - Specify V(warm) to retain an existing CSD in its current state.
        The module verifies whether the specified data set exists and whether it contains any records.
        If both conditions are met, the module leaves the data set as is.
        If the data set does not exist or if it is empty, the operation fails.
      - Specify V(changed) to run a DFHCSDUP script to update an existing CSD.
    choices:
      - "initial"
      - "absent"
      - "warm"
      - "changed"
    required: true
    type: str
  input_location:
    description:
      - The type of location from which to load the DFHCSDUP script.
      - Specify V(DATA_SET) to load from a PDS, PDSE, or sequential data set.
      - Specify V(USS) to load from a file on UNIX System Services (USS).
      - Specify V(LOCAL) to load from a file local to the Ansible control node.
      - Specify V(INLINE) to allow a script to be passed directly through the O(input_content) parameter.
    choices:
      - "DATA_SET"
      - "USS"
      - "LOCAL"
      - "INLINE"
    type: str
    required: false
    default: "DATA_SET"
  input_src:
    description:
      - The path to the source file that contains the DFHCSDUP script to submit.
      - 'It can be a data set. For example: "TESTER.DEFS.SCRIPT" or "TESTER.DEFS(SCRIPT)"'
      - 'It can be a USS file. For example: "/u/tester/defs/script.csdup"'
      - 'It can be a local file. For example: "/User/tester/defs/script.csdup"'
    type: str
    required: false
  input_content:
    description:
      - The content of the DFHCSDUP script to submit, if you are using the O(input_location=INLINE) option.
    type: str
    required: false
  log:
    description:
      - Specify the recovery attribute for the CSD, overriding the CSD system initialization parameters.
      - Specify NONE for a nonrecoverable CSD.
      - Specify UNDO for a CSD that is limited to file backout only.
      - Specify ALL for a CSD for which you want both forward recovery and file backout. If you specify O(log=ALL), you
        must also specify LOGSTREAMID to identify the 26-character name of the z/OS™ log stream to be used as the
        forward recovery log. The CICS collection does not support defining forward recovery log streams; you
        must follow the instructions in L(Defining forward recovery log streams,
        https://www.ibm.com/docs/en/cics-ts/latest?topic=journaling-defining-forward-recovery-log-streams).
    choices:
      - "NONE"
      - "UNDO"
      - "ALL"
    required: false
    type: str
  logstream_id:
    description:
      - The 26-character name of the z/OS™ log stream to be used as the forward recovery log.
      - This is required when you use O(log=ALL).
    type: str
    required: false
  """
