#!/usr/bin/python
# -*- coding: utf-8 -*-

# Copyright (c) IBM Corporation 2019, 2025
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

__metaclass__ = type


DOCUMENTATION = r"""
module: zos_data_set
version_added: "1.3.0"
short_description: Manage data sets
description:
  - Create, delete and set attributes of data sets.
  - When forcing data set replacement, contents will not be preserved.
author:
  - "Blake Becker (@blakeinate)"
  - "Rich Parker (@richp405)"
options:
  name:
    description:
      - The name of the data set being managed. (e.g C(USER.TEST))
      - If I(name) is not provided, a randomized data set name will be generated
        with the HLQ matching the module-runners username.
      - Required if I(type=member) or I(state!=present) and not using I(batch).
    type: str
    required: false
  state:
    description:
      - The final state desired for specified data set.
      - >
        If I(state=absent) and the data set does not exist on the managed node,
        no action taken, module completes successfully with I(changed=False).
      - >
        If I(state=absent) and the data set does exist on the managed node,
        remove the data set, module completes successfully with I(changed=True).
      - >
        If I(state=absent) and I(type=member) and I(force=True), the data set
        will be opened with I(DISP=SHR) such that the entire data set can be
        accessed by other processes while the specified member is deleted.
      - >
        If I(state=absent) and I(volumes) is provided, and the data set is not
        found in the catalog, the module attempts to perform catalog using supplied
        I(name) and I(volumes). If the attempt to catalog the data set catalog is successful,
        then the data set is removed. Module completes successfully with I(changed=True).
      - >
        If I(state=absent) and I(volumes) is provided, and the data set is not
        found in the catalog, the module attempts to perform catalog using supplied
        I(name) and I(volumes). If the attempt to catalog the data set catalog fails,
        then no action is taken. Module completes successfully with I(changed=False).
      - >
        If I(state=absent) and I(volumes) is provided, and the data set is found in
        the catalog, the module compares the catalog volume attributes to the provided
        I(volumes). If the volume attributes are different, the cataloged data set
        will be uncataloged temporarily while the requested data set be deleted is cataloged.
        The module will catalog the original data set on completion, if the attempts to
        catalog fail, no action is taken. Module completes successfully with I(changed=False).
      - >
        If I(state=absent) and I(type=gdg) and the GDG base has active generations the module
        will complete successfully with I(changed=False). To remove it option I(force) needs
        to be used. If the GDG base does not have active generations the module will complete
        successfully with I(changed=True).
      - >
        If I(state=present) and the data set does not exist on the managed node,
        create and catalog the data set, module completes successfully with I(changed=True).
      - >
        If I(state=present) and I(replace=True) and the data set is present on
        the managed node the existing data set is deleted, and a new data set is created and cataloged
        with the desired attributes, module completes successfully with I(changed=True).
      - >
        If I(state=present) and I(replace=False) and the data set is present
        on the managed node, no action taken, module completes successfully with I(changed=False).
      - >
        If I(state=present) and I(type=member) and the member does not exist in the data set,
        create a member formatted to store data, module completes successfully with I(changed=True).
        Note, a PDSE does not allow a mixture of formats such that there is
        executables (program objects) and data. The member created is formatted to store data,
        not an executable.
      - >
        If I(state=cataloged) and I(volumes) is provided and the data set is already cataloged,
        no action taken, module completes successfully with I(changed=False).
      - >
        If I(state=cataloged) and I(volumes) is provided and the data set is not cataloged,
        module attempts to perform catalog using supplied I(name) and I(volumes). If the attempt to
        catalog the data set catalog is successful, module completes successfully with I(changed=True).
      - >
        If I(state=cataloged) and I(volumes) is provided and the data set is not cataloged,
        module attempts to perform catalog using supplied I(name) and I(volumes). If the attempt to
        catalog the data set catalog fails, returns failure with I(changed=False).
      - >
        If I(state=uncataloged) and the data set is not found, no action taken,
        module completes successfully with I(changed=False).
      - >
        If I(state=uncataloged) and the data set is found, the data set is uncataloged,
        module completes successfully with I(changed=True).
      - >
        If I(state=present), the data set is already cataloged and I(volumes) is provided,
        the module will compare the volumes where it is cataloged against the provided I(volumes).
        If they don't match, the module will fail with an error indicating the data set is cataloged
        on a different volume.
        To resolve this, you must first uncatalog the data set before creating it on the new volume.
      - >
        If I(state=present), the data set is already cataloged, I(volumes) is provided,
        and the volumes match exactly, no action is taken and the module completes successfully
        with I(changed=False).
    required: false
    type: str
    default: present
    choices:
      - present
      - absent
      - cataloged
      - uncataloged
  type:
    description:
      - The data set type to be used when creating a data set. (e.g C(pdse)).
      - C(member) expects to be used with an existing partitioned data set.
      - Choices are case-sensitive.
    required: false
    type: str
    choices:
      - ksds
      - esds
      - rrds
      - lds
      - seq
      - pds
      - pdse
      - library
      - basic
      - large
      - member
      - hfs
      - zfs
      - gdg
    default: pds
  space_primary:
    description:
      - The amount of primary space to allocate for the dataset.
      - The unit of space used is set using I(space_type).
    type: int
    required: false
    default: 5
  space_secondary:
    description:
      - The amount of secondary space to allocate for the dataset.
      - The unit of space used is set using I(space_type).
    type: int
    required: false
    default: 3
  space_type:
    description:
      - The unit of measurement to use when defining primary and secondary space.
      - Valid units of size are C(k), C(m), C(g), C(cyl), and C(trk).
    type: str
    choices:
      - k
      - m
      - g
      - cyl
      - trk
    required: false
    default: m
  record_format:
    description:
      - The format of the data set. (e.g C(FB))
      - Choices are case-sensitive.
      - When I(type=ksds), I(type=esds), I(type=rrds), I(type=lds) or I(type=zfs)
        then I(record_format=None), these types do not have a default
        I(record_format).
    required: false
    choices:
      - fb
      - vb
      - fba
      - vba
      - u
      - f
    type: str
    default: fb
    aliases:
      - format
  sms_storage_class:
    description:
      - The storage class for an SMS-managed dataset.
      - Required for SMS-managed datasets that do not match an SMS-rule.
      - Not valid for datasets that are not SMS-managed.
      - Note that all non-linear VSAM datasets are SMS-managed.
    type: str
    required: false
    aliases:
      - data_class
  sms_data_class:
    description:
      - The data class for an SMS-managed dataset.
      - Optional for SMS-managed datasets that do not match an SMS-rule.
      - Not valid for datasets that are not SMS-managed.
      - Note that all non-linear VSAM datasets are SMS-managed.
    type: str
    required: false
  sms_management_class:
    description:
      - The management class for an SMS-managed dataset.
      - Optional for SMS-managed datasets that do not match an SMS-rule.
      - Not valid for datasets that are not SMS-managed.
      - Note that all non-linear VSAM datasets are SMS-managed.
    type: str
    required: false
  record_length:
    description:
      - The length, in bytes, of each record in the data set.
      - For variable data sets, the length must include the 4-byte prefix area.
      - "Defaults vary depending on format: If FB/FBA 80, if VB/VBA 137, if U 0."
    type: int
    required: false
  block_size:
    description:
      - The block size to use for the data set.
    type: int
    required: false
  directory_blocks:
    description:
      - The number of directory blocks to allocate to the data set.
    type: int
    required: false
  key_offset:
    description:
      - The key offset to use when creating a KSDS data set.
      - I(key_offset) is required when I(type=ksds).
      - I(key_offset) should only be provided when I(type=ksds)
    type: int
    required: false
  key_length:
    description:
      - The key length to use when creating a KSDS data set.
      - I(key_length) is required when I(type=ksds).
      - I(key_length) should only be provided when I(type=ksds)
    type: int
    required: false
  empty:
    description:
      - Sets the I(empty) attribute for Generation Data Groups.
      - If false, removes only the oldest GDS entry when a new GDS is created that causes GDG limit to be exceeded.
      - If true, removes all GDS entries from a GDG base when a new GDS is created that causes the
        GDG limit to be exceeded.
    type: bool
    required: false
    default: false
  extended:
    description:
      - Sets the I(extended) attribute for Generation Data Groups.
      - If false, allow up to 255 generation data sets (GDSs) to be associated with the GDG.
      - If true, allow up to 999 generation data sets (GDS) to be associated with the GDG.
    type: bool
    required: false
    default: false
  fifo:
    description:
      - Sets the I(fifo) attribute for Generation Data Groups.
      - If false, the order is the newest GDS defined to the oldest GDS. This is the default value.
      - If true, the order is the oldest GDS defined to the newest GDS.
    type: bool
    required: false
    default: false
  limit:
    description:
      - Sets the I(limit) attribute for Generation Data Groups.
      - Specifies the maximum number, from 1 to 255(up to 999 if extended), of GDS that can be
        associated with the GDG being defined.
      - I(limit) is required when I(type=gdg).
    type: int
    required: false
  purge:
    description:
      - Sets the I(purge) attribute for Generation Data Groups.
      - Specifies whether to override expiration dates when a generation data set (GDS)
        is rolled off and the C(scratch) option is set.
    type: bool
    required: false
    default: false
  scratch:
    description:
      - "When C(state=absent), specifies whether to physically remove the data set from the volume."
      - If C(scratch=true), the data set is deleted and its entry is removed from the volume's VTOC.
      - If C(scratch=false), the data set is uncataloged but not physically removed from the volume.
        This is the equivalent of using C(NOSCRATCH) in an C(IDCAMS DELETE) command.
      - When C(state=present) option B(scratch) sets the I(scratch) attribute for Generation Data Groups and is
        ignored for any other data set type.
      - When C(state=present) and C(type=GDG) specifies what action is to be taken for a generation data set located on disk
        volumes when the data set is uncataloged from the GDG base as a result of
        EMPTY/NOEMPTY processing.
    type: bool
    required: false
  volumes:
    description:
      - >
        If cataloging a data set, I(volumes) specifies the name of the volume(s) where the data set is located.
      - >
        If creating a data set, I(volumes) specifies the volume(s) where the data set should be created.
      - >
        If I(volumes) is provided when I(state=present), and the data set is not found in the catalog,
        L(zos_data_set,./zos_data_set.html) will check the volume table of contents to see if the data set exists.
        If the data set does exist, it will be cataloged.
      - >
        If I(volumes) is provided when I(state=absent) and the data set is not found in the catalog,
        L(zos_data_set,./zos_data_set.html) will check the volume table of contents to see if the data set exists.
        If the data set does exist, it will be cataloged and promptly removed from the system.
      - I(volumes) is required when I(state=cataloged).
      - Accepts a string when using a single volume and a list of strings when using multiple.
    type: raw
    required: false
    aliases:
      - volume
  replace:
    description:
      - When I(replace=True), and I(state=present), existing data set matching I(name) will be replaced.
      - >
        Replacement is performed by deleting the existing data set and creating a new data set with the same name and desired
        attributes. Since the existing data set will be deleted prior to creating
        the new data set, no data set will exist if creation of the new data set fails.
      - If I(replace=True), all data in the original data set will be lost.
    type: bool
    required: false
    default: false
  tmp_hlq:
    description:
      - Override the default high level qualifier (HLQ) for temporary and backup
        datasets.
      - The default HLQ is the Ansible user used to execute the module and if
        that is not available, then the value C(TMPHLQ) is used.
    required: false
    type: str
  force:
    description:
      - Specifies that the data set can be shared with others during a member
        delete operation which results in the data set you are updating to be
        simultaneously updated by others.
      - This is helpful when a data set is being used in a long running process
        such as a started task and you are wanting to delete a member.
      - The I(force=True) option enables sharing of data sets through the
        disposition I(DISP=SHR).
      - The I(force=True) only applies to data set members when I(state=absent)
        and I(type=member) and when removing a GDG base with active generations.
      - If I(force=True), I(type=gdg) and I(state=absent) it will force remove
        a GDG base with active generations.
    type: bool
    required: false
    default: false
  batch:
    description:
      - Batch can be used to perform operations on multiple data sets in a single module call.
    type: list
    elements: dict
    required: false
    suboptions:
      name:
        description:
          - The name of the data set being managed. (e.g C(USER.TEST))
          - If I(name) is not provided, a randomized data set name will be generated
            with the HLQ matching the module-runners username.
          - Required if I(type=member) or I(state!=present)
        type: str
        required: false
      state:
        description:
          - The final state desired for specified data set.
          - >
            If I(state=absent) and the data set does not exist on the managed node,
            no action taken, module completes successfully with I(changed=False).
          - >
            If I(state=absent) and the data set does exist on the managed node,
            remove the data set, module completes successfully with I(changed=True).
          - >
            If I(state=absent) and I(type=member) and I(force=True), the data
            set will be opened with I(DISP=SHR) such that the entire data set
            can be accessed by other processes while the specified member is
            deleted.
          - >
            If I(state=absent) and I(volumes) is provided, and the data set is not
            found in the catalog, the module attempts to perform catalog using supplied
            I(name) and I(volumes). If the attempt to catalog the data set catalog is successful,
            then the data set is removed. Module completes successfully with I(changed=True).
          - >
            If I(state=absent) and I(volumes) is provided, and the data set is not
            found in the catalog, the module attempts to perform catalog using supplied
            I(name) and I(volumes). If the attempt to catalog the data set catalog fails,
            then no action is taken. Module completes successfully with I(changed=False).
          - >
            If I(state=absent) and I(volumes) is provided, and the data set is found in
            the catalog, the module compares the catalog volume attributes to the provided
            I(volumes). If they volume attributes are different, the cataloged data set
            will be uncataloged temporarily while the requested data set be deleted is cataloged.
            The module will catalog the original data set on completion, if the attempts to
            catalog fail, no action is taken. Module completes successfully with I(changed=False).
          - >
            If I(state=present) and the data set does not exist on the managed node,
            create and catalog the data set, module completes successfully with I(changed=True).
          - >
            If I(state=present) and I(replace=True) and the data set is present on
            the managed node the existing data set is deleted, and a new data set is created and cataloged
            with the desired attributes, module completes successfully with I(changed=True).
          - >
            If I(state=present) and I(replace=False) and the data set is present
            on the managed node, no action taken, module completes successfully with I(changed=False).
          - >
            If I(state=present) and I(type=member) and the member does not exist in the data set,
            create a member formatted to store data, module completes successfully with I(changed=True).
            Note, a PDSE does not allow a mixture of formats such that there is
            executables (program objects) and data. The member created is formatted to store data,
            not an executable.
          - >
            If I(state=cataloged) and I(volumes) is provided and the data set is already cataloged,
            no action taken, module completes successfully with I(changed=False).
          - >
            If I(state=cataloged) and I(volumes) is provided and the data set is not cataloged,
            module attempts to perform catalog using supplied I(name) and I(volumes). If the attempt to
            catalog the data set catalog is successful, module completes successfully with I(changed=True).
          - >
            If I(state=cataloged) and I(volumes) is provided and the data set is not cataloged,
            module attempts to perform catalog using supplied I(name) and I(volumes). If the attempt to
            catalog the data set catalog fails, returns failure with I(changed=False).
          - >
            If I(state=uncataloged) and the data set is not found, no action taken,
            module completes successfully with I(changed=False).
          - >
            If I(state=uncataloged) and the data set is found, the data set is uncataloged,
            module completes successfully with I(changed=True).
        required: false
        type: str
        default: present
        choices:
          - present
          - absent
          - cataloged
          - uncataloged
      type:
        description:
          - The data set type to be used when creating a data set. (e.g C(pdse))
          - C(member) expects to be used with an existing partitioned data set.
          - Choices are case-sensitive.
        required: false
        type: str
        choices:
          - ksds
          - esds
          - rrds
          - lds
          - seq
          - pds
          - pdse
          - library
          - basic
          - large
          - member
          - hfs
          - zfs
          - gdg
        default: pds
      space_primary:
        description:
          - The amount of primary space to allocate for the dataset.
          - The unit of space used is set using I(space_type).
        type: int
        required: false
        default: 5
      space_secondary:
        description:
          - The amount of secondary space to allocate for the dataset.
          - The unit of space used is set using I(space_type).
        type: int
        required: false
        default: 3
      space_type:
        description:
          - The unit of measurement to use when defining primary and secondary space.
          - Valid units of size are C(k), C(m), C(g), C(cyl), and C(trk).
        type: str
        choices:
          - k
          - m
          - g
          - cyl
          - trk
        required: false
        default: m
      record_format:
        description:
          - The format of the data set. (e.g C(FB))
          - Choices are case-sensitive.
          - When I(type=ksds), I(type=esds), I(type=rrds), I(type=lds) or
            I(type=zfs) then I(record_format=None), these types do not have a
            default I(record_format).
        required: false
        choices:
          - fb
          - vb
          - fba
          - vba
          - u
          - f
        type: str
        default: fb
        aliases:
          - format
      sms_storage_class:
        description:
          - The storage class for an SMS-managed dataset.
          - Required for SMS-managed datasets that do not match an SMS-rule.
          - Not valid for datasets that are not SMS-managed.
          - Note that all non-linear VSAM datasets are SMS-managed.
        type: str
        required: false
        aliases:
          - data_class
      sms_data_class:
        description:
          - The data class for an SMS-managed dataset.
          - Optional for SMS-managed datasets that do not match an SMS-rule.
          - Not valid for datasets that are not SMS-managed.
          - Note that all non-linear VSAM datasets are SMS-managed.
        type: str
        required: false
      sms_management_class:
        description:
          - The management class for an SMS-managed dataset.
          - Optional for SMS-managed datasets that do not match an SMS-rule.
          - Not valid for datasets that are not SMS-managed.
          - Note that all non-linear VSAM datasets are SMS-managed.
        type: str
        required: false
      record_length:
        description:
          - The length, in bytes, of each record in the data set.
          - For variable data sets, the length must include the 4-byte prefix area.
          - "Defaults vary depending on format: If FB/FBA 80, if VB/VBA 137, if U 0."
        type: int
        required: false
      block_size:
        description:
          - The block size to use for the data set.
        type: int
        required: false
      directory_blocks:
        description:
          - The number of directory blocks to allocate to the data set.
        type: int
        required: false
      key_offset:
        description:
          - The key offset to use when creating a KSDS data set.
          - I(key_offset) is required when I(type=ksds).
          - I(key_offset) should only be provided when I(type=ksds)
        type: int
        required: false
      key_length:
        description:
          - The key length to use when creating a KSDS data set.
          - I(key_length) is required when I(type=ksds).
          - I(key_length) should only be provided when I(type=ksds)
        type: int
        required: false
      empty:
        description:
          - Sets the I(empty) attribute for Generation Data Groups.
          - If false, removes only the oldest GDS entry when a new GDS is created that causes GDG limit to be exceeded.
          - If true, removes all GDS entries from a GDG base when a new GDS is created that causes the
            GDG limit to be exceeded.
        type: bool
        required: false
        default: false
      extended:
        description:
          - Sets the I(extended) attribute for Generation Data Groups.
          - If false, allow up to 255 generation data sets (GDSs) to be associated with the GDG.
          - If true, allow up to 999 generation data sets (GDS) to be associated with the GDG.
        type: bool
        required: false
        default: false
      fifo:
        description:
          - Sets the I(fifo) attribute for Generation Data Groups.
          - If false, the order is the newest GDS defined to the oldest GDS. This is the default value.
          - If true, the order is the oldest GDS defined to the newest GDS.
        type: bool
        required: false
        default: false
      limit:
        description:
          - Sets the I(limit) attribute for Generation Data Groups.
          - Specifies the maximum number, from 1 to 255(up to 999 if extended), of GDS that can be
            associated with the GDG being defined.
          - I(limit) is required when I(type=gdg).
        type: int
        required: false
      purge:
        description:
          - Sets the I(purge) attribute for Generation Data Groups.
          - Specifies whether to override expiration dates when a generation data set (GDS)
            is rolled off and the C(scratch) option is set.
        type: bool
        required: false
        default: false
      scratch:
        description:
          - "When C(state=absent), specifies whether to physically remove the data set from the volume."
          - If C(scratch=true), the data set is deleted and its entry is removed from the volume's VTOC.
          - If C(scratch=false), the data set is uncataloged but not physically removed from the volume.
            This is the equivalent of using C(NOSCRATCH) in an C(IDCAMS DELETE) command.
          - The default is C(true) for non-GDG data sets and C(false) for GDG data sets.
        type: bool
        required: false
      volumes:
        description:
          - >
            If cataloging a data set, I(volumes) specifies the name of the volume(s) where the data set is located.
          - >
            If creating a data set, I(volumes) specifies the volume(s) where the data set should be created.
          - >
            If I(volumes) is provided when I(state=present), and the data set is not found in the catalog,
            L(zos_data_set,./zos_data_set.html) will check the volume table of contents to see if the data set exists.
            If the data set does exist, it will be cataloged.
          - >
            If I(volumes) is provided when I(state=absent) and the data set is not found in the catalog,
            L(zos_data_set,./zos_data_set.html) will check the volume table of contents to see if the data set exists.
            If the data set does exist, it will be cataloged and promptly removed from the system.
          - I(volumes) is required when I(state=cataloged).
          - Accepts a string when using a single volume and a list of strings when using multiple.
        type: raw
        required: false
        aliases:
          - volume
      replace:
        description:
          - When I(replace=True), and I(state=present), existing data set matching I(name) will be replaced.
          - >
            Replacement is performed by deleting the existing data set and creating a new data set with
            the same name and desired attributes. Since the existing data set will
            be deleted prior to creating the new data set, no data set will exist if creation of the new data set fails.
          - If I(replace=True), all data in the original data set will be lost.
        type: bool
        required: false
        default: false
      force:
        description:
          - Specifies that the data set can be shared with others during a member
            delete operation which results in the data set you are updating to
            be simultaneously updated by others.
          - This is helpful when a data set is being used in a long running
            process such as a started task and you are wanting to delete a
            member.
          - The I(force=True) option enables sharing of data sets through the
            disposition I(DISP=SHR).
          - The I(force=True) only applies to data set members when
            I(state=absent) and I(type=member).
        type: bool
        required: false
        default: false

attributes:
  action:
    support: none
    description: Indicates this has a corresponding action plugin so some parts of the options can be executed on the controller.
  async:
    support: full
    description: Supports being used with the ``async`` keyword.
  check_mode:
    support: full
    description: Can run in check_mode and return changed status prediction without modifying target. If not supported, the action will be skipped.
"""
EXAMPLES = r"""
- name: Create a sequential data set if it does not exist
  zos_data_set:
    name: someds.name.here
    type: seq
    state: present

- name: Create a PDS data set if it does not exist
  zos_data_set:
    name: someds.name.here
    type: pds
    space_primary: 5
    space_type: m
    record_format: fba
    record_length: 25

- name: Attempt to replace a data set if it exists
  zos_data_set:
    name: someds.name.here
    type: pds
    space_primary: 5
    space_type: m
    record_format: u
    record_length: 25
    replace: true

- name: Attempt to replace a data set if it exists. If not found in the catalog, check if it is available on volume 222222, and catalog if found.
  zos_data_set:
    name: someds.name.here
    type: pds
    space_primary: 5
    space_type: m
    record_format: u
    record_length: 25
    volumes: "222222"
    replace: true

- name: Create an ESDS data set if it does not exist
  zos_data_set:
    name: someds.name.here
    type: esds

- name: Create a KSDS data set if it does not exist
  zos_data_set:
    name: someds.name.here
    type: ksds
    key_length: 8
    key_offset: 0

- name: Create an RRDS data set with storage class MYDATA if it does not exist
  zos_data_set:
    name: someds.name.here
    type: rrds
    sms_storage_class: mydata

- name: Delete a data set if it exists
  zos_data_set:
    name: someds.name.here
    state: absent

- name: Uncatalog a data set but do not remove it from the volume.
  zos_data_set:
    name: someds.name.here
    type: seq
    state: absent
    scratch: false

- name: Delete a data set if it exists. If data set not cataloged, check on volume 222222 for the data set, and then catalog and delete if found.
  zos_data_set:
    name: someds.name.here
    state: absent
    volumes: "222222"

- name: Write a member to an existing PDS; replace if member exists
  zos_data_set:
    name: someds.name.here(mydata)
    type: member
    replace: true

- name: Write a member to an existing PDS; do not replace if member exists
  zos_data_set:
    name: someds.name.here(mydata)
    type: member

- name: Remove a member from an existing PDS
  zos_data_set:
    name: someds.name.here(mydata)
    state: absent
    type: member

- name: Remove a member from an existing PDS/E by opening with disposition DISP=SHR
  zos_data_set:
    name: someds.name.here(mydata)
    state: absent
    type: member
    force: true

- name: Create multiple partitioned data sets and add one or more members to each
  zos_data_set:
    batch:
      - name: someds.name.here1
        type: pds
        space_primary: 5
        space_type: m
        record_format: fb
        replace: true
      - name: someds.name.here1(member1)
        type: member
      - name: someds.name.here2(member1)
        type: member
        replace: true
      - name: someds.name.here2(member2)
        type: member

- name: Catalog a data set present on volume 222222 if it is uncataloged.
  zos_data_set:
    name: someds.name.here
    state: cataloged
    volumes: "222222"

- name: Uncatalog a data set if it is cataloged.
  zos_data_set:
    name: someds.name.here
    state: uncataloged

- name: Create a data set on volumes 000000 and 222222 if it does not exist.
  zos_data_set:
    name: someds.name.here
    state: present
    volumes:
      - "000000"
      - "222222"
"""
RETURN = r"""
data_sets:
  description: The affected data set, including temporary generated data set, in the order provided to the module.
  returned: always
  type: list
  elements: str
  contains:
    name:
      description: The data set name.
      type: str
      returned: always
    state:
      description: The final state desired for specified data set.
      type: str
      returned: always
    type:
      description: The data set type.
      type: str
      returned: always
    space_primary:
      description: The amount of primary space allocated for the dataset.
      type: int
      returned: always
    space_secondary:
      description: The amount of secondary space allocated for the dataset.
      type: int
      returned: always
    space_type:
      description: The unit of measurement used when defining primary and secondary space.
      type: str
      returned: always
    record_format:
      description: The format of the data set.
      type: str
      sample: fb
      returned: always
    sms_storage_class:
      description:
        - The storage class for the SMS-managed dataset.
        - Returned empty if the data set was not specified as SMS-managed dataset.
      type: str
      returned: always
    sms_data_class:
      description:
        - The data class for an SMS-managed dataset.
        - Returned empty if the data set was not specified as SMS-managed dataset.
      type: str
      returned: always
    sms_management_class:
      description:
        - The management class for an SMS-managed dataset.
        - Returned empty if the data set was not specified as SMS-managed dataset.
      type: str
      returned: always
    record_length:
      description:  The length, in bytes, of each record in the data set.
      type: int
      returned: always
    block_size:
      description: The block size used for the data set.
      type: int
      returned: always
    directory_blocks:
      description:
        - The number of directory blocks to allocate to the data set.
      type: int
      returned: always
    key_offset:
      description: The key offset used when creating a KSDS data set.
      type: int
      returned: always
    key_length:
      description: The key length used when creating a KSDS data set.
      type: int
      returned: always
    empty:
      description:
        - I(empty) attribute for Generation Data Groups.
        - Returned empty if the data set provided was not defined as a GDG.
      type: bool
      returned: always
    extended:
      description:
        - I(extended) attribute for Generation Data Groups.
        - Returned empty if the data set provided was not defined as a GDG.
      type: bool
      returned: always
    fifo:
      description:
        - I(fifo) attribute for Generation Data Groups.
        - Returned empty if the data set provided was not defined as a GDG.
      type: bool
      returned: always
    limit:
      description:
        - I(limit) attribute for Generation Data Groups.
        - Returned empty if the data set provided was not defined as a GDG.
      type: int
      returned: always
    purge:
      description:
        - I(purge) attribute for Generation Data Groups.
        - Returned empty if the data set provided was not defined as a GDG.
      type: bool
      returned: always
    scratch:
      description:
        - I(scratch) attribute for Generation Data Groups.
        - Returned empty if the data set provided was not defined as a GDG.
      type: bool
      returned: always
    volumes:
      description:
        - Specifies the name of the volume(s) where the data set is located.
        - Returned empty if volume was not provided.
      type: list
      returned: always
msg:
    description: A string with a generic message relayed to the user.
    returned: always
    type: str
    sample: Error while gathering data set information
"""

from ansible_collections.ibm.ibm_zos_core.plugins.module_utils.better_arg_parser import (
    BetterArgParser,
)
from ansible_collections.ibm.ibm_zos_core.plugins.module_utils.data_set import (
    DataSet, GenerationDataGroup, MVSDataSet, Member
)
from ansible.module_utils.basic import AnsibleModule
from ansible_collections.ibm.ibm_zos_core.plugins.module_utils.dependency_checker import (
    validate_dependencies,
)
from ansible_collections.ibm.ibm_zos_core.plugins.module_utils.log import SingletonLogger

import re

# CONSTANTS
DATA_SET_TYPES = [
    "ksds",
    "esds",
    "rrds",
    "lds",
    "seq",
    "pds",
    "pdse",
    "basic",
    "large",
    "library",
    "member",
    "hfs",
    "zfs",
    "gdg",
]

DATA_SET_FORMATS = [
    "fb",
    "vb",
    "fba",
    "vba",
    "u",
    "f",
]

DEFAULT_RECORD_LENGTHS = {
    "fb": 80,
    "fba": 80,
    "vb": 137,
    "vba": 137,
    "u": 0,
}

DATA_SET_TYPES_VSAM = [
    "ksds",
    "esds",
    "rrds",
    "lds",
    "zfs",
]

# ------------- Functions to validate arguments ------------- #


def get_individual_data_set_parameters(params):
    """Builds a list of data set parameters
    to be used in future operations.

    Parameters
    ----------
    params : dict
        The parameters from
        Ansible's AnsibleModule object module.params.

    Returns
    -------
    Union[dict]
        A list of dicts where each list item
        represents one data set. Each dictionary holds the parameters
        (passed to the zos_data_set module) for the data set which it represents.

    Raises
    ------
    ValueError
        Raised if top-level parameters "name"
        and "batch" are both provided.
    ValueError
        Raised if neither top-level parameters "name"
        or "batch" are provided.
    """
    if params.get("name") and params.get("batch"):
        raise ValueError(
            'Top-level parameters "name" and "batch" are mutually exclusive.'
        )
    elif not params.get("name") and not params.get("batch"):
        raise ValueError(
            'One of the following parameters is required: "name", "batch".'
        )
    if params.get("name"):
        data_sets_parameter_list = [params]
    else:
        data_sets_parameter_list = params.get("batch")
    return data_sets_parameter_list


# * can be replaced by built-in
def data_set_name(contents, dependencies):
    """Validates provided data set name(s) are valid.
    Returns a list containing the name(s) of data sets.

    Parameters
    ----------
    contents : str
        Name of the dataset.
    dependencies : dict
        Any dependencies needed for contents argument to be validated.

    Returns
    -------
    None
        If the dependencies have a batch.
    str
        The data set name.

    Raises
    ------
    ValueError
        Data set name must be provided.
    ValueError
        Data set and member name must be provided.
    ValueError
        A value is invalid.
    """
    if dependencies.get("batch"):
        return None
    if contents is None:
        if dependencies.get("state") != "present":
            raise ValueError('Data set name must be provided when "state!=present"')
        if dependencies.get("type") != "member":
            tmphlq = dependencies.get("tmp_hlq")
            if tmphlq is None:
                tmphlq = ""
            contents = DataSet.temp_name(tmphlq)
        else:
            raise ValueError(
                'Data set and member name must be provided when "type=member"'
            )
    dsname = str(contents)
    if not re.fullmatch(
        r"^(?:(?:[A-Z$#@]{1}[A-Z0-9$#@-]{0,7})(?:[.]{1})){1,21}[A-Z$#@]{1}[A-Z0-9$#@-]{0,7}$",
        dsname,
        re.IGNORECASE,
    ):
        if (
            re.fullmatch(
                r"^(?:(?:[A-Z$#@]{1}[A-Z0-9$#@-]{0,7})(?:[.]{1})){1,21}[A-Z$#@]{1}[A-Z0-9$#@-]{0,7}(?:\([+-]{0,1}\d{1,4}\)){0,1}$",
                dsname,
                re.IGNORECASE,
            )
        ):
            return dsname.upper()
        elif not (
            re.fullmatch(
                r"^(?:(?:[A-Z$#@]{1}[A-Z0-9$#@-]{0,7})(?:[.]{1})){1,21}[A-Z$#@]{1}[A-Z0-9$#@-]{0,7}(?:\(([A-Z$#@]{1}[A-Z0-9$#@]{0,7})\)){0,1}$",
                dsname,
                re.IGNORECASE,
            )
            and dependencies.get("type") == "member"
        ):
            raise ValueError(
                "Value {0} is invalid for data set argument.".format(dsname)
            )
    return dsname.upper()


# * dependent on state
def space_type(contents, dependencies):
    """Validates provided data set unit of space is valid.
    Returns the unit of space.

    Parameters
    ----------
    contents : str
        Unit of space of the dataset.
    dependencies : dict
        Any dependencies needed for contents argument to be validated.

    Returns
    -------
    str
        The data set unit of space.

    Raises
    ------
    ValueError
        Value provided is invalid.
"""
    if dependencies.get("state") == "absent":
        return "m"
    if contents is None:
        return None
    match = re.fullmatch(r"(m|g|k|trk|cyl)", contents, re.IGNORECASE)
    if not match:
        raise ValueError(
            'Value {0} is invalid for space_type argument. Valid space types are "k", "m", "g", "trk" or "cyl".'.format(
                contents
            )
        )
    return contents


# * dependent on state
def sms_class(contents, dependencies):
    """Validates provided sms class is of valid length.
    Returns the sms class.

    Parameters
    ----------
    contents : str
        Name of the sms class.
    dependencies : dict
        Any dependencies needed for contents argument to be validated.

    Returns
    -------
    None
        If the state is absent or contents is none.
    str
        The sms class set name.

    Raises
    ------
    ValueError
        Value is invalid.
    """
    if dependencies.get("state") == "absent" or contents is None:
        return None
    if len(contents) < 1 or len(contents) > 8:
        raise ValueError(
            (
                "Value {0} is invalid for an SMS class argument. "
                "SMS class must be at least 1 and at most 8 characters."
            ).format(contents)
        )
    return contents


def valid_when_state_present(contents, dependencies):
    """Ensures no arguments that are invalid when state!=present
    are allowed.

    Parameters
    ----------
    contents : str
        Arguments to be validated.
    dependencies : dict
        Any dependencies needed for contents argument to be validated.

    Returns
    -------
    None
        If the state is absent or contents is none.
    str
        Valid arguments.
    """
    if dependencies.get("state") == "absent" or contents is None:
        return None
    return contents


# * dependent on state
# * dependent on format
def record_length(contents, dependencies):
    """Validates provided record length is valid.
    Returns the record length as integer.

    Parameters
    ----------
    contents : str
        Length of the dataset.
    dependencies : dict
        Any dependencies needed for contents argument to be validated.

    Returns
    -------
    None
        If the state is absent or contents is none.
    str
        The data set length.

    Raises
    ------
    ValueError
        Value is invalid.
    """
    if dependencies.get("state") == "absent":
        return None
    contents = (
        DEFAULT_RECORD_LENGTHS.get(dependencies.get("record_format"), None)
        if contents is None
        else int(contents)
    )
    if contents is None:
        return None
    if not re.fullmatch(r"[0-9]*", str(contents)) or (contents < 0 or contents > 32768):
        raise ValueError(
            "Value {0} is invalid for record_length argument. record_length must be between 0 and 32768 bytes.".format(
                contents
            )
        )
    return contents


# * dependent on state
# * dependent on record_length
def record_format(contents, dependencies):
    """Validates data set format is valid.
    Returns uppercase data set format.

    Parameters
    ----------
    contents : str
        Format of the dataset.
    dependencies : dict
        Any dependencies needed for contents argument to be validated.

    Returns
    -------
    str
        The data set format in uppercase. Default is 'FB'.

    Raises
    ------
    ValueError
        Value is invalid.
    """
    if dependencies.get("state") == "absent":
        return "fb"
    if contents is None:
        return "fb"
    formats = "|".join(DATA_SET_FORMATS)
    if not re.fullmatch(formats, contents, re.IGNORECASE):
        raise ValueError(
            "Value {0} is invalid for format argument. format must be one of the following: {1}.".format(
                contents, ", ".join(DATA_SET_FORMATS)
            )
        )
    return contents


# * dependent on state
def data_set_type(contents, dependencies):
    """Validates data set type is valid.
    Returns uppercase data set type.

    Parameters
    ----------
    contents : str
        Type of the dataset.
    dependencies : dict
        Any dependencies needed for contents argument to be validated.

    Returns
    -------
    str
        The data set type in uppercase. Default is PDS.

    Raises
    ------
    ValueError
        Value is invalid.
    """
    # if dependencies.get("state") == "absent" and contents != "MEMBER":
    #     return None
    if contents is None:
        return "pds"

    if contents == "gdg" and dependencies.get("state") == "present" and dependencies.get("limit") is None:
        raise ValueError(
            "Limit must be provided when data set type is gdg and state=present."
        )
    types = "|".join(DATA_SET_TYPES)
    if not re.fullmatch(types, contents, re.IGNORECASE):
        raise ValueError(
            "Value {0} is invalid for type argument. type must be one of the following: {1}.".format(
                contents, ", ".join(DATA_SET_TYPES)
            )
        )
    return contents


def limit_type(contents, dependencies):
    """Validates limit is valid. Limit option is dependent on state.
    Returns limit.

    Parameters
    ----------
    contents : int
        Limit for GDG type.
    dependencies : dict
        Any dependencies needed for contents argument to be validated.

    Returns
    -------
    int
        The limit for GDG type.

    Raises
    ------
    ValueError
        Value is invalid.
    """
    if not isinstance(contents, int):
        raise ValueError(
            "Value {0} is invalid for limit option. Limit must be an integer from 1 to 255, if extended up to 999.".format(
                contents
            )
        )
    return contents


# * dependent on state
def volumes(contents, dependencies):
    """Validates volume is valid.
    Returns uppercase volume.

    Parameters
    ----------
    contents : str
        Name of the volume.
    dependencies : dict
        Any dependencies needed for contents argument to be validated.

    Returns
    -------
    None
        If the state is absent or contents is none.
    str
        The volume name.

    Raises
    ------
    ValueError
        Argument is invalid.
    ValueError
        Volume is required when state is cataloged.
    """
    if contents is None:
        if dependencies.get("state") == "cataloged":
            raise ValueError("Volume is required when state==cataloged.")
        return None
    if not isinstance(contents, list):
        contents = [contents]
    for vol in contents:
        if not re.fullmatch(
            r"^[A-Z0-9]{1,6}$",
            str(vol),
            re.IGNORECASE,
        ):
            raise ValueError(
                'Invalid argument type for "{0}". expected volume name'.format(vol)
            )
        vol = vol.upper()
    return contents


# * dependent on state
# * dependent on type
def key_length(contents, dependencies):
    """Validates data set key length is valid.
    Returns data set key length as integer.

    Parameters
    ----------
    contents : str
        key_length.
    dependencies : dict
        Any dependencies needed for contents argument to be validated.

    Returns
    -------
    None
        If the state is absent or contents is none.
    int
        key_length.

    Raises
    ------
    ValueError
        Argument is invalid.
    ValueError
        key_length was not provided when requesting KSDS data set.
    ValueError
        key_length can not be provided when type is not KSDS.
    """
    if dependencies.get("state") == "absent":
        return None
    if dependencies.get("type") == "ksds" and contents is None:
        raise ValueError("key_length is required when requesting KSDS data set.")
    if dependencies.get("type") != "ksds" and contents is not None:
        raise ValueError("key_length is only valid when type=ksds.")
    if contents is None:
        return None
    contents = int(contents)
    if not re.match(r"[0-9]+", str(contents)):
        raise ValueError(
            "Value {0} is invalid for key_length argument.".format(contents)
        )
    return contents


# * dependent on state
# * dependent on type
# * dependent on key_length
def key_offset(contents, dependencies):
    """Validates data set key offset is valid.
    Returns data set key offset as integer.

    Parameters
    ----------
    contents : str
        Key offset of the data set.
    dependencies : dict
        Any dependencies needed for contents argument to be validated.

    Returns
    -------
    None
        If the state is absent or contents is none.
    int
        Key offset of the data set.

    Raises
    ------
    ValueError
        Argument is invalid.
    ValueError
        key_offset was not provided when requesting KSDS data set.
    ValueError
        key_offset can not be provided when type is not KSDS.
    """
    if dependencies.get("state") == "absent":
        return None
    if dependencies.get("type") == "ksds" and contents is None:
        raise ValueError("key_offset is required when requesting KSDS data set.")
    if dependencies.get("type") != "ksds" and contents is not None:
        raise ValueError("key_offset is only valid when type=ksds.")
    if contents is None:
        return None
    contents = int(contents)
    if not re.match(r"[0-9]+", str(contents)):
        raise ValueError(
            "Value {0} is invalid for key_offset argument. offset must be between 0 and length of object - 1.".format(
                contents
            )
        )
    return contents


def get_data_set_handler(**params):
    """Get object initialized based on parameters.
    Parameters
    ----------
    **params
      Data set parameters.

    Returns
    -------
    MVSDataSet or GenerationDataGroup or Member object.
    """
    if params.get("type") == "gdg":
        return GenerationDataGroup(
            name=params.get("name"),
            limit=params.get("limit", None),
            empty=params.get("empty", None),
            purge=params.get("purge", None),
            scratch=params.get("scratch", None),
            extended=params.get("extended", None),
            fifo=params.get("fifo", None),
        )
    elif params.get("type") == "member":
        return Member(
            name=params.get("name")
        )
    else:
        return MVSDataSet(
            name=params.get("name"),
            record_format=params.get("record_format", None),
            volumes=params.get("volumes", None),
            data_set_type=params.get("type", None),
            block_size=params.get("block_size", None),
            record_length=params.get("record_length", None),
            space_primary=params.get("space_primary", None),
            space_secondary=params.get("space_secondary", None),
            space_type=params.get("space_type", None),
            directory_blocks=params.get("directory_blocks", None),
            key_length=params.get("key_length", None),
            key_offset=params.get("key_offset", None),
            sms_storage_class=params.get("sms_storage_class", None),
            sms_data_class=params.get("sms_data_class", None),
            sms_management_class=params.get("sms_management_class", None),
        )


def perform_data_set_operations(data_set, state, replace, tmp_hlq, force, noscratch):
    """Calls functions to perform desired operations on
    one or more data sets. Returns boolean indicating if changes were made.

    Parameters
    ----------
    data_set : {object | MVSDataSet | Member | GenerationDataGroup }
        Data set object to perform operations on.
    state : str
        State of the data sets.
    replace : str
        Whether or not replace an existing data set if it has the same name.
    tmp_hlq : str
        Temporary high level qualifier to use for temporary data sets.
    force : str
        Whether or not the data set can be shared with others during the
        operation.

    Returns
    -------
    bool
        If changes were made.
    """
    changed = False
    if state == "present" and data_set.data_set_type == "member":
        changed = data_set.ensure_present(replace=replace, tmphlq=tmp_hlq)
    elif state == "present" and data_set.data_set_type == "gdg":
        changed = data_set.ensure_present(replace=replace)
    elif state == "present":
        changed = data_set.ensure_present(replace=replace, tmp_hlq=tmp_hlq, force=force)
    elif state == "absent" and data_set.data_set_type == "member":
        changed = data_set.ensure_absent(force=force)
    elif state == "absent" and data_set.data_set_type == "gdg":
        changed = data_set.ensure_absent(force=force, noscratch=noscratch)
    elif state == "absent":
        changed = data_set.ensure_absent(tmp_hlq=tmp_hlq, noscratch=noscratch)
    elif state == "cataloged":
        changed = data_set.ensure_cataloged(tmp_hlq=tmp_hlq)
    elif state == "uncataloged":
        changed = data_set.ensure_uncataloged(tmp_hlq=tmp_hlq)
    return changed


def parse_and_validate_args(params):
    """Parse and validate args.

    Parameters
    ----------
    params : dict
        Params to validated and parsed.

    Returns
    -------
    dict
        Parsed args.
    """

    arg_defs = dict(
        # Used for batch data set args
        batch=dict(
            type="list",
            elements="dict",
            options=dict(
                name=dict(
                    type=data_set_name,
                    default=data_set_name,
                    dependencies=["type", "state"],
                ),
                state=dict(
                    type="str",
                    default="present",
                    choices=["present", "absent", "cataloged", "uncataloged"],
                ),
                type=dict(
                    type=data_set_type,
                    required=False,
                    dependencies=["state", "limit"],
                    choices=DATA_SET_TYPES,
                ),
                space_type=dict(
                    type=space_type,
                    required=False,
                    dependencies=["state"],
                    choices=["k", "m", "g", "cyl", "trk"],
                    default="m",
                ),
                space_primary=dict(type="int", required=False, dependencies=["state"]),
                space_secondary=dict(
                    type="int", required=False, dependencies=["state"]
                ),
                record_format=dict(
                    type=record_format,
                    required=False,
                    dependencies=["state"],
                    choices=["fb", "vb", "fba", "vba", "u", "f"],
                    aliases=["format"],
                    default="fb",
                ),
                sms_management_class=dict(
                    type=sms_class, required=False, dependencies=["state"]
                ),
                # I know this alias is odd, ZOAU used to document they supported
                # SMS data class when they were actually passing as storage class
                # support for backwards compatability with previous module versions
                sms_storage_class=dict(
                    type=sms_class,
                    required=False,
                    dependencies=["state"],
                    aliases=["data_class"],
                ),
                sms_data_class=dict(
                    type=sms_class, required=False, dependencies=["state"]
                ),
                block_size=dict(
                    type=valid_when_state_present,
                    required=False,
                    dependencies=["state"],
                ),
                directory_blocks=dict(
                    type=valid_when_state_present,
                    required=False,
                    dependencies=["state"],
                ),
                record_length=dict(
                    type=record_length,
                    required=False,
                    dependencies=["state", "record_format"],
                ),
                key_offset=dict(
                    type=valid_when_state_present,
                    required=False,
                    dependencies=["state", "type", "key_length"],
                ),
                key_length=dict(
                    type=valid_when_state_present,
                    required=False,
                    dependencies=["state", "type"],
                ),
                replace=dict(
                    type="bool",
                    default=False,
                ),
                volumes=dict(
                    type=volumes,
                    required=False,
                    aliases=["volume"],
                    dependencies=["state"],
                ),
                limit=dict(
                    type="int",
                    required=False
                ),
                empty=dict(
                    type="bool",
                    required=False,
                    default=False
                ),
                purge=dict(
                    type="bool",
                    required=False,
                    default=False
                ),
                scratch=dict(
                    type="bool",
                    required=False
                ),
                extended=dict(
                    type="bool",
                    required=False,
                    default=False
                ),
                fifo=dict(
                    type="bool",
                    required=False,
                    default=False
                ),
                force=dict(
                    type="bool",
                    required=False,
                    default=False,
                ),
            ),
        ),
        # For individual data set args
        name=dict(
            type=data_set_name,
            default=data_set_name,
            required=False,
            dependencies=["type", "state", "batch", "tmp_hlq"],
        ),
        state=dict(
            type="str",
            default="present",
            choices=["present", "absent", "cataloged", "uncataloged"],
        ),
        type=dict(type=data_set_type, required=False, dependencies=["state", "limit"]),
        space_type=dict(
            type=space_type,
            required=False,
            dependencies=["state"],
            choices=["k", "m", "g", "cyl", "trk"],
            default="m",
        ),
        space_primary=dict(type="int", required=False, dependencies=["state"]),
        space_secondary=dict(type="int", required=False, dependencies=["state"]),
        record_format=dict(
            type=record_format,
            required=False,
            dependencies=["state"],
            choices=["fb", "vb", "fba", "vba", "u", "f"],
            aliases=["format"],
            default="fb",
        ),
        sms_management_class=dict(
            type=sms_class, required=False, dependencies=["state"]
        ),
        # I know this alias is odd, ZOAU used to document they supported
        # SMS data class when they were actually passing as storage class
        # support for backwards compatibility with previous module versions
        sms_storage_class=dict(
            type=sms_class,
            required=False,
            dependencies=["state"],
            aliases=["data_class"],
        ),
        sms_data_class=dict(type=sms_class, required=False, dependencies=["state"]),
        block_size=dict(
            type=valid_when_state_present,
            required=False,
            dependencies=["state"],
        ),
        directory_blocks=dict(
            type=valid_when_state_present,
            required=False,
            dependencies=["state"],
        ),
        record_length=dict(
            type=record_length,
            required=False,
            dependencies=["state", "record_format"],
        ),
        key_offset=dict(type=valid_when_state_present, required=False),
        key_length=dict(type=valid_when_state_present, required=False),
        replace=dict(
            type="bool",
            default=False,
        ),
        # GDG options
        limit=dict(type="int", required=False),
        empty=dict(type="bool", required=False, default=False),
        purge=dict(type="bool", required=False, default=False),
        scratch=dict(type="bool", required=False,),
        extended=dict(type="bool", required=False, default=False),
        fifo=dict(type="bool", required=False, default=False),
        # End of GDG options
        volumes=dict(
            type=volumes,
            required=False,
            aliases=["volume"],
            dependencies=["state"],
        ),
        tmp_hlq=dict(
            type='qualifier_or_empty',
            required=False,
            default=None
        ),
        force=dict(
            type="bool",
            required=False,
            default=False,
        ),
        mutually_exclusive=[
            ["batch", "name"],
            # ["batch", "state"],
            # ["batch", "space_type"],
            # ["batch", "space_primary"],
            # ["batch", "space_secondary"],
            # ["batch", "record_format"],
            ["batch", "sms_management_class"],
            ["batch", "sms_storage_class"],
            ["batch", "sms_data_class"],
            ["batch", "block_size"],
            ["batch", "record_length"],
            ["batch", "key_offset"],
            ["batch", "key_length"],
            # ["batch", "replace"],
            ["batch", "volumes"],
            # ["batch", "force"],
        ],
    )
    parser = BetterArgParser(arg_defs)
    parsed_args = parser.parse_args(params)
    parsed_args = {
        key: value for key, value in parsed_args.items() if value is not None
    }
    return parsed_args


def determine_scratch(data_set_params):
    scratch = data_set_params.get("scratch")
    if scratch is None:
        if data_set_params.get("type") == "gdg" and data_set_params.get("state") == "present":
            scratch = False
        elif data_set_params.get("state") == "absent":
            scratch = True
    return scratch


def build_return_schema(data_set_list):
    """ Builds return values schema with empty values.

        Parameters
        ----------
        data_set_list : dict
            List of data sets.

        Returns
        -------
        dict
            Dictionary used to return values at execution finalization.
    """
    data_set_schema = {
        "name": "",
        "state": "",
        "type": "",
        "space_primary": "",
        "space_secondary": "",
        "space_type": "",
        "record_format": "",
        "sms_storage_class": "",
        "sms_data_class": "",
        "sms_management_class": "",
        "record_length": "",
        "block_size": "",
        "directory_blocks": "",
        "key_offset": "",
        "key_length": "",
        "empty": "",
        "extended": "",
        "fifo": "",
        "limit": "",
        "purge": "",
        "scratch": "",
        "volumes": [],
    }

    data_sets = [data_set_schema.copy() | data_set.attributes for data_set in data_set_list]
    result = {
        "data_sets": data_sets,
        "msg": "",
        "failed": False
    }
    return result


def run_module():
    """Runs the module.

    Raises
    ------
    fail_json
        Any exception during processing of data set params.
    """
    # TODO: add logic to handle aliases during parsing

    module_args = dict(
        # Used for batch data set args
        batch=dict(
            type="list",
            elements="dict",
            options=dict(
                name=dict(
                    type="str",
                    required=False,
                ),
                state=dict(
                    type="str",
                    default="present",
                    choices=["present", "absent", "cataloged", "uncataloged"],
                ),
                type=dict(
                    type="str",
                    required=False,
                    default="pds",
                    choices=DATA_SET_TYPES,
                ),
                space_type=dict(
                    type="str",
                    required=False,
                    default="m",
                    choices=["k", "m", "g", "cyl", "trk"],
                ),
                space_primary=dict(type="int", required=False, default=5),
                space_secondary=dict(type="int", required=False, default=3),
                record_format=dict(
                    type="str",
                    required=False,
                    aliases=["format"],
                    default="fb",
                    choices=["fb", "vb", "fba", "vba", "u", "f"],
                ),
                sms_management_class=dict(type="str", required=False),
                # I know this alias is odd, ZOAU used to document they supported
                # SMS data class when they were actually passing as storage class
                # support for backwards compatibility with previous module versions
                sms_storage_class=dict(
                    type="str", required=False, aliases=["data_class"]
                ),
                sms_data_class=dict(type="str", required=False),
                block_size=dict(
                    type="int",
                    required=False,
                ),
                directory_blocks=dict(
                    type="int",
                    required=False,
                ),
                record_length=dict(
                    type="int",
                    required=False,
                ),
                key_offset=dict(type="int", required=False, no_log=False),
                key_length=dict(type="int", required=False, no_log=False),
                replace=dict(
                    type="bool",
                    default=False,
                ),
                # GDG options
                limit=dict(type="int", required=False),
                empty=dict(type="bool", required=False, default=False),
                purge=dict(type="bool", required=False, default=False),
                scratch=dict(type="bool", required=False,),
                extended=dict(type="bool", required=False, default=False),
                fifo=dict(type="bool", required=False, default=False),
                volumes=dict(type="raw", required=False, aliases=["volume"]),
                force=dict(
                    type="bool",
                    required=False,
                    default=False,
                ),
            ),
        ),
        # For individual data set args
        name=dict(
            type="str",
            required=False,
        ),
        state=dict(
            type="str",
            default="present",
            choices=["present", "absent", "cataloged", "uncataloged"],
        ),
        type=dict(
            type="str",
            required=False,
            default="pds",
            choices=DATA_SET_TYPES,
        ),
        space_type=dict(
            type="str",
            required=False,
            default="m",
            choices=["k", "m", "g", "cyl", "trk"],
        ),
        space_primary=dict(type="int", required=False, default=5),
        space_secondary=dict(type="int", required=False, default=3),
        record_format=dict(
            type="str",
            required=False,
            aliases=["format"],
            choices=["fb", "vb", "fba", "vba", "u", "f"],
            default="fb"
        ),
        sms_management_class=dict(type="str", required=False),
        # I know this alias is odd, ZOAU used to document they supported
        # SMS data class when they were actually passing as storage class
        # support for backwards compatability with previous module versions
        sms_storage_class=dict(type="str", required=False, aliases=["data_class"]),
        sms_data_class=dict(type="str", required=False),
        block_size=dict(
            type="int",
            required=False,
        ),
        directory_blocks=dict(
            type="int",
            required=False,
        ),
        record_length=dict(
            type="int",
            required=False,
        ),
        key_offset=dict(type="int", required=False, no_log=False),
        key_length=dict(type="int", required=False, no_log=False),
        replace=dict(
            type="bool",
            default=False,
        ),
        # GDG options
        limit=dict(type="int", required=False, no_log=False),
        empty=dict(type="bool", required=False, default=False),
        purge=dict(type="bool", required=False, default=False),
        scratch=dict(type="bool", required=False,),
        extended=dict(type="bool", required=False, default=False),
        fifo=dict(type="bool", required=False, default=False),
        # End of GDG options
        volumes=dict(
            type="raw",
            required=False,
            aliases=["volume"],
        ),
        tmp_hlq=dict(
            type="str",
            required=False,
            default=None
        ),
        force=dict(
            type="bool",
            required=False,
            default=False
        ),
    )
    result = dict(changed=False)

    module = AnsibleModule(argument_spec=module_args, supports_check_mode=True)
    validate_dependencies(module)

    # This evaluation will always occur as a result of the limitation on the
    # better arg parser, this will serve as a solution for now and ensure
    # the non-batch and batch arguments are correctly set
    # This section is copied down inside if/check_mode false, so it modifies after the arg parser
    if module.params.get("batch") is not None:
        for entry in module.params.get("batch"):
            if entry.get('type') is not None and entry.get("type") in DATA_SET_TYPES_VSAM:
                entry["record_format"] = None
        if module.params.get("type") is not None:
            module.params["type"] = None
        if module.params.get("state") is not None:
            module.params["state"] = None
        if module.params.get("space_type") is not None:
            module.params["space_type"] = None
        if module.params.get("space_primary") is not None:
            module.params["space_primary"] = None
        if module.params.get("space_secondary") is not None:
            module.params["space_secondary"] = None
        if module.params.get("replace") is not None:
            module.params["replace"] = None
        if module.params.get("record_format") is not None:
            module.params["record_format"] = None
    elif module.params.get("type") is not None:
        if module.params.get("type") in DATA_SET_TYPES_VSAM:
            # For VSAM types set the value to nothing and let the code manage it
            # module.params["record_format"] = None
            if module.params.get("record_format") is not None:
                del module.params["record_format"]

    data_set_list = []

    if not module.check_mode:
        try:
            # Update the dictionary for use by better arg parser by adding the
            # batch keyword after the arg spec is evaluated else you get a lint
            # error 'invalid-ansiblemodule-schema'
            module_args['state']['dependencies'] = ['batch']
            params = parse_and_validate_args(module.params)
            data_set_param_list = get_individual_data_set_parameters(params)

            # Initialize logging module
            module_verbosity_level = module._verbosity
            SingletonLogger().get_logger(module_verbosity_level)

            for data_set_params in data_set_param_list:
                data_set_params["scratch"] = determine_scratch(data_set_params)
                data_set_params["noscratch"] = not data_set_params["scratch"]
                # this returns MVSDataSet, Member or GenerationDataGroup
                data_set = get_data_set_handler(**data_set_params)
                current_changed = perform_data_set_operations(
                    data_set=data_set,
                    state=data_set_params.get("state"),
                    replace=data_set_params.get("replace"),
                    tmp_hlq=data_set_params.get("tmp_hlq"),
                    force=data_set_params.get("force"),
                    noscratch=data_set_params.get("noscratch"),
                )
                data_set_list.append(data_set)
                result["changed"] = result["changed"] or current_changed
            # Build return schema from created data sets.
            result.update(build_return_schema(data_set_list))
        except Exception as e:
            module.fail_json(msg=repr(e), **result)
    module.exit_json(**result)


def main():
    run_module()


if __name__ == "__main__":
    main()
