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

from __future__ import absolute_import, division, print_function

__metaclass__ = type


DOCUMENTATION = r"""
module: zos_mvs_raw
author:
  - "Xiao Yuan Ma (@bjmaxy)"
  - "Blake Becker (@blakeinate)"
  - "Oscar Fernando Flores (@fernandofloresg)"
short_description: Run a z/OS program.
description:
  - Run a z/OS program.
  - This is analogous to a job step in JCL.
  - Defaults will be determined by underlying API if value not provided.
version_added: "1.1.0"
options:
  program_name:
    description:
      - The name of the z/OS program to run (e.g. IDCAMS, IEFBR14, IEBGENER etc.).
    required: true
    type: str
    aliases:
      - pgm
      - program
  parm:
    description:
      - The program arguments (e.g. -a='MARGINS(1,72)').
    required: false
    type: str
  auth:
    description:
      - Determines whether this program should run with authorized privileges.
      - If I(auth=true), the program runs as APF authorized.
      - If I(auth=false), the program runs as unauthorized.
    required: false
    type: bool
    default: false
  verbose:
    description:
      - Determines if verbose output should be returned from the underlying utility used by this module.
      - When I(verbose=true) verbose output is returned on module failure.
    required: false
    type: bool
    default: false
  max_rc:
    description:
      - Specifies the maximum return code allowed for the program output. If the
        program generates a return code higher than the specified maximum, the module will fail.
    required: false
    type: int
    default: 0
  dds:
    description:
      - The input data source.
      - I(dds) supports 6 types of sources
      - 1. I(dd_data_set) for data set files.
      - 2. I(dd_unix) for UNIX files.
      - 3. I(dd_input) for in-stream data set.
      - 4. I(dd_dummy) for no content input.
      - 5. I(dd_concat) for a data set concatenation.
      - 6. I(dds) supports any combination of source types.

    required: false
    type: list
    elements: dict
    suboptions:
      dd_data_set:
        description:
          - Specify a data set.
          - I(dd_data_set) can reference an existing data set or be
            used to define a new data set to be created during execution.
        required: false
        type: dict
        suboptions:
          dd_name:
            description:
              - The DD name.
            required: true
            type: str
          data_set_name:
            description:
              - The data set name.
              - A data set name can be a GDS relative name.
              - When using GDS relative name and it is a positive generation, I(disposition=new) must be used.
            type: str
            required: false
          raw:
            description:
              - Create a new data set and let the MVS program assign its own default DCB attributes.
              - When C(raw=true), all supplied DCB attributes like disposition, space, volumes, SMS, keys, record settings, etc. are ignored.
              - Using C(raw) option is not possible for all programs, use this for cases where the MVS program that is called is able to assign
                its own default dataset attributes.
            type: bool
            default: false
          type:
            description:
              - The data set type. Only required when I(disposition=new).
              - Maps to DSNTYPE on z/OS.
            type: str
            choices:
              - library
              - pds
              - pdse
              - large
              - basic
              - seq
              - rrds
              - esds
              - lds
              - ksds
          disposition:
            description:
              - I(disposition) indicates the status of a data set.
              - Defaults to shr.
            type: str
            required: false
            choices:
              - new
              - shr
              - mod
              - old
          disposition_normal:
            description:
              - I(disposition_normal) indicates what to do with the data set after a normal termination of the program.
            type: str
            required: false
            choices:
              - delete
              - keep
              - catalog
              - uncatalog
          disposition_abnormal:
            description:
              - I(disposition_abnormal) indicates what to do with the data set after an abnormal termination of the
                program.
            type: str
            required: false
            choices:
              - delete
              - keep
              - catalog
              - uncatalog
          reuse:
            description:
              - Determines if a data set should be reused if I(disposition=new) and if a data set with a matching name already exists.
              - If I(reuse=true), I(disposition) will be automatically switched to C(SHR).
              - If I(reuse=false), and a data set with a matching name already exists, allocation will fail.
              - Mutually exclusive with I(replace).
              - I(reuse) is only considered when I(disposition=new)
            type: bool
            default: false
          replace:
            description:
              - Determines if a data set should be replaced if I(disposition=new) and a data set with a matching name already exists.
              - If I(replace=true), the original data set will be deleted, and a new data set created.
              - If I(replace=false), and a data set with a matching name already exists, allocation will fail.
              - Mutually exclusive with I(reuse).
              - I(replace) is only considered when I(disposition=new)
              - I(replace) will result in loss of all data in the original data set unless I(backup) is specified.
            type: bool
            default: false
          backup:
            description:
              - Determines if a backup should be made of an existing data set when I(disposition=new), I(replace=true),
                and a data set with the desired name is found.
              - I(backup) is only used when I(replace=true).
            type: bool
            default: false
          space_type:
            description:
              - The unit of measurement to use when allocating space for a new data set
                using I(space_primary) and I(space_secondary).
            type: str
            choices:
              - trk
              - cyl
              - b
              - k
              - m
              - g
          space_primary:
            description:
              - The primary amount of space to allocate for a new data set.
              - The value provided to I(space_type) is used as the unit of space for the allocation.
              - Not applicable when I(space_type=blklgth) or I(space_type=reclgth).
            type: int
          space_secondary:
            description:
              - When primary allocation of space is filled,
                secondary space will be allocated with the provided size as needed.
              - The value provided to I(space_type) is used as the unit of space for the allocation.
              - Not applicable when I(space_type=blklgth) or I(space_type=reclgth).
            type: int
          volumes:
            description:
              - The volume or volumes on which a data set resides or will reside.
              - Do not specify the same volume multiple times.
            type: raw
            required: false
          sms_management_class:
            description:
              - The desired management class for a new SMS-managed data set.
              - I(sms_management_class) is ignored if specified for an existing data set.
              - All values must be between 1-8 alpha-numeric characters.
            type: str
            required: false
          sms_storage_class:
            description:
              - The desired storage class for a new SMS-managed data set.
              - I(sms_storage_class) is ignored if specified for an existing data set.
              - All values must be between 1-8 alpha-numeric characters.
            type: str
            required: false
          sms_data_class:
            description:
              - The desired data class for a new SMS-managed data set.
              - I(sms_data_class) is ignored if specified for an existing data set.
              - All values must be between 1-8 alpha-numeric characters.
            type: str
            required: false
          block_size:
            description:
              - The maximum length of a block in bytes.
              - Default is dependent on I(record_format)
            type: int
            required: false
          directory_blocks:
            description:
              - The number of directory blocks to allocate to the data set.
            type: int
            required: false
          key_label:
            description:
              - The label for the encryption key used by the system to encrypt the data set.
              - I(key_label) is the public name of a protected encryption key in the ICSF key repository.
              - I(key_label) should only be provided when creating an extended format data set.
              - Maps to DSKEYLBL on z/OS.
            type: str
            required: false
          encryption_key_1:
            description:
              - The encrypting key used by the Encryption Key Manager.
              - Specification of the key labels does not by itself enable encryption.
                Encryption must be enabled by a data class that specifies an encryption format.
            type: dict
            required: false
            suboptions:
              label:
                description:
                  - The label for the key encrypting key used by the Encryption Key
                    Manager.
                  - Key label must have a private key associated with it.
                  - I(label) can be a maximum of 64 characters.
                  - Maps to KEYLAB1 on z/OS.
                type: str
                required: true
              encoding:
                description:
                  - How the label for the key encrypting key specified by
                    I(label) is encoded by the Encryption Key Manager.
                  - I(encoding) can either be set to C(l) for label encoding,
                    or C(h) for hash encoding.
                  - Maps to KEYCD1 on z/OS.
                type: str
                required: true
                choices:
                  - l
                  - h
          encryption_key_2:
            description:
              - The encrypting key used by the Encryption Key Manager.
              - Specification of the key labels does not by itself enable encryption.
                Encryption must be enabled by a data class that specifies an encryption format.
            type: dict
            required: false
            suboptions:
              label:
                description:
                  - The label for the key encrypting key used by the Encryption Key
                    Manager.
                  - Key label must have a private key associated with it.
                  - I(label) can be a maximum of 64 characters.
                  - Maps to KEYLAB2 on z/OS.
                type: str
                required: true
              encoding:
                description:
                  - How the label for the key encrypting key specified by
                    I(label) is encoded by the Encryption Key Manager.
                  - I(encoding) can either be set to C(l) for label encoding,
                    or C(h) for hash encoding.
                  - Maps to KEYCD2 on z/OS.
                type: str
                required: true
                choices:
                  - l
                  - h
          key_length:
            description:
              - The length of the keys used in a new data set.
              - If using SMS, setting I(key_length) overrides the key length defined in the SMS data class of the data set.
              - Valid values are (0-255 non-vsam), (1-255 vsam).
            type: int
            required: false
          key_offset:
            description:
              - The position of the first byte of the record key in each logical record of a new VSAM data set.
              - The first byte of a logical record is position 0.
              - Provide I(key_offset) only for VSAM key-sequenced data sets.
            type: int
            required: false
          record_length:
            description:
              - The logical record length. (e.g C(80)).
              - For variable data sets, the length must include the 4-byte prefix area.
              - "Defaults vary depending on format: If FB/FBA 80, if VB/VBA 137, if U 0."
              - Valid values are (1-32760 for non-VSAM,  1-32761 for VSAM).
              - Maps to LRECL on z/OS.
            type: int
            required: false
          record_format:
            description:
              - The format and characteristics of the records for new data set.
            type: str
            choices:
              - u
              - vb
              - vba
              - fb
              - fba
          return_content:
            description:
              - Determines how content should be returned to the user.
              - If not provided, no content from the DD is returned.
            type: dict
            required: false
            suboptions:
              type:
                description:
                  - The type of the content to be returned.
                  - C(text) means return content in encoding specified by I(response_encoding).
                  - I(src_encoding) and I(response_encoding) are only used when I(type=text).
                  - C(base64) means return content as base64 encoded in binary.
                type: str
                choices:
                  - text
                  - base64
                required: true
              src_encoding:
                description:
                  - The encoding of the data set on the z/OS system.
                type: str
                default: ibm-1047
              response_encoding:
                description:
                  - The encoding to use when returning the contents of the data set.
                type: str
                default: iso8859-1
      dd_unix:
        description:
          - The path to a file in UNIX System Services (USS).
        required: false
        type: dict
        suboptions:
          dd_name:
            description:
              - The DD name.
            required: true
            type: str
          path:
            description:
              - The path to an existing UNIX file.
              - Or provide the path to an new created UNIX file when I(status_group=OCREAT).
              - The provided path must be absolute.
            required: true
            type: str
          disposition_normal:
            description:
              - Indicates what to do with the UNIX file after normal termination of
                the program.
            type: str
            choices:
              - keep
              - delete
          disposition_abnormal:
            description:
              - Indicates what to do with the UNIX file after abnormal termination of
                the program.
            type: str
            choices:
              - keep
              - delete
          mode:
            description:
              - The file access attributes when the UNIX file is created specified in I(path).
              - Specify the mode as an octal number similarly to chmod.
              - Maps to PATHMODE on z/OS.
            type: int
          status_group:
            description:
              - The status for the UNIX file specified in I(path).
              - If you do not specify a value for the I(status_group) parameter, the module assumes that the
                pathname exists, searches for it, and fails the module if the pathname does not exist.
              - Maps to PATHOPTS status group file options on z/OS.
              - You can specify up to 6 choices.
              - I(oappend) sets the file offset to the end of the file before each write,
                so that data is written at the end of the file.
              - I(ocreat) specifies that if the file does not exist, the system is to create it.
                If a directory specified in the pathname does not exist, a new directory and a new file are not created.
                If the file already exists and I(oexcl) was not specified,
                the system allows the program to use the existing file.
                If the file already exists and I(oexcl) was specified,
                the system fails the allocation and the job step.
              - I(oexcl) specifies that if the file does not exist, the system is to create it.
                If the file already exists, the system fails the allocation and the job step.
                The system ignores I(oexcl) if I(ocreat) is not also specified.
              - I(onoctty) specifies that if the PATH parameter identifies a terminal device,
                opening of the file does not make the terminal device the controlling terminal for the process.
              - I(ononblock) specifies the following, depending on the type of file
              - For a FIFO special file
              - 1. With I(ononblock) specified and I(ordonly) access,
                   an open function for reading-only returns without delay.
              - 2. With I(ononblock) not specified and I(ordonly) access,
                   an open function for reading-only blocks (waits) until a
                   process opens the file for writing.
              - 3. With I(ononblock) specified and I(owronly) access,
                   an open function for writing-only returns an error if no
                   process currently has the file open for reading.
              - 4. With I(ononblock) not specified and I(owronly) access,
                   an open function for writing-only blocks (waits) until a
                   process opens the file for reading.
              - 5. For a character special file that supports nonblocking open
              - 6. If I(ononblock) is specified, an open function returns without
                   blocking (waiting) until the device is ready or available.
                   Device response depends on the type of device.
              - 7. If I(ononblock) is not specified, an open function blocks
                   (waits) until the device is ready or available.
              - I(ononblock) has no effect on other file types.
              - I(osync) specifies that the system is to move data from buffer storage
                to permanent storage before returning control from a callable service that performs a write.
              - "I(otrunc) specifies that the system is to truncate the file length to zero if
                all the following are true: the file specified exists,
                the file is a regular file,
                and the file successfully opened with I(ordwr) or I(owronly)."
              - When I(otrunc) is specified, the system does not change the mode and owner.
                I(otrunc) has no effect on FIFO special files or character special files.
            type: list
            elements: str
            choices:
              - oappend
              - ocreat
              - oexcl
              - onoctty
              - ononblock
              - osync
              - otrunc
            required: false
          access_group:
            description:
              - The kind of access to request for the UNIX file specified in I(path).
            type: str
            choices:
              - r
              - w
              - rw
              - read_only
              - write_only
              - read_write
              - ordonly
              - owronly
              - ordwr
            required: false
          file_data_type:
            description:
              - The type of data that is (or will be) stored in the file specified in I(path).
              - Maps to FILEDATA on z/OS.
            type: str
            default: binary
            choices:
              - binary
              - text
              - record
          block_size:
            description:
              - The block size, in bytes, for the UNIX file.
              - Default is dependent on I(record_format)
            type: int
            required: false
          record_length:
            description:
              - The logical record length for the UNIX file.
              - I(record_length) is required in situations where the data will be processed as
                records and therefore, I(record_length), I(block_size) and I(record_format) need to be supplied since
                a UNIX file would normally be treated as a stream of bytes.
              - Maps to LRECL on z/OS.
            type: int
            required: false
          record_format:
            description:
              - The record format for the UNIX file.
              - I(record_format) is required in situations where the data will be processed as
                records and therefore, I(record_length), I(block_size) and I(record_format) need to be supplied since
                a UNIX file would normally be treated as a stream of bytes.
            type: str
            choices:
              - u
              - vb
              - vba
              - fb
              - fba
          return_content:
            description:
              - Determines how content should be returned to the user.
              - If not provided, no content from the DD is returned.
            type: dict
            required: false
            suboptions:
              type:
                description:
                  - The type of the content to be returned.
                  - C(text) means return content in encoding specified by I(response_encoding).
                  - I(src_encoding) and I(response_encoding) are only used when I(type=text).
                  - C(base64) means return content as base64 encoded in binary.
                type: str
                choices:
                  - text
                  - base64
                required: true
              src_encoding:
                description:
                  - The encoding of the file on the z/OS system.
                type: str
                default: ibm-1047
              response_encoding:
                description:
                  - The encoding to use when returning the contents of the file.
                type: str
                default: iso8859-1
      dd_input:
        description:
          - I(dd_input) is used to specify an in-stream data set.
          - Input will be saved to a temporary data set with a record length of 80.
        required: false
        type: dict
        suboptions:
          dd_name:
            description:
              - The DD name.
            required: true
            type: str
          content:
            description:
              - The input contents for the DD.
              - I(dd_input) supports single or multiple lines of input.
              - Multi-line input can be provided as a multi-line string
                or a list of strings with 1 line per list item.
              - If a list of strings is provided, newlines will be
                added to each of the lines when used as input.
              - 'If a multi-line string is provided, use the proper block scalar
                style. YAML supports both
                L(literal,https://yaml.org/spec/1.2.2/#literal-style) and
                L(folded,https://yaml.org/spec/1.2.2/#line-folding) scalars.
                It is recommended to use the literal style indicator
                "|" with a block indentation indicator, for example;
                I(content: | 2) is a literal block style indicator with a 2 space
                indentation, the entire block will be indented and newlines
                preserved. The block indentation range is 1 - 9. While generally
                unnecessary, YAML does support block
                L(chomping,https://yaml.org/spec/1.2.2/#8112-block-chomping-indicator)
                indicators  "+" and "-" as well.'
              - When using the I(content) option for instream-data, the module
                will ensure that all lines contain a blank in columns 1 and 2
                and add blanks when not present while retaining a maximum length
                of 80 columns for any line. This is true for all I(content) types;
                string, list of strings and when using a YAML block indicator.
            required: true
            type: raw
          reserved_cols:
            description:
              - Determines how many columns at the beginning of the content are reserved with
                empty spaces.
            type: int
            required: false
            default: 2
          return_content:
            description:
              - Determines how content should be returned to the user.
              - If not provided, no content from the DD is returned.
            type: dict
            required: false
            suboptions:
              type:
                description:
                  - The type of the content to be returned.
                  - C(text) means return content in encoding specified by I(response_encoding).
                  - I(src_encoding) and I(response_encoding) are only used when I(type=text).
                  - C(base64) means return content as base64 encoded in binary.
                type: str
                choices:
                  - text
                  - base64
                required: true
              src_encoding:
                description:
                  - The encoding of the data set on the z/OS system.
                  - for I(dd_input), I(src_encoding) should generally not need to be changed.
                type: str
                default: ibm-1047
              response_encoding:
                description:
                  - The encoding to use when returning the contents of the data set.
                type: str
                default: iso8859-1
      dd_output:
        description:
          - Use I(dd_output) to specify
            - Content sent to the DD should be returned to the user.
        required: false
        type: dict
        suboptions:
          dd_name:
            description:
              - The DD name.
            required: true
            type: str
          return_content:
            description:
              - Determines how content should be returned to the user.
              - If not provided, no content from the DD is returned.
            type: dict
            required: true
            suboptions:
              type:
                description:
                  - The type of the content to be returned.
                  - C(text) means return content in encoding specified by I(response_encoding).
                  - I(src_encoding) and I(response_encoding) are only used when I(type=text).
                  - C(base64) means return content as base64 encoded in binary.
                type: str
                choices:
                  - text
                  - base64
                required: true
              src_encoding:
                description:
                  - The encoding of the data set on the z/OS system.
                  - for I(dd_input), I(src_encoding) should generally not need to be changed.
                type: str
                default: ibm-1047
              response_encoding:
                description:
                  - The encoding to use when returning the contents of the data set.
                type: str
                default: iso8859-1
      dd_dummy:
        description:
          - Use I(dd_dummy) to specify
            - No device or external storage space is to be allocated to the data set.
            - No disposition processing is to be performed on the data set.
          - I(dd_dummy) accepts no content input.
        required: false
        type: dict
        suboptions:
          dd_name:
            description:
              - The DD name.
            required: true
            type: str
      dd_vio:
        description:
          - I(dd_vio) is used to handle temporary data sets.
          - VIO data sets reside in the paging space; but,
            to the problem program and the access method,
            the data sets appear to reside on a direct access storage device.
          - You cannot use VIO for permanent data sets,
            VSAM data sets, or partitioned data sets extended (PDSEs).
        required: false
        type: dict
        suboptions:
          dd_name:
            description:
              - The DD name.
            required: true
            type: str
      dd_volume:
        description:
          - Use I(dd_volume) to specify the volume to use in the DD statement.
        required: false
        type: dict
        suboptions:
          dd_name:
            description: The DD name.
            required: true
            type: str
          volume_name:
            description:
              - The volume serial number.
            type: str
            required: true
          unit:
            description:
              - Device type for the volume.
              - This option is case sensitive.
            type: str
            required: true
          disposition:
            description:
              - I(disposition) indicates the status of a data set.
            type: str
            required: true
            choices:
              - new
              - shr
              - mod
              - old
      dd_concat:
        description:
          - I(dd_concat) is used to specify a data set concatenation.
        required: false
        type: dict
        suboptions:
          dd_name:
            description:
              - The DD name.
            required: true
            type: str
          dds:
            description:
              - "A list of DD statements, which can contain any of the following types:
                I(dd_data_set), I(dd_unix), and I(dd_input)."
            required: false
            type: list
            elements: dict
            suboptions:
              dd_data_set:
                description:
                  - Specify a data set.
                  - I(dd_data_set) can reference an existing data set. The
                    data set referenced with C(data_set_name) must be allocated
                    before the module L(zos_mvs_raw,./zos_mvs_raw.html) is run, you can
                    use L(zos_data_set,./zos_data_set.html) to allocate a data set.
                required: false
                type: dict
                suboptions:
                  data_set_name:
                    description:
                      - The data set name.
                      - A data set name can be a GDS relative name.
                      - When using GDS relative name and it is a positive generation, I(disposition=new) must be used.
                    type: str
                    required: false
                  raw:
                    description:
                      - Create a new data set and let the MVS program assign its own default DCB attributes.
                      - When C(raw=true), all supplied DCB attributes like disposition, space, volumes, SMS, keys, record settings, etc. are ignored.
                      - Using C(raw) option is not possible for all programs, use this for cases where the MVS program that is called is able to assign
                        its own default dataset attributes.
                    type: bool
                    default: false
                  type:
                    description:
                      - The data set type. Only required when I(disposition=new).
                      - Maps to DSNTYPE on z/OS.
                    type: str
                    choices:
                      - library
                      - pds
                      - pdse
                      - large
                      - basic
                      - seq
                      - rrds
                      - esds
                      - lds
                      - ksds
                  disposition:
                    description:
                      - I(disposition) indicates the status of a data set.
                      - Defaults to shr.
                    type: str
                    required: false
                    choices:
                      - new
                      - shr
                      - mod
                      - old
                  disposition_normal:
                    description:
                      - I(disposition_normal) indicates what to do with the data set after normal termination of the program.
                    type: str
                    required: false
                    choices:
                      - delete
                      - keep
                      - catalog
                      - uncatalog
                  disposition_abnormal:
                    description:
                      - I(disposition_abnormal) indicates what to do with the data set after abnormal termination of the
                        program.
                    type: str
                    required: false
                    choices:
                      - delete
                      - keep
                      - catalog
                      - uncatalog
                  reuse:
                    description:
                      - Determines if data set should be reused if I(disposition=new) and a data set with matching name already exists.
                      - If I(reuse=true), I(disposition) will be automatically switched to C(SHR).
                      - If I(reuse=false), and a data set with a matching name already exists, allocation will fail.
                      - Mutually exclusive with I(replace).
                      - I(reuse) is only considered when I(disposition=new)
                    type: bool
                    default: false
                  replace:
                    description:
                      - Determines if data set should be replaced if I(disposition=new) and a data set with matching name already exists.
                      - If I(replace=true), the original data set will be deleted, and a new data set created.
                      - If I(replace=false), and a data set with a matching name already exists, allocation will fail.
                      - Mutually exclusive with I(reuse).
                      - I(replace) is only considered when I(disposition=new)
                      - I(replace) will result in loss of all data in the original data set unless I(backup) is specified.
                    type: bool
                    default: false
                  backup:
                    description:
                      - Determines if a backup should be made of existing data set when I(disposition=new), I(replace=true),
                        and a data set with the desired name is found.
                      - I(backup) is only used when I(replace=true).
                    type: bool
                    default: false
                  space_type:
                    description:
                      - The unit of measurement to use when allocating space for a new data set
                        using I(space_primary) and I(space_secondary).
                    type: str
                    choices:
                      - trk
                      - cyl
                      - b
                      - k
                      - m
                      - g
                  space_primary:
                    description:
                      - The primary amount of space to allocate for a new data set.
                      - The value provided to I(space_type) is used as the unit of space for the allocation.
                      - Not applicable when I(space_type=blklgth) or I(space_type=reclgth).
                    type: int
                  space_secondary:
                    description:
                      - When primary allocation of space is filled,
                        secondary space will be allocated with the provided size as needed.
                      - The value provided to I(space_type) is used as the unit of space for the allocation.
                      - Not applicable when I(space_type=blklgth) or I(space_type=reclgth).
                    type: int
                  volumes:
                    description:
                      - The volume or volumes on which a data set resides or will reside.
                      - Do not specify the same volume multiple times.
                    type: raw
                    required: false
                  sms_management_class:
                    description:
                      - The desired management class for a new SMS-managed data set.
                      - I(sms_management_class) is ignored if specified for an existing data set.
                      - All values must be between 1-8 alpha-numeric characters.
                    type: str
                    required: false
                  sms_storage_class:
                    description:
                      - The desired storage class for a new SMS-managed data set.
                      - I(sms_storage_class) is ignored if specified for an existing data set.
                      - All values must be between 1-8 alpha-numeric characters.
                    type: str
                    required: false
                  sms_data_class:
                    description:
                      - The desired data class for a new SMS-managed data set.
                      - I(sms_data_class) is ignored if specified for an existing data set.
                      - All values must be between 1-8 alpha-numeric characters.
                    type: str
                    required: false
                  block_size:
                    description:
                      - The maximum length of a block in bytes.
                      - Default is dependent on I(record_format)
                    type: int
                    required: false
                  directory_blocks:
                    description:
                      - The number of directory blocks to allocate to the data set.
                    type: int
                    required: false
                  key_label:
                    description:
                      - The label for the encryption key used by the system to encrypt the data set.
                      - I(key_label) is the public name of a protected encryption key in the ICSF key repository.
                      - I(key_label) should only be provided when creating an extended format data set.
                      - Maps to DSKEYLBL on z/OS.
                    type: str
                    required: false
                  encryption_key_1:
                    description:
                      - The encrypting key used by the Encryption Key Manager.
                      - Specification of the key labels does not by itself enable encryption.
                        Encryption must be enabled by a data class that specifies an encryption format.
                    type: dict
                    required: false
                    suboptions:
                      label:
                        description:
                          - The label for the key encrypting key used by the Encryption Key
                            Manager.
                          - Key label must have a private key associated with it.
                          - I(label) can be a maximum of 64 characters.
                          - Maps to KEYLAB1 on z/OS.
                        type: str
                        required: true
                      encoding:
                        description:
                          - How the label for the key encrypting key specified by
                            I(label) is encoded by the Encryption Key Manager.
                          - I(encoding) can either be set to C(l) for label encoding,
                            or C(h) for hash encoding.
                          - Maps to KEYCD1 on z/OS.
                        type: str
                        required: true
                        choices:
                          - l
                          - h
                  encryption_key_2:
                    description:
                      - The encrypting key used by the Encryption Key Manager.
                      - Specification of the key labels does not by itself enable encryption.
                        Encryption must be enabled by a data class that specifies an encryption format.
                    type: dict
                    required: false
                    suboptions:
                      label:
                        description:
                          - The label for the key encrypting key used by the Encryption Key
                            Manager.
                          - Key label must have a private key associated with it.
                          - I(label) can be a maximum of 64 characters.
                          - Maps to KEYLAB2 on z/OS.
                        type: str
                        required: true
                      encoding:
                        description:
                          - How the label for the key encrypting key specified by
                            I(label) is encoded by the Encryption Key Manager.
                          - I(encoding) can either be set to C(l) for label encoding,
                            or C(h) for hash encoding.
                          - Maps to KEYCD2 on z/OS.
                        type: str
                        required: true
                        choices:
                          - l
                          - h
                  key_length:
                    description:
                      - The length of the keys used in a new data set.
                      - If using SMS, setting I(key_length) overrides the key length defined in the SMS data class of the data set.
                      - Valid values are (0-255 non-vsam), (1-255 vsam).
                    type: int
                    required: false
                  key_offset:
                    description:
                      - The position of the first byte of the record key in each logical record of a new VSAM data set.
                      - The first byte of a logical record is position 0.
                      - Provide I(key_offset) only for VSAM key-sequenced data sets.
                    type: int
                    required: false
                  record_length:
                    description:
                      - The logical record length. (e.g C(80)).
                      - For variable data sets, the length must include the 4-byte prefix area.
                      - "Defaults vary depending on format: If FB/FBA 80, if VB/VBA 137, if U 0."
                      - Valid values are (1-32760 for non-vsam,  1-32761 for vsam).
                      - Maps to LRECL on z/OS.
                    type: int
                    required: false
                  record_format:
                    description:
                      - The format and characteristics of the records for new data set.
                    type: str
                    choices:
                      - u
                      - vb
                      - vba
                      - fb
                      - fba
                  return_content:
                    description:
                      - Determines how content should be returned to the user.
                      - If not provided, no content from the DD is returned.
                    type: dict
                    required: false
                    suboptions:
                      type:
                        description:
                          - The type of the content to be returned.
                          - C(text) means return content in encoding specified by I(response_encoding).
                          - I(src_encoding) and I(response_encoding) are only used when I(type=text).
                          - C(base64) means return content as base64 encoded in binary.
                        type: str
                        choices:
                          - text
                          - base64
                        required: true
                      src_encoding:
                        description:
                          - The encoding of the data set on the z/OS system.
                        type: str
                        default: ibm-1047
                      response_encoding:
                        description:
                          - The encoding to use when returning the contents of the data set.
                        type: str
                        default: iso8859-1
              dd_unix:
                description:
                  - The path to a file in UNIX System Services (USS).
                required: false
                type: dict
                suboptions:
                  path:
                    description:
                      - The path to an existing UNIX file.
                      - Or provide the path to an new created UNIX file when I(status_group=ocreat).
                      - The provided path must be absolute.
                    required: true
                    type: str
                  disposition_normal:
                    description:
                      - Indicates what to do with the UNIX file after normal termination of
                        the program.
                    type: str
                    choices:
                      - keep
                      - delete
                  disposition_abnormal:
                    description:
                      - Indicates what to do with the UNIX file after abnormal termination of
                        the program.
                    type: str
                    choices:
                      - keep
                      - delete
                  mode:
                    description:
                      - The file access attributes when the UNIX file is created specified in I(path).
                      - Specify the mode as an octal number similar to chmod.
                      - Maps to PATHMODE on z/OS.
                    type: int
                  status_group:
                    description:
                      - The status for the UNIX file specified in I(path).
                      - If you do not specify a value for the I(status_group) parameter the module assumes that the
                        pathname exists, searches for it, and fails the module if the pathname does not exist.
                      - Maps to PATHOPTS status group file options on z/OS.
                      - You can specify up to 6 choices.
                      - I(oappend) sets the file offset to the end of the file before each write,
                        so that data is written at the end of the file.
                      - I(ocreat) specifies that if the file does not exist, the system is to create it.
                        If a directory specified in the pathname does not exist, one is not created,
                        and the new file is not created.
                        If the file already exists and I(oexcl) was not specified,
                        the system allows the program to use the existing file.
                        If the file already exists and I(oexcl) was specified,
                        the system fails the allocation and the job step.
                      - I(oexcl) specifies that if the file does not exist, the system is to create it.
                        If the file already exists, the system fails the allocation and the job step.
                        The system ignores I(oexcl) if I(ocreat) is not also specified.
                      - I(onoctty) specifies that if the PATH parameter identifies a terminal device,
                        opening of the file does not make the terminal device the controlling terminal for the process.
                      - I(ononblock) specifies the following, depending on the type of file
                      - For a FIFO special file
                      - 1. With I(ononblock) specified and I(ordonly) access,
                           an open function for reading-only returns without
                           delay.
                      - 2. With I(ononblock) not specified and I(ordonly) access,
                           an open function for reading-only blocks (waits) until
                           a process opens the file for writing.
                      - 3. With I(ononblock) specified and I(owronly) access,
                           an open function for writing-only returns an error if
                           no process currently has the file open for reading.
                      - 4. With I(ononblock) not specified and I(owronly) access,
                           an open function for writing-only blocks (waits) until
                           a process opens the file for reading.
                      - 5. For a character special file that supports nonblocking
                           open
                      - 6. If I(ononblock) is specified, an open function returns
                           without blocking (waiting) until the device is ready
                           or available. Device response depends on the type of
                           device.
                      - 7. If I(ononblock) is not specified, an open function
                           blocks (waits) until the device is ready or available.
                      - I(ononblock) has no effect on other file types.
                      - I(osync) specifies that the system is to move data from buffer storage
                        to permanent storage before returning control from a callable service that performs a write.
                      - "I(otrunc) specifies that the system is to truncate the file length to zero if
                        all the following are true: the file specified exists,
                        the file is a regular file,
                        and the file successfully opened with I(ordwr) or I(owronly)."
                      - When I(otrunc) is specified, the system does not change the mode and owner.
                        I(otrunc) has no effect on FIFO special files or character special files.
                    type: list
                    elements: str
                    choices:
                      - oappend
                      - ocreat
                      - oexcl
                      - onoctty
                      - ononblock
                      - osync
                      - otrunc
                    required: false
                  access_group:
                    description:
                      - The kind of access to request for the UNIX file specified in I(path).
                    type: str
                    choices:
                      - r
                      - w
                      - rw
                      - read_only
                      - write_only
                      - read_write
                      - ordonly
                      - owronly
                      - ordwr
                  file_data_type:
                    description:
                      - The type of data that is (or will be) stored in the file specified in I(path).
                      - Maps to FILEDATA on z/OS.
                    type: str
                    default: binary
                    choices:
                      - binary
                      - text
                      - record
                  block_size:
                    description:
                      - The block size, in bytes, for the UNIX file.
                      - Default is dependent on I(record_format)
                    type: int
                    required: false
                  record_length:
                    description:
                      - The logical record length for the UNIX file.
                      - I(record_length) is required in situations where the data will be processed as
                        records and therefore, I(record_length), I(block_size) and I(record_format) need to be supplied since
                        a UNIX file would normally be treated as a stream of bytes.
                      - Maps to LRECL on z/OS.
                    type: int
                    required: false
                  record_format:
                    description:
                      - The record format for the UNIX file.
                      - I(record_format) is required in situations where the data will be processed as
                        records and therefore, I(record_length), I(block_size) and I(record_format) need to be supplied since
                        a UNIX file would normally be treated as a stream of bytes.
                    type: str
                    choices:
                      - u
                      - vb
                      - vba
                      - fb
                      - fba
                  return_content:
                    description:
                      - Determines how content should be returned to the user.
                      - If not provided, no content from the DD is returned.
                    type: dict
                    required: false
                    suboptions:
                      type:
                        description:
                          - The type of the content to be returned.
                          - C(text) means return content in encoding specified by I(response_encoding).
                          - I(src_encoding) and I(response_encoding) are only used when I(type=text).
                          - C(base64) means return content as base64 encoded in binary.
                        type: str
                        choices:
                          - text
                          - base64
                        required: true
                      src_encoding:
                        description:
                          - The encoding of the file on the z/OS system.
                        type: str
                        default: ibm-1047
                      response_encoding:
                        description:
                          - The encoding to use when returning the contents of the file.
                        type: str
                        default: iso8859-1
              dd_input:
                description:
                  - I(dd_input) is used to specify an in-stream data set.
                  - Input will be saved to a temporary data set with a record length of 80.
                required: false
                type: dict
                suboptions:
                  content:
                    description:
                      - The input contents for the DD.
                      - I(dd_input) supports single or multiple lines of input.
                      - Multi-line input can be provided as a multi-line string
                        or a list of strings with 1 line per list item.
                      - If a list of strings is provided, newlines will be
                        added to each of the lines when used as input.
                      - 'If a multi-line string is provided, use the proper block scalar
                        style. YAML supports both
                        L(literal,https://yaml.org/spec/1.2.2/#literal-style) and
                        L(folded,https://yaml.org/spec/1.2.2/#line-folding) scalars.
                        It is recommended to use the literal style indicator
                        "|" with a block indentation indicator, for example;
                        I(content: | 2) is a literal block style indicator with a 2 space
                        indentation, the entire block will be indented and newlines
                        preserved. The block indentation range is 1 - 9. While generally
                        unnecessary, YAML does support block
                        L(chomping,https://yaml.org/spec/1.2.2/#8112-block-chomping-indicator)
                        indicators  "+" and "-" as well.'
                      - When using the I(content) option for instream-data, the module
                        will ensure that all lines contain a blank in columns 1 and 2
                        and add blanks when not present while retaining a maximum length
                        of 80 columns for any line. This is true for all I(content) types;
                        string, list of strings and when using a YAML block indicator.
                    required: true
                    type: raw
                  reserved_cols:
                    description:
                      - Determines how many columns at the beginning of the content are reserved with
                        empty spaces.
                    type: int
                    required: false
                    default: 2
                  return_content:
                    description:
                      - Determines how content should be returned to the user.
                      - If not provided, no content from the DD is returned.
                    type: dict
                    required: false
                    suboptions:
                      type:
                        description:
                          - The type of the content to be returned.
                          - C(text) means return content in encoding specified by I(response_encoding).
                          - I(src_encoding) and I(response_encoding) are only used when I(type=text).
                          - C(base64) means return content as base64 encoded in binary.
                        type: str
                        choices:
                          - text
                          - base64
                        required: true
                      src_encoding:
                        description:
                          - The encoding of the data set on the z/OS system.
                          - for I(dd_input), I(src_encoding) should generally not need to be changed.
                        type: str
                        default: ibm-1047
                      response_encoding:
                        description:
                          - The encoding to use when returning the contents of the data set.
                        type: str
                        default: iso8859-1
  tmp_hlq:
    description:
      - Override the default high level qualifier (HLQ) for temporary and backup
        datasets.
      - The default HLQ is the Ansible user used to execute the module and if
        that is not available, then the value C(TMPHLQ) is used.
    required: false
    type: str

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

notes:
    - When executing programs using L(zos_mvs_raw,./zos_mvs_raw.html), you may encounter errors
      that originate in the programs implementation. Two such known issues are
      noted below of which one has been addressed with an APAR.
    - 1. L(zos_mvs_raw,./zos_mvs_raw.html) module execution fails when invoking
      Database Image Copy 2 Utility or Database Recovery Utility in conjunction
      with FlashCopy or Fast Replication.
    - 2. L(zos_mvs_raw,./zos_mvs_raw.html) module execution fails when invoking DFSRRC00 with parm
      "UPB,PRECOMP", "UPB, POSTCOMP" or "UPB,PRECOMP,POSTCOMP". This issue is
      addressed by APAR PH28089.
    - 3. When executing a program, refer to the programs documentation as each programs requirments
      can vary fom DDs, instream-data indentation and continuation characters.
seealso:
- module: ibm.ibm_zos_core.zos_data_set
"""

RETURN = r"""
ret_code:
  description: The return code.
  returned: always
  type: dict
  contains:
    code:
      description: The return code number returned from the program.
      type: int
dd_names:
  description: All the related dds with the program.
  returned: on success
  type: list
  elements: dict
  contains:
    dd_name:
      description: The data definition name.
      type: str
    name:
      description: The data set or path name associated with the data definition.
      type: str
    content:
      description: The content contained in the data definition.
      type: list
      elements: str
    record_count:
      description: The lines of the content.
      type: int
    byte_count:
      description: The number of bytes in the response content.
      type: int
backups:
  description: List of any data set backups made during execution.
  returned: always
  type: dict
  contains:
    original_name:
      description: The original data set name for which a backup was made.
      type: str
    backup_name:
      description: The name of the data set containing the backup of content from data set in original_name.
      type: str
stdout:
  description: The stdout from a USS command or MVS command, if applicable.
  returned: always
  type: str
stderr:
  description: The stderr of a USS command or MVS command, if applicable.
  returned: failure
  type: str
"""

EXAMPLES = r"""
- name: List data sets matching pattern in catalog,
    save output to a new sequential data set and return output as text.
  zos_mvs_raw:
    program_name: idcams
    auth: true
    dds:
      - dd_data_set:
          dd_name: sysprint
          data_set_name: mypgm.output.ds
          disposition: new
          reuse: true
          type: seq
          space_primary: 5
          space_secondary: 1
          space_type: m
          volumes:
            - "000000"
          record_format: fb
          return_content:
            type: text
      - dd_input:
          dd_name: sysin
          content: " LISTCAT ENTRIES('SOME.DATASET.*')"

- name: Run ADRDSSU to dump a dataset without having to specify the DCB attributes for dd_data_set by using raw option.
  zos_mvs_raw:
    program_name: ADRDSSU
    auth: true
    verbose: true
    dds:
      - dd_data_set:
          dd_name: OUTDD
          data_set_name: "USER.TEST.DUMP"
          raw: true
      - dd_input:
          dd_name: SYSIN
          content: |
            DUMP DATASET(INCLUDE(USER.TEST.SOURCE)) -
            OUTDDNAME(OUTDD)
      - dd_output:
          dd_name: SYSPRINT
          return_content:
            type: text

- name: Full volume dump using ADDRDSU.
  zos_mvs_raw:
    program_name: adrdssu
    auth: true
    dds:
      - dd_data_set:
          dd_name: dumpdd
          data_set_name: mypgm.output.ds
          disposition: new
          disposition_normal: catalog
          disposition_abnormal: delete
          space_type: cyl
          space_primary: 10
          space_secondary: 10
          record_format: u
          record_length: 0
          block_size: 32760
          type: seq
      - dd_volume:
          dd_name: voldd
          volume_name: "000000"
          unit: "3390"
          disposition: old
      - dd_input:
          dd_name: sysin
          content: " VOLDUMP VOL(voldd) DSNAME(dumpdd) FULL"
      - dd_output:
          dd_name: sysprint
          return_content:
            type: text

- name: List data sets matching patterns in catalog,
    save output to a new sequential data set and return output as text.
  zos_mvs_raw:
    program_name: idcams
    auth: true
    dds:
      - dd_data_set:
          dd_name: sysprint
          data_set_name: mypgm.output.ds
          disposition: new
          reuse: true
          type: seq
          space_primary: 5
          space_secondary: 1
          space_type: m
          volumes:
            - "000000"
          record_format: fb
          return_content:
            type: text
      - dd_input:
          dd_name: sysin
          content:
            - LISTCAT ENTRIES('SOME.DATASET.*')
            - LISTCAT ENTRIES('SOME.OTHER.DS.*')
            - LISTCAT ENTRIES('YET.ANOTHER.DS.*')

- name: List data sets matching pattern in catalog,
    save output to an existing sequential data set and
    return output as text.
  zos_mvs_raw:
    program_name: idcams
    auth: true
    dds:
      - dd_data_set:
          dd_name: sysprint
          data_set_name: mypgm.output.ds
          disposition: shr
          return_content:
            type: text
      - dd_input:
          dd_name: sysin
          content: " LISTCAT ENTRIES('SOME.DATASET.*')"

- name: List data sets matching pattern in catalog,
    save output to a sequential data set. If the data set exists,
    then reuse it, if it does not exist, create it. Returns output as text.
  zos_mvs_raw:
    program_name: idcams
    auth: true
    dds:
      - dd_data_set:
          dd_name: sysprint
          data_set_name: mypgm.output.ds
          disposition: new
          reuse: true
          type: seq
          space_primary: 5
          space_secondary: 1
          space_type: m
          volumes:
            - "000000"
          record_format: fb
          return_content:
            type: text
      - dd_input:
          dd_name: sysin
          content: " LISTCAT ENTRIES('SOME.DATASET.*')"

- name: List data sets matching pattern in catalog,
    save output to a sequential data set. If the data set exists,
    then back up the existing data set and replace it.
    If the data set does not exist, create it.
    Returns backup name (if a backup was made) and output as text,
    and backup name.
  zos_mvs_raw:
    program_name: idcams
    auth: true
    dds:
      - dd_data_set:
          dd_name: sysprint
          data_set_name: mypgm.output.ds
          disposition: new
          replace: true
          backup: true
          type: seq
          space_primary: 5
          space_secondary: 1
          space_type: m
          volumes:
            - "000000"
            - "111111"
            - "SCR002"
          record_format: fb
          return_content:
            type: text
      - dd_input:
          dd_name: sysin
          content: " LISTCAT ENTRIES('SOME.DATASET.*')"

- name: List data sets matching pattern in catalog,
    save output to a file in UNIX System Services.
  zos_raw:
    save output to a file in UNIX System Services.
  zos_mvs_raw:
    program_name: idcams
    auth: true
    dds:
      - dd_unix:
          dd_name: sysprint
          path: /u/myuser/outputfile.txt
      - dd_input:
          dd_name: sysin
          content: " LISTCAT ENTRIES('SOME.DATASET.*')"

- name: List data sets matching pattern in catalog,
    save output to a file in UNIX System Services.
    Return the contents of the file in encoding IBM-1047,
    while the file is encoded in ISO8859-1.
  zos_mvs_raw:
    program_name: idcams
    auth: true
    dds:
      - dd_unix:
          dd_name: sysprint
          path: /u/myuser/outputfile.txt
          return_content:
            type: text
            src_encoding: iso8859-1
            response_encoding: ibm-1047
      - dd_input:
          dd_name: sysin
          content: " LISTCAT ENTRIES('SOME.DATASET.*')"

- name: List data sets matching pattern in catalog,
    return output to user, but don't store in persistent storage.
    Return the contents of the file in encoding IBM-1047,
    while the file is encoded in ISO8859-1.
  zos_mvs_raw:
    program_name: idcams
    auth: true
    dds:
      - dd_output:
          dd_name: sysprint
          return_content:
            type: text
            src_encoding: iso8859-1
            response_encoding: ibm-1047
      - dd_input:
          dd_name: sysin
          content: " LISTCAT ENTRIES('SOME.DATASET.*')"

- name: Take a set of data sets and write them to an archive.
  zos_mvs_raw:
    program_name: adrdssu
    auth: true
    dds:
      - dd_data_set:
          dd_name: archive
          data_set_name: myhlq.stor.darv1
          disposition: old
      - dd_data_set:
          dd_name: sysin
          data_set_name: myhlq.adrdssu.cmd
          disposition: shr
      - dd_dummy:
          dd_name: sysprint

- name: Merge two sequential data sets and write them to new data set
  zos_mvs_raw:
    program_name: sort
    auth: false
    parm: "MSGPRT=CRITICAL,LIST"
    dds:
      - dd_data_set:
          dd_name: sortin01
          data_set_name: myhlq.dfsort.main
          disposition: shr
      - dd_data_set:
          dd_name: sortin02
          data_set_name: myhlq.dfsort.new
      - dd_input:
          dd_name: sysin
          content: " MERGE FORMAT=CH,FIELDS=(1,9,A)"
      - dd_data_set:
          dd_name: sortout
          data_set_name: myhlq.dfsort.merge
          type: seq
          disposition: new
      - dd_unix:
          dd_name: sysout
          path: /tmp/sortpgmoutput.txt
          mode: 644
          status_group:
            - ocreat
          access_group: w

- name: List data sets matching a pattern in catalog,
    save output to a concatenation of data set members and
    files.
  zos_mvs_raw:
    pgm: idcams
    auth: true
    dds:
      - dd_concat:
          dd_name: sysprint
          dds:
            - dd_data_set:
                data_set_name: myhlq.ds1.out(out1)
            - dd_data_set:
                data_set_name: myhlq.ds1.out(out2)
            - dd_data_set:
                data_set_name: myhlq.ds1.out(out3)
            - dd_unix:
                path: /tmp/overflowout.txt
      - dd_input:
          dd_name: sysin
          content: " LISTCAT ENTRIES('SYS1.*')"

- name: Drop the contents of input dataset into output dataset using REPRO command.
  zos_mvs_raw:
    pgm: idcams
    auth: true
    dds:
      - dd_data_set:
          dd_name: INPUT
          data_set_name: myhlq.ds1.input
      - dd_data_set:
          dd_name: OUTPUT
          data_set_name: myhlq.ds1.output
      - dd_input:
          dd_name: sysin
          content: |
            " REPRO -
              INFILE(INPUT) -
              OUTFILE(OUTPUT)"
      - dd_output:
          dd_name: sysprint
          return_content:
            type: text

- name: Define a cluster using a literal block style indicator
      with a 2 space indentation.
  zos_mvs_raw:
    program_name: idcams
    auth: true
    dds:
      - dd_output:
          dd_name: sysprint
          return_content:
            type: text
      - dd_input:
          dd_name: sysin
          content: 2
            DEFINE CLUSTER -
                      (NAME(ANSIBLE.TEST.VSAM) -
                      CYL(10 10)  -
                      FREESPACE(20 20) -
                      INDEXED -
                      KEYS(32 0) -
                      NOERASE -
                      NONSPANNED -
                      NOREUSE -
                      SHAREOPTIONS(3 3) -
                      SPEED -
                      UNORDERED -
                      RECORDSIZE(4086 32600) -
                      VOLUMES(222222) -
                      UNIQUE)

- name: Simple FTP connection using frist and second columns.
  zos_mvs_raw:
    program_name: AMAPDUPL
    auth: true
    dds:
      - dd_output:
          dd_name: sysprint
          return_content:
            type: text
      - dd_data_set:
          dd_name: SYSUT1
          data_set_name: myhlq.ds1.output
          disposition: shr
      - dd_input:
          dd_name: sysin
          reserved_cols: 0
          content: |
            USERID=anonymous
            PASSWORD=anonymous
            TARGET_SYS=testcase.boulder.ibm.com
            TARGET_DSN=wessamp.bigfile

- name: List data sets matching pattern in catalog,
    save output to a new generation of gdgs.
  zos_mvs_raw:
    program_name: idcams
    auth: true
    dds:
      - dd_data_set:
          dd_name: sysprint
          data_set_name: TEST.CREATION(+1)
          disposition: new
          return_content:
            type: text
      - dd_input:
          dd_name: sysin
          content: " LISTCAT ENTRIES('SOME.DATASET.*')"

- name: List data sets matching pattern in catalog,
    save output to a gds already created.
  zos_mvs_raw:
    program_name: idcams
    auth: true
    dds:
      - dd_data_set:
          dd_name: sysprint
          data_set_name: TEST.CREATION(-2)
          return_content:
            type: text
      - dd_input:
          dd_name: sysin
          content: " LISTCAT ENTRIES('SOME.DATASET.*')"

- name: Recall a migrated data set.
  zos_mvs_raw:
    program_name: ikjeft01
    auth: true
    dds:
      - dd_output:
          dd_name: systsprt
          return_content:
            type: text
      - dd_input:
          dd_name: systsin
          content:
            - "HRECALL 'MY.DATASET' WAIT"
"""

from ansible_collections.ibm.ibm_zos_core.plugins.module_utils.better_arg_parser import (
    BetterArgParser,
)


from ansible_collections.ibm.ibm_zos_core.plugins.module_utils.dd_statement import (
    DDStatement,
    DummyDefinition,
    VIODefinition,
    VolumeDefinition,
)

from ansible_collections.ibm.ibm_zos_core.plugins.module_utils.zos_mvs_raw import (
    MVSCmd,
    RawDatasetDefinition,
    RawFileDefinition,
    RawInputDefinition,
    RawOutputDefinition,
)
from ansible_collections.ibm.ibm_zos_core.plugins.module_utils.dependency_checker import (
    validate_dependencies,
)
from ansible_collections.ibm.ibm_zos_core.plugins.module_utils.import_handler import ZOAUImportError
from ansible_collections.ibm.ibm_zos_core.plugins.module_utils import data_set
from ansible.module_utils.basic import AnsibleModule
from ansible_collections.ibm.ibm_zos_core.plugins.module_utils.ansible_module import (
    AnsibleModuleHelper,
)

import base64
import re
import traceback

from shlex import quote
from ansible_collections.ibm.ibm_zos_core.plugins.module_utils.log import SingletonLogger

try:
    from zoautil_py import datasets, zoau_io
except Exception:
    datasets = ZOAUImportError(traceback.format_exc())
    zoau_io = ZOAUImportError(traceback.format_exc())

ENCODING_ENVIRONMENT_VARS = {"_BPXK_AUTOCVT": "OFF"}


ACCESS_GROUP_NAME_MAP = {
    "read_only": "ordonly",
    "write_only": "owronly",
    "read_write": "ordwr",
    "r": "ordonly",
    "w": "owronly",
    "rw": "ordwr",
}


backups = []


def run_module():
    """Executes all module-related functions.

    Raises:
        ZOSRawError: When an issue occurs attempting to run the desired program.
    """
    dd_name_base = dict(dd_name=dict(type="str", required=True))

    dd_data_set_base = dict(
        data_set_name=dict(type="str"),
        raw=dict(type="bool", default=False),
        disposition=dict(type="str", choices=["new", "shr", "mod", "old"]),
        disposition_normal=dict(
            type="str",
            choices=["delete", "keep", "catalog", "uncatalog"],
        ),
        disposition_abnormal=dict(
            type="str",
            choices=["delete", "keep", "catalog", "uncatalog"],
        ),
        space_type=dict(type="str", choices=["trk", "cyl", "b", "k", "m", "g"]),
        space_primary=dict(type="int"),
        space_secondary=dict(type="int"),
        volumes=dict(type="raw"),
        sms_management_class=dict(type="str"),
        sms_storage_class=dict(type="str"),
        sms_data_class=dict(type="str"),
        block_size=dict(type="int"),
        directory_blocks=dict(type="int"),
        key_label=dict(type="str", no_log=True),
        type=dict(
            type="str",
            choices=[
                "library",
                "pds",
                "pdse",
                "seq",
                "basic",
                "large",
                "ksds",
                "rrds",
                "lds",
                "esds",
            ],
        ),
        encryption_key_1=dict(
            type="dict",
            no_log=True,
            options=dict(
                label=dict(type="str", required=True),
                encoding=dict(type="str", required=True, choices=["l", "h"]),
            ),
        ),
        encryption_key_2=dict(
            type="dict",
            no_log=True,
            options=dict(
                label=dict(type="str", required=True),
                encoding=dict(type="str", required=True, choices=["l", "h"]),
            ),
        ),
        key_length=dict(type="int", no_log=False),
        key_offset=dict(type="int", no_log=False),
        record_length=dict(type="int"),
        record_format=dict(type="str", choices=["u", "vb", "vba", "fb", "fba"]),
        return_content=dict(
            type="dict",
            options=dict(
                type=dict(type="str", choices=["text", "base64"], required=True),
                src_encoding=dict(type="str", default="ibm-1047"),
                response_encoding=dict(type="str", default="iso8859-1"),
            ),
        ),
        reuse=dict(type="bool", default=False),
        replace=dict(type="bool", default=False),
        backup=dict(type="bool", default=False),
    )

    dd_input_base = dict(
        content=dict(type="raw", required=True),
        reserved_cols=dict(type="int", required=False, default=2),
        return_content=dict(
            type="dict",
            options=dict(
                type=dict(type="str", choices=["text", "base64"], required=True),
                src_encoding=dict(type="str", default="ibm-1047"),
                response_encoding=dict(type="str", default="iso8859-1"),
            ),
        ),
    )

    dd_output_base = dict(
        return_content=dict(
            type="dict",
            required=True,
            options=dict(
                type=dict(type="str", choices=["text", "base64"], required=True),
                src_encoding=dict(type="str", default="ibm-1047"),
                response_encoding=dict(type="str", default="iso8859-1"),
            ),
        ),
    )

    dd_unix_base = dict(
        path=dict(type="str", required=True),
        disposition_normal=dict(type="str", choices=["keep", "delete"]),
        disposition_abnormal=dict(type="str", choices=["keep", "delete"]),
        mode=dict(type="int"),
        status_group=dict(
            type="list",
            elements="str",
            choices=[
                "ocreat",
                "oexcl",
                "oappend",
                "onoctty",
                "ononblock",
                "osync",
                "otrunc",
            ],
        ),
        access_group=dict(
            type="str",
            choices=[
                "r",
                "w",
                "rw",
                "read_only",
                "write_only",
                "read_write",
                "ordonly",
                "owronly",
                "ordwr",
            ],
        ),
        file_data_type=dict(
            type="str", choices=["binary", "text", "record"], default="binary"
        ),
        block_size=dict(type="int"),
        record_length=dict(type="int"),
        record_format=dict(type="str", choices=["u", "vb", "vba", "fb", "fba"]),
        return_content=dict(
            type="dict",
            options=dict(
                type=dict(type="str", choices=["text", "base64"], required=True),
                src_encoding=dict(type="str", default="ibm-1047"),
                response_encoding=dict(type="str", default="iso8859-1"),
            ),
        ),
    )

    dd_dummy_base = dict()

    dd_vio_base = dict()

    dd_concat_base = dict(
        dds=dict(
            type="list",
            elements="dict",
            options=dict(
                dd_data_set=dict(type="dict", options=dd_data_set_base),
                dd_input=dict(type="dict", options=dd_input_base),
                dd_unix=dict(type="dict", options=dd_unix_base),
            ),
        )
    )
    dd_volume_base = dict(
        volume_name=dict(type="str", required=True),
        unit=dict(type="str", required=True),
        disposition=dict(type="str", choices=["new", "shr", "mod", "old"], required=True),
    )
    dd_data_set = dict(type="dict", options=combine_dicts(dd_name_base, dd_data_set_base))
    dd_unix = dict(type="dict", options=combine_dicts(dd_name_base, dd_unix_base))
    dd_input = dict(type="dict", options=combine_dicts(dd_name_base, dd_input_base))
    dd_output = dict(type="dict", options=combine_dicts(dd_name_base, dd_output_base))
    dd_dummy = dict(type="dict", options=combine_dicts(dd_name_base, dd_dummy_base))
    dd_vio = dict(type="dict", options=combine_dicts(dd_name_base, dd_vio_base))
    dd_concat = dict(type="dict", options=combine_dicts(dd_name_base, dd_concat_base))
    dd_volume = dict(type="dict", options=combine_dicts(dd_name_base, dd_volume_base))

    module_args = dict(
        program_name=dict(type="str", aliases=["program", "pgm"], required=True),
        auth=dict(type="bool", default=False),
        verbose=dict(type="bool", default=False),
        parm=dict(type="str", required=False),
        tmp_hlq=dict(type="str", required=False, default=None),
        max_rc=dict(type="int", required=False, default=0),
        dds=dict(
            type="list",
            elements="dict",
            options=dict(
                dd_data_set=dd_data_set,
                dd_unix=dd_unix,
                dd_input=dd_input,
                dd_output=dd_output,
                dd_vio=dd_vio,
                dd_concat=dd_concat,
                dd_dummy=dd_dummy,
                dd_volume=dd_volume,
            ),
        ),
    )

    # ---------------------------------------------------------------------------- #
    #                            Validate arguments                                #
    # ---------------------------------------------------------------------------- #

    module = AnsibleModule(argument_spec=module_args, supports_check_mode=True)
    validate_dependencies(module)

    # ---------------------------------------------------------------------------- #
    #                                  Main Logic                                  #
    # ---------------------------------------------------------------------------- #

    result = dict(changed=False, dd_names=[], ret_code=dict(code=8))
    response = {}
    dd_statements = []

    if not module.check_mode:
        try:
            parms = parse_and_validate_args(module.params)

            # Initialize logging module
            module_verbosity_level = module._verbosity
            SingletonLogger().get_logger(module_verbosity_level)

            tmphlq = parms.get("tmp_hlq")
            dd_statements = build_dd_statements(parms)
            program = parms.get("program_name")
            program_parm = parms.get("parm")
            authorized = parms.get("auth")
            verbose = parms.get("verbose")
            max_rc = parms.get("max_rc")
            program_response = run_zos_program(
                program=program,
                parm=program_parm,
                dd_statements=dd_statements,
                authorized=authorized,
                verbose=verbose,
                tmphlq=tmphlq,
            )
            response = build_response(program_response.rc, dd_statements, program_response.stdout, program_response.stderr)
            result = combine_dicts(result, response)

            if program_response.rc > max_rc:
                raise ZOSRawError(
                    program,
                    "{0} {1}".format(program_response.stdout, program_response.stderr),
                )

            if program_response.rc != 0:
                result["changed"] = False
            else:
                result["changed"] = True

        except Exception as e:
            result["backups"] = backups
            module.fail_json(msg=repr(e), **result)
    else:
        result = dict(changed=True, dd_names=[], ret_code=dict(code=0))
    module.exit_json(**result)
    # ---------------------------------------------------------------------------- #


def parse_and_validate_args(params):
    """Perform additional argument validation to validate and update input content,

    Parameters
    ----------
        params : dict
               The raw module parameters as provided by AnsibleModule.

    Returns
    -------
        parsed_args : dict
                    The module parameters after validation and content updates.
    """
    dd_name_base = dict(dd_name=dict(type="dd", required=True))

    dd_data_set_base = dict(
        data_set_name=dict(type="data_set", required=True),
        raw=dict(type="bool", default=False),
        disposition=dict(type="str", choices=["new", "shr", "mod", "old"]),
        disposition_normal=dict(
            type="str",
            choices=["delete", "keep", "catalog", "uncatalog"],
        ),
        disposition_abnormal=dict(
            type="str",
            choices=["delete", "keep", "catalog", "uncatalog"],
        ),
        space_type=dict(type="str", choices=["trk", "cyl", "b", "k", "m", "g"]),
        space_primary=dict(type="int"),
        space_secondary=dict(type="int"),
        volumes=dict(type=volumes),
        sms_management_class=dict(type=sms_class),
        sms_storage_class=dict(type=sms_class),
        sms_data_class=dict(type=sms_class),
        block_size=dict(type="int"),
        directory_blocks=dict(type="int"),
        key_label=dict(type="str"),
        type=dict(
            type="str",
            choices=[
                "library",
                "pds",
                "pdse",
                "seq",
                "basic",
                "large",
                "ksds",
                "rrds",
                "lds",
                "esds",
            ],
        ),
        encryption_key_1=dict(
            type="dict",
            options=dict(
                label=dict(type="str", required=True),
                encoding=dict(type="str", required=True, choices=["l", "h"]),
            ),
        ),
        encryption_key_2=dict(
            type="dict",
            options=dict(
                label=dict(type="str", required=True),
                encoding=dict(type="str", required=True, choices=["l", "h"]),
            ),
        ),
        key_length=dict(
            type=key_length, default=key_length_default, dependencies=["type"]
        ),
        key_offset=dict(
            type=key_offset, default=key_offset_default, dependencies=["type"]
        ),
        record_length=dict(type="int"),
        record_format=dict(type="str", choices=["u", "vb", "vba", "fb", "fba"]),
        return_content=dict(
            type="dict",
            options=dict(
                type=dict(type="str", choices=["text", "base64"], required=True),
                src_encoding=dict(type="str", default="ibm-1047"),
                response_encoding=dict(type="str", default="iso8859-1"),
            ),
        ),
        reuse=dict(type=reuse, default=False, dependencies=["disposition"]),
        replace=dict(
            type=replace, default=False, dependencies=["reuse", "disposition"]
        ),
        backup=dict(type=backup, default=False, dependencies=["replace"]),
    )

    dd_input_base = dict(
        content=dict(type=dd_content, required=True, dependencies=["reserved_cols"]),
        reserved_cols=dict(type="int", required=False, default=2),
        return_content=dict(
            type="dict",
            options=dict(
                type=dict(type="str", choices=["text", "base64"], required=True),
                src_encoding=dict(type="str", default="ibm-1047"),
                response_encoding=dict(type="str", default="iso8859-1"),
            ),
        ),
    )

    dd_output_base = dict(
        return_content=dict(
            type="dict",
            required=True,
            options=dict(
                type=dict(type="str", choices=["text", "base64"], required=True),
                src_encoding=dict(type="str", default="ibm-1047"),
                response_encoding=dict(type="str", default="iso8859-1"),
            ),
        ),
    )

    dd_unix_base = dict(
        path=dict(type="path", required=True),
        disposition_normal=dict(type="str", choices=["keep", "delete"]),
        disposition_abnormal=dict(type="str", choices=["keep", "delete"]),
        mode=dict(type="int"),
        status_group=dict(
            type=status_group,
        ),
        access_group=dict(
            type=access_group,
        ),
        file_data_type=dict(
            type="str", choices=["binary", "text", "record"], default="binary"
        ),
        block_size=dict(type="int"),
        record_length=dict(type="int"),
        record_format=dict(type="str", choices=["u", "vb", "vba", "fb", "fba"]),
        return_content=dict(
            type="dict",
            options=dict(
                type=dict(type="str", choices=["text", "base64"], required=True),
                src_encoding=dict(type="str", default="ibm-1047"),
                response_encoding=dict(type="str", default="iso8859-1"),
            ),
        ),
    )

    dd_dummy_base = dict()

    dd_vio_base = dict()

    dd_concat_base = dict(
        dds=dict(
            type="list",
            elements="dict",
            options=dict(
                dd_data_set=dict(type="dict", options=dd_data_set_base),
                dd_input=dict(type="dict", options=dd_input_base),
                dd_unix=dict(type="dict", options=dd_unix_base),
            ),
        )
    )
    dd_volume_base = dict(
        volume_name=dict(type="str", required=True),
        unit=dict(type="str", required=True),
        disposition=dict(type="str", choices=["new", "shr", "mod", "old"], required=True),
    )
    dd_data_set = dict(type="dict", options=combine_dicts(dd_name_base, dd_data_set_base))
    dd_unix = dict(type="dict", options=combine_dicts(dd_name_base, dd_unix_base))
    dd_input = dict(type="dict", options=combine_dicts(dd_name_base, dd_input_base))
    dd_output = dict(type="dict", options=combine_dicts(dd_name_base, dd_output_base))
    dd_dummy = dict(type="dict", options=combine_dicts(dd_name_base, dd_dummy_base))
    dd_vio = dict(type="dict", options=combine_dicts(dd_name_base, dd_vio_base))
    dd_concat = dict(type="dict", options=combine_dicts(dd_name_base, dd_concat_base))
    dd_volume = dict(type="dict", options=combine_dicts(dd_name_base, dd_volume_base))

    module_args = dict(
        program_name=dict(type="str", aliases=["program", "pgm"], required=True),
        auth=dict(type="bool", default=False),
        verbose=dict(type="bool", default=False),
        parm=dict(type="str", required=False),
        tmp_hlq=dict(type="qualifier_or_empty", required=False, default=None),
        max_rc=dict(type="int", required=False, default=0),
        dds=dict(
            type="list",
            elements="dict",
            default=[],
            options=dict(
                dd_data_set=dd_data_set,
                dd_unix=dd_unix,
                dd_input=dd_input,
                dd_output=dd_output,
                dd_vio=dd_vio,
                dd_concat=dd_concat,
                dd_dummy=dd_dummy,
                dd_volume=dd_volume,
            ),
        ),
        # verbose=dict(type="bool", required=False),
        # debug=dict(type="bool", required=False),
    )
    parser = BetterArgParser(module_args)
    parsed_args = parser.parse_args(params)
    return parsed_args


def combine_dicts(dict1, dict2):
    """Combine two dictionaries.
    Provides clean way to combine two dictionaries in python >= 2

    Parameters
    ----------
        dict1 : dict
              The first dict to add to combine
        dict2 : dict
              The second dict to add to combine

    Returns
    -------
        merged_dict : dict
                    The combination of dict1 and dict2.
    """
    merged_dict = dict1.copy()
    merged_dict.update(dict2)
    return merged_dict


def key_length(contents, dependencies):
    """Validates key length

    Parameters
    ----------
        contents : int
                 Argument contents
        dependencies : dict
                     Any dependent arguments

    Raises
    -------
        ValueError : str
                   When invalid argument provided.

    Returns
    -------
        contents : int
                 Provided key length
    """
    if contents is None:
        return contents
    if contents is not None and dependencies.get("type") != "ksds":
        raise ValueError('key_length is only valid when "type=ksds".')
    if not re.fullmatch(r"[0-9]+", str(contents)):
        raise ValueError(
            'Invalid argument "{0}" for type "key_length".'.format(str(contents))
        )

    return int(contents)


def key_offset(contents, dependencies):
    """Validates key offset

    Parameters
    ----------
        contents : int
                 Argument contents
        dependencies : dict
                     Any dependent arguments

    Raises
    -------
        ValueError : str
                   When invalid argument provided.

    Returns
    -------
        contents : int
                 Provided key offset
    """
    if contents is None:
        return contents
    if contents is not None and dependencies.get("type") != "ksds":
        raise ValueError('key_offset is only valid when "type=ksds".')

    if not re.fullmatch(r"[0-9]+", str(contents)):
        raise ValueError(
            'Invalid argument "{0}" for type "key_offset".'.format(str(contents))
        )

    return int(contents)


def key_length_default(contents, dependencies):
    """Determines default key length

    Parameters
    ----------
        contents : int
                 Argument contents
        dependencies : dict
                     Any dependent arguments

    Returns
    -------
        length : int
               Default key length
    """
    KEY_LENGTH = 5
    length = None
    if contents is None and dependencies.get("type") == "ksds":
        length = KEY_LENGTH
    elif dependencies.get("type") == "ksds":
        length = contents
    return length


def key_offset_default(contents, dependencies):
    """Determines default key offset

    Parameters
    ----------
        contents : int
                 Argument contents
        dependencies : dict
                     Any dependent arguments

    Returns
    -------
        offset : int
               Default key offset
    """
    KEY_OFFSET = 0
    offset = None
    if contents is None and dependencies.get("type") == "ksds":
        offset = KEY_OFFSET
    elif dependencies.get("type") == "ksds":
        offset = contents
    return offset


def dd_content(contents, dependencies):
    """Reformats dd content arguments

    Parameters
    ----------
        contents : Union[str, list[str]]
                 Argument contents
        dependencies : dict
                     Any dependent arguments

    Returns
    -------
        contents : str
                 Content string to save to data set
    """
    if contents is None:
        return None
    if contents is not None:
        # Empty string can be passed for content but not modify to ensure proper entry
        spaces = dependencies.get("reserved_cols")
        if len(contents) > 0:
            contents = modify_contents(contents, spaces)
        return contents
    if isinstance(contents, list):
        return "\n".join(contents)
    return contents


def modify_contents(contents, spaces):
    """Return the content of dd_input to a valid form for a JCL program.

    Parameters
    ----------
        contents : str or list
                 The string or list with the program.

    Returns
    -------
        contents : str
                 The content in a proper multi line str.
    """
    if not isinstance(contents, list):
        contents = list(contents.split("\n"))
    contents = prepend_spaces(contents, spaces)
    contents = "\n".join(contents)
    return contents


def prepend_spaces(lines, spaces=2):
    """Return the array with two spaces at the beggining.

    Parameters
    ----------
        lines : list
              The list with a line of a program.
        spaces : int
              The number of columns to add as left padding to the content.

    Raises
    -------
        ValueError : str
                   When invalid argument provided.

    Returns
    -------
        new_lines : list[str]
                  The list in a proper two spaces and the code.
    """
    module = AnsibleModuleHelper(argument_spec={})
    for index, line in enumerate(lines):
        if len(line) > 0:
            if len(line) > 80:
                msg = """Length of line {0} is over 80 characters. The maximum length allowed is 80 characters. """
                module.fail_json(msg=msg.format(line))
            else:
                len_line = len(line)
                lines[index] = line.rjust(len_line + spaces, " ")
                if len(lines[index]) > 80:
                    msg = """Length of line {0} is over 80 characters. The maximum length allowed is 80 characters. Including the spaces at the beginning.
                    Be aware that the module has reserved {1} columns to the left for JCL processing this value can be modified if the program allows for it."""
                    module.fail_json(msg=msg.format(line, spaces))
    return lines


def sms_class(contents, dependencies):
    """Validates provided sms class is of valid length.

    Parameters
    ----------
        contents : str
                 Argument contents
        dependencies : dict
                     Any dependent arguments

    Returns
    -------
        contents : str
                  the sms class
    """
    if not contents:
        return None
    if len(contents) < 1 or len(contents) > 8:
        raise ValueError(
            (
                "Value {0} is invalid for an SMS class argument. "
                "SMS class must be between 1 and 8 characters."
            ).format(contents)
        )
    return contents


def volumes(contents, dependencies):
    """Validate volume arguments.

    Parameters
    ----------
        contents : Union[str, list[str]]
                 The contents provided for the volume argument.
        dependencies : dict
                     Any arguments this argument is dependent on.

    Raises
    -------
        ValueError : str
                   When invalid argument provided.

    Returns
    -------
        contents : list[str]
                 The contents returned as a list of volumes
    """
    if not contents:
        return None
    if not isinstance(contents, list):
        contents = [contents]
    for vol in contents:
        if not re.fullmatch(
            r"^[A-Z0-9]{1,6}$",
            str(vol),
            re.IGNORECASE,
        ):
            raise ValueError('Invalid argument "{0}" for type "volumes".'.format(vol))
        vol = vol.upper()
    return contents


def reuse(contents, dependencies):
    """Validate reuse argument.

    Parameters
    ----------
        contents : bool
                 The contents provided for the reuse argument.
        dependencies : dict
                     Any arguments this argument is dependent on.

    Raises
    -------
        ValueError : str
                   When invalid argument provided.

    Returns
    -------
        contents : bool
                 The value of reuse.
    """
    if contents is True and dependencies.get("disposition") != "new":
        raise ValueError('Argument "reuse" is only valid when "disposition" is "new".')
    return contents


def replace(contents, dependencies):
    """Validate replace argument.

    Parameters
    ----------
        contents : bool
                 The contents provided for the replace argument.
        dependencies : dict
                     Any arguments this argument is dependent on.

    Raises
    -------
        ValueError : str
                   When invalid argument provided.

    Returns
    -------
        contents : bool
                 The value of replace.
    """
    if contents is True and dependencies.get("reuse") is True:
        raise ValueError('Arguments "replace" and "reuse" are mutually exclusive.')
    if contents is True and dependencies.get("disposition") != "new":
        raise ValueError(
            'Argument "replace" is only valid when "disposition" is "new".'
        )
    return contents


def backup(contents, dependencies):
    """Validate backup argument.

    Parameters
    ----------
        contents : bool
                 The contents provided for the backup argument.
        dependencies : dict
                     Any arguments this argument is dependent on.

    Raises
    -------
        ValueError : str
                   When invalid argument provided.

    Returns
    -------
        contents : bool
                 The value of backup.
    """
    if contents is True and dependencies.get("replace") is False:
        raise ValueError('Argument "backup" is only valid when "replace" is True.')
    return contents


def status_group(contents, dependencies):
    """Validate status group argument.

    Parameters
    ----------
        contents : list[str]
                 The contents provided for the status_group argument.
        dependencies : dict
                     Any arguments this argument is dependent on.

    Returns
    -------
        contents : list[str]
                 The access group as expected by mvscmd.
    """
    if not contents:
        return None
    choices = ["ocreat", "oexcl", "oappend", "onoctty", "ononblock", "osync", "otrunc"]
    for item in contents:
        if item.lower() not in choices:
            raise ValueError(
                "Invalid argument '{0}' for type status_group. Valid options are: {1}.".format(
                    item, ", ".join(choices)
                )
            )
        else:
            item = item.lower()
    return contents


def access_group(contents, dependencies):
    """Validate access group argument.

    Parameters
    ----------
        contents : str
                 The contents provided for the access_group argument.
        dependencies : dict
                     Any arguments this argument is dependent on.

    Returns
    -------
        contents : str
                 The access group as expected by mvscmd.
    """
    if contents and ACCESS_GROUP_NAME_MAP.get(contents):
        contents = ACCESS_GROUP_NAME_MAP.get(contents)
    return contents


def build_dd_statements(parms):
    """Build a list of DDStatement objects from provided module parms.

    Parameters
    ----------
        parms : dict
              Module parms after formatting and validation.

    Raises
    -------
        ValueError : None
             If no data definition can be found matching provided DD type.

    Returns
    -------
        dd_statements : list[DDStatement]
                      List of DDStatement objects representing DD statements specified in module parms.
    """
    dd_statements = []
    tmphlq = parms.get("tmp_hlq")
    for dd in parms.get("dds"):
        dd_name, key = get_dd_name_and_key(dd)
        dd = set_extra_attributes_in_dd(dd, tmphlq, key)
        data_definition = build_data_definition(dd)
        if data_definition is None:
            raise ValueError("No valid data definition found.")
        dd_statement = DDStatement(dd_name, data_definition)
        dd_statements.append(dd_statement)
    return dd_statements


def get_key(dd):
    """
    Get the key of the dd.
    Parameters
    ----------
        dd : dict
           A single DD parm as specified in module parms.

    Returns
    -------
        key : str
            Type of dd.
    """
    dd_key = ""
    keys_list = list(dd.keys())
    for key in keys_list:
        if "dd" in key:
            dd_key = key
    return dd_key


def get_dd_name_and_key(dd):
    """
    Get the key and dd_name of the dd.
    Parameters
    ----------
        dd : dict
           A single DD parm as specified in module parms.

    Returns
    -------
        dd_name : str
                Identifier of the dd.
        key : str
            Type of dd.
    """
    dd_name = ""
    key = ""
    if dd.get("dd_data_set"):
        dd_name = dd.get("dd_data_set").get("dd_name")
        raw_flag = dd.get("dd_data_set").get("raw", False)
        data_set_name, disposition = resolve_data_set_names(dd.get("dd_data_set").get("data_set_name"),
                                                            dd.get("dd_data_set").get("disposition"),
                                                            dd.get("dd_data_set").get("type"),
                                                            raw=raw_flag)
        dd.get("dd_data_set")["data_set_name"] = data_set_name
        dd.get("dd_data_set")["disposition"] = disposition
        key = "dd_data_set"
    elif dd.get("dd_unix"):
        dd_name = dd.get("dd_unix").get("dd_name")
        key = "dd_unix"
    elif dd.get("dd_input"):
        dd_name = dd.get("dd_input").get("dd_name")
        key = "dd_input"
    elif dd.get("dd_output"):
        dd_name = dd.get("dd_output").get("dd_name")
        key = "dd_output"
    elif dd.get("dd_vio"):
        dd_name = dd.get("dd_vio").get("dd_name")
        key = "dd_vio"
    elif dd.get("dd_dummy"):
        dd_name = dd.get("dd_dummy").get("dd_name")
        key = "dd_dummy"
    elif dd.get("dd_concat"):
        dd_name = dd.get("dd_concat").get("dd_name")
        key = "dd_concat"
    elif dd.get("dd_volume"):
        dd_name = dd.get("dd_volume").get("dd_name")
        key = "dd_volume"
    return dd_name, key


def set_extra_attributes_in_dd(dd, tmphlq, key):
    """
    Set any extra attributes in dds like in global tmp_hlq.
    Parameters
    ----------
        dd : dict
           A single DD parm as specified in module parms.

    Returns
    -------
        dd : dict
            A single DD parm as specified in module parms.
    """
    if key == "dd_concat":
        for single_dd in dd.get("dd_concat").get("dds", []):
            key_concat = get_key(single_dd)
            set_extra_attributes_in_dd(single_dd, tmphlq, key_concat)
    elif dd.get(key):
        dd.get(key)["tmphlq"] = tmphlq
    return dd


def resolve_data_set_names(dataset, disposition, type, raw=False):
    """Resolve cases for data set names as relative gds or positive
      that could be accepted if disposition is new.
      Parameters
      ----------
      dataset : str
          Data set name to determine if is a GDS relative name or regular name.
      disposition : str
          Disposition of data set for it creation.
      Returns
      -------
      str
          The absolute name of dataset or relative positive if disposition is new.
      str
          The disposition base on the system
    """
    if raw:
        return dataset, disposition or "shr"
    if disposition:
        disp = disposition
    else:
        disp = "shr"
    if data_set.DataSet.is_gds_relative_name(dataset):
        if data_set.DataSet.is_gds_positive_relative_name(dataset):
            if disp == "new":
                if type:
                    new_generation = datasets.create(name=dataset, dataset_type=type)
                    return new_generation.name, "shr"
                else:
                    new_generation = datasets.create(name=dataset, dataset_type="seq")
                    return new_generation.name, "shr"
            else:
                raise ("To generate a new GDS as {0} disposition 'new' is required.".format(dataset))
        else:
            data = data_set.MVSDataSet(
                name=dataset
            )
            src = data.name
            if data.is_gds_active:
                if disposition and disp == "new":
                    raise ("GDS {0} already created, incorrect parameters for disposition and data_set_name".format(src))
                else:
                    return src, disposition
            else:
                raise ("{0} does not exist".format(src))
    else:
        return dataset, disp


def validate_raw_parameter(dd_params):
    """Validate that when raw=true, no other dataset parameters are specified."""
    if dd_params.get('raw'):
        # Parameters that should not be used with raw=true
        incompatible_params = [
            'disposition_normal', 'disposition_abnormal',
            'space_type', 'space_primary', 'space_secondary', 'volumes',
            'sms_management_class', 'sms_storage_class', 'sms_data_class',
            'block_size', 'directory_blocks', 'key_label', 'type',
            'encryption_key_1', 'encryption_key_2', 'key_length', 'key_offset',
            'record_length', 'record_format'
        ]

        specified_params = []
        for param in incompatible_params:
            if dd_params.get(param) is not None:
                specified_params.append(param)

        if specified_params:
            raise ValueError(
                "The following parameters cannot be used when 'raw=true': {}. "
                "When using raw datasets, the program determines all dataset attributes. "
                "Either remove these parameters or set raw=false.".format(
                    ', '.join(specified_params))
            )


def build_data_definition(dd):
    """Build a DataDefinition object for a particular DD parameter.

    Parameters
    ----------
        dd : dict
           A single DD parm as specified in module parms.

    Returns
    -------
        data_definition : Union[list[RawDatasetDefinition, RawFileDefinition,vRawInputDefinition],
                          RawDatasetDefinition, RawFileDefinition, RawInputDefinition, DummyDefinition]
                        The DataDefinition object or a list of DataDefinition objects.
    """
    data_definition = None
    if dd.get("dd_data_set"):
        validate_raw_parameter(dd.get("dd_data_set"))
        data_definition = RawDatasetDefinition(
            **(dd.get("dd_data_set")))
        if data_definition.backup:
            backups.append(get_backups(data_definition.backup, dd.get("dd_data_set").get("data_set_name")))
    elif dd.get("dd_unix"):
        data_definition = RawFileDefinition(
            **(dd.get("dd_unix")))
    elif dd.get("dd_input"):
        data_definition = RawInputDefinition(
            **(dd.get("dd_input")))
    elif dd.get("dd_output"):
        data_definition = RawOutputDefinition(
            **(dd.get("dd_output")))
    elif dd.get("dd_vio"):
        data_definition = VIODefinition(
            dd.get("dd_vio").get("tmphlq"))
    elif dd.get("dd_dummy"):
        data_definition = DummyDefinition()
    elif dd.get("dd_volume"):
        volume_args = dd["dd_volume"]
        data_definition = VolumeDefinition(
            volume_name=volume_args.get("volume_name"),
            unit=volume_args.get("unit"),
            disposition=volume_args.get("disposition"),
        )

    elif dd.get("dd_concat"):
        data_definition = []
        for single_dd in dd.get("dd_concat").get("dds", []):
            data_definition.append(build_data_definition(single_dd))
    return data_definition


def get_backups(backup, data_set_name):
    backups = {"original_name": data_set_name, "backup_name": backup}
    return backups


def run_zos_program(
    program, parm="", dd_statements=None, authorized=False, verbose=False, tmphlq=None
):
    """Run a program on z/OS.

    Parameters
    ----------
        program : str
                The name of the program to run.
        parm : str, optional
             Additional argument string if required. Defaults to "".
        dd_statements : list[DDStatement], optional
                      DD statements to allocate for the program. Defaults to [].
        authorized : bool, optional
                   Determines if program will execute as an authorized user. Defaults to False.
        tmphlq : str, optional
               Arguments overwrite variable tmp_hlq

    Returns
    -------
        response : MVSCmdResponse
                 Holds the response information for program execution.
    """
    if not dd_statements:
        dd_statements = []
    response = None
    if authorized:
        response = MVSCmd.execute_authorized(
            pgm=program, parm=parm, dds=dd_statements, verbose=verbose, tmp_hlq=tmphlq
        )
    else:
        response = MVSCmd.execute(
            pgm=program, parm=parm, dds=dd_statements, verbose=verbose, tmp_hlq=tmphlq
        )
    return response


def build_response(rc, dd_statements, stdout, stderr):
    """Build response dictionary to return at module completion.

    Parameters
    ----------
        rc : int
           The return code of the program.
        dd_statements : list[DDStatement]
                      The DD statements for the program.

    Returns
    -------
        response : dict
                 Response dictionary in format expected for response on module completion.
    """
    response = {"ret_code": {"code": rc}}
    response["backups"] = gather_backups(dd_statements)
    response["dd_names"] = gather_output(dd_statements)
    response["stdout"] = stdout
    response["stderr"] = stderr
    return response


def gather_backups(dd_statements):
    """Gather backup information for all data sets which had
    a backup made during module execution.

    Parameters
    ----------
        dd_statements : list[DDStatement]
                      The DD statements for the program.

    Returns
    -------
        backups : list[dict]
                List of backups in format expected for response on module completion.
    """
    backups = []
    for dd_statement in dd_statements:
        backups += get_dd_backup(dd_statement)
    return backups


def get_dd_backup(dd_statement):
    """Gather backup information for a single data set
    if the DD is a data set DD and a backup was made.

    Parameters
    ----------
        dd : DataDefinition
           A single DD statement.

    Returns
    -------
        dd_backup : list[dict]
                  List of backups in format expected for response on module completion.
    """
    dd_backup = []
    if (
        isinstance(dd_statement.definition, RawDatasetDefinition)
        and dd_statement.definition.backup
    ):
        dd_backup = [get_data_set_backup(dd_statement)]
    elif isinstance(dd_statement.definition, list):
        dd_backup = get_concatenation_backup(dd_statement)
    return dd_backup


def get_data_set_backup(dd_statement):
    """Get backup of a single data set DD statement.

    Parameters
    ----------
        dd_statement : DDStatement
                     A single DD statement.

    Returns
    -------
        backup : dict
               Backup information in format expected for response on module completion.
    """
    backup = {}
    backup["backup_name"] = dd_statement.definition.backup
    backup["original_name"] = dd_statement.definition.name
    return backup


def get_concatenation_backup(dd_statement):
    """Get the backup information for a single concatenation DD statement.

    Parameters
    ----------
        dd_statement : DDStatement
                     A single DD statement.

    Returns
    -------
        dd_backup : list[dict]
                  The backup information of a single DD, in format expected for response on module completion.
                  Response can contain multiple backups.
    """
    # create new DDStatement objects for each concat member
    # makes it easier to handle concat and non-concat DDs consistently
    dds = []
    for dd in dd_statement.definition:
        new_dd = DDStatement(dd_statement.name, dd)
        dds.append(new_dd)
    dd_backup = gather_backups(dds)
    return dd_backup


def gather_output(dd_statements):
    """Gather DD contents for all DD statements for which
    content was requested.

    Parameters
    ----------
        dd_statements : list[DDStatement]
                      The DD statements for the program.

    Returns
    -------
        output : list[dict]
               The list of DD outputs, in format expected for response on module completion.
    """
    output = []
    for dd_statement in dd_statements:
        output += get_dd_output(dd_statement)
    return output


def get_dd_output(dd_statement):
    """Get the output for a single DD statement.

    Parameters
    ----------
        dd_statement : DDStatement
                     A single DD statement.

    Returns
    -------
        dd_output : list[dict]
                  The output of a single DD, in format expected for response on module completion.
    """
    dd_output = []
    if (
        isinstance(dd_statement.definition, RawDatasetDefinition)
        and dd_statement.definition.return_content.type
    ):
        dd_output = [get_data_set_output(dd_statement)]
    elif (
        isinstance(dd_statement.definition, RawFileDefinition)
        and dd_statement.definition.return_content.type
    ):
        dd_output = [get_unix_file_output(dd_statement)]
    elif (
        isinstance(dd_statement.definition, RawInputDefinition)
        and dd_statement.definition.return_content.type
    ):
        dd_output = [get_data_set_output(dd_statement)]
    elif (
        isinstance(dd_statement.definition, RawOutputDefinition)
        and dd_statement.definition.return_content.type
    ):
        dd_output = [get_data_set_output(dd_statement)]
    elif isinstance(dd_statement.definition, list):
        dd_output = get_concatenation_output(dd_statement)
    return dd_output


def get_data_set_output(dd_statement):
    """Get the output of a single data set DD statement.

    Parameters
    ----------
        dd_statement : DDStatement
                     A single DD statement.

    Returns
    -------
        dd_response : dict
                    The output of a single DD, in format expected for response on module completion.
    """
    contents = ""
    if dd_statement.definition.return_content.type == "text":
        contents = get_data_set_content(
            name=dd_statement.definition.name,
            base64_encode=False,
            from_encoding=dd_statement.definition.return_content.src_encoding,
            to_encoding=dd_statement.definition.return_content.response_encoding,
        )
    elif dd_statement.definition.return_content.type == "base64":
        contents = get_data_set_content(name=dd_statement.definition.name, base64_encode=True)
    return build_dd_response(dd_statement.name, dd_statement.definition.name, contents)


def get_unix_file_output(dd_statement):
    """Get the output of a single UNIX file DD statement.

    Parameters
    ----------
        dd_statement : DDStatement
                     A single DD statement.

    Returns
    -------
        dd_response : dict
                    The output of a single DD, in format expected for response on module completion.
    """
    contents = ""
    if dd_statement.definition.return_content.type == "text":
        contents = get_unix_content(
            name=dd_statement.definition.name,
            base64_encode=False,
            from_encoding=dd_statement.definition.return_content.src_encoding,
            to_encoding=dd_statement.definition.return_content.response_encoding,
        )
    elif dd_statement.definition.return_content.type == "base64":
        contents = get_unix_content(name=dd_statement.definition.name, base64_encode=True)
    return build_dd_response(dd_statement.name, dd_statement.definition.name, contents)


def get_concatenation_output(dd_statement):
    """Get the output of a single concatenation DD statement.

    Parameters
    ----------
        dd_statement : DDStatement
                     A single DD statement.

    Returns
    -------
        dd_response : list[dict]
                    The output of a single DD, in the format expected for response on module completion.
                    Response can contain multiple outputs.
    """
    # create new DDStatement objects for each concat member
    # makes it easier to handle concat and non-concat DDs consistently
    dds = []
    for dd in dd_statement.definition:
        new_dd = DDStatement(dd_statement.name, dd)
        dds.append(new_dd)
    dd_response = gather_output(dds)
    return dd_response


def build_dd_response(dd_name, name, contents):
    """Gather additional response metrics and format
    as expected for response on module completion.

    Parameters
    ----------
        dd_name : str
                The DD name associated with this response.
        name : str
             The data set or UNIX file name associated with the response.
        contents : str
                 The raw contents taken from the data set or UNIX file.

    Returns
    -------
        dd_response : dict
                    Response content info of a single DD, in the format expected for response on module completion.
    """
    dd_response = {}
    dd_response["dd_name"] = dd_name
    dd_response["name"] = name
    dd_response["content"] = contents.split("\n")
    dd_response["record_count"] = len(dd_response.get("content", []))
    dd_response["byte_count"] = len(contents)  # len(contents.encode("utf-8"))
    return dd_response


def get_data_set_content(name, base64_encode=False, from_encoding=None, to_encoding=None):
    """Retrieve the raw contents of a data set.

    Parameters
    ----------
        name : str
             The name of the data set.
        base64_encode : bool, optional
               Determines if contents are retrieved as binary and base64 encoded. Defaults to False.
        from_encoding : str, optional
                      The encoding of the data set on the z/OS system. Defaults to None.
        to_encoding : str, optional
                    The encoding to receive the data back in. Defaults to None.

    Returns
    -------
        quoted_name : str
                    The raw content of the data set.
    """
    quoted_name = quote(name)
    if "'" not in quoted_name:
        quoted_name = "'{0}'".format(quoted_name)

    if base64_encode:
        with zoau_io.RecordIO("//{0}".format(quoted_name), "r") as records:
            content = base64.b64encode(b''.join(records.readrecords())).decode()
    else:
        content = get_content('"//{0}"'.format(quoted_name), from_encoding, to_encoding)
    return content


def get_unix_content(name, base64_encode=False, from_encoding=None, to_encoding=None):
    """Retrieve the raw contents of a UNIX file.

    Parameters
    ----------
        name : str
             The name of the UNIX file.
        base64_encode : bool, optional
               Determines if contents are retrieved as binary and base64 encoded. Defaults to False.
        from_encoding : str, optional
                      The encoding of the UNIX file on the z/OS system. Defaults to None.
        to_encoding : str, optional
                    The encoding to receive the data back in. Defaults to None.

    Returns
    -------
        stdout : str
               The raw content of the UNIX file.
    """
    if base64_encode:
        with open(name, "rb") as f:
            content = base64.b64encode(f.read()).decode()
    else:
        content = get_content("{0}".format(quote(name)), from_encoding, to_encoding)
    return content


def get_content(formatted_name, from_encoding=None, to_encoding=None):
    """Retrieve raw contents of a data set or UNIXfile.

    Parameters
    ----------
        name : str
             The name of the data set or UNIX file, formatted and quoted for proper usage in command.
        from_encoding : str, optional
                      The encoding of the data set or UNIX file on the z/OS system. Defaults to None.
        to_encoding : str, optional
                    The encoding to receive the data back in. Defaults to None.

    Returns
    -------
        stdout : str
               The raw content of the data set or UNIX file. If unsuccessful in retrieving data, returns empty string.
    """
    module = AnsibleModuleHelper(argument_spec={})
    conversion_command = " | iconv -f {0} -t {1}".format(
        quote(from_encoding), quote(to_encoding)
    )
    # * name argument should already be quoted by the time it reaches here
    # TODO: determine if response should be byte object
    rc, stdout, stderr = module.run_command(
        "cat {0}{1}".format(formatted_name, conversion_command),
        use_unsafe_shell=True,
        environ_update=ENCODING_ENVIRONMENT_VARS,
        errors='replace'
    )
    if rc:
        return ""
    else:
        return stdout


class ZOSRawError(Exception):
    def __init__(self, program="", error=""):
        self.msg = "An error occurred during execution of z/OS program {0}. {1}".format(
            program, error
        )
        super().__init__(self.msg)


def main():
    run_module()


if __name__ == "__main__":
    main()
