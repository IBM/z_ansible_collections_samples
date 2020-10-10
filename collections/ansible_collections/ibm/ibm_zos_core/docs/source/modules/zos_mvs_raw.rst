
:github_url: https://github.com/ansible-collections/ibm_zos_core/blob/dev/plugins/modules/zos_mvs_raw.py

.. _zos_mvs_raw_module:


zos_mvs_raw -- Run a z/OS program.
==================================



.. contents::
   :local:
   :depth: 1


Synopsis
--------
- Run a z/OS program.
- This is analogous to a job step in JCL.
- Defaults will be determined by underlying API if value not provided.





Parameters
----------


     
auth
  Determines whether this program should run with authorized privileges.

  If *auth=true*, the program runs as APF authorized.

  If *auth=false*, the program runs as unauthorized.


  | **required**: False
  | **type**: bool


     
dds
  The input data source.

  *dds* supports 6 types of sources

  1. *dd_data_set* for data set files.

  2. *dd_unix* for UNIX files.

  3. *dd_input* for in-stream data set.

  4. *dd_dummy* for no content input.

  5. *dd_concat* for a data set concatenation.

  6. *dds* supports any combination of source types.


  | **required**: False
  | **type**: list


     
  dd_concat
    *dd_concat* is used to specify a data set concatenation.


    | **required**: False
    | **type**: dict


     
    dd_name
      The DD name.


      | **required**: True
      | **type**: str


     
    dds
      A list of DD statements, which can contain any of the following types: *dd_data_set*, *dd_unix*, and *dd_input*.


      | **required**: False
      | **type**: list


     
      dd_data_set
        Specify a data set.

        *dd_data_set* can reference an existing data set or be used to define a new data set to be created during execution.


        | **required**: False
        | **type**: dict


     
        backup
          Determines if a backup should be made of existing data set when *disposition=NEW*, *replace=true*, and a data set with the desired name is found.

          *backup* is only used when *replace=true*.


          | **required**: False
          | **type**: bool


     
        block_size
          The maximum length of a block in bytes.

          Default is dependent on *record_format*


          | **required**: False
          | **type**: int


     
        data_set_name
          The data set name.


          | **required**: False
          | **type**: str


     
        directory_blocks
          The number of directory blocks to allocate to the data set.


          | **required**: False
          | **type**: int


     
        disposition
          *disposition* indicates the status of a data set.

          Defaults to shr.


          | **required**: False
          | **type**: str
          | **choices**: new, shr, mod, old


     
        disposition_abnormal
          *disposition_abnormal* indicates what to do with the data set after abnormal termination of the program.


          | **required**: False
          | **type**: str
          | **choices**: delete, keep, catlg, catalog, uncatlg, uncatalog


     
        disposition_normal
          *disposition_normal* indicates what to do with the data set after normal termination of the program.


          | **required**: False
          | **type**: str
          | **choices**: delete, keep, catlg, catalog, uncatlg, uncatalog


     
        encryption_key_1
          The encrypting key used by the Encryption Key Manager.

          Specification of the key labels does not by itself enable encryption. Encryption must be enabled by a data class that specifies an encryption format.


          | **required**: False
          | **type**: dict


     
          encoding
            How the label for the key encrypting key specified by *label* is encoded by the Encryption Key Manager.

            *encoding* can either be set to ``L`` for label encoding, or ``H`` for hash encoding.

            Maps to KEYCD1 on z/OS.


            | **required**: True
            | **type**: str
            | **choices**: l, h


     
          label
            The label for the key encrypting key used by the Encryption Key Manager.

            Key label must have a private key associated with it.

            *label* can be a maximum of 64 characters.

            Maps to KEYLAB1 on z/OS.


            | **required**: True
            | **type**: str



     
        encryption_key_2
          The encrypting key used by the Encryption Key Manager.

          Specification of the key labels does not by itself enable encryption. Encryption must be enabled by a data class that specifies an encryption format.


          | **required**: False
          | **type**: dict


     
          encoding
            How the label for the key encrypting key specified by *label* is encoded by the Encryption Key Manager.

            *encoding* can either be set to ``L`` for label encoding, or ``H`` for hash encoding.

            Maps to KEYCD2 on z/OS.


            | **required**: True
            | **type**: str
            | **choices**: l, h


     
          label
            The label for the key encrypting key used by the Encryption Key Manager.

            Key label must have a private key associated with it.

            *label* can be a maximum of 64 characters.

            Maps to KEYLAB2 on z/OS.


            | **required**: True
            | **type**: str



     
        key_label
          The label for the encryption key used by the system to encrypt the data set.

          *key_label* is the public name of a protected encryption key in the ICSF key repository.

          *key_label* should only be provided when creating an extended format data set.

          Maps to DSKEYLBL on z/OS.


          | **required**: False
          | **type**: str


     
        key_length
          The length of the keys used in a new data set.

          If using SMS, setting *key_length* overrides the key length defined in the SMS data class of the data set.

          Valid values are (0-255 non-vsam), (1-255 vsam).


          | **required**: False
          | **type**: int


     
        key_offset
          The position of the first byte of the record key in each logical record of a new VSAM data set.

          The first byte of a logical record is position 0.

          Provide *key_offset* only for VSAM key-sequenced data sets.


          | **required**: False
          | **type**: int


     
        record_format
          The format and characteristics of the records for new data set.


          | **required**: False
          | **type**: str
          | **choices**: u, vb, vba, fb, fba


     
        record_length
          The logical record length. (e.g ``80``).

          For variable data sets, the length must include the 4-byte prefix area.

          Defaults vary depending on format: If FB/FBA 80, if VB/VBA 137, if U 0.

          Valid values are (1-32760 for non-vsam,  1-32761 for vsam).

          Maps to LRECL on z/OS.


          | **required**: False
          | **type**: int


     
        replace
          Determines if data set should be replaced if *disposition=NEW* and a data set with matching name already exists.

          If *replace=true*, the original data set will be deleted, and a new data set created.

          If *replace=false*, and a data set with a matching name already exists, allocation will fail.

          Mutually exclusive with *reuse*.

          *replace* is only considered when *disposition=NEW*

          *replace* will result in loss of all data in the original data set unless *backup* is specified.


          | **required**: False
          | **type**: bool


     
        return_content
          Determines how content should be returned to the user.

          If not provided, no content from the DD is returned.


          | **required**: False
          | **type**: dict


     
          response_encoding
            The encoding to use when returning the contents of the data set.


            | **required**: False
            | **type**: str
            | **default**: iso8859-1


     
          src_encoding
            The encoding of the data set on the z/OS system.


            | **required**: False
            | **type**: str
            | **default**: ibm-1047


     
          type
            The type of the content to be returned.

            ``text`` means return content in encoding specified by *response_encoding*.

            *src_encoding* and *response_encoding* are only used when *type=text*.

            ``base64`` means return content in binary mode.


            | **required**: True
            | **type**: str
            | **choices**: text, base64



     
        reuse
          Determines if data set should be reused if *disposition=NEW* and a data set with matching name already exists.

          If *reuse=true*, *disposition* will be automatically switched to ``SHR``.

          If *reuse=false*, and a data set with a matching name already exists, allocation will fail.

          Mutually exclusive with *replace*.

          *reuse* is only considered when *disposition=NEW*


          | **required**: False
          | **type**: bool


     
        sms_data_class
          The desired data class for a new SMS-managed data set.

          *sms_data_class* is ignored if specified for an existing data set.

          All values must be between 1-8 alpha-numeric characters.


          | **required**: False
          | **type**: str


     
        sms_management_class
          The desired management class for a new SMS-managed data set.

          *sms_management_class* is ignored if specified for an existing data set.

          All values must be between 1-8 alpha-numeric characters.


          | **required**: False
          | **type**: str


     
        sms_storage_class
          The desired storage class for a new SMS-managed data set.

          *sms_storage_class* is ignored if specified for an existing data set.

          All values must be between 1-8 alpha-numeric characters.


          | **required**: False
          | **type**: str


     
        space_primary
          The primary amount of space to allocate for a new data set.

          The value provided to *space_type* is used as the unit of space for the allocation.

          Not applicable when *space_type=blklgth* or *space_type=reclgth*.


          | **required**: False
          | **type**: int


     
        space_secondary
          When primary allocation of space is filled, secondary space will be allocated with the provided size as needed.

          The value provided to *space_type* is used as the unit of space for the allocation.

          Not applicable when *space_type=blklgth* or *space_type=reclgth*.


          | **required**: False
          | **type**: int


     
        space_type
          The unit of measurement to use when allocating space for a new data set using *space_primary* and *space_secondary*.


          | **required**: False
          | **type**: str
          | **choices**: trk, cyl, b, k, m, g


     
        type
          The data set type. Only required when *disposition=new*.

          Maps to DSNTYPE on z/OS.


          | **required**: False
          | **type**: str
          | **choices**: library, pds, pdse, large, basic, seq, rrds, esds, lds, ksds


     
        volumes
          The volume or volumes on which a data set resides or will reside.

          Do not specify the same volume multiple times.


          | **required**: False
          | **type**: raw



     
      dd_input
        *dd_input* is used to specify an in-stream data set.

        Input will be saved to a temporary data set with a record length of 80.


        | **required**: False
        | **type**: dict


     
        content
          The input contents for the DD.

          *dd_input* supports single or multiple lines of input.

          Multi-line input can be provided as a multi-line string or a list of strings with 1 line per list item.

          If a list of strings is provided, newlines will be added to each of the lines when used as input.


          | **required**: False
          | **type**: raw


     
        return_content
          Determines how content should be returned to the user.

          If not provided, no content from the DD is returned.


          | **required**: False
          | **type**: dict


     
          response_encoding
            The encoding to use when returning the contents of the data set.


            | **required**: False
            | **type**: str
            | **default**: iso8859-1


     
          src_encoding
            The encoding of the data set on the z/OS system.

            for *dd_input*, *src_encoding* should generally not need to be changed.


            | **required**: False
            | **type**: str
            | **default**: ibm-1047


     
          type
            The type of the content to be returned.

            ``text`` means return content in encoding specified by *response_encoding*.

            *src_encoding* and *response_encoding* are only used when *type=text*.

            ``base64`` means return content in binary mode.


            | **required**: True
            | **type**: str
            | **choices**: text, base64




     
      dd_unix
        The path to a file in UNIX System Services (USS).


        | **required**: False
        | **type**: dict


     
        access_group
          The kind of access to request for the UNIX file specified in *path*.


          | **required**: False
          | **type**: str
          | **choices**: r, w, rw, read_only, write_only, read_write, ordonly, owronly, ordwr


     
        block_size
          The block size, in bytes, for the UNIX file.

          Default is dependent on *record_format*


          | **required**: False
          | **type**: int


     
        disposition_abnormal
          Indicates what to do with the UNIX file after abnormal termination of the program.


          | **required**: False
          | **type**: str
          | **choices**: keep, delete


     
        disposition_normal
          Indicates what to do with the UNIX file after normal termination of the program.


          | **required**: False
          | **type**: str
          | **choices**: keep, delete


     
        file_data_type
          The type of data that is (or will be) stored in the file specified in *path*.

          Maps to FILEDATA on z/OS.


          | **required**: False
          | **type**: str
          | **default**: binary
          | **choices**: binary, text, record


     
        mode
          The file access attributes when the UNIX file is created specified in *path*.

          Specify the mode as an octal number similar to chmod.

          Maps to PATHMODE on z/OS.


          | **required**: False
          | **type**: int


     
        path
          The path to an existing UNIX file.

          Or provide the path to an new created UNIX file when *status_group=OCREAT*.

          The provided path must be absolute.


          | **required**: True
          | **type**: str


     
        record_format
          The record format for the UNIX file.

          *record_format* is required in situations where the data will be processed as records and therefore, *record_length*, *block_size* and *record_format* need to be supplied since a UNIX file would normally be treated as a stream of bytes.


          | **required**: False
          | **type**: str
          | **choices**: u, vb, vba, fb, fba


     
        record_length
          The logical record length for the UNIX file.

          *record_length* is required in situations where the data will be processed as records and therefore, *record_length*, *block_size* and *record_format* need to be supplied since a UNIX file would normally be treated as a stream of bytes.

          Maps to LRECL on z/OS.


          | **required**: False
          | **type**: int


     
        return_content
          Determines how content should be returned to the user.

          If not provided, no content from the DD is returned.


          | **required**: False
          | **type**: dict


     
          response_encoding
            The encoding to use when returning the contents of the file.


            | **required**: False
            | **type**: str
            | **default**: iso8859-1


     
          src_encoding
            The encoding of the file on the z/OS system.


            | **required**: False
            | **type**: str
            | **default**: ibm-1047


     
          type
            The type of the content to be returned.

            ``text`` means return content in encoding specified by *response_encoding*.

            *src_encoding* and *response_encoding* are only used when *type=text*.

            ``base64`` means return content in binary mode.


            | **required**: True
            | **type**: str
            | **choices**: text, base64



     
        status_group
          The status for the UNIX file specified in *path*.

          If you do not specify a value for the *status_group* parameter the module assumes that the pathname exists, searches for it, and fails the module if the pathname does not exist.

          Maps to PATHOPTS status group file options on z/OS.

          You can specify up to 6 choices.

          *oappend* sets the file offset to the end of the file before each write, so that data is written at the end of the file.

          *ocreat* specifies that if the file does not exist, the system is to create it. If a directory specified in the pathname does not exist, one is not created, and the new file is not created. If the file already exists and *oexcl* was not specified, the system allows the program to use the existing file. If the file already exists and *oexcl* was specified, the system fails the allocation and the job step.

          *oexcl* specifies that if the file does not exist, the system is to create it. If the file already exists, the system fails the allocation and the job step. The system ignores *oexcl* if *ocreat* is not also specified.

          *onoctty* specifies that if the PATH parameter identifies a terminal device, opening of the file does not make the terminal device the controlling terminal for the process.

          *ononblock* specifies the following, depending on the type of file

          For a FIFO special file

          1. With *ononblock* specified and *ordonly* access, an open function for reading-only returns without delay.

          2. With *ononblock* not specified and *ordonly* access, an open function for reading-only blocks (waits) until a process opens the file for writing.

          3. With *ononblock* specified and *owronly* access, an open function for writing-only returns an error if no process currently has the file open for reading.

          4. With *ononblock* not specified and *owronly* access, an open function for writing-only blocks (waits) until a process opens the file for reading.

          5. For a character special file that supports nonblocking open

          6. If *ononblock* is specified, an open function returns without blocking (waiting) until the device is ready or available. Device response depends on the type of device.

          7. If *ononblock* is not specified, an open function blocks (waits) until the device is ready or available.

          *ononblock* has no effect on other file types.

          *osync* specifies that the system is to move data from buffer storage to permanent storage before returning control from a callable service that performs a write.

          *otrunc* specifies that the system is to truncate the file length to zero if all the following are true: the file specified exists, the file is a regular file, and the file successfully opened with *ordwr* or *owronly*.

          When *otrunc* is specified, the system does not change the mode and owner. *otrunc* has no effect on FIFO special files or character special files.


          | **required**: False
          | **type**: list
          | **choices**: oappend, ocreat, oexcl, onoctty, ononblock, osync, otrunc





     
  dd_data_set
    Specify a data set.

    *dd_data_set* can reference an existing data set or be used to define a new data set to be created during execution.


    | **required**: False
    | **type**: dict


     
    backup
      Determines if a backup should be made of an existing data set when *disposition=NEW*, *replace=true*, and a data set with the desired name is found.

      *backup* is only used when *replace=true*.


      | **required**: False
      | **type**: bool


     
    block_size
      The maximum length of a block in bytes.

      Default is dependent on *record_format*


      | **required**: False
      | **type**: int


     
    data_set_name
      The data set name.


      | **required**: False
      | **type**: str


     
    dd_name
      The DD name.


      | **required**: True
      | **type**: str


     
    directory_blocks
      The number of directory blocks to allocate to the data set.


      | **required**: False
      | **type**: int


     
    disposition
      *disposition* indicates the status of a data set.

      Defaults to shr.


      | **required**: False
      | **type**: str
      | **choices**: new, shr, mod, old


     
    disposition_abnormal
      *disposition_abnormal* indicates what to do with the data set after an abnormal termination of the program.


      | **required**: False
      | **type**: str
      | **choices**: delete, keep, catlg, catalog, uncatlg, uncatalog


     
    disposition_normal
      *disposition_normal* indicates what to do with the data set after a normal termination of the program.


      | **required**: False
      | **type**: str
      | **choices**: delete, keep, catlg, catalog, uncatlg, uncatalog


     
    encryption_key_1
      The encrypting key used by the Encryption Key Manager.

      Specification of the key labels does not by itself enable encryption. Encryption must be enabled by a data class that specifies an encryption format.


      | **required**: False
      | **type**: dict


     
      encoding
        How the label for the key encrypting key specified by *label* is encoded by the Encryption Key Manager.

        *encoding* can either be set to ``L`` for label encoding, or ``H`` for hash encoding.

        Maps to KEYCD1 on z/OS.


        | **required**: True
        | **type**: str
        | **choices**: l, h


     
      label
        The label for the key encrypting key used by the Encryption Key Manager.

        Key label must have a private key associated with it.

        *label* can be a maximum of 64 characters.

        Maps to KEYLAB1 on z/OS.


        | **required**: True
        | **type**: str



     
    encryption_key_2
      The encrypting key used by the Encryption Key Manager.

      Specification of the key labels does not by itself enable encryption. Encryption must be enabled by a data class that specifies an encryption format.


      | **required**: False
      | **type**: dict


     
      encoding
        How the label for the key encrypting key specified by *label* is encoded by the Encryption Key Manager.

        *encoding* can either be set to ``L`` for label encoding, or ``H`` for hash encoding.

        Maps to KEYCD2 on z/OS.


        | **required**: True
        | **type**: str
        | **choices**: l, h


     
      label
        The label for the key encrypting key used by the Encryption Key Manager.

        Key label must have a private key associated with it.

        *label* can be a maximum of 64 characters.

        Maps to KEYLAB2 on z/OS.


        | **required**: True
        | **type**: str



     
    key_label
      The label for the encryption key used by the system to encrypt the data set.

      *key_label* is the public name of a protected encryption key in the ICSF key repository.

      *key_label* should only be provided when creating an extended format data set.

      Maps to DSKEYLBL on z/OS.


      | **required**: False
      | **type**: str


     
    key_length
      The length of the keys used in a new data set.

      If using SMS, setting *key_length* overrides the key length defined in the SMS data class of the data set.

      Valid values are (0-255 non-vsam), (1-255 vsam).


      | **required**: False
      | **type**: int


     
    key_offset
      The position of the first byte of the record key in each logical record of a new VSAM data set.

      The first byte of a logical record is position 0.

      Provide *key_offset* only for VSAM key-sequenced data sets.


      | **required**: False
      | **type**: int


     
    record_format
      The format and characteristics of the records for new data set.


      | **required**: False
      | **type**: str
      | **choices**: u, vb, vba, fb, fba


     
    record_length
      The logical record length. (e.g ``80``).

      For variable data sets, the length must include the 4-byte prefix area.

      Defaults vary depending on format: If FB/FBA 80, if VB/VBA 137, if U 0.

      Valid values are (1-32760 for non-vsam,  1-32761 for vsam).

      Maps to LRECL on z/OS.


      | **required**: False
      | **type**: int


     
    replace
      Determines if a data set should be replaced if *disposition=NEW* and a data set with a matching name already exists.

      If *replace=true*, the original data set will be deleted, and a new data set created.

      If *replace=false*, and a data set with a matching name already exists, allocation will fail.

      Mutually exclusive with *reuse*.

      *replace* is only considered when *disposition=NEW*

      *replace* will result in loss of all data in the original data set unless *backup* is specified.


      | **required**: False
      | **type**: bool


     
    return_content
      Determines how content should be returned to the user.

      If not provided, no content from the DD is returned.


      | **required**: False
      | **type**: dict


     
      response_encoding
        The encoding to use when returning the contents of the data set.


        | **required**: False
        | **type**: str
        | **default**: iso8859-1


     
      src_encoding
        The encoding of the data set on the z/OS system.


        | **required**: False
        | **type**: str
        | **default**: ibm-1047


     
      type
        The type of the content to be returned.

        ``text`` means return content in encoding specified by *response_encoding*.

        *src_encoding* and *response_encoding* are only used when *type=text*.

        ``base64`` means return content in binary mode.


        | **required**: True
        | **type**: str
        | **choices**: text, base64



     
    reuse
      Determines if a data set should be reused if *disposition=NEW* and if a data set with a matching name already exists.

      If *reuse=true*, *disposition* will be automatically switched to ``SHR``.

      If *reuse=false*, and a data set with a matching name already exists, allocation will fail.

      Mutually exclusive with *replace*.

      *reuse* is only considered when *disposition=NEW*


      | **required**: False
      | **type**: bool


     
    sms_data_class
      The desired data class for a new SMS-managed data set.

      *sms_data_class* is ignored if specified for an existing data set.

      All values must be between 1-8 alpha-numeric characters.


      | **required**: False
      | **type**: str


     
    sms_management_class
      The desired management class for a new SMS-managed data set.

      *sms_management_class* is ignored if specified for an existing data set.

      All values must be between 1-8 alpha-numeric characters.


      | **required**: False
      | **type**: str


     
    sms_storage_class
      The desired storage class for a new SMS-managed data set.

      *sms_storage_class* is ignored if specified for an existing data set.

      All values must be between 1-8 alpha-numeric characters.


      | **required**: False
      | **type**: str


     
    space_primary
      The primary amount of space to allocate for a new data set.

      The value provided to *space_type* is used as the unit of space for the allocation.

      Not applicable when *space_type=blklgth* or *space_type=reclgth*.


      | **required**: False
      | **type**: int


     
    space_secondary
      When primary allocation of space is filled, secondary space will be allocated with the provided size as needed.

      The value provided to *space_type* is used as the unit of space for the allocation.

      Not applicable when *space_type=blklgth* or *space_type=reclgth*.


      | **required**: False
      | **type**: int


     
    space_type
      The unit of measurement to use when allocating space for a new data set using *space_primary* and *space_secondary*.


      | **required**: False
      | **type**: str
      | **choices**: trk, cyl, b, k, m, g


     
    type
      The data set type. Only required when *disposition=new*.

      Maps to DSNTYPE on z/OS.


      | **required**: False
      | **type**: str
      | **choices**: library, pds, pdse, large, basic, seq, rrds, esds, lds, ksds


     
    volumes
      The volume or volumes on which a data set resides or will reside.

      Do not specify the same volume multiple times.


      | **required**: False
      | **type**: raw



     
  dd_dummy
    Use *dd_dummy* to specify - No device or external storage space is to be allocated to the data set. - No disposition processing is to be performed on the data set.

    *dd_dummy* accepts no content input.


    | **required**: False
    | **type**: dict


     
    dd_name
      The DD name.


      | **required**: True
      | **type**: str



     
  dd_input
    *dd_input* is used to specify an in-stream data set.

    Input will be saved to a temporary data set with a record length of 80.


    | **required**: False
    | **type**: dict


     
    content
      The input contents for the DD.

      *dd_input* supports single or multiple lines of input.

      Multi-line input can be provided as a multi-line string or a list of strings with 1 line per list item.

      If a list of strings is provided, newlines will be added to each of the lines when used as input.


      | **required**: False
      | **type**: raw


     
    dd_name
      The DD name.


      | **required**: True
      | **type**: str


     
    return_content
      Determines how content should be returned to the user.

      If not provided, no content from the DD is returned.


      | **required**: False
      | **type**: dict


     
      response_encoding
        The encoding to use when returning the contents of the data set.


        | **required**: False
        | **type**: str
        | **default**: iso8859-1


     
      src_encoding
        The encoding of the data set on the z/OS system.

        for *dd_input*, *src_encoding* should generally not need to be changed.


        | **required**: False
        | **type**: str
        | **default**: ibm-1047


     
      type
        The type of the content to be returned.

        ``text`` means return content in encoding specified by *response_encoding*.

        *src_encoding* and *response_encoding* are only used when *type=text*.

        ``base64`` means return content in binary mode.


        | **required**: True
        | **type**: str
        | **choices**: text, base64




     
  dd_output
    Use *dd_output* to specify - Content sent to the DD should be returned to the user.


    | **required**: False
    | **type**: dict


     
    dd_name
      The DD name.


      | **required**: True
      | **type**: str


     
    return_content
      Determines how content should be returned to the user.

      If not provided, no content from the DD is returned.


      | **required**: True
      | **type**: dict


     
      response_encoding
        The encoding to use when returning the contents of the data set.


        | **required**: False
        | **type**: str
        | **default**: iso8859-1


     
      src_encoding
        The encoding of the data set on the z/OS system.

        for *dd_input*, *src_encoding* should generally not need to be changed.


        | **required**: False
        | **type**: str
        | **default**: ibm-1047


     
      type
        The type of the content to be returned.

        ``text`` means return content in encoding specified by *response_encoding*.

        *src_encoding* and *response_encoding* are only used when *type=text*.

        ``base64`` means return content in binary mode.


        | **required**: True
        | **type**: str
        | **choices**: text, base64




     
  dd_unix
    The path to a file in UNIX System Services (USS).


    | **required**: False
    | **type**: dict


     
    access_group
      The kind of access to request for the UNIX file specified in *path*.


      | **required**: False
      | **type**: str
      | **choices**: r, w, rw, read_only, write_only, read_write, ordonly, owronly, ordwr


     
    block_size
      The block size, in bytes, for the UNIX file.

      Default is dependent on *record_format*


      | **required**: False
      | **type**: int


     
    dd_name
      The DD name.


      | **required**: True
      | **type**: str


     
    disposition_abnormal
      Indicates what to do with the UNIX file after abnormal termination of the program.


      | **required**: False
      | **type**: str
      | **choices**: keep, delete


     
    disposition_normal
      Indicates what to do with the UNIX file after normal termination of the program.


      | **required**: False
      | **type**: str
      | **choices**: keep, delete


     
    file_data_type
      The type of data that is (or will be) stored in the file specified in *path*.

      Maps to FILEDATA on z/OS.


      | **required**: False
      | **type**: str
      | **default**: binary
      | **choices**: binary, text, record


     
    mode
      The file access attributes when the UNIX file is created specified in *path*.

      Specify the mode as an octal number similarly to chmod.

      Maps to PATHMODE on z/OS.


      | **required**: False
      | **type**: int


     
    path
      The path to an existing UNIX file.

      Or provide the path to an new created UNIX file when *status_group=OCREAT*.

      The provided path must be absolute.


      | **required**: True
      | **type**: str


     
    record_format
      The record format for the UNIX file.

      *record_format* is required in situations where the data will be processed as records and therefore, *record_length*, *block_size* and *record_format* need to be supplied since a UNIX file would normally be treated as a stream of bytes.


      | **required**: False
      | **type**: str
      | **choices**: u, vb, vba, fb, fba


     
    record_length
      The logical record length for the UNIX file.

      *record_length* is required in situations where the data will be processed as records and therefore, *record_length*, *block_size* and *record_format* need to be supplied since a UNIX file would normally be treated as a stream of bytes.

      Maps to LRECL on z/OS.


      | **required**: False
      | **type**: int


     
    return_content
      Determines how content should be returned to the user.

      If not provided, no content from the DD is returned.


      | **required**: False
      | **type**: dict


     
      response_encoding
        The encoding to use when returning the contents of the file.


        | **required**: False
        | **type**: str
        | **default**: iso8859-1


     
      src_encoding
        The encoding of the file on the z/OS system.


        | **required**: False
        | **type**: str
        | **default**: ibm-1047


     
      type
        The type of the content to be returned.

        ``text`` means return content in encoding specified by *response_encoding*.

        *src_encoding* and *response_encoding* are only used when *type=text*.

        ``base64`` means return content in binary mode.


        | **required**: True
        | **type**: str
        | **choices**: text, base64



     
    status_group
      The status for the UNIX file specified in *path*.

      If you do not specify a value for the *status_group* parameter, the module assumes that the pathname exists, searches for it, and fails the module if the pathname does not exist.

      Maps to PATHOPTS status group file options on z/OS.

      You can specify up to 6 choices.

      *oappend* sets the file offset to the end of the file before each write, so that data is written at the end of the file.

      *ocreat* specifies that if the file does not exist, the system is to create it. If a directory specified in the pathname does not exist, a new directory and a new file are not created. If the file already exists and *oexcl* was not specified, the system allows the program to use the existing file. If the file already exists and *oexcl* was specified, the system fails the allocation and the job step.

      *oexcl* specifies that if the file does not exist, the system is to create it. If the file already exists, the system fails the allocation and the job step. The system ignores *oexcl* if *ocreat* is not also specified.

      *onoctty* specifies that if the PATH parameter identifies a terminal device, opening of the file does not make the terminal device the controlling terminal for the process.

      *ononblock* specifies the following, depending on the type of file

      For a FIFO special file

      1. With *ononblock* specified and *ordonly* access, an open function for reading-only returns without delay.

      2. With *ononblock* not specified and *ordonly* access, an open function for reading-only blocks (waits) until a process opens the file for writing.

      3. With *ononblock* specified and *owronly* access, an open function for writing-only returns an error if no process currently has the file open for reading.

      4. With *ononblock* not specified and *owronly* access, an open function for writing-only blocks (waits) until a process opens the file for reading.

      5. For a character special file that supports nonblocking open

      6. If *ononblock* is specified, an open function returns without blocking (waiting) until the device is ready or available. Device response depends on the type of device.

      7. If *ononblock* is not specified, an open function blocks (waits) until the device is ready or available.

      *ononblock* has no effect on other file types.

      *osync* specifies that the system is to move data from buffer storage to permanent storage before returning control from a callable service that performs a write.

      *otrunc* specifies that the system is to truncate the file length to zero if all the following are true: the file specified exists, the file is a regular file, and the file successfully opened with *ordwr* or *owronly*.

      When *otrunc* is specified, the system does not change the mode and owner. *otrunc* has no effect on FIFO special files or character special files.


      | **required**: False
      | **type**: list
      | **choices**: oappend, ocreat, oexcl, onoctty, ononblock, osync, otrunc



     
  dd_vio
    *dd_vio* is used to handle temporary data sets.

    VIO data sets reside in the paging space; but, to the problem program and the access method, the data sets appear to reside on a direct access storage device.

    You cannot use VIO for permanent data sets, VSAM data sets, or partitioned data sets extended (PDSEs).


    | **required**: False
    | **type**: dict


     
    dd_name
      The DD name.


      | **required**: True
      | **type**: str




     
parm
  The program arguments (e.g. -a='MARGINS(1,72)').


  | **required**: False
  | **type**: str


     
program_name
  The name of the z/OS program to run (e.g. IDCAMS, IEFBR14, IEBGENER etc.).


  | **required**: True
  | **type**: str


     
verbose
  Determines if verbose output should be returned from the underlying utility used by this module.

  When *verbose=true* verbose output is returned on module failure.


  | **required**: False
  | **type**: bool




Examples
--------

.. code-block:: yaml+jinja

   
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
             reuse: yes
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
             reuse: yes
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
             reuse: yes
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
             replace: yes
             backup: yes
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
       auth: yes
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
       auth: no
       parm: "MSGPRT=CRITICAL,LIST"
       dds:
         - dd_data_set:
             dd_name: sortin01
             data_set_name: myhlq.dfsort.master
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
       auth: yes
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




Notes
-----

.. note::
   When executing programs using :ref:`zos_mvs_raw <zos_mvs_raw_module>`, you may encounter errors that originate in the programs implementation. Two such known issues are noted below of which one has been addressed with an APAR.

   1. :ref:`zos_mvs_raw <zos_mvs_raw_module>` module execution fails when invoking Database Image Copy 2 Utility or Database Recovery Utility in conjunction with FlashCopy or Fast Replication.

   2. :ref:`zos_mvs_raw <zos_mvs_raw_module>` module execution fails when invoking DFSRRC00 with parm "UPB,PRECOMP", "UPB, POSTCOMP" or "UPB,PRECOMP,POSTCOMP". This issue is addressed by APAR PH28089.






Return Values
-------------


   
                              
       ret_code
        | The return code.
      
        | **returned**: always
        | **type**: dict
              
   
                              
        code
          | The return code number returned from the program.
      
          | **type**: int
      
        
      
      
                              
       dd_names
        | All the related dds with the program.
      
        | **returned**: on success
        | **type**: list
              
   
                              
        dd_name
          | The data definition name.
      
          | **type**: str
      
      
                              
        name
          | The data set or path name associated with the data definition.
      
          | **type**: str
      
      
                              
        content
          | The content contained in the data definition.
      
          | **type**: list
      
      
                              
        record_count
          | The lines of the content.
      
          | **type**: int
      
      
                              
        byte_count
          | The number of bytes in the response content.
      
          | **type**: int
      
        
      
      
                              
       backups
        | List of any data set backups made during execution.
      
        | **returned**: always
        | **type**: dict
              
   
                              
        original_name
          | The original data set name for which a backup was made.
      
          | **type**: str
      
      
                              
        backup_name
          | The name of the data set containing the backup of content from data set in original_name.
      
          | **type**: str
      
        
      
        
