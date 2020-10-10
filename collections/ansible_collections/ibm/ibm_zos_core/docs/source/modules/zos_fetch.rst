
:github_url: https://github.com/ansible-collections/ibm_zos_core/blob/dev/plugins/modules/zos_fetch.py

.. _zos_fetch_module:


zos_fetch -- Fetch data from z/OS
=================================



.. contents::
   :local:
   :depth: 1


Synopsis
--------
- This module fetches a UNIX System Services (USS) file, PS(sequential data set), PDS, PDSE, member of a PDS or PDSE, or KSDS(VSAM data set) from a remote z/OS system.
- When fetching a sequential data set, the destination file name will be the same as the data set name.
- When fetching a PDS or PDSE, the destination will be a directory with the same name as the PDS or PDSE.
- When fetching a PDS/PDSE member, destination will be a file.
- Files that already exist at ``dest`` will be overwritten if they are different than ``src``.





Parameters
----------


     
dest
  Local path where the file or data set will be stored.

  If dest is an existing file or directory, the contents will be overwritten.


  | **required**: True
  | **type**: path


     
encoding
  Specifies which encodings the fetched data set should be converted from and to. If this parameter is not provided, encoding conversions will not take place.


  | **required**: False
  | **type**: dict


     
  from
    The character set of the source *src*.

    Supported character sets rely on the charset conversion utility (iconv) version; the most common character sets are supported.


    | **required**: True
    | **type**: str


     
  to
    The destination *dest* character set for the output to be written as.

    Supported character sets rely on the charset conversion utility (iconv) version; the most common character sets are supported.


    | **required**: True
    | **type**: str



     
fail_on_missing
  When set to true, the task will fail if the source file is missing.


  | **required**: False
  | **type**: bool
  | **default**: true


     
flat
  Override the default behavior of appending hostname/path/to/file to the destination. If set to "true", the file or data set will be fetched to the destination directory without appending remote hostname to the destination.


  | **required**: False
  | **type**: bool
  | **default**: true


     
ignore_sftp_stderr
  During data transfer through sftp, the module fails if the sftp command directs any content to stderr. The user is able to override this behavior by setting this parameter to ``true``. By doing so, the module would essentially ignore the stderr stream produced by sftp and continue execution.


  | **required**: False
  | **type**: bool


     
is_binary
  Specifies if the file being fetched is a binary.


  | **required**: False
  | **type**: bool
  | **default**: false


     
sftp_port
  Indicates which port should be used to connect to the remote z/OS system to perform data transfer.

  If this parameter is not specified, ``ansible_port`` will be used.

  If ``ansible_port`` is not specified, port 22 will be used.


  | **required**: False
  | **type**: int


     
src
  Name of a UNIX System Services (USS) file, PS(sequential data set), PDS, PDSE, member of a PDS, PDSE or KSDS(VSAM data set).

  USS file paths should be absolute paths.


  | **required**: True
  | **type**: str


     
use_qualifier
  Indicates whether the data set high level qualifier should be used when fetching.


  | **required**: False
  | **type**: bool
  | **default**: false


     
validate_checksum
  Verify that the source and destination checksums match after the files are fetched.


  | **required**: False
  | **type**: bool
  | **default**: true




Examples
--------

.. code-block:: yaml+jinja

   
   - name: Fetch file from USS and store in /tmp/fetched/hostname/tmp/somefile
     zos_fetch:
       src: /tmp/somefile
       dest: /tmp/fetched

   - name: Fetch a sequential data set and store in /tmp/SOME.DATA.SET
     zos_fetch:
       src: SOME.DATA.SET
       dest: /tmp/
       flat: true

   - name: Fetch a PDS as binary and store in /tmp/SOME.PDS.DATASET
     zos_fetch:
       src: SOME.PDS.DATASET
       dest: /tmp/
       flat: true
       is_binary: true

   - name: Fetch a UNIX file and don't validate its checksum
     zos_fetch:
       src: /tmp/somefile
       dest: /tmp/
       flat: true
       validate_checksum: false

   - name: Fetch a VSAM data set
     zos_fetch:
       src: USER.TEST.VSAM
       dest: /tmp/
       flat: true

   - name: Fetch a PDS member named 'DATA'
     zos_fetch:
       src: USER.TEST.PDS(DATA)
       dest: /tmp/
       flat: true

   - name: Fetch a USS file and convert from IBM-037 to ISO8859-1
     zos_fetch:
       src: /etc/profile
       dest: /tmp/
       encoding:
         from: IBM-037
         to: ISO8859-1
       flat: true




Notes
-----

.. note::
   When fetching PDSE and VSAM data sets, temporary storage will be used on the remote z/OS system. After the PDSE or VSAM data set is successfully transferred, the temporary storage will be deleted. The size of the temporary storage will correspond to the size of PDSE or VSAM data set being fetched. If module execution fails, the temporary storage will be deleted.

   To ensure optimal performance, data integrity checks for PDS, PDSE, and members of PDS or PDSE are done through the transfer methods used. As a result, the module response will not include the ``checksum`` parameter.

   All data sets are always assumed to be cataloged. If an uncataloged data set needs to be fetched, it should be cataloged first.

   Fetching HFS or ZFS type data sets is currently not supported.

   For supported character sets used to encode data, refer to https://ansible-collections.github.io/ibm_zos_core/supplementary.html#encode



See Also
--------

.. seealso::

   - :ref:`zos_data_set_module`
   - :ref:`zos_copy_module`



Return Values
-------------


   
                              
       file
        | The source file path or data set on the remote machine.
      
        | **returned**: success
        | **type**: str
        | **sample**: SOME.DATA.SET

            
      
      
                              
       dest
        | The destination file path on the controlling machine.
      
        | **returned**: success
        | **type**: str
        | **sample**: /tmp/SOME.DATA.SET

            
      
      
                              
       is_binary
        | Indicates the transfer mode that was used to fetch.
      
        | **returned**: success
        | **type**: bool      
        | **sample**:

              .. code-block::

                       true
            
      
      
                              
       checksum
        | The SHA256 checksum of the fetched file or data set. checksum validation is performed for all USS files and sequential data sets.
      
        | **returned**: success and src is a non-partitioned data set
        | **type**: str
        | **sample**: 8d320d5f68b048fc97559d771ede68b37a71e8374d1d678d96dcfa2b2da7a64e

            
      
      
                              
       data_set_type
        | Indicates the fetched data set type.
      
        | **returned**: success
        | **type**: str
        | **sample**: PDSE

            
      
      
                              
       note
        | Notice of module failure when C(fail_on_missing) is false.
      
        | **returned**: failure and fail_on_missing=false
        | **type**: str
        | **sample**: The data set USER.PROCLIB does not exist. No data was fetched.

            
      
      
                              
       msg
        | Message returned on failure.
      
        | **returned**: failure
        | **type**: str
        | **sample**: The source 'TEST.DATA.SET' does not exist or is uncataloged.

            
      
      
                              
       stdout
        | The stdout from a USS command or MVS command, if applicable.
      
        | **returned**: failure
        | **type**: str
        | **sample**: DATA SET 'USER.PROCLIB' NOT IN CATALOG

            
      
      
                              
       stderr
        | The stderr of a USS command or MVS command, if applicable
      
        | **returned**: failure
        | **type**: str
        | **sample**: File /tmp/result.log not found.

            
      
      
                              
       stdout_lines
        | List of strings containing individual lines from stdout
      
        | **returned**: failure
        | **type**: list      
        | **sample**:

              .. code-block::

                       ["u\u0027USER.TEST.PDS NOT IN CATALOG..\u0027"]
            
      
      
                              
       stderr_lines
        | List of strings containing individual lines from stderr.
      
        | **returned**: failure
        | **type**: list      
        | **sample**:

              .. code-block::

                       ["u\u0027Unable to traverse PDS USER.TEST.PDS not found\u0027"]
            
      
      
                              
       rc
        | The return code of a USS command or MVS command, if applicable.
      
        | **returned**: failure
        | **type**: int
        | **sample**: 8

            
      
        
