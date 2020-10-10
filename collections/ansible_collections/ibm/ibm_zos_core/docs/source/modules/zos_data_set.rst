
:github_url: https://github.com/ansible-collections/ibm_zos_core/blob/dev/plugins/modules/zos_data_set.py

.. _zos_data_set_module:


zos_data_set -- Manage data sets
================================



.. contents::
   :local:
   :depth: 1


Synopsis
--------
- Create, delete and set attributes of data sets.
- When forcing data set replacement, contents will not be preserved.





Parameters
----------


     
batch
  Batch can be used to perform operations on multiple data sets in a single module call.


  | **required**: False
  | **type**: list


     
  block_size
    The block size to use for the data set.


    | **required**: False
    | **type**: int


     
  directory_blocks
    The number of directory blocks to allocate to the data set.


    | **required**: False
    | **type**: int


     
  key_length
    The key length to use when creating a KSDS data set.

    *key_length* is required when *type=KSDS*.

    *key_length* should only be provided when *type=KSDS*


    | **required**: False
    | **type**: int


     
  key_offset
    The key offset to use when creating a KSDS data set.

    *key_offset* is required when *type=KSDS*.

    *key_offset* should only be provided when *type=KSDS*


    | **required**: False
    | **type**: int


     
  name
    The name of the data set being managed. (e.g ``USER.TEST``)

    If *name* is not provided, a randomized data set name will be generated with the HLQ matching the module-runners username.

    Required if *type=MEMBER* or *state!=present*


    | **required**: False
    | **type**: str


     
  record_format
    The format of the data set. (e.g ``FB``)

    Choices are case-insensitive.


    | **required**: False
    | **type**: str
    | **default**: FB
    | **choices**: FB, VB, FBA, VBA, U


     
  record_length
    The length, in bytes, of each record in the data set.

    For variable data sets, the length must include the 4-byte prefix area.

    Defaults vary depending on format: If FB/FBA 80, if VB/VBA 137, if U 0.


    | **required**: False
    | **type**: int


     
  replace
    When *replace=True*, and *state=present*, existing data set matching *name* will be replaced.

    Replacement is performed by deleting the existing data set and creating a new data set with the same name and desired attributes. Since the existing data set will be deleted prior to creating the new data set, no data set will exist if creation of the new data set fails.


    If *replace=True*, all data in the original data set will be lost.


    | **required**: False
    | **type**: bool


     
  sms_data_class
    The data class for an SMS-managed dataset.

    Optional for SMS-managed datasets that do not match an SMS-rule.

    Not valid for datasets that are not SMS-managed.

    Note that all non-linear VSAM datasets are SMS-managed.


    | **required**: False
    | **type**: str


     
  sms_management_class
    The management class for an SMS-managed dataset.

    Optional for SMS-managed datasets that do not match an SMS-rule.

    Not valid for datasets that are not SMS-managed.

    Note that all non-linear VSAM datasets are SMS-managed.


    | **required**: False
    | **type**: str


     
  sms_storage_class
    The storage class for an SMS-managed dataset.

    Required for SMS-managed datasets that do not match an SMS-rule.

    Not valid for datasets that are not SMS-managed.

    Note that all non-linear VSAM datasets are SMS-managed.


    | **required**: False
    | **type**: str


     
  space_primary
    The amount of primary space to allocate for the dataset.

    The unit of space used is set using *space_type*.


    | **required**: False
    | **type**: int
    | **default**: 5


     
  space_secondary
    The amount of secondary space to allocate for the dataset.

    The unit of space used is set using *space_type*.


    | **required**: False
    | **type**: int
    | **default**: 3


     
  space_type
    The unit of measurement to use when defining primary and secondary space.

    Valid units of size are ``K``, ``M``, ``G``, ``CYL``, and ``TRK``.


    | **required**: False
    | **type**: str
    | **default**: M
    | **choices**: K, M, G, CYL, TRK


     
  state
    The final state desired for specified data set.

    If *state=absent* and the data set does not exist on the managed node, no action taken, module completes successfully with *changed=False*.


    If *state=absent* and the data set does exist on the managed node, remove the data set, module completes successfully with *changed=True*.


    If *state=absent* and *volumes* is provided, and the data set is not found in the catalog, the module attempts to perform catalog using supplied *name* and *volumes*. If the attempt to catalog the data set catalog is successful, then the data set is removed. Module completes successfully with *changed=True*.


    If *state=absent* and *volumes* is provided, and the data set is not found in the catalog, the module attempts to perform catalog using supplied *name* and *volumes*. If the attempt to catalog the data set catalog fails, then no action is taken. Module completes successfully with *changed=False*.


    If *state=present* and the data set does not exist on the managed node, create and catalog the data set, module completes successfully with *changed=True*.


    If *state=present* and *replace=True* and the data set is present on the managed node the existing data set is deleted, and a new data set is created and cataloged with the desired attributes, module completes successfully with *changed=True*.


    If *state=present* and *replace=False* and the data set is present on the managed node, no action taken, module completes successfully with *changed=False*.


    If *state=cataloged* and *volumes* is provided and the data set is already cataloged, no action taken, module completes successfully with *changed=False*.


    If *state=cataloged* and *volumes* is provided and the data set is not cataloged, module attempts to perform catalog using supplied *name* and *volumes*. If the attempt to catalog the data set catalog is successful, module completes successfully with *changed=True*.


    If *state=cataloged* and *volumes* is provided and the data set is not cataloged, module attempts to perform catalog using supplied *name* and *volumes*. If the attempt to catalog the data set catalog fails, returns failure with *changed=False*.


    If *state=uncataloged* and the data set is not found, no action taken , module completes successfully with *changed=False*.


    If *state=uncataloged* and the data set is found, the data set is uncataloged, module completes successfully with *changed=True*.



    | **required**: False
    | **type**: str
    | **default**: present
    | **choices**: present, absent, cataloged, uncataloged


     
  type
    The data set type to be used when creating a data set. (e.g ``pdse``)

    ``MEMBER`` expects to be used with an existing partitioned data set.

    Choices are case-insensitive.


    | **required**: False
    | **type**: str
    | **choices**: KSDS, ESDS, RRDS, LDS, SEQ, PDS, PDSE, LIBRARY, BASIC, LARGE, MEMBER


     
  volumes
    If cataloging a data set, *volumes* specifies the name of the volume(s) where the data set is located.


    If creating a data set, *volumes* specifies the volume(s) where the data set should be created.


    If *volumes* is provided when *state=present*, and the data set is not found in the catalog, :ref:`zos_data_set <zos_data_set_module>` will check the volume table of contents to see if the data set exists. If the data set does exist, it will be cataloged.


    If *volumes* is provided when *state=absent* and the data set is not found in the catalog, :ref:`zos_data_set <zos_data_set_module>` will check the volume table of contents to see if the data set exists. If the data set does exist, it will be cataloged and promptly removed from the system.


    *volumes* is required when *state=cataloged*.

    Accepts a string when using a single volume and a list of strings when using multiple.


    | **required**: False
    | **type**: raw



     
block_size
  The block size to use for the data set.


  | **required**: False
  | **type**: int


     
directory_blocks
  The number of directory blocks to allocate to the data set.


  | **required**: False
  | **type**: int


     
key_length
  The key length to use when creating a KSDS data set.

  *key_length* is required when *type=KSDS*.

  *key_length* should only be provided when *type=KSDS*


  | **required**: False
  | **type**: int


     
key_offset
  The key offset to use when creating a KSDS data set.

  *key_offset* is required when *type=KSDS*.

  *key_offset* should only be provided when *type=KSDS*


  | **required**: False
  | **type**: int


     
name
  The name of the data set being managed. (e.g ``USER.TEST``)

  If *name* is not provided, a randomized data set name will be generated with the HLQ matching the module-runners username.

  Required if *type=MEMBER* or *state!=present* and not using *batch*.


  | **required**: False
  | **type**: str


     
record_format
  The format of the data set. (e.g ``FB``)

  Choices are case-insensitive.


  | **required**: False
  | **type**: str
  | **default**: FB
  | **choices**: FB, VB, FBA, VBA, U


     
record_length
  The length, in bytes, of each record in the data set.

  For variable data sets, the length must include the 4-byte prefix area.

  Defaults vary depending on format: If FB/FBA 80, if VB/VBA 137, if U 0.


  | **required**: False
  | **type**: int


     
replace
  When *replace=True*, and *state=present*, existing data set matching *name* will be replaced.

  Replacement is performed by deleting the existing data set and creating a new data set with the same name and desired attributes. Since the existing data set will be deleted prior to creating the new data set, no data set will exist if creation of the new data set fails.


  If *replace=True*, all data in the original data set will be lost.


  | **required**: False
  | **type**: bool


     
sms_data_class
  The data class for an SMS-managed dataset.

  Optional for SMS-managed datasets that do not match an SMS-rule.

  Not valid for datasets that are not SMS-managed.

  Note that all non-linear VSAM datasets are SMS-managed.


  | **required**: False
  | **type**: str


     
sms_management_class
  The management class for an SMS-managed dataset.

  Optional for SMS-managed datasets that do not match an SMS-rule.

  Not valid for datasets that are not SMS-managed.

  Note that all non-linear VSAM datasets are SMS-managed.


  | **required**: False
  | **type**: str


     
sms_storage_class
  The storage class for an SMS-managed dataset.

  Required for SMS-managed datasets that do not match an SMS-rule.

  Not valid for datasets that are not SMS-managed.

  Note that all non-linear VSAM datasets are SMS-managed.


  | **required**: False
  | **type**: str


     
space_primary
  The amount of primary space to allocate for the dataset.

  The unit of space used is set using *space_type*.


  | **required**: False
  | **type**: int
  | **default**: 5


     
space_secondary
  The amount of secondary space to allocate for the dataset.

  The unit of space used is set using *space_type*.


  | **required**: False
  | **type**: int
  | **default**: 3


     
space_type
  The unit of measurement to use when defining primary and secondary space.

  Valid units of size are ``K``, ``M``, ``G``, ``CYL``, and ``TRK``.


  | **required**: False
  | **type**: str
  | **default**: M
  | **choices**: K, M, G, CYL, TRK


     
state
  The final state desired for specified data set.

  If *state=absent* and the data set does not exist on the managed node, no action taken, module completes successfully with *changed=False*.


  If *state=absent* and the data set does exist on the managed node, remove the data set, module completes successfully with *changed=True*.


  If *state=absent* and *volumes* is provided, and the data set is not found in the catalog, the module attempts to perform catalog using supplied *name* and *volumes*. If the attempt to catalog the data set catalog is successful, then the data set is removed. Module completes successfully with *changed=True*.


  If *state=absent* and *volumes* is provided, and the data set is not found in the catalog, the module attempts to perform catalog using supplied *name* and *volumes*. If the attempt to catalog the data set catalog fails, then no action is taken. Module completes successfully with *changed=False*.


  If *state=present* and the data set does not exist on the managed node, create and catalog the data set, module completes successfully with *changed=True*.


  If *state=present* and *replace=True* and the data set is present on the managed node the existing data set is deleted, and a new data set is created and cataloged with the desired attributes, module completes successfully with *changed=True*.


  If *state=present* and *replace=False* and the data set is present on the managed node, no action taken, module completes successfully with *changed=False*.


  If *state=cataloged* and *volumes* is provided and the data set is already cataloged, no action taken, module completes successfully with *changed=False*.


  If *state=cataloged* and *volumes* is provided and the data set is not cataloged, module attempts to perform catalog using supplied *name* and *volumes*. If the attempt to catalog the data set catalog is successful, module completes successfully with *changed=True*.


  If *state=cataloged* and *volumes* is provided and the data set is not cataloged, module attempts to perform catalog using supplied *name* and *volumes*. If the attempt to catalog the data set catalog fails, returns failure with *changed=False*.


  If *state=uncataloged* and the data set is not found, no action taken , module completes successfully with *changed=False*.


  If *state=uncataloged* and the data set is found, the data set is uncataloged, module completes successfully with *changed=True*.



  | **required**: False
  | **type**: str
  | **default**: present
  | **choices**: present, absent, cataloged, uncataloged


     
type
  The data set type to be used when creating a data set. (e.g ``pdse``)

  ``MEMBER`` expects to be used with an existing partitioned data set.

  Choices are case-insensitive.


  | **required**: False
  | **type**: str
  | **choices**: KSDS, ESDS, RRDS, LDS, SEQ, PDS, PDSE, LIBRARY, BASIC, LARGE, MEMBER


     
volumes
  If cataloging a data set, *volumes* specifies the name of the volume(s) where the data set is located.


  If creating a data set, *volumes* specifies the volume(s) where the data set should be created.


  If *volumes* is provided when *state=present*, and the data set is not found in the catalog, :ref:`zos_data_set <zos_data_set_module>` will check the volume table of contents to see if the data set exists. If the data set does exist, it will be cataloged.


  If *volumes* is provided when *state=absent* and the data set is not found in the catalog, :ref:`zos_data_set <zos_data_set_module>` will check the volume table of contents to see if the data set exists. If the data set does exist, it will be cataloged and promptly removed from the system.


  *volumes* is required when *state=cataloged*.

  Accepts a string when using a single volume and a list of strings when using multiple.


  | **required**: False
  | **type**: raw




Examples
--------

.. code-block:: yaml+jinja

   
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
       space_type: M
       record_format: fba
       record_length: 25

   - name: Attempt to replace a data set if it exists
     zos_data_set:
       name: someds.name.here
       type: pds
       space_primary: 5
       space_type: M
       record_format: u
       record_length: 25
       replace: yes

   - name: Attempt to replace a data set if it exists. If not found in the catalog, check if it is available on volume 222222, and catalog if found.
     zos_data_set:
       name: someds.name.here
       type: pds
       space_primary: 5
       space_type: M
       record_format: u
       record_length: 25
       volumes: "222222"
       replace: yes

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

   - name: Delete a data set if it exists. If data set not cataloged, check on volume 222222 for the data set, and then catalog and delete if found.
     zos_data_set:
       name: someds.name.here
       state: absent
       volumes: "222222"

   - name: Write a member to an existing PDS; replace if member exists
     zos_data_set:
       name: someds.name.here(mydata)
       type: MEMBER
       replace: yes

   - name: Write a member to an existing PDS; do not replace if member exists
     zos_data_set:
       name: someds.name.here(mydata)
       type: MEMBER

   - name: Remove a member from an existing PDS
     zos_data_set:
       name: someds.name.here(mydata)
       state: absent
       type: MEMBER

   - name: Create multiple partitioned data sets and add one or more members to each
     zos_data_set:
       batch:
         - name:  someds.name.here1
           type: PDS
           space_primary: 5
           space_type: M
           record_format: fb
           replace: yes
         - name: someds.name.here1(member1)
           type: MEMBER
         - name: someds.name.here2(member1)
           type: MEMBER
           replace: yes
         - name: someds.name.here2(member2)
           type: MEMBER

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









Return Values
-------------


   
                              
       names
        | The data set names, including temporary generated data set names, in the order provided to the module.
      
        | **returned**: always
        | **type**: list
      
        
