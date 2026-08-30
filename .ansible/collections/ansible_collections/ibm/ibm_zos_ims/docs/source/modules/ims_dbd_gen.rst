
:github_url: https://github.com/ansible-collections/ibm_zos_core/blob/dev/plugins/modules/ims_dbd_gen.py

.. _ims_dbd_gen_module:


ims_dbd_gen -- Generate IMS DBD
===============================



.. contents::
   :local:
   :depth: 1


Synopsis
--------
- This ims_dbd_gen module generates IMS database descriptor (DBD) resource(s) to define a database so that it can be used by IMS application programs.
- A database descriptor (DBD) is a DL/I control block describing the database, segments, fields, indexes and relationships.
- Generating a DBD is a two step process that assembles the DBD source and binds it into a database definition.





Parameters
----------


src
  The src field can reference a PDS, PDSE member, sequential data set, or UNIX System Services file path.

  If a PDS is specified, all members within the PDS will be treated as individual DBD source members to be processed.

  | **required**: False
  | **type**: str


location
  The DBD source location. Supported options are DATA_SET or USS. The default is DATA_SET.

  The DATA_SET option can be used for a PDS, PDSE, or sequential data set.

  | **required**: False
  | **type**: str
  | **default**: DATA_SET
  | **choices**: DATA_SET, USS


replace
  When 'replace' is 'true', an existing DBD member matching the name in the

  input DBD source will be overwitten.

  | **required**: False
  | **type**: bool
  | **default**: True


member_list
  A list of member names if the source specified is a data set.

  Optionally, proceeding the source_member, a colon with a target name for the generated DBD member can be specified. If no target name is specified, source_name will be used as the target name.

  If 'member_list' is empty and location is set to 'DATA_SET' or not specified, then src is expected to be a sequential data set.

  Elements are of the list are str or dict with single key-value pair

  | **required**: False
  | **type**: list


dbd_name
  Target name of the generated DBD member.

  This parameter is only required and applies if src is a sequential data set.

  | **required**: False
  | **type**: str


batch
  Batch can be used to perform multiple operations in a single module call.

  Expects a list of the location(s) of the IMS Database Descriptor (DBD) source to be compiled.

  The source can reference a PDS or PDSE member, sequential data set or UNIX System Services file path.

  | **required**: False
  | **type**: list
  | **elements**: dict


  src
    The src field can reference a PDS, PDSE member, sequential data set, or UNIX System Services file path.

    If a PDS is specified, all members within the PDS will be treated as individual DBD source members to be processed.

    | **required**: True
    | **type**: str


  location
    The DBD source location. Supported options are DATA_SET or USS. The default is DATA_SET.

    The DATA_SET option can be used for a PDS, PDSE, or sequential data set.

    | **required**: True
    | **type**: str
    | **default**: DATA_SET
    | **choices**: DATA_SET, USS


  replace
    When 'replace' is 'true', an existing DBD member matching the name in the input DBD source will be overwitten.

    | **required**: False
    | **type**: bool
    | **default**: True


  member_list
    A list of member names if the source specified is a data set.

    Optionally, proceeding the source_member, a colon with a target name for the generated DBD member can be specified. If no target name is specified, source_name will be used as the target name.

    If 'member_list' is empty and location is set to 'DATA_SET' or not specified, then src is expected to be a sequential data set.

    Elements are of the list are str or dict with single key-value pair

    | **required**: False
    | **type**: list


  dbd_name
    Target name of the generated DBD member.

    This parameter is only required and applies if src is a sequential data set.

    | **required**: False
    | **type**: str



sys_lib
  A list of required macro libraries that are needed to compile the DBD source. These libraries will be used as the sys_lib at compile time.

  | **required**: True
  | **type**: list


dest
  The target output DBDLIB partitioned data set where the DBD members will be generated to.

  | **required**: True
  | **type**: str




Examples
--------

.. code-block:: yaml+jinja

   
   - name: Basic example of IMS DBDGEN module with a single USS source.
     ims_dbd_gen:
       src: /tmp/src/somefile
       location: USS
       'replace': true
       dest: SOME.PARTITIONED.DATA.SET.DBDLIB
       sys_lib:
         - SOME.DATA.SET.SDFSMAC
         - SYS1.MACLIB
   - name: Basic example of IMS DBDGEN module with a single sequential data set.source.
     ims_dbd_gen:
       src: SOME.DATA.SET.DBD
       'replace': true
       dest: SOME.PARTITIONED.DATA.SET.DBDLIB
       dbd_name: exampleDBD
       sys_lib:
         - SOME.DATA.SET.SDFSMAC
         - SYS1.MACLIB
   - name: Basic example of IMS DBDGEN module with a single PDS source.
     ims_dbd_gen:
       src: SOME.DATA.SET.DBD.SRC
       'replace': true
       member_list:
         - 'DEDBJN21': 'DBD1'
         - 'DEDBJN21': 'DBD2'
         - 'DEDBJNV1': 'DBD3'
       dest: SOME.PARTITIONED.DATA.SET.DBDLIB
       sys_lib:
         - SOME.DATA.SET.SDFSMAC
         - SYS1.MACLIB
   - name: Basic example of IMS DBDGEN module with a batch input uniform source type.
     ims_dbd_gen:
       batch:
         -
           src: /tmp/src/somefile1
           location: USS
           'replace': true
         -
           src: /tmp/src/somefile2
           location: USS
           'replace': true
       dest: SOME.PARTITIONED.DATA.SET.DBDLIB
       sys_lib:
         - SOME.DATA.SET.SDFSMAC
         - SYS1.MACLIB
   - name: Basic example of IMS DBDGEN module with a batch input varied source type.
     ims_dbd_gen:
       batch:
         -
           src: /tmp/src/somefile
           location: USS
           'replace': true
         -
           src: SOME.DATA.SET.DBD.SRC
           location: DATA_SET
           member_list: [DSMEMBR1, DSMEMBR2 : target2, DSMEMBR3]
         -
           src: SOME.DATA.SET.DBD.SRC
           member_list: [DSMEMBR4 : target4]
           'replace': true
         -
           src: SOME.DATA.SET.DBD.SEQ
           location: DATA_SET
           dbd_name: SEQ
       dest: SOME.PARTITIONED.DATA.SET.DBDLIB
       sys_lib:
         - SOME.DATA.SET.SDFSMAC
         - SYS1.MACLIB




Notes
-----

.. note::
   Currently ims_dbd_gen does not support copying symbolic links from both local to remote and remote to remote.







Return Values
-------------


batch_result
  List of output for each DBDGEN run on each element in the list of input source if input is batch.

  | **returned**: on batch call
  | **type**: list
  | **elements**: dict

  return_text
    Status message.

    | **returned**: always
    | **type**: str
    | **sample**: Invalid input source list being passed without content.

  src
    input dbd src name processed.

    | **returned**: always
    | **type**: str


msg
  The message of the DBDGEN execution result.

  | **returned**: always
  | **type**: str
  | **sample**: DBDGEN execution was successful.

rc
  Module return code (0 for success)

  | **returned**: always
  | **type**: int

stderr
  Module standard error

  | **returned**: failure
  | **type**: str
  | **sample**: Output data set for DDNAME has invalid record format.

stdout
  Module standard output

  | **returned**: success
  | **type**: str
  | **sample**: DBDGEN execution was successful

