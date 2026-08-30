
:github_url: https://github.com/ansible-collections/ibm_zos_core/blob/dev/plugins/modules/ims_psb_gen.py

.. _ims_psb_gen_module:


ims_psb_gen -- Generate IMS PSB
===============================



.. contents::
   :local:
   :depth: 1


Synopsis
--------
- The Program Specification Block (PSB) Generation utility places the created PSB in the PSB library so that it can be used by IMS application programs.





Parameters
----------


src
  The src field can reference a PDS, PDSE member, sequential data set, or UNIX System Services file path.

  If a PDS is specified, all members within the PDS will be treated as individual PSB source members to be processed.

  | **required**: False
  | **type**: str


location
  The PSB source location, Supported options are DATA_SET or USS. The default is DATA_SET.

  The DATA_SET option can be used for a PDS, PDSE, or sequential data set.

  | **required**: False
  | **type**: str
  | **default**: DATA_SET
  | **choices**: DATA_SET, USS


replace
  When 'replace' is 'true', an existing PSB member matching the name in the input PSB source will be overwitten.

  | **required**: False
  | **type**: bool
  | **default**: True


member_list
  A list of member names if the source specified is a data set.

  Optionally, proceeding the source_member, a colon with a target name for the generated PSB member can be specified. If no target name is specified, source_name will be used as the target name.

  If 'member_list' is empty and location is set to 'DATA_SET' or not specified, then src is expected to be a sequential data set.

  Elements are of the list are str or dict with single key-value pair

  | **required**: False
  | **type**: list


psb_name
  Target name of the generated PSB member.

  This parameter is only required and applies if src is a sequential data set.

  | **required**: False
  | **type**: str


batch
  Batch can be used to perform multiple operations in a single module call.

  Expects a list of the location(s) of the IMS Program Specification Block (PSB) source to be compiled.

  The source can reference a PDS or PDSE member, sequential data set or UNIX System Services file path.

  | **required**: False
  | **type**: list
  | **elements**: dict


  src
    The src field can reference a PDS, PDSE member, sequential data set, or UNIX System Services file path.

    If a PDS is specified, all members within the PDS will be treated as individual PSB source members to be processed.

    | **required**: True
    | **type**: str


  location
    The PSB source location, Supported options are DATA_SET or USS. The default is DATA_SET.

    The DATA_SET option can be used for a PDS, PDSE, or sequential data set.

    | **required**: False
    | **type**: str
    | **default**: DATA_SET
    | **choices**: DATA_SET, USS


  replace
    When 'replace' is 'true', an existing PSB member matching the name in the input PSB source will be overwitten.

    | **required**: False
    | **type**: bool
    | **default**: True


  member_list
    A list of member names if the source specified is a data set.

    Optionally, proceeding the source_member, a colon with a target name for the generated PSB member can be specified. If no target name is specified, source_name will be used as the target name.

    If 'member_list' is empty and location is set to 'DATA_SET' or not specified, then src is expected to be a sequential data set.

    Elements are of the list are str or dict with single key-value

    | **required**: False
    | **type**: list


  psb_name
    Target name of the generated PSB member.

    This parameter is only required and applies if src is a sequential data set.

    | **required**: False
    | **type**: str



sys_lib
  A list of required macro libraries that are needed to compile the PSB source. These libraries will be used as the sys_lib at compile time.

  | **required**: True
  | **type**: list


dest
  The target output PSBLIB partitioned data set in which the PSB members will be generated.

  | **required**: True
  | **type**: str




Examples
--------

.. code-block:: yaml+jinja

   
   ---
   - name: Basic example of IMS PSBGEN module with single data set
     ims_psb_gen:
       src: /tmp/src/somefile
       location: USS
       replace: true
       dest: SOME.DATA.SET.PSBLIB
       sys_lib:
       - SOME.DATA.SET.SDFSMAC
       - SYS1.MACLIB

   - name: Basic example of IMS PSBGEN module
     ims_psb_gen:
       batch:
       -
         src: /tmp/psbgen02
         location: USS
         replace: true
       -
         src: OMVSADM.IMSTESTU.ANSIBLE.PSB.SRC
         location: DATA_SET
         member_list: [PSBGENL : TARGET1, PSBGENL : TARGET2]
       -
         src: OMVSADM.IMSTESTU.ANSIBLE.PSB.SRC
         member_list: [PSBGENL, PSBGENL]
         replace: true
       -
         src: OMVSADM.IMSTESTU.ANSIBLE.PSB.SRC
         member_list:
         - 'COGPSBL': 'TARGET3'
         - 'COGPSBL2': 'TARGET4'
         replace: true
       -
         src: OMVSADM.IMSTESTU.ANSIBLE.PSB.SQ
         location: DATA_SET
         psb_name: SEQ
       dest: IMSBANK.IMS1.PSBLIB
       sys_lib:
       - IMSBLD.I15RTSMM.SDFSMAC
       - SYS1.MACLIB




Notes
-----

.. note::
   Currently ims_psb_gen does not support copying symbolic links from both local to remote and remote to remote.







Return Values
-------------


batch_result
  List of output for each PSBGEN run on each element in the list of input source if input is batch.

  | **returned**: on batch call
  | **type**: list
  | **elements**: dict

  return_text
    Status message.

    | **returned**: always
    | **type**: str
    | **sample**: Invalid input source list being passed without content.

  src
    input psb src name processed.

    | **returned**: always
    | **type**: str


msg
  The message of the PSBGEN execution result.

  | **returned**: always
  | **type**: str
  | **sample**: PSBGEN execution was successful.

rc
  Module return code (0 for success)

  | **returned**: always
  | **type**: int

stderr
  Module standard error.

  | **returned**: failure
  | **type**: str
  | **sample**: Output data set for DDNAME has invalid record format.

stdout
  Module standard output.

  | **returned**: success
  | **type**: str
  | **sample**: PSBGEN execution was successful.

