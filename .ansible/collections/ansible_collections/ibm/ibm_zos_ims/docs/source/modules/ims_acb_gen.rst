
:github_url: https://github.com/ansible-collections/ibm_zos_core/blob/dev/plugins/modules/ims_acb_gen.py

.. _ims_acb_gen_module:


ims_acb_gen -- Generate IMS ACB
===============================



.. contents::
   :local:
   :depth: 1


Synopsis
--------
- The ims_acb_gen module generates an IMS application control block (ACB) necessary for an IMS application program to be scheduled and run.
- The ims_dbd_gen and ims_psb_gen modules can be used to generate the associated IMS database descriptors (DBDs) and program specification block (PSBs) to be used with the ims_acb_gen module.
- The DBD and PSB control blocks will be merged and expanded into an IMS internal format called application control blocks (ACBs).





Parameters
----------


command_input
  This field specifies two command options(BUILD/DELETE).

  BUILD - Specifies that blocks are built for the named PSBs, which refer to the named DBDs.

  DELETE - Specifies that blocks are deleted from the ACBLIB data set. The named PSBs and all PSBs that refer to the named DBDs are deleted.

  | **required**: True
  | **type**: str
  | **choices**: BUILD, DELETE


compression
  PRECOMP,POSTCOMP, in any combination, cause the required in-place compression.

  The default is none.

  | **required**: False
  | **type**: str


psb_name
  The name of the PS**s**. Specifies that blocks are built or deleted for all PSBs that are named on this control statement.

  This field requires "ALL" or a list of psb names to be specified.

  | **required**: False
  | **type**: list


dbd_name
  The name of the DBD(s). Specifies that blocks are built or deleted for this DBD, and for all PSBs that reference this DBD either directly or indirectly through logical relationships.

  | **required**: False
  | **type**: list


acb_lib
  The ACB Maintenance utility maintains the prebuilt blocks (ACB) library (IMS.ACBLIB). The ACB library is a consolidated library of program (PSB) and database (DBD) descriptions.

  The IMS acb_lib must be used exclusively. The module can only be executed using an ACB library which is not concurrently allocated to an active IMS system.

  | **required**: True
  | **type**: str


psb_lib
  The ACB Maintenance utility receives input from the IMS PSBLIB data set.

  The ACB Maintenance utility does not change the PS**s** in PSBLIB. If changes are made in PSBs or DBDs that require changes in the associated PSB, make these changes before running the module.

  Changes in PSBs might also require modifications to the affected application programs. For example, if a DBD has a segment name changed, all PSBs which are sensitive to that segment must have their SENSEG statements changed.

  | **required**: True
  | **type**: list


dbd_lib
  The ACB Maintenance utility receives input from the IMS DBDLIB data set.

  The ACB Maintenance utility does not change the DBD(s) in DBDLIB. If changes are made in PSBs or DBDs that require changes in the associated DBD, make these changes before running the module.

  | **required**: True
  | **type**: list


steplib
  Points to the IMS SDFSRESL data set, which contains the IMS nucleus and required IMS modules. If STEPLIB is unauthorized by having unauthorized libraries that are concatenated to SDFSRESL, you must specify the *reslib* parameter.

  The steplib parameter can also be specified in the target inventory's environment_vars.

  The steplib input parameter to the module will take precedence over the value specified in the environment_vars.

  | **required**: False
  | **type**: list


reslib
  Points to an authorized library that contains the IMS SVC modules. For IMS batch, SDFSRESL and any data set that is concatenated to it in the reslib field must be authorized through the Authorized Program Facility (APF).

  | **required**: False
  | **type**: list


build_psb
  Specifies whether ims_acb_gen rebuilds all PSBs that reference a changed DBD in the *dbdname* parameter.

  TRUE indicates that ims_acb_gen rebuilds all PSBs that reference the changed DBD on the *dbdname* parameter.

  FALSE indicates that ims_acb_gen does not rebuild PSBs that reference the changed DBD if the changed DBD does not change the physical structure of the database.

  | **required**: False
  | **type**: bool
  | **default**: True




Examples
--------

.. code-block:: yaml+jinja

   
   - name: Example of creating ACBs for specific PSBs.
     ims_acb_gen:
       command_input: BUILD
       COMPRESSION: PRECOMP,POSTCOMP
       psb_name:
         - PSB1
         - PSB2
         - PSB3
       dbd_name:
         - DBD1
         - DBD2
       psb_lib:
         - SOME.IMS.PSBLIB1
         - SOME.IMS.PSBLIB2
       dbd_lib:
         - SOME.IMS.DBDLIB1
         - SOME.IMS.DBDLIB2
         - SOME.IMS.DBDLIB3
       acb_lib: SOME.IMS.ACBLIB
       reslib:
         - SOME.IMS.SDFSRESL1
         - SOME.IMS.SDFSRESL2
       steplib:
         - SOME.IMS.SDFSRESL1
         - SOME.IMS.SDFSRESL2
       build_psb: false

   - name: Example of creating blocks for all PSBs in the psb_lib data set.
     ims_acb_gen:
       command_input: BUILD
       psb_name: ALL
       psb_lib:
         - SOME.IMS.PSBLIB1
       dbd_lib:
         - SOME.IMS.DBDLIB1
       acb_lib: SOME.IMS.ACBLIB

   - name: Example of deleting PSBs and DBDs
     ims_acb_gen:
       command_input: DELETE
       psb_name:
         - PSB1
       dbd_name:
         - DBD1
         - DBD2
         - DBD3
         - DBD4
         - DBD5
         - DBD6
       acb_lib: SOME.IMS.ACBLIB
       reslib:
         - SOME.IMS.SDFSRESL1




Notes
-----

.. note::
   The *steplib* parameter can also be specified in the target inventory's environment_vars.

   The *steplib* input parameter to the module will take precedence over the value specified in the environment_vars.

   If only the *steplib* parameter is specified, then only the *steplib* concatenation will be used to resolve the IMS RESLIB dataset.

   If both *steplib* and *reslib* are specified, then both parameters will be used by the ACB Maintenenace Utility and *reslib* will be used to resolve the IMS RESLIB dataset.

   Specifying only *reslib* without *steplib* is not supported.

   The ACB Maintenenace utility SYSUT3/SYSUT4 DD options are not supported by this module.







Return Values
-------------


msg
  Execution result message from the ims_acb_gen module.

  | **returned**: always
  | **type**: str
  | **sample**: ACBGEN execution is successful.

content
  The response from the execution of the ACB Maintenance Utility.

  | **returned**: always
  | **type**: list

rc
  The resulting return code from the ACB Maintenance Utility.

  | **returned**: always
  | **type**: str
  | **sample**: 0

changed
  Indicates if any changes were made during module execution.

  True is always returned unless a module or failure has occurred.

  | **returned**: always
  | **type**: bool

debug
  additional messages returned from ZOAU.

  For more information, refer to the `ZOAU messages documentation <https://www.ibm.com/support/knowledgecenter/en/SSKFYE_1.0.0/bgy.html>`_

  | **returned**: always
  | **type**: str

