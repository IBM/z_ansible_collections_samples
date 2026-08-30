
:github_url: https://github.com/ansible-collections/ibm_zos_core/blob/dev/plugins/modules/ims_dbrc.py

.. _ims_dbrc_module:


ims_dbrc -- Submit IMS DBRC Commands
====================================



.. contents::
   :local:
   :depth: 1


Synopsis
--------
- Use Database Recovery Control (DBRC) to record and manage information that is stored in a set of VSAM data sets that are collectively called the Recovery Control (RECON) data set.
- Based on this information, you can use DBRC to advise IMS about how to proceed with certain IMS actions.





Parameters
----------


command
  This is the well-formatted DBRC command to submit.

  | **required**: True
  | **type**: list


dbd_lib
  The data set that contains the descriptions for the databases that are under the control of DBRC.

  | **required**: False
  | **type**: str


dynamic_allocation_dataset
  The dynamic allocation data set that will be used to complete the DBRC execution.

  Required if `recon1`, `recon2`, and `recon3` are not specified.

  | **required**: False
  | **type**: str


genjcl_input_dataset
  The PDS, which contains the skeletal JCL members used by the DBRC utility to generate JCL.

  Equivalent to the JCLPDS control statement.

  | **required**: False
  | **type**: str


genjcl_output_dataset
  The data set which is to receive the generated JCL. It is required only for the GENJCL commands.

  Equivalent to the JCLOUT control statement.

  | **required**: False
  | **type**: str


max_rc
  The maximum acceptable return code allowed for the module to complete succesfully.

  | **required**: False
  | **type**: int


recon1
  The RECON1 data set that will be used to complete the DBRC execution.

  Required if `dynamic_allocation_dataset` is not specified.

  | **required**: False
  | **type**: str


recon2
  The RECON2 data set that will be used to complete the DBRC execution.

  Required if `dynamic_allocation_dataset` is not specified.

  | **required**: False
  | **type**: str


recon3
  The RECON3 data set that will be used to complete the DBRC execution.

  Required if `dynamic_allocation_dataset` is not specified.

  | **required**: False
  | **type**: str


steplib
  List of STEPLIB datasets that contain the IMS nucleus and the required action modules.

  | **required**: True
  | **type**: list




Examples
--------

.. code-block:: yaml+jinja

   
   - name: Sample DBRC Single Command
     ims_dbrc:
       command: LIST.RECON STATUS
       steplib:
         - IMSTESTU.IMS1501.MARKER
         - IMSTESTL.IMS1.EXITLIB
         - IMSTESTG.IMS15R.TSTRES
         - IMSBLD.IMS15R.USERLIB
         - IMSBLD.I15RTSMM.CRESLIB
       dbd_lib: IMSTESTL.IMS1.DBDLIB
       genjcl_input_dataset: IMSTESTL.IMS1.GENJCL
       genjcl_output_dataset: IMSTESTL.IMS1.JCLOUT
       dynamic_allocation_dataset: IMSTESTL.IMS1.DYNALLOC

   - name: Sample DBRC Multiple Commands with dynamic_allocation_dataset Specified
     ims_dbrc:
       command:
         - LIST.RECON STATUS
         - LIST.DB ALL
         - LIST.DBDS DBD(CUSTOMER)
       steplib:
         - IMSTESTU.IMS1501.MARKER
         - IMSTESTL.IMS1.EXITLIB
         - IMSTESTG.IMS15R.TSTRES
         - IMSBLD.IMS15R.USERLIB
         - IMSBLD.I15RTSMM.CRESLIB
       dbd_lib: IMSTESTL.IMS1.DBDLIB
       genjcl_input_dataset: IMSTESTL.IMS1.GENJCL
       genjcl_output_dataset: IMSTESTL.IMS1.JCLOUT
       dynamic_allocation_dataset: IMSTESTL.IMS1.SDFSRESL

   - name: Sample DBRC Multiple Commands with RECON specified
     ims_dbrc:
       command:
         - LIST.RECON STATUS
         - INIT.DB DBD(TESTDB)
         - DELETE.DB DBD(TESTDB)
       steplib:
         - IMSTESTU.IMS1501.MARKER
         - IMSTESTL.IMS1.EXITLIB
         - IMSTESTG.IMS15R.TSTRES
         - IMSBLD.IMS15R.USERLIB
         - IMSBLD.I15RTSMM.CRESLIB
       dbd_lib: IMSTESTL.IMS1.DBDLIB
       genjcl_input_dataset: IMSTESTL.IMS1.GENJCL
       genjcl_output_dataset: IMSTESTL.IMS1.JCLOUT
       recon1: IMSTESTL.IMS1.RECON1
       recon2: IMSTESTL.IMS1.RECON2
       recon3: IMSTESTL.IMS1.RECON3




Notes
-----

.. note::
   The *steplib* parameter can also be specified in the target inventory's environment_vars.

   The *steplib* input parameter to the module will take precedence over the value specified in the environment_vars.







Return Values
-------------


dbrc_output
  The output provided by the specified DBRC Command(s).

  | **returned**: sometimes
  | **type**: list

  command
    The original submitted command that corresponds to the output.

    | **returned**: always
    | **type**: str

  messages
    Compiled list of messages returned from the DBRC output.

    | **returned**: always
    | **type**: list

  output
    Parsed DBRC output that maps each field to its corresponding value.

    | **returned**: always
    | **type**: dict


msg
  The output message that the `ims_dbrc` module generates.

  | **returned**: always
  | **type**: str

rc
  The return code returned by the DBRC module.

  | **returned**: always
  | **type**: int

unformatted_output
  Unformatted output response from the all of the submitted DBRC commands.

  | **returned**: always
  | **type**: list

