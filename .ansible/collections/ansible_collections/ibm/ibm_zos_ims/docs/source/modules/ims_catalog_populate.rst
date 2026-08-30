
:github_url: https://github.com/ansible-collections/ibm_zos_core/blob/dev/plugins/modules/ims_catalog_populate.py

.. _ims_catalog_populate_module:


ims_catalog_populate -- Add records to the  IMS Catalog
=======================================================



.. contents::
   :local:
   :depth: 1


Synopsis
--------
- The IMS Catalog Populate utility DFS3PU00 loads, inserts, or updates DBD and PSB instances into the database data sets of the IMS catalog from ACB library data sets.





Parameters
----------


mode
  Indicates the mode in which the Catalog Populate utility must be run.

  | **required**: True
  | **type**: str
  | **choices**: LOAD, UPDATE, READ


online_batch
  Indicates if this utility is to be run in a BMP region.

  | **required**: False
  | **type**: bool


ims_id
  The identifier of the IMS system on which the job is to be run.

  Required if online_batch is true

  | **required**: False
  | **type**: str


dbrc
  Indicates if the IMS Database Recovery Control facility is enabled.

  | **required**: False
  | **type**: bool


irlm_id
  The IRLM ID if IRLM is enabled. Cannot be specified when online_batch is true.

  | **required**: False
  | **type**: str


modstat
  Describes the IMS MODSTAT data set.

  | **required**: False
  | **type**: str


reslib
  Points to an authorized library that contains the IMS SVC modules.

  | **required**: False
  | **type**: list


buffer_pool_param_dataset
  Defines the buffer pool parameters data set. This option is required if you are running the utility as a DLI.

  | **required**: False
  | **type**: str


primary_log_dataset
  Defines the primary IMS log data set. This is required if dbrc is set to true or if mode 'UPDATE' is selected.

  | **required**: False
  | **type**: dict


  dataset_name
    Describes the name of the data set.

    | **required**: True
    | **type**: str


  disposition
    The status of the data set.

    | **required**: False
    | **type**: str
    | **choices**: NEW, OLD, SHR, EXCL


  record_format
    The record format.

    | **required**: False
    | **type**: str
    | **choices**: FB, VB, FBA, VBA, U


  record_length
    The logical record length in bytes.

    | **required**: False
    | **type**: int


  block_size
    The block size.

    | **required**: False
    | **type**: int


  primary
    The amount of primary space to allocate for the data set.

    | **required**: False
    | **type**: int


  primary_unit
    The unit of size to use when specifying primary space.

    | **required**: False
    | **type**: str


  secondary
    The amount of secondary space to allocate for the data set.

    | **required**: False
    | **type**: int


  secondary_unit
    The unit of size to use when specifying secondary space.

    | **required**: False
    | **type**: str


  normal_disposition
    Data set action after normal termination.

    | **required**: False
    | **type**: str
    | **choices**: DELETE, KEEP, CATLG, UNCATLG


  abnormal_disposition
    Data set action after abnormal termination.

    | **required**: False
    | **type**: str
    | **choices**: DELETE, KEEP, CATLG, UNCATLG


  volumes
    A list of volume serials. When providing multiple volumes, processing will begin with the first volume in the provided list. Offline volumes are not considered.

    | **required**: False
    | **type**: list
    | **elements**: str


  type
    The type of data set.

    | **required**: False
    | **type**: str
    | **choices**: SEQ, BASIC, LARGE, PDS, PDSE, LIBRARY, LDS, RRDS, ESDS, KSDS


  storage_class
    The storage class for an SMS-managed data set. Not valid for data sets that are not SMS-managed.

    | **required**: False
    | **type**: str


  management_class
    The management class for an SMS-managed data set. Not valid for data sets that are not SMS-managed.

    | **required**: False
    | **type**: str


  data_class
    The data class for an SMS-managed data set. Not valid for data sets that are not SMS-managed.

    | **required**: False
    | **type**: str



secondary_log_dataset
  Defines the secondary IMS log data set.

  | **required**: False
  | **type**: dict


  dataset_name
    Describes the name of the data set.

    | **required**: True
    | **type**: str


  disposition
    The status of the data set.

    | **required**: False
    | **type**: str
    | **choices**: NEW, OLD, SHR, EXCL


  record_format
    The record format.

    | **required**: False
    | **type**: str
    | **choices**: FB, VB, FBA, VBA, U


  record_length
    The logical record length in bytes.

    | **required**: False
    | **type**: int


  block_size
    The block size.

    | **required**: False
    | **type**: int


  primary
    The amount of primary space to allocate for the data set.

    | **required**: False
    | **type**: int


  primary_unit
    The unit of size to use when specifying primary space.

    | **required**: False
    | **type**: str


  secondary
    The amount of secondary space to allocate for the data set.

    | **required**: False
    | **type**: int


  secondary_unit
    The unit of size to use when specifying secondary space.

    | **required**: False
    | **type**: str


  normal_disposition
    Data set action after normal termination.

    | **required**: False
    | **type**: str
    | **choices**: DELETE, KEEP, CATLG, UNCATLG, CATALOG, UNCATALOG


  abnormal_disposition
    Data set action after abnormal termination

    | **required**: False
    | **type**: str
    | **choices**: DELETE, KEEP, CATLG, UNCATLG, CATALOG, UNCATALOG


  volumes
    A list of volume serials. When providing multiple volumes, processing will begin with the first volume in the provided list. Offline volumes are not considered.

    | **required**: False
    | **type**: list
    | **elements**: str


  type
    The type of data set.

    | **required**: False
    | **type**: str
    | **choices**: SEQ, BASIC, LARGE, PDS, PDSE, LIBRARY, LDS, RRDS, ESDS, KSDS


  storage_class
    The storage class for an SMS-managed data set. Not valid for data sets that are not SMS-managed.

    | **required**: False
    | **type**: str


  management_class
    The management class for an SMS-managed data set. Not valid for data sets that are not SMS-managed.

    | **required**: False
    | **type**: str


  data_class
    The data class for an SMS-managed data set. Not valid for data sets that are not SMS-managed.

    | **required**: False
    | **type**: str



psb_lib
  Defines the IMS.PSBLIB data set.

  | **required**: True
  | **type**: list
  | **elements**: str


dbd_lib
  Defines the IMS.DBDLIB data sets.

  | **required**: True
  | **type**: list
  | **elements**: str


check_timestamp
  Determines if the utility should check timestamps of ACB members with duplicate names.

  If true, the utility will check if the ACB generation timestamp is different from the previously processed ACB member with the same name.

  If the timestamp is different, it will use the ACB with the duplicate name. If not, it will ignore the ACB with the duplicate name.

  | **required**: False
  | **type**: bool


acb_lib
  Defines an ACB library data set that contains the ACB members that are used to populate the IMS catalog.

  | **required**: True
  | **type**: list
  | **elements**: str


bootstrap_dataset
  Optionally defines the IMS directory bootstrap data set.

  | **required**: False
  | **type**: dict


  dataset_name
    Describes the name of the data set.

    | **required**: True
    | **type**: str


  disposition
    The status of the data set.

    | **required**: False
    | **type**: str
    | **choices**: NEW, OLD, SHR, EXCL


  block_size
    The block size.

    | **required**: False
    | **type**: int


  primary
    The amount of primary space to allocate for the data set.

    | **required**: False
    | **type**: int


  primary_unit
    The unit of size to use when specifying primary space.

    | **required**: False
    | **type**: str


  secondary
    The amount of secondary space to allocate for the data set.

    | **required**: False
    | **type**: int


  secondary_unit
    The unit of size to use when specifying secondary space.

    | **required**: False
    | **type**: str


  normal_disposition
    Data set action after normal termination.

    | **required**: False
    | **type**: str
    | **choices**: DELETE, KEEP, CATLG, UNCATLG


  abnormal_disposition
    Data set action after abnormal termination

    | **required**: False
    | **type**: str
    | **choices**: DELETE, KEEP, CATLG, UNCATLG


  volumes
    A list of volume serials. When providing multiple volumes, processing will begin with the first volume in the provided list. Offline volumes are not considered.

    | **required**: False
    | **type**: list
    | **elements**: str


  storage_class
    The storage class for an SMS-managed data set. Not valid for data sets that are not SMS-managed.

    | **required**: False
    | **type**: str


  management_class
    The management class for an SMS-managed data set. Not valid for data sets that are not SMS-managed.

    | **required**: False
    | **type**: str


  data_class
    The data class for an SMS-managed data set. Not valid for data sets that are not SMS-managed.

    | **required**: False
    | **type**: str



directory_datasets
  Optionally defines the IMS directory data sets that are used to store the ACBs.

  If this is omitted, the utility dynamically deletes any preexisting directory data sets and dynamically creates two new data sets to store the ACBs.

  The data set name must conform to the same naming convention as for a system-created directory data set.

  | **required**: False
  | **type**: list
  | **elements**: dict


  dataset_name
    Describes the name of the data set.

    | **required**: True
    | **type**: str


  disposition
    The status of the data set.

    | **required**: False
    | **type**: str
    | **choices**: NEW, OLD, SHR, EXCL


  primary
    The amount of primary space to allocate for the data set.

    | **required**: False
    | **type**: int


  primary_unit
    The unit of size to use when specifying primary space.

    | **required**: False
    | **type**: str


  secondary
    The amount of secondary space to allocate for the data set.

    | **required**: False
    | **type**: int


  secondary_unit
    The unit of size to use when specifying secondary space.

    | **required**: False
    | **type**: str


  normal_disposition
    Data set action after normal termination.

    | **required**: False
    | **type**: str
    | **choices**: DELETE, KEEP, CATLG, UNCATLG


  abnormal_disposition
    Data set action after abnormal termination.

    | **required**: False
    | **type**: str
    | **choices**: DELETE, KEEP, CATLG, UNCATLG


  volumes
    A list of volume serials. When providing multiple volumes, processing will begin with the first volume in the provided list. Offline volumes are not considered.

    | **required**: False
    | **type**: list
    | **elements**: str


  storage_class
    The storage class for an SMS-managed data set. Not valid for data sets that are not SMS-managed.

    | **required**: False
    | **type**: str


  management_class
    The management class for an SMS-managed data set. Not valid for data sets that are not SMS-managed.

    | **required**: False
    | **type**: str


  data_class
    The data class for an SMS-managed data set. Not valid for data sets that are not SMS-managed.

    | **required**: False
    | **type**: str



temp_acb_dataset
  An optional control statement to define an empty work data set to be used as an IMS.ACBLIB data set for the IMS Catalog Populate utility.

  If IMS Management of ACBs is not enabled, this statement is ommitted.

  This data set does not need to conform to any IMS Catalog or system-defined naming convention.

  | **required**: False
  | **type**: dict


  dataset_name
    Describes the name of the data set.

    | **required**: True
    | **type**: str


  disposition
    The status of the data set.

    | **required**: False
    | **type**: str
    | **choices**: NEW, OLD, SHR, EXCL


  primary
    The amount of primary space to allocate for the data set.

    | **required**: False
    | **type**: int


  primary_unit
    The unit of size to use when specifying primary space.

    | **required**: False
    | **type**: str


  secondary
    The amount of secondary space to allocate for the data set.

    | **required**: False
    | **type**: int


  secondary_unit
    The unit of size to use when specifying secondary space.

    | **required**: False
    | **type**: str


  normal_disposition
    Data set action after normal termination.

    | **required**: False
    | **type**: str
    | **choices**: DELETE, KEEP, CATLG, UNCATLG


  abnormal_disposition
    Data set action after abnormal termination.

    | **required**: False
    | **type**: str
    | **choices**: DELETE, KEEP, CATLG, UNCATLG


  volumes
    A list of volume serials. When providing multiple volumes, processing will begin with the first volume in the provided list. Offline volumes are not considered.

    | **required**: False
    | **type**: list
    | **elements**: str


  storage_class
    The storage class for an SMS-managed data set. Not valid for data sets that are not SMS-managed.

    | **required**: False
    | **type**: str


  management_class
    The management class for an SMS-managed data set. Not valid for data sets that are not SMS-managed.

    | **required**: False
    | **type**: str


  data_class
    The data class for an SMS-managed data set. Not valid for data sets that are not SMS-managed.

    | **required**: False
    | **type**: str



directory_staging_dataset
  Optionally defines the size and placement IMS of the directory staging data set.

  The data set must follow the naming convention for the IMS Catalog Directory.

  | **required**: False
  | **type**: dict


  dataset_name
    Describes the name of the data set.

    | **required**: True
    | **type**: str


  disposition
    The status of the data set.

    | **required**: False
    | **type**: str
    | **choices**: NEW, OLD, SHR, EXCL


  primary
    The amount of primary space to allocate for the data set.

    | **required**: False
    | **type**: int


  primary_unit
    The unit of size to use when specifying primary space.

    | **required**: False
    | **type**: str


  secondary
    The amount of secondary space to allocate for the data set.

    | **required**: False
    | **type**: int


  secondary_unit
    The unit of size to use when specifying secondary space.

    | **required**: False
    | **type**: str


  normal_disposition
    Data set action after normal termination.

    | **required**: False
    | **type**: str
    | **choices**: DELETE, KEEP, CATLG, UNCATLG


  abnormal_disposition
    Data set action after abnormal termination.

    | **required**: False
    | **type**: str
    | **choices**: DELETE, KEEP, CATLG, UNCATLG


  volumes
    A list of volume serials. When providing multiple volumes, processing will begin with the first volume in the provided list. Offline volumes are not considered.

    | **required**: False
    | **type**: list
    | **elements**: str


  storage_class
    The storage class for an SMS-managed data set. Not valid for data sets that are not SMS-managed.

    | **required**: False
    | **type**: str


  management_class
    The management class for an SMS-managed data set. Not valid for data sets that are not SMS-managed.

    | **required**: False
    | **type**: str


  data_class
    The data class for an SMS-managed data set. Not valid for data sets that are not SMS-managed.

    | **required**: False
    | **type**: str



proclib
  Defines the IMS.PROCLIB data set that contains the DFSDFxxx member. The  DFSDFxxx member defines various attributes of the IMS catalog that are required by the utility.

  | **required**: True
  | **type**: list


steplib
  Points to IMS.SDFSRESL, which contains the IMS nucleus and required IMS modules.

  The steplib parameter can also be specified in the target inventory's environment_vars.

  The steplib input parameter to the module will take precedence over the value specified in the environment_vars.

  | **required**: False
  | **type**: list


sysabend
  Defines the dump data set. This defaults to = \*

  | **required**: False
  | **type**: str


control_statements
  The control statement parameters.

  | **required**: False
  | **type**: dict


  print_duplicate_resources
    Specifies that the DFS3PU00 utility lists each DBD or PSB resource in the input ACB library that is not added to the IMS catalog because it is a duplicate of an instance in the IMS catalog.

    Equivalent to the DUPLIST control statement.

    | **required**: False
    | **type**: bool


  print_inserted_resources
    If the IMS management of ACBs is enabled, the utility also lists each DBD or PSB resources that is either added to the IMS directory or saved to the staging data set for importing into the IMS directory later.

    Equivalent to the ISRTLIST control statement.

    | **required**: False
    | **type**: bool
    | **default**: True


  max_error_msgs
    Terminate the IMS Catalog Populate utility when more than n messages indicate errors that prevent certain DBDs and PSBs from having their metadata that is written to the IMS catalog.

    Equivalent to the ERRORMAX=n control statement.

    | **required**: False
    | **type**: int


  resource_chkp_freq
    Specifies the number of DBD and PSB resource instances to be inserted between checkpoints. n can be a 1 to 8 digit numeric value between 1 to 99999999.

    Equivalent to the RESOURCE_CHKP_FREQ=n control statement.

    | **required**: False
    | **type**: int


  segment_chkp_freq
    Specifies the number of segments to be inserted between checkpoints. Can be a 1 to 8 digit numeric value between 1 to 99999999.

    Equivalent to the SEGMENT_CHKP_FREQ=n control statement.

    | **required**: False
    | **type**: int


  managed_acbs
    Use the managed_acbs parameter to perform the following actions.

    Set up IMS to manage the runtime application control blocks for your databases and program views.

    Update an IMS system that manages ACBs with new or modified ACBs from an ACB library data set.

    Save ACBs from an ACB library to a staging data set for later importing into an IMS system that manages ACBs.

    | **required**: False
    | **type**: dict


    setup
      Creates the IMS directory data sets that are required by IMS to manage application control blocks.

      | **required**: False
      | **type**: bool


    stage
      Saves ACBs from the input ACB libraries to a staging data set.

      | **required**: False
      | **type**: dict


      save_acb
        If an ACB already exists in the IMS system, determines if it should be saved unconditionally or by the latest timestamp.

        | **required**: False
        | **type**: str
        | **choices**: LATEST, UNCOND


      clean_staging_dataset
        If the staging data set is not allocated to any online IMS system, scratch and recreate the staging data set before adding the resources to the staging data set.

        | **required**: False
        | **type**: bool


      gsampcb
        GSAM resources are included for MANAGEDACBS= running in DLI mode using PSB DFSCP001.

        When GSAMPCB is specified, the IEFRDER batch log data set is not used by the catalog members information gather task.

        GSAMPCB and clean_staging_dataset are mutually exclusive.

        | **required**: False
        | **type**: bool


      gsamdbd
        The name of the changed GSAM database. You can use the gsamdbd variable with the STAGE or UPDATE parameter.

        LATEST, UNCOND, DELETE, SHARE, and GSAMPCB are not supported if you specify the gsamdbd variable.

        | **required**: False
        | **type**: str



    update
      Updates the existing IMS directory system data sets directly in exclusive mode. The ACBs are not placed in the staging data set.

      | **required**: False
      | **type**: dict


      replace_acb
        If an ACB already exists in the IMS system, determines if it should be overwritten unconditionally or by the latest timestamp.

        | **required**: False
        | **type**: str
        | **choices**: LATEST, UNCOND


      share_mode
        For dynamic option (DOPT) PSBs only, allocates the required IMS directory data sets in a shared mode so that the DOPT PSBs can be added to the IMS catalog without interrupting online processing.

        | **required**: False
        | **type**: bool


      gsampcb
        GSAM resources are included for MANAGEDACBS= running in DLI mode using PSB DFSCP001. When GSAMPCB is specified, the IEFRDER batch log data set is not used by the catalog members information gather task.

        | **required**: False
        | **type**: bool


      gsamdbd
        The name of the changed GSAM database. You can use the gsamdbd variable with the STAGE or UPDATE parameter.

        LATEST, UNCOND, DELETE, SHARE, and GSAMPCB are not supported if you specify the gsamdbd variable.

        | **required**: False
        | **type**: str







Examples
--------

.. code-block:: yaml+jinja

   
   - name: Example of a loading the IMS Catalog running as a BMP
     ims_catalog_populate:
       online_batch: True
       ims_id: IMS1
       mode: LOAD
       acb_lib:
         - SOME.IMS.ACBLIB
       reslib:
         - SOME.IMS.SDFSRESL
       steplib:
         - SOME.IMS.SDFSRESL
       proclib:
         - SOME.IMS.PROCLIB
       dbd_lib:
         - SOME.IMS.DBDLIB
       psb_lib:
         - SOME.IMS.PSBLIB
       buffer_pool_param_dataset: "SOME.IMS.PROCLIB(DFSVSMHP)"
       primary_log_dataset:
         dataset_name: SOME.IMS.LOG

   - name: Example of loading the IMS Catalog and the IMS Directory data sets with MANAGEDACBS enabled
     ims_catalog_populate:
       mode: LOAD
       acb_lib:
         - SOME.IMS.ACBLIB
       reslib:
         - SOME.IMS.SDFSRESL
       steplib:
         - SOME.IMS.SDFSRESL
       proclib:
         - SOME.IMS.PROCLIB
       dbd_lib:
         - SOME.IMS.DBDLIB
       psb_lib:
         - SOME.IMS.PSBLIB
       buffer_pool_param_dataset: "SOME.IMS.PROCLIB(DFSVSMHP)"
       control_statements:
         managed_acbs:
           setup: true

   - name: Example of updating the IMS Catalog and staging libraries into the IMS directory staging data set
     ims_catalog_populate:
       mode: UPDATE
       acb_lib:
         - SOME.IMS.ACBLIB
       reslib:
         - SOME.IMS.SDFSRESL
       steplib:
         - SOME.IMS.SDFSRESL
       proclib:
         - SOME.IMS.PROCLIB
       dbd_lib:
         - SOME.IMS.DBDLIB
       psb_lib:
         - SOME.IMS.PSBLIB
       buffer_pool_param_dataset: "SOME.IMS.PROCLIB(DFSVSMHP)"
       primary_log_dataset:
         dataset_name: SOME.IMS.LOG
       control_statements:
         managed_acbs:
           stage:
             save_acb: UNCOND
             clean_staging_dataset: true




Notes
-----

.. note::
   The *steplib* parameter can also be specified in the target inventory's environment_vars.

   The *steplib* input parameter to the module will take precedence over the value specified in the environment_vars.

   If only the *steplib* parameter is specified, then only the *steplib* concatenation will be used to resolve the IMS RESLIB data set.

   Specifying only *reslib* without *steplib* is not supported.







Return Values
-------------


content
  The standard output returned running the IMS Catalog Populate module.

  | **returned**: sometimes
  | **type**: str
  | **sample**: DFS4434I INSTANCE 2020200562326 OF DBD AUTODB   WAS ADDED TO A NEWLY CREATED RECORD IN THE IMS CATALOG.

rc
  The return code from the IMS Catalog Populate utility.

  | **returned**: sometimes
  | **type**: str
  | **sample**: 1

stderr
  The standard error output returned from running the IMS Catalog Populate utility.

  | **returned**: sometimes
  | **type**: str

msg
  Messages returned from the IMS Catalog Populate module.

  | **returned**: sometimes
  | **type**: str
  | **sample**: You cannot define directory data sets, the bootstrap data set, or directory staging data sets with MANAGEDACBS=STAGE or MANAGEDACBS=UPDATE

