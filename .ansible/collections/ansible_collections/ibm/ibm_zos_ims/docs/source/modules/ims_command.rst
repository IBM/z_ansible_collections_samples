
:github_url: https://github.com/ansible-collections/ibm_zos_core/blob/dev/plugins/modules/ims_command.py

.. _ims_command_module:


ims_command -- Submit IMS Commands
==================================



.. contents::
   :local:
   :depth: 1


Synopsis
--------
- Submit Type 1 and Type 2 IMS Commands. User specifies a well formatted IMS Command string along with PLEX and (optional) ROUTE information.
- IMS will return a completion code, return code, and reason code along with any relevant text indicating the status of the command that was run.





Parameters
----------


batch
  submit multiple IMS commands with a single invocation of the module.

  | **required**: False
  | **type**: list
  | **elements**: dict


  command
    This is the (well-formatted) command to submit to IMS Batch.

    | **required**: True
    | **type**: str


  plex
    Specify the IMSPLEX in which the IMS Command will be submitted.

    | **required**: True
    | **type**: str


  route
    Specify the IMS System in which the IMS Command will be submitted.

    Leaving this field empty will result in invoking all available routes within the specified PLEX.

    | **required**: False
    | **type**: list



command
  This is the (well-formatted) command to submit to IMS Batch.

  | **required**: True
  | **type**: str


plex
  Specify the IMSPLEX in which the IMS Command will be submitted.

  | **required**: True
  | **type**: str


route
  Specify the IMS System in which the IMS Command will be submitted.

  Leaving this field empty will result in invoking all available routes within the specified PLEX.

  | **required**: False
  | **type**: list




Examples
--------

.. code-block:: yaml+jinja

   
   - name: Query all programs for IMS1 in PLEX1
     ims_command:
       command: QUERY PGM SHOW(ALL)
       plex: PLEX1
       route: IMS1

   - name: Query all programs for IMS1 and IMS2 in PLEX1
     ims_command:
       command: QUERY PGM SHOW(ALL)
       plex: PLEX1
       route: ['IMS1', 'IMS2']

   - name: Query all transactions for all routes in PLEX1
     ims_command:
       command: QUERY TRAN SHOW(ALL)
       plex: PLEX1

   - name: Stop all transactions for IMS2 in PLEX1
     ims_command:
       command: UPDATE TRAN STOP(Q)
       plex: PLEX1
       route: IMS2

   - name: Create a DB called IMSDB1 for IMS3 in PLEX2
     ims_command:
       command: CREATE DB NAME(IMSDB1)
       plex: PLEX2
       route: IMS3

   - name: Batch call - query all pgms, create pgm, and query for new
     ims_command:
       batch:
         -
           command: QUERY PGM SHOW(ALL)
           plex: PLEX1
           route: IMS1
         -
           command: CREATE PGM NAME(EXAMPLE1)
           plex: PLEX1
           route: IMS1
         -
           command: QUERY PGM SHOW(ALL)
           plex: PLEX1
           route: IMS1




Notes
-----

.. note::
   This module requires Structured Call Interface (SCI) and Operations Manager (OM) to be active in the target IMSplex.







Return Values
-------------


failed
  Indicates the outcome of the module.

  | **returned**: always
  | **type**: bool

ims_output
  The output provided by the specified IMS Command. All the IMS return, reason, and completion codes from running the commands along with associated text.

  | **returned**: sometimes
  | **type**: list
  | **elements**: dict

  ims_member_data
    Output from Type 1 commands.

    | **returned**: sometimes
    | **type**: dict

  ims_member_messages
    Messages from the IMS instance in which the command was routed.

    | **returned**: sometimes
    | **type**: dict

  return_codes
    Return codes indicating the general result of running the IMS command.

    | **returned**: always
    | **type**: dict

    imsrc
      General IMS return code.

      | **type**: str

    reason
      Return code indicating specific status of the command.

      | **type**: str

    results
      Return code indicating the results of the command.

      | **type**: str


  subgroup_info
    Returns output from the OM instance in which the command was routed.

    | **returned**: always
    | **type**: dict

    ctl.rc
      Return code (i.e. 0000000).

      | **type**: str

    ctl.rsn
      CTL reason code.

      | **type**: str


  type_2_data
    Data resulting from the output of the IMS command submitted.

    | **returned**: sometimes
    | **type**: dict

    CC
      Completion code for the line of output. Completion code is always returned.

      | **type**: str

    CCText
      Completion code text that describes the meaning of the nonzero completion code.

      | **type**: str



