# -*- coding: utf-8 -*-

# (c) Copyright IBM Corp. 2024
# Apache License, Version 2.0 (see https://opensource.org/licenses/Apache-2.0)

from __future__ import (absolute_import, division, print_function)
__metaclass__ = type


class ModuleDocFragment(object):

    DOCUMENTATION = r"""
options:
  space_primary:
    description:
      - The size of the primary space allocated to the CICS startup JCL data set.
        Note that this is just the value; the unit is specified with O(space_type).
      - This option takes effect only when the CICS startup JCL data set is being created.
        If the CICS startup JCL data set already exists, the option has no effect.
      - If this option is not set, the primary space is dynamically calculated based on the
        size of the generated CICS startup JCL.
      - If the target data set is a member in a PDS or PDSE, then this value does not have any effect.
    type: int
    required: false
  space_secondary:
    description:
      - The size of the secondary space allocated to the CICS startup JCL data set.
        Note that this is just the value; the unit is specified with O(space_type).
      - This option takes effect only when the CICS startup JCL data set is being created.
        If the CICS startup JCL data set already exists, the option has no effect.
      - If this option is not set, the secondary space is dynamically calculated as 10% of
        the total size of the generated CICS startup JCL.
      - If the target data set is a member in a PDS or PDSE, then this value does not have any effect.
    type: int
    required: false
  space_type:
    description:
      - The unit portion of the CICS startup JCL data set size. Note that this is
        just the unit; the value for the primary space is specified with O(space_primary)
        and the value for the secondary space is specified with O(space_secondary).
      - This option takes effect only when the CICS startup JCL data set is being created.
        If the CICS startup JCL data set already exists, the option has no effect.
      - The size can be specified in megabytes (V(M)), kilobytes (V(K)),
        cylinders (V(CYL)), or tracks (V(TRK)).
      - If neither O(space_secondary) nor O(space_primary) is set, then this value does
        not have any effect.
      - If the target data set is a member in a PDS or PDSE, then this value does not have any effect.
    required: false
    type: str
    choices:
      - M
      - K
      - CYL
      - TRK
    default: M
  volumes:
    description:
      - The volume(s) where the data set is created. Use a string to define a singular volume or a list of strings for multiple volumes.
      - If the target data set is a member in a PDS or PDSE, then this value does not have any effect.
    type: raw
    required: false
  state:
    description:
      - The intended state for the CICS startup JCL data set, which the module aims to achieve.
      - Specify V(absent) to remove the CICS startup JCL data set entirely, if it already exists.
      - Specify V(initial) to create the CICS startup JCL data set if it does not already exist.
      - Specify V(warm) to retain an existing CICS startup JCL data set in its current state.
        The module verifies whether the specified data set exists and whether it matches the
        generated startup JCL.
        If both conditions are met, the module leaves the data set as is.
        If the data set does not exist or does not match, the operation fails.
    choices:
      - "initial"
      - "absent"
      - "warm"
    required: true
    type: str
  job_parameters:
    description:
        - Specifies various parameters to be applied to the CICS startup job.
    type: dict
    required: false
    suboptions:
      accounting_information:
        description:
          - Allows jobs to be grouped into a class.
        type: dict
        required: false
        suboptions:
          pano:
            description:
              - Specifies the programmer's accounting number. The value is 1 through 4 alphanumeric characters.
            type: str
            required: false
          room:
            description:
              - Specifies the programmer's room number. The value is 1 through 4 alphanumeric characters.
            type: str
            required: false
          time:
            description:
              - Specifies the estimated execution time in minutes. The value is 1 through 4 decimal numbers. For example,
                code 30 for 30 minutes. If you omit a time subparameter and a TIME parameter on the JES2 /*JOBPARM
                statement, JES2 uses an installation default specified at initialization. If job execution exceeds the
                time, JES2 sends a message to the operator.
            type: int
            required: false
          lines:
            description:
              - Specifies the estimated line count, in thousands of lines, from this job's sysout data sets. The value is 1
                through 4 decimal numbers. For example, code 5 for 5000 lines. If you omit lines, JES2 uses an
                installation default specified at initialization.
            type: int
            required: false
          cards:
            description:
              - Specifies the estimated number of cards JES2 is to punch from this job's sysout data sets. The value is 1
                through 4 decimal numbers. If you omit cards, JES2 uses an installation default specified at
                initialization.
            type: int
            required: false
          forms:
            description:
              - Specifies the forms that JES2 is to use for printing this job's sysout data sets. The value is 1 through 4
                alphanumeric characters. For example, code 5 for 5-part forms. If you omit forms, JES2 uses an
                installation default specified at initialization.
            type: str
            required: false
          copies:
            description:
              - Specifies the number of times JES2 is to print or punch this job's sysout data sets. The value is 1
                through 3 decimal numbers and must not exceed an installation-specified limit. The maximum is 255. For
                example, code 2 for two copies. If you omit copies, JES2 assumes one copy.
            type: int
            required: false
          log:
            description:
              - Specifies whether JES2 is to print the job log. Code N to surpress printing of the job log. If you code any
                other character or omit this subparameter, JES2 prints the job log. If your installation specified NOLOG
                for this job's class during JES2 initialization, JES2 does not print the job log.
            type: str
            required: false
          linect:
            description:
              - Specifies the number of lines JES2 is to print per page for this job's sysout data sets. The value is 1
                through 3 decimal numbers. If you omit linect, JES2 uses an installation default specified at
                initialization. If you code a zero, JES2 does not eject to a new page when the number of lines exceeds
                the installation default.
            type: int
            required: false
      class:
        description:
          - Allows jobs to be grouped into a class.
        type: str
        required: false
      job_name:
        description:
          - The name of the CICS startup job. The default value is C(APPLID).
        type: str
        required: false
      memlimit:
        description:
          - Use the MEMLIMIT parameter to specify the limit on the total size of usable 64-bit z/OS storage in a
            single address space.
        type: str
        required: false
      msglevel:
        description:
          - Use the MSGLEVEL parameter to control the listing of the JCL output for the job.
        type: dict
        required: false
        suboptions:
          statements:
            description:
              - Indicates which job control statements the system is to print in the statement images portion of the JCL
                output.
            type: int
            choices:
              - "0"
              - "1"
              - "2"
            required: false
          messages:
            description:
              - Indicates which messages the system is to print in the system messages portion of the JCL output.
            type: int
            choices:
              - "0"
              - "1"
            required: false
      msgclass:
        description:
          - Use the MSGCLASS parameter to assign the job log to an output class. The job log is a record of
            job-related information for the programmer.
        type: str
        required: false
      programmer_name:
        description:
          - Use the programmer's name parameter to identify the person or group responsible for a job.
        type: str
        required: false
      region:
        description:
          - Use the REGION parameter to specify the amount of central or virtual storage that the job requires. The
            system applies the value that you code on REGION to each step of the job.
        type: str
        required: false
      user:
        description:
          - Code the USER parameter to identify to the system the person submitting the job. The user ID is used by
            RACF®, the system resources manager (SRM), and other system components.
        type: str
        required: false
  applid:
    description:
      - The name of your z/OS Communications Server application identifier for this CICS region.
    type: str
    required: true
  cics_data_sets:
    description:
      - The data set names of the C(SDFHAUTH), C(SDFHLOAD) and C(SDFHLIC) libraries, for example,
        C(CICSTS61.CICS.SDFHAUTH) and C(CICSTS61.CICS.SDFHLOAD).
    type: dict
    required: true
    suboptions:
      template:
        description:
          - The templated location of the libraries.
        type: str
        required: false
      sdfhauth:
        description:
          - The location of the C(SDFHAUTH) librarty to override the template.
        type: str
        required: false
      sdfhload:
        description:
          - The location of the C(SDFHLOAD) library. If O(cics_data_sets.template) is provided, this value overrides the template.
        type: str
        required: false
      sdfhlic:
        description:
          - The location of the C(SDFHLIC) library. If O(cics_data_sets.template) is provided, this value overrides the template.
        type: str
        required: false
  le_data_sets:
    description:
      - The data set names of the C(SCEECICS), C(SCEERUN) and C(SCEERUN2) libraries.
    type: dict
    required: true
    suboptions:
      template:
        description:
          - The templated location of the Language Enviornment runtime libraries.
        required: false
        type: str
      sceecics:
        description:
          - The location of the C(SCEECICS) library. If O(le_data_sets.template) is provided, this value overrides the template.
        type: str
        required: false
      sceerun:
        description:
          - The location of the C(SCEERUN) library. If O(le_data_sets.template) is provided, this value overrides the template.
        type: str
        required: false
      sceerun2:
        description:
          - The location of the C(SCEERUN2) library. If O(le_data_sets.template) is provided, this value overrides the template.
        type: str
        required: false
  cpsm_data_sets:
    description:
      - The data set names of the C(SEYUAUTH) and C(SEYULOAD) libraries, for example, C(CTS610.CPSM610.SEYUAUTH).
    type: dict
    required: false
    suboptions:
      template:
        description:
          - The templated location of the CICSPlex SM libraries.
        required: false
        type: str
      seyuauth:
        description:
          - The location of the C(SEYUAUTH) library. If O(cpsm_data_sets.template) is provided, this value overrides the template.
        required: false
        type: str
      seyuload:
        description:
          - The location of the C(SEYULOAD) library. If O(cpsm_data_sets.template) is provided, this value overrides the template.
        required: false
        type: str
  steplib:
    description:
      - Any locations of additional data sets other than C(SDFHAUTH), C(SDFHLIC), C(SCEERUN), or C(SCEERUN2), to be added to the STEPLIB concatenation.
        The STEPLIB concatenation is where you specify the libraries that contain the modules loaded by the z/OS operating system.
        You can either add data sets at the very top of the list or append them to the bottom of the list. There are other data sets in between,
        as determined by the defaults or other input parameters; for example, C(SEYUAUTH) and C(SEYULOAD) as sepcified with O(cpsm_data_sets),
        C(SCEERUN) and C(SCEERUN2) as specified with O(le_data_sets), C(SDFHAUTH) and C(SDFHLIC) as specified with O(cics_data_sets), and so on.
    type: dict
    required: false
    suboptions:
      top_data_sets:
        description:
          - The C(STEPLIB) data sets to be added to the very top of the list.
        type: list
        required: false
        elements: str
      data_sets:
        description:
          - The C(STEPLIB) data sets to be added to the bottom of the list.
        type: list
        required: false
        elements: str
  dfhrpl:
    description:
      - Any locations of additional data sets other than C(SDFHLOAD), C(SCEECICS), C(SCEERUN), or C(SCEERUN2), to be added to the DFHRPL concatenation.
        The DFHRPL concatenation is where you specify the libraries that contain modules loaded by CICS, for example, the libraries
        containing your CICS application programs, your CICS control tables, and so on. You can either add data sets at the very top of the
        list or append them to the bottom of the list. There are other data sets in between, as determined by the defaults or other input
        parameters; for example, C(SCEERUN) and C(SCEERUN2) as specified with O(le_data_sets), C(SDFHLOAD) as specified with O(cics_data_sets), and so on.
    type: dict
    required: false
    suboptions:
      top_data_sets:
        description:
          - The C(DFHRPL) data sets to be added to the very top of the list.
        required: false
        type: list
        elements: str
      data_sets:
        description:
          - The C(DFHRPL) data sets to be added to the bottom of the list.
        type: list
        required: false
        elements: str
  region_data_sets:
    description:
      - The location of the region data sets, for example, C(REGIONS.ABCD01.DFHAUXT), C(REGIONS.ABCD01.DFHCSD) and
        C(REGIONS.ABCD01.DFHGCD).
    type: dict
    required: true
    suboptions:
      template:
        description:
          - The base location of the region data sets to be created by using a template, for example,
            C(REGIONS.ABCD0001.<< data_set_name >>). This is not required if you provide the data set
            name (dsn) of all the data sets individually.
        required: false
        type: str
      dfhauxt:
        description:
          - Overrides the templated location for the auxiliary trace A data set.
        required: false
        type: dict
        suboptions:
          dsn:
            description:
              - The name of the auxiliary trace A data set to override the template.
            type: str
            required: false
      dfhbuxt:
        description:
          - Overrides the templated location for the auxiliary trace B data set.
        required: false
        type: dict
        suboptions:
          dsn:
            description:
              - The name of the auxiliary trace B data set to override the template.
            type: str
            required: false
      dfhcsd:
        description:
          - Overrides the templated location for the CSD.
        required: false
        type: dict
        suboptions:
          dsn:
            description:
              - The name of the CSD to override the template.
            type: str
            required: false
      dfhdmpa:
        description:
          - Overrides the templated location for the dump A data set.
        required: false
        type: dict
        suboptions:
          dsn:
            description:
              - The name of the dump A data set to override the template.
            type: str
            required: false
      dfhdmpb:
        description:
          - Overrides the templated location for the dump B data set.
        required: false
        type: dict
        suboptions:
          dsn:
            description:
              - The name of the dump B data set to override the template.
            type: str
            required: false
      dfhlrq:
        description:
          - Overrides the templated location for the local request queue data set.
        required: false
        type: dict
        suboptions:
          dsn:
            description:
              - The data set name of the local request queue to override the template.
            type: str
            required: false
      dfhgcd:
        description:
          - Overrides the templated location for the global catalog data set.
        required: false
        type: dict
        suboptions:
          dsn:
            description:
              - The data set name of the global catalog to override the template.
            type: str
            required: false
      dfhlcd:
        description:
          - Overrides the templated location for the local catalog data set.
        required: false
        type: dict
        suboptions:
          dsn:
            description:
              - The data set name of the local catalog to override the template.
            type: str
            required: false
      dfhintra:
        description:
          - Overrides the templated location for the intrapartition data set.
        required: false
        type: dict
        suboptions:
          dsn:
            description:
              - The name of the intrapartition data set to override the template.
            type: str
            required: false
      dfhtemp:
        description:
          - Overrides the templated location for the temporary storage data set.
        required: false
        type: dict
        suboptions:
          dsn:
            description:
              - The data set name of the temporary storage to override the template.
            type: str
            required: false
      dfhstart:
        description:
          - Overrides the templated location for the CICS startup JCL data set.
        required: false
        type: dict
        suboptions:
          dsn:
            description:
              - The data set name of the CICS startup JCL data set to override the template.
              - The data set name can also be set to a member of an existing PDS or PDSE.
            type: str
            required: false
  output_data_sets:
    description:
      - The system output data sets such as C(CEEMSG) and C(SYSPRINT), as well as the destination class of the output.
    type: dict
    required: false
    suboptions:
      default_sysout_class:
        description:
          - The class to be applied as the default for all of the output data sets. If it isn't provided and if no
            overrides are specified for an individual output data set, * is applied.
        type: str
        required: false
      ceemsg:
        description:
          - Overrides the default class to use a custom class for the C(CEEMSG) data set. Alternatively, omit the
            C(CEEMSG) data set from being added to the job.
        type: dict
        required: false
        suboptions:
          sysout:
            description:
              - Specify the output class to assign the C(CEEMSG) data set to.
            type: str
            required: false
          omit:
            description:
              - Specifies whether C(CEEMSG) should be excluded from being added to the list of sysout data sets.
            type: bool
            required: false
      ceeout:
        description:
          - Overrides the default class to use a custom class for the C(CEEOUT) data set. Alternatively, omit the
            C(CEEOUT) data set from being added to the job.
        type: dict
        required: false
        suboptions:
          sysout:
            description:
              - Specify the output class to assign the C(CEEOUT) data set to.
            type: str
            required: false
          omit:
            description:
              - Specifies whether C(CEEOUT) should be excluded from being added to the list of sysout data sets.
            type: bool
            required: false
      msgusr:
        description:
          - Overrides the default class to use a custom class for the C(MSGUSR) data set. Alternatively, omit the
            C(MSGUSR) data set from being added to the job.
        type: dict
        required: false
        suboptions:
          sysout:
            description:
              - Specify the output class to assign the C(MSGUSR) data set to.
            type: str
            required: false
          omit:
            description:
              - Specifies whether C(MSGUSR) should be excluded from being added to the list of sysout data sets.
            type: bool
            required: false
      sysprint:
        description:
          - Overrides the default class to use a custom class for the C(SYSPRINT) data set. Alternatively, omit the
            C(SYSPRINT) data set from being added to the job.
        type: dict
        required: false
        suboptions:
          sysout:
            description:
              - Specify the output class to assign the C(SYSPRINT) data set to.
            type: str
            required: false
          omit:
            description:
              - Specifies whether C(SYSPRINT) should be excluded from being added to the list of sysout data sets.
            required: false
            type: bool
      sysudump:
        description:
          - Overrides the default class to use a custom class for the C(SYSUDUMP) data set. Alternatively, omit the
            C(SYSUDUMP) data set from being added to the job.
        type: dict
        required: false
        suboptions:
          sysout:
            description:
              - Specify the output class to assign the C(SYSUDUMP) data set to.
            type: str
            required: false
          omit:
            description:
              - Specifies whether C(SYSUDUMP) should be excluded from being added to the list of sysout data sets.
            required: false
            type: bool
      sysabend:
        description:
          - Overrides the default class to use a custom class for the C(SYSABEND) data set. Alternatively, omit the
            C(SYSABEND) data set from being added to the job.
        type: dict
        required: false
        suboptions:
          sysout:
            description:
              - Specify the output class to assign the C(SYSABEND) data set to.
            type: str
            required: false
          omit:
            description:
              - Specifies whether C(SYSABEND) should be excluded from being added to the list of sysout data sets.
            type: bool
            required: false
      sysout:
        description:
          - Overrides the default class to use a custom class for the C(SYSOUT) data set. Alternatively, omit the
            C(SYSOUT) data set from being added to the job.
        type: dict
        required: false
        suboptions:
          sysout:
            description:
              - Specify the output class to assign the C(SYSOUT) data set to.
            type: str
            required: false
          omit:
            description:
              - Specifies whether C(SYSOUT) should be excluded from being added to the list of sysout data sets.
            type: bool
            required: false
      dfhcxrf:
        description:
          - Overrides the default class to use a custom class for the C(DFHCXRF) data set. Alternatively, omit the
            C(DFHCXRF) data set from being added to the job.
        type: dict
        required: false
        suboptions:
          sysout:
            description:
              - Specify the output class to assign the C(DFHCXRF) data set to.
            type: str
            required: false
          omit:
            description:
              - Specifies whether C(DFHCXRF) should be excluded from being added to the list of sysout data sets.
            type: bool
            required: false
      logusr:
        description:
          - Overrides the default class to use a custom class for the C(LOGUSR) data set. Alternatively, omit the
            C(LOGUSR) data set from being added to the job.
        type: dict
        required: false
        suboptions:
          sysout:
            description:
              - Specify the output class to assign the C(LOGUSR) data set to.
            type: str
            required: false
          omit:
            description:
              - Specifies whether C(LOGUSR) should be excluded from being added to the list of sysout data sets.
            type: bool
            required: false
  sit_parameters:
    description:
      - Define the system initalization parameters for the CICS region.
    type: dict
    required: false
    suboptions:
      adi:
        description:
          - The ADI parameter specifies the alternate delay interval in seconds for an alternate CICS® region when you
            are running CICS with XRF.
        required: false
        type: int
      aibridge:
        description:
          - The AIBRIDGE parameter specifies whether the autoinstall user replaceable module (URM) is to be called
            when creating bridge facilities (virtual terminals) used by the 3270 bridge mechanism.
          - Specify this parameter only in the bridge router region.
        required: false
        type: str
        choices:
          - AUTO
          - "YES"
      aicons:
        description:
          - The AICONS parameter specifies whether you want autoinstall support for consoles.
        required: false
        type: str
        choices:
          - "NO"
          - AUTO
          - "YES"
      aiexit:
        description:
          - The AIEXIT parameter specifies the name of the autoinstall user-replaceable program that you want CICS® to
            use when autoinstalling local z/OS® Communications Server terminals, APPC connections, virtual terminals,
            and shipped terminals and connections.
        required: false
        type: str
      aildelay:
        description:
          - The AILDELAY parameter specifies the delay period that elapses after all sessions between CICS® and an
            autoinstalled terminal, APPC device, or APPC system are ended, before the terminal or connection entry is
            deleted.
        required: false
        type: int
      aiqmax:
        description:
          - The AIQMAX parameter specifies the maximum number of z/OS® Communications Server terminals and APPC
            connections that can be queued concurrently for autoinstall, the limit is the sum of installs and deletes.
        required: false
        type: int
      airdelay:
        description:
          - The AIRDELAY parameter specifies the delay period that elapses after an emergency restart before
            autoinstalled terminal and APPC connection entries that are not in session are deleted.
        required: false
        type: int
      akpfreq:
        description:
          - The AKPFREQ parameter specifies the number of write requests to the CICS® system log stream output buffer
            required before CICS writes an activity keypoint.
        required: false
        type: int
      autconn:
        description:
          - The AUTCONN parameter specifies that the reconnection of terminals after an XRF takeover is to be delayed,
            to allow time for manual switching.
        required: false
        type: int
      autodst:
        description:
          - The AUTODST parameter specifies whether CICS is to activate automatic dynamic storage tuning for
            application programs.
        required: false
        type: str
        choices:
          - "NO"
          - "YES"
      autoresettime:
        description:
          - The AUTORESETTIME parameter specifies the action CICS  takes for automatic time changes.
        required: false
        type: str
        choices:
          - IMMEDIATE
          - "NO"
          - "YES"
      auxtr:
        description:
          - The AUXTR parameter specifies whether the auxiliary trace destination is to be activated at system
            initialization.
        required: false
        type: str
        choices:
          - "OFF"
          - "ON"
      auxtrsw:
        description:
          - The AUXTRSW parameter specifies whether you want the auxiliary trace autoswitch facility.
        required: false
        type: str
        choices:
          - "NO"
          - NEXT
          - ALL
      bms:
        description:
          - The BMS system initialization parameter specifies which version of basic mapping support you require in
            CICS.
        required: false
        type: str
      brmaxkeeptime:
        description:
          - The BRMAXKEEPTIME parameter specifies the maximum time (in seconds) that bridge facilities (virtual
            terminals used by the 3270 bridge) are kept if they are not used.
        required: false
        type: int
      cdsasze:
        description:
          - The CDSASZE system initialization parameter specifies the size of the CDSA.
        required: false
        type: int
      certexpirywarn:
        description:
          - The CERTEXPIRYWARN parameter specifies whether CICS® warns about expiring certificates, and if so, how many days ahead of the expiry.
        required: false
        type: str
      chkstrm:
        description:
          - The CHKSTRM parameter specifies that terminal storage-violation checking is to be activated or
            deactivated.
        required: false
        type: str
        choices:
          - CURRENT
          - NONE
      chkstsk:
        description:
          - The CHKSTSK parameter specifies that task storage-violation checking at startup is to be activated or
            deactivated.
        required: false
        type: str
        choices:
          - CURRENT
          - NONE
      cicssvc:
        description:
          - The CICSSVC parameter  specifies the number that you have assigned to the CICS type 3 SVC.
        required: false
        type: int
      cilock:
        description:
          - The CILOCK parameter specifies whether or not the control interval lock of a non-RLS VSAM file is to be
            kept after a successful read-for-update request.
        required: false
        type: str
        choices:
          - "NO"
          - "YES"
      clintcp:
        description:
          - The CLINTCP parameter specifies the default client code page to be used by the DFHCNV data conversion
            table, but only if the CLINTCP parameter in the DFHCNV macro is set to SYSDEF.
        required: false
        type: str
      clsdstp:
        description:
          - The CLSDSTP system initialization parameter specifies the notification required for an EXEC CICS ISSUE
            PASS command.
        required: false
        type: str
        choices:
          - NOTIFY
          - NONOTIFY
      clt:
        description:
          - The CLT parameter specifies the suffix for the command list table (CLT), if this SIT is used by an
            alternate XRF system.
        required: false
        type: str
      cmdprot:
        description:
          - The CMDPROT parameter specifies whether to allow or inhibit CICS validation of start addresses of storage
            referenced as output parameters on EXEC CICS commands.
        required: false
        type: str
        choices:
          - "YES"
          - "NO"
      cmdsec:
        description:
          - The CMDSEC parameter specifies whether or not you want CICS to honor the CMDSEC option specified on a
            transaction's resource definition.
        required: false
        type: str
        choices:
          - ASIS
          - ALWAYS
      confdata:
        description:
          - The CONFDATA parameter specifies whether CICS is to redact sensitive data that might otherwise appear in
            CICS trace entries or in dumps.
        required: false
        type: str
        choices:
          - HIDE
          - SHOW
      conftxt:
        description:
          - The CONFTXT system initialization parameter specifies whether CICS is to prevent z/OS Communications
            Server from tracing user data.
        required: false
        type: str
        choices:
          - "NO"
          - "YES"
      cpsmconn:
        description:
          - The CPSMCONN parameter specifies whether you want CICS to invoke the specified  component during
            initialization of the region.
        required: false
        type: str
        choices:
          - "NO"
          - CMAS
          - LMAS
          - SMSSJ
          - WUI
      crlprofile:
        description:
          - The CRLPROFILE parameter specifies the name of the profile that is used to authorize CICS to access the
            certification revocation lists (CRLs) that are stored in an LDAP server.
        required: false
        type: str
      csdacc:
        description:
          - The CSDACC parameter specifies the type of access to the CSD to be permitted to this CICS region.
        required: false
        type: str
        choices:
          - READWRITE
          - READONLY
      csdbkup:
        description:
          - The CSDBKUP parameter specifies whether or not the CSD is eligible for BWO.
        required: false
        type: str
        choices:
          - STATIC
          - DYNAMIC
      csdbufnd:
        description:
          - The CSDBUFND parameter specifies the number of buffers to be used for CSD data.
        required: false
        type: int
      csdbufni:
        description:
          - The CSDBUFNI parameter specifies the number of buffers to be used for the CSD index.
        required: false
        type: int
      csddisp:
        description:
          - The CSDDISP parameter specifies the disposition of the data set to be allocated to the CSD.
        required: false
        type: str
        choices:
          - OLD
          - SHR
      csddsn:
        description:
          - The CSDDSN parameter specifies the 1-44 character JCL data set name (DSNAME) to be used for the CSD.
        required: false
        type: str
      csdfrlog:
        description:
          - The CSDFRLOG parameter specifies a number that corresponds to the journal name that CICS uses to identify
            the forward recovery log stream for the CSD.
        required: false
        type: int
      csdinteg:
        description:
          - The CSDINTEG parameter specifies the level of read integrity for the CSD if it is accessed in RLS mode.
        required: false
        type: str
        choices:
          - UNCOMMITTED
          - CONSISTENT
          - REPEATABLE
      csdjid:
        description:
          - The CSDJID parameter specifies the journal identifier of the journal that you want CICS to use for
            automatic journaling of file requests against the CSD.
        required: false
        type: str
      csdlsrno:
        description:
          - The CSDLSRNO system initialization parameter specifies whether the CSD is to be associated with a local
            shared resource (LSR) pool.
        required: false
        type: str
      csdrecov:
        description:
          - The CSDRECOVsystem initialization parameter specifies whether the CSD is a recoverable file.
        required: false
        type: str
        choices:
          - NONE
          - ALL
          - BACKOUTONLY
      csdrls:
        description:
          - The CSDRLS system initialization parameter specifies whether CICS is to access the CSD in RLS mode.
        required: false
        type: str
        choices:
          - "NO"
          - "YES"
      csdstrno:
        description:
          - The CSDSTRNO system initialization parameter specifies the number of concurrent requests that can be
            processed against the CSD.
        required: false
        type: int
      cwakey:
        description:
          - The CWAKEY system initialization parameter specifies the storage key for the common work area (CWA) if
            you are operating CICS with storage protection (STGPROT=YES).
        required: false
        type: str
        choices:
          - USER
          - CICS
      dae:
        description:
          - The DAE system initialization parameter specifies the default DAE action when new system dump table
            entries are created.
        required: false
        type: str
        choices:
          - "NO"
          - "YES"
      datform:
        description:
          - The DATFORM system initialization parameter specifies the external date display standard that you want to
            use for CICS date displays.
        required: false
        type: str
        choices:
          - MMDDYY
          - DDMMYY
          - YYMMDD
      db2conn:
        description:
          - The DB2CONN system initialization parameter specifies whether you want CICS to start the  connection
            automatically during initialization.
        required: false
        type: str
        choices:
          - "NO"
          - "YES"
      dbctlcon:
        description:
          - The DBCTLCON system initialization parameter specifies whether you want CICS to start the DBCTL connection
            automatically during initialization.
        required: false
        type: str
        choices:
          - "NO"
          - "YES"
      debugtool:
        description:
          - The DEBUGTOOL system initialization parameter specifies whether you want to use debugging profiles to
            select the programs that will run under the control of a debugging tool.
        required: false
        type: str
        choices:
          - "NO"
          - "YES"
      dfltuser:
        description:
          - The DFLTUSER system initialization parameter specifies the RACF userid of the default user; that is, the
            user whose security attributes are used to protect CICS resources in the absence of other, more specific,
            user identification.
        required: false
        type: str
      dip:
        description:
          - The DIP system initialization parameter specifies whether the batch data interchange program, DFHDIP, is
            to be included.
        required: false
        type: str
        choices:
          - "NO"
          - "YES"
      dismacp:
        description:
          - The DISMACP system initialization parameter specifies whether CICS is to disable any transaction that
            terminates abnormally with an ASRD or ASRE abend.
        required: false
        type: str
        choices:
          - "NO"
          - "YES"
      doccodepage:
        description:
          - The DOCCODEPAGE system initialization parameter specifies the default host code page to be used by the
            document domain.
        required: false
        type: str
      dsalim:
        description:
          - The DSALIM system initialization parameter specifies the upper limit of the total amount of storage within
            which CICS® can allocate the individual dynamic storage areas (DSAs) that reside in 24-bit storage.
        required: false
        type: str
      dshipidl:
        description:
          - The DSHIPIDL system initialization parameter specifies the minimum time, in hours, minutes, and seconds,
            that an inactive shipped terminal definition must remain installed in this region.
        required: false
        type: int
      dshipint:
        description:
          - The DSHIPINT system initialization parameter specifies the interval between invocations of the timeout
            delete mechanism.
        required: false
        type: int
      dsrtpgm:
        description:
          - The DSRTPGM system initialization parameter specifies the name of a distributed routing program. The
            distributed routing program must be specified in the DSRTPGM parameter for all routing and potential
            target regions.
        required: false
        type: str
      dtrpgm:
        description:
          - The DTRPGM system initialization parameter specifies the name of a dynamic routing program.
        required: false
        type: str
      dtrtran:
        description:
          - The DTRTRAN system initialization parameter specifies the name of the transaction definition that you want
            CICS to use for dynamic transaction routing.
        required: false
        type: str
      dump:
        description:
          - The DUMP system initialization parameter specifies whether the CICS dump domain is to take SDUMPs.
        required: false
        type: str
        choices:
          - "YES"
          - "NO"
          - TABLEONLY
      dumpds:
        description:
          - The DUMPDS system initialization parameter specifies the transaction dump data set that is to be opened
            during CICS initialization.
        required: false
        type: str
        choices:
          - AUTO
          - A
          - B
      dumpsw:
        description:
          - The DUMPSW system initialization parameter specifies whether you want CICS to switch automatically to the
            next dump data set when the first is full.
        required: false
        type: str
        choices:
          - "NO"
          - NEXT
          - ALL
      duretry:
        description:
          - The DURETRY system initialization parameter specifies, in seconds, the total time that CICS is to continue
            trying to obtain a system dump using the SDUMP macro.
        required: false
        type: int
      ecdsasze:
        description:
          - The ECDSASZE system initialization parameter specifies the size of the ECDSA.
        required: false
        type: str
      edsalim:
        description:
          - The EDSALIM system initialization parameter specifies the upper limit of the total amount of storage
            within which CICS® can allocate the individual extended dynamic storage areas (ExxDSAs) that reside in
            31-bit (above-the-line) storage; that is, above 16 MB but below 2 GB.
        required: false
        type: str
      eodi:
        description:
          - The EODI system initialization parameter specifies the end-of-data indicator for input from sequential
            devices.
        required: false
        type: str
      erdsasze:
        description:
          - The ERDSASZE system initialization parameter specifies the size of the ERDSA.
        required: false
        type: str
      esdsasze:
        description:
          - The ESDSASZE system initialization parameter specifies the size of the ESDSA.
        required: false
        type: str
      esmexits:
        description:
          - The ESMEXITS system initialization parameter specifies whether installation data is to be passed through
            the RACROUTE interface to the external security manager (ESM) for use in exits written for the ESM.
        required: false
        type: str
        choices:
          - NOINSTLN
          - INSTLN
      eudsasze:
        description:
          - The EUDSASZE system initialization parameter specifies the size of the EUDSA.
        required: false
        type: str
      fcqronly:
        description:
          - The FCQRONLY system initialization parameter specifies whether you want CICS to force all file control
            requests to run under the CICS QR TCB. This parameter applies to file control requests that access VSAM
            RLS files and local VSAM LSR files.
        required: false
        type: str
        choices:
          - "NO"
          - "YES"
      fct:
        description:
          - The FCT system initialization parameter specifies the suffix of the file control table to be used.
        required: false
        type: str
      fepi:
        description:
          - The FEPI system initialization parameter specifies whether or not you want to use the Front End
            Programming Interface feature (FEPI).
        required: false
        type: str
        choices:
          - "NO"
          - "YES"
      fldsep:
        description:
          - The FLDSEP system initialization parameter specifies 'ON'e through four field-separator characters, each
            of which indicates end of field in the terminal input data.
        required: false
        type: str
      fldstrt:
        description:
          - The FLDSTRT system initialization parameter specifies a single character to be the field-name-start
            character for free-form input for built-in functions.
        required: false
        type: str
      forceqr:
        description:
          - The FORCEQR system initialization parameter specifies whether you want CICS to force all CICS API user
            application programs that are specified as threadsafe to run under the CICS QR TCB, as if they were
            specified as quasi-reentrant programs.
        required: false
        type: str
        choices:
          - "NO"
          - "YES"
      fsstaff:
        description:
          - The FSSTAFF system initialization parameter prevents transactions initiated by function-shipped EXEC CICS
            START requests being started against incorrect terminals.
        required: false
        type: str
        choices:
          - "YES"
          - "NO"
      ftimeout:
        description:
          - The FTIMEOUT system initialization parameter specifies a timeout interval for requests made on files that
            are opened in RLS mode.
        required: false
        type: int
      gmtext:
        description:
          - The GMTEXT system initialization parameter specifies whether the default logon message text (WELCOME TO
            CICS) or your own message text is to be displayed on the screen.
        required: false
        type: str
      gmtran:
        description:
          - The GMTRAN system initialization parameter specifies the ID of a transaction.
        required: false
        type: str
      gntran:
        description:
          - The GNTRAN system initialization parameter specifies the transaction that you want CICS to invoke when a
            user's terminal-timeout period expires, and instructs CICS whether to keep a pseudo-conversation in use at
            a terminal that is the subject of a timeout sign-off.
        required: false
        type: str
      grname:
        description:
          - The GRNAME system initialization parameter specifies the z/OS Communications Server generic resource name,
            as 1 through 8 characters, under which a group of CICS terminal-owning regions in a CICSplex register to
            z/OS Communications Server.
        required: false
        type: str
      grplist:
        description:
          - The GRPLIST system initialization parameter specifies the names of up to four lists of resource definition
            groups on the CICS system definition file (CSD). The resource definitions in all the groups in the
            specified lists are loaded during initialization when CICS performs a cold start. If a warm or emergency
            start is performed, the resource definitions are derived from the global catalog, and the GRPLIST
            parameter is ignored.
        required: false
        type: str
      gtftr:
        description:
          - The GTFTR system initialization parameter specifies whether CICS can use the MVS generalized trace
            facility (GTF) as a destination for trace data.
        required: false
        type: str
        choices:
          - "OFF"
          - "ON"
      hpo:
        description:
          - The HPO system initialization parameter specifies whether you want to use the z/OS Communications Server
            authorized path feature of the high performance option (HPO).
        required: false
        type: str
        choices:
          - "NO"
          - "YES"
      httpserverhdr:
        description:
          - The HTTPSERVERHDR system initialization parameter specifies the value (up to 64 characters) that CICS sets
            in the server header of HTTP responses.
        required: false
        type: str
      httpusragenthdr:
        description:
          - The HTTPUSRAGENTHDR system initialization parameter specifies the value (up to 64 characters) that CICS
            sets in the user-agent header of HTTP requests.
        required: false
        type: str
      icp:
        description:
          - The ICP system initialization parameter specifies that you want to perform a cold start for interval
            control program.
        required: false
        type: str
        choices:
          - COLD
      icv:
        description:
          - The ICV system initialization parameter specifies the region exit time interval in milliseconds.
        required: false
        type: int
      icvr:
        description:
          - The ICVR system initialization parameter specifies the default runaway task time interval in milliseconds
            as a decimal number.
        required: false
        type: int
      icvtsd:
        description:
          - The ICVTSD system initialization parameter specifies the terminal scan delay value.
        required: false
        type: int
      infocenter:
        description:
          - The INFOCENTER system initialization parameter specifies the location of the online . If you add this
            parameter to the Web User Interface (WUI) CICS startup JCL, a link labeled Information Center is displayed
            on WUI views and menus. If you do not code this parameter, CICS does not construct links to IBM
            Documentation.
        required: false
        type: str
      initparm:
        description:
          - The INITPARM system initialization parameter specifies parameters that are to be passed to application
            programs that use the ASSIGN INITPARM command.
        required: false
        type: str
      intrdrjobuser:
        description:
          - The INTRDRJOBUSER system initialization parameter instructs whether to use the task user ID or the
            CICS® region user ID as the job user ID for a JOB card that is submitted, without a USER parameter,
            by using SPOOLOPEN with USERID("INTRDR") and SPOOLWRITE. The default is the task user ID unless set
            otherwise by INTRDRJOBUSER.
        required: false
        type: str
        choices:
          - "TASK"
          - "REGION"
      inttr:
        description:
          - The INTTR system initialization parameter specifies whether the internal CICS trace destination is to be
            activated at system initialization.
        required: false
        type: str
        choices:
          - "ON"
          - "OFF"
      ircstrt:
        description:
          - The IRCSTRT system initialization parameter specifies whether IRC is to be started up at system
            initialization.
        required: false
        type: str
        choices:
          - "NO"
          - "YES"
      isc:
        description:
          - The ISC system initialization parameter specifies whether the CICS programs required for multiregion
            operation (MRO) and  are to be included.
        required: false
        type: str
        choices:
          - "NO"
          - "YES"
      jesdi:
        description:
          - The JESDI system initialization parameter specifies, in a SIT for an alternate XRF system, the JES delay
            interval.
        required: false
        type: int
      jvmprofiledir:
        description:
          - The JVMPROFILEDIR system initialization parameter specifies the name (up to 240 characters long) of a z/OS
            UNIX directory that contains the JVM profiles
            for CICS. CICS searches this directory for the profiles it needs to configure JVMs.
        required: false
        type: str
      kerberosuser:
        description:
          - The KERBEROSUSER system initialization parameter specifies the user ID that is associated with the
            Kerberos service principal for the CICS region.
        required: false
        type: str
      keyring:
        description:
          - The KEYRING system initialization parameter specifies the fully qualified name of the key ring, within the
            RACF database, that contains the keys and X.509 certificates used by CICS support for the Secure Sockets
            Layer (SSL) and for web services security. The region user ID that will use the key ring must either own
            the key ring or have the authority to use the key ring if it is owned by a different region user ID. You
            can create an initial key ring with the DFH$RING exec in .CICS.SDFHSAMP.
        required: false
        type: str
      lgdfint:
        description:
          - The LGDFINT system initialization parameter specifies the log defer interval to be used by CICS® log
            manager when determining how long to delay a forced journal write request before invoking the MVS™ system
            logger.
        required: false
        type: int
      lgnmsg:
        description:
          - The LGNMSG system initialization parameter specifies whether z/OS Communications Server logon data is
            to be made available to an application program.
        required: false
        type: str
        choices:
          - "NO"
          - "YES"
      llacopy:
        description:
          - The LLACOPY system initialization parameter specifies the situations where CICS uses either the LLACOPY
            macro or the BLDL macro when locating modules in the DFHRPL or dynamic LIBRARY concatenation.
        required: false
        type: str
        choices:
          - "YES"
          - "NO"
          - NEWCOPY
      localccsid:
        description:
          - The LOCALCCSID system initialization parameter specifies the default CCSID for the local region.
        required: false
        type: int
      lpa:
        description:
          - The LPA system initialization parameter specifies whether CICS and user modules can be used from the link
            pack areas.
        required: false
        type: str
        choices:
          - "NO"
          - "YES"
      maxopentcbs:
        description:
          - The MAXOPENTCBS system initialization parameter specifies the maximum number, in the range 32 through
            4032, of open task control blocks (open TCBs) CICS® can create in the pool of L8 and L9 mode TCBs.
        required: false
        type: int
      maxsockets:
        description:
          - The MAXSOCKETS system initialization parameter specifies the maximum number of IP sockets that can be
            managed by the CICS sockets domain.
        required: false
        type: int
      maxssltcbs:
        description:
          - The MAXSSLTCBS system initialization parameter specifies the maximum number of S8 TCBs that can run in the
            SSL pool.
        required: false
        type: int
      maxxptcbs:
        description:
          - The MAXXPTCBS system initialization parameter specifies the maximum number, in the range 1 through 2000,
            of open X8 and X9 TCBs that can exist concurrently in the CICS region.
        required: false
        type: int
      mct:
        description:
          - The MCT system initialization parameter specifies the monitoring control table suffix.
        required: false
        type: str
      mintlslevel:
        description:
          - The MINTLSLEVEL system initialization parameter specifies the minimum TLS protocol that CICS uses for
            secure TCP/IP connections.
        required: false
        type: str
        choices:
          - TLS11
          - TLS12
          - TLS13
      mn:
        description:
          - The MN system initialization parameter specifies whether monitoring is to be switched 'ON' or 'OFF' at
            initialization.
        required: false
        type: str
        choices:
          - "OFF"
          - "ON"
      mnconv:
        description:
          - The MNCONV system initialization parameter specifies whether conversational tasks have separate
            performance class records produced for each pair of terminal control I/O requests.
        required: false
        type: str
        choices:
          - "NO"
          - "YES"
      mnexc:
        description:
          - The MNEXC system initialization parameter specifies whether the monitoring exception class is to be made
            active during initialization.
        required: false
        type: str
        choices:
          - "OFF"
          - "ON"
      mnfreq:
        description:
          - The MNFREQ system initialization parameter specifies the interval for which CICS automatically produces a
            transaction performance class record for any long-running transaction.
        required: false
        type: int
      mnidn:
        description:
          - The MNIDN system initialization parameter specifies whether the monitoring identity class is to be made
            active during CICS initialization.
        required: false
        type: str
        choices:
          - "OFF"
          - "ON"
      mnper:
        description:
          - The MNPER system initialization parameter specifies whether the monitoring performance class is to be made
            active during CICS initialization.
        required: false
        type: str
        choices:
          - "OFF"
          - "ON"
      mnres:
        description:
          - The MNRES system initialization parameter specifies whether transaction resource monitoring is to be made
            active during CICS initialization.
        required: false
        type: str
        choices:
          - "OFF"
          - "ON"
      mnsync:
        description:
          - The MNSYNC system initialization parameter specifies whether you want CICS to produce a transaction
            performance class record when a transaction takes an implicit or explicit syncpoint (unit-of-work).
        required: false
        type: str
        choices:
          - "NO"
          - "YES"
      mntime:
        description:
          - The MNTIME system initialization parameter specifies whether you want the time stamp fields in the
            performance class monitoring data to be returned to an application using the EXEC CICS COLLECT STATISTICS
            MONITOR(taskno) command in either GMT or local time.
        required: false
        type: str
        choices:
          - GMT
          - LOCAL
      mqconn:
        description:
          - The MQCONN system initialization parameter specifies whether you want CICS to start a connection to
            automatically during initialization.
        required: false
        type: str
        choices:
          - "NO"
          - "YES"
      mrobtch:
        description:
          - The MROBTCH system initialization parameter specifies the number of events that must occur before CICS is
            posted for dispatch because of the batching mechanism.
        required: false
        type: int
      mrofse:
        description:
          - The MROFSE system initialization parameter specifies whether you want to extend the lifetime of the
            long-running mirror to keep it allocated until the end of the task rather than after a user syncpoint for
            function shipping applications.
        required: false
        type: str
        choices:
          - "NO"
          - "YES"
      mrolrm:
        description:
          - The MROLRM system initialization parameter specifies whether you want to establish an MRO long-running
            mirror task.
        required: false
        type: str
        choices:
          - "NO"
          - "YES"
      msgcase:
        description:
          - The MSGCASE system initialization parameter specifies how you want the message domains to display mixed
            case messages.
        required: false
        type: str
        choices:
          - MIXED
          - UPPER
      msglvl:
        description:
          - The MSGLVL system initialization parameter specifies the message level that controls the generation of
            messages to the console and JES message log.
        required: false
        type: int
        choices:
          - "1"
          - "0"
      mxt:
        description:
          - The MXT system initialization parameter specifies the maximum number, in the range 10 through 2000, of
            user tasks that can exist in a CICS system at the same time. The MXT value does not include CICS system
            tasks.
        required: false
        type: int
      natlang:
        description:
          - The NATLANG system initialization parameter specifies the single-character code for the language to be
            supported in this CICS run.
        required: false
        type: str
        choices:
          - E
          - C
          - K
      ncpldft:
        description:
          - The NCPLDFT system initialization parameter specifies the name of the default named counter pool to be
            used by the CICS region 'ON' calls it makes to a named counter server.
        required: false
        type: str
      newsit:
        description:
          - The NEWSIT system initialization parameter specifies whether CICS is to load the specified SIT, and
            enforce the use of all system initialization parameters, modified by any system initialization parameters
            provided by PARM, SYSIN, or the system console, even in a warm start.
        required: false
        type: str
        choices:
          - "NO"
          - "YES"
      nistsp800131a:
        description:
          - The NISTSP800131A system initialization parameter specifies whether the CICS region is to check for
            conformance to the NIST SP800-131A standard.
        required: false
        type: str
        choices:
          - NOCHECK
          - CHECK
      nonrlsrecov:
        description:
          - The NONRLSRECOV system initialization parameter specifies whether VSAM catalog recovery options should
            override those specified on the CICS FILE resource definition for all non-RLS files. Default behavior,
            with NONRLSRECOV=VSAMCAT, will take recovery attributes from the catalog if they are present, and
            from the file definition otherwise. RLS files must always specify recovery options on the catalog.
        required: false
        type: str
        choices:
          - VSAMCAT
          - FILEDEF
      nqrnl:
        description:
          - The NQRNL system initialization parameter controls resource name list (RNL) processing by z/OS global
            resource serialization, which can cause the scope value of a resource to change. CICS uses z/OS global
            resource serialization to provide sysplex-wide protection of application resources.
        required: false
        type: str
        choices:
          - "NO"
          - "YES"
      offsite:
        description:
          - The 'OFF'SITE system initialization parameter specifies whether CICS is to restart in 'OFF'-site recovery
            mode; that is, a restart is taking place at a remote site.
        required: false
        type: str
        choices:
          - "NO"
          - "YES"
      opertim:
        description:
          - The OPERTIM system initialization parameter specifies the write-to-operator timeout value, in the range 0
            through 86400 seconds (24 hours).
        required: false
        type: int
      opndlim:
        description:
          - The OPNDLIM system initialization parameter specifies the destination and close destination request limit.
        required: false
        type: int
      parmerr:
        description:
          - The PARMERR system initialization parameter specifies what action you want to follow if CICS detects
            incorrect system initialization parameter overrides during initialization.
        required: false
        type: str
        choices:
          - INTERACT
          - IGNORE
          - ABEND
      pdi:
        description:
          - The PDI system initialization parameter specifies the XRF primary delay interval, in seconds, in a SIT for
            an active CICS region.
        required: false
        type: int
      pdir:
        description:
          - The PDIR system initialization parameter specifies a suffix for the PDIR list.
        required: false
        type: str
      pgaictlg:
        description:
          - The PGAICTLG system initialization parameter specifies whether autoinstalled program definitions should be
            cataloged.
        required: false
        type: str
        choices:
          - MODIFY
          - NONE
          - ALL
      pgaiexit:
        description:
          - The PGAIEXIT system initialization parameter specifies the name of the program autoinstall exit program.
        required: false
        type: str
      pgaipgm:
        description:
          - The PGAIPGM system initialization parameter specifies the state of the program autoinstall function at
            initialization.
        required: false
        type: str
        choices:
          - INACTIVE
          - ACTIVE
      pgchain:
        description:
          - The PGCHAIN system initialization parameter specifies the character string that is identified by terminal
            control as a BMS terminal page-chaining command.
        required: false
        type: str
      pgcopy:
        description:
          - The PGCOPY system initialization parameter specifies the character string that is identified by terminal
            control as a BMS command to copy output from one terminal to another.
        required: false
        type: str
      pgpurge:
        description:
          - The PGPURGE system initialization parameter specifies the character string that is identified by terminal
            control as a BMS terminal page-purge command.
        required: false
        type: str
      pgret:
        description:
          - The PGRET system initialization parameter specifies the character string that is recognized by terminal
            control as a BMS terminal page-retrieval command.
        required: false
        type: str
      pltpi:
        description:
          - The PLTPI system initialization parameter specifies the suffix for, or the full name of, a program list
            table that contains a list of programs to be run in the final stages of system initialization.
        required: false
        type: str
      pltpisec:
        description:
          - The PLTPISEC system initialization parameter specifies whether you want CICS to perform command security
            or resource security checking for PLT programs during CICS initialization.
        required: false
        type: str
        choices:
          - NONE
          - CMDSEC
          - RESSEC
          - ALL
      pltpiusr:
        description:
          - The PLTPIUSR system initialization parameter specifies the user ID that CICS uses for security checking
            for PLT programs that run during CICS initialization.
        required: false
        type: str
      pltsd:
        description:
          - The PLTSD system initialization parameter specifies the suffix for, or full name of, a program list table
            that contains a list of programs to be run during system termination.
        required: false
        type: str
      prgdlay:
        description:
          - The PRGDLAY system initialization parameter specifies the BMS purge delay time interval that is added t
            the specified delivery time to determine when a message is to be considered undeliverable and therefore
            purged.
        required: false
        type: int
      print:
        description:
          - The PRINT system initialization parameter specifies the method of requesting printout of the contents of
            a 3270 screen.
        required: false
        type: str
        choices:
          - "NO"
          - "YES"
          - PA1
          - PA2
          - PA3
      prtyage:
        description:
          - The PRTYAGE system initialization parameter specifies the number of milliseconds to be used in the
            priority aging algorithm that is used to increment the priority of a task.
        required: false
        type: int
      prvmod:
        description:
          - The PRVMOD system initialization parameter specifies the names of those modules that are not to be used
            from the LPA.
        required: false
        type: str
      psbchk:
        description:
          - The PSBCHK system initialization parameter specifies whether CICS is to perform PSB authorization checks
            for remote terminal users who use transaction routing to initiate a transaction in this CICS region to
            access an attached IMS system.
        required: false
        type: str
        choices:
          - "NO"
          - "YES"
      psdint:
        description:
          - The PSDINT system initialization parameter specifies the persistent session delay interval, which states
            if, and for how long, z/OS CommunicationsServer holds sessions in a recovery-pending state.
        required: false
        type: int
      pstype:
        description:
          - The PSTYPE system initialization parameter specifies whether CICS uses z/OS Communications Server
            single-node persistent sessions (SNPS), multinode persistent sessions (MNPS), or does not use z/OS
            Communications Server persistent sessions support (NOPS).
        required: false
        type: str
        choices:
          - SNPS
          - MNPS
          - NOPS
      pvdelay:
        description:
          - The PVDELAY system initialization parameter specifies the persistent verification delay as a value in the
            range 0 through 10080 minutes (up to 7 days).
        required: false
        type: int
      quiestim:
        description:
          - The QUIESTIM system initialization parameter specifies a timeout value for data set quiesce requests.
        required: false
        type: int
      racfsync:
        description:
          - The RACFSYNC system initialization parameter specifies whether CICS listens for type 71 ENF events and
            refreshes user security.
        required: false
        type: str
        choices:
          - "YES"
          - "NO"
          - "CPSM"
      ramax:
        description:
          - The RAMAX system initialization parameter specifies the size in bytes of the I/O area allocated for each
            RECEIVE ANY issued by CICS, in the range 0 through 32767 bytes.
        required: false
        type: int
      rapool:
        description:
          - The RAPOOL system initialization parameter specifies the number of concurrent receive-any requests that
            CICS is to process from the z/OS Communications Server for SNA.
        required: false
        type: str
      rdsasze:
        description:
          - The RDSASZE system initialization parameter specifies the size of the RDSA.
        required: false
        type: str
      rentpgm:
        description:
          - The RENTPGM system initialization parameter specifies whether you want CICS to allocate the read-only DSAs
            from read-only key-0 protected storage.
        required: false
        type: str
        choices:
          - PROTECT
          - NOPROTECT
      resoverrides:
        description:
          - The RESOVERRIDES system initialization parameter specifies the 1-64 character name of the resource
            overrides file. For more information, see .
        required: false
        type: str
      resp:
        description:
          - The RESP system initialization parameter specifies the type of request that CICS terminal control receives
            from logical units.
        required: false
        type: str
        choices:
          - FME
          - RRN
      ressec:
        description:
          - The RESSEC system initialization parameter specifies whether you want CICS to honor the RESSEC option
            specified on a transaction's resource definition.
        required: false
        type: str
        choices:
          - ASIS
          - ALWAYS
      rls:
        description:
          - The RLS system initialization parameter specifies whether CICS is to support VSAM record-level sharing
            (RLS).
        required: false
        type: str
        choices:
          - "NO"
          - "YES"
      rlstolsr:
        description:
          - The RLSTOLSR system initialization parameter specifies whether CICS is to include files that are to be
            opened in RLS mode when calculating the number
            of buffers, strings, and other resources for an LSR pool.
        required: false
        type: str
        choices:
          - "NO"
          - "YES"
      rmtran:
        description:
          - The RMTRAN system initialization parameter specifies the name of the transaction that you want an
            alternate CICS to initiate when logged-on class 1 terminals, which are defined with the attribute
            RECOVNOTIFY(TRANSACTION) specified, are switched following a takeover.
        required: false
        type: str
      rrms:
        description:
          - The RRMS system initialization parameter specifies whether CICS is to register as a resource manager with
            recoverable resource management services (RRMS).
        required: false
        type: str
        choices:
          - "NO"
          - "YES"
      rst:
        description:
          - The RST system initialization parameter specifies a recoverable service table suffix.
        required: false
        type: str
      rstsignoff:
        description:
          - The RSTSIGNOFF system initialization parameter specifies whether all users signed-on to the active CICS
            region are to remain signed-on following a persistent sessions restart or an XRF takeover.
        required: false
        type: str
        choices:
          - NOFORCE
          - FORCE
      rstsigntime:
        description:
          - The RSTSIGNTIME parameter specifies the timeout delay interval for signon retention during a persistent
            sessions restart or an XRF takeover.
        required: false
        type: int
      ruwapool:
        description:
          - The RUWAPOOL parameter specifies the option for allocating a storage pool the first time a program invoked
            by Language Environment runs in a task.
        required: false
        type: str
        choices:
          - "NO"
          - "YES"
      sdsasze:
        description:
          - The SDSASZE system initialization parameter specifies the size of the SDSA.
        required: false
        type: str
      sdtran:
        description:
          - The SDTRAN system initialization parameter specifies the name of the shutdown transaction to be started at
            the beginning of normal and immediate shutdown.
        required: false
        type: str
      sec:
        description:
          - The SEC system initialization parameter specifies what level of external security you want CICS to use.
        required: false
        type: str
        choices:
          - "YES"
          - "NO"
      secprfx:
        description:
          - The SECPRFX system initialization parameter specifies whether CICS prefixes the resource names in any
            authorization requests to RACF.
        required: false
        type: str
      sit:
        description:
          - The SIT system initialization parameter specifies the suffix, if any, of the system initialization table
            that you want CICS to load at the start of initialization.
        required: false
        type: str
      skrxxxx:
        description:
          - The SKRxxxx system initialization parameter specifies that a single-keystroke-retrieval operation is
            required.
          - 'Provide a dictionary with the key specifying a key on the 3270 keyboard and the value identifying a page
            retrieval command that the 3270 key represents. For example, PF20: PGPURGE'
          - The valid keys you can specify are PA1 through PA3, and PF1 through PF24.
        required: false
        type: dict
      snpreset:
        description:
          - The SNPRESET system initialization parameter specifies whether preset userid terminals share a single
            access control environment element (ACEE) that is associated with the userid, or a unique ACEE for every
            terminal.
        required: false
        type: str
        choices:
          - UNIQUE
          - SHARED
      snscope:
        description:
          - The SNSCOPE system initialization parameter specifies whether a userid can be signed on to CICS more than
            once, within the scope of a single CICS region, a single MVS image, and a sysplex.
        required: false
        type: str
        choices:
          - NONE
          - CICS
          - MVSIMAGE
          - SYSPLEX
      sotuning:
        description:
          - The SOTUNING system initialization parameter specifies whether performance tuning for HTTP connections
            will occur to protect CICS from unconstrained resource demand.
        required: false
        type: str
        choices:
          - "YES"
          - "520"
      spctr:
        description:
          - The SPCTR system initialization parameter specifies the level of special tracing required for CICS as a
            whole.
        required: false
        type: str
      spctrxx:
        description:
          - The SPCTRxx system initialization parameter specifies the level of special tracing activated for a particular CICS
            component. When you enable special tracing for a transaction, a terminal, or both, the trace points of this component
            at the specified trace level are eligible to make trace calls at any given point in the process of a special tracing task.
          - 'Provide a dictionary with the key specifying a two-letter code that represents a component and the value specifying the
            trace level. For example: AP=1-2'
          - You can provide several dictionaries to specify the level of special tracing for several components. Each component
            is defined by one dictionary.
          - For information about CICS components and their respetive two-letter code, see
            L(Component names and abbreviations,https://www.ibm.com/docs/en/cics-ts/6.1?topic=component-names-abbreviations).
        required: false
        type: dict
      spool:
        description:
          - The SPOOL system initialization parameter specifies whether the system spooling interface is required.
        required: false
        type: str
        choices:
          - "NO"
          - "YES"
      srbsvc:
        description:
          - The SRBSVC system initialization parameter specifies the number that you have assigned to the CICS type 6
            SVC.
        required: false
        type: int
      srt:
        description:
          - The SRT system initialization parameter specifies the system recovery table suffix.
        required: false
        type: str
      srvercp:
        description:
          - The SRVERCP system initialization parameter specifies the default server code page to be used by the
            DFHCNV data conversion table but only if the SRVERCP parameter in the DFHCNV macro is set to SYSDEF.
        required: false
        type: str
      sslcache:
        description:
          - The SSLCACHE system initialization parameter specifies whether session IDs for SSL sessions are to be
            cached locally or at sysplex level for reuse by the CICS® region. The SSL cache allows CICS to perform
            abbreviated handshakes with clients that it has previously authenticated.
        required: false
        type: str
        choices:
          - CICS
          - SYSPLEX
      ssldelay:
        description:
          - The SSLDELAY system initialization parameter specifies the length of time in seconds for which CICS
            retains session ids for secure socket connections.
        required: false
        type: int
      start:
        description:
          - The START system initialization parameter specifies the type of start for the system initialization
            program.
        required: false
        type: str
        choices:
          - AUTO
          - INITIAL
          - COLD
          - STANDBY
          - (INITIAL, ALL)
          - (AUTO, ALL)
          - (COLD, ALL)
          - (STANDBY, ALL)
      starter:
        description:
          - The STARTER system initialization parameter specifies whether the generation of starter system modules
            (with $ and # suffixes) is permitted, and various MNOTES are suppressed.
        required: false
        type: str
        choices:
          - "YES"
          - "NO"
      stateod:
        description:
          - The STATEOD system initialization parameter specifies the end-of-day time in the format hhmmss.
        required: false
        type: int
      statint:
        description:
          - The STATINT system initialization parameter specifies the recording interval for system statistics in the
            format hhmmss.
        required: false
        type: int
      statrcd:
        description:
          - The STATRCD system initialization parameter specifies the interval statistics recording status at CICS
            initialization.
        required: false
        type: str
        choices:
          - "OFF"
          - "ON"
      stgprot:
        description:
          - The STGPROT system initialization parameter specifies whether you want storage protection to operate in
            the CICS region.
        required: false
        type: str
        choices:
          - "YES"
          - "NO"
      stgrcvy:
        description:
          - The STGRCVY system initialization parameter specifies whether CICS should try to recover from a storage
            violation.
        required: false
        type: str
        choices:
          - "NO"
          - "YES"
      stntr:
        description:
          - The STNTR system initialization parameter specifies the level of standard tracing required for CICS as a
            whole.
        required: false
        type: str
      stntrxx:
        description:
          - The STNTRxx system initialization parameter specifies the level of standard tracing for a particular CICS component.
          - 'Provide a dictionary with the key specifying a two-letter code that represents a component and the value specifying the
            trace level. For example: AP=1-2'
          - You can provide several dictionaries to specify the level of standard tracing for several components. Each component
            is defined by one dictionary. For components that are not defined here, their standard tracing levels are determined
            by STNTR.
          - For information about CICS components and their respective two-letter code, see
            L(Component names and abbreviations,https://www.ibm.com/docs/en/cics-ts/6.1?topic=component-names-abbreviations).
        required: false
        type: dict
      subtsks:
        description:
          - The SUBTSKS system initialization parameter specifies the number of task control blocks (TCBs) you want
            CICS to use for running tasks in concurrent mode.
        required: false
        type: int
        choices:
          - "0"
          - "1"
      suffix:
        description:
          - The SUFFIX system initialization parameter specifies the last two characters of the name of this system
            initialization table.
        required: false
        type: str
      sysidnt:
        description:
          - The SYSIDNT system initialization parameter specifies a 1- to 4-character name that is known only to your
            CICS region.
        required: false
        type: str
      systr:
        description:
          - The SYSTR system initialization parameter specifies the setting of the main system trace flag.
        required: false
        type: str
        choices:
          - "ON"
          - "OFF"
      sydumax:
        description:
          - The SYDUMAX system initialization parameter specifies the limit on the number of system dumps that can be
            taken per dump table entry.
        required: false
        type: int
      takeovr:
        description:
          - The TAKEOVR system initialization parameter specifies the action to be taken by the alternate CICS region,
            following the apparent loss of the surveillance signal in the active CICS region.
        required: false
        type: str
        choices:
          - MANUAL
          - AUTO
          - COMMAND
      tbexits:
        description:
          - The TBEXITS system initialization parameter specifies the names of your backout exit programs for use
            during emergency restart backout processing.
        required: false
        type: str
      tcp:
        description:
          - The TCP system initialization parameter specifies whether the pregenerated non-z/OS Communications Server
            terminal control program, DFHTCP, is to be included.
        required: false
        type: str
        choices:
          - "NO"
          - "YES"
      tcpip:
        description:
          - The TCPIP system initialization parameter specifies whether CICS TCP/IP services are to be activated at
            CICS startup.
        required: false
        type: str
        choices:
          - "YES"
          - "NO"
      tcsactn:
        description:
          - The TCSACTN system initialization parameter specifies the required action that CICS terminal control
            should take if the terminal control shutdown wait threshold expires.
        required: false
        type: str
        choices:
          - NONE
          - UNBIND
          - FORCE
      tcswait:
        description:
          - The TCSWAIT system initialization parameter specifies the required CICS terminal control shutdown wait
            threshold.
        required: false
        type: str
      tct:
        description:
          - The TCT system initialization parameter specifies which terminal control table, if any, is to be loaded.
        required: false
        type: str
      tctuakey:
        description:
          - The TCTUAKEY system initialization parameter specifies the storage key for the terminal control table user
            areas (TCTUAs) if you are operating CICS with storage protection (STGPROT=YES).
        required: false
        type: str
        choices:
          - USER
          - CICS
      tctualoc:
        description:
          - The TCTUALOC system initialization parameter specifies where terminal user areas (TCTUAs) are to be
            stored.
        required: false
        type: str
        choices:
          - BELOW
          - ANY
      td:
        description:
          - The TD system initialization parameter specifies the number of VSAM buffers and strings to be used for
            intrapartition transient data (TD).
        required: false
        type: str
      tdintra:
        description:
          - The TDINTRA system initialization parameter specifies whether CICS is to initialize with empty
            intrapartition TD queues.
        required: false
        type: str
        choices:
          - NOEMPTY
          - EMPTY
      traniso:
        description:
          - The TRANISO system initialization parameter specifies, together with the STGPROT system initialization
            parameter, whether you want transaction isolation in the CICS region.
        required: false
        type: str
        choices:
          - "NO"
          - "YES"
      trap:
        description:
          - The TRAP system initialization parameter specifies whether the FE global trap exit is to be activated at
            system initialization.
        required: false
        type: str
        choices:
          - "OFF"
          - "ON"
      trdumax:
        description:
          - The TRDUMAX system initialization parameter specifies the limit on the number of transaction dumps that
            may be taken per Dump Table entry.
        required: false
        type: int
      trtabsz:
        description:
          - The TRTABSZ system initialization parameter specifies the size, in kilobytes, of the internal trace table.
        required: false
        type: int
      trtransz:
        description:
          - The TRTRANSZ system initialization parameter specifies the size, in kilobytes, of the transaction dump
            trace table.
        required: false
        type: int
      trtranty:
        description:
          - The TRTRANTY system initialization parameter specifies which trace entries should be copied from the
            internal trace table to the transaction dump trace table.
        required: false
        type: str
        choices:
          - TRAN
          - ALL
      ts:
        description:
          - The TS system initialization parameter specifies whether you want to perform a cold start for temporary
            storage, as well as the number of VSAM buffers and strings to be used for auxiliary temporary storage.
        required: false
        type: str
      tsmainlimit:
        description:
          - The TSMAINLIMIT system initialization parameter specifies a limit for the storage that is available for
            main temporary storage queues to use. You can specify an amount of storage in the range 1 - 32768 MB
            (32 GB), but this amount must not be greater than 25% of the value of the z/OS parameter MEMLIMIT.
            The default is 64 MB.
        required: false
        type: str
      tst:
        description:
          - The TST system initialization parameter specifies the temporary storage table suffix.
        required: false
        type: str
      udsasze:
        description:
          - The UDSASZE system initialization parameter specifies the size of the UDSA.
        required: false
        type: str
      uownetql:
        description:
          - The UOWNETQL system initialization parameter specifies a qualifier for the NETUOWID for units of work
            initiated on the local CICS region.
        required: false
        type: str
      usertr:
        description:
          - The USERTR system initialization parameter specifies whether the main user trace flag is to be set on or
            off.
        required: false
        type: str
        choices:
          - "ON"
          - "OFF"
      usrdelay:
        description:
          - The USRDELAY system initialization parameter specifies the maximum time, in the range 0 - 10080 minutes
            (up to seven days), that an eligible user ID and its associated attributes are cached in the CICS region
            after use. A user ID that is retained in the user table can be reused.
        required: false
        type: int
      ussconfig:
        description:
          - The USSCONFIG system initialization parameter specifies the name and path of the root directory for
            configuration files on z/OS UNIX.
        required: false
        type: str
      usshome:
        description:
          - The USSHOME system initialization parameter specifies the name and path of the root directory for
            files on z/OS UNIX.
        required: false
        type: str
      vtam:
        description:
          - The VTAM system initialization parameter specifies whether the z/OS Communications Server access method is
            to be used.
        required: false
        type: str
        choices:
          - "YES"
          - "NO"
      vtprefix:
        description:
          - The VTPREFIX system initialization parameter specifies the first character to be used for the terminal
            identifiers (termids) of autoinstalled virtual terminals.
        required: false
        type: str
      webdelay:
        description:
          - The WEBDELAY system initialization parameter specifies two Web delay periods.
        required: false
        type: str
      wlmhealth:
        description:
          - The WLMHEALTH system initialization parameter specifies the time interval and the health adjustment value
            to be used by CICS® on z/OS® Workload Manager Health API (IWM4HLTH) calls, which CICS makes to inform z/OS
            WLM about the health state of a CICS region.
        required: false
        type: str
      wrkarea:
        description:
          - The WRKAREA system initialization parameter specifies the number of bytes to be allocated to the common
            work area (CWA).
        required: false
        type: int
      xappc:
        description:
          - The XAPPC system initialization parameter specifies whether RACF session security can be used when
            establishing APPC sessions.
        required: false
        type: str
        choices:
          - "NO"
          - "YES"
      xcfgroup:
        description:
          - The XCFGROUP system initialization parameter specifies the name of the cross-system coupling facility
            (XCF) group to be joined by this region.
        required: false
        type: str
      xcmd:
        description:
          - The XCMD system initialization parameter specifies whether you want CICS to perform command security
            checking, and optionally the RACF resource class name in which you have defined the command security
            profiles.
        required: false
        type: str
      xdb2:
        description:
          - The XDB2 system initialization parameter specifies whether you want CICS to perform DB2ENTRY security
            checking.
        required: false
        type: str
      xdct:
        description:
          - The XDCT system initialization parameter specifies whether you want CICS to perform resource security
            checking for transient data queues.
        required: false
        type: str
      xfct:
        description:
          - The XFCT system initialization parameter specifies whether you want CICS to perform file resource security
            checking, and optionally specifies the RACF resource class name in which you have defined the file
            resource security profiles.
        required: false
        type: str
      xhfs:
        description:
          - The XHFS system initialization parameter specifies whether CICS is to check the transaction user's ability
            to access files in the z/OS UNIX System Services file system.
        required: false
        type: str
        choices:
          - "YES"
          - "NO"
      xjct:
        description:
          - The XJCT system initialization parameter specifies whether you want CICS to perform journal resource
            security checking.
        required: false
        type: str
      xlt:
        description:
          - The XLT system initialization parameter specifies a suffix for the transaction list table.
        required: false
        type: str
      xpct:
        description:
          - The XPCT system initialization parameter specifies whether you want CICS to perform started transaction
            resource security checking, and optionally specifies the name of the RACF resource class name in which you
            have defined the started task security profiles.
        required: false
        type: str
      xppt:
        description:
          - The XPPT system initialization parameter specifies that CICS is to perform application program resource
            security checks and optionally specifies the RACF resource class name in which you have defined the
            program resource security profiles.
        required: false
        type: str
      xpsb:
        description:
          - The XPSB system initialization parameter specifies whether you want CICS to perform program specification
            block (PSB) security checking and optionally specifies the RACF resource class name in which you have
            defined the PSB security profiles.
        required: false
        type: str
      xptkt:
        description:
          - The XPTKT system initialization parameter specifies whether CICS checks if a user can generate a
            PassTicket for the user's userid using the EXEC CICS REQUEST PASSTICKET command, the EXEC CICS REQUEST
            ENCRYPTPTKT command, or the EXEC FEPI REQUEST PASSTICKET command.
        required: false
        type: str
        choices:
          - "YES"
          - "NO"
      xres:
        description:
          - The XRES system initialization parameter specifies whether you want CICS to perform resource security
            checking for particular CICS resources and optionally specifies the general resource class name in which
            you have defined the resource security profiles.
        required: false
        type: str
      xrf:
        description:
          - The XRF system initialization parameter specifies whether XRF support is to be included in the CICS
            region.
        required: false
        type: str
        choices:
          - "NO"
          - "YES"
      xtran:
        description:
          - The XTRAN system initialization parameter specifies whether you want CICS to perform transaction security
            checking and optionally specifies the RACF resource class name in which you have defined the transaction
            security profiles.
        required: false
        type: str
      xtst:
        description:
          - The XTST system initialization parameter specifies whether you want CICS to perform security checking for
            temporary storage queues and optionally specifies
            the RACF resource class name in which you have defined the temporary storage security profiles.
        required: false
        type: str
      xuser:
        description:
          - The XUSER system initialization parameter specifies whether CICS is to perform surrogate user checks.
        required: false
        type: str
        choices:
          - "YES"
          - "NO"
      epcdsasze:
        description:
          - The EPCDSASZE parameter specifies the size of the EPCDSA dynamic storage area. Message DFHSM0136I at
            initialization shows the value that is set.
        required: false
        type: str
      epudsasze:
        description:
          - The EPUDSASZE parameter specifies the size of the EPUDSA dynamic storage area. Message DFHSM0136I at
            initialization shows the value that is set.
        required: false
        type: str
      maxtlslevel:
        description:
          - The MAXTLSLEVEL system initialization parameter specifies the maximum TLS protocol that CICS uses for
            secure TCP/IP connections.
        required: false
        type: str
        choices:
          - TLS11
          - TLS12
          - TLS13
      pcdsasze:
        description:
          - The PCDSASZE parameter specifies the size of the PCDSA dynamic storage area. Message DFHSM0136I at
            initialization shows the value that is set.
        required: false
        type: int
      pudsasze:
        description:
          - The PUDSASZE parameter specifies the size of the PUDSA dynamic storage area. Message DFHSM0136I at
            initialization shows the value that is set.
        required: false
        type: str
      sdtmemlimit:
        description:
          - The SDTMEMLIMIT system initialization parameter specifies a limit to the amount of storage above the bar
            that is available for shared data tables to use for control information (entry descriptors, backout
            elements, and index nodes). The default is 4 GB. When you set this parameter, check your current setting
            for the z/OS MEMLIMIT parameter.
        required: false
        type: str
      zosmoninterval:
        description:
          - The ZOSMONINTERVAL system initialization parameter specifies the sampling interval, in seconds, for
            the CICS® z/OS storage monitor task.
        required: false
        type: int
      zossosnewtcb:
        description:
          - The ZOSSOSNEWTCB system initialization parameter specifies the action that CICS® takes in response to
            a new open TCB that is being attached directly by CICS when the z/OS® user region storage or extended
            user region storage is short on storage (SOS). These open TCBs are L8, L9, X8 and X9 TCBs.
        required: false
        type: str
        choices:
          - "DELAY"
          - "NODELAY"
      zossos24unalloc:
        description:
          - The ZOSSOS24UNALLOC system initialization parameter specifies short-on-storage (SOS) thresholds in KB
            for the total amount of unallocated z/OS® user region storage and for the largest contiguous storage
            area available in it.
        required: false
        type: str
      zossos31unalloc:
        description:
          - The ZOSSOS31UNALLOC system initialization parameter specifies short-on-storage (SOS) thresholds in KB
            for the total amount of unallocated z/OS® extended user region storage and for the largest contiguous
            storage area available in it.
        required: false
        type: str
      zossos64unalloc:
        description:
          - The ZOSSOS64UNALLOC system initialization parameter specifies a short-on-storage (SOS) threshold in
            MB for the amount of unallocated z/OS® MEMLIMIT storage in the 64-bit addressing range.
        required: false
        type: int
"""
