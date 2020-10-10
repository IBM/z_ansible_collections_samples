# Copyright (c) IBM Corporation 2020
# Apache License, Version 2.0 (see https://opensource.org/licenses/Apache-2.0)


from __future__ import absolute_import, division, print_function

__metaclass__ = type

from ansible_collections.ibm.ibm_zos_core.plugins.module_utils.ansible_module import (
    AnsibleModuleHelper,
)


def iebcopy(cmd, dds=None, authorized=False):
    """IEBCOPY is a data set utility that is used to copy or merge members
    between one or more partitioned data sets, or partitioned data sets extended
    (PDSEs), in full or in part. You can also use IEBCOPY to create a backup of
    a partitioned data set into a sequential data set (called an unload data set
    or PDSU), and to copy members from the backup into a partitioned data set.

    Arguments:
        cmd {str} -- The command to pass to IEBCOPY
        dds {dict} -- Any DD statements to pass to MVS command
        authorized {bool} -- Whether the command should be run in authorized
        mode
    """
    return _run_mvs_command("IEBCOPY", cmd, dds, authorized)


def iebedit(cmd, dds=None, authorized=False):
    """You can use IEBEDIT to create a data set containing a selection of jobs
    or job steps. These jobs or job steps can be entered into the job stream at
    a later time for processing.

    Arguments:
        cmd {str} -- The command to pass to IEBEDIT
        dds {dict} -- Any DD statements to pass to MVS command
        authorized {bool} -- Whether the command should be run in authorized
        mode
    """
    return _run_mvs_command("IEBEDIT", cmd, dds, authorized)


def iebcompr(cmd, dds=None, authorized=False):
    """IEBCOMPR is a data set utility that is used to compare two sequential
    data sets, two partitioned data sets or two partisioned data sets (PDSEs)
    at the logical record level to verify a backup copy. Fixed, variable, or
    undefined records from blocked or unblocked data sets or members can also be
    compared. However, you should not use IEBCOMPR to compare load modules.

    Arguments:
        cmd {str} -- The command to pass to IEBCOMPR
        dds {dict} -- Any DD statements to pass to MVS command
        authorized {bool} -- Whether the command should be run in authorized
        mode
    """
    return _run_mvs_command("IEBCOMPR", cmd, dds, authorized)


def iebdg(cmd, dds=None, authorized=False):
    """IEBDG is a data set utility that is used to provide a pattern of test
    data to be used as a programming debugging aid. This pattern of data can
    then be analyzed quickly for predictable results.

    Arguments:
        cmd {str} -- The command to pass to IEBDG
        dds {dict} -- Any DD statements to pass to MVS command
        authorized {bool} -- Whether the command should be run in authorized
        mode
    """
    return _run_mvs_command("IEBDG", cmd, dds, authorized)


def iebgener(cmd, dds=None, authorized=False):
    """You can use IEBGENER to perform these tasks:
    - Create a backup copy of a sequential data set, a member of a partitioned
      data set or PDSE or a z/OS UNIX System Services (z/OS UNIX) file such as a
      HFS file.
    - Produce a partitioned data set or PDSE, or a member of a partitioned data
      set or PDSE, from a sequential data set or a z/OS UNIX file.
    - Expand an existing partitioned data set or PDSE by creating partitioned
      members and merging them into the existing data set.
    - Produce an edited sequential or partitioned data set or PDSE.
    - Manipulate data sets containing double-byte character set data.
    - Print sequential data sets, members of partitioned data sets or PDSEs or
      z/OS UNIX files.
    - Reblock or change the logical record length of a data set.
    - Copy user labels on sequential output data sets.
    - Supply editing facilities and exits for your routines that process labels,
      manipulate input data, create keys, and handle permanent input/output
      errors.

    Arguments:
        cmd {str} -- The command to pass to IEBGENER
        dds {dict} -- Any DD statements to pass to MVS command
        authorized {bool} -- Whether the command should be run in authorized
        mode
    """
    return _run_mvs_command("IEBGENER", cmd, dds, authorized)


def idcams(cmd, dds=None, authorized=False):
    """IDCAMS, which is the program name for access method services, is used
    primarily to define and manage VSAM data sets and integrated catalog
    facility catalogs.

    Arguments:
        cmd {str} -- The command to pass to IDCAMS
        dds {dict} -- Any DD statements to pass to MVS command
        authorized {bool} -- Whether the command should be run in authorized
        mode
    """
    return _run_mvs_command("IDCAMS", cmd, dds, authorized)


def ikjeft01(cmd, dds=None, authorized=False):
    """IKJEFT01 is the TSO/E program. You can use it whenever you wish to perform
    a TSO function within a batch job. It allows you to perform any TSO function.
    For a general list of all TSO functions, type TSO HELP. Additionally,
    IKJEFT01 allows you to run programs written in TSO/E Command List (CLIST)
    and/or TSO/E REXX. Optionally, you can also invoke other environments, such
    as ISPF , allowing you to run ISPF Dialogs in a batch environment.

    Arguments:
        cmd {str} -- The command to pass to IKJEFT01
        dds {dict} -- Any DD statements to pass to MVS command
        authorized {bool} -- Whether the command should be run in authorized
        mode
    """
    return _run_mvs_command("IKJEFT01", cmd, dds, authorized)


def iehlist(cmd, dds=None, authorized=False):
    """IEHLIST is a system utility used to list entries in the directory of one
    or more partitioned data sets or PDSEs, or entries in an indexed or
    non-indexed volume table of contents. Any number of listings can be
    requested in a single execution of the program.

    Arguments:
        cmd {str} -- The command to pass to IEHLIST
        dds {dict} -- Any DD statements to pass to MVS command
        authorized {bool} -- Whether the command should be run in authorized
        mode
    """
    return _run_mvs_command("IEHLIST", cmd, dds, authorized)


def _run_mvs_command(pgm, cmd, dd=None, authorized=False):
    """Run a particular MVS command.

    Arguments:
        pgm {str} -- The MVS program to run
        cmd {str} -- The input command to pass to the program

    Keyword Arguments:
        dd {dict} -- The DD definitions required by the program. (Default {None})
        authorized {bool} -- Indicates whether the MVS program should run
        as authorized. (Default {False})

    Returns:
        tuple[int, str, str] -- A tuple of return code, stdout and stderr
    """
    module = AnsibleModuleHelper(argument_spec={})
    sysprint = "sysprint"
    sysin = "sysin"
    pgm = pgm.upper()
    if pgm == "IKJEFT01":
        sysprint = "systsprt"
        sysin = "systsin"

    mvscmd = "mvscmd"
    if authorized:
        mvscmd += "auth"
    mvscmd += " --pgm={0} --{1}=* --{2}=stdin".format(pgm, sysprint, sysin)
    if dd:
        for k, v in dd.items():
            mvscmd += " --{0}={1}".format(k, v)

    return module.run_command(mvscmd, data=cmd)
