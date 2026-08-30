# Copyright (c) IBM Corporation 2020, 2025
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#     http://www.apache.org/licenses/LICENSE-2.0
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.


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

    Parameters
    ----------
    cmd : str
        The command to pass to IEBCOPY.
    dds : dict
        Any DD statements to pass to MVS command.
    authorized : bool
        Whether the command should be run in authorized
        mode.

    Returns
    -------
    tuple(int, str, str)
        A tuple of return code, stdout and stderr.
    """
    return _run_mvs_command("IEBCOPY", cmd, dds, authorized)


def iebedit(cmd, dds=None, authorized=False):
    """You can use IEBEDIT to create a data set containing a selection of jobs
    or job steps. These jobs or job steps can be entered into the job stream at
    a later time for processing.

    Parameters
    ----------
    cmd : str
        The command to pass to IEBEDIT.
    dds : dict
        Any DD statements to pass to MVS command.
    authorized : bool
        Whether the command should be run in authorized
        mode.

    Returns
    -------
    tuple(int, str, str)
        A tuple of return code, stdout and stderr.
    """
    return _run_mvs_command("IEBEDIT", cmd, dds, authorized)


def iebcompr(cmd, dds=None, authorized=False):
    """IEBCOMPR is a data set utility that is used to compare two sequential
    data sets, two partitioned data sets or two partisioned data sets (PDSEs)
    at the logical record level to verify a backup copy. Fixed, variable, or
    undefined records from blocked or unblocked data sets or members can also be
    compared. However, you should not use IEBCOMPR to compare load modules.

    Parameters
    ----------
    cmd : str
        The command to pass to IEBCOMPR.
    dds : dict
        Any DD statements to pass to MVS command.
    authorized : bool
        Whether the command should be run in authorized
        mode.

    Returns:
    tuple(int, str, str)
        A tuple of return code, stdout and stderr.
    """
    return _run_mvs_command("IEBCOMPR", cmd, dds, authorized)


def iebdg(cmd, dds=None, authorized=False):
    """IEBDG is a data set utility that is used to provide a pattern of test
    data to be used as a programming debugging aid. This pattern of data can
    then be analyzed quickly for predictable results.

    Parameters
    ----------
    cmd : str
        The command to pass to IEBDG.
    dds : dict
        Any DD statements to pass to MVS command.
    authorized : bool
        Whether the command should be run in authorized
        mode.

    Returns
    -------
    tuple(int, str, str)
        A tuple of return code, stdout and stderr.
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

    Parameters
    ----------
    cmd : str
        The command to pass to IEBGENER.
    dds : dict
        Any DD statements to pass to MVS command.
    authorized : bool
        Whether the command should be run in authorized
        mode.

    Returns
    -------
    tuple(int, str, str)
        A tuple of return code, stdout and stderr.
    """
    return _run_mvs_command("IEBGENER", cmd, dds, authorized)


def idcams(cmd, dds=None, authorized=False):
    """IDCAMS, which is the program name for access method services, is used
    primarily to define and manage VSAM data sets and integrated catalog
    facility catalogs.

    Parameters
    ----------
    cmd : str
        The command to pass to IDCAMS.
    dds : dict
        Any DD statements to pass to MVS command.
    authorized : bool
        Whether the command should be run in authorized
        mode.

    Returns
    -------
    tuple(int, str, str)
        A tuple of return code, stdout and stderr.
    """
    return _run_mvs_command("IDCAMS", cmd.upper(), dds, authorized)


def ikjeft01(cmd, dds=None, authorized=False, tmphlq=None):
    """IKJEFT01 is the TSO/E program. You can use it whenever you wish to perform
    a TSO function within a batch job. It allows you to perform any TSO function.
    For a general list of all TSO functions, type TSO HELP. Additionally,
    IKJEFT01 allows you to run programs written in TSO/E Command List (CLIST)
    and/or TSO/E REXX. Optionally, you can also invoke other environments, such
    as ISPF , allowing you to run ISPF Dialogs in a batch environment.

    Parameters
    ----------
    cmd : str
        The command to pass to IKJEFT01.
    dds : dict
        Any DD statements to pass to MVS command.
    authorized : bool
        Whether the command should be run in authorized
        mode.
    tmphlq : str
        High Level Qualifier for temporary datasets.

    Returns
    -------
    tuple(int, str, str)
        A tuple of return code, stdout and stderr.
    """
    return _run_mvs_command("IKJEFT01", cmd, dds, authorized, tmphlq=tmphlq)


def iehlist(cmd, dds=None, authorized=False):
    """IEHLIST is a system utility used to list entries in the directory of one
    or more partitioned data sets or PDSEs, or entries in an indexed or
    non-indexed volume table of contents. Any number of listings can be
    requested in a single execution of the program.

    Parameters
    ----------
    cmd : str
        The command to pass to IEHLIST.
    dds : dict
        Any DD statements to pass to MVS command.
    authorized : bool
        Whether the command should be run in authorized
        mode.

    Returns
    -------
    tuple(int, str, str)
        A tuple of return code, stdout and stderr.
    """
    return _run_mvs_command("IEHLIST", cmd, dds, authorized)


def amaterse(cmd="", dds=None, authorized=False):
    """AMATERSE is a service aid program that operates in problem state.
    You can use AMATERSE to pack a data set before transmitting a copy
    to another site, typically employing FTP as the transmission mechanism.
    A complementary unpack service is provided to create a similar data set
    at the receiving site.

    Parameters
    ----------
    dds : dict
        Any DD statements to pass to MVS command.
    authorized : bool
        Whether the command should be run in authorized
        mode.

    Returns
    -------
    tuple(int, str, str)
        A tuple of return code, stdout and stderr.
    """
    return _run_mvs_command("AMATERSE", "", dds, authorized)


def iefbr14(dds=None):
    """IEFBR14 performs no action other than return a completion code of 0;
    however, "running" this utility invokes other system components that
    perform useful tasks.

    Parameters
    ----------
    dds : dict
        Any DD statements to pass to MVS command.

    Returns
    -------
    tuple(int, str, str)
        A tuple of return code, stdout and stderr.
    """
    return _run_mvs_command("IEFBR14", "", dds, False)


def adrdssu(cmd, dds=None, authorized=False):
    """The ADRDSSU program enables you to copy SMS-compressed data without
    having to decompress the data and also provides support for copying
    wildcard-named files.
    Is a DFSMSdss utility that provides backup and recovery functions
    at both the data set and volume levels.

    Returns
    -------
    tuple(int, str, str)
        A tuple of return code, stdout and stderr.
    """
    return _run_mvs_command("ADRDSSU", cmd, dds, authorized)


def _run_mvs_command(pgm, cmd, dd=None, authorized=False, tmphlq=None):
    """Run a particular MVS command.

    Parameters
    ----------
    pgm : str
        The MVS program to run.
    cmd : str
        The input command to pass to the program.

    Keyword Parameters
    ------------------
    dd : dict
        The DD definitions required by the program. (Default {None})
    authorized : bool
        Indicates whether the MVS program should run
        as authorized. (Default {False})
    tmphlq : str
        High Level Qualifier for temporary datasets.

    Returns
    -------
    tuple(int, str, str)
        A tuple of return code, stdout and stderr.
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
    if tmphlq:
        mvscmd += " -Q={0}".format(tmphlq)
    if pgm == "IEFBR14":
        mvscmd += " --pgm={0}".format(pgm)
    else:
        mvscmd += " --pgm={0} --{1}=* --{2}=stdin".format(pgm, sysprint, sysin)
    if dd:
        for k, v in dd.items():
            mvscmd += " --{0}={1}".format(k, v)

    return module.run_command(mvscmd, data=cmd, errors='replace')
