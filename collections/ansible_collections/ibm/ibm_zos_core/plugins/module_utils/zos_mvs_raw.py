# Copyright (c) IBM Corporation 2020
# Apache License, Version 2.0 (see https://opensource.org/licenses/Apache-2.0)
from __future__ import absolute_import, division, print_function

__metaclass__ = type

from ansible_collections.ibm.ibm_zos_core.plugins.module_utils.dd_statement import (
    DDStatement,
    DatasetDefinition,
    FileDefinition,
    StdinDefinition,
    DummyDefinition,
)


from ansible_collections.ibm.ibm_zos_core.plugins.module_utils.ansible_module import (
    AnsibleModuleHelper,
)


class MVSCmd(object):
    """Provides an interface to execute authorized and unauthorized MVS commands.
    """

    @staticmethod
    def execute(pgm, dds, parm="", debug=False, verbose=False):
        """Execute an unauthorized MVS command.

        Args:
            pgm (str): The name of the program to execute.
            dds (list[DDStatement]): A list of DDStatement objects.
            parm (str, optional): Argument string if required by the program. Defaults to "".

        Returns:
            MVSCmdResponse: The response of the command.
        """
        module = AnsibleModuleHelper(argument_spec={})
        command = "mvscmd {0} {1} {2} ".format(
            "-d" if debug else "",
            "-v" if verbose else "",
            MVSCmd._build_command(pgm, dds, parm),
        )
        rc, out, err = module.run_command(command)
        return MVSCmdResponse(rc, out, err)

    @staticmethod
    def execute_authorized(pgm, dds, parm="", debug=False, verbose=False):
        """Execute an authorized MVS command.

        Args:
            pgm (str): The name of the program to execute.
            dds (list[DDStatement]): A list of DDStatement objects.
            parm (str, optional): Argument string if required by the program. Defaults to "".

        Returns:
            MVSCmdResponse: The response of the command.
        """
        module = AnsibleModuleHelper(argument_spec={})
        command = "mvscmdauth {0} {1} {2} ".format(
            "-d" if debug else "",
            "-v" if verbose else "",
            MVSCmd._build_command(pgm, dds, parm),
        )
        rc, out, err = module.run_command(command)
        return MVSCmdResponse(rc, out, err)

    @staticmethod
    def _build_command(pgm, dds, parm):
        """Build the command string to be used by ZOAU mvscmd/mvscmdauth.

        Args:
            pgm (str): [description]
            dds (list[DDStatement]): A list of DDStatement objects.
            parm (str, optional): Argument string if required by the program. Defaults to "".

        Returns:
            str: Command string formatted as expected by mvscmd/mvscmdauth.
        """
        args_string = ""
        if parm:
            args_string = "--args='{0}'".format(parm)
        pgm_string = "--pgm={0}".format(pgm)
        dds_string = ""
        for dd in dds:
            dds_string += " " + dd.get_mvscmd_string()
        command = "{0} {1} {2}".format(pgm_string, args_string, dds_string)
        return command


class MVSCmdResponse(object):
    """Holds response information for MVSCmd call.
    """

    def __init__(self, rc, stdout, stderr):
        self.rc = rc
        self.stdout = stdout
        self.stderr = stderr
