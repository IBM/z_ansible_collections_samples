from __future__ import (absolute_import, division, print_function)
from ansible_collections.ibm.ibm_zos_core.plugins.module_utils.dd_statement import (  # pylint: disable=import-error
    DDStatement,
    DatasetDefinition,
    StdoutDefinition,
    StdinDefinition,
)
from ansible_collections.ibm.ibm_zos_core.plugins.module_utils.zos_mvs_raw import MVSCmd  # pylint: disable=import-error
import re

from ansible_collections.ibm.ibm_zos_ims.plugins.module_utils.ims_module_error_messages import ACBGENErrorMessages as em  # pylint: disable=import-error

__metaclass__ = type


class acbgen(object):
    ACBGEN_UTILITY = "DFSRRC00"
    COMPRESS_UTILITY = "IEBCOPY"

    def __init__(self, command_input, compression, psb_name, dbd_name, acb_lib, psb_lib, dbd_lib, reslib, steplib, build_psb):
        """IMSAcbgen constructor for generating IMS ACB using zos_mvs_raw
        Args:
           command_input (str): 'BUILD' or 'DELETE' - command input to specify.
           compression (str): 'PRECOMP','POSTCOMP' or 'PRECOMP,POSTCOMP' or default is none.
           psb_name (list): 'ALL' or a list of psb names to be specified.
           dbd_name (list): List of dbd names.
           acb_lib (str): This dataset is the consolidated library of PSB and DBD descriptions.
           psb_lib (list): List of psblib datasets.
           dbd_lib (list): List of dbdlib datasets.
           reslib (list): List of reslib datasets.
           steplib (list): Points to the list of IMS SDFSRESL data set, which contains the IMS nucleus and required IMS modules.
           build_psb (bool): TRUE for rebuilding all PSBs that reference the changed DBD,
                             FALSE for not rebuilding the PSBs that references teh changed DBD.
        """
        self.command_input = command_input
        self.compression = compression
        self.psb_name = psb_name
        self.dbd_name = dbd_name
        self.acb_lib = acb_lib
        self.psb_lib = psb_lib
        self.dbd_lib = dbd_lib
        self.reslib = reslib
        self.steplib = steplib
        self.build_psb = build_psb
        self._assert_valid_input_types()
        self.result = {}

    def _assert_valid_input_types(self):
        """This assertion function validates that all parameters are the correct data type.
         Raises:
              TypeError: Raised if parameter is the wrong data type.
        """
        if self.command_input and not isinstance(self.command_input, str):
            raise TypeError(em.INCORRECT_COMMAND_INPUT_TYPE)
        if self.compression and not isinstance(self.compression, str):
            raise TypeError(em.INCORRECT_COMPRESSION_TYPE)
        if self.acb_lib and not isinstance(self.acb_lib, str):
            raise TypeError(em.INCORRECT_ACBLIB_TYPE)
        if self.dbd_name and not all(isinstance(item, str) for item in self.dbd_name):
            raise TypeError(em.INCORRECT_DBD_NAME_TYPE)
        if self.psb_name and not all(isinstance(item, str) for item in self.psb_name):
            raise TypeError(em.INCORRECT_PSB_NAME_TYPE)
        if self.psb_lib and not all(isinstance(item, str) for item in self.psb_lib):
            raise TypeError(em.INCORRECT_PSBLIB_TYPE)
        if self.dbd_lib and not all(isinstance(item, str) for item in self.dbd_lib):
            raise TypeError(em.INCORRECT_DBDLIB_TYPE)
        if self.steplib and not all(isinstance(item, str) for item in self.steplib):
            raise TypeError(em.INCORRECT_STEPLIB_TYPE)
        if self.reslib and not all(isinstance(item, str) for item in self.reslib):
            raise TypeError(em.INCORRECT_RESLIB_TYPE)
        if self.build_psb and not isinstance(self.build_psb, bool):
            raise TypeError(em.INCORRECT_BUILD_PSB_TYPE)
        if not self.psb_name and not self.dbd_name:
            raise ValueError(em.INCORRECT_COMMAND_ARGS)

    def _build_acbgen_statements(self):
        """Builds the list of DDStatements that will be provided to the zos_mvs_raw to execute DFSRRC00
         based on the user input.

        Returns:
          (list[DDStatement]): List of DDStatements
        """
        acbgen_utility_fields = []
        ims_dataset_list = []
        sysprint = DDStatement("SYSPRINT", StdoutDefinition())

        acbgen_utility_fields.append(sysprint)

        if self.steplib:
            steplib_data_set_definitions = [
                DatasetDefinition(steplib) for steplib in self.steplib]
            steplib = DDStatement("STEPLIB", steplib_data_set_definitions)
            acbgen_utility_fields.append(steplib)

        if self.reslib:
            reslib = self.reslib
        else:
            reslib = self.steplib

        if self.reslib:
            reslib_data_set_definitions = [
                DatasetDefinition(reslib) for reslib in self.reslib]
            reslib_dd_statement = DDStatement("DFSRESLB", reslib_data_set_definitions)
            acbgen_utility_fields.append(reslib_dd_statement)
        if self.psb_lib:
            for psblib in self.psb_lib:
                ims_dataset_list.append(DatasetDefinition(psblib))
        if self.dbd_lib:
            for dbdlib in self.dbd_lib:
                ims_dataset_list.append(DatasetDefinition(dbdlib))

        if ims_dataset_list:
            ims_data_set_definitions = DDStatement("IMS", ims_dataset_list)
            acbgen_utility_fields.append(ims_data_set_definitions)

        if self.acb_lib:
            acblib_data_set_definitions = DDStatement(
                "IMSACB", DatasetDefinition(self.acb_lib, disposition="old"))
            acbgen_utility_fields.append(acblib_data_set_definitions)

        commandList = []
        # Generate DD statements for commands
        if self.command_input:
            psb_name_str = ""
            if self.psb_name:
                psb_name_str = " " + self._split_lines_psb()
                commandList.append(psb_name_str)
            dbd_name_str = ""
            if self.dbd_name:
                dbd_name_str = " " + self._split_lines_dbd()
                commandList.append(dbd_name_str)

        for a in commandList:
            print(" commandList:  ", a)
        command_stdin_definitions = DDStatement(
            "SYSIN", StdinDefinition("\n".join(commandList)))
        acbgen_utility_fields.append(command_stdin_definitions)

        return acbgen_utility_fields

    def _build_compress_statements(self):
        """Builds the list of DDStatements that will be provided to the zos_mvs_raw to execute IEBCOPY
         based on the user input.

        Returns:
          (list[DDStatement]): List of DDStatements
        """
        iebcopy_utility_fields = []
        sysprint = DDStatement("SYSPRINT", StdoutDefinition())
        iebcopy_utility_fields.append(sysprint)
        compctl_stdin_definitions = DDStatement(
            "SYSIN", StdinDefinition("  COPY  INDD=IMSACB,OUTDD=IMSACB"))
        iebcopy_utility_fields.append(compctl_stdin_definitions)
        acblib_data_set_definitions = DDStatement(
            "IMSACB", DatasetDefinition(self.acb_lib, disposition="old"))
        iebcopy_utility_fields.append(acblib_data_set_definitions)
        return iebcopy_utility_fields

    def _split_lines_psb(self):
        """Splitting the command on new line in case if it exceeds 80 characters.

        Returns: A string or array of PSB commands.
        """
        return_str = ""
        return_arr = []
        psbnames = self.psb_name
        if len(psbnames) < 6:
            return_str = self._build_psb_name_string(
                self.command_input, psbnames)
            return return_str
        else:
            for i in range(0, len(psbnames), 5):
                new_psb_names_five = psbnames[i: i + 5]
                psb_line = self._build_psb_name_string(
                    self.command_input, new_psb_names_five)
                return_arr.append(psb_line)
            return "\n ".join(return_arr)
        return return_arr

    def _build_psb_name_string(self, command_input, psbnames):
        """Build the PSB command string.

        Args:
          command_input(str): The user input that can be either "BUILD" or "DELETE".
          psbnames(list): This can be either "ALL" or a list of psb strings.

        Returns: Well-Formed PSB command.
        """
        psb_str = ""
        if psbnames:
            for psb in psbnames:
                # if a match is not found
                if not re.fullmatch(r'[A-Z$#@]{1}[A-Z0-9$#@]{0,7}', psb, re.IGNORECASE):
                    return psb
                else:
                    # if a match is found
                    if psb == "ALL":
                        psb_str = command_input + " PSB=ALL"
                        break
                    psb_str = command_input + \
                        " PSB=(" + ",".join(psbnames) + ")"
        return psb_str

    def _split_lines_dbd(self):
        """Splitting the command on new line in case if it exceeds 80 characters.

        Returns: A string or array of DBD commands.
        """
        return_str = ""
        return_arr = []
        dbdnames = self.dbd_name
        if len(dbdnames) < 6:
            return_str = self._build_dbd_name_string(
                self.command_input, dbdnames, self.build_psb)
            return return_str
        else:
            for i in range(0, len(dbdnames), 5):
                new_db_names_five = dbdnames[i: i + 5]
                dbd_line = self._build_dbd_name_string(
                    self.command_input, new_db_names_five, self.build_psb)
                return_arr.append(dbd_line)
            return "\n ".join(return_arr)
        return return_arr

    def _build_dbd_name_string(self, command_input, dbdnames, build_psb):
        """Build the DBD command string.

        Args:
          command_input(str): The user input that can be either "BUILD" or "DELETE".
          psbnames(list): This must be a list of dbd strings.
          build_psb(bool): This option is available for BUILD DBD. By default its set to true(yes).

        Returns: Well-Formed DBD command.
        """
        dbd_str = ""
        bldpsb_value = ""
        if build_psb:
            bldpsb_value = "YES"
        else:
            bldpsb_value = "NO"

        if dbdnames:
            for dbd in dbdnames:
                # if a match is not found
                if not re.fullmatch(r'[A-Z$#@]{1}[A-Z0-9$#@]{0,7}', dbd, re.IGNORECASE):
                    return dbd
                else:
                    # if a match is found
                    if dbdnames and build_psb:
                        dbd_str = command_input + \
                            " DBD=(" + ",".join(dbdnames) + ")"
                    elif dbdnames and command_input == "BUILD":
                        dbd_str = (
                            command_input
                            + " DBD=("
                            + ",".join(dbdnames)
                            + ")"
                            + ",BLDPSB="
                            + bldpsb_value
                        )
                    elif dbdnames:
                        dbd_str = command_input + \
                            " DBD=(" + ",".join(dbdnames) + ")"
        return dbd_str

    def combine_results(self, result):
        """Add results of execution to existing
        results if there are any.

        Args:
            result ([type]): [description]

        Returns:
            [type]: [description]
        """
        if self.result is None:
            self.result = {}
        self.result['rc'] = max(self.result.get("rc", -1), result.rc)
        self.result["output"] = self.result.get("output", "") + result.stdout
        self.result["error"] = self.result.get("error", "") + result.stderr
        return self.result

    def compress(self):
        """Executes the compression utility IEBCOPY using the zos_mvs_raw module based on the user input.
        """
        iebcopy_utility_fields = self._build_compress_statements()
        response = MVSCmd.execute(
            acbgen.COMPRESS_UTILITY, iebcopy_utility_fields, verbose=True)
        return response

    def execute(self):
        """Executes the ACBGEN utility DFSRRC00 using the zos_mvs_raw module based on the user input.

        Returns:
          (dict): (1) rc:      Return Code returned by zos_mvs_raw module.
                  (2) error:   The stderr returned by the zos_raw module.
                  (3) output:  The original output provided by zos_raw.
        """
        self.result = {}
        response = None
        param_string = "UPB,"
        print(" compression: ", self.compression)
        if self.compression and self.compression.upper().startswith("PRECOMP"):
            response = self.compress()
            self.result = self.combine_results(response)
            if self.result.get("rc") > 4:
                return self.result
        acbgen_utility_fields = self._build_acbgen_statements()
        response = MVSCmd.execute(
            acbgen.ACBGEN_UTILITY, acbgen_utility_fields, param_string, verbose=True)
        self.result = self.combine_results(response)
        if self.compression and self.compression.upper().endswith("POSTCOMP"):
            response = self.compress()
            self.result = self.combine_results(response)
        return self.result
