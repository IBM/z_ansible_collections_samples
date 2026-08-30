from __future__ import (absolute_import, division, print_function)
__metaclass__ = type
import re
from ansible_collections.ibm.ibm_zos_core.plugins.module_utils.dd_statement import (  # pylint: disable=import-error
    DDStatement,
    FileDefinition,
    DatasetDefinition,
    StdoutDefinition,
    StdinDefinition
)
from ansible_collections.ibm.ibm_zos_core.plugins.module_utils.zos_mvs_raw import MVSCmd  # pylint: disable=import-error
from ansible_collections.ibm.ibm_zos_ims.plugins.module_utils.ims_module_error_messages import DBRCErrorMessages as em  # pylint: disable=import-error


class dbrc():
    DBRC_UTILITY = "dspurx00"
    REPLACEMENT_VALUES = {
        "** NONE **": None,
        "**NULL**": None,
        "NONE": None,
        "YES": True,
        "NO": False,
        "ON": True,
        "OFF": False
    }

    def __init__(self, commands, steplib, dynamic_allocation_dataset=None, dbd_lib=None,
                 genjcl_output_dataset=None, genjcl_input_dataset=None, recon1=None, recon2=None, recon3=None):
        """IMSDBRC constructor used to run DBRC commands using zos_raw.

        Args:
            commands (str, list[str]): List of the DBRC commands to be executed.
            steplib (str, list[str]): List of STEPLIB datasets that contain the IMS nucleus and the
                required action modules.
            dynamic_allocation_dataset (str, optional): The dynamic allocation data set that will be used to complete the DBRC
                execution. Required if 'recon1', 'recon2', and 'recon3' are not specified. Defaults to None.
            dbd_lib (str, optional): The data set that contains the database descriptions for the databases
                that are under the control of DBRC. Defaults to None.
            genjcl_output_dataset (str, optional): The data set which is to receive generated JCL. It is required only for
                the GENJCL commands. Defaults to None.
            genjcl_input_dataset (str, optional): The PDS, which contains the JCL and control statements for the utility
                that DBRC uses to generate a job. Defaults to None.
            recon1 (str, optional): The RECON1 data set that will be used to complete the DBRC execution.
                Defaults to None. Required if 'dynamic_allocation_dataset' is not specified.
            recon2 (str, optional): The RECON2 data set that will be used to complete the DBRC execution.
                Defaults to None. Required if 'dynamic_allocation_dataset' is not specified.
            recon3 (str, optional): The RECON3 data set that will be used to complete the DBRC execution.
                Defaults to None. Required if 'dynamic_allocation_dataset' is not specified.
        """
        self.commands = commands
        self._original_commands = commands
        self.steplib_list = steplib
        self.dynamic_allocation_dataset = dynamic_allocation_dataset
        self.dbd_lib = dbd_lib
        self.genjcl_output_dataset = genjcl_output_dataset
        self.genjcl_input_dataset = genjcl_input_dataset
        self.recon1 = recon1
        self.recon2 = recon2
        self.recon3 = recon3
        self._changed = False
        self._assert_valid_inputs()
        self._format_command_input()

    def _assert_valid_inputs(self):
        """Validates the data types and requirements to run DBRC commands.
        """
        self._assert_valid_input_types()
        self._assert_dynalloc_recon_requirement()

    def _format_command_input(self):
        """Modifies the command input provided by the user in preparation for using the zos_raw interface.
        We replace spaces with a dash and new line (" -\n") in order to avoid the termination character
        specifically for long commands.
        """
        if isinstance(self.commands, list):
            self._original_commands = [cmd.strip() for cmd in self.commands]
            self.commands = [cmd.strip().replace(" ", " -\n") for cmd in self.commands]
        elif isinstance(self.commands, str):
            self._original_commands = [self._original_commands.strip()]
            self.commands = [self.commands.strip().replace(" ", " -\n")]

    def _assert_dynalloc_recon_requirement(self):
        """This assertion function validates that either the 'dynamic_allocation_dataset' parameter was specified, or
        all of the recon parameters were specified. This is a requirement to run the DBRC utility.

        Raises:
            ValueError: Neither dynamic_allocation_dataset nor all three recon data sets were specified.
        """
        # TODO: Determine if each of the RECONs need to be present or just 1 minimum
        if not self.dynamic_allocation_dataset and not (self.recon1 and self.recon2 and self.recon3):
            raise ValueError(em.DYNALLOC_RECON_REQUIREMENT_MSG)

    def _assert_valid_input_types(self):
        """This assertion function validates that all parameters are the correct data type. All
        parameters in the constructor must be strings except for 'commands' and 'steplib' which
        can also be a list of strings.

        Raises:
            TypeError: Raised if parameter is the wrong data type.
        """
        if isinstance(self.steplib_list, str):
            self.commands = [self.commands]
        elif not isinstance(self.commands, list) or not all(isinstance(cmd, str) for cmd in self.commands):
            raise TypeError(em.INCORRECT_CMD_TYPE)

        if isinstance(self.steplib_list, str):
            self.steplib_list = [self.steplib_list]
        elif not isinstance(self.steplib_list, list) or not all(isinstance(steplib, str) for steplib in self.steplib_list):
            raise TypeError(em.INCORRECT_STEPLIB_TYPE)

        if self.dbd_lib and not isinstance(self.dbd_lib, str):
            raise TypeError(em.INCORRECT_DBDLIB_TYPE)

        if self.dynamic_allocation_dataset and not isinstance(self.dynamic_allocation_dataset, str):
            raise TypeError(em.INCORRECT_DYNALLOC_TYPE)

        if self.genjcl_output_dataset and not isinstance(self.genjcl_output_dataset, str):
            raise TypeError(em.INCORRECT_JCLOUT_TYPE)

        if self.genjcl_input_dataset and not isinstance(self.genjcl_input_dataset, str):
            raise TypeError(em.INCORRECT_GENJCL_TYPE)

        if self.recon1 and not isinstance(self.recon1, str):
            raise TypeError(em.INCORRECT_RECON_TYPE)

        if self.recon2 and not isinstance(self.recon2, str):
            raise TypeError(em.INCORRECT_RECON_TYPE)

        if self.recon3 and not isinstance(self.recon3, str):
            raise TypeError(em.INCORRECT_RECON_TYPE)

    def _extract_values(self, output_line):
        """Given a line from the output string, this function parses a line that contains
        equals signs (=), and returns a dictionary with key value pairs based on the line.

        Args:
            output_line (str): The original line of output given from the DBRC utility result.

        Returns:
            (dict): Mapping of the fields that corresponds to the line from the output

        Example:
            output_line: 'FORCER    LOG DSN CHECK=CHECK44    STARTNEW=NO'
            returns: {"LOG DSN CHECK": "CHECK44", "STARTNEW": False}
        """
        fields = {}
        elements = output_line.split("=")
        i = 0
        double_space = "  "

        def filter_function(elem):
            return elem and elem != " "

        while i < len(elements) - 1:
            value_index = i + 1
            key_list = list(filter(filter_function, elements[i].split(double_space)))
            value_list = list(filter(filter_function, elements[i + 1].split(double_space)))

            last_key_index = len(key_list) - 1
            key = key_list[last_key_index].strip()
            if len(key_list) == 1 and i > 0 and i < len(elements) - 1:
                unformatted_key = key_list[0]
                try:
                    start_index = re.search(r'\d+\s', unformatted_key).end()
                    key = unformatted_key[start_index:].strip()
                except Exception as e:
                    key = unformatted_key.strip()
            if len(elements) != 2 and \
                (elements[i + 1][0:2] == double_space or
                    (len(value_list) == 1 and value_index > 0 and value_index < len(elements) - 1)):

                fields[key] = None
            else:
                value = value_list[0].strip()
                if value in dbrc.REPLACEMENT_VALUES:
                    value = dbrc.REPLACEMENT_VALUES[value]
                fields[key] = value
            i += 1

        return fields

    def _format_command(self, raw_command):
        """Properly formats the specific command corresponding to the output being parsed.

        Args:
            raw_command (str): Unformatted str that contains the current command.

        Returns:
            (str): Formatted command string.
        """
        if raw_command[0] == "0":
            return raw_command[1:].strip()
        return raw_command.strip()

    def _get_indentifier(self, raw_output_line):
        """Returns the "identifier" of the particular block of output being parsed.

        Args:
            raw_output_line (str): Unformatted line from the output that contains the identifier.

        Returns:
            (str): Formatted identifier extracted from the provided line of output.
        """
        return raw_output_line.split("  ")[0].strip()

    def _parse_output(self, raw_output):
        """Main function that parses the entire output provided by the zos_raw module.

        Args:
            raw_output (str): Unformatted output text provided from zos_raw module.

        Returns:
            (list): List of dictionaries containing parsed output mappings.
            (list[str]): Original output provided by zos_raw.
            (boolean): True if failure detected, False otherwise.
        """
        original_output = [elem.strip() for elem in raw_output.split("\n")]
        output_fields = []
        command = ''
        separation_pattern = r'-{5,}'
        rec_ctrl_pattern = r'recovery control\s*page\s\d+'
        success_pattern = r'COMMAND COMPLETED WITH CONDITION CODE 0[0|4]'
        dsp_pattern = r'DSP\d{4}I'
        failure_pattern = r'invalid command name'
        output_index = 0
        command_index = -1
        failure_detected = False
        try:
            for index, line in enumerate(original_output):
                if re.search(rec_ctrl_pattern, line, re.IGNORECASE) \
                        and not re.search(dsp_pattern, original_output[index + 1], re.IGNORECASE):
                    command_index += 1
                    command = self._original_commands[command_index]
                    output_fields.append({})
                    output_fields[command_index]['COMMAND'] = command
                    output_fields[command_index]['MESSAGES'] = []
                    output_fields[command_index]['OUTPUT'] = [{}]
                    output_index = 0
                elif re.search(separation_pattern, line, re.IGNORECASE):
                    if output_fields[command_index]['OUTPUT'][output_index]:
                        output_fields[command_index]['OUTPUT'].append({})
                        output_index += 1
                    output_id = {"IDENTIFIER": self._get_indentifier(original_output[index + 1])}
                    output_fields[command_index]['OUTPUT'][output_index].update(output_id)
                elif "=" in line:
                    output_fields[command_index]['OUTPUT'][output_index].update(self._extract_values(line))
                elif re.search(dsp_pattern, line, re.IGNORECASE):
                    output_fields[command_index]['MESSAGES'].append(line)
                if re.search(failure_pattern, line, re.IGNORECASE):
                    failure_detected = True
                elif re.search(success_pattern, line, re.IGNORECASE):
                    self._changed = True
        except Exception as e:
            print(repr(e))
            output_fields = {}
            failure_detected = True
        # finally:
        return output_fields, original_output, failure_detected

    def _add_utility_statement(self, name, data_set, dbrc_utility_fields):
        """Adds the DD Statement to the list of dbrc_utility_fields if the data set name
        was provided.

        Args:
            name (str): Name parameter to provide to DDStatement constructor.
            data_set (str): Name of the dataset to be provided to the DatasetDefinition cosntructor.
            dbrc_utility_fields (list[str]): List that contains DDStatement objects.
        """
        if data_set:
            dbrc_utility_fields.append(DDStatement(name, DatasetDefinition(data_set)))

    def _build_utility_statements(self):
        """Builds the list DDStatements that will be provided to the zos_raw execute function
        based on the user input.

        Returns:
            (list[DDStatement]): List of DDStatements
        """
        dbrc_utility_fields = []
        steplib_data_set_definitions = [DatasetDefinition(steplib) for steplib in self.steplib_list]
        if self.dynamic_allocation_dataset:
            steplib_data_set_definitions.append(DatasetDefinition(self.dynamic_allocation_dataset))
        steplib = DDStatement("steplib", steplib_data_set_definitions)
        dbrc_utility_fields.append(steplib)
        self._add_utility_statement("recon1", self.recon1, dbrc_utility_fields)
        self._add_utility_statement("recon2", self.recon2, dbrc_utility_fields)
        self._add_utility_statement("recon3", self.recon3, dbrc_utility_fields)
        self._add_utility_statement("jclpds", self.genjcl_input_dataset, dbrc_utility_fields)
        self._add_utility_statement("genjcl", self.genjcl_output_dataset, dbrc_utility_fields)
        self._add_utility_statement("ims", self.dbd_lib, dbrc_utility_fields)
        dbrc_commands = StdinDefinition("\n".join(self.commands))
        sysin = DDStatement("sysin", dbrc_commands)
        sysprint = DDStatement("sysprint", StdoutDefinition())
        dbrc_utility_fields.extend([sysin, sysprint])
        return dbrc_utility_fields

    def execute(self):
        """Executes the DBRC utility dspurx00 using the zos_raw module based on the user input.

        Returns:
            (dict): (1) original_output:  The original, unformatted output provided by zos_raw and
                    (2) dbrc_fields:      The parsed mappings for the output.
                    (3) failure_detected: Boolean value representing failure in output.
                    (4) error:            The stderr returned by the zos_raw module.
                    (5) rc:               Return code reeturned by the zos_raw module.
                    (6) changed:          Boolean value representing change in target state.
        """
        try:
            dbrc_utility_fields = self._build_utility_statements()
            response = MVSCmd.execute(dbrc.DBRC_UTILITY, dbrc_utility_fields)
            fields, original_output, failure_detected = self._parse_output(response.stdout)
            # fields, original_output = self._parse_output(TEST_INPUT)
            res = {
                "dbrc_fields": fields,
                "original_output": original_output,
                "failure_detected": failure_detected or int(response.rc) > 4,
                "error": response.stderr,
                "rc": response.rc,
                "changed": self._changed
            }

        except Exception as e:
            print("ERROR:", e)
            res = {
                "dbrc_fields": [],
                "original_output": [],
                "failure_detected": True,
                "error": repr(e),
                "rc": None,
                "changed": False
            }
        # finally:
        return res
