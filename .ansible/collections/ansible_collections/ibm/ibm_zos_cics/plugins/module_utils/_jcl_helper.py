# -*- coding: utf-8 -*-

# (c) Copyright IBM Corp. 2024
# Apache License, Version 2.0 (see https://opensource.org/licenses/Apache-2.0)

# FOR INTERNAL USE IN THE COLLECTION ONLY.

from __future__ import absolute_import, division, print_function

__metaclass__ = type
import re

ACCOUNTING_INFORMATION = 'accounting_information'
CONTENT = 'content'
CONCAT_JCL_PREFIX = '//         '
DD = 'DD'
DD_INSTREAM = 'DD *'
DDS = 'dds'
DLM = 'dlm'
END_INSTREAM = '/*'
EXEC = 'EXEC'
EXECS = 'execs'
GMTEXT = 'GMTEXT'
JCL_PREFIX = '//'
JOB = 'JOB'
JOB_NAME = 'job_name'
JOB_CARD = 'job_card'
MAX_LINE_LENGTH = 72
MSGLEVEL = 'msglevel'
NAME = 'name'
NO_DD_NAME = '         '
PROGRAMMER_NAME = 'programmer_name'


class JCLHelper:
    def __init__(self):
        self.job_data = {JOB_CARD: {},
                         EXECS: [],
                         }
        self.jcl = []

    def render_jcl(self):
        """Renders the JCL from the JCLHelper.job_data structure. Appends all JCL to the JCL parameter of this class.
        """
        self._write_job_statement(self.job_data[JOB_CARD])
        self._write_exec_statements(self.job_data[EXECS])
        self._write_null_statement()

    def _write_job_statement(self, job_parameters):
        job_statement = JCLHelper._build_job_statement(job_parameters)
        self._write_list_of_strings(
            JCLHelper._split_long_dd_statement(job_statement))

    def _write_dds(self, dds):
        """Writes dd statements to the JCL.

        Parameters
        ----------
        dds : list
            List of dictionaries, containing the dd information.
            Example Syntax:
            [{"DDNAME1": [{"dsn": DATA.NAME", "disp": "SHR"}],
            {"DDNAME2": [{"content": ["INSTREAM DATA", "TO WRITE"]}],
            {"DDNAME3" : [{"dsn": "DATA.NAME.1"}, {"dsn" : "DATA.NAME.2"}]}]
        """
        for dd_card in dds:
            for dd_name, data in dd_card.items():
                if isinstance(data, dict):
                    # Intream data isn't stored in a list of dicts, it's stored in a dict.
                    if data.get(CONTENT):
                        self._write_instream_data(dd_name, data)
                elif len(data) == 1:
                    self._write_dd_statement(dd_name, data[0])
                else:
                    self._write_dd_concatenation(dd_name, data)

    def _write_exec_statements(self, list_of_exec_dicts):
        """Builds and writes an EXEC Statement, with any additional parameters, and writes the corresponding
        dd statements.
                Parameters
                ----------
                list_of_exec_dicts : list
                    List of dictionaries with all the exec parameters, and a dictionary of dd statements.
                """
        dd_dict = None
        for exec_statement in list_of_exec_dicts:
            if exec_statement.get(DDS) or exec_statement.get(DDS) is not {}:
                dd_dict = exec_statement.pop(DDS)
            exec_statement_string = JCLHelper._build_exec_statement_string(exec_statement)

            self._write_list_of_strings(
                JCLHelper._split_long_dd_statement(exec_statement_string))
            if dd_dict:
                self._write_dds(dd_dict)

    @staticmethod
    def _build_exec_statement_string(exec_dict):
        step_name = JCLHelper._format_dd_name(exec_dict.pop(NAME))
        exec_string = '{0}{1}{2}'.format(JCL_PREFIX, step_name, EXEC)
        parameters = JCLHelper._concatenate_key_value_pairs_into_list(exec_dict)
        if parameters:
            return JCLHelper._add_parameters_onto_dd_statement(exec_string, parameters, False)
        else:
            return exec_string

    def _write_list_of_strings(self, jcl_lines):
        """Writes a list of strings to the JCL List.

        Parameters
        ----------
        jcl_lines : list
            The lines of JCL you want to append to your JCL string.
        """
        if isinstance(jcl_lines, list):
            self.jcl.extend(jcl_lines)
        else:
            self.jcl.append(jcl_lines)

    def _write_instream_data(self, dd_name, data):
        """Writes instream data to a DD Card and adds to JCL List.

        Parameters
        ----------
        dd_name : str
            The name of the DD Card
        data : dict
            The lines of data you want appended to the JCL, and passed into the DD card.
        """

        # Write the opening line of an instream data statement
        formatted_dd_name = JCLHelper._format_dd_name(dd_name)
        dd_line = (JCL_PREFIX + formatted_dd_name + DD_INSTREAM)
        content_for_instream = data.pop(CONTENT)
        parameters = JCLHelper._concatenate_key_value_pairs_into_list(data)
        dd_line = JCLHelper._add_parameters_onto_dd_statement(
            dd_line, parameters, True)
        self.jcl.append(dd_line)

        # Write the instream data
        self._write_list_of_strings(content_for_instream)
        # Apply different delimiter if necessary, or just the default
        self.jcl.append(data.pop(DLM, END_INSTREAM))

    def _write_dd_statement(self, dd_name, additional_parameters):
        """Writes and builds a DD statement to the JCL List

        Parameters
        ----------
        dd_name : str
            The name of the DD Card
        additional_parameters: dict
            A dict of key value pairs, E.g. {'PARM1':'one','PARM2':'two'}
        """

        dd_statement = self._build_dd_statement(dd_name, additional_parameters)
        if dd_statement:
            self._write_list_of_strings(
                JCLHelper._split_long_dd_statement(dd_statement))

    def _write_dd_concatenation(self, dd_name, additional_parameters):
        """Writes multiple data sets to a DD name and adds to JCL List

        Parameters
        ----------
        dd_name : str
            The name of the DD Card
        additional_parameters: dict
            A dict of key value pairs, E.g. {'PARM1':'one','PARM2':'two'}
        """

        dd_strings = self._build_dd_concatenation_list(
            dd_name, additional_parameters)
        self._write_list_of_strings(JCLHelper._split_long_dd_statement_list(dd_strings))

    def _write_null_statement(self):
        self.jcl.append(JCL_PREFIX)

    @staticmethod
    def _build_job_statement(job_parameters):
        positional_parameters = JCLHelper._format_job_positional_parameters(job_parameters)
        if job_parameters.get(MSGLEVEL):
            job_parameters[MSGLEVEL] = JCLHelper._format_msglevel_parameter(job_parameters[MSGLEVEL])
        # Put key values equal to one another
        job_name = job_parameters.pop(JOB_NAME)
        list_of_additional_parameters = JCLHelper._concatenate_key_value_pairs_into_list(job_parameters)

        job_string = '{0}{1}{2}'.format(JCL_PREFIX, JCLHelper._format_dd_name(job_name), JOB)
        if positional_parameters:
            job_string = '{0} {1}'.format(job_string, positional_parameters)
            return JCLHelper._add_parameters_onto_dd_statement(job_string, list_of_additional_parameters, True)
        return JCLHelper._add_parameters_onto_dd_statement(job_string, list_of_additional_parameters, False)

    @staticmethod
    def _format_job_positional_parameters(job_parameters):
        if job_parameters:
            accounting_info = JCLHelper._format_accounting_information(job_parameters.pop(ACCOUNTING_INFORMATION, None))
            programmer_name = JCLHelper._format_programmer_name(job_parameters.pop(PROGRAMMER_NAME, None))
            if programmer_name:
                return "{0},{1}".format(accounting_info, programmer_name)
            elif accounting_info or accounting_info != "":
                return accounting_info

    @staticmethod
    def _format_accounting_information(acc_information):
        if acc_information:
            # Putting into a list as keys need to be a specific order
            acc_values = [acc_information.get("pano"), acc_information.get("room"), acc_information.get("times"),
                          acc_information.get("lines"), acc_information.get("cards"), acc_information.get("forms"),
                          acc_information.get("copies"), acc_information.get("log"), acc_information.get("linect")]
            result = ""
            amount_of_values_added = 0
            for value in acc_values:
                if value:
                    amount_of_values_added += 1
                    result += str(value)
                # Needs a comma appending as we need to either seperate values or indicate missing keys.
                result += ','
            result = result.rstrip(",")
            if amount_of_values_added < 2:
                return result
            return "({0})".format(result)
        # Return an empty string so that if programmer name is set, we can handle the formatting nice and tidy.
        return ""

    @staticmethod
    def _format_programmer_name(programmer_name):
        if programmer_name:
            formatted_string = ""
            for char in programmer_name:
                if char == "'":
                    formatted_string += "''"  # Duplicate the apostrophe
                else:
                    formatted_string += char
            return "'{0}'".format(formatted_string)

    @staticmethod
    def _format_msglevel_parameter(msglevel_dict):
        msglevel_var = msglevel_dict.get("statements", "")
        messages = msglevel_dict.get("messages")
        if messages is not None:
            msglevel_var = "({0},{1})".format(msglevel_var, messages)
        return msglevel_var

    def _build_dd_concatenation_list(self, dd_name, list_of_dicts):
        # Get the dictionary in the list, to append a DD name.
        concatenation_of_statements = [
            self._build_dd_statement(dd_name, list_of_dicts[0])]

        # For the rest of the dd's, no DD name needed.
        for parameter_dict in list_of_dicts[1:]:
            parameters_string = JCLHelper._build_parameter_string(
                JCLHelper._concatenate_key_value_pairs_into_list(parameter_dict))
            current_line = '{0}{1} {2}'.format(
                CONCAT_JCL_PREFIX, DD, parameters_string)
            concatenation_of_statements.append(current_line)
        return concatenation_of_statements

    @staticmethod
    def _build_dd_statement(dd_name, additional_parameters=None):
        """Builds a DD Statement string from a DD Card name, and any additional parameters to follow

        Parameters
        ----------
        dd_name : str
            Name of the DD Card
        additional_parameters : dict
            A dict of key value pairs, E.g. {'PARM1':'one','PARM2':'two'}

        Returns
        -------
        str
            The built DD Statement from the name and any parameters.
        """
        if dd_name is None:
            return None
        dd_name = JCLHelper._format_dd_name(dd_name)
        parameters = JCLHelper._concatenate_key_value_pairs_into_list(
            additional_parameters)
        parameters_string = JCLHelper._build_parameter_string(parameters)
        dd_statement = '{0}{1}{2} {3}'.format(
            JCL_PREFIX, dd_name, DD, parameters_string)
        return dd_statement

    @staticmethod
    def _exceeds_line_length(dd_statement):
        return len(dd_statement) > MAX_LINE_LENGTH

    @staticmethod
    def _split_long_dd_statement_list(dd_statement_list):
        split_statement = []
        if isinstance(dd_statement_list, str):
            return JCLHelper._split_long_dd_statement(dd_statement_list)

        for statement in dd_statement_list:
            result = JCLHelper._split_long_dd_statement(statement)
            if isinstance(result, list):
                split_statement.extend(result)
            else:
                split_statement.append(result)
        return split_statement

    @staticmethod
    def _split_long_dd_statement(dd_statement):
        split_statement = []
        if JCLHelper._exceeds_line_length(dd_statement):
            words_in_statement = iter(dd_statement.split(","))
            current = next(words_in_statement)
            for word in words_in_statement:
                if len(current) + 2 + len(word) > MAX_LINE_LENGTH:
                    split_statement.append(current + ',')
                    current = JCL_PREFIX + NO_DD_NAME + word
                else:
                    current = '{0}{1}{2}'.format(current, ',', word)
            split_statement.append(current)
        else:
            split_statement.append(dd_statement)
        return split_statement

    @staticmethod
    def _add_parameters_onto_dd_statement(existing_dd_line, parameter_list, comma_prefix):
        if parameter_list:
            parameter_string = ','.join(parameter_list)
            joiner = "," if comma_prefix is True else " "
            existing_dd_line = '{0}{1}{2}'.format(
                existing_dd_line, joiner, parameter_string)
        return existing_dd_line

    @staticmethod
    def _format_dd_name(dd_name):
        return dd_name.upper().ljust(9, ' ')

    @staticmethod
    def _build_parameter_string(parameter_list):
        parameter_string = ""
        if parameter_list:
            parameter_string = ','.join(parameter_list)
        return parameter_string

    @staticmethod
    def _concatenate_key_value_pairs_into_list(dict_to_unpack):
        """Puts key and value equal to one another

        Parameters
        ----------
        dict_to_unpack : dict
            A dictionary of key value pairs to put equal to one another inside a list.
            If the value in the dictionary is another dictionary, it checks the dictionary length is a
            multiple of 2 and puts 2 values equal to one another. If value is blank, key will be stored
            on its own.
            E.g. {"PARAM1": "ONE", "SINGLE": "" } -> ["PARAM1=1", "SINGLE"]

        Returns
        -------
        list
            A list of name=values, E.g. ["PARAM1=ONE", "DUMMY", "SINGLE"]
        """
        list_of_pairs = []
        for k, v in dict_to_unpack.items():
            k = k.upper()
            if v == "":
                paired = k
            else:
                if k == GMTEXT:
                    v = JCLHelper._add_single_quotes_to_text(v)
                paired = '{0}={1}'.format(k, v)
            list_of_pairs.append(paired)
        return list_of_pairs

    @staticmethod
    def _add_single_quotes_to_text(value):
        if re.match("^\"\'([^']|\'\')*\'\"$", value):
            return value
        value = value.strip('"').strip("'")
        if "'" in value:
            value = value.replace("'", "''")

        return "'{0}'".format(value)
