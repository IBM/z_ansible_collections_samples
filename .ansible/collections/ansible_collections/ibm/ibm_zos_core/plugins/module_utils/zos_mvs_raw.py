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

from ansible_collections.ibm.ibm_zos_core.plugins.module_utils.dd_statement import (
    FileDefinition,
    DatasetDefinition,
    InputDefinition,
    OutputDefinition,
)
from ansible_collections.ibm.ibm_zos_core.plugins.module_utils.data_set import DataSet
from ansible_collections.ibm.ibm_zos_core.plugins.module_utils import (
    backup as zos_backup,
)


class MVSCmd(object):
    """Provides an interface to execute authorized and unauthorized MVS commands.
    """

    @staticmethod
    def execute(pgm, dds, parm="", debug=False, verbose=False, tmp_hlq=None):
        """Execute an unauthorized MVS command.

        Parameters
        ----------
            pgm : str
                The name of the program to execute.
            dds : list[DDStatement]
                A list of DDStatement objects.
            parm : str, optional)
                 Argument string if required by the program. Defaults to "".

        Returns
        -------
            MVSCmdResponse : object
                           The response of the command.
        """
        module = AnsibleModuleHelper(argument_spec={})
        command = "mvscmd {0} {1} {2} {3}".format(
            "-d" if debug else "",
            "-v" if verbose else "",
            "--tmphlq={0}".format(tmp_hlq.upper()) if tmp_hlq else "",
            MVSCmd._build_command(pgm, dds, parm),
        )
        rc, out, err = module.run_command(command, errors='replace')
        return MVSCmdResponse(rc, out, err)

    @staticmethod
    def execute_authorized(pgm, dds, parm="", debug=False, verbose=False, tmp_hlq=None):
        """Execute an authorized MVS command.

        Parameters
        ----------
            pgm : str
                The name of the program to execute.
            dds : list[DDStatement]
                A list of DDStatement objects.
            parm : str, optional
                 Argument string if required by the program. Defaults to "".
            tmp_hlq : str
                    The name of the temporary high level qualifier to use for temp data sets.

        Returns
        -------
            MVSCmdResponse : object
                           The response of the command.
        """
        module = AnsibleModuleHelper(argument_spec={})
        command = "mvscmdauth {0} {1} {2} {3} ".format(
            "-d" if debug else "",
            "-v" if verbose else "",
            "--tmphlq={0}".format(tmp_hlq.upper()) if tmp_hlq else "",
            MVSCmd._build_command(pgm, dds, parm),
        )
        rc, out, err = module.run_command(command, errors='replace')
        return MVSCmdResponse(rc, out, err)

    @staticmethod
    def _build_command(pgm, dds, parm):
        """Build the command string to be used by ZOAU mvscmd/mvscmdauth.

        Parameters
        ----------
            pgm : str
                [description]
            dds : list[DDStatement]
                A list of DDStatement objects.
            parm : str, optional
                 Argument string if required by the program. Defaults to "".

        Returns
        -------
            command : str
                    Command string formatted as expected by mvscmd/mvscmdauth.
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


class RawDatasetDefinition(DatasetDefinition):
    """Wrapper around DatasetDefinition to contain information about
    desired return contents.
    """

    def __init__(
        self,
        data_set_name,
        disposition="",
        disposition_normal=None,
        disposition_abnormal=None,
        space_type=None,
        space_primary=None,
        space_secondary=None,
        volumes=None,
        sms_management_class=None,
        sms_storage_class=None,
        sms_data_class=None,
        block_size=None,
        directory_blocks=None,
        key_label=None,
        type=None,
        encryption_key_1=None,
        encryption_key_2=None,
        key_length=None,
        key_offset=None,
        record_length=None,
        record_format=None,
        reuse=None,
        replace=None,
        backup=None,
        return_content=None,
        tmphlq=None,
        raw=False,
        **kwargs
    ):
        """
        DatasetDefinition (DatasetDefinition): Dataset DD data type to be used in a DDStatement.
        Parameters
        ----------
        Args:
            data_set_name : str
                          The name of the data set.
            disposition : str, optional
                        The disposition of the data set. Defaults to "".
            type : str, optional
                 The type of the data set. Defaults to None.
            space_primary : int, optional
                          The primary amount of space of the data set. Defaults to None.
            space_secondary : int, optional
                            The secondary amount of space of the data set. Defaults to None.
            space_type : str, optional
                       The unit of space to use for primary and secondary space. Defaults to None.
            disposition_normal : str, optional
                               What to do with the data set after normal termination of the program. Defaults to None.
            disposition_abnormal : str, optional
                                 What to do with the data set after abnormal termination of the program. Defaults to None.
            block_size : int, optional
                       The block size of the data set. Defaults to None.
            directory_blocks : int, optional
                             The number of directory blocks to allocate for the data set. Defaults to None.
            record_format : str, optional
                          The record format of the data set. Defaults to None.
            record_length : int, optional
                          The length, in bytes, of each record in the data set. Defaults to None.
            sms_storage_class : str, optional
                              The storage class for an SMS-managed dataset. Defaults to None.
            sms_data_class : str, optional
                           The data class for an SMS-managed dataset. Defaults to None.
            sms_management_class : str, optional
                                 The management class for an SMS-managed dataset. Defaults to None.
            key_length : int, optional
                       The key length of a record. Defaults to None.
            key_offset : int, optional
                       The key offset is the position of the first byte of the key
                       in each logical record of a the specified VSAM data set. Defaults to None.
            volumes : list, optional
                    A list of volume serials.. Defaults to None.
            key_label : str, optional
                      The label for the encryption key used by the system to encrypt the data set. Defaults to None.
            encryption_key_1 : dict, optional
                             The label for the key encrypting key used by the Encryption Key Manager and how the label
                             for the key encrypting key specified.
            encryption_key_2 : dict, optional
                             The label for the key encrypting key used by the Encryption Key Manager and how the label
                             for the key encrypting key specified
            reuse : bool, optional
                  Determines if data set should be reused. Defaults to None.
            replace : bool, optional
                    Determines if data set should be replaced. Defaults to None.
            backup : bool, optional
                   Determines if a backup should be made of existing data set when disposition=NEW, replace=true,
                   and a data set with the desired name is found.. Defaults to None.
            return_content : dict, optional
                           Determines how content should be returned to the user. Defaults to None.
            tmphlq : str, optional
                   HLQ to be used for temporary datasets. Defaults to None.
        ----------
        """
        self.raw = raw
        self.backup = None
        self.return_content = ReturnContent(**(return_content or {}))
        self.tmphlq = tmphlq
        if raw:
            super().__init__(
                dataset_name=data_set_name,
                raw=True
            )
            return
        primary_unit = space_type
        secondary_unit = space_type
        key_label1 = None
        key_encoding1 = None
        key_label2 = None
        key_encoding2 = None
        if encryption_key_1:
            if encryption_key_1.get("label"):
                key_label1 = encryption_key_1.get("label")
            if encryption_key_1.get("encoding"):
                key_encoding1 = encryption_key_1.get("encoding")
        if encryption_key_2:
            if encryption_key_2.get("label"):
                key_label2 = encryption_key_2.get("label")
            if encryption_key_2.get("encoding"):
                key_encoding2 = encryption_key_2.get("encoding")

        should_reuse = False
        if (reuse or replace) and DataSet.data_set_exists(data_set_name, volumes):
            if reuse:
                should_reuse = True
            elif replace:
                if backup:
                    self.backup = zos_backup.mvs_file_backup(data_set_name, None, tmphlq)
                DataSet.delete(data_set_name)

        if not should_reuse:
            super().__init__(
                dataset_name=data_set_name,
                disposition=disposition,
                type=type,
                primary=space_primary,
                primary_unit=primary_unit,
                secondary=space_secondary,
                secondary_unit=secondary_unit,
                normal_disposition=disposition_normal,
                conditional_disposition=disposition_abnormal,
                block_size=block_size,
                directory_blocks=directory_blocks,
                record_format=record_format,
                record_length=record_length,
                storage_class=sms_storage_class,
                data_class=sms_data_class,
                management_class=sms_management_class,
                key_length=key_length,
                key_offset=key_offset,
                volumes=volumes,
                dataset_key_label=key_label,
                key_label1=key_label1,
                key_encoding1=key_encoding1,
                key_label2=key_label2,
                key_encoding2=key_encoding2,
            )
        else:
            # TODO: determine if encoding labels are useful for existing data sets
            super().__init__(
                dataset_name=data_set_name,
                disposition="shr",
                type=type,
                normal_disposition=disposition_normal,
                conditional_disposition=disposition_abnormal,
                volumes=volumes,
                dataset_key_label=key_label,
                key_label1=key_label1,
                key_encoding1=key_encoding1,
                key_label2=key_label2,
                key_encoding2=key_encoding2,
            )

    def __str__(self):
        if self.raw:
            return f"{self.dataset_name},raw"
        return super().__str__()


class RawFileDefinition(FileDefinition):
    """Wrapper around FileDefinition to contain information about
    desired return contents.
    """

    def __init__(
        self,
        path,
        disposition_normal=None,
        disposition_abnormal=None,
        mode=None,
        status_group=None,
        access_group=None,
        file_data_type=None,
        block_size=None,
        record_length=None,
        record_format=None,
        return_content=None,
        **kwargs
    ):
        """
        FileDefinition (FileDefinition): File DD data type to be used in a DDStatement.
        Parameters
        ----------
            path : str
                 An absolute UNIX file path.
            disposition_normal : str, optional
                               What to do with path after normal program termination. Defaults to None.
            disposition_abnormal : str, optional
                                 What to do with path after abnormal program termination. Defaults to None.
            mode : int, optional
                 The file access attributes for the UNIX file being allocated. Defaults to None.
            access_group : str, optional
                         The access mode for UNIX file. Defaults to None.
            status_group : list[str], optional
                         The status for UNIX file being allocated. Defaults to None.
            file_data_type : str, optional
                           The type of data that is (or will be) stored in the UNIX file. Defaults to None.
            record_length : int, optional
                          The specified logical record length for the UNIX file. Defaults to None.
            block_size : int, optional
                       The specified block size for the UNIX file being allocated. Defaults to None.
            record_format : str, optional
                          The specified record format for the UNIX file. Defaults to None.
            return_content : dict, optional
                           Determines how content should be returned to the user. Defaults to None.
        """
        self.return_content = ReturnContent(**(return_content or {}))
        super().__init__(
            path_name=path,
            normal_disposition=disposition_normal,
            conditional_disposition=disposition_abnormal,
            path_mode=mode,
            access_group=access_group,
            status_group=status_group,
            file_data=file_data_type,
            record_length=record_length,
            block_size=block_size,
            record_format=record_format,
        )


class RawInputDefinition(InputDefinition):
    """Wrapper around InputDefinition to contain information about
    desired return contents.
    """

    def __init__(
            self,
            content="",
            return_content=None,
            tmphlq="",
            **kwargs
    ):
        """
        InputDefinition (InputDefinition): Input DD data type to be used in a DDStatement.
        Parameters
        ----------
            content : str
                    The content to write to temporary data set / stdin.
            return_content : dict, optional
                           Determines how content should be returned to the user. Defaults to {}.
        """
        self.return_content = ReturnContent(**(return_content or {}))
        super().__init__(
            content=content,
            tmphlq=tmphlq)


class RawOutputDefinition(OutputDefinition):
    """Wrapper around OutputDefinition to contain information about
    desired return contents.
    """

    def __init__(
            self,
            return_content=None,
            tmphlq="",
            **kwargs
    ):
        """
        OutputDefinition (OutputDefinition): Output DD data type to be used in a DDStatement.
        Parameters
        ----------
            content : str
                    The content to write to temporary data set / stdin.
            return_content : dict, optional
                           Determines how content should be returned to the user. Defaults to {}.
        """
        self.return_content = ReturnContent(**(return_content or {}))
        super().__init__(
            tmphlq=tmphlq
        )


class ReturnContent(object):
    """Holds information about what type of content
    should be returned for a particular DD, if any.
    """

    def __init__(self, type=None, src_encoding=None, response_encoding=None):
        """
        Parameters
        ----------
            type : str, optional
                 The type of content to return.
            src_encoding : str, optional
                         The encoding of the data set or file on the z/OS system.
            response_encoding : str, optional
                              The encoding to use when returning the contents of the data set or file.
        """
        self.type = type
        self.src_encoding = src_encoding
        self.response_encoding = response_encoding
