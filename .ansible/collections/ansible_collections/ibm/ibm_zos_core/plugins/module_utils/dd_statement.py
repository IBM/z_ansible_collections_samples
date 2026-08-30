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
import traceback
__metaclass__ = type

from ansible_collections.ibm.ibm_zos_core.plugins.module_utils.import_handler import (
    ZOAUImportError,
)

from ansible_collections.ibm.ibm_zos_core.plugins.module_utils.data_set import DataSet

try:
    from zoautil_py import datasets
except ImportError:
    datasets = ZOAUImportError(traceback.format_exc())

space_units = {"b": "", "kb": "k", "mb": "m", "gb": "g"}


class DDStatement(object):
    def __init__(self, name, definition):
        """A Python representation of a z/OS DD statement.

        Parameters
        ----------
        name : str
            The DD name to use for this DD statement.
        definition : Union[DataDefinition, list[DataDefinition]]
            One or more DataDefinition objects for the DD.

        Attributes
        ----------
        name : str
            The DD name to use for this DD statement.
        definition : Union[DataDefinition, list[DataDefinition]]
            One or more DataDefinition objects for the DD.

        Raises
        ------
        ValueError
            When a value other than a DataDefinition is provided for definition parameter.
        """
        self.name = name
        self.definition = definition
        self._assert_valid_definition()

    def get_mvscmd_string(self):
        """Build the string representing this DD statement
        to be used as part of mvscmd/mvscmdauth call.

        Returns
        -------
        str
            The string representation of this DD statement, as expected by mvscmd.
        """
        mvscmd_string = "--{0}=".format(self.name)
        if isinstance(self.definition, list):
            self._assert_valid_concatenation()
            if self.name.lower() != "steplib":
                dd_strings = [x.name + x._build_arg_string() for x in self.definition]
            else:
                dd_strings = [x.name for x in self.definition]
            mvscmd_string += ":".join(dd_strings)
        else:
            mvscmd_string += self.definition.name
            if self.name.lower() != "steplib":
                mvscmd_string += self.definition._build_arg_string()
        return mvscmd_string

    def _assert_valid_definition(self):
        """Assert that definition passed to DDStatement
        is valid.
        """
        if isinstance(self.definition, list):
            self._assert_valid_concatenation()
        else:
            self._assert_valid_data_definition()

    def _assert_valid_data_definition(self):
        """Assert that the provided single data set definition
        is not an invalid type.

        Raises
        ------
        ValueError
            When an invalid type is specified in DD concatenation.
        """
        if not isinstance(self.definition, DataDefinition):
            raise ValueError("DDStatement expects an object of type DataDefinition.")

    def _assert_valid_concatenation(self):
        """Assert that the provided data set concatenation does
        not contain any invalid types.

        Raises
        ------
        ValueError
            When an invalid type is specified in DD concatenation.
        """
        for dd in self.definition:
            if not isinstance(
                dd, (DatasetDefinition, FileDefinition, StdinDefinition),
            ):
                raise ValueError(
                    "Incorrect DataDefinition type specified on DD statement. Valid types are DatasetDefinition, FileDefinition and StdinDefinition."
                )


class DataDefinition(object):
    def __init__(self, name):
        """Generic DD data type to be used in a DDStatement.

        Parameters
        ----------
        name : str
            The name used to refer to the resource pointed to by the DD.

        Attributes
        ----------
        name : str
            The name used to refer to the resource pointed to by the DD.
        """
        self.name = name

    def _build_arg_string(self):
        """Build a string representing the arguments of this particular data type
        to be used by mvscmd/mvscmdauth.

        Raises
        ------
        NotImplementedError
            When the abstract version of the method is called.
        """
        raise NotImplementedError

    def _append_mvscmd_string(self, string, variable_name, variable):
        """Appends additional arguments to a formatted mvscmd DD name string.
           If no values are provided, returns string as provided.

        Parameters
        ----------
        string : str
            The string to append an argument to.
        variable_name : str
            The name of the argument to use as expected by mvscmd.
        variable : Union[str, int, list[str, int]]
            The argument value to append.

        Returns
        -------
        str
            The provided string with additional arguments appended.
        """
        if (
            variable is None
            or variable_name is None
            or (isinstance(variable, list) and len(variable) == 0)
        ):
            return string
        string += ",{0}=".format(variable_name)
        if isinstance(variable, list):
            string += ",".join([str(x) for x in variable])
        else:
            string += str(variable)
        return string


class FileDefinition(DataDefinition):
    def __init__(
        self,
        path_name,
        normal_disposition=None,
        conditional_disposition=None,
        path_mode=None,
        access_group=None,
        status_group=None,
        file_data=None,
        record_length=None,
        block_size=None,
        record_format=None,
    ):
        """File DD data type to be used in a DDStatement.
        Defaults and validation are handled my mvscmd.

        Parameters
        ----------
        path_name : str
            An absolute UNIX file path.
        normal_disposition : str, optional
            What to do with path after normal program termination.
            May be one of keep, delete.
            Defaults to None.
        conditional_disposition : str, optional
            What to do with path after abnormal program termination.
            May be one of keep, delete.
            Defaults to None.
        path_mode : Union[str, int], optional
            The file access attributes for the UNIX file.
            Provide in chmod-like number format. Defaults to None.
        access_group : str, optional
            The access mode for UNIX file.
            Options are: ORDWR, ORDONLY, OWRONLY.
            Defaults to None.
        status_group : list[str], optional
            The status for UNIX file.
            Specify up to 6 of: OCREAT, OEXCL, OAPPEND, ONOCTTY, ONONBLOCK, OSYNC, OTRUNC.
            Defaults to None.
        file_data : str, optional
            The type of data that is (or will be) stored in the UNIX file.
            Defaults to None.
        record_length : int, optional
            The specified logical record length for the
            UNIX file being allocated. This is required in situations where the data will be processed as
            records and therefore, the record length, block size and record format need to be supplied since
            a UNIX file would normally be treated as a stream of bytes.
            Defaults to None.
        block_size : int, optional
            The specified block size for the UNIX file
            being allocated since a UNIX file would normally
            be treated as a stream of bytes.
            Defaults to None.
        record_format : str, optional
            The specified record format for the UNIX file
            being allocated since an UNIX file would normally
            be treated as a stream of bytes.
            Defaults to None.

        Attributes
        ----------
        normal_disposition : str, optional
            What to do with path after normal program termination.
            May be one of keep, delete.
            Defaults to None.
        conditional_disposition : str, optional
            What to do with path after abnormal program termination.
            May be one of keep, delete.
            Defaults to None.
        path_mode : Union[str, int], optional
            The file access attributes for the UNIX file.
            Provide in chmod-like number format. Defaults to None.
        access_group : str, optional
            The access mode for UNIX file.
            Options are: ORDWR, ORDONLY, OWRONLY.
            Defaults to None.
        status_group : list[str], optional
            The status for UNIX file.
            Specify up to 6 of: OCREAT, OEXCL, OAPPEND, ONOCTTY, ONONBLOCK, OSYNC, OTRUNC.
            Defaults to None.
        file_data : str, optional
            The type of data that is (or will be) stored in the UNIX file.
            Defaults to None.
        record_length : int, optional
            The specified logical record length for the
            UNIX file being allocated. This is required in situations where the data will be processed as
            records and therefore, the record length, block size and record format need to be supplied since
            a UNIX file would normally be treated as a stream of bytes.
            Defaults to None.
        block_size : int, optional
            The specified block size for the UNIX file
            being allocated since a UNIX file would normally
            be treated as a stream of bytes.
            Defaults to None.
        record_format : str, optional
            The specified record format for the UNIX file
            being allocated since an UNIX file would normally
            be treated as a stream of bytes.
            Defaults to None.
        """
        super().__init__(path_name)
        self.normal_disposition = normal_disposition
        self.conditional_disposition = conditional_disposition
        self.path_mode = path_mode
        self.access_group = access_group
        self.status_group = status_group
        self.file_data = file_data
        self.record_length = record_length
        self.block_size = block_size
        self.record_format = record_format

    def _build_arg_string(self):
        """Build a string representing the arguments of this particular data type
        to be used by mvscmd/mvscmdauth.

        Returns
        -------
        str
            String to be used by mvscmd/mvscmdauth.
        """
        mvscmd_string = ""
        mvscmd_string = self._append_mvscmd_string(
            mvscmd_string, "normdisp", self.normal_disposition
        )
        mvscmd_string = self._append_mvscmd_string(
            mvscmd_string, "conddisp", self.conditional_disposition
        )
        mvscmd_string = self._append_mvscmd_string(
            mvscmd_string, "pathmode", self.path_mode
        )
        path_opts = []
        if self.status_group:
            path_opts = self.status_group
        if self.access_group:
            path_opts.append(self.access_group)
        mvscmd_string = self._append_mvscmd_string(
            mvscmd_string, "statusgroup", path_opts
        )
        mvscmd_string = self._append_mvscmd_string(
            mvscmd_string, "filedata", self.file_data
        )
        mvscmd_string = self._append_mvscmd_string(
            mvscmd_string, "lrecl", self.record_length
        )
        mvscmd_string = self._append_mvscmd_string(
            mvscmd_string, "blksize", self.block_size
        )
        mvscmd_string = self._append_mvscmd_string(
            mvscmd_string, "recfm", self.record_format
        )
        return mvscmd_string


class DatasetDefinition(DataDefinition):
    def __init__(
        self,
        dataset_name,
        disposition="",
        type=None,
        primary=None,
        primary_unit="TRK",
        secondary=None,
        secondary_unit="TRK",
        normal_disposition=None,
        conditional_disposition=None,
        block_size=None,
        directory_blocks=None,
        record_format=None,
        record_length=None,
        storage_class=None,
        data_class=None,
        management_class=None,
        key_length=None,
        key_offset=None,
        volumes=None,
        dataset_key_label=None,
        key_label1=None,
        key_encoding1=None,
        key_label2=None,
        key_encoding2=None,
        raw=False,
    ):
        """Dataset DD data type to be used in a DDStatement.
        Defaults and validation are handled my mvscmd.

        Parameters
        ----------
        dataset_name : str
            The name of the dataset to associate with the DD statement.
        disposition : str, optional
            The expected disposition of the dataset.
            Valid options are: EXCL, OLD, SHR, MOD, NEW.
            Defaults to "".
        type : str, optional
            The type of dataset.
            Valid options are: SEQ, BASIC, LARGE, PDS, PDSE, LIBRARY, LDS, RRDS, ESDS, KSDS.
            Defaults to None.
        primary : int, optional
            The amount of primary space to allocate for the dataset.
            Defaults to None.
        primary_unit : str, optional
            The unit of size to use when specifying primary space.
            May be one of: K or KB (kilobytes), M or MB (megabytes),
            G or GB (gigabytes), C or CYL (cylinders), T or TRK (tracks).
            Defaults to "TRK".
        secondary : int, optional
            The amount of secondary space to allocate for the dataset.
            Defaults to None.
        secondary_unit : str, optional
            The unit of size to use when specifying secondary space.
            May be one of: K or KB (kilobytes), M or MB (megabytes),
            G or GB (gigabytes), C or CYL (cylinders), T or TRK (tracks).
            Defaults to "TRK".
        normal_disposition : str, optional
            Tells the system what to do with the data set after normal termination of the program.
            Valid options are: delete, keep, catalog/catlg, uncatalog/uncatlg.
            Defaults to None.
        conditional_disposition : str, optional
            Tells the system what to do with the data set after abnormal termination of the program.
            Valid options are: delete, keep, catalog/catlg, uncatalog/uncatlg.
            Defaults to None.
        block_size : int, optional
            The block size of the data set.
            Defaults to None.
        directory_blocks : int, optional
            The number of directory blocks to allocate for the data set.
            Defaults to None.
        record_format : str, optional
            The record format of the dataset.
            Valid options are: FB, VB, FBA, VBA, U.
            Defaults to None.
        record_length : int, optional
            The length, in bytes, of each record in the data set.
            Defaults to None.
        storage_class : str, optional
            The storage class for an SMS-managed dataset.
            Not valid for datasets that are not SMS-managed.
            Note that all non-linear VSAM datasets are SMS-managed.
            Defaults to None.
        data_class : str, optional
            The data class for an SMS-managed dataset.
            Optional for SMS-managed datasets that do not match an SMS-rule.
            Not valid for datasets that are not SMS-managed.
            Note that all non-linear VSAM datasets are SMS-managed.
            Defaults to None.
        management_class : str, optional
            Is the management class for an SMS-managed dataset.
            Optional for SMS-managed datasets that do not match an SMS-rule.
            Not valid for datasets that are not SMS-managed.
            Note that all non-linear VSAM datasets are SMS-managed.
            Defaults to None.
        key_length : int, optional
            The key length of a record.
            Required for Key Sequenced Datasets (KSDS).
            Defaults to None.
        key_offset : int, optional
            The key offset is the position of the first byte of the key
            in each logical record of a the specified VSAM data set.
            If the key is at the beginning of the logical record, the offset is zero.
            Required for Key Sequenced Datasets (KSDS).
            Defaults to None.
        volumes : Union[str, list[str]], optional
            A list of volume serials.
            When providing multiple volumes, processing will begin with
            the first volume in the provided list. Offline volumes are not considered.
            Volumes can always be provided when not using SMS.
            When using SMS, volumes can be provided when the storage class being used
            has GUARANTEED_SPACE=YES specified. Otherwise, the allocation will fail.
            Defaults to None.
        dataset_key_label : str, optional
            The label for the encryption key used by the system to encrypt the data set.
            Only applicable when using encrypted datasets.
            Defaults to None.
        key_label1 : str, optional
            The label for the key encrypting key used by the Encryption Key Manager.
            Only applicable when using encrypted datasets.
            Defaults to None.
        key_encoding1 : str, optional
            How the label for the key encrypting key specified by keylab1 is encoded by the Encryption Key Manager.
            Valid values are: L, H.
            Only applicable when using encrypted datasets.
            Defaults to None.
        key_label2 : str, optional
            The label for the key encrypting key used by the Encryption Key Manager.
            Only applicable when using encrypted datasets.
            Defaults to None.
        key_encoding2 : str, optional
            How the label for the key encrypting key specified by keylab2 is encoded by the Encryption Key Manager.
            Valid values are: L, H
            Only applicable when using encrypted datasets.
            Defaults to None.

        Attributes
        ----------
        disposition : str
            The expected disposition of the dataset.
        type : str
            The type of dataset.
        primary : int
            The amount of primary space to allocate for the dataset.
        secondary : int
            The amount of secondary space to allocate for the dataset.
        normal_disposition : str
            tells the system what to do with the data set after normal termination of the program.
        conditional_disposition : str
            Tells the system what to do with the data set after abnormal termination of the program.
        block_size : int
            The block size of the data set.
        directory_blocks : int
            The number of directory blocks to allocate for the data set.
        record_format : str
            The record format of the dataset.
        record_length : int
            The length, in bytes, of each record in the data set.
        storage_class : str
            The storage class for an SMS-managed dataset.
            Not valid for datasets that are not SMS-managed.
            Note that all non-linear VSAM datasets are SMS-managed.
        data_class : str
            The data class for an SMS-managed dataset.
            Optional for SMS-managed datasets that do not match an SMS-rule.
            Not valid for datasets that are not SMS-managed.
            Note that all non-linear VSAM datasets are SMS-managed.
        management_class : str
            Is the management class for an SMS-managed dataset.
            Optional for SMS-managed datasets that do not match an SMS-rule.
            Not valid for datasets that are not SMS-managed.
            Note that all non-linear VSAM datasets are SMS-managed.
        key_length : int
            The key length of a record.
            Required for Key Sequenced Datasets (KSDS).
        key_offset : int
            The key offset is the position of the first byte of the key
            in each logical record of a the specified VSAM data set.
            If the key is at the beginning of the logical record, the offset is zero.
            Required for Key Sequenced Datasets (KSDS).
        volumes : Union[str, list[str]]
            A list of volume serials.
            When providing multiple volumes, processing will begin with
            the first volume in the provided list. Offline volumes are not considered.
            Volumes can always be provided when not using SMS.
            When using SMS, volumes can be provided when the storage class being used
            has GUARANTEED_SPACE=YES specified. Otherwise, the allocation will fail.
        dataset_key_label : str
            The label for the encryption key used by the system to encrypt the data set.
            Only applicable when using encrypted datasets.
        key_label1 : str
            The label for the key encrypting key used by the Encryption Key Manager.
            Only applicable when using encrypted datasets.
        key_encoding1 : str
            How the label for the key encrypting key specified by keylab1 is encoded by the Encryption Key Manager.
            Valid values are: L, H
            Only applicable when using encrypted datasets.
        key_label2 : str
            The label for the key encrypting key used by the Encryption Key Manager.
            Only applicable when using encrypted datasets.
        key_encoding2 : str
            How the label for the key encrypting key specified by keylab2 is encoded by the Encryption Key Manager.
            Valid values are: L, H
            Only applicable when using encrypted datasets.
        """
        super().__init__(dataset_name)
        self.disposition = disposition
        self.type = type
        self.raw = raw
        if not raw:
            if primary_unit and space_units.get(primary_unit.lower()) is not None:
                primary_unit = space_units.get(primary_unit.lower())
            if secondary_unit and space_units.get(secondary_unit.lower()) is not None:
                secondary_unit = space_units.get(secondary_unit.lower())
            if primary and primary_unit:
                self.primary = str(primary) + primary_unit
            else:
                self.primary = primary
            if secondary and secondary_unit:
                self.secondary = str(secondary) + secondary_unit
            else:
                self.secondary = secondary
        else:
            self.primary = None
            self.secondary = None

        DISPOSITION_ARG_MAP = {"catlg": "catalog", "uncatlg": "uncatalog"}
        self.normal_disposition = (
            DISPOSITION_ARG_MAP.get(normal_disposition.lower(), normal_disposition)
            if normal_disposition
            else None
        )
        self.conditional_disposition = (
            DISPOSITION_ARG_MAP.get(
                conditional_disposition.lower(), conditional_disposition
            )
            if conditional_disposition
            else None
        )
        self.block_size = block_size
        self.directory_blocks = directory_blocks
        self.record_format = record_format
        self.record_length = record_length
        self.storage_class = storage_class
        self.data_class = data_class
        self.management_class = management_class
        self.key_length = key_length
        self.key_offset = key_offset
        self.volumes = volumes
        self.dataset_key_label = dataset_key_label
        self.key_label1 = key_label1
        self.key_encoding1 = key_encoding1
        self.key_label2 = key_label2
        self.key_encoding2 = key_encoding2

    def _build_arg_string(self):
        """Build a string representing the arguments of this particular data type
        to be used by mvscmd/mvscmdauth.

        Returns
        -------
        str
            String to be used by mvscmd/mvscmdauth.
        """
        if self.raw:
            mvscmd_string = ",{0}".format(self.disposition) if self.disposition else ""
            mvscmd_string += ",raw"
            return mvscmd_string
        else:
            if not self.disposition:
                return ""
            mvscmd_string = ",{0}".format(self.disposition) if self.disposition else ""
            mvscmd_string = self._append_mvscmd_string(mvscmd_string, "type", self.type)
            mvscmd_string = self._append_mvscmd_string(
                mvscmd_string, "primary", self.primary
            )
            mvscmd_string = self._append_mvscmd_string(
                mvscmd_string, "secondary", self.secondary
            )
            mvscmd_string = self._append_mvscmd_string(
                mvscmd_string, "normdisp", self.normal_disposition
            )
            mvscmd_string = self._append_mvscmd_string(
                mvscmd_string, "conddisp", self.conditional_disposition
            )
            mvscmd_string = self._append_mvscmd_string(
                mvscmd_string, "blksize", self.block_size
            )
            mvscmd_string = self._append_mvscmd_string(
                mvscmd_string, "dirblks", self.directory_blocks
            )
            mvscmd_string = self._append_mvscmd_string(
                mvscmd_string, "recfm", self.record_format
            )
            mvscmd_string = self._append_mvscmd_string(
                mvscmd_string, "lrecl", self.record_length
            )
            mvscmd_string = self._append_mvscmd_string(
                mvscmd_string, "storclas", self.storage_class
            )
            mvscmd_string = self._append_mvscmd_string(
                mvscmd_string, "dataclas", self.data_class
            )
            mvscmd_string = self._append_mvscmd_string(
                mvscmd_string, "mgmtclas", self.management_class
            )
            mvscmd_string = self._append_mvscmd_string(
                mvscmd_string, "keylen", self.key_length
            )
            mvscmd_string = self._append_mvscmd_string(
                mvscmd_string, "keyoffset", self.key_offset
            )
            mvscmd_string = self._append_mvscmd_string(
                mvscmd_string, "volumes", self.volumes
            )
            mvscmd_string = self._append_mvscmd_string(
                mvscmd_string, "dskeylbl", self.dataset_key_label
            )
            mvscmd_string = self._append_mvscmd_string(
                mvscmd_string, "keylab1", self.key_label1
            )
            mvscmd_string = self._append_mvscmd_string(
                mvscmd_string, "keylab2", self.key_label2
            )
            mvscmd_string = self._append_mvscmd_string(
                mvscmd_string, "keycd1", self.key_encoding1
            )
            mvscmd_string = self._append_mvscmd_string(
                mvscmd_string, "keycd2", self.key_encoding2
            )
            return mvscmd_string


class VolumeDefinition(DataDefinition):
    def __init__(self, volume_name, unit, disposition):
        """
        Volume DD data type to be used in a DDStatement.
        Parameters
        ----------
        volume_name : str
            The volume name to associate with the DD statement.
        unit : str
            The unit of measurement to use when defining the volume.
        disposition : str
            The disposition of the volume.
        """
        super().__init__(volume_name)
        self.unit = unit
        self.disposition = disposition

    def _build_arg_string(self):
        """Build a string representing the arguments of this particular data type
        to be used by mvscmd/mvscmdauth.

        Returns
        -------
        str
            ',vol'
        """
        mvscmd_string = ",vol"
        mvscmd_string = self._append_mvscmd_string(mvscmd_string, "unit", self.unit)
        if self.disposition:
            mvscmd_string += f",{self.disposition}"
        return mvscmd_string


class StdoutDefinition(DataDefinition):
    def __init__(self):
        """Stdout DD data type to be used in a DDStatement.
        """
        super().__init__("*")

    def _build_arg_string(self):
        """Build a string representing the arguments of this particular data type
        to be used by mvscmd/mvscmdauth.

        Returns
        -------
        str
            ''
        """
        return ""


class DummyDefinition(DataDefinition):
    def __init__(self):
        """DUMMY DD data type to be used in a DDStatement.
        """
        super().__init__("DUMMY")

    def _build_arg_string(self):
        """Build a string representing the arguments of this particular data type
        to be used by mvscmd/mvscmdauth.

        Returns
        -------
        str
            ''
        """
        return ""


class StdinDefinition(DataDefinition):
    def __init__(
        self,
        content,
        tmphlq="",
        record_format="FB",
        space_primary=5,
        space_secondary=5,
        space_type="M",
        record_length=80,
    ):
        """Stdin DD Data type to be used in a DDStatement.
        This should be used in cases where "DD *" would be used in a jcl.

        Parameters
        ----------
        content : Union[str, list[str]]
            The content to write to temporary data set / stdin.
            Content can be provided as a string or a list of strings where each list item
            corresponds to a single line.
        record_format : str, optional
            The record format to use for the dataset.
            Valid options are: FB, VB, FBA, VBA, U.
            Defaults to "FB".
        space_primary : int, optional
            The amount of primary space to allocate for the dataset.
            Defaults to 5.
        space_secondary : int, optional
            The amount of secondary space to allocate for the dataset.
            Defaults to 5.
        space_type : str, optional
            The unit of measurement to use when defining primary and secondary space.
            Defaults to "M".
        record_length : int, optional
            The length, in bytes, of each record in the data set.
            Defaults to 80.
        """
        self.name = None
        name = DataSet.create_temp(
            hlq=tmphlq,
            record_format=record_format,
            space_primary=space_primary,
            space_secondary=space_secondary,
            space_type=space_type,
            record_length=record_length,
        )
        super().__init__(name)
        if isinstance(content, list):
            content = "\n".join(content)
        DataSet.write(name, content)

    def __del__(self):
        """Delete dataset with the name of this object
        """
        if self.name:
            DataSet.delete(self.name)

    def _build_arg_string(self):
        """Build a string representing the arguments of this particular data type
        to be used by mvscmd/mvscmdauth.

        Returns
        -------
        str
            ''
        """
        return ""


class InputDefinition(StdinDefinition):
    """Input DD Data type to be used in a DDStatement.
    This should be used in cases where "DD *" would be used in a jcl.
    Added for consistent naming with OutputDefinition, is exact same as StdinDefinition.

    Parameters
    ----------
    content : Union[str, list[str]]
        The content to write to temporary data set / stdin.
        Content can be provided as a string or a list of strings where each list item
        corresponds to a single line.
    record_format : str, optional
        The record format to use for the dataset.
        Valid options are: FB, VB, FBA, VBA, U.
        Defaults to "FB".
    space_primary : int, optional
        The amount of primary space to allocate for the dataset.
        Defaults to 5.
    space_secondary : int, optional
        The amount of secondary space to allocate for the dataset.
        Defaults to 5.
    space_type : str, optional
        The unit of measurement to use when defining primary and secondary space.
        Defaults to "M".
    record_length _ int, optional
        The length, in bytes, of each record in the data set.
        Defaults to 80.
    """

    pass


class OutputDefinition(DataDefinition):
    def __init__(
        self,
        tmphlq="",
        record_format="FB",
        space_primary=100,
        space_secondary=50,
        space_type="trk",
        record_length=121,
    ):
        """Output DD Data type to be used in a DDStatement.
        This should be used in cases where user wants to receive
        output from a program but does not want to store in a
        persistent data set or file.

        Parameters
        ----------
        record_format : str, optional
            The record format to use for the dataset.
            Valid options are: FB, VB, FBA, VBA, U.
            Defaults to "VB".
        space_primary : int, optional
            The amount of primary space to allocate for the dataset.
            Defaults to 5.
        space_secondary : int, optional
            The amount of secondary space to allocate for the dataset.
            Defaults to 5.
        space_type : str, optional
            The unit of measurement to use when defining primary and secondary space.
            Defaults to "M".
        record_length : int, optional
            The length, in bytes, of each record in the data set.
            Defaults to 80.
        """
        self.name = None
        name = DataSet.create_temp(
            hlq=tmphlq,
            record_format=record_format,
            space_primary=space_primary,
            space_secondary=space_secondary,
            space_type=space_type,
            record_length=record_length,
        )
        super().__init__(name)

    def __del__(self):
        """Delete dataset with the name of this object
        """
        if self.name:
            DataSet.delete(self.name)

    def _build_arg_string(self):
        """Build a string representing the arguments of this particular data type
        to be used by mvscmd/mvscmdauth.

        Returns
        -------
        str
            ''
        """
        return ""


class VIODefinition(DataDefinition):
    def __init__(self, tmphlq=None):
        """VIO DD type to be used in a DDStatement.
        VIO uses DASD space and system I/O more efficiently than other temporary data sets.
        A temporary data set will be created for use in cases where VIO is unavailable.
        Defaults for VIODefinition should be sufficient.

        Parameters
        ----------
        tmphlq : str, optional
            HLQ to be used for temporary datasets. Defaults to None.
        """
        if tmphlq:
            hlq = tmphlq
        else:
            hlq = datasets.get_hlq()
        name = datasets.tmp_name(high_level_qualifier=hlq)
        super().__init__(name)

    def __del__(self):
        """Try to delete the temporary data set
        if VIO wrote to disk during execution.
        """
        try:
            DataSet.delete(self.name)
        except DataSet.DatasetDeleteError:
            pass

    def _build_arg_string(self):
        """Build a string representing the arguments of this particular data type
        to be used by mvscmd/mvscmdauth.

        Returns
        -------
        str
            ',vio'
        """
        return ",vio"
