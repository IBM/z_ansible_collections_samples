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

import re
import tempfile
import traceback
from os import path, walk, environ
from random import sample
from string import ascii_uppercase, digits

# from ansible.module_utils._text import to_bytes
from ansible.module_utils.common.text.converters import to_bytes
from ansible_collections.ibm.ibm_zos_core.plugins.module_utils import (
    better_arg_parser, mvs_cmd)
from ansible_collections.ibm.ibm_zos_core.plugins.module_utils.ansible_module import \
    AnsibleModuleHelper
from ansible_collections.ibm.ibm_zos_core.plugins.module_utils.import_handler import (
    MissingImport, ZOAUImportError)

try:
    from ansible_collections.ibm.ibm_zos_core.plugins.module_utils import vtoc
except ImportError:
    vtoc = MissingImport("vtoc")

try:
    from zoautil_py import datasets, exceptions, gdgs, mvscmd, ztypes
    from zoautil_py.exceptions import GenerationDataGroupCreateException
except ImportError:
    datasets = ZOAUImportError(traceback.format_exc())
    exceptions = ZOAUImportError(traceback.format_exc())
    gdgs = ZOAUImportError(traceback.format_exc())
    mvscmd = ZOAUImportError(traceback.format_exc())
    ztypes = ZOAUImportError(traceback.format_exc())
    GenerationDataGroupCreateException = ZOAUImportError(traceback.format_exc())


class DataSet(object):
    """Perform various data set operations such as creation, deletion and cataloging."""

    # Module args mapped to equivalent ZOAU data set create args
    _ZOAU_DS_CREATE_ARGS = {
        "name": "name",
        "type": "type",
        "space_primary": "primary_space",
        "space_secondary": "secondary_space",
        "record_format": "record_format",
        "sms_storage_class": "storage_class_name",
        "sms_data_class": "data_class_name",
        "sms_management_class": "management_class_name",
        "record_length": "record_length",
        "key_offset": "key_offset",
        "key_length": "key_length",
        "block_size": "block_size",
        "directory_blocks": "directory_blocks",
        "volumes": "volumes",
    }

    _VSAM_CATALOG_COMMAND_NOT_INDEXED = """ DEFINE CLUSTER -
    (NAME('{0}') -
    VOLUMES({1}) -
    RECATALOG {2}) -
    DATA(NAME('{0}.DATA'))
    """

    _VSAM_CATALOG_COMMAND_INDEXED = """ DEFINE CLUSTER -
    (NAME('{0}') -
    VOLUMES({1}) -
    RECATALOG {2}) -
    DATA(NAME('{0}.DATA')) -
    INDEX(NAME('{0}.INDEX'))
    """

    _NON_VSAM_UNCATALOG_COMMAND = " UNCATLG DSNAME={0}"

    _VSAM_UNCATALOG_COMMAND = " DELETE '{0}' NOSCRATCH"

    MVS_PARTITIONED = frozenset({"PE", "PO", "PDSE", "PDS"})
    MVS_SEQ = frozenset({"PS", "SEQ", "BASIC"})
    MVS_VSAM = frozenset({"KSDS", "ESDS", "RRDS", "LDS", "VSAM"})

    @staticmethod
    def ensure_present(
        name,
        replace,
        type,
        raw_name=None,
        space_primary=None,
        space_secondary=None,
        space_type=None,
        record_format=None,
        record_length=None,
        block_size=None,
        directory_blocks=None,
        key_length=None,
        key_offset=None,
        sms_storage_class=None,
        sms_data_class=None,
        sms_management_class=None,
        volumes=None,
        tmp_hlq=None,
        force=None,
    ):
        """Creates data set if it does not already exist.

        Parameters
        ----------
        name : str
            The name of the dataset.
        raw_name : str
            Original name without escaping or gds name resolve operations performed.
        replace : bool
            Used to determine behavior when data set already exists.
        type : str, optional
            The type of dataset.
            Valid options are: SEQ, BASIC, LARGE, PDS, PDSE, LIBRARY, LDS, RRDS, ESDS, KSDS.
            Defaults to None.
        space_primary : int, optional
            The amount of primary space to allocate for the dataset.
            Defaults to None.
        space_secondary : int, optional
            The amount of secondary space to allocate for the dataset.
            Defaults to None.
        space_type : str, optional
            The unit of measurement to use when defining primary and secondary space.
            Defaults to None.
        record_format : str, optional
            The record format to use for the dataset.
            Valid options are: F, FB, VB, FBA, VBA, U.
            Defaults to None.
        record_length : int, optional
            The length, in bytes, of each record in the data set.
            Defaults to None.
        block_size : int, optional
            The block size to use for the data set.
            Defaults to None.
        directory_blocks : int, optional
            The number of directory blocks to allocate to the data set.
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
        sms_storage_class : str, optional
            The storage class for an SMS-managed dataset.
            Required for SMS-managed datasets that do not match an SMS-rule.
            Not valid for datasets that are not SMS-managed.
            Note that all non-linear VSAM datasets are SMS-managed.
            Defaults to None.
        sms_data_class : str, optional
            The data class for an SMS-managed dataset.
            Optional for SMS-managed datasets that do not match an SMS-rule.
            Not valid for datasets that are not SMS-managed.
            Note that all non-linear VSAM datasets are SMS-managed.
            Defaults to None.
        sms_management_class : str, optional
            The management class for an SMS-managed dataset.
            Optional for SMS-managed datasets that do not match an SMS-rule.
            Not valid for datasets that are not SMS-managed.
            Note that all non-linear VSAM datasets are SMS-managed.
            Defaults to None.
        volumes : Union[str, list[str]], optional
            A list of volume serials.
            When providing multiple volumes, processing will begin with
            the first volume in the provided list. Offline volumes are not considered.
            Volumes can always be provided when not using SMS.
            When using SMS, volumes can be provided when the storage class being used
            has GUARANTEED_SPACE=YES specified. Otherwise, the allocation will fail.
            Defaults to None.
        tmp_hlq : str, optional
            High level qualifier for temporary datasets.
        force : bool, optional
            Used to determine behavior when performing member operations on a PDSE.
            Defaults to None.

        Returns
        -------
        bool
            Indicates if changes were made.

        Raises
        ------
        DatasetCreateError
            When data set creation fails.
        """
        arguments = locals()
        arguments.pop("replace", None)
        present = False
        changed = False
        data_set = None
        if DataSet.data_set_cataloged(name, tmphlq=tmp_hlq):
            present = True
        # Validate volume conflicts when:
        # 1. Dataset exists in catalog (present=True).
        # 2. User hasn't requested replacement (replace=False).
        # 3. Specific volumes were requested (volumes parameter provided).
        if present and not replace and volumes:
            cataloged_volumes = DataSet.data_set_cataloged_volume_list(name, tmphlq=tmp_hlq)
            requested_volumes = [vol.upper() for vol in volumes]
            if not any(vol.upper() in requested_volumes for vol in cataloged_volumes):
                raise DatasetCatalogedOnDifferentVolumeError(
                    name=name,
                    existing_volumes=cataloged_volumes,
                    requested_volumes=volumes
                )

        if not present:
            try:
                changed, data_set = DataSet.create(**arguments)
            except DatasetCreateError as e:
                raise_error = True
                # data set exists on volume
                if "DatasetVerificationError" in e.msg or "Error Code: 0x4704" in e.msg:
                    present, changed = DataSet.attempt_catalog_if_necessary(
                        name, volumes, tmphlq=tmp_hlq
                    )
                    if present and changed:
                        raise_error = False
                if raise_error:
                    raise
        if present:
            if not replace:
                return changed, data_set
            changed, data_set = DataSet.replace(**arguments)
        if type.upper() == "ZFS":
            DataSet.format_zfs(name)
        return changed, data_set

    @staticmethod
    def ensure_absent(name, volumes=None, tmphlq=None, noscratch=False):
        """Deletes provided data set if it exists.

        Parameters
        ----------
        name : str
            The name of the data set to ensure is absent.
        volumes : list[str]
            The volumes the data set may reside on.
        tmphlq : str
            High Level Qualifier for temporary datasets.
        noscratch : bool
            If True, the data set is uncataloged but not physically removed from the volume.

        Returns
        -------
        bool
            Indicates if changes were made.
        """
        changed, present = DataSet.attempt_catalog_if_necessary_and_delete(name, volumes, tmphlq=tmphlq, noscratch=noscratch)
        return changed

    # ? should we do additional check to ensure member was actually created?
    @staticmethod
    def ensure_member_present(name, replace=False, tmphlq=None):
        """Creates data set member if it does not already exist.

        Parameters
        ----------
        name : str
            The name of the data set to ensure is present.
        replace : bool
            Used to determine behavior when data set already
            exists.
        tmphlq : str
            High Level Qualifier for temporary datasets.

        Returns
        -------
        bool
            Indicates if changes were made.
        """
        if DataSet.data_set_member_exists(name):
            if not replace:
                return False
            DataSet.delete_member(name)
        DataSet.create_member(name, tmphlq=tmphlq)
        return True

    @staticmethod
    def ensure_member_absent(name, force=False):
        """Deletes provided data set member if it exists.
        Returns a boolean indicating if changes were made.

        Parameters
        ----------
        force : bool
            Mode to execute.

        Returns
        -------
        bool
            True if the data set member exists.
        """
        if DataSet.data_set_member_exists(name):
            DataSet.delete_member(name, force)
            return True
        return False

    @staticmethod
    def ensure_cataloged(name, volumes, tmphlq=None):
        """Ensure a data set is cataloged. Data set can initially
        be in cataloged or uncataloged state when this function is called.

        Parameters
        ----------
        name : str
            The data set name to ensure is cataloged.
        volume : str
            The volume on which the data set should exist.
        tmphlq : str
            High Level Qualifier for temporary datasets.

        Returns
        -------
        bool
            If changes were made.
        """
        if DataSet.data_set_cataloged(name, None, tmphlq=tmphlq):
            return False
        try:
            DataSet.catalog(name, volumes, tmphlq=tmphlq)
        except DatasetCatalogError:
            raise DatasetCatalogError(
                name, volumes, "-1", "Data set was not found. Unable to catalog."
            )
        return True

    @staticmethod
    def ensure_uncataloged(name, tmphlq=None):
        """Ensure a data set is uncataloged. Data set can initially
        be in cataloged or uncataloged state when this function is called.

        Parameters
        ----------
        name : str
            The data set name to ensure is uncataloged.
        tmphlq : str
            High Level Qualifier for temporary datasets.

        Returns
        -------
        bool
            If changes were made.
        """
        if DataSet.data_set_cataloged(name, tmphlq=tmphlq):
            DataSet.uncatalog(name, tmphlq=tmphlq)
            return True
        return False

    @staticmethod
    def allocate_model_data_set(ds_name, model, executable=False, asa_text=False, vol=None, tmphlq=None):
        """Allocates a data set based on the attributes of a 'model' data set.
        Useful when a data set needs to be created identical to another. Supported
        model(s) are Physical Sequential (PS), Partitioned Data Sets (PDS/PDSE),
        and VSAM data sets. If `ds_name` has a member (i.e., "DATASET(member)"),
        it will be shortened to just the partitioned data set name.

        Parameters
        ----------
        ds_name : str
            The name of the data set to allocate. If the ds_name
            is a partitioned member e.g. hlq.llq.ds(mem), only the data set name
            must be used. See extract_dsname(ds_name) in data_set.py.
        model : str
            The name of the data set whose allocation parameters
            should be used to allocate the new data set 'ds_name'.
        executable : bool
            Whether the new data set should support executables.
        asa_text : bool
            Whether the new data set should support ASA control
            characters (have record format FBA).
        vol : str
            The volume where data set should be allocated.
        tmphlq : str
            High Level Qualifier for temporary datasets.

        Raise
        -----
        NonExistentSourceError
            When the model data set does not exist.
        MVSCmdExecError
            When the call to IKJEFT01 to allocate the
            data set fails.

        """
        if not DataSet.data_set_exists(model, tmphlq=tmphlq):
            raise DatasetNotFoundError(model)

        ds_name = extract_dsname(ds_name)
        model_type = DataSet.data_set_type(model, tmphlq=tmphlq)

        # The break lines are absolutely necessary, a JCL code line can't
        # be longer than 72 characters. The following JCL is compatible with
        # all data set types.
        alloc_cmd = """ ALLOC DS('{0}') -
        LIKE ('{1}')""".format(ds_name, model)

        # Now adding special parameters for sequential and partitioned
        # data sets.
        if model_type not in DataSet.MVS_VSAM:
            try:
                data_set = datasets.list_datasets(model)[0]
            except IndexError:
                raise AttributeError("Could not retrieve model data set block size.")
            block_size = data_set.block_size
            alloc_cmd = """{0} -
            BLKSIZE({1})""".format(alloc_cmd, block_size)

        if vol:
            alloc_cmd = """{0} -
            VOLUME({1})""".format(alloc_cmd, vol.upper())

        if asa_text:
            alloc_cmd = """{0} -
            RECFM(F,B,A)""".format(alloc_cmd)

        if executable:
            alloc_cmd = """{0} -
            RECFM(U) -
            DSNTYPE(LIBRARY)""".format(alloc_cmd)

        rc, out, err = mvs_cmd.ikjeft01(alloc_cmd, authorized=True, tmphlq=tmphlq)
        if rc != 0:
            raise MVSCmdExecError(rc, out, err)

    @staticmethod
    def allocate_gds_model_data_set(ds_name, model, executable=False, asa_text=False, vol=None, tmphlq=None):
        """
        Allocates a new current generation of a generation data group using a model
        data set to set its attributes.

        Parameters
        ----------
        ds_name : str
            Name of the data set that will be allocated. It must be a GDS
            relative name.
        model : str
            The name of the data set whose allocation parameters
            should be used to allocate the new data set.
        executable : bool, optional
            Whether the new data set should support executables.
        asa_text : bool, optional
            Whether the new data set should support ASA control
            characters (have record format FBA).
        vol : str, optional
            The volume where the new data set should be allocated.
        tmphlq : str
            High Level Qualifier for temporary datasets.

        Returns
        -------
        str
            Absolute name of the newly allocated generation data set.

        Raises
        ------
        DatasetCreateError
            When the allocation fails.
        """
        model_attributes = datasets.list_datasets(model)[0]
        dataset_type = model_attributes.organization
        record_format = model_attributes.record_format

        if executable:
            dataset_type = "library"
        elif dataset_type in DataSet.MVS_SEQ:
            dataset_type = "seq"
        elif dataset_type in DataSet.MVS_PARTITIONED:
            dataset_type = "pdse"

        if asa_text:
            record_format = "fba"
        elif executable:
            record_format = "u"

        data_set_object = MVSDataSet(
            name=ds_name,
            data_set_type=dataset_type,
            state="absent",
            record_format=record_format,
            volumes=vol,
            block_size=model_attributes.block_size,
            record_length=model_attributes.record_length,
            space_primary=model_attributes.total_space,
            space_type=''
        )

        success = data_set_object.ensure_present(tmp_hlq=tmphlq)
        if not success:
            raise DatasetCreateError(
                data_set=ds_name,
                msg=f"Error while trying to allocate {ds_name}."
            )

    @staticmethod
    def data_set_cataloged(name, volumes=None, tmphlq=None):
        """Determine if a data set is in catalog.

        Parameters
        ----------
        name : str
            The data set name to check if cataloged.
        volume : str
            The volume the data set may reside on.
        tmphlq : str
            High Level Qualifier for temporary datasets.

        Returns
        -------
        bool
            If data is is cataloged.

        """

        # Resolve GDS names before passing it into listcat
        if DataSet.is_gds_relative_name(name):
            try:
                name = DataSet.resolve_gds_absolute_name(name)
            except GDSNameResolveError:
                # if GDS name cannot be resolved, it's not in the catalog.
                return False

        # We need to unescape because this calls to system can handle
        # special characters just fine.
        name = name.upper().replace("\\", '')

        module = AnsibleModuleHelper(argument_spec={})
        stdin = " LISTCAT ENTRIES('{0}')".format(name)

        cmd = "mvscmdauth --pgm=idcams --sysprint=* --sysin=stdin"
        if tmphlq:
            cmd = "{0} -Q={1}".format(cmd, tmphlq)

        rc, stdout, stderr = module.run_command(
            cmd,
            data=stdin,
            errors='replace'
        )

        # The above 'listcat entries' command to idcams returns:
        # rc=0 if data set found in catalog
        # rc=4 if data set NOT found in catalog
        # rc>4 for other errors
        if rc > 4:
            raise MVSCmdExecError(rc, stdout, stderr)

        if volumes:
            cataloged_volume_list = DataSet.data_set_cataloged_volume_list(name, tmphlq=tmphlq) or []
            if bool(set(volumes) & set(cataloged_volume_list)):
                return True
        else:
            if re.search(r"-\s" + re.escape(name) + r"\s*\n\s+IN-CAT", stdout):
                return True

        return False

    @staticmethod
    def data_set_cataloged_volume_list(name, tmphlq=None):
        """Get the volume list for a cataloged dataset name.

        Parameters
        ----------
        name : str
            The data set name to check if cataloged.
        tmphlq : str
            High Level Qualifier for temporary datasets.

        Returns
        -------
        Union[str]
            A list of volumes where the dataset is cataloged.

        """
        name = name.upper()
        module = AnsibleModuleHelper(argument_spec={})
        stdin = " LISTCAT ENTRIES('{0}') ALL".format(name)

        cmd = "mvscmdauth --pgm=idcams --sysprint=* --sysin=stdin"
        if tmphlq:
            cmd = "{0} -Q={1}".format(cmd, tmphlq)

        rc, stdout, stderr = module.run_command(
            cmd,
            data=stdin,
            errors='replace'
        )
        # The above 'listcat entries all' command to idcams returns:
        # rc=0 if data set found in catalog
        # rc=4 if data set NOT found in catalog
        # rc>4 for other errors
        if rc > 4:
            raise MVSCmdExecError(rc, stdout, stderr)

        delimiter = 'VOLSER------------'
        arr = stdout.split(delimiter)[1:]  # throw away header

        # Volume serials (VOLSER) under 6 chars will have one or more leading '-'s due to the chosen delimiter.
        # The volser is in between the beginning of each str and the first space.
        # Strip away any leading '-'s, then split on the next whitespace and throw away the remaining in each str.
        volume_list = [x.strip('-').split()[0] for x in arr]

        volume_list = list(set(volume_list))  # remove duplicates, order doesn't matter
        return volume_list

    @staticmethod
    def data_set_exists(name, volume=None, tmphlq=None):
        """Determine if a data set exists.
        This will check the catalog in addition to
        the volume table of contents.

        Parameters
        ----------
        name : str
            The data set name to check if exists.
        volume : str
            The volume the data set may reside on.
        tmphlq : str
            High Level Qualifier for temporary datasets.

        Returns
        -------
        bool
            If data is found.
        """
        if DataSet.data_set_cataloged(name, tmphlq=tmphlq):
            return True
        elif volume is not None:
            return DataSet._is_in_vtoc(name, volume, tmphlq=tmphlq)
        return False

    @staticmethod
    def data_set_member_exists(name):
        """Checks for existence of data set member.

        Parameters
        ----------
        name : str
            The data set name including member.

        Returns
        -------
        bool
            If data set member exists.
        """
        module = AnsibleModuleHelper(argument_spec={})
        rc, stdout, stderr = module.run_command(
            "head \"//'{0}'\"".format(name), errors='replace')
        if rc != 0 or (stderr and "EDC5067I" in stderr):
            return False
        return True

    @staticmethod
    def data_set_shared_members(src, dest):
        """Checks for the existence of members from a source data set in
        a destination data set.

        Parameters
        ----------
        src : str
            The source data set name. The name can contain a wildcard pattern.
        dest : str
            The destination data set name.

        Returns
        -------
        bool
            If at least one of the members in src exists in dest.
        """
        src_members = datasets.list_members(src)

        for member in src_members:
            if DataSet.data_set_member_exists("{0}({1})".format(dest, member)):
                return True

        return False

    @staticmethod
    def get_member_name_from_file(file_name):
        """Creates a member name for a partitioned data set by taking up to the
        first 8 characters from a filename without its file extension

        Parameters
        ----------
        file_name : str
            A file name that can include a file extension.

        Returns
        -------
        str
            Member name constructed from the file name.
        """
        # Removing the file extension.
        member_name = path.splitext(file_name)[0]
        # Taking the first 8 characters from the file name.
        member_name = member_name.replace(".", "")[0:8]

        return member_name

    @staticmethod
    def files_in_data_set_members(src, dest):
        """Checks for the existence of members corresponding to USS files in a
        destination data set. The file names get converted to the form they
        would take when copied into a partitioned data set.

        Parameters
        ----------
        src : str
            USS path to a file or a directory.
        dest : str
            Name of the destination data set.

        Returns
        -------
        bool
            If at least one of the members in src exists in dest.
        """
        if path.isfile(src):
            files = [path.basename(src)]
        else:
            dummy_path, dummy_dirs, files = next(walk(src))

        files = [DataSet.get_member_name_from_file(file) for file in files]

        for file in files:
            if DataSet.data_set_member_exists("{0}({1})".format(dest, file)):
                return True

        return False

    @staticmethod
    def data_set_volume(name, tmphlq=None):
        """Checks the volume where a data set is located.

        Parameters
        ----------
        name : str
            The name of the data set.
        tmphlq : str
            High Level Qualifier for temporary datasets.

        Returns
        -------
        str
            Name of the volume where the data set is.

        Raises
        ------
        DatasetNotFoundError
            When data set cannot be found on the system.
        DatasetVolumeError
            When the function is unable to parse the value
            of VOLSER.
        """
        data_set_information = datasets.list_datasets(name)

        if len(data_set_information) > 0:
            return data_set_information[0].volume

        # If listing failed to return a data set, then it's probably a VSAM.
        output = DataSet._get_listcat_data(name, tmphlq=tmphlq)

        if re.findall(r"NOT FOUND|NOT LISTED", output):
            raise DatasetNotFoundError(name)

        volser_output = re.findall(r"VOLSER-*[A-Z|0-9]+", output)

        if volser_output:
            return volser_output[0].replace("VOLSER", "").replace("-", "")
        else:
            raise DatasetVolumeError(name)

    @staticmethod
    def data_set_type(name, volume=None, tmphlq=None):
        """Checks the type of a data set, data sets must be cataloged.

        Parameters
        ----------
        name : str
            The name of the data set.
        volume : str
            The volume the data set may reside on.
        tmphlq : str
            High Level Qualifier for temporary datasets.

        Returns
        -------
        str
            The type of the data set (one of "PS", "PO", "DA", "KSDS",
            "ESDS", "LDS" or "RRDS").
        None
            If the data set does not exist or ZOAU is not able to determine
            the type.

        """
        if not DataSet.data_set_exists(name, volume, tmphlq=tmphlq):
            return None

        data_sets_found = datasets.list_datasets(name)

        # Using the organization property when it's a sequential or partitioned
        # dataset. VSAMs and GDGs are not found by datasets.list_datasets.
        if len(data_sets_found) > 0:
            return data_sets_found[0].organization

        # Now trying to list GDGs through gdgs.
        data_sets_found = gdgs.list_gdg_names(name)
        if len(data_sets_found) > 0:
            return "GDG"

        # Next, trying to get the DATA information of a VSAM through
        # LISTCAT.
        output = DataSet._get_listcat_data(name, tmphlq=tmphlq)

        # Filtering all the DATA information to only get the ATTRIBUTES block.
        data_set_attributes = re.findall(
            r"ATTRIBUTES.*STATISTICS", output, re.DOTALL)
        if len(data_set_attributes) == 0:
            return None

        if re.search(r"\bINDEXED\b", data_set_attributes[0]):
            return "KSDS"
        elif re.search(r"\bNONINDEXED\b", data_set_attributes[0]):
            return "ESDS"
        elif re.search(r"\bLINEAR\b", data_set_attributes[0]):
            return "LDS"
        elif re.search(r"\bNUMBERED\b", data_set_attributes[0]):
            return "RRDS"
        else:
            return None

    @staticmethod
    def _get_listcat_data(name, tmphlq=None):
        """Runs IDCAMS to get the DATA information associated with a data set.

        Parameters
        ----------
        name : str
            Name of the data set.
        tmphlq : str
            High Level Qualifier for temporary datasets.

        Returns
        -------
        str
            Standard output from IDCAMS.

        Raises
        ------
        MVSCmdExecError
            When IDCAMS fails to get the data.
        """
        name = name.upper()
        module = AnsibleModuleHelper(argument_spec={})
        stdin = " LISTCAT ENT('{0}') DATA ALL".format(name)

        cmd = "mvscmdauth --pgm=idcams --sysprint=* --sysin=stdin"
        if tmphlq:
            cmd = "{0} -Q={1}".format(cmd, tmphlq)

        rc, stdout, stderr = module.run_command(
            cmd, data=stdin, errors='replace'
        )

        if rc != 0:
            raise MVSCmdExecError(rc, stdout, stderr)

        return stdout

    @staticmethod
    def is_empty(name, volume=None, tmphlq=None):
        """Determines whether a data set is empty.

        Parameters
        ----------
        name : str
            The name of the data set.
        volume : str
            The volume where the data set resides.
        tmphlq : str
            High Level Qualifier for temporary datasets.

        Returns
        -------
        bool
            Whether the data set is empty or not.
        """
        if not DataSet.data_set_exists(name, volume, tmphlq=tmphlq):
            raise DatasetNotFoundError(name)

        ds_type = DataSet.data_set_type(name, volume, tmphlq=tmphlq)

        if ds_type in DataSet.MVS_PARTITIONED:
            return DataSet._pds_empty(name)
        elif ds_type in DataSet.MVS_SEQ:
            module = AnsibleModuleHelper(argument_spec={})
            rc, stdout, stderr = module.run_command("head \"//'{0}'\"".format(name), errors='replace')
            return rc == 0 and len(stdout.strip()) == 0
        elif ds_type in DataSet.MVS_VSAM:
            return DataSet._vsam_empty(name, tmphlq=tmphlq)

    @staticmethod
    def _pds_empty(name):
        """Determines if a partitioned data set is empty.

        Parameters
        ----------
        name : str
            The name of the PDS/PDSE.

        Returns
        -------
        bool
            If PDS/PDSE is empty.
            Returns True if it is empty. False otherwise.
        """
        module = AnsibleModuleHelper(argument_spec={})
        ls_cmd = "mls {0}".format(name)
        rc, out, err = module.run_command(ls_cmd, errors='replace')
        # RC 2 for mls means that there aren't any members.
        return rc == 2

    @staticmethod
    def _vsam_empty(name, tmphlq=None):
        """Determines if a VSAM data set is empty.

        Parameters
        ----------
        name : str
            The name of the VSAM data set.
        tmphlq : str
            High Level Qualifier for temporary datasets.

        Returns
        -------
        bool
            If VSAM data set is empty.
            Returns True if VSAM data set exists and is empty.
            False otherwise.
        """
        module = AnsibleModuleHelper(argument_spec={})
        empty_cmd = """  PRINT -
        INFILE(MYDSET) -
        COUNT(1)"""

        cmd = "mvscmdauth --pgm=idcams --sysprint=* --sysin=stdin --mydset={0}".format(
            name
        )
        if tmphlq:
            cmd = "{0} -Q={1}".format(cmd, tmphlq)

        rc, out, err = module.run_command(
            cmd, data=empty_cmd, errors='replace'
        )

        if rc == 4 or "VSAM OPEN RETURN CODE IS 160" in out:
            return True
        elif rc != 0:
            return False

    @staticmethod
    def attempt_catalog_if_necessary(name, volumes, tmphlq=None):
        """Attempts to catalog a data set if not already cataloged.

        Parameters
        ----------
        name : str
            The name of the data set.
        volumes : list[str]
            The volumes the data set may reside on.
        tmphlq : str
            High Level Qualifier for temporary datasets.

        Returns
        -------
        tuple(bool,bool)
            Whether the data set is now present. Whether changes were made.
        """
        changed = False
        present = False
        if DataSet.data_set_cataloged(name, tmphlq=tmphlq):
            present = True
        elif volumes is not None:
            errors = False
            try:
                DataSet.catalog(name, volumes, tmphlq=tmphlq)
            except DatasetCatalogError:
                errors = True
            if not errors:
                changed = True
                present = True
        return present, changed

    @staticmethod
    def attempt_catalog_if_necessary_and_delete(name, volumes, tmphlq=None, noscratch=False):
        """Attempts to catalog a data set if not already cataloged, then deletes
           the data set.
           This is helpful when a data set currently cataloged is not the data
           set needing to be deleted, meaning the one in the provided volumes
           is needing to be deleted.. Recall, you can have a data set in
           two different volumes, and only one cataloged.

        Parameters
        ----------
        name : str
            The name of the data set.
        volumes : list[str]
            The volumes the data set may reside on.
        tmphlq : str
            High Level Qualifier for temporary datasets.
        noscratch : bool
            If True, the data set is uncataloged but not physically removed from the volume.

        Returns
        -------
        tuple(bool,bool)
            Whether changes were made. Whether the data set is now present.
        """

        changed = False
        present = True

        if volumes:
            # Check if the data set is cataloged
            present = DataSet.data_set_cataloged(name, tmphlq=tmphlq)

            if present:
                # Data set is cataloged, now check it its cataloged on the provided volumes
                # If it is, we just delete because the DS is the right one wanting deletion.
                present = DataSet.data_set_cataloged(name, volumes, tmphlq=tmphlq)

                if present:
                    DataSet.delete(name, noscratch=noscratch)
                    changed = True
                    present = False
                else:
                    # It appears that what is in catalog does not match the provided
                    # volumes, therefore the user wishes we delete a data set on a
                    # particular volue, NOT what is in catalog.
                    # for the provided volumes

                    # We need to identify the volumes where the current cataloged data set
                    # is located for use later when we recatalog. Code is strategically
                    # placed before the uncatalog.
                    cataloged_volume_list_original = DataSet.data_set_cataloged_volume_list(name, tmphlq=tmphlq)

                    try:
                        DataSet.uncatalog(name, tmphlq=tmphlq)
                    except DatasetUncatalogError:
                        return changed, present

                    # Catalog the data set for the provided volumes
                    try:
                        DataSet.catalog(name, volumes, tmphlq=tmphlq)
                    except DatasetCatalogError:
                        try:
                            # A failure, so recatalog the original data set on the original volumes
                            DataSet.catalog(name, cataloged_volume_list_original, tmphlq=tmphlq)
                        except DatasetCatalogError:
                            pass
                        return changed, present

                    # Check the recatalog, ensure it cataloged before we try to remove
                    present = DataSet.data_set_cataloged(name, volumes, tmphlq=tmphlq)

                    if present:
                        try:
                            DataSet.delete(name, noscratch=noscratch)
                        except DatasetDeleteError:
                            try:
                                DataSet.uncatalog(name, tmphlq=tmphlq)
                            except DatasetUncatalogError:
                                try:
                                    DataSet.catalog(name, cataloged_volume_list_original, tmphlq=tmphlq)
                                except DatasetCatalogError:
                                    pass
                            return changed, present
                        try:
                            DataSet.catalog(name, cataloged_volume_list_original, tmphlq=tmphlq)
                            changed = True
                            present = False
                        except DatasetCatalogError:
                            changed = True
                            present = False
                            return changed, present
            else:
                try:
                    DataSet.catalog(name, volumes, tmphlq=tmphlq)
                except DatasetCatalogError:
                    return changed, present

                present = DataSet.data_set_cataloged(name, volumes, tmphlq=tmphlq)

                if present:
                    DataSet.delete(name, noscratch=noscratch)
                    changed = True
                    present = False
        else:
            present = DataSet.data_set_cataloged(name, None, tmphlq=tmphlq)
            if present:
                try:
                    DataSet.delete(name, noscratch=noscratch)
                    changed = True
                    present = False
                except DatasetDeleteError:
                    return changed, present

        return changed, present

    @staticmethod
    def _is_in_vtoc(name, volume, tmphlq=None):
        """Determines if data set is in a volume's table of contents.

        Parameters
        ----------
        name : str
            The name of the data set to search for.
        volume : str
            The volume to search the table of contents of.
        tmphlq : str
            High Level Qualifier for temporary datasets.

        Returns
        -------
        bool
            If data set was found in table of contents for volume.
        """
        data_sets = vtoc.get_volume_entry(volume, tmphlq=tmphlq)
        data_set = vtoc.find_data_set_in_volume_output(name, data_sets)
        if data_set is not None:
            return True
        vsam_name = name + ".data"
        vsam_data_set = vtoc.find_data_set_in_volume_output(
            vsam_name, data_sets)
        if vsam_data_set is not None:
            return True
        return False

    @staticmethod
    def replace(
        name,
        type,
        raw_name=None,
        space_primary=None,
        space_secondary=None,
        space_type=None,
        record_format=None,
        record_length=None,
        block_size=None,
        directory_blocks=None,
        key_length=None,
        key_offset=None,
        sms_storage_class=None,
        sms_data_class=None,
        sms_management_class=None,
        volumes=None,
        tmp_hlq=None,
        force=None,
    ):
        """Attempts to replace an existing data set.
        Parameters
        ----------
        name : str
            The name of the dataset
        raw_name : str
            Original name without escaping or gds name resolve operations performed.
        type : str, optional
            The type of dataset.
            Valid options are: SEQ, BASIC, LARGE, PDS, PDSE, LIBRARY, LDS, RRDS, ESDS, KSDS.
            Defaults to None.
        space_primary : int, optional
            The amount of primary space to allocate for the dataset.
            Defaults to None.
        space_secondary : int, optional
            The amount of secondary space to allocate for the dataset.
            Defaults to None.
        space_type : str, optional
            The unit of measurement to use when defining primary and secondary space.
            Defaults to None.
        record_format : str, optional
            The record format to use for the dataset.
            Valid options are: F, FB, VB, FBA, VBA, U.
            Defaults to None.
        record_length : int, optional
            The length, in bytes, of each record in the data set.
            Defaults to None.
        block_size : int, optional
            The block size to use for the data set.
            Defaults to None.
        directory_blocks : int, optional
            The number of directory blocks to allocate to the data set.
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
        sms_storage_class : str, optional
            The storage class for an SMS-managed dataset.
            Required for SMS-managed datasets that do not match an SMS-rule.
            Not valid for datasets that are not SMS-managed.
            Note that all non-linear VSAM datasets are SMS-managed.
            Defaults to None.
        sms_data_class : str, optional
            The data class for an SMS-managed dataset.
            Optional for SMS-managed datasets that do not match an SMS-rule.
            Not valid for datasets that are not SMS-managed.
            Note that all non-linear VSAM datasets are SMS-managed.
            Defaults to None.
        sms_management_class : str, optional
            The management class for an SMS-managed dataset.
            Optional for SMS-managed datasets that do not match an SMS-rule.
            Not valid for datasets that are not SMS-managed.
            Note that all non-linear VSAM datasets are SMS-managed.
            Defaults to None.
        volumes : Union[str, list[str]], optional
            A list of volume serials.
            When providing multiple volumes, processing will begin with
            the first volume in the provided list. Offline volumes are not considered.
            Volumes can always be provided when not using SMS.
            When using SMS, volumes can be provided when the storage class being used
            has GUARANTEED_SPACE=YES specified. Otherwise, the allocation will fail.
            Defaults to None.
        tmp_hlq : str, optional
            High level qualifier for temporary datasets.
        force : bool, optional
            Used to determine behavior when performing member operations on a pdse.
            Defaults to None.
        """
        arguments = locals()
        DataSet.delete(name)
        changed, data_set = DataSet.create(**arguments)
        return changed, data_set

    @staticmethod
    def _build_zoau_args(**kwargs):
        """Build zoau arguments

        Parameters
        ----------
        **kwargs : dict
            Arguments

        Returns
        -------
        dict
            Renamed arguments
        """
        primary = kwargs.get("space_primary")
        secondary = kwargs.get("space_secondary")
        space_type = kwargs.get("space_type")
        volumes = kwargs.get("volumes")
        if primary is not None:
            primary = str(primary)
            if space_type:
                primary += space_type
        if secondary is not None:
            secondary = str(secondary)
            if space_type:
                secondary += space_type

        ds_type = kwargs.get("type")
        if ds_type and ds_type.upper() == "ZFS":
            ds_type = "LDS"

        volumes = ",".join(volumes) if volumes else None
        kwargs["space_primary"] = primary
        kwargs["space_secondary"] = secondary
        kwargs["dataset_type"] = ds_type
        kwargs["volumes"] = volumes
        kwargs.pop("space_type", None)
        renamed_args = {}
        for arg, val in kwargs.items():
            if val is None:
                continue
            if DataSet._ZOAU_DS_CREATE_ARGS.get(arg):
                renamed_args[DataSet._ZOAU_DS_CREATE_ARGS.get(arg)] = val
            else:
                renamed_args[arg] = val
        return renamed_args

    @staticmethod
    def create(
        name,
        type,
        raw_name=None,
        space_primary=None,
        space_secondary=None,
        space_type=None,
        record_format=None,
        record_length=None,
        block_size=None,
        directory_blocks=None,
        key_length=None,
        key_offset=None,
        sms_storage_class=None,
        sms_data_class=None,
        sms_management_class=None,
        volumes=None,
        tmp_hlq=None,
        force=None,
    ):
        """A wrapper around zoautil_py
        datasets.create() to raise exceptions on failure.
        Reasonable default arguments will be set by ZOAU when necessary.

        Parameters
        ----------
        name : str
            The name of the dataset.
        raw_name : str
            Original name without escaping or gds name resolve operations performed.
        type : str, optional
            The type of dataset.
            Valid options are: SEQ, BASIC, LARGE, PDS, PDSE, LIBRARY, LDS, RRDS, ESDS, KSDS.
            Defaults to None.
        space_primary : int, optional
            The amount of primary space to allocate for the dataset.
            Defaults to None.
        space_secondary : int, optional
            The amount of secondary space to allocate for the dataset.
            Defaults to None.
        space_type : str, optional
            The unit of measurement to use when defining primary and secondary space.
            Defaults to None.
        record_format : str, optional
            The record format to use for the dataset.
            Valid options are: F, FB, VB, FBA, VBA, U.
            Defaults to None.
        record_length : int, optional
            The length, in bytes, of each record in the data set.
            Defaults to None.
        block_size : int, optional
            The block size to use for the data set.
            Defaults to None.
        directory_blocks : int, optional
            The number of directory blocks to allocate to the data set.
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
        sms_storage_class : str, optional
            The storage class for an SMS-managed dataset.
            Required for SMS-managed datasets that do not match an SMS-rule.
            Not valid for datasets that are not SMS-managed.
            Note that all non-linear VSAM datasets are SMS-managed.
            Defaults to None.
        sms_data_class : str, optional
            The data class for an SMS-managed dataset.
            Optional for SMS-managed datasets that do not match an SMS-rule.
            Not valid for datasets that are not SMS-managed.
            Note that all non-linear VSAM datasets are SMS-managed.
            Defaults to None.
        sms_management_class : str, optional
            The management class for an SMS-managed dataset.
            Optional for SMS-managed datasets that do not match an SMS-rule.
            Not valid for datasets that are not SMS-managed.
            Note that all non-linear VSAM datasets are SMS-managed.
            Defaults to None.
        volumes : Union[str, list[str]], optional
            A list of volume serials.
            When providing multiple volumes, processing will begin with
            the first volume in the provided list. Offline volumes are not considered.
            Volumes can always be provided when not using SMS.
            When using SMS, volumes can be provided when the storage class being used
            has GUARANTEED_SPACE=YES specified. Otherwise, the allocation will fail.
            Defaults to None.
        tmp_hlq : str, optional
            High level qualifier for temporary datasets.
        force : bool, optional
            Used to determine behavior when performing member operations on a pdse.
            Defaults to None.
        Raises
        ------
        DatasetCreateError
            When data set creation fails.
        """
        original_args = locals()
        formatted_args = DataSet._build_zoau_args(**original_args)
        try:
            data_set = datasets.create(**formatted_args)
        except exceptions._ZOAUExtendableException as create_exception:
            raise DatasetCreateError(
                raw_name if raw_name else name,
                create_exception.response.rc,
                create_exception.response.stdout_response + "\n" + create_exception.response.stderr_response
            )
        except exceptions.DatasetVerificationError:
            raise DatasetCreateError(
                raw_name if raw_name else name,
                msg="Unable to verify the data set was created. Received DatasetVerificationError from ZOAU.",
            )
        changed = data_set is not None
        return changed, data_set

    @staticmethod
    def delete(name, noscratch=False):
        """A wrapper around zoautil_py
        datasets.delete() to raise exceptions on failure.

        Parameters
        ----------
        name : str
            The name of the data set to delete.

        Raises
        ------
        DatasetDeleteError
            When data set deletion fails.
        """
        rc = datasets.delete(name, no_scratch=noscratch)
        if rc > 0:
            raise DatasetDeleteError(name, rc)

    @staticmethod
    # TODO: verify that this method works for all lengths etc
    def create_member(name, tmphlq=None):
        """Create a data set member if the partitioned data set exists.
        Also used to overwrite a data set member if empty replacement is desired.

        Parameters
        ----------
        name : str
            The data set name, including member name, to create.
        tmphlq : str
            High Level Qualifier for temporary datasets.

        Raises
        ------
        DatasetNotFoundError
            If data set cannot be found.
        DatasetMemberCreateError
            If member creation fails.
        """
        module = AnsibleModuleHelper(argument_spec={})
        base_dsname = name.split("(")[0]
        if not base_dsname or not DataSet.data_set_cataloged(base_dsname, tmphlq=tmphlq):
            raise DatasetNotFoundError(name)
        tmp_file = tempfile.NamedTemporaryFile(delete=True)
        rc, stdout, stderr = module.run_command(
            "cp {0} \"//'{1}'\"".format(tmp_file.name, name), errors='replace'
        )
        if rc != 0:
            raise DatasetMemberCreateError(name, rc)

    @staticmethod
    def delete_member(name, force=False):
        """A wrapper around zoautil_py
        datasets.delete_members() to raise exceptions on failure.

        Parameters
        ----------
        name : str
            The name of the data set, including member name, to delete.

        Raises
        ------
        DatasetMemberDeleteError
            When data set member deletion fails.
        """
        rc = datasets.delete_members(name, force=force)
        if rc > 0:
            raise DatasetMemberDeleteError(name, rc)

    @staticmethod
    def catalog(name, volumes, tmphlq=None):
        """Catalog an uncataloged data set

        Parameters
        ----------
        name : str
            The name of the data set to catalog.
        volumes : list[str]
            The volume(s) the data set resides on.
        tmphlq : str
            High Level Qualifier for temporary datasets.
        """
        if DataSet.is_vsam(name, volumes, tmphlq=tmphlq):
            DataSet._catalog_vsam(name, volumes, tmphlq=tmphlq)
        else:
            DataSet._catalog_non_vsam(name, volumes, tmphlq=tmphlq)

    @staticmethod
    # TODO: extend for multi volume data sets
    def _catalog_non_vsam(name, volumes, tmphlq=None):
        """Catalog a non-VSAM data set.

        Parameters
        ----------
        name : str
            The data set to catalog.
        volumes : str
            The volume(s) the data set resides on.
        tmphlq : str
            High Level Qualifier for temporary datasets.

        Raises
        ------
        DatasetCatalogError
            When attempt at catalog fails.
        """
        module = AnsibleModuleHelper(argument_spec={})
        iehprogm_input = DataSet._build_non_vsam_catalog_command(
            name.upper(), volumes)

        cmd = "mvscmdauth --pgm=iehprogm --sysprint=* --sysin=stdin"
        if tmphlq:
            cmd = "{0} -Q={1}".format(cmd, tmphlq)

        rc, stdout, stderr = module.run_command(
            cmd, data=iehprogm_input, errors='replace'
        )

        if rc != 0 or "NORMAL END OF TASK RETURNED" not in stdout:
            raise DatasetCatalogError(name, volumes, rc)
        return

    @staticmethod
    # TODO: extend for multi volume data sets
    def _catalog_vsam(name, volumes, tmphlq=None):
        """Catalog a VSAM data set.

        Parameters
        ----------
        name : str
            The data set to catalog.
        volumes : str
            The volume(s) the data set resides on.
        tmphlq : str
            High Level Qualifier for temporary datasets.

        Raises
        ------
        DatasetCatalogError
            When attempt at catalog fails.
        """
        module = AnsibleModuleHelper(argument_spec={})
        data_set_name = name.upper()
        success = False
        command_rc = 0
        command = ""

        # In order to catalog a uncataloged data set, we can't rely on LISTCAT
        # so using the VTOC entries we can make some assumptions of if the data set
        # is indexed, linear etc.
        ds_vtoc_data_entry = vtoc.get_data_set_entry(name + ".DATA", volumes[0], tmphlq=tmphlq)
        ds_vtoc_index_entry = vtoc.get_data_set_entry(name + ".INDEX", volumes[0], tmphlq=tmphlq)

        if ds_vtoc_data_entry and ds_vtoc_index_entry:
            data_set_type_vsam = "INDEXED"
        else:
            data_set_type_vsam = "NONINDEXED"

        if data_set_type_vsam != "INDEXED":
            command = DataSet._VSAM_CATALOG_COMMAND_NOT_INDEXED.format(
                data_set_name,
                DataSet._build_volume_string_idcams(volumes),
                data_set_type_vsam,
            )
        else:
            command = DataSet._VSAM_CATALOG_COMMAND_INDEXED.format(
                data_set_name,
                DataSet._build_volume_string_idcams(volumes),
                data_set_type_vsam,
            )

        cmd = "mvscmdauth --pgm=idcams --sysprint=* --sysin=stdin"
        if tmphlq:
            cmd = "{0} -Q={1}".format(cmd, tmphlq)
        command_rc, stdout, stderr = module.run_command(cmd, data=command, errors='replace')

        if command_rc == 0:
            success = True
            # break

        if not success:
            # Liberty taken such that here we can assume  its a LINEAR VSAM
            command = DataSet._VSAM_CATALOG_COMMAND_NOT_INDEXED.format(
                data_set_name,
                DataSet._build_volume_string_idcams(volumes),
                "LINEAR",
            )

            cmd = "mvscmdauth --pgm=idcams --sysprint=* --sysin=stdin"
            if tmphlq:
                cmd = "{0} -Q={1}".format(cmd, tmphlq)

            command_rc, stdout, stderr = module.run_command(cmd, data=command, errors='replace')

            if command_rc == 0:
                success = True

        if not success:
            raise DatasetCatalogError(
                name,
                volumes,
                command_rc,
                "Attempt to catalog VSAM data set failed.",
            )
        return

    @staticmethod
    def uncatalog(name, tmphlq=None):
        """Uncatalog a data set.

        Parameters
        ----------
        name : str
            The name of the data set to uncatalog.
        tmphlq : str
            High Level Qualifier for temporary datasets.

        """
        if DataSet.is_vsam(name, tmphlq=tmphlq):
            DataSet._uncatalog_vsam(name, tmphlq=tmphlq)
        else:
            DataSet._uncatalog_non_vsam(name, tmphlq=tmphlq)

    @staticmethod
    def _uncatalog_non_vsam(name, tmphlq=None):
        """Uncatalog a non-VSAM data set.

        Parameters
        ----------
        name : str
            The name of the data set to uncatalog.
        tmphlq : str
            High Level Qualifier for temporary datasets.

        Raises
        ------
        DatasetUncatalogError
            When uncataloging fails.
        """
        module = AnsibleModuleHelper(argument_spec={})
        iehprogm_input = DataSet._NON_VSAM_UNCATALOG_COMMAND.format(name)
        temp_name = None
        try:
            temp_name = DataSet.create_temp(name.split(".")[0])
            DataSet.write(temp_name, iehprogm_input)

            cmd = "mvscmdauth --pgm=iehprogm --sysprint=* --sysin={0}".format(temp_name)
            if tmphlq:
                cmd = "{0} -Q={1}".format(cmd, tmphlq)

            rc, stdout, stderr = module.run_command(cmd, errors='replace')

            if rc != 0 or "NORMAL END OF TASK RETURNED" not in stdout:
                raise DatasetUncatalogError(name, rc)
        finally:
            if temp_name:
                datasets.delete(temp_name)
        return

    @staticmethod
    def _uncatalog_vsam(name, tmphlq=None):
        """Uncatalog a VSAM data set.

        Parameters
        ----------
        name : str
            The name of the data set to uncatalog.
        tmphlq : str
            High Level Qualifier for temporary datasets.


        Raises
        ------
        DatasetUncatalogError
            When uncatalog fails.
        """
        module = AnsibleModuleHelper(argument_spec={})
        idcams_input = DataSet._VSAM_UNCATALOG_COMMAND.format(name)

        cmd = "mvscmdauth --pgm=idcams --sysprint=* --sysin=stdin"
        if tmphlq:
            cmd = "{0} -Q={1}".format(cmd, tmphlq)

        rc, stdout, stderr = module.run_command(cmd, data=idcams_input, errors='replace')

        if rc != 0:
            raise DatasetUncatalogError(name, rc)

    @staticmethod
    def is_vsam(name, volumes=None, tmphlq=None):
        """Determine a given data set is VSAM. If volume is not provided,
        then LISTCAT will be used to check data set info. If volume is provided,
        then VTOC will be used to check data set info. If not in VTOC
        may not return accurate information.

        Parameters
        ----------
        name : str
            The name of the data set.

        Keyword Parameters
        ------------------
        volumes : list[str]
            The name(s) of the volume(s). (default: (None))
        tmphlq : str
            High Level Qualifier for temporary datasets.

        Returns
        -------
        bool
            If the data set is VSAM.
        """
        if not volumes:
            return DataSet._is_vsam_from_listcat(name, tmphlq=tmphlq)
        # ? will multivolume data set have vtoc info for each volume?
        return DataSet._is_vsam_from_vtoc(name, volumes[0], tmphlq=tmphlq)

    @staticmethod
    def _is_vsam_from_vtoc(name, volume, tmphlq=None):
        """Use VTOC to determine if a given data set is VSAM.

        Parameters
        ----------
        name : str
            The name of the data set.
        volume : str
            The volume name whose table of contents will be searched.
        tmphlq : str
            High Level Qualifier for temporary datasets.

        Returns
        -------
        bool
            If the data set is VSAM.
        """
        data_sets = vtoc.get_volume_entry(volume, tmphlq=tmphlq)
        vsam_name = name + ".DATA"
        data_set = vtoc.find_data_set_in_volume_output(vsam_name, data_sets)
        if data_set is None:
            data_set = vtoc.find_data_set_in_volume_output(name, data_sets)
        if data_set is not None and data_set.get("data_set_organization", "") == "VS":
            return True
        return False

    @staticmethod
    def _is_vsam_from_listcat(name, tmphlq=None):
        """Use LISTCAT command to determine if a given data set is VSAM.

        Parameters
        ----------
        name : str
            The name of the data set.
        tmphlq : str
            High Level Qualifier for temporary datasets.

        Returns
        -------
        bool
            If the data set is VSAM.
        """
        module = AnsibleModuleHelper(argument_spec={})
        stdin = " LISTCAT ENTRIES('{0}')".format(name.upper())

        cmd = "mvscmdauth --pgm=idcams --sysprint=* --sysin=stdin"
        if tmphlq:
            cmd = "{0} -Q={1}".format(cmd, tmphlq)

        rc, stdout, stderr = module.run_command(cmd, data=stdin, errors='replace')
        if re.search(r"^0CLUSTER[ ]+-+[ ]+" + name + r"[ ]*$", stdout, re.MULTILINE):
            return True
        return False

    @staticmethod
    def is_gds_relative_name(name):
        """Determine if name is a gdg relative name based
        on the GDS relative name syntax eg. 'USER.GDG(-2)'.

        Parameters
        ----------
        name : str
            Data set name to determine if is a GDS relative name.

        Returns
        -------
        bool
            Whether the name is a GDS relative name.
        """
        pattern = r'(.+)\(([\\]?[+-]?\d+)\)'
        match = re.fullmatch(pattern, name)
        return bool(match)

    @staticmethod
    def is_gds_positive_relative_name(name):
        """Determine if name is a gdg relative positive name
        based on the GDS relative name syntax eg. 'USER.GDG(+1)'.
        Parameters
        ----------
        name : str
            Data set name to determine if is a GDS relative name.
        Returns
        -------
        bool
            Whether the name is a GDS positive relative name.
        """
        pattern = r'(.+)\(([\\]?[+]\d+)\)'
        match = re.fullmatch(pattern, name)
        return bool(match)

    @staticmethod
    def resolve_gds_absolute_name(relative_name):
        """Given a GDS relative name, returns its absolute name.

        Parameters
        ----------
        relative_name : str
            GDS relative name to be resolved.

        Returns
        -------
        str
            GDS absolute name.

        Raises
        ------
        GDSNameResolveError
            Error resolving the GDS relative name, either because
            the name is not a valid GDS syntax or failure to retrieve
            the GDG data based on the gdg base name.
        """
        pattern = r'(.+)\(([\\]?[-+]?\d+)\)'
        match = re.search(pattern, relative_name)
        try:
            gdg_base = match.group(1)
            rel_generation = int(match.group(2))
            if rel_generation > 0:
                # Fail if we are trying to resolve a future generation.
                raise Exception
            gdg = gdgs.GenerationDataGroupView(name=gdg_base)
            generations = gdg.generations
            # From ZOAU 1.4 version relative notation 0 or -1 is on automatic give
            gds = generations[rel_generation]
        except Exception:
            raise GDSNameResolveError(relative_name)

        return gds.name

    @staticmethod
    def escape_data_set_name(name):
        """Escapes special characters ($, @, #) inside a data set name.

        Parameters
        ----------
            name : str
                Name of the data set.

        Returns
        -------
            str
                Escaped data set name.
        """
        special_chars = ['$', '@', '#', '-']
        escaped_name = name.replace('\\', '')

        for char in special_chars:
            escaped_name = escaped_name.replace(char, f"\\{char}")

        return escaped_name

    @staticmethod
    def temp_name(hlq=""):
        """Get temporary data set name.

        Parameters
        ----------
        hlq : str, optional
            The HLQ to use for the temporary data set. Defaults to "".

        Returns
        -------
        str
            The temporary data set name.
        """
        if not hlq:
            hlq = datasets.get_hlq()
        temp_name = datasets.tmp_name(hlq)
        return temp_name

    @staticmethod
    def create_temp(
        hlq="",
        type="SEQ",
        record_format="FB",
        space_primary=5,
        space_secondary=5,
        space_type="M",
        record_length=80,
    ):
        """Create a temporary data set.
        User is responsible for removing the data set after use.

        Parameters
        ----------
        hlq : str
            The HLQ to use for the temporary data set's name.
        type : str, optional
            The type of dataset.
            Valid options are: SEQ, BASIC, LARGE, PDS, PDSE, LIBRARY, LDS, RRDS, ESDS, KSDS.
            Defaults to "SEQ".
        record_format : str, optional
            The record format to use for the dataset.
            Valid options are: F, FB, VB, FBA, VBA, U.
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

        Returns
        -------
        str
            The name of the temporary data set.
        """
        temp_name = DataSet.temp_name(hlq)
        DataSet.create(
            temp_name,
            type=type,
            space_primary=space_primary,
            space_secondary=space_secondary,
            space_type=space_type,
            record_format=record_format,
            record_length=record_length,
        )
        return temp_name

    @staticmethod
    def format_zfs(name):
        """Format an existing LDS as a ZFS file system.

        Parameters
        ----------
        name : str
            The name of the data set to format.

        Raises
        ------
        DatasetFormatError
            When data set formatting fails.
        """
        module = AnsibleModuleHelper(argument_spec={})
        rc, stdout, stderr = module.run_command(
            "zfsadm format -aggregate {0}".format(name), errors='replace'
        )
        if rc != 0:
            raise DatasetFormatError(
                name, rc, "{0} {1}".format(stdout, stderr))
        return

    @staticmethod
    def write(name, contents):
        """Write text to a data set.

        Parameters
        ----------
        name : str
            The name of the data set.
        contents : str
            The text to write to the data set.

        Raises
        ------
        DatasetWriteError
            When write to the data set fails.
        """
        module = AnsibleModuleHelper(argument_spec={})
        temp = tempfile.NamedTemporaryFile(delete=True)
        with open(temp.name, "w") as f:
            f.write(contents)
        rc, stdout, stderr = module.run_command(
            "cp -O u {0} \"//'{1}'\"".format(temp.name, name), errors='replace'
        )
        if rc != 0:
            raise DatasetWriteError(name, rc)

    @staticmethod
    def _build_non_vsam_catalog_command(name, volumes):
        """Build the command string to use
        for non-VSAM data set catalog operation.
        This is necessary because IEHPROGM required
        strict formatting when spanning multiple lines.

        Parameters
        ----------
        name : str
            The data set to catalog.
        volumes : list[str]
            The volume(s) the data set resides on.

        Returns
        -------
        str
            The command string formatted for use with IEHPROGM.
        """
        command_part_1 = DataSet._format_jcl_line(
            "    CATLG DSNAME={0},".format(name))
        command_part_2 = DataSet._build_volume_string_iehprogm(volumes)
        return command_part_1 + command_part_2

    @staticmethod
    def _format_jcl_line(string, eol_char="X", include_newline=True):
        """Formats a single line of text to contain EOL character in colums 72,
        which is required for some programs available through JCL.

        Arguments
        ---------
        string : str
            The string to format.
        eol_char : str, optional
            The character to place in column 72 of the string.
            Defaults to "X".
        include_newline : bool, optional
            Determines if a newline will be appended
            to the end of the formatted string.
            Defaults to True.

        Returns
        -------
        str
            The string formatted with special character in column 72
        """
        formatted = "{line: <{max_len}}".format(line=string, max_len=71)
        formatted += eol_char
        if include_newline:
            formatted += "\n"
        return formatted

    @staticmethod
    def _build_volume_string_idcams(volumes):
        """Build string for volume portion of idcams input

        Parameters
        ----------
        volumes : list[str]
            List of volumes used to build string.

        Returns
        -------
        str
            string built from volumes.
        """
        return " -\n    ".join(volumes)

    @staticmethod
    def _build_volume_string_iehprogm(volumes):
        """Build string for volume portion of iehprogm input.

        Parameters
        ----------
        volumes : list[str]
            List of volumes used to build string.

        Returns
        -------
        str
            string built from volumes.
        """
        volume_string = ""
        for index, volume in enumerate(volumes):
            single_volume_string = ""
            if index == 0:
                single_volume_string = "               VOL=3390=({0}".format(
                    volume.upper()
                )
            else:
                single_volume_string = "               {0}".format(
                    volume.upper())
            if index + 1 != len(volumes):
                single_volume_string += ","
                volume_string += DataSet._format_jcl_line(single_volume_string)
            else:
                volume_string += single_volume_string + ")\n"
        return volume_string

    @staticmethod
    def check_if_data_set_migrated(name):
        """Compares the output of datasets.list_dataset_names with and
        without migrated data set names to check if name has been migrated.

        Parameters
        ----------
        name : str
            Name of a data set.

        Returns
        -------
        bool
            Whether the data set has been migrated.
        """
        has_been_migrated = False

        non_migrated_list = datasets.list_dataset_names(name, migrated=False)
        migrated_list = datasets.list_dataset_names(name, migrated=True)

        if name in migrated_list and name not in non_migrated_list:
            has_been_migrated = True

        return has_been_migrated

    @staticmethod
    def recall_migrated_data_set(name, module, tmp_hlq=None):
        """Recalls a data set using HRECALL.

        Parameters
        ----------
        name : str
            Name of a data set.
        module : AnsibleModuleHelper
            Ansible object capable of executing commands.

        Keyword Parameters
        ------------------
        tmp_hlq : str
            Temp HLQ to use with mvscmdauth.

        Returns
        -------
        tuple(int, str, str)
            Return code, standard output and standard error from
            the HRECALL call.
        """
        name = name.replace('$', '\\$')
        recall_cmd = f"""tsocmd "HRECALL '{name}'" """
        rc, stdout, stderr = module.run_command(recall_cmd)

        return rc, stdout, stderr

    @staticmethod
    def get_name_if_data_set_is_alias(name, tmp_hlq=None):
        """Checks the catalog to see if 'name' corresponds to a data set
        alias and returns the original data set name in case it is.
        Creates a temp data set to hold the IDCAMS command.

        Parameters
        ----------
        name : str
            Name of a data set or alias.

        Keyword Parameters
        ------------------
        tmp_hlq : str
            Temp HLQ to use with mvscmdauth.

        Returns
        -------
        tuple(bool, str)
            A tuple containing whether name corresponds to a data
            set alias and the name of the data set that the alias
            points to.
        """
        # We need to unescape because this call to the system can handle
        # special characters just fine.
        name = name.upper().replace("\\", '')
        idcams_cmd = f""" LISTCAT -\n ENTRIES('{name}') -\n ALIAS ALL"""
        response = DataSet._execute_idcams_cmd(idcams_cmd, tmp_hlq=tmp_hlq)

        if response.rc == 0:
            base_name = re.search(
                r'(ASSOCIATIONS\s*\n\s*[0-9a-zA-Z]+-+)([0-9a-zA-Z\.@\$#-]+)',
                response.stdout_response
            ).group(2)
            return True, base_name
        elif response.rc == 4:
            return False, name
        elif response.rc != 0 or response.stderr_response != '':
            raise MVSCmdExecError(
                rc=response.rc,
                out=response.stdout_response,
                err=response.stderr_response
            )

    @staticmethod
    def _execute_idcams_cmd(
        cmd,
        tmp_hlq=None,
        space_primary=1,
        space_type='k',
        record_format='fb',
        record_length=120
    ):
        """Runs an IDCAMS command using mvscmdauth's Python API.

        Parameters
        ----------
            cmd : str
                IDCAMS command to run.

        Keyword Parameters
        ------------------
            tmp_hlq : str
                Temp HLQ to use with mvscmdauth.
            space_primary : int
                Units of primary space for the input data set for IDCAMS.
            space_type : str
                Unit of data set space.
            record_format : str
                Record format for the input data set.
            record_length : int
                Record length for the input data set.

        Returns
        -------
        ztypes.ZOAUResponse
            Response object returned by mvscmd.execute_authorized.
        """
        temp_dd_location = None

        try:
            temp_dd_location = DataSet.create_temp(
                hlq=tmp_hlq,
                type='SEQ',
                record_format=record_format,
                space_primary=space_primary,
                space_secondary=0,
                space_type=space_type,
                record_length=record_length
            )

            datasets.write(temp_dd_location, cmd)
            cmd_dd = ztypes.DatasetDefinition(temp_dd_location, disposition='SHR')

            dds = [
                ztypes.DDStatement('SYSPRINT', '*'),
                ztypes.DDStatement('SYSIN', cmd_dd)
            ]

            if tmp_hlq:
                environ['TMPHLQ'] = tmp_hlq

            response = mvscmd.execute_authorized('IDCAMS', dds=dds)

            if tmp_hlq:
                del environ['TMPHLQ']

            return response
        finally:
            if temp_dd_location:
                datasets.delete(temp_dd_location)


class DataSetUtils(object):
    def __init__(self, data_set, tmphlq=None):
        """A standard utility to gather information about
        a particular data set. Note that the input data set is assumed
        to be cataloged.

        Parameters
        ----------
        data_set : str
            Name of the input data set.
        tmphlq : str
            High Level Qualifier for temporary datasets.

        """
        self.module = AnsibleModuleHelper(argument_spec={})
        self.data_set = data_set.upper()
        self.path = data_set
        self.is_uss_path = "/" in data_set
        self.ds_info = dict()
        self.tmphlq = tmphlq
        if not self.is_uss_path:
            self.ds_info.update(self._gather_data_set_info())

    def exists(self):
        """Determines whether the input data set exists. The input data
        set can be VSAM or non-VSAM.

        Returns
        -------
        bool
            If the data set exists.
        """
        if self.is_uss_path:
            return path.exists(to_bytes(self.path))
        return self.ds_info.get("exists")

    def member_exists(self, member):
        """Determines whether the input data set contains the given member.

        Parameters
        ----------
        member : str
            The name of the data set member.

        Returns
        -------
        bool
            If the member exists.
        """
        if self.ds_type() == "PO":
            rc, out, err = self.module.run_command(
                "head \"//'{0}({1})'\"".format(self.data_set, member), errors='replace'
            )
            if rc == 0 and not re.findall(r"EDC5067I", err):
                return True
        return False

    def ds_type(self):
        """Retrieves the data set type of the input data set.

        Returns
        -------
        str
            Type of the input data set.
            'PS'
                Physical Sequential.
            'PO'
                Partitioned (PDS or PDS(E)).
            'VSAM'
                Virtual Storage Access Method.
            'DA'
                Direct Access.
            'IS'
                Indexed Sequential.
            'USS'
                USS file or directory.
        None
            If the data set does not exist or a non-existent USS file.
        """
        if self.is_uss_path and self.exists():
            return "USS"
        return self.ds_info.get("dsorg")

    def volume(self):
        """Retrieves the volume name where the input data set is stored.

        Returns
        -------
        str
            Volume where the data set is stored.
        None
            If the data set does not exist.

        Raises
        ------
        AttributeError
            When input data set is a USS file or directory.
        """
        if self.is_uss_path:
            raise AttributeError(
                "USS file or directory has no attribute 'Volume'")
        return self.ds_info.get("volser")

    def lrecl(self):
        """Retrieves the record length of the input data set. Record length
        specifies the length, in bytes, of each record in the data set.

        Returns
        -------
        int
            The record length, in bytes, of each record.
        None
            If the data set does not exist or the data set is VSAM.

        Raises
        ------
        AttributeError
            When input data set is a USS file or directory.
        """
        if self.is_uss_path:
            raise AttributeError(
                "USS file or directory has no attribute 'lrecl'")
        return self.ds_info.get("lrecl")

    def blksize(self):
        """Retrieves the BLKSIZE of the input data set.

        Returns
        -------
        int
            The blksize of the input data set.
        None
            If the data set does not exist or the data set is VSAM.

        Raises
        ------
        AttributeError
            When input data set is a USS file or directory.
        """
        if self.is_uss_path:
            raise AttributeError(
                "USS file or directory has no attribute 'blksize'")
        return self.ds_info.get("blksize")

    def recfm(self):
        """Retrieves the record format of the input data set.

        Returns
        -------
        str
            Record format.
            'F'
                Fixed.
            'FB'
                Fixed Blocked.
            'V'
                Variable.
            'VB'
                Variable Blocked.
            'U'
                Undefined.
            'VBS'
                Variable Blocked Spanned.
            'VS'
                Variable Spanned.
        None
            If the data set does not exist or the data set is VSAM.

        Raises
        ------
        AttributeError
            When input data set is a USS file or directory.
        """
        if self.is_uss_path:
            raise AttributeError(
                "USS file or directory has no attribute 'recfm'")
        return self.ds_info.get("recfm")

    def _gather_data_set_info(self):
        """Retrieves information about the input data set using LISTDS and
        LISTCAT commands.

        Returns
        -------
        dict
            Dictionary containing data set attributes.

        Raises
        ------
        DatasetBusyError
            The dataset may be open by another user.
        MVSCmdExecError
            Another error while executing the command.
        """
        result = dict()
        self.data_set = self.data_set.upper().replace("\\", '')
        listds_rc, listds_out, listds_err = mvs_cmd.ikjeft01(
            "  LISTDS '{0}'".format(self.data_set),
            authorized=True,
            tmphlq=self.tmphlq
        )

        if listds_rc == 0:
            result.update(self._process_listds_output(listds_out))
        else:
            if re.findall(r"ALREADY IN USE", listds_out):
                raise DatasetBusyError(self.data_set)
            if re.findall(r"NOT IN CATALOG", listds_out):
                self.ds_info["exists"] = False
            else:
                raise MVSCmdExecError(listds_rc, listds_out, listds_err)

        listcat_rc, listcat_out, listcat_err = mvs_cmd.idcams(
            "  LISTCAT ENT({0}) ALL".format(self.data_set), authorized=True
        )
        if listcat_rc == 0:
            result.update(self._process_listcat_output(listcat_out))
        else:
            if re.findall(r"NOT FOUND|NOT LISTED", listcat_out):
                self.ds_info["exists"] = False
            else:
                raise MVSCmdExecError(listcat_rc, listcat_out, listcat_err)
        return result

    def _process_listds_output(self, output):
        """Parses the output generated by LISTDS command.

        Parameters
        ----------
        output : str
            The output of LISTDS command.

        Returns
        -------
        dict
            Dictionary containing the output parameters of LISTDS.
        """
        result = dict()
        if "NOT IN CATALOG" in output:
            result["exists"] = False
        else:
            result["exists"] = True
            ds_search = re.search(
                r"(-|--)DSORG(-\s*|\s*)\n(.*)", output, re.MULTILINE)
            if ds_search:
                ds_params = ds_search.group(3).split()
                result["dsorg"] = ds_params[-1]
                if result.get("dsorg") != "VSAM":
                    result["recfm"] = ds_params[0]
                    if ds_params[1].isdigit():
                        result["lrecl"] = int(ds_params[1])
                    if len(ds_params) > 2 and ds_params[2].isdigit():
                        result["blksize"] = int(ds_params[2])
        return result

    def _process_listcat_output(self, output):
        """Parses the output generated by LISTCAT command.

        Parameters
        ----------
        output : str
            The output of LISTCAT command.

        Returns
        -------
        dict
            Dictionary containing the output parameters of LISTCAT.
        """
        result = dict()
        if "NOT FOUND" not in output:
            volser_output = re.findall(r"VOLSER-*[A-Z|0-9]*", output)
            if volser_output:
                result["volser"] = "".join(
                    re.findall(r"-[A-Z|0-9]*", volser_output[0])
                ).replace("-", "")
        return result

    @staticmethod
    def verify_dataset_disposition(data_set, disposition):
        """Function to call iefbr14 to verify the dsp of data_set

        Args:
            data_set {str}: Name of dataset to verify the dsp=shr

        Returns:
            bool:  If the data_set is in dsp=shr
        """
        data_set_disp = f"{data_set},{disposition}"
        dd = {"dd": data_set_disp}
        rc, stdput, stderr = mvs_cmd.iefbr14(dds=dd)

        if rc != 0:
            return True
        else:
            return False


class MVSDataSet():
    """
    This class represents a z/OS data set that can be yet to be created or
    already created in the system. It encapsulates the data set attributes
    to easy access and provides operations to perform in the same data set.

    """
    def __init__(
        self,
        name,
        escape_name=False,
        data_set_type=None,
        state=None,
        organization=None,
        record_format=None,
        volumes=None,
        block_size=None,
        record_length=None,
        space_primary=None,
        space_secondary=None,
        space_type=None,
        directory_blocks=None,
        key_length=None,
        key_offset=None,
        sms_storage_class=None,
        sms_data_class=None,
        sms_management_class=None,
        total_space=None,
        used_space=None,
        last_referenced=None,
        is_cataloged=None,
    ):
        # Different class variables
        self.data_set_possible_states = {"unknown", "present", "absent"}
        self.name = name
        self.organization = organization
        self.record_format = record_format
        self.volumes = volumes
        self.block_size = block_size
        self.record_length = record_length
        self.total_space = total_space
        self.used_space = used_space
        self.last_referenced = last_referenced
        self.raw_name = name
        self.data_set_type = data_set_type
        self.state = state
        self.space_primary = space_primary
        self.space_secondary = space_secondary
        self.space_type = space_type
        self.directory_blocks = directory_blocks
        self.key_length = key_length
        self.key_offset = key_offset
        self.sms_storage_class = sms_storage_class
        self.sms_data_class = sms_data_class
        self.sms_management_class = sms_management_class
        self.volumes = volumes
        self.is_gds_active = False
        self.is_cataloged = False

        # If name has escaped chars or is GDS relative name we clean it.
        if escape_name:
            self.name = DataSet.escape_data_set_name(self.name)
        if DataSet.is_gds_relative_name(self.name):
            try:
                self.name = DataSet.resolve_gds_absolute_name(self.name)
                self.is_gds_active = True
            except Exception:
                # This means the generation is a positive version so is only used for creation.
                self.is_gds_active = False
        if self.data_set_type and (self.data_set_type.upper() in DataSet.MVS_VSAM or self.data_set_type == "zfs"):
            # When trying to create a new VSAM with a specified record format will fail
            # with ZOAU
            self.record_format = None

    def create(self, tmp_hlq=None, replace=True, force=False):
        """Creates the data set in question.

        Returns
        -------
        int
            Indicates if changes were made.
        """
        arguments = {
            "name": self.name,
            "raw_name": self.raw_name,
            "type": self.data_set_type,
            "space_primary": self.space_primary,
            "space_secondary": self.space_secondary,
            "space_type": self.space_type,
            "record_format": self.record_format,
            "record_length": self.record_length,
            "block_size": self.block_size,
            "directory_blocks": self.directory_blocks,
            "key_length": self.key_length,
            "key_offset": self.key_offset,
            "sms_storage_class": self.sms_storage_class,
            "sms_data_class": self.sms_data_class,
            "sms_management_class": self.sms_management_class,
            "volumes": self.volumes,
            "tmp_hlq": tmp_hlq,
            "force": force,
        }
        formatted_args = DataSet._build_zoau_args(**arguments)
        changed = False
        if DataSet.data_set_exists(self.name, tmphlq=tmp_hlq):
            DataSet.delete(self.name)
            changed = True
        zoau_data_set = datasets.create(**formatted_args)
        if zoau_data_set is not None:
            self.set_state("present")
            self.name = zoau_data_set.name
            return True
        return changed

    def ensure_present(self, tmp_hlq=None, replace=False, force=False):
        """ Make sure that the data set is created or fail creating it.

        Parameters
        ----------
        tmp_hlq : str
            High level qualifier for temporary datasets.
        replace : bool
            Used to determine behavior when data set already exists.
        force : bool
            Used to determine behavior when performing member operations on a pdse.

        Returns
        -------
        int
            Indicates if changes were made.
        """
        arguments = {
            "name": self.name,
            "raw_name": self.raw_name,
            "type": self.data_set_type,
            "space_primary": self.space_primary,
            "space_secondary": self.space_secondary,
            "space_type": self.space_type,
            "record_format": self.record_format,
            "record_length": self.record_length,
            "block_size": self.block_size,
            "directory_blocks": self.directory_blocks,
            "key_length": self.key_length,
            "key_offset": self.key_offset,
            "sms_storage_class": self.sms_storage_class,
            "sms_data_class": self.sms_data_class,
            "sms_management_class": self.sms_management_class,
            "volumes": self.volumes,
            "replace": replace,
            "tmp_hlq": tmp_hlq,
            "force": force,
        }
        rc, data_set = DataSet.ensure_present(**arguments)
        if data_set is not None:
            self.merge_attributes_from_zoau_data_set(data_set)
        self.set_state("present")
        return rc

    def ensure_absent(self, tmp_hlq=None, noscratch=False):
        """Removes the data set.

        Parameters
        ----------
        tmp_hlq : str
            High level qualifier for temporary datasets.

        Returns
        -------
        int
            Indicates if changes were made.
        """
        rc = DataSet.ensure_absent(self.name, self.volumes, tmphlq=tmp_hlq, noscratch=noscratch)
        if rc == 0:
            self.set_state("absent")
        return rc

    def delete(self):
        """Deletes the data set in question.

        Returns
        -------
        int
            Indicates if changes were made.
        """
        DataSet.ensure_absent(self.name, self.volumes)
        self.set_state("absent")

    def ensure_cataloged(self, tmp_hlq=None):
        """
        Ensures the data set is cataloged, if not catalogs it.

        Parameters
        ----------
        tmp_hlq : str
            High level qualifier for temporary datasets.

        Returns
        -------
        int
            Indicates if changes were made.
        """
        rc = DataSet.ensure_cataloged(name=self.name, volumes=self.volumes, tmphlq=tmp_hlq)
        self.is_cataloged = True
        return rc

    def catalog(self, tmp_hlq=None):
        """Catalog the data set in question.

        Parameters
        ----------
        tmp_hlq : str
            High level qualifier for temporary datasets.

        Returns
        -------
        int
            Indicates if changes were made.
        """
        rc = DataSet.catalog(self.name, self.volumes, tmphlq=tmp_hlq)
        self.is_cataloged = True
        return rc

    def ensure_uncataloged(self, tmp_hlq=None):
        """
        Ensures the data set is uncataloged, if not catalogs it.

        Parameters
        ----------
        tmp_hlq : str
            High level qualifier for temporary datasets.

        Returns
        -------
        int
            Indicates if changes were made.
        """
        rc = DataSet.ensure_uncataloged(self.name, tmphlq=tmp_hlq)
        self.is_cataloged = False
        return rc

    def uncatalog(self, tmp_hlq=None):
        """Uncatalog the data set in question.

        Parameters
        ----------
        tmp_hlq : str
            High level qualifier for temporary datasets.

        Returns
        -------
        int
            Indicates if changes were made.
        """
        rc = DataSet.uncatalog(self.name, tmphlq=tmp_hlq)
        self.is_cataloged = False
        return rc

    def set_state(self, new_state):
        """Used to set the data set state.

        Parameters
        ----------
        new_state : str {unknown, present, absent}
            New state of the data set.

        Returns
        -------
            bool
                If state was set properly.
        """
        if new_state not in self.data_set_possible_states:
            raise ValueError(f"State {self.state} not supported for MVSDataset class.")
        return True

    def merge_attributes_from_zoau_data_set(self, zoau_data_set):
        self.name = zoau_data_set.name
        self.record_format = zoau_data_set.record_format and zoau_data_set.record_format.lower()
        self.record_length = zoau_data_set.record_length
        self.volumes = zoau_data_set.volume and zoau_data_set.volume.lower()
        self.block_size = zoau_data_set.block_size
        self.type = zoau_data_set.type and zoau_data_set.type.lower()

    @property
    def attributes(self):
        data_set_attributes = {
            "name": self.name,
            "state": self.state,
            "type": self.data_set_type,
            "space_primary": self.space_primary,
            "space_secondary": self.space_secondary,
            "space_type": self.space_type,
            "record_format": self.record_format,
            "sms_storage_class": self.sms_storage_class,
            "sms_data_class": self.sms_data_class,
            "sms_management_class": self.sms_management_class,
            "record_length": self.record_length,
            "block_size": self.block_size,
            "directory_blocks": self.directory_blocks,
            "key_offset": self.key_offset,
            "key_length": self.key_length,
            "volumes": self.volumes,
        }
        return data_set_attributes


class Member():
    """Represents a member on z/OS.

    Attributes
    ----------
    name : str
        Data set member name.
    parent_data_set_type : str {pds, pdse}
        Parent data set type.
    data_set_type : str
        Member data set type, should always be "member".
    """
    def __init__(
            self,
            name,
            parent_data_set_type="pds",
    ):
        self.name = name
        self.parent_data_set_type = parent_data_set_type
        self.data_set_type = "member"

    def ensure_absent(self, force):
        """ Make sure that the member is absent or fail deleting it.

        Parameters
        ----------
        force : bool
            Used to determine behavior when performing member operations on a pdse.

        Returns
        -------
        int
            Indicates if changes were made.
        """
        rc = DataSet.ensure_member_absent(self.name, force)
        return rc

    def ensure_present(self, replace=None, tmphlq=None):
        """ Make sure that the member is created or fail creating it.

        Parameters
        ----------
        replace : bool
            Used to determine behavior when member already exists.
        tmphlq : str
            High Level Qualifier for temporary datasets.

        Returns
        -------
        int
            Indicates if changes were made.
        """
        rc = DataSet.ensure_member_present(self.name, replace, tmphlq=tmphlq)
        return rc

    @property
    def attributes(self):
        member_attributes = {
            "name": self.name,
            "parent_data_set_type": self.parent_data_set_type,
            "data_set_type": self.data_set_type,
        }
        return member_attributes


class GenerationDataGroup():
    """Represents a Generation Data Group base in z/OS.

    Attributes
    ----------
    name : str
        The name of the GDG base.
    limit : int
        Maximum number of generations associated with this GDG base.
    empty : bool
        Empty attribute for the GDG base.
    purge : bool
        purge attribute for the GDG base.
    scratch : bool
        scratch attribute for the GDG base.
    extended : bool
        extended attribute for the GDG base. If extended a GDG base can
        have up to 999 generations, if not just up to 255.
    fifo : bool
        fifo attribute for the GDG base.
    data_set_type : str
        data_set_type will always be "gdg"
    raw_name : str
        The raw name of the data set.
    gdg : GenerationDataGroupView
        ZOAU GenerationDataGroupView object to interact with the GDG base.
    """
    def __init__(
            self,
            name,
            limit,
            empty=False,
            purge=False,
            scratch=False,
            extended=False,
            fifo=False,
    ):
        self.name = name
        self.limit = limit
        self.empty = empty
        self.purge = purge
        self.scratch = scratch
        self.extended = extended
        self.fifo = fifo
        self.data_set_type = "gdg"
        self.raw_name = name
        self.gdg = None
        self.state = 'present'

    @staticmethod
    def _validate_gdg_name(name):
        """Validates the length of a GDG name."""
        if name and len(name) > 35:
            raise GenerationDataGroupCreateError(
                msg="GDG creation failed: dataset name exceeds 35 characters."
            )

    def create(self):
        """Creates the GDG.

        Returns
        -------
        int
            Indicates if changes were made.
        """
        GenerationDataGroup._validate_gdg_name(self.name)
        gdg = gdgs.create(
            name=self.name,
            limit=self.limit,
            empty=self.empty,
            purge=self.purge,
            scratch=self.scratch,
            extended=self.extended,
            fifo=self.fifo,
        )
        self.gdg = gdg
        self.state = 'present'
        return True

    def ensure_present(self, replace):
        """Make sure that the data set is created or fail creating it.
        Parameters
        ----------
        replace : bool
            Used to determine behavior when member already exists.

        Returns
        -------
        int
            Indicates if changes were made.
        """
        arguments = vars(self)
        changed = False
        present = False
        gdg = None
        name = arguments.get("name")

        # Add this line to validate the name length before any operation
        GenerationDataGroup._validate_gdg_name(name)

        def _create_gdg(args):
            try:
                return gdgs.create(**args)
            except exceptions._ZOAUExtendableException as e:
                # Now, check if it's the specific exception we want to handle.
                if isinstance(e, GenerationDataGroupCreateException):
                    stderr = getattr(e.response, 'stderr_response', '')
                    if "BGYSC5906E" in stderr:
                        raise GenerationDataGroupCreateError(msg="FIFO creation failed: the system may not support FIFO datasets or is not configured for it.")
                    elif "BGYSC6104E" in stderr:
                        raise GenerationDataGroupCreateError(msg="GDG creation failed: 'purge=true' requires 'scratch=true'.")
                    else:
                        raise GenerationDataGroupCreateError(msg=f"GDG creation failed. Raw error: {stderr}")
                else:
                    # If it's a different ZOAU error, re-raise it.
                    raise e
        if gdgs.exists(arguments.get("name")):
            present = True

        if not present:
            gdg = _create_gdg(arguments)

        else:
            if not replace:
                return changed
            changed = self.ensure_absent(True)
            gdg = _create_gdg(arguments)
        if isinstance(gdg, gdgs.GenerationDataGroupView):
            changed = True
        return changed

    def ensure_absent(self, force, noscratch=False):
        """Ensure gdg base is deleted. If force is True and there is an
        existing GDG with active generations it will remove them and delete
        the GDG.
        Parameters
        ----------
        force : bool
            If the GDG base has active generations, remove them and delete the GDG base.

        Returns
        -------
        int
            Indicates if changes were made.
        """
        # Check whether GDG exists or not
        if gdgs.exists(name=self.name):
            # Try to delete
            rc = datasets.delete(self.name, no_scratch=noscratch)
            if rc > 0:
                if force:
                    try:
                        if isinstance(self.gdg, gdgs.GenerationDataGroupView):
                            self.gdg.delete()
                        else:
                            gdg_view = gdgs.GenerationDataGroupView(name=self.name)
                            gdg_view.delete()
                    except exceptions._ZOAUExtendableException as e:
                        stderr = getattr(e.response, 'stderr_response', '')
                        if "BGYSC1603I" in stderr:
                            raise GenerationDataGroupDeleteError(
                                msg=(
                                    "Data set deletion failed: the generation data set is currently in use "
                                    " by another job or session. Try deleting after ensuring no active usage."
                                )
                            )
                        elif "BGYSC5906E" in stderr:
                            raise GenerationDataGroupDeleteError(
                                msg="GDG deletion failed due to an IDCAMS failure. A GDS might be in use or locked."
                            )
                        else:
                            raise GenerationDataGroupDeleteError(
                                msg=f"GDG deletion failed. Raw error: {stderr}"
                            )
        else:
            return False
        return True

    def clear(self):
        """Deletes the active generations without removing the GDG base.
        Parameters
        ----------
        force : bool
            If the GDG base has active generations, remove them and delete the GDG base.

        Returns
        -------
        int
            Indicates if changes were made.
        """
        if isinstance(self.gdg, gdgs.GenerationDataGroupView):
            self.gdg.clear()
        else:
            gdg_view = gdgs.GenerationDataGroupView(name=self.name)
            gdg_view.clear()
        return True

    @property
    def attributes(self):
        data_set_attributes = {
            "name": self.name,
            "state": self.state,
            "type": self.data_set_type,
            "empty": self.empty,
            "extended": self.extended,
            "fifo": self.fifo,
            "limit": self.limit,
            "purge": self.purge,
            "scratch": self.scratch,
        }
        return data_set_attributes


def is_member(data_set):
    """Determine whether the input string specifies a data set member.

    Parameters
    ----------
    data_set : str
        Input to search in data set members.

    Returns
    -------
    bool
        If the input is a member of a data set.
    """
    try:
        arg_def = dict(data_set=dict(arg_type="data_set_member"))
        parser = better_arg_parser.BetterArgParser(arg_def)
        parser.parse_args({"data_set": data_set})
    except ValueError:
        return False
    return True


def is_data_set(data_set):
    """Determine whether the input string specifies a data set name.

    Parameters
    ----------
    data_set : str
        Input to search among data set names.

    Returns
    -------
    bool
        If the input is a data set name.
    """
    try:
        arg_def = dict(data_set=dict(arg_type="data_set_base"))
        parser = better_arg_parser.BetterArgParser(arg_def)
        parser.parse_args({"data_set": data_set})
    except ValueError:
        return False
    return True


def extract_dsname(data_set):
    """Extract the actual name of the data set from a given input source.

    Parameters
    ----------
    data_set : str
        Input data set name.

    Returns
    -------
    str
        The actual name of the data set.
    """
    result = ""
    for c in data_set:
        if c == "(":
            break
        result += c
    return result


def extract_member_name(data_set):
    """Extract the member name from a given input source.

    Parameters
    ----------
    data_set : str
        Input source name.

    Returns
    -------
    str
        The member name.
    """
    start = data_set.find("(")
    member = ""
    for i in range(start + 1, len(data_set)):
        if data_set[i] == ")":
            break
        member += data_set[i]
    return member


def temp_member_name():
    """Generate a temp member name.

    Returns
    -------
    str
        The temp name.
    """
    first_char_set = ascii_uppercase + "#@$"
    rest_char_set = ascii_uppercase + digits + "#@$"
    # using sample as k=1 and k=7 to avoid using random.choice just for oneline import
    # Issue: https://bandit.readthedocs.io/en/latest/blacklists/blacklist_calls.html#b311-random
    # Standard pseudo-random generators are not suitable for security/cryptographic purposes.
    # Ignoring this bandit blacklisted issue because we are not using this pseudo-random generator for
    # SECURITY/CRYPTOGRAPHIC purposes but rather a random sample of characters for a random name generator.
    temp_name = sample(first_char_set, k=1)  # nosec B311
    temp_name += sample(rest_char_set, k=7)  # nosec B311
    temp_name = "".join(temp_name)
    return temp_name


class DatasetDeleteError(Exception):
    def __init__(self, data_set, rc):
        """Error during deletion of a data set.

        Parameters
        ----------
        data_set : str
            Name of the data set that it tried to delete.
        rc : int
            Return code.

        Attributes
        ----------
        msg : str
            Human readable string describing the exception.
        """
        self.msg = 'An error occurred during deletion of data set "{0}". RC={1}'.format(
            data_set, rc
        )
        super().__init__(self.msg)


class DatasetCreateError(Exception):
    def __init__(self, data_set, rc=None, msg=""):
        """Error during creation of a data set.

        Parameters
        ----------
        data_set : str
            Name of the data set that it tried to create.
        rc : int
            Return code.
        msg : str
            Human readable string describing the exception.

        Attributes
        ----------
        msg : str
            Human readable string describing the exception.
        """
        if rc:
            self.msg = (
                'An error occurred during creation of data set "{0}". RC={1}, {2}'.format(
                    data_set, rc, msg
                )
            )
        else:
            self.msg = (
                'An error occurred during creation of data set "{0}". {1}'.format(
                    data_set, msg
                )
            )
        super().__init__(self.msg)


class DatasetMemberDeleteError(Exception):
    def __init__(self, data_set, rc):
        """Error during deletion of a data set's member.

        Parameters
        ----------
        data_set : str
            Name of the data set and member that it tried to delete.
        rc : int
            Return code.

        Attributes
        ----------
        msg : str
            Human readable string describing the exception.
        """
        self.msg = (
            'An error occurred during deletion of data set member"{0}". RC={1}'.format(
                data_set, rc
            )
        )
        super().__init__(self.msg)


class DatasetMemberCreateError(Exception):
    def __init__(self, data_set, rc):
        """Error during creation of a data set's member.

        Parameters
        ----------
        data_set : str
            Name of the data set and member that it tried to create.
        rc : int
            Return code.

        Attributes
        ----------
        msg : str
            Human readable string describing the exception.
        """
        self.msg = (
            'An error occurred during creation of data set member"{0}". RC={1}'.format(
                data_set, rc
            )
        )
        super().__init__(self.msg)


class DatasetNotFoundError(Exception):
    def __init__(self, data_set):
        """Failed to found the data set.

        Parameters
        ----------
        data_set : str
            Name of the data set that it tried to find.

        Attributes
        ----------
        msg : str
            Human readable string describing the exception.
        """
        self.msg = 'The data set "{0}" could not be located.'.format(data_set)
        super().__init__(self.msg)


class DatasetCatalogError(Exception):
    def __init__(self, data_set, volumes, rc, message=""):
        """Error during cataloging of a data set.

        Parameters
        ----------
        data_set : str
            Name of the data set that it tried to catalog.
        volumes : list[str]
            Volume the data set is in.
        rc : int
            Return code.
        message : str
            Human readable string describing the exception.

        Attributes
        ----------
        msg : str
            Human readable string describing the exception.
        """
        self.msg = 'An error occurred during cataloging of data set "{0}" on volume(s) "{1}". RC={2}. {3}'.format(
            data_set, ", ".join(volumes), rc, message
        )
        super().__init__(self.msg)


class DatasetCatalogedOnDifferentVolumeError(Exception):
    def __init__(self, name, existing_volumes, requested_volumes):
        existing_vol_str = ", ".join(existing_volumes) if existing_volumes else "none"
        requested_vol_str = ", ".join(requested_volumes) if requested_volumes else "none"
        self.msg = (
            "Data set {0} is cataloged with volume {1}, if you want to create data set {0} "
            "in volume {2} uncatalog the data set first and then create it."
        ).format(name, existing_vol_str, requested_vol_str)
        super().__init__(self.msg)


class DatasetUncatalogError(Exception):
    def __init__(self, data_set, rc):
        """Error during uncaloging of a data set.

        Parameters
        ----------
        data_set : str
            Name of the data set that it tried to uncatalog.
        rc : int
            Return code.

        Attributes
        ----------
        msg : str
            Human readable string describing the exception.
        """
        self.msg = (
            'An error occurred during uncatalog of data set "{0}". RC={1}'.format(
                data_set, rc
            )
        )
        super().__init__(self.msg)


class DatasetWriteError(Exception):
    def __init__(self, data_set, rc, message=""):
        """Error during write of a data set.

        Parameters
        ----------
        data_set : str
            Name of the data set that it tried to write.
        rc : int
            Return code.
        msg : str
            Human readable string describing the exception.

        Attributes
        ----------
        msg : str
            Human readable string describing the exception.
        """
        self.msg = (
            'An error occurred during write of data set "{0}". RC={1}. {2}'.format(
                data_set, rc, message
            )
        )
        super().__init__(self.msg)


class DatasetFormatError(Exception):
    def __init__(self, data_set, rc, message=""):
        """Error during formating of a data set.

        Parameters
        ----------
        data_set : str
            Name of the data set that it tried to format.
        rc : int
            Return code.
        msg : str
            Human readable string describing the exception.

        Attributes
        ----------
        msg : str
            Human readable string describing the exception.
        """
        self.msg = (
            'An error occurred during format of data set "{0}". RC={1}. {2}'.format(
                data_set, rc, message
            )
        )
        super().__init__(self.msg)


class MVSCmdExecError(Exception):
    def __init__(self, rc, out, err):
        """Error during cmd execution.

        Parameters
        ----------
        rc : int
            Return code.
        out : str
            Output of the error.
        err : str
            The error.

        Attributes
        ----------
        msg : str
            Human readable string describing the exception.
        """
        self.msg = (
            "Failure during execution of mvscmd; Return code: {0}; "
            "stdout: {1}; stderr: {2}".format(rc, out, err)
        )
        super().__init__(self.msg)


class DatasetVolumeError(Exception):
    def __init__(self, data_set):
        """Error trying to find of a data set on a volume.

        Parameters
        ----------
        data_set : str
            Name of the data set that it tried to find.

        Attributes
        ----------
        msg : str
            Human readable string describing the exception.
        """
        self.msg = (
            "The data set {0} could not be found on a volume in the system.".format(
                data_set)
        )
        super().__init__(self.msg)


class DatasetBusyError(Exception):
    def __init__(self, data_set):
        """Error trying to open a dataset due to it being busy.

        Parameters
        ----------
        data_set : str
            Name of the data set that it tried to open.

        Attributes
        ----------
        msg : str
            Human readable string describing the exception.
        """
        self.msg = (
            "Dataset {0} may already be open by another user. "
            "Close the dataset and try again".format(data_set)
        )
        super().__init__(self.msg)


class GDSNameResolveError(Exception):
    def __init__(self, data_set):
        self.msg = (
            "Error resolving relative generation data set name. {0} "
            "Make sure the generation exists and is active.".format(data_set)
        )
        super().__init__(self.msg)


class GenerationDataGroupCreateError(Exception):
    def __init__(self, msg):
        """Error during creation of a Generation Data Group."""
        self.msg = msg
        super().__init__(self.msg)


class GenerationDataGroupDeleteError(Exception):
    def __init__(self, msg):
        """Error during deletion of a Generation Data Group."""
        self.msg = msg
        super().__init__(self.msg)
