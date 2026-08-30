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

from tempfile import NamedTemporaryFile, mkstemp, mkdtemp
from math import floor, ceil
from os import path, walk, makedirs, unlink

import shutil
import errno
import os
import re
import locale
import traceback

from ansible_collections.ibm.ibm_zos_core.plugins.module_utils.import_handler import (
    ZOAUImportError,
)
from ansible_collections.ibm.ibm_zos_core.plugins.module_utils.better_arg_parser import (
    BetterArgParser,
)
from ansible_collections.ibm.ibm_zos_core.plugins.module_utils import copy, system, validation
from ansible_collections.ibm.ibm_zos_core.plugins.module_utils.ansible_module import (
    AnsibleModuleHelper,
)

try:
    from zoautil_py import datasets
except Exception:
    datasets = ZOAUImportError(traceback.format_exc())

from shlex import quote


class Defaults:
    DEFAULT_ASCII_CHARSET = "UTF-8"
    DEFAULT_EBCDIC_USS_CHARSET = "IBM-1047"
    DEFAULT_EBCDIC_MVS_CHARSET = "IBM-037"

    @staticmethod
    def get_default_system_charset():
        """Get the default encoding of the current machine.

        Returns
        -------
        str
            The encoding of the current machine.
        """
        system_charset = locale.getdefaultlocale()[1]
        if system_charset is None:
            module = AnsibleModuleHelper(argument_spec={})
            rc, out, err = module.run_command("locale -c charmap", errors='replace')
            if rc != 0 or not out or err:
                if system.is_zos():
                    system_charset = Defaults.DEFAULT_EBCDIC_USS_CHARSET
                else:
                    system_charset = Defaults.DEFAULT_ASCII_CHARSET
            else:
                out = out.splitlines()
                system_charset = out[1].strip() if len(out) > 1 else out[0].strip()

        return system_charset


class EncodeUtils(object):
    def __init__(self):
        """Call the coded character set conversion utility iconv
        to convert a USS file from one coded character set to another.
        """
        self.module = AnsibleModuleHelper(argument_spec={})
        self.tmphlq = None

    def _validate_data_set_name(self, ds):
        """Validate data set name using BetterArgParser.

        Parameters
        ----------
        ds : str
            The source data set name.

        Returns
        -------
        str
            Parsed data set name.
        """
        arg_defs = dict(
            ds=dict(arg_type="data_set"),
        )
        parser = BetterArgParser(arg_defs)
        parsed_args = parser.parse_args({"ds": ds})
        return parsed_args.get("ds")

    def _validate_path(self, path):
        """Validate path using BetterArgParser.

        Parameters
        ----------
        path : str
            The path.

        Returns
        -------
        str
            Parsed path.
        """
        arg_defs = dict(
            path=dict(arg_type="path"),
        )
        parser = BetterArgParser(arg_defs)
        parsed_args = parser.parse_args({"path": path})
        return parsed_args.get("path")

    def _validate_data_set_or_path(self, path):
        """Validate data set or path using BetterArgParser.

        Parameters
        ----------
        path : str
            The path.

        Returns
        -------
        str
            Parsed path.
        """
        arg_defs = dict(
            path=dict(arg_type="data_set_or_path"),
        )
        parser = BetterArgParser(arg_defs)
        parsed_args = parser.parse_args({"path": path})
        return parsed_args.get("path")

    def _validate_encoding(self, encoding):
        """Validate encoding using BetterArgParser.

        Parameters
        ---------
        encoding : str
            The encoding.

        Returns
        -------
        str
            Parsed encoding.
        """
        arg_defs = dict(
            encoding=dict(arg_type="encoding"),
        )
        parser = BetterArgParser(arg_defs)
        parsed_args = parser.parse_args({"encoding": encoding})
        return parsed_args.get("encoding")

    def listdsi_data_set(self, ds, tmphlq=None):
        """Invoke IDCAMS LISTCAT command to get the record length and space used
        to estimate the space used by the VSAM data set.

        Parameters
        ----------
        ds : str
            The VSAM data set to be checked.
        tmphlq : str
            High Level Qualifier for temporary datasets.

        Returns
        -------
        int
            The maximum record length of the VSAM data set.
        int
            The space used by the VSAM data set(KB).

        Raises
        ------
        EncodeError
            When any exception is raised during the conversion.
        """
        ds = self._validate_data_set_name(ds)
        reclen = 80
        space_u = 1024
        listcat_cmd = " LISTCAT ENT('{0}') ALL".format(ds)

        cmd = "mvscmdauth --pgm=ikjeft01 --systsprt=stdout --systsin=stdin"
        if tmphlq:
            cmd = "{0} -Q={1}".format(cmd, tmphlq)

        rc, out, err = self.module.run_command(cmd, data=listcat_cmd, errors='replace')
        if rc:
            raise EncodeError(err)
        if out:
            find_reclen = re.findall(r"MAXLRECL-*\d+", out)
            find_cisize = re.findall(r"CISIZE-*\d+", out)
            find_recnum = re.findall(r"REC-TOTAL-*\d+", out)
            find_freeci = re.findall(r"FREESPACE-%CI-*\d+", out)
            find_freeca = re.findall(r"FREESPACE-%CA-*\d+", out)
            find_cioca = re.findall(r"CI/CA-*\d+", out)
            find_trkoca = re.findall(r"TRACKS/CA-*\d+", out)
            if find_reclen:
                reclen = int("".join(re.findall(r"\d+", find_reclen[0])))
            if find_cisize:
                cisize = int("".join(re.findall(r"\d+", find_cisize[0])))
            if find_recnum:
                recnum = int("".join(re.findall(r"\d+", find_recnum[0])))
            if find_freeci:
                freeci = int("".join(re.findall(r"\d+", find_freeci[0])))
            if find_freeca:
                freeca = int("".join(re.findall(r"\d+", find_freeca[0])))
            if find_cioca:
                cioca = int("".join(re.findall(r"\d+", find_cioca[0])))
            if find_trkoca:
                trkoca = int("".join(re.findall(r"\d+", find_trkoca[0])))

            # Algorithm used for VSAM data set space evaluation
            # Step01. Get the number of records in each VSAM CI
            # Step02. The CI used by the VSAM data set
            # Step03. The CA used by the VSAM data set
            # Step04. Calculate the VSAM data set space using the CA number
            # This value will be used by the temporary PS when coping a VSAM data set
            # For DASD volume type 3390, 56664 bytes per track
            rec_in_ci = floor((cisize - cisize * freeci - 10) / reclen)
            ci_num = ceil(recnum / rec_in_ci)
            ca_num = ceil(ci_num / (cioca * (1 - freeca)))
            if ca_num > 0:
                space_u = ceil(ca_num * trkoca * 566664 / 1024)
        return reclen, space_u

    def temp_data_set(self, reclen, space_u):
        """Creates a temporary data set with the given record length and size.

        Parameters
        ----------
        lrecl : int
            The record length of the data set.
        space_u : str
            The size of the data set.

        Returns
        -------
        str
            Name of the allocated data set.

        Raises
        ------
        ZOAUException
            When any exception is raised during the data set allocation.
            DatasetVerificationError: When the data set creation could not be verified.
        """
        size = str(space_u * 2) + "K"
        if self.tmphlq:
            hlq = self.tmphlq
        else:
            hlq = datasets.get_hlq()
        temp_ps = datasets.tmp_name(high_level_qualifier=hlq)
        temporary_data_set = datasets.create(
            name=temp_ps,
            dataset_type="SEQ",
            primary_space=size,
            record_format="VB",
            record_length=reclen,
        )
        return temporary_data_set.name

    def get_codeset(self):
        """Get the list of supported encodings from the  USS command 'iconv -l'.

        Returns
        -------
        Union[str]
            The code set list supported in current USS platform.

        Raises
        ------
        EncodeError
            When any exception is raised during the conversion.
        """
        code_set = None
        iconv_list_cmd = ["iconv", "-l"]
        rc, out, err = self.module.run_command(iconv_list_cmd, errors='replace')
        if rc:
            raise EncodeError(err)
        if out:
            code_set_list = list(filter(None, re.split(r"[\n|\t]", out)))
            code_set = [c for i, c in enumerate(code_set_list) if i > 0 and i % 2 == 0]
        return code_set

    def string_convert_encoding(self, src, from_encoding, to_encoding):
        """Convert the encoding of the data when the src is a normal string.

        Parameters
        ----------
        src : str
            The input string content.
        from_encoding : str
            The source code set of the string.
        to_encoding : str
            The destination code set for the string.

        Returns
        -------
        str
            The string content after the encoding.

        Raises
        ------
        EncodeError
            When any exception is raised during the conversion.
        """
        from_encoding = self._validate_encoding(from_encoding)
        to_encoding = self._validate_encoding(to_encoding)
        iconv_cmd = "printf {0} | iconv -f {1} -t {2}".format(
            quote(src), quote(from_encoding), quote(to_encoding)
        )
        rc, out, err = self.module.run_command(iconv_cmd, use_unsafe_shell=True, errors='replace')
        if rc:
            raise EncodeError(err)
        return out

    def uss_convert_encoding(self, src, dest, from_code, to_code):
        """Convert the encoding of the data in a USS file.

        Parameters
        ----------
        src : str
            The input file name, it should be a uss file.
        dest : str
            The output file name, it should be a uss file.
        from_code : str
            The source code set of the input file.
        to_code : str
            The destination code set for the output file.

        Returns
        -------
        bool
            Indicate whether the conversion is successful or not.

        Raises
        ------
        EncodeError
            When any exception is raised during the conversion.
        MoveFileError
            When any exception is raised during moving files.
        """
        src = self._validate_path(src)
        dest = self._validate_path(dest)
        from_code = self._validate_encoding(from_code)
        to_code = self._validate_encoding(to_code)
        convert_rc = False
        temp_fo = None
        if not src == dest:
            temp_fi = dest
        else:
            temp_fo, temp_fi = mkstemp()
        iconv_cmd = "iconv -f {0} -t {1} {2} > {3}".format(
            quote(from_code), quote(to_code), quote(src), quote(temp_fi)
        )
        try:
            rc, out, err = self.module.run_command(iconv_cmd, use_unsafe_shell=True, errors='replace')
            if rc:
                raise EncodeError(err)
            if dest == temp_fi:
                convert_rc = True
            else:
                try:
                    src_mode = os.stat(src).st_mode
                    temp_mode = os.stat(temp_fi).st_mode
                    if src_mode != temp_mode:
                        os.chmod(temp_fi, src_mode)
                    shutil.move(temp_fi, dest)
                    convert_rc = True
                except (OSError, IOError) as e:
                    raise MoveFileError(src, dest, e)
        except Exception:
            raise
        finally:
            if temp_fo:
                try:
                    os.close(temp_fo)
                    unlink(temp_fi)
                except OSError as e:
                    if e.errno != errno.ENOENT:
                        raise
        return convert_rc

    def uss_convert_encoding_prev(self, src, dest, from_code, to_code):
        """For multiple files conversion, such as a USS path or MVS PDS data set,
        use this method to split then do the conversion.

        Parameters
        ----------
        src : str
            The input uss path or a file.
        dest : str
            The output uss path or a file.
        from_code : str
            The source code set of the input path.
        to_code : str
            The destination code set for the output path.

        Returns
        -------
        bool
            Indicate whether the conversion is successful or not.

        Raises
        ------
        EncodeError
            When directory is empty or copy multiple files to a single file.
        """
        src = self._validate_path(src)
        dest = self._validate_path(dest)
        from_code = self._validate_encoding(from_code)
        to_code = self._validate_encoding(to_code)
        convert_rc = False
        file_list = list()
        if path.isdir(src):
            for (dir, subdir, files) in walk(src):
                for file in files:
                    file_list.append(path.join(validation.validate_safe_path(dir), validation.validate_safe_path(file)))
            if len(file_list) == 0:
                raise EncodeError(
                    "Directory {0} is empty. Please check the path.".format(src)
                )
            elif len(file_list) == 1:
                dest_f = dest
                src_f = src
                if path.isdir(dest):
                    file_name = path.basename(file_list[0])
                    src_f = path.join(validation.validate_safe_path(src), validation.validate_safe_path(file_name))
                    dest_f = path.join(validation.validate_safe_path(dest), validation.validate_safe_path(file_name))
                convert_rc = self.uss_convert_encoding(
                    src_f, dest_f, from_code, to_code
                )
            else:
                if path.isfile(dest):
                    raise EncodeError(
                        "Can't convert multiple files (src) {0} to a single file"
                        " (dest) {1}.".format(src, dest)
                    )
                else:
                    for file in file_list:
                        if dest == src:
                            dest_f = file
                        else:
                            dest_f = file.replace(src, dest, 1)
                            dest_dir = path.dirname(dest_f)
                            if not path.exists(dest_dir):
                                makedirs(dest_dir)
                        convert_rc = self.uss_convert_encoding(
                            file, dest_f, from_code, to_code
                        )
        else:
            if path.isdir(dest):
                file_name = path.basename(path.abspath(src))
                dest = path.join(validation.validate_safe_path(dest), validation.validate_safe_path(file_name))
            convert_rc = self.uss_convert_encoding(src, dest, from_code, to_code)

        return convert_rc

    def mvs_convert_encoding(
        self, src, dest, from_code, to_code, src_type=None, dest_type=None, tmphlq=None
    ):
        """Convert the encoding of the data from
           1) USS to MVS(PS, PDS/E VSAM)
           2) MVS to USS
           3) MVS to MVS

        Parameters
        ----------
        src : str
            The input MVS data set or USS path to be converted.
        dest : str
            The output MVS data set or USS path to be converted.
        from_code : str
            The source code set of the input MVS data set.
        to_code : str
            The destination code set of the output MVS data set.

        Keyword Parameters
        -----------------
        src_type : str
            The input MVS data set or type: PS, PDS, PDSE, VSAM(KSDS).
        dest_type : str
            The output MVS data set type.
        tmphlq : str
            High Level Qualifier for temporary datasets.

        Returns
        -------
        bool
            Indicate whether the conversion is successful or not.
        """
        src = self._validate_data_set_or_path(src)
        dest = self._validate_data_set_or_path(dest)
        from_code = self._validate_encoding(from_code)
        to_code = self._validate_encoding(to_code)
        convert_rc = False
        temp_ps = None
        temp_src = src
        temp_dest = dest
        try:
            if src_type == "PS":
                temp_src_fo = NamedTemporaryFile()
                temp_src = temp_src_fo.name
                rc, out, err = copy.copy_uss_mvs(src, temp_src)
            if src_type == "PO":
                temp_src = mkdtemp()
                rc, out, err = copy.copy_uss_mvs(src, temp_src)
            if src_type == "KSDS":
                reclen, space_u = self.listdsi_data_set(src.upper(), tmphlq=tmphlq)
                # RDW takes the first 4 bytes in the VB format, hence we need to add an extra buffer to the vsam max recl.
                reclen += 4
                temp_ps = self.temp_data_set(reclen, space_u)
                rc, out, err = copy.copy_vsam_ps(src.upper(), temp_ps, tmphlq=tmphlq)
                temp_src_fo = NamedTemporaryFile()
                temp_src = temp_src_fo.name
                rc, out, err = copy.copy_uss_mvs(temp_ps, temp_src)
            if dest_type == "PS" or dest_type == "KSDS":
                temp_dest_fo = NamedTemporaryFile()
                temp_dest = temp_dest_fo.name
            if dest_type == "PO":
                temp_dest = mkdtemp()
            rc = self.uss_convert_encoding_prev(temp_src, temp_dest, from_code, to_code)
            if rc:
                if not dest_type:
                    convert_rc = True
                else:
                    if dest_type == "KSDS":
                        reclen, space_u = self.listdsi_data_set(dest.upper(), tmphlq=tmphlq)
                        # RDW takes the first 4 bytes or records in the VB format, hence we need to add an extra buffer to the vsam max recl.
                        reclen += 4
                        temp_ps = self.temp_data_set(reclen, space_u)
                        rc, out, err = copy.copy_uss_mvs(temp_dest, temp_ps)
                        rc, out, err = copy.copy_vsam_ps(temp_ps, dest.upper(), tmphlq=tmphlq)
                        convert_rc = True
                    elif dest_type == "PO":
                        for (dir, subdir, files) in walk(temp_dest):
                            for file in files:
                                temp_file = path.join(validation.validate_safe_path(dir), validation.validate_safe_path(file))
                                rc, out, err = copy.copy_uss_mvs(temp_file, dest)
                                convert_rc = True
                    else:
                        rc, out, err = copy.copy_uss_mvs(temp_dest, dest)
                        convert_rc = True
        except Exception:
            raise
        finally:
            if temp_ps:
                datasets.delete(temp_ps)
            if temp_src and temp_src != src:
                if os.path.isdir(temp_src):
                    shutil.rmtree(temp_src)
            if temp_dest and temp_dest != dest:
                if os.path.isdir(temp_dest):
                    shutil.rmtree(temp_dest)

        return convert_rc

    def uss_tag_encoding(self, file_path, tag):
        """Tag the file/directory specified with the given code set.
        If `file_path` is a directory, all of the files and subdirectories will
        be tagged recursively.

        Parameters
        ----------
        file_path : str
            Absolute file path to tag.
        tag : str
            Code set to tag the file/directory.

        Raises
        ------
        TaggingError
            When the chtag command fails.
        """
        is_dir = os.path.isdir(file_path)

        tag_cmd = "chtag -{0}c {1} {2}".format("R" if is_dir else "t", tag, file_path)
        rc, out, err = self.module.run_command(tag_cmd, errors='replace')
        if rc != 0:
            raise TaggingError(file_path, tag, rc, out, err)

    def uss_file_tag(self, file_path):
        """Returns the current tag set for a file.

        Parameters
        ----------
        file_path : str
            USS path to the file.

        Returns
        -------
        str
            Current tag set for the file, as returned by 'ls -T'.
        None
            If the file does not exist or the command fails.
        """
        if not os.path.exists(file_path):
            return None

        try:
            tag_cmd = "ls -T {0}".format(file_path)
            rc, stdout, stderr = self.module.run_command(tag_cmd, errors='replace')

            if rc != 0:
                return None

            # The output from 'ls -T' should be like this:
            # t IBM-037     T=on  ansible-zos-copy-payload-D230123-T123818
            # The second item from the split should be the tag.
            ls_parts = stdout.split()
            return ls_parts[1]
        except Exception:
            return None


class EncodeError(Exception):
    def __init__(self, message):
        """Error during encoding.

        Parameters
        ----------
        message : str
            Human readable string describing the exception.

        Attributes
        ----------
        msg : str
            Human readable string describing the exception.
        """
        self.msg = 'An error occurred during encoding: "{0}"'.format(message)
        super(EncodeError, self).__init__(self.msg)


class TaggingError(Exception):
    def __init__(self, file_path, tag, rc, stdout, stderr):
        """Error during tagging.

        Parameters
        ----------
        file_path : str
            File to tag.
        tag : str
            Tag to put in the file.
        rc : int
            Return code.
        stdout : str
            Standard output.
        stderr : str
            Standard error.

        Attributes
        ----------
        msg : str
            Human readable string describing the exception.
        rc : int
            Return code.
        stdout : str
            Standard output.
        stderr : str
            Standard error.
        """
        self.msg = 'An error occurred during tagging of {0} to {1}'.format(
            file_path,
            tag
        )
        self.rc = rc
        self.stdout = stdout
        self.stderr = stderr
        super(TaggingError, self).__init__(self.msg)


class MoveFileError(Exception):
    def __init__(self, src, dest, e):
        """Error while moving a file.

        Parameters
        ----------
        src : str
            From where the file moves.
        dest : str
            To where the file moves.
        e : str
            Exception message.

        Attributes
        ----------
        msg : str
            Human readable string describing the exception.
        """
        self.msg = "Failed when moving {0} to {1}: {2}".format(src, dest, e)
        super().__init__(self.msg)
