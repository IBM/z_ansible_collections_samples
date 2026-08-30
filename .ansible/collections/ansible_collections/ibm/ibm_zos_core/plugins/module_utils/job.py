# Copyright (c) IBM Corporation 2019, 2025
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

import fnmatch
import re
import traceback
from time import sleep
from timeit import default_timer as timer
# Only importing this module so we can catch a JSONDecodeError that sometimes happens
# when a job's output has non-printable chars that conflict with JSON's control
# chars.
from json import JSONDecodeError
from ansible_collections.ibm.ibm_zos_core.plugins.module_utils.better_arg_parser import (
    BetterArgParser,
)
from ansible_collections.ibm.ibm_zos_core.plugins.module_utils.import_handler import (
    ZOAUImportError
)

try:
    from zoautil_py import exceptions
except ImportError:
    exceptions = ZOAUImportError(traceback.format_exc())

try:
    # For files that import individual functions from a ZOAU module,
    # we'll replace the imports to instead get the module.
    # This way, we'll always make a call to the module, allowing us
    # to properly get the exception we need and avoid the issue
    # described in #837.
    # from zoautil_py.jobs import read_output, list_dds, listing
    from zoautil_py import jobs
except Exception:
    jobs = ZOAUImportError(traceback.format_exc())

JOB_ERROR_STATUSES = frozenset(["ABEND",      # ZOAU job ended abnormally
                                "SEC ERROR",  # Security error (legacy Ansible code)
                                "SEC",        # ZOAU security error
                                "JCL ERROR",  # Job had a JCL error (legacy Ansible code)
                                "JCLERR",     # ZOAU job had a JCL error
                                "CANCELED",   # ZOAU job was cancelled
                                "CAB",        # ZOAU converter abend
                                "CNV",        # ZOAU converter error
                                "SYS",        # ZOAU system failure
                                "FLU"         # ZOAU job was flushed
                                ])


def job_output(job_id=None, owner=None, job_name=None, dd_name=None, sysin=False, dd_scan=True, duration=0, timeout=0, start_time=timer()):
    """Get the output from a z/OS job based on various search criteria.

    Keyword Parameters
    ------------------
    job_id : str
        The job ID to search for (default: {None}).
    owner : str
        The owner of the job (default: {None}).
    job_name : str
        The job name search for (default: {None}).
    dd_name : str
        The data definition to retrieve (default: {None}).
    sysin : bool
        The input DD to retrieve SYSIN value (default: {False}).
    dd_scan : bool
        Whether or not to pull information from the dd's for this job {default: {True}}.
    duration : int
        The time the submitted job ran for.
    timeout : int
        How long to wait in seconds for a job to complete.
    start_time : int
        Time the JCL started its submission.

    Returns
    -------
    Union[dict]
        The output information for a list of jobs matching specified criteria.
        If no job status is found it will return a ret_code diction with
        parameter 'msg_txt" = "The job could not be found.
    """
    arg_defs = dict(
        job_id=dict(arg_type="qualifier_pattern"),
        owner=dict(arg_type="username_pattern"),
        job_name=dict(arg_type="qualifier_pattern"),
        dd_name=dict(arg_type=_ddname_pattern),
    )

    parser = BetterArgParser(arg_defs)
    parsed_args = parser.parse_args({
        "job_id": job_id,
        "owner": owner,
        "job_name": job_name,
        "dd_name": dd_name
    })
    job_id = parsed_args.get("job_id") or "*"
    job_name = parsed_args.get("job_name") or "*"
    owner = parsed_args.get("owner") or "*"
    dd_name = parsed_args.get("dd_name") or ""

    job_detail = _get_job_status(
        job_id=job_id,
        owner=owner,
        job_name=job_name,
        dd_name=dd_name,
        duration=duration,
        sysin=sysin,
        dd_scan=dd_scan,
        timeout=timeout,
        start_time=start_time
    )

    if len(job_detail) == 0:
        # some systems have issues with "*" while some require it to see results
        job_id = "" if job_id == "*" else job_id
        owner = "" if owner == "*" else owner
        job_name = "" if job_name == "*" else job_name

        job_detail = _get_job_status(
            job_id=job_id,
            owner=owner,
            job_name=job_name,
            dd_name=dd_name,
            sysin=sysin,
            dd_scan=dd_scan,
            duration=duration,
            timeout=timeout,
            start_time=start_time
        )
    return job_detail


def _job_not_found(job_id, owner, job_name, dd_name):
    """Returns the information of a not founded job.

    Keyword Parameters
    ------------------
    job_id : str
        The job ID to search for (default: {None}).
    owner : str
        The owner of the job (default: {None}).
    job_name : str
        The job name search for (default: {None}).
    dd_name : str
        The data definition to retrieve (default: {None}).

    Returns
    -------
    Union[dict]
        The empty job information in a list.
        If no job status is found it will return a ret_code diction with
        parameter 'msg_txt" = "The job could not be found.
    """
    # Note that the text in the msg_txt is used in test cases and thus sensitive to change
    jobs = []
    if job_id != '*' and job_name != '*':
        job_not_found_msg = "{0} with the job_id {1}".format(job_name.upper(), job_id.upper())
    elif job_id != '*':
        job_not_found_msg = "with the job_id {0}".format(job_id.upper())
    else:
        job_not_found_msg = "with the name {0}".format(job_name.upper())
    job = {}

    job["job_not_found"] = True
    job["job_id"] = job_id
    job["job_name"] = job_name
    job["subsystem"] = None
    job["system"] = None
    job["owner"] = owner
    job["cpu_time"] = None
    job["execution_node"] = None
    job["origin_node"] = None
    job["content_type"] = None
    job["creation_date"] = None
    job["creation_time"] = None
    job["execution_time"] = None
    job["job_class"] = None
    job["svc_class"] = None
    job["priority"] = None
    job["asid"] = None
    job["queue_position"] = None
    job["program_name"] = None

    job["ret_code"] = {}
    job["ret_code"]["msg"] = None
    job["ret_code"]["code"] = None
    job["ret_code"]["msg_code"] = None
    job["ret_code"]["msg_txt"] = "The job {0} could not be found.".format(job_not_found_msg)
    job["steps"] = []

    job["class"] = None

    job["dds"] = []
    dd = {}
    dd["dd_name"] = dd_name
    dd["record_count"] = 0
    dd["id"] = None
    dd["stepname"] = None
    dd["procstep"] = None
    dd["byte_count"] = 0
    dd["content"] = None
    job["dds"].append(dd)

    jobs.append(job)

    return jobs


def job_status(job_id=None, owner=None, job_name=None, dd_name=None):
    """Get the status information of a z/OS job based on various search criteria.

    Keyword Parameters
    ------------------
    job_id : str
        The job ID to search for (default: {None}).
    owner : str
        The owner of the job (default: {None}).
    job_name : str
        The job name search for (default: {None}).
    dd_name : str
        If populated, return ONLY this DD in the job list (default: {None})
        note: no routines call job_status with dd_name, so we are speeding this routine with
        'dd_scan=False'.

    Returns
    -------
    Union[dict]
        The status information for a list of jobs matching search criteria.
        If no job status is found it will return a ret_code diction with
        parameter 'msg_txt" = "The job could not be found.".

    """
    arg_defs = dict(
        job_id=dict(arg_type="str"),
        owner=dict(arg_type="username_pattern"),
        job_name=dict(arg_type="str"),
    )

    parser = BetterArgParser(arg_defs)
    parsed_args = parser.parse_args(
        {"job_id": job_id, "owner": owner, "job_name": job_name}
    )

    job_id = parsed_args.get("job_id") or "*"
    job_name = parsed_args.get("job_name") or "*"
    owner = parsed_args.get("owner") or "*"

    job_status_result = _get_job_status(
        job_id=job_id,
        owner=owner,
        job_name=job_name,
        dd_scan=False,
    )

    if len(job_status_result) == 0:
        job_id = "" if job_id == "*" else job_id
        job_name = "" if job_name == "*" else job_name
        owner = "" if owner == "*" else owner

        job_status_result = _get_job_status(
            job_id=job_id,
            owner=owner,
            job_name=job_name,
            dd_scan=False,
        )

    return job_status_result


def _parse_steps(job_str):
    """Parse the dd section of output to retrieve step-wise CC's.

    Parameters
    ----------
    job_str : str
        The content for a given dd.

    Returns
    -------
    Union[dict]
        A list of step names listed as "step executed" the related CC.
    """
    stp = []
    if "STEP WAS EXECUTED" in job_str:
        steps = re.findall(
            r"(.*?)\s-\sSTEP\sWAS\sEXECUTED\s-\s(.*?)\n", job_str)
        for match in steps:
            st = {
                "step_name": match[0].split()[-1],
                "step_cc": int(match[1].split()[-1]) if match[1].split()[-1] is not None else None,
            }
            stp.append(st)

    return stp


def _get_job_status(job_id="*", owner="*", job_name="*", dd_name=None, sysin=False, dd_scan=True, duration=0, timeout=0, start_time=timer()):
    """Get job status.

    Parameters
    ----------
    job_id : str
        The job ID to search for (default: {None}).
    owner : str
        The owner of the job (default: {None}).
    job_name : str
        The job name search for (default: {None}).
    dd_name : str
        The data definition to retrieve (default: {None}).
    sysin : bool
        The input DD SYSIN (default: {False}).
    dd_scan : bool
        Whether or not to pull information from the dd's for this job {default: {True}}.
    duration : int
        The time the submitted job ran for.
    timeout : int
        How long to wait in seconds for a job to complete.
    start_time : int
        Time the JCL started its submission.

    Returns
    -------
    Union[dict]
        The output information for a list of jobs matching specified criteria.
        If no job status is found it will return a ret_code diction with
        parameter 'msg_txt" = "The job could not be found.
    """
    if job_id == "*":
        job_id_temp = None
    else:
        # Preserve the original job_id for the failure path
        job_id_temp = job_id

    final_entries = []

    # In ZOAU>= 1.3.0, include_extended has to be set to true so we get the program name for a job.
    # Observation shows the job_name parameter is not being used, so we will drop that

    # expanding > 1.3.0 of zoau, to include all params
    entries = jobs.fetch_multiple(job_id=job_id_temp, job_owner=owner, include_extended=True)

    while ((entries is None or len(entries) == 0) and duration <= timeout):
        current_time = timer()
        duration = round(current_time - start_time)
        sleep(1)
        entries = jobs.fetch_multiple(job_id=job_id_temp, job_owner=owner, include_extended=True)

    if entries:
        for entry in entries:
            if owner != "*":
                if owner != entry.owner:
                    continue
            if job_name != "*":
                if not fnmatch.fnmatch(entry.name, job_name):
                    continue
            if job_id_temp is not None:
                if not fnmatch.fnmatch(entry.job_id, job_id):
                    continue

            job = {}
            job["job_id"] = entry.job_id
            job["job_name"] = entry.name
            job["subsystem"] = None
            job["system"] = None
            job["owner"] = entry.owner
            job["cpu_time"] = None
            job["execution_node"] = None
            job["origin_node"] = None
            # Sometimes, with job type STC, the first entry will have an extra
            # space at the end of it.
            job["content_type"] = entry.job_type.strip()

            # From v1.3.0, ZOAU sets unavailable job fields as None, instead of '?'.
            job["ret_code"] = {}
            job["ret_code"]["msg"] = entry.status
            job["ret_code"]["msg_code"] = entry.return_code
            job["ret_code"]["code"] = None
            if entry.return_code and len(entry.return_code) > 0:
                if entry.return_code.isdigit():
                    job["ret_code"]["code"] = int(entry.return_code)
            job["ret_code"]["msg_txt"] = entry.status

            # Beginning in ZOAU v1.3.0, the Job class changes svc_class to service_class.
            job["svc_class"] = entry.service_class
            job["job_class"] = entry.job_class
            job["priority"] = entry.priority
            job["asid"] = entry.asid
            job["creation_date"] = str(entry.creation_datetime)[0:10]
            job["creation_time"] = str(entry.creation_datetime)[12:]
            job["queue_position"] = entry.queue_position
            job["program_name"] = entry.program_name
            job["class"] = None
            job["steps"] = []
            job["dds"] = []
            job["duration"] = duration
            if hasattr(entry, "execution_time"):
                job["execution_time"] = entry.execution_time
            if hasattr(entry, "system"):
                job["system"] = entry.system
            if hasattr(entry, "subsystem"):
                job["subsystem"] = entry.subsystem
            if hasattr(entry, "cpu_time"):
                job["cpu_time"] = entry.cpu_time
            if hasattr(entry, "execution_node"):
                job["execution_node"] = entry.execution_node
            if hasattr(entry, "origin_node"):
                job["origin_node"] = entry.origin_node

            if dd_scan:
                # If true, it means the job is not ready for DD queries and the duration and
                # timeout should apply here instructing the user to add more time
                is_dd_query_exception = False
                is_jesjcl = False
                list_of_dds = []

                try:
                    list_of_dds = jobs.list_dds(entry.job_id, sysin=sysin)
                except exceptions.DDQueryException:
                    is_dd_query_exception = True

                # Check if the Job has JESJCL, if not, its in the JES INPUT queue, thus wait the full wait_time_s.
                # Idea here is to force a TYPRUN{HOLD|JCLHOLD|COPY} job to go the full wait duration since we have
                # currently no way to detect them, but if we know the job is one of the JOB_ERROR_STATUS lets
                # exit the wait time supplied as we know it is a job failure.
                is_jesjcl = True if search_dictionaries("dd_name", "JESJCL", list_of_dds) else False
                is_job_error_status = True if entry.status in JOB_ERROR_STATUSES else False

                while ((list_of_dds is None or len(list_of_dds) == 0 or is_dd_query_exception) and
                        (not is_jesjcl and not is_job_error_status and duration <= timeout)):
                    current_time = timer()
                    duration = round(current_time - start_time)
                    sleep(1)
                    try:
                        # Note, in the event of an exception, eg job has TYPRUN=HOLD
                        # list_of_dds will still be populated with valuable content
                        list_of_dds = jobs.list_dds(entry.job_id, sysin=sysin)
                        is_jesjcl = True if search_dictionaries("dd_name", "JESJCL", list_of_dds) else False
                        is_job_error_status = True if entry.status in JOB_ERROR_STATUSES else False
                    except exceptions.DDQueryException:
                        is_dd_query_exception = True
                        continue

                job["duration"] = duration
                for single_dd in list_of_dds:

                    dd = {}

                    if "dd_name" not in single_dd:
                        continue

                    # If dd_name not None, only that specific dd_name should be returned
                    if dd_name is not None:
                        if dd_name not in single_dd["dd_name"]:
                            continue
                        else:
                            dd["dd_name"] = single_dd["dd_name"]

                    if "records" in single_dd:
                        dd["record_count"] = single_dd["records"]
                    else:
                        dd["record_count"] = None

                    if "dsid" in single_dd:
                        dd["id"] = single_dd["dsid"]
                    else:
                        dd["id"] = "?"

                    if "step_name" in single_dd:
                        dd["stepname"] = single_dd["step_name"]
                    else:
                        dd["stepname"] = None

                    if "procstep" in single_dd:
                        dd["procstep"] = single_dd["procstep"]
                    else:
                        dd["procstep"] = None

                    if "bytes" in single_dd:
                        dd["byte_count"] = single_dd["bytes"]
                    else:
                        dd["byte_count"] = 0

                    tmpcont = None
                    if "step_name" in single_dd:
                        if "dd_name" in single_dd:
                            # In case ZOAU fails when reading the job output, we'll add a
                            # message to the user telling them of this. ZOAU cannot read
                            # partial output from a job, so we have to make do with nothing
                            # from this step if it fails.
                            try:
                                tmpcont = jobs.read_output(
                                    entry.job_id,
                                    single_dd["step_name"],
                                    single_dd["dd_name"]
                                )
                            except (UnicodeDecodeError, JSONDecodeError, TypeError, KeyError) as e:
                                error_type = e.__class__.__name__
                                tmpcont = (
                                    f"Non-printable UTF-8 characters were present in this output, a {error_type} error has occurred."
                                    "Please access the content from the job log."
                                )

                    dd["content"] = tmpcont.split("\n")

                    job["steps"].extend(_parse_steps(tmpcont))

                    job["dds"].append(dd)
                    if job["class"] is None:
                        job["class"] = entry.job_class

                    if job["system"] is None:
                        if "--  S Y S T E M  " in tmpcont:
                            tmptext = tmpcont.split("--  S Y S T E M  ")[1]
                            job["system"] = (tmptext.split(
                                "--", 1)[0]).replace(" ", "")

                    if job["subsystem"] is None:
                        if "--  N O D E " in tmpcont:
                            tmptext = tmpcont.split("--  N O D E ")[1]
                            job["subsystem"] = (tmptext.split("\n")[
                                                0]).replace(" ", "")

            final_entries.append(job)
    if not final_entries:
        final_entries = _job_not_found(job_id, owner, job_name, "unavailable")
    return final_entries


def _ddname_pattern(contents, resolve_dependencies):
    """Resolver for ddname_pattern type arguments.

    Parameters
    ----------
    contents : bool
        The contents of the argument.
    resolved_dependencies : dict
        Contains all of the dependencies and their contents,
        which have already been handled,
        for use during current arguments handling operations.

    Returns
    -------
    str
        The arguments contents after any necessary operations.

    Raises
    ------
    ValueError
        When contents is invalid argument type.
    """
    if not re.fullmatch(
        r"^(?:[A-Z]{1}[A-Z0-9]{0,7})|(?:\?{1})$",
        str(contents),
        re.IGNORECASE,
    ):
        raise ValueError(
            'Invalid argument type for "{0}". Expected "dd_name_pattern"'.format(
                contents
            )
        )
    return str(contents)


def search_dictionaries(key, value, list_of_dictionaries):
    """ Searches a list of dictionaries given key and returns
        the value dictionary.

        Arguments:
            key {str} -- dictionary key to search for.
            value {str} -- value to match for the dictionary key
            list {str} -- list of dictionaries

        Returns:
            dictionary -- dictionary matching the key and value

        Raises:
            TypeError -- When input is not a list of dictionaries
    """
    if not isinstance(list_of_dictionaries, list):
        raise TypeError(
            "Unsupported type for 'list_of_dictionaries', must be a list of dictionaries")

    return [element for element in list_of_dictionaries if element[key] == value]
