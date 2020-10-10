# Copyright (c) IBM Corporation 2019, 2020
# Apache License, Version 2.0 (see https://opensource.org/licenses/Apache-2.0)

from __future__ import absolute_import, division, print_function

__metaclass__ = type

from tempfile import NamedTemporaryFile
from os import chmod, path, remove
from stat import S_IEXEC, S_IREAD, S_IWRITE
import json
import re
from ansible_collections.ibm.ibm_zos_core.plugins.module_utils.better_arg_parser import (
    BetterArgParser,
)
from ansible_collections.ibm.ibm_zos_core.plugins.module_utils.ansible_module import (
    AnsibleModuleHelper,
)


def job_output(job_id=None, owner=None, job_name=None, dd_name=None):
    """Get the output from a z/OS job based on various search criteria.

    Keyword Arguments:
        job_id {str} -- The job ID to search for (default: {None})
        owner {str} -- The owner of the job (default: {None})
        job_name {str} -- The job name search for (default: {None})
        dd_name {str} -- The data definition to retrieve (default: {None})

    Raises:
        RuntimeError: When job output cannot be retrieved successfully but job exists.
        RuntimeError: When no job output is found

    Returns:
        list[dict] -- The output information for a list of jobs matching specified criteria.
    """
    arg_defs = dict(
        job_id=dict(arg_type="qualifier_pattern"),
        owner=dict(arg_type="qualifier_pattern"),
        job_name=dict(arg_type="qualifier_pattern"),
        dd_name=dict(arg_type=_ddname_pattern),
    )

    parser = BetterArgParser(arg_defs)
    parsed_args = parser.parse_args(
        {"job_id": job_id, "owner": owner, "job_name": job_name, "dd_name": dd_name}
    )

    job_id = parsed_args.get("job_id") or "*"
    job_name = parsed_args.get("job_name") or "*"
    owner = parsed_args.get("owner") or "*"
    dd_name = parsed_args.get("ddname") or ""

    job_detail_json = _get_job_output(job_id, owner, job_name, dd_name)
    if len(job_detail_json) == 0:
        # some systems have issues with "*" while some require it to see results
        job_id = "" if job_id == "*" else job_id
        owner = "" if owner == "*" else owner
        job_name = "" if job_name == "*" else job_name
        job_detail_json = _get_job_output(job_id, owner, job_name, dd_name)

    for job in job_detail_json:
        job["ret_code"] = {} if job.get("ret_code") is None else job.get("ret_code")
        job["ret_code"]["code"] = _get_return_code_num(
            job.get("ret_code").get("msg", "")
        )
        job["ret_code"]["msg_code"] = _get_return_code_str(
            job.get("ret_code").get("msg", "")
        )
        job["ret_code"]["msg_txt"] = ""
        if job.get("ret_code").get("msg", "") == "":
            job["ret_code"]["msg"] = "AC"
    return job_detail_json


def _get_job_output(job_id="*", owner="*", job_name="*", dd_name=""):
    job_detail_json = {}
    rc, out, err = _get_job_output_str(job_id, owner, job_name, dd_name)
    if rc != 0:
        raise RuntimeError(
            "Failed to retrieve job output. RC: {0} Error: {1}".format(
                str(rc), str(err)
            )
        )
    if not out:
        raise RuntimeError("Failed to retrieve job output. No job output found.")
    job_detail_json = json.loads(out, strict=False)
    return job_detail_json


def _get_job_output_str(job_id="*", owner="*", job_name="*", dd_name=""):
    """Generate JSON output string containing Job info from SDSF.
    Writes a temporary REXX script to the USS filesystem to gather output.

    Keyword Arguments:
        job_id {str} -- The job ID to search for (default: {''})
        owner {str} -- The owner of the job (default: {''})
        job_name {str} -- The job name search for (default: {''})
        dd_name {str} -- The data definition to retrieve (default: {''})

    Returns:
        tuple[int, str, str] -- RC, STDOUT, and STDERR from the REXX script.
    """
    get_job_detail_json_rexx = """/* REXX */
arg options
parse var options param
upper param
parse var param 'JOBID=' jobid ' OWNER=' owner,
' JOBNAME=' jobname ' DDNAME=' ddname

rc=isfcalls('ON')

jobid = strip(jobid,'L')
if (jobid <> '') then do
ISFFILTER='JobID EQ '||jobid
end
owner = strip(owner,'L')
if (owner <> '') then do
ISFOWNER=owner
end
jobname = strip(jobname,'L')
if (jobname <> '') then do
ISFPREFIX=jobname
end
ddname = strip(ddname,'L')
if (ddname == '?') then do
ddname = ''
end

Address SDSF "ISFEXEC ST (ALTERNATE DELAYED)"
if rc<>0 then do
Say '[]'
Exit 0
end
if isfrows == 0 then do
Say '[]'
end
else do
Say '['
do ix=1 to isfrows
    linecount = 0
    if ix<>1 then do
    Say ','
    end
    Say '{'
    Say '"'||'job_id'||'":"'||value('JOBID'||"."||ix)||'",'
    Say '"'||'job_name'||'":"'||value('JNAME'||"."||ix)||'",'
    Say '"'||'subsystem'||'":"'||value('ESYSID'||"."||ix)||'",'
    Say '"'||'owner'||'":"'||value('OWNERID'||"."||ix)||'",'
    Say '"'||'ret_code'||'":{"'||'msg'||'":"'||value('RETCODE'||"."||ix)||'"},'
    Say '"'||'class'||'":"'||value('JCLASS'||"."||ix)||'",'
    Say '"'||'content_type'||'":"'||value('JTYPE'||"."||ix)||'",'
    Address SDSF "ISFACT ST TOKEN('"TOKEN.ix"') PARM(NP ?)",
"("prefix JDS_
    lrc=rc
    if lrc<>0 | JDS_DDNAME.0 == 0 then do
    Say '"ddnames":[]'
    end
    else do
    Say '"ddnames":['
    do jx=1 to JDS_DDNAME.0
        if jx<>1 & ddname == '' then do
        Say ','
        end
        if ddname == '' | ddname == value('JDS_DDNAME'||"."||jx) then do
        Say '{'
        Say '"'||'ddname'||'":"'||value('JDS_DDNAME'||"."||jx)||'",'
        Say '"'||'record_count'||'":"'||value('JDS_RECCNT'||"."||jx)||'",'
        Say '"'||'id'||'":"'||value('JDS_DSID'||"."||jx)||'",'
        Say '"'||'stepname'||'":"'||value('JDS_STEPN'||"."||jx)||'",'
        Say '"'||'procstep'||'":"'||value('JDS_PROCS'||"."||jx)||'",'
        Say '"'||'byte_count'||'":"'||value('JDS_BYTECNT'||"."||jx)||'",'
        Say '"'||'content'||'":['
        Address SDSF "ISFBROWSE ST TOKEN('"token.ix"')"
        untilline = linecount + JDS_RECCNT.jx
        startingcount = linecount + 1
        do kx=linecount+1 to  untilline
            if kx<>startingcount then do
            Say ','
            end
            linecount = linecount + 1
            Say '"'||escapeNewLine(escapeDoubleQuote(isfline.kx))||'"'
        end
        Say ']'
        Say '}'
        end
        else do
            linecount = linecount + JDS_RECCNT.jx
        end
    end
    Say ']'
    end
    Say '}'
end
Say ']'
end

rc=isfcalls('OFF')

return 0

escapeDoubleQuote: Procedure
Parse Arg string
out=''
Do While Pos('"',string)<>0
Parse Var string prefix '"' string
out=out||prefix||'\\"'
End
Return out||string

escapeNewLine: Procedure
Parse Arg string
Return translate(string, '4040'x, '1525'x)
"""
    try:
        module = AnsibleModuleHelper(argument_spec={})
        if dd_name is None or dd_name == "?":
            dd_name = ""
        jobid_param = "jobid=" + job_id
        owner_param = "owner=" + owner
        jobname_param = "jobname=" + job_name
        ddname_param = "ddname=" + dd_name

        tmp = NamedTemporaryFile(delete=True)
        with open(tmp.name, "w") as f:
            f.write(get_job_detail_json_rexx)
        chmod(tmp.name, S_IEXEC | S_IREAD | S_IWRITE)
        args = [jobid_param, owner_param, jobname_param, ddname_param]

        cmd = [tmp.name, " ".join(args)]
        rc, out, err = module.run_command(args=cmd)
    except Exception:
        raise
    return rc, out, err


def job_status(job_id=None, owner=None, job_name=None):
    """Get the status information of a z/OS job based on various search criteria.

    Keyword Arguments:
        job_id {str} -- The job ID to search for (default: {None})
        owner {str} -- The owner of the job (default: {None})
        job_name {str} -- The job name search for (default: {None})

    Raises:
        RuntimeError: When job status cannot be retrieved successfully but job exists.
        RuntimeError: When no job status is found.

    Returns:
        list[dict] -- The status information for a list of jobs matching search criteria.
    """
    arg_defs = dict(
        job_id=dict(arg_type="qualifier_pattern"),
        owner=dict(arg_type="qualifier_pattern"),
        job_name=dict(arg_type="qualifier_pattern"),
    )

    parser = BetterArgParser(arg_defs)
    parsed_args = parser.parse_args(
        {"job_id": job_id, "owner": owner, "job_name": job_name}
    )

    job_id = parsed_args.get("job_id") or "*"
    job_name = parsed_args.get("job_name") or "*"
    owner = parsed_args.get("owner") or "*"

    job_status_json = _get_job_status(job_id, owner, job_name)
    if len(job_status_json) == 0:
        job_id = "" if job_id == "*" else job_id
        job_name = "" if job_name == "*" else job_name
        owner = "" if owner == "*" else owner
        job_status_json = _get_job_status(job_id, owner, job_name)

    for job in job_status_json:
        job["ret_code"] = {} if job.get("ret_code") is None else job.get("ret_code")
        job["ret_code"]["code"] = _get_return_code_num(
            job.get("ret_code").get("msg", "")
        )
        job["ret_code"]["msg_code"] = _get_return_code_str(
            job.get("ret_code").get("msg", "")
        )
        job["ret_code"]["msg_txt"] = ""
        if job.get("ret_code").get("msg", "") == "":
            job["ret_code"]["msg"] = "AC"
    return job_status_json


def _get_job_status(job_id="*", owner="*", job_name="*"):
    job_status_json = {}
    rc, out, err = _get_job_status_str(job_id, owner, job_name)
    if rc != 0:
        raise RuntimeError(
            "Failed to retrieve job status. RC: {0} Error: {1}".format(
                str(rc), str(err)
            )
        )
    if not out:
        raise RuntimeError("Failed to retrieve job status. No job status found.")
    job_status_json = json.loads(out, strict=False)
    return job_status_json


def _get_job_status_str(job_id="*", owner="*", job_name="*"):
    """Generate JSON output string containing Job status info from SDSF.
    Writes a temporary REXX script to the USS filesystem to gather output.

    Keyword Arguments:
        job_id {str} -- The job ID to search for (default: {''})
        owner {str} -- The owner of the job (default: {''})
        job_name {str} -- The job name search for (default: {''})

    Returns:
        tuple[int, str, str] -- RC, STDOUT, and STDERR from the REXX script.
    """
    get_job_status_json_rexx = """/* REXX */
arg options
parse var options param
upper param
parse var param 'JOBID=' jobid ' OWNER=' owner,
' JOBNAME=' jobname

rc=isfcalls('ON')

jobid = strip(jobid,'L')
if (jobid <> '') then do
ISFFILTER='JobID EQ '||jobid
end
owner = strip(owner,'L')
if (owner <> '') then do
ISFOWNER=owner
end
jobname = strip(jobname,'L')
if (jobname <> '') then do
ISFPREFIX=jobname
end

Address SDSF "ISFEXEC ST (ALTERNATE DELAYED)"
if rc<>0 then do
Say '[]'
Exit 0
end
if isfrows == 0 then do
Say '[]'
end
else do
Say '['
do ix=1 to isfrows
    linecount = 0
    if ix<>1 then do
    Say ','
    end
    Say '{'
    Say '"'||'job_id'||'":"'||value('JOBID'||"."||ix)||'",'
    Say '"'||'job_name'||'":"'||value('JNAME'||"."||ix)||'",'
    Say '"'||'subsystem'||'":"'||value('ESYSID'||"."||ix)||'",'
    Say '"'||'system'||'":"'||value('SYSNAME'||"."||ix)||'",'
    Say '"'||'owner'||'":"'||value('OWNERID'||"."||ix)||'",'
    Say '"'||'ret_code'||'":{"'||'msg'||'":"'||value('RETCODE'||"."||ix)||'"},'
    Say '"'||'class'||'":"'||value('JCLASS'||"."||ix)||'",'
    Say '"'||'content_type'||'":"'||value('JTYPE'||"."||ix)||'"'
    Say '}'
end
Say ']'
end

rc=isfcalls('OFF')

return 0

escapeDoubleQuote: Procedure
Parse Arg string
out=''
Do While Pos('"',string)<>0
Parse Var string prefix '"' string
out=out||prefix||'\\"'
End
Return out||string

escapeNewLine: Procedure
Parse Arg string
Return translate(string, '4040'x, '1525'x)
"""
    try:
        module = AnsibleModuleHelper(argument_spec={})
        jobid_param = "jobid=" + job_id
        owner_param = "owner=" + owner
        jobname_param = "jobname=" + job_name

        tmp = NamedTemporaryFile(delete=True)
        with open(tmp.name, "w") as f:
            f.write(get_job_status_json_rexx)
        chmod(tmp.name, S_IEXEC | S_IREAD | S_IWRITE)
        args = [jobid_param, owner_param, jobname_param]

        cmd = [tmp.name, " ".join(args)]
        rc, out, err = module.run_command(args=cmd)
    except Exception:
        raise
    return rc, out, err


def _get_return_code_num(rc_str):
    """Parse an integer return code from
    z/OS job output return code string.

    Arguments:
        rc_str {str} -- The return code message from z/OS job log (eg. "CC 0000")

    Returns:
        Union[int, NoneType] -- Returns integer RC if possible, if not returns NoneType
    """
    rc = None
    match = re.search(r"\s*CC\s*([0-9]+)", rc_str)
    if match:
        rc = int(match.group(1))
    return rc


def _get_return_code_str(rc_str):
    """Parse an integer return code from
    z/OS job output return code string.

    Arguments:
        rc_str {str} -- The return code message from z/OS job log (eg. "CC 0000" or "ABEND")

    Returns:
        Union[str, NoneType] -- Returns string RC or ABEND code if possible, if not returns NoneType
    """
    rc = None
    match = re.search(r"(?:\s*CC\s*([0-9]+))|(?:ABEND\s*((?:S|U)[0-9]+))", rc_str)
    if match:
        rc = match.group(1) or match.group(2)
    return rc


def _ddname_pattern(contents, resolve_dependencies):
    """Resolver for ddname_pattern type arguments

    Arguments:
        contents {bool} -- The contents of the argument.
        resolved_dependencies {dict} -- Contains all of the dependencies and their contents,
        which have already been handled,
        for use during current arguments handling operations.

    Raises:
        ValueError: When contents is invalid argument type
    Returns:
        str -- The arguments contents after any necessary operations.
    """
    if not re.fullmatch(
        r"^(?:[A-Z]{1}[A-Z0-9]{0,7})|(?:\?{1})$", str(contents), re.IGNORECASE,
    ):
        raise ValueError(
            'Invalid argument type for "{0}". expected "ddname_pattern"'.format(
                contents
            )
        )
    return str(contents)
