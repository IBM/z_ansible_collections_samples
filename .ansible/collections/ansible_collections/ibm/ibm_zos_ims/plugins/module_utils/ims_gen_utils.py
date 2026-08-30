from __future__ import (absolute_import, division, print_function)
__metaclass__ = type
import re
import tempfile
from pprint import pprint
from ansible_collections.ibm.ibm_zos_ims.plugins.module_utils.ims_module_error_messages import ErrorMessages as ims_em

# ZOAUTIL_TEMP_USS = "/tmp/test.jcl"
# ZOAUTIL_TEMP_USS2 = "/tmp/test2.jcl"


# def submit_uss_jcl(module):
#     (conv_rc, stdout, stderr) = module.run_command('iconv -f ISO8859-1 -t IBM-1047 %s > %s' % (ZOAUTIL_TEMP_USS, ZOAUTIL_TEMP_USS2), use_unsafe_shell=True)
#     if conv_rc == 0:
#         pprint('Submitting JCL in USS')
#         rc, stdout, stderr = module.run_command(['submit', '-j', ZOAUTIL_TEMP_USS2])
#         print(rc)
#         if rc != 0:
#             raise SubmitJCLError('SUBMIT JOB FAILED:  Stderr :' + stderr)
#         if 'Error' in stderr:
#             raise SubmitJCLError('SUBMIT JOB FAILED: ' + stderr)
#         if 'Not accepted by JES' in stderr:
#             raise SubmitJCLError('SUBMIT JOB FAILED: ' + stderr)
#         if stdout != '':
#             jobId = stdout.replace("\n", "").strip()
#         else:
#             raise SubmitJCLError('SUBMIT JOB FAILED: ' + 'NO JOB ID IS RETURNED. PLEASE CHECK THE JCL.')
#         pprint('this is the job id')
#         pprint(jobId)
#     else:
#         module.fail_json(msg='The Local file encoding conversion failed. Please check the source file.' + stderr, **result)
#     return jobId


class Error(Exception):
    pass


class SubmitJCLError(Error):
    def __init__(self, jobs):
        self.msg = 'An error occurred during submission of jobs "{0}"'.format(jobs)


def data_set_exists(name, run_command):
    """Checks for existence of data set."""
    rc, stdout, stderr = run_command('head "//\'{0}\'"'.format(name))
    if rc != 0 or (stderr and 'EDC5049I' in stderr):
        return False
    return True


def data_set_member_exists(name, run_command):
    """Checks for existence of data set member."""
    rc, stdout, stderr = run_command('head "//\'{0}\'"'.format(name))
    if rc != 0 or (stderr and 'EDC5067I' in stderr):
        return False
    return True


def file_exists(name, run_command):
    """Checks for existence of USS file on the host machine."""
    rc, stdout, stderr = run_command('file {0}'.format(name))

    if stdout and ('FSUM6484' in stdout):
        return False, stdout
        # file is missing
    return True, ''
# def file_exists(name, run_command):
#     """Checks for existence of USS file on the host machine."""
#     _, _, stderr = run_command('test -f {}'.format(name))
#     if stderr and ('FSUM3425') in stderr:
#         # file is missing
#         return False, stderr
#     return True, ''


def validate_member_list(member_list):
    """This function validates member list.
    Arguments:
        member_list{list} -- member list param.
    Returns:
        {bool} -- flag marking valid parameters
        {str} -- return text containing error message
    """
    for member in member_list:
        return_text = ''
        # process member as a str
        if isinstance(member, str):
            if not is_valid_member_str(member):
                return False, ims_em.INVALID_MEMBER_NAME + str(member)

        # process member as a dict
        elif isinstance(member, dict):
            # expect single-entry dict
            if len(member) != 1:
                return False, ims_em.INVALID_MEMBER_LIST_TYPE

            src_member, target_name = [(k, v) for k, v in member.items()][0]

            # src_member must be a str and a valid member name
            if not isinstance(src_member, str):
                return False, ims_em.INVALID_MEMBER_LIST_TYPE
            else:
                if not is_valid_member_str(src_member):
                    return False, ims_em.INVALID_MEMBER_NAME + str(src_member)

            # target_name must be a str and a valid member name
            if not isinstance(target_name, str):
                return False, ims_em.INVALID_MEMBER_LIST_TYPE
            else:
                if not is_valid_member_str(target_name):
                    return False, ims_em.INVALID_MEMBER_NAME + str(target_name)
        # process member as non-str, non-dict
        else:
            return_text = ims_em.INVALID_MEMBER_LIST_TYPE

            return False, return_text
    return True, ''


def is_valid_member_str(member_name):
    """This function checks if member is a valid name
        Arguments:
        member_name {str} -- str to be checked and validated
        Returns:
        {bool} -- flag for valid member name
    """

    # re.fullmatch returns an re.Match object if there is a match or None
    return re.fullmatch(r"^[A-Z$#@]{1}[A-Z0-9$#@]{0,7}$", str(member_name), re.IGNORECASE) is not None


def run_gen_file(filename, dest, syslib_list, overwrite, run_command):
    """This function runs PSBGEN or DBDGEN when specifically using USS file as input source. It runs
    validation on input parameters and then runs assemble and link commands in sequence.
    It also verifies if destination data set has correct member name,
    same as input filename as the result of PSBGEN or DBDGEN processing.
    Arguments:
        filename {str} -- The name of USS file as input data set which contains PSB or DBD source.
        dest {str} -- The name of destination PSBLIB or DBDLIB data set.
        syslib_list {list str} -- A list of required macro libraries, needed to compile the PSB or DBD source.
        overwrite {bool} -- If set to true, it will link and overwrite the existing destination member.

    Returns:
        {int} -- Return code - indicating the general status of function execution, if zero, its a success.
        {str} -- Return text.
    """
    rc = 0
    out = "Invoking run_gen_file method. "
    stderr = ''
    tmpFile = tempfile.gettempdir() + '/tempGenObj.o'
    # create member name same as filename
    member = filename.split('/')[-1]
    member = member.split(".")[0]
    member = member.upper()

    if not overwrite:
        # check if destination already has member
        rc = data_set_member_exists(dest + '(' + member + ')', run_command)
        if rc:
            return 3, '', 'Destination data set member already exists: ' + member

    ldcommand = 'ld -o '
    ldcommand = 'ld -o "//\'' + dest + '(' + member + ')' + '\'" ' + tmpFile

    # store sys_libs
    syslibCommand = ''
    for syslib in syslib_list:
        syslibCommand = syslibCommand + ' -I ' + syslib
    out += "Attempting to run assemble command. "

    # assemble and create output object
    ascommand = 'as -mOBJECT' + syslibCommand + ' -o' + tmpFile + ' ' + filename
    rc, out, stderr = run_command(ascommand)
    if rc != 0:
        return rc, out, stderr

    # link the generated output to source
    out += "Attempting to run link command. "
    rc, out, stderr = run_command(ldcommand)

    if rc != 0:
        return rc, out, stderr

    # remove temp object file
    run_command('rm ' + tmpFile)

    # check with member is present in the destination data set after assemble and compile
    rc = data_set_member_exists(dest + '(' + member + ')', run_command)
    if not rc:
        return 4, '', 'Error when processing, destination data does not have member: ' + member

    # return success
    out += 'run_gen_file succeeded for source: ' + filename
    return 0, out, ''


def run_gen_data_set(source, src_member, dest, dest_member, syslib_list, overwrite, run_command):
    """This function runs PSBGEN or DBDGEN when specifically using data set as input source. It runs
    validation on input parameters and then runs assemble and link commands in sequence.
    It also verifies if destination data set has correct member name;
    if the source is a regular data set, then member name is same as src_member;
    else, if the source is a sequential data set, then it is dest_member.
    Arguments:
        source {str} -- The name of input data set.
        src_member {str} -- The name of member in input data set which contains PSB or DBD source.
        dest {str} -- The name of destination PSBLIB or DBDLIB data set.
        dest_member {str} -- The target name of member within PSBLIB or DBDLIB data set to store output. Same as src_member if not explicitly specified.
        syslib_list {List str} -- A list of required macro libraries, needed to compile the PSB or DBD source.
        overwrite {bool} -- If set to true, it will link and overwrite the existing destination member.

    Returns:
        {int} -- Return code, indicating the general status of function execution, if zero, its a success.
        {str} -- Return text.
    """
    rc = 0
    out = "Invoking run_gen_data_set method. "
    stderr = ''
    tmpFile = tempfile.gettempdir() + '/tempGenObj.o'

    if dest_member == '' and src_member == '':
        return 1, '', 'Either source member or destination member should be set.'
    if not overwrite:
        # check if destination data set already has member
        rc = data_set_member_exists(dest + '(' + dest_member + ')', run_command)
        if rc:
            return 3, '', 'Destination data set member already exists: ' + dest_member + '.'

    # store sys_libs
    syslibCommand = ''
    for sys_lib in syslib_list:
        syslibCommand = syslibCommand + ' -I ' + sys_lib
    out += "Attempt to run assemble command. "

    # assemble and create output object
    if src_member == '':
        # sequential data set
        ascommand = 'as -mOBJECT' + syslibCommand + ' -o' + \
            tmpFile + ' "//\'' + source + '\'"'
    else:
        ascommand = 'as -mOBJECT' + syslibCommand + ' -o' + \
            tmpFile + ' "//\'' + source + '(' + src_member + ')' + '\'"'

    rc, out, stderr = run_command(ascommand)
    if rc != 0:
        return rc, out, stderr

    # input is seqential data set
    ldcommand = 'ld -o '
    ldcommand = 'ld -o "//\'' + dest + '(' + dest_member + ')' + '\'" ' + tmpFile

    # link the generated output to source
    out += "Attempt to run link command."
    rc, out, stderr = run_command(ldcommand)

    if rc != 0:
        return rc, out, stderr

    # remove temp object file
    run_command('rm ' + tmpFile)

    # check with member is present in the destination data set after assemble and compile
    rc = data_set_member_exists(dest + '(' + dest_member + ')', run_command)
    if not rc:
        return 4, '', 'Error when processing. Destination data set does not have member: ' + dest_member + '.'

    # return success
    out += 'run_gen_data_set succeeded for source: ' + source
    return 0, out, ''


def execute_gen_command(source, dest, syslib_list, run_command, module, result):
    location_type = {'DATA_SET', 'USS', None}
    DSN_REGEX = r'^(?:(?:[A-Z]{1}[A-Z0-9]{0,7})(?:[.]{1})){1,21}[A-Z]{1}[A-Z0-9]{0,7}(?:\([A-Z]{1}[A-Z0-9]{0,7}\)){0,1}$'
    # Loops through source list
    src = None
    if 'src' in source:
        src = source['src']
    if not src:
        # result['ims_output'].append({
        #     'return_code': 1,
        #     'std_error': 'Required source parameter is missing.'
        # })
        # module.fail_json(
        #     msg='Invalid input source list being passed without content.', **result)
        failed = True
        return_code = 1
        return_text = 'Invalid input source list being passed without content.'
        return src, return_code, return_text, failed

    module.log('Checking source : ' + src)

    location = None
    overwrite = True
    if 'location' in source:
        location = source['location']
    if 'replace' in source:
        overwrite = source['replace']

    if location in location_type:
        if location == 'DATA_SET' or location is None:
            data_set_name_pattern = re.compile(DSN_REGEX)
            check = data_set_name_pattern.search(src)
            if not check:
                # result['ims_output'].append({
                #     'return_code': 1,
                #     'src': src,
                #     'std_error': 'Invalid input data set for src: ' + src
                # })
                # module.fail_json(
                #     msg='The value specified in the source data set is not a valid data set name.', **result)
                failed = True
                return_code = 1
                return_text = 'Received invalid data set name for input source: ' + src
                return src, return_code, return_text, failed
            else:
                # Check if destination psblib/dbdlib exists
                # process data set if member_list exists and not empty
                if 'member_list' in source and source['member_list']:
                    module.log('Generating ' + dest + ' for source: ' + src + ' with members: ')
                    # loop through members
                    members_text = ''
                    # validate member_list
                    valid, return_text = validate_member_list(source['member_list'])
                    if not valid:
                        failed = True
                        return_code = 2
                        return src, return_code, return_text, failed

                    for item in source['member_list']:
                        if type(item) == str:
                            # set target name same as src
                            src_member = item
                            target_name = item
                        # elif type(item) == dict:
                        # else:
                        #     throw error
                        else:
                            src_member, target_name = [(k, v) for k, v in item.items()][0]

                        if src_member == '':
                            # result['ims_output'].append({
                            #     'return_code': 2,
                            #     'src': src,
                            #     'std_error': 'Data set member is empty',
                            #     'return_text': return_text
                            # })
                            # module.fail_json(
                            #     msg='Failed to validate data source.', **result)
                            failed = True
                            return_code = 2
                            return_text = 'Data source could not be validated. Data set member is empty.'
                            return src, return_code, return_text, failed
                        # Check if member exists
                        rc = data_set_member_exists(
                            src + '(' + src_member + ')', run_command)
                        if not rc:
                            # result['ims_output'].append({
                            #     'return_code': 2,
                            #     'src': src,
                            #     'std_error': 'Data set member: ('+member+'): does not exists.',
                            #     'return_text': return_text
                            # })
                            # module.fail_json(
                            #     msg='Failed to validate data source.', **result)
                            failed = True
                            return_code = 2
                            return_text = 'Data source could not be validated. Data set member does not exist: ' + src_member
                            return src, return_code, return_text, failed

                            # TODO Failed to validate DBD source - {0} instead of DBD to handle both

                        rc, out, stderr = run_gen_data_set(
                            src, src_member, dest, target_name, syslib_list, overwrite, run_command)

                        if rc != 0:
                            # result['ims_output'].append({
                            #     'return_code': rc,
                            #     'src': src,
                            #     'std_error': 'error while processing '+src+'('+member+'): '+stderr,
                            #     'return_text': return_text
                            # })
                            # module.fail_json(
                            #     msg='GEN Command was not successful for source: '+src, **result)
                            failed = True
                            return_code = rc
                            return_text = 'Error assembling or linking source: ' + src
                            result['stderr'] = stderr
                            return src, return_code, return_text, failed
                        module.log(msg=out)
                        # save member name for printing output
                        members_text = members_text + '(' + src_member + ') '
                        return_text = 'Generated ' + dest + ' for source: ' + \
                            src + ' with members: ' + members_text
                        module.log(return_text)
                else:

                    # process flat data set if member_list is empty for input source

                    # Validate member_name is set
                    member_name = None
                    member_name_str = ''
                    if 'dbd_name' in source:
                        member_name = source['dbd_name']
                        member_name_str = 'dbd_name'
                    elif 'psb_name' in source:
                        member_name = source['psb_name']
                        member_name_str = 'psb_name'
                    if not member_name:
                        # result['ims_output'].append({
                        #     'return_code': 1,
                        #     'src': src,
                        #     'std_error': 'Input Parameter member_name is not set for given sequential data source: '+src
                        # })
                        # module.fail_json(
                        #     msg='Input parameter member_name is not set.', **result)
                        failed = True
                        return_code = 1
                        return_text = 'Input parameter ' + member_name_str + ' is not set for given sequential data source: ' + src
                        return src, return_code, return_text, failed

                    # continue processing flat data set
                    module.log(str('Generating ' + dest + ' for source: ' + src + ' with member: ' + member_name))

                    if not data_set_exists(src, run_command):
                        # result['ims_output'].append({
                        #     'return_code': 1,
                        #     'src': src,
                        #     'std_error': 'error while processing source: does not exists. '+src,
                        #     'return_text': return_text
                        # })
                        # module.fail_json(
                        #     msg='Failed to validate data source.', **result) #TODO Failed to validate DBD source - {0} instead of DBD to handle both

                        failed = True
                        return_code = 1
                        return_text = 'Data source could not be validated. Data set source does not exist: ' + src
                        return src, return_code, return_text, failed

                    rc, out, stderr = run_gen_data_set(
                        src, '', dest, member_name, syslib_list, overwrite, run_command)

                    if rc != 0:
                        # result['ims_output'].append({
                        #     'return_code': rc,
                        #     'src': src,
                        #     'std_error': 'error while processing '+src+': ' +stderr,
                        #     'return_text': return_text
                        # })
                        # module.fail_json(
                        #     msg='GEN Command was not successful for source: '+src, **result)
                        failed = True
                        return_code = rc
                        return_text = 'Error assembling or linking source: ' + src
                        result['stderr'] = stderr
                        return src, return_code, return_text, failed

                    module.log(msg=out)
                    module.log(str('Generated ' + dest + ' for source: ' + src + ' with member: ' + member_name + '.'))

        elif location == 'USS':
            # process USS file
            module.log(str('Generating ' + dest + ' for source: ' + src))
            rc, stderr = file_exists(src, run_command)
            if not rc:
                # result['ims_output'].append({
                #     'return_code': 1,
                #     'src': src,
                #     'std_error': 'error while processing source: '+stderr,
                #     'return_text': return_text
                # })
                # module.fail_json(
                #     msg='Failed to validate input file', **result)

                failed = True
                return_code = 1
                return_text = 'Failed to validate input file: ' + src
                result['stderr'] = stderr
                return src, return_code, return_text, failed

            rc, out, stderr = run_gen_file(
                src, dest, syslib_list, overwrite, run_command)

            if rc != 0:
                # result['ims_output'].append({
                #     'return_code': rc,
                #     'src': src,
                #     'std_error': 'error while processing '+src+': '+stderr,
                #     'return_text': return_text
                # })
                # module.fail_json(
                #     msg='GEN Command was not successful for source: '+src, **result)
                failed = True
                return_code = rc
                return_text = 'Error assembling or linking source: ' + src

                result['stderr'] = stderr
                return src, return_code, return_text, failed

            module.log(msg=out)
            module.log('Generated ' + dest + ' for source: ' + src)
    else:
        # result['ims_output'].append({
        #     'return_code': 1,
        #     'src': src,
        #     'std_error': 'Invalid location for given source : '+location +'.',
        #     'return_text': "Invalid location for input source."
        # })
        # module.fail_json(
        #     msg='Location is not valid. DATA_SET and USS are supported.', **result)
        failed = True
        return_code = 1
        return_text = "Invalid location for input source. Valid options for location are: ['DATA_SET', 'USS']"
        return src, return_code, return_text, failed

    failed = False
    return_code = 0
    return_text = ''
    return src, return_code, return_text, failed
