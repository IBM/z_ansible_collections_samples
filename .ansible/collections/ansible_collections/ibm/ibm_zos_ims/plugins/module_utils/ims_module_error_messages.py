from __future__ import (absolute_import, division, print_function)

__metaclass__ = type


class ErrorMessages():
    BATCH_FAILURE_MSG = "Failed. Check 'msg' field in 'ims_output' for more details."
    INVALID_CHAR_IN_CMD = "Invalid character(s) found in command."
    INVALID_MEMBER_LIST_TYPE = "Received unexpected type in member_list. Member_list is expected to be a list of strings specifying the source members \
        and/or single-entry dicts where the key specifies the source member as a str and the value specifies the target name as a str."
    INVALID_MEMBER_NAME = "Received invalid name for a data set member: "
    INVALID_PLEX_MSG = "Malformed Plex."
    INVALID_ROUTE_MSG = "One or more routes specified are malformed."
    JSON_DECODE_ERROR_MSG = "Unable to decode string into JSON."
    MISSING_COMMAND = "Missing required arguments: command"
    MISSING_PLEX = "Missing required arguments: plex"
    NO_OUTPUT_MSG = "No job output was found."
    NO_RC_MSG = "Error verifying return code."
    NON_ZERO_RC_MSG = "Non-zero return code returned."
    NON_ZERO_ERR_MSG = "Refer to IMS return codes."
    REXX_RETURN_CODE_MSG = "The following REXX error code was returned: "
    SUBMISSION_ERROR_MSG = "Error submitting IMS Command."
    SUCCESS_MSG = "Success"


class DBRCErrorMessages():
    INCORRECT_CMD_TYPE = "'commands' must be a string or list of strings."
    INCORRECT_STEPLIB_TYPE = "'steplib' must be a string or list of strings."
    INCORRECT_DBDLIB_TYPE = "'dbd_lib' must be a string."
    INCORRECT_DYNALLOC_TYPE = "'dynamic_allocation_dataset' must be a string."
    INCORRECT_GENJCL_TYPE = "'genjcl_input_dataset' must be a string."
    INCORRECT_JCLOUT_TYPE = "'genjcl_output_dataset' must be a string."
    INCORRECT_RECON_TYPE = "'recon' parameters must be strings."
    INVALID_MAX_RC = "Invalid input for 'max_rc'."
    DYNALLOC_RECON_REQUIREMENT_MSG = "'dynamic_allocation_dataset' or ('recon1', 'recon2', 'recon3') must be specified."
    SUCCESS_MSG = "Success"
    FAILURE_MSG = "Failure. See output for details."
    EMPTY_OUTPUT_MSG = "DBRC command(s) could not be processed. Please check input parameters."
    MISSING_COMMAND = "missing required arguments: command"
    MISSING_STEPLIB = "missing required arguments: steplib"


class ACBGENErrorMessages():
    INCORRECT_COMMAND_INPUT_TYPE = "'command_input' must be a string."
    INCORRECT_COMPRESSION_TYPE = "'compression' must be a string - PRECOMP,POSTCOMP, in any combination. The default is none."
    INCORRECT_PSB_NAME_TYPE = "'psb_name' must be string(ALL) or list of strings."
    INCORRECT_DBD_NAME_TYPE = "'dbd_name' must be a list of strings."
    INCORRECT_ACBLIB_TYPE = "'acb_lib' must be a string."
    INCORRECT_PSBLIB_TYPE = "'psb_lib' must be a list of strings."
    INCORRECT_DBDLIB_TYPE = "'dbd_lib' must be a list of strings."
    INCORRECT_RESLIB_TYPE = "'reslib' must be a list of strings."
    INCORRECT_STEPLIB_TYPE = "'steplib' must be a list of strings."
    INCORRECT_BUILD_PSB_TYPE = "'build_psb' must be boolean value - yes, no or true, false"
    COMMAND_INPUT_REQUIREMENT_MSG = "'command_input' must be specified"
    ACBLIB_REQUIREMENT_MSG = "'acb_lib' must be specified "
    PSBLIB_REQUIREMENT_MSG = "'psb_lib' must be specified"
    DBDLIB_REQUIREMENT_MSG = "'dbd_lib' must be specified"
    SUCCESS_MSG = "ACBGEN execution is successful."
    FAILURE_MSG = "Failure. See output for details."
    EMPTY_OUTPUT_MSG = "ACBGEN could not be processed. Please check input parameters."
    INCORRECT_TYPE = "Incorrect type provided. A string or list of strings is expected"
    INCORRECT_ITEM_LIST = "Items provided in list do not match the string type expected."
    INCORRECT_COMMAND_ARGS = "Must provide at least one of 'psb_name', 'dbd_name'"
