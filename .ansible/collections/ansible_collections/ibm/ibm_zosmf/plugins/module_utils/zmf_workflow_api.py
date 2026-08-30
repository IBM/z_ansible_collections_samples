# Copyright (c) IBM Corporation 2021
# Apache License, Version 2.0 (see https://opensource.org/licenses/Apache-2.0)

from __future__ import (absolute_import, division, print_function)
__metaclass__ = type

from ansible_collections.ibm.ibm_zosmf.plugins.module_utils.zmf_util import \
    handle_request
import json
import re


def __get_workflow_api_version():
    """
    Return the version of z/OSMF workflow services API.
    :rtype: str
    """
    return '1.0'


def __get_workflow_apis():
    """
    Return the details of all z/OSMF workflow services APIs.
    :rtype: dict[str, dict]
    """
    version = __get_workflow_api_version()
    return dict(
        # list the z/OSMF workflow instances for a system or sysplex
        list=dict(
            method='get',
            url='https://{zmf_host}:{zmf_port}/zosmf/workflow/rest/' \
                + version + '/workflows',
            args=dict(
                workflowName=dict(
                    required=False, type='str', nickname='workflow_name'
                ),
                category=dict(
                    required=False, type='str', nickname='workflow_category',
                    choices=['general', 'configuration']
                ),
                system=dict(
                    required=False, type='str', nickname='workflow_host'
                ),
                statusName=dict(required=False, type='str', nickname=''),
                owner=dict(
                    required=False, type='str', nickname='workflow_owner'
                ),
                vendor=dict(
                    required=False, type='str', nickname='workflow_vendor'
                )
            ),
            ok_rcode=200
        ),
        # retrieve the contents of a z/OSMF workflow definition from a z/OS
        # system
        retrieveDefinition=dict(
            method='get',
            url='https://{zmf_host}:{zmf_port}/zosmf/workflow/rest/' \
                + version + '/workflowDefinition',
            args=dict(
                definitionFilePath=dict(
                    required=True, type='str', nickname='workflow_file'
                ),
                workflowDefinitionFileSystem=dict(
                    required=False, type='str',
                    nickname='workflow_file_system'
                ),
                returnData=dict(
                    required=False, type='str', default='variables',
                    nickname=''
                )
            ),
            ok_rcode=200
        ),
        # retrieve the properties of a z/OSMF workflow instance
        retrieveProperties=dict(
            method='get',
            url='https://{zmf_host}:{zmf_port}/zosmf/workflow/rest/' \
                + version + '/workflows/{workflowKey}',
            args=dict(
                returnData=dict(
                    required=False, type='str',
                    default='steps,variables', nickname=''
                )
            ),
            ok_rcode=200
        ),
        # create a z/OSMF workflow instance on a z/OS system
        create=dict(
            method='post',
            url='https://{zmf_host}:{zmf_port}/zosmf/workflow/rest/' \
                + version + '/workflows',
            args=dict(
                workflowName=dict(
                    required=True, type='str', nickname='workflow_name'
                ),
                workflowDefinitionFile=dict(
                    required=True, type='str', nickname='workflow_file'
                ),
                workflowDefinitionFileSystem=dict(
                    required=False, type='str',
                    nickname='workflow_file_system'
                ),
                variableInputFile=dict(
                    required=False, type='str',
                    nickname='workflow_vars_file'
                ),
                variables=dict(
                    required=False, type='dict', nickname='workflow_vars'
                ),
                resolveGlobalConflictByUsing=dict(
                    required=False, type='str', default='global',
                    nickname='workflow_resolve_global_conflict_by_using',
                    choices=['global', 'input']
                ),
                system=dict(
                    required=True, type='str', nickname='workflow_host'
                ),
                owner=dict(
                    required=True, type='str', nickname='workflow_owner'
                ),
                comments=dict(
                    required=False, type='str', nickname='workflow_comments'
                ),
                assignToOwner=dict(
                    required=False, type='bool', default=True,
                    nickname='workflow_assign_to_owner'
                ),
                accessType=dict(
                    required=False, type='str', default='Public',
                    nickname='workflow_access_type',
                    choices=['Public', 'Restricted', 'Private']
                ),
                accountInfo=dict(
                    required=False, type='str',
                    nickname='workflow_account_info'
                ),
                jobStatement=dict(
                    required=False, type='str',
                    nickname='workflow_job_statement'
                ),
                deleteCompletedJobs=dict(
                    required=False, type='bool', default=False,
                    nickname='workflow_delete_completed_jobs'
                )
            ),
            ok_rcode=201
        ),
        # start a z/OSMF workflow instance on a z/OS system
        start=dict(
            method='put',
            url='https://{zmf_host}:{zmf_port}/zosmf/workflow/rest/' \
                + version + '/workflows/{workflowKey}/operations/start',
            args=dict(
                resolveConflictByUsing=dict(
                    required=False, type='str', default='outputFileValue',
                    nickname='workflow_resolve_conflict_by_using',
                    choices=[
                        'outputFileValue',
                        'existingValue',
                        'leaveConflict'
                    ]
                ),
                stepName=dict(
                    required=False, type='str',
                    nickname='workflow_step_name'
                ),
                performSubsequent=dict(
                    required=False, type='bool', default=True,
                    nickname='workflow_perform_subsequent'
                ),
                notificationUrl=dict(
                    required=False, type='str',
                    nickname='workflow_notification_url'
                )
            ),
            ok_rcode=202
        ),
        # remove a z/OSMF workflow instance from a z/OS system
        delete=dict(
            method='delete',
            url='https://{zmf_host}:{zmf_port}/zosmf/workflow/rest/' \
                + version + '/workflows/{workflowKey}',
            args={},
            ok_rcode=204
        )
    )


def __get_workflow_api_argument_spec(api):
    """
    Return the details of the specific workflow API.
    :param str api: the name of API
    :rtype: dict[str, str/int/dict]
    """
    workflow_apis = __get_workflow_apis()
    if api in workflow_apis:
        return workflow_apis[api]


def __get_workflow_api_url(module, url, workflow_key):
    """
    Return the parsed URL of the specific workflow API.
    :param AnsibleModule module: the ansible module
    :param str url: the initial URL of API
    :param str workflow_key: the key of workflow instance
    :rtype: str
    """
    # format the input for zmd_port
    if (module.params['zmf_port'] is None
            or str(module.params['zmf_port']).strip() == ''
            or str(module.params['zmf_port']).strip() == '-1'):
        module.params['zmf_port'] = ''
    else:
        module.params['zmf_port'] = str(module.params['zmf_port']).strip()
    matchObj = re.findall('{(.+?)}', url)
    for x in matchObj:
        if x == 'workflowKey':
            if workflow_key is None or workflow_key.strip() == '':
                module.fail_json(msg='Missing required argument or invalid'
                                 + ' argument: workflow_key.')
            else:
                url = re.sub('{' + x + '}', workflow_key.strip(), url)
        elif x == 'zmf_port' and module.params[x] == '':
            url = re.sub(':{' + x + '}', module.params[x], url)
        else:
            url = re.sub('{' + x + '}', module.params[x].strip(), url)
    return url


def __get_workflow_api_params(module, args):
    """
    Return the parsed params of the specific workflow API.
    :param AnsibleModule module: the ansible module
    :param dict[str, dict] args: the initial params of API
    :rtype: dict[str, str/list]
    """
    params = {}
    for k, v in args.items():
        if k == 'returnData':
            params[k] = v['default']
        elif k == 'owner':
            if (module.params[v['nickname']] is not None
                    and module.params[v['nickname']].strip() != ''):
                params[k] = module.params[v['nickname']].strip()
            elif (module.params['zmf_user'] is not None
                    and module.params['zmf_user'].strip() != ''):
                params[k] = module.params['zmf_user'].strip()
            elif v['required'] is True:
                module.fail_json(msg='Missing required argument or invalid'
                                 + ' argument: ' + v['nickname'] + '.')
        elif (v['nickname'] != ''
                and module.params[v['nickname']] is not None
                and str(module.params[v['nickname']]).strip() != ''):
            # format the input for params with choices
            if 'choices' in v:
                found = False
                for vv in v['choices']:
                    if (module.params[v['nickname']].strip().lower()
                            == vv.lower()):
                        found = True
                        params[k] = vv
                        break
                if found is False:
                    module.fail_json(
                        msg='Missing required argument or invalid argument: '
                            + v['nickname']
                            + '. The following values are valid: '
                            + str(v['choices']) + '.'
                    )
            elif v['type'] == 'str':
                params[k] = module.params[v['nickname']].strip()
            else:
                params[k] = module.params[v['nickname']]
        elif v['nickname'] != '' and v['required'] is True:
            module.fail_json(msg='Missing required argument or invalid'
                             + ' argument: ' + v['nickname'] + '.')
    if 'variables' in params:
        params['variables'] = __parse_dict_vars(module, params['variables'])
        if params['variables'] == []:
            params.pop('variables')
    return params


def __parse_dict_vars(module, dict_vars):
    """
    Transfer the given dict to list.
    :param AnsibleModule module: the ansible module
    :param dict[str, str/list/dict] dict_vars: the given dict
    :rtype: list[dict[str, str]]
    """
    list_vars = []
    for k, v in dict_vars.items():
        if isinstance(v, dict):
            module.fail_json(
                msg='Invalid argument: workflow_vars. Only '
                + 'string type or array type is accepted for each variable.')
        elif isinstance(v, list):
            v = json.dumps(v)
        list_vars.append({'name': k, 'value': v})
    return list_vars


def call_workflow_api(module, session, api, workflow_key):
    """
    Return the response or error message of the specific workflow API.
    :param AnsibleModule module: the ansible module
    :param Request session: the current connection session
    :param str api: the name of API
    :param str workflow_key: the key of workflow instance
    :rtype: dict or str
    """
    zmf_api = __get_workflow_api_argument_spec(api)
    zmf_api_url = __get_workflow_api_url(module, zmf_api['url'], workflow_key)
    zmf_api_params = __get_workflow_api_params(module, zmf_api['args'])
    if ((module.params['state'] == 'existed'
            or module.params['state'] == 'deleted') and api == 'list'):
        v = ''
        if 'workflowName' in zmf_api_params:
            v = zmf_api_params['workflowName']
        zmf_api_params.clear()
        zmf_api_params['workflowName'] = v
    return handle_request(module, session, zmf_api['method'],
                          zmf_api_url, zmf_api_params, zmf_api['ok_rcode'])


def get_request_argument_spec():
    """
    Return the mapping between arguments of ansible module and params of all
    workflow APIs.
    Return the arguments of ansible module used for workflow APIs.
    :rtype: (dict[str, dict], dict[str, dict])
    """
    mapping = {}
    argument_spec = {}
    workflow_apis = __get_workflow_apis()
    for k, v in workflow_apis.items():
        for kk, vv in v['args'].items():
            if vv['nickname'] != '':
                argument_spec[vv['nickname']] = dict(
                    required=False, type=vv['type']
                )
                if 'choices' in vv:
                    argument_spec[vv['nickname']].update(choices=vv['choices'])
                if 'default' in vv:
                    argument_spec[vv['nickname']].update(default=vv['default'])
                    mapping[vv['nickname']] = dict(
                        name=kk, default=vv['default']
                    )
                else:
                    mapping[vv['nickname']] = dict(name=kk)
    return mapping, argument_spec
