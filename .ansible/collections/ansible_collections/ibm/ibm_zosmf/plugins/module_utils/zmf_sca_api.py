# Copyright (c) IBM Corporation 2021
# Apache License, Version 2.0 (see https://opensource.org/licenses/Apache-2.0)

from __future__ import (absolute_import, division, print_function)
__metaclass__ = type

from ansible_collections.ibm.ibm_zosmf.plugins.module_utils.zmf_util import \
    handle_request
import json
import re


def __get_api_version():
    """
    Return the version of z/OSMF workflow services API.
    :rtype: str
    """
    return 'v1'


def __get_sca_apis():
    """
    Return the details of all z/OSMF workflow services APIs.
    :rtype: dict[str, dict]
    """
    version = __get_api_version()
    resource_dict = dict(
        serviceId=dict(
            required=False, type='str', nickname=''
        ),
        serviceName=dict(
            required=False, type='str', nickname=''
        ),
        version=dict(
            required=False, type='str', nickname=''
        ),
        vendor=dict(
            required=False, type='str', nickname=''
        ),
        resourceItems=dict(
            required=True, type='list', nickname='', elements='dict',
            options=dict(
                itemId=dict(
                    required=False, type='str', nickname=''
                ),
                itemType=dict(
                    required=False, type='str', nickname=''
                ),
                itemCategory=dict(
                    required=False, type='str', nickname=''
                ),
                itemDescription=dict(
                    required=False, type='str', nickname=''
                ),
                resourceProfile=dict(
                    required=True, type='str', nickname='resourceProfile'
                ),
                resourceClass=dict(
                    required=True, type='str', nickname='resourceClass'
                ),
                access=dict(
                    required=True, type='str', nickname='access'
                ),
                whoNeedsAccess=dict(
                    required=False, type='str', nickname=''
                )
            )
        )
    )
    return dict(
        # validate descriptor
        validateDescriptor=dict(
            method='post',
            url='https://{zmf_host}:{zmf_port}/zosmf/config/security/' \
                + version + '/validate/descriptor?userid={userid}',
            args=dict(
                path=dict(
                    required=True, type='str', nickname='path_of_security_requirements'
                )
            ),
            ok_rcode=200
        ),
        # validate resource
        validateResource=dict(
            method='post',
            url='https://{zmf_host}:{zmf_port}/zosmf/config/security/' \
                + version + '/validate?userid={userid}',
            args=resource_dict,
            ok_rcode=200
        ),
        # provision descriptor
        provisionDescriptor=dict(
            method='post',
            url='https://{zmf_host}:{zmf_port}/zosmf/config/security/' \
                + version + '/provision/descriptor?userid={userid}',
            args=dict(
                path=dict(
                    required=True, type='str', nickname='path_of_security_requirements'
                )
            ),
            ok_rcode=200
        ),
        # provision resource
        provisionResource=dict(
            method='post',
            url='https://{zmf_host}:{zmf_port}/zosmf/config/security/' \
                + version + '/provision?userid={userid}',
            args=resource_dict,
            ok_rcode=200
        )
    )


def __get_sca_api_argument_spec(api):
    """
    Return the details of the specific sca API.
    :param str api: the name of API
    :rtype: dict[str, str/int/dict]
    """
    sca_apis = __get_sca_apis()
    if api in sca_apis:
        return sca_apis[api]


def __get_sca_api_url(module, url, userid):
    """
    Return the parsed URL of the specific sca API.
    :param AnsibleModule module: the ansible module
    :param str url: the initial URL of API
    :param str userid: user ID or group ID that the security resources will be validated for
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
        if x == 'userid':
            if userid is not None and userid.strip() != '':
                url = re.sub('{' + x + '}', userid.strip(), url)
        elif x == 'zmf_port' and module.params[x] == '':
            url = re.sub(':{' + x + '}', module.params[x], url)
        else:
            url = re.sub('{' + x + '}', module.params[x].strip(), url)
    return url


def __get_sca_api_params(module, args):
    """
    Return the parsed params of the specific sca API.
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
                msg='Invalid argument: sca_vars. Only '
                + 'string type or array type is accepted for each variable.')
        elif isinstance(v, list):
            v = json.dumps(v)
        list_vars.append({'name': k, 'value': v})
    return list_vars


def call_sca_api(module, session, api, body=None):
    """
    Return the response or error message of the specific sca API.
    :param AnsibleModule module: the ansible module
    :param Request session: the current connection session
    :param str api: the name of API
    :param str body: the body of API
    :rtype: dict or str
    """
    zmf_api = __get_sca_api_argument_spec(api)
    zmf_api_url = __get_sca_api_url(module, zmf_api['url'], module.params['target_userid'])

    zmf_api_params = __get_sca_api_params(module, zmf_api['args'])

    return handle_request(module, session, zmf_api['method'], zmf_api_url, zmf_api_params, zmf_api['ok_rcode'],
                          {'Content-Type': 'application/json'}, 30, body)


def get_request_argument_spec():
    """
    Return the mapping between arguments of ansible module and params of all
    sca APIs.
    Return the arguments of ansible module used for sca APIs.
    :rtype: (dict[str, dict], dict[str, dict])
    """
    mapping = {}
    argument_spec = {}
    sca_apis = __get_sca_apis()
    for k, v in sca_apis.items():
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
