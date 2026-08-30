# Copyright (c) IBM Corporation 2021
# Apache License, Version 2.0 (see https://opensource.org/licenses/Apache-2.0)

from __future__ import (absolute_import, division, print_function)
__metaclass__ = type

from ansible_collections.ibm.ibm_zosmf.plugins.module_utils.zmf_util import \
    handle_request
import re


def __get_auth_apis():
    """
    Return the details of all z/OSMF authentication APIs.
    :rtype: dict[str, dict]
    """
    return dict(
        # get the authentication tokens on user login for z/OSMF server
        getAuth=dict(
            method='post',
            header={'Content-Type': 'application/x-www-form-urlencoded'},
            url='https://{zmf_host}:{zmf_port}/zosmf/services/authenticate',
            args={},
            ok_rcode=200
        )
    )


def __get_auth_api_argument_spec(api):
    """
    Return the details of the specific authentication API.
    :param str api: the name of API
    :rtype: dict[str, str/int/dict]
    """
    auth_apis = __get_auth_apis()
    if api in auth_apis:
        return auth_apis[api]


def __get_auth_api_url(module, url):
    """
    Return the parsed URL of the specific authentication API.
    :param AnsibleModule module: the ansible module
    :param str url: the initial URL of API
    :rtype: str
    """
    # format the input for zmd_port
    if module.params['zmf_port'] is None:
        module.params['zmf_port'] = ''
    else:
        module.params['zmf_port'] = str(module.params['zmf_port']).strip()
    matchObj = re.findall('{(.+?)}', url)
    for x in matchObj:
        if x == 'zmf_port' and module.params[x] == '':
            url = re.sub(':{' + x + '}', module.params[x], url)
        else:
            url = re.sub('{' + x + '}', module.params[x].strip(), url)
    return url


def call_auth_api(module, session, api):
    """
    Return the response or error message of the specific authentication API.
    :param AnsibleModule module: the ansible module
    :param Request session: the current connection session
    :param str api: the name of API
    :rtype: dict or str
    """
    zmf_api = __get_auth_api_argument_spec(api)
    zmf_api_url = __get_auth_api_url(module, zmf_api['url'])
    return handle_request(module, session, zmf_api['method'], zmf_api_url,
                          zmf_api['args'], zmf_api['ok_rcode'],
                          zmf_api['header'])
