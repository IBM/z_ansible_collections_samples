# Copyright (c) IBM Corporation 2021
# Apache License, Version 2.0 (see https://opensource.org/licenses/Apache-2.0)

from __future__ import (absolute_import, division, print_function)
__metaclass__ = type

import json
from ansible.module_utils.urls import Request
import ansible.module_utils.six.moves.http_cookiejar as cookiejar


def get_auth_argument_spec():
    """
    Return the arguments of ansible module used for authentication with z/OSMF
    server.
    :rtype: dict[str, dict]
    """
    return dict(
        zmf_host=dict(required=True, type='str'),
        zmf_port=dict(required=False, type='int'),
        zmf_user=dict(required=False, type='str', no_log=True),
        zmf_password=dict(required=False, type='str', no_log=True),
        zmf_crt=dict(required=False, type='str', no_log=True),
        zmf_key=dict(required=False, type='str', no_log=True)
    )


def get_connect_argument_spec():
    """
    Return the arguments of ansible module used for session setup.
    :rtype: dict[str, dict]
    """
    return dict(
        zmf_host=dict(required=False, type='str'),
        zmf_port=dict(required=False, type='int'),
        zmf_user=dict(required=False, type='str', no_log=True),
        zmf_password=dict(required=False, type='str', no_log=True),
        zmf_crt=dict(required=False, type='str', no_log=True),
        zmf_key=dict(required=False, type='str', no_log=True),
        zmf_credential=dict(required=False, type='dict', no_log=True)
    )


def get_connect_session(module):
    """
    Return the connection Request.
    :param AnsibleModule module: the ansible module
    :rtype: Request
    """
    session = Request()
    crt = module.params['zmf_crt']
    key = module.params['zmf_key']
    user = module.params['zmf_user']
    pw = module.params['zmf_password']
    auth = None
    if 'zmf_credential' in module.params:
        auth = module.params['zmf_credential']
    if auth is not None and ('ltpa_token_2' in auth or 'jwt_token' in auth):
        # use ltpa_token_2 or jwt_token to authenticate
        if 'ltpa_token_2' in auth:
            cookie = cookiejar.Cookie(0, 'LtpaToken2', auth['ltpa_token_2'], None, False, auth['zmf_host'],
                                      True, True, '/', True, False, None, None, None, None, None)
        else:
            cookie = cookiejar.Cookie(0, 'jwtToken', auth['jwt_token'], None, False, auth['zmf_host'],
                                      True, True, '/', True, False, None, None, None, None, None)
        cookies = cookiejar.CookieJar()
        cookies.set_cookie(cookie)
        session.cookies = cookies
        module.params['zmf_host'] = auth['zmf_host']
        module.params['zmf_port'] = auth['zmf_port']
        return session
    elif ((crt is not None and crt.strip() != '')
            and (key is not None and key.strip() != '')):
        # use client cert and key to authenticate
        session.client_cert = crt.strip()
        session.client_key = key.strip()
        return session
    elif ((user is not None and user.strip() != '')
            and (pw is not None and pw.strip() != '')):
        # use username and password to authenticate
        session.url_username = user.strip()
        session.url_password = pw.strip()
        session.force_basic_auth = True
        return session
    else:
        # fail the module since auth is must for zosmf connection
        module.fail_json(msg='HTTP setup error: either zmf_user/zmf_password'
                         + ' or zmf_crt/zmf_key are required.')


def __get_request_headers():
    """
    Return the request headers for calling z/OSMF APIs.
    :rtype: dict[str, str]
    """
    return {'X-CSRF-ZOSMF-HEADER': 'ZOSMF'}


def handle_request(module, session, method, url, params=None, rcode=200,
                   header=None, timeout=30, body=None):
    """
    Return the response or error message of HTTP request.
    :param AnsibleModule module: the ansible module
    :param Request session: the current connection session
    :param str method: the method of HTTP request
    :param str url: the URL of HTTP request
    :param dict params: the params of HTTP request
    :param int rcode: the expected return code of HTTP request
    :param dict header: the header of HTTP request
    :param int timeout: the timeout of HTTP request
    :param str body: the body of HTTP request
    :rtype: dict or str
    """
    headers = __get_request_headers()
    if header is not None:
        headers.update(header)
    try:
        if method == 'get':
            # convert params dict to string and append it to the URL
            try:
                import urllib.parse as urllib
            except Exception:
                import urllib

            encoded_url = url + '?' + "&".join(["=".join([key, urllib.quote(str(val))]) for key, val in params.items()])
            # response = session.get(url + '?' + "&".join(["=".join([key, str(val)]) for key, val in params.items()]),
            response = session.get(encoded_url,
                                   headers=headers, validate_certs=False, timeout=timeout)
        elif method == 'put':
            if body is not None:
                response = session.put(url, data=body, headers=headers,
                                       validate_certs=False, timeout=timeout)
            else:
                response = session.put(url, data=json.dumps(params),
                                       headers=headers, validate_certs=False,
                                       timeout=timeout)
        elif method == 'post':
            if body is not None:
                response = session.post(url, data=body, headers=headers,
                                        validate_certs=False, timeout=timeout)
            else:
                response = session.post(url, data=json.dumps(params),
                                        headers=headers, validate_certs=False,
                                        timeout=timeout)
        elif method == 'delete':
            response = session.delete(url, headers=headers, validate_certs=False,
                                      timeout=timeout)
    except Exception as ex:
        if ('status' in dir(ex) and ex.status is not None) or ('code' in dir(ex) and ex.code is not None):
            ex_status = None
            if 'status' in dir(ex) and ex.status is not None:
                ex_status = ex.status
            elif 'code' in dir(ex) and ex.code is not None:
                ex_status = ex.code
            # In v2r3, response content is a string which will cause error in json.loads.
            try:
                content = ex.read()
                if content:
                    response_content = json.loads(content)
                    if 'messageText' in response_content:
                        return 'HTTP request error: ' + str(ex_status) + ' : ' \
                            + response_content['messageText']
                    elif 'errorMsg' in response_content:
                        return 'HTTP request error: ' + str(ex_status) + ' : ' \
                            + response_content['errorMsg']
                    elif 'return-code' in response_content:
                        return 'HTTP request error: ' + str(ex_status) \
                            + ' : return-code=' \
                            + str(response_content['return-code']) \
                            + ' reason-code=' + str(response_content['reason-code']) \
                            + ' reason=' + response_content['reason']
                    elif 'returnCode' in response_content:
                        return 'HTTP request error: ' + str(ex_status) \
                            + ' : return-code=' \
                            + str(response_content['returnCode']) \
                            + ' reason-code=' + str(response_content['reasonCode']) \
                            + ' reason=' + response_content['message']
                    else:
                        return content.decode()
                else:
                    return 'HTTP request error: ' + str(ex_status) + ' : ' \
                        + ex.reason
            except Exception:
                return 'HTTP request error, status code: ' + str(ex_status)
        else:
            module.fail_json(msg='HTTP request error, ex: ' + repr(ex))
    else:
        # in python2, addinfourl instance has no attribute 'status'
        if 'status' in dir(response):
            response_code = response.status
        else:
            response_code = response.code
        content = response.read()

        if content:
            response_content = json.loads(content)
        else:
            response_content = {}

        if response_code == rcode:
            if '/zosmf/services/authenticate' in url:
                # in python2, addinfourl instance has no attribute 'headers'
                if 'status' in dir(response):
                    return dict(response.headers)
                else:
                    # in python2, set-cookie in the header dict, so we need to transform it.
                    if response.info().dict['set-cookie'] is not None:
                        return {'Set-Cookie': response.info().dict['set-cookie']}
                    return response.info().dict
            else:
                return response_content
        else:
            return 'HTTP request error, response_code: ' + str(response_code)


def handle_request_raw(module, session, method, url, params=None, header=None,
                       body=None, timeout=30):
    headers = __get_request_headers()
    if header is not None:
        headers.update(header)
    try:
        if method == 'get':
            response = session.get(url + '?' + "&".join(["=".join([key, str(val)]) for key, val in params.items()]),
                                   headers=headers, validate_certs=False, timeout=timeout)
        elif method == 'put':
            if body is not None:
                response = session.put(url, data=body, headers=headers,
                                       validate_certs=False, timeout=timeout)
            else:
                response = session.put(url, data=json.dumps(params),
                                       headers=headers, validate_certs=False,
                                       timeout=timeout)
        elif method == 'post':
            if body is not None:
                response = session.post(url, data=body, headers=headers,
                                        validate_certs=False, timeout=timeout)
            else:
                response = session.post(url, data=json.dumps(params),
                                        headers=headers, validate_certs=False,
                                        timeout=timeout)
        elif method == 'delete':
            response = session.delete(url, headers=headers, validate_certs=False,
                                      timeout=timeout)
    except Exception as ex:
        module.fail_json(msg='HTTP request error: ' + repr(ex))
    else:
        return response.read()


def cmp_list(list1, list2):
    """
    Recursively compare the given lists.
    :param list list1: the given list
    :param list list2: the given list
    :returns: True if the given lists are same.
    :rtype: bool
    """
    if len(list1) != len(list2):
        return False
    else:
        for v in list1:
            found = False
            for vv in list2:
                if ((isinstance(v, str) or isinstance(v, bool))
                        and (isinstance(vv, str) or isinstance(vv, bool))):
                    if str(v).strip().upper() == str(vv).strip().upper():
                        found = True
                        break
                elif isinstance(v, list) and isinstance(vv, list):
                    if cmp_list(v, vv) is True:
                        found = True
                        break
                elif isinstance(v, dict) and isinstance(vv, dict):
                    if cmp_dict(v, vv) is True:
                        found = True
                        break
            if found is False:
                return False
    return True


def cmp_dict(dict1, dict2):
    """
    Recursively compare the given dicts.
    :param dict dict1: the given dict
    :param dict dict2: the given dict
    :returns: True if the given dicts are same.
    :rtype: bool
    """
    if len(dict1) != len(dict2):
        return False
    else:
        key1 = list(dict1.keys()).sort()
        key2 = list(dict2.keys()).sort()
        if key1 != key2:
            return False
        else:
            for k, v in dict1.items():
                if isinstance(v, str) or isinstance(v, bool):
                    if str(v).strip().upper() != str(dict2[k]).strip().upper():
                        return False
                elif isinstance(v, list):
                    if not isinstance(dict2[k], list):
                        return False
                    else:
                        if cmp_list(v, dict2[k]) is False:
                            return False
                elif isinstance(v, dict):
                    if not isinstance(dict2[k], dict):
                        return False
                    else:
                        if cmp_dict(v, dict2[k]) is False:
                            return False
    return True
