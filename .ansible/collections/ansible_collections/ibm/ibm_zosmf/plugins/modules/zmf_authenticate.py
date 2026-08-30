#!/usr/bin/python
# -*- coding: utf-8 -*-

# Copyright (c) IBM Corporation 2021
# Apache License, Version 2.0 (see https://opensource.org/licenses/Apache-2.0)

from __future__ import (absolute_import, division, print_function)
__metaclass__ = type

DOCUMENTATION = r"""
---
module: zmf_authenticate
short_description: Authenticate with z/OSMF server
description:
    - >
      Authenticate with z/OSMF server by either username/password or HTTPS
      client authenticate.
    - Return the authentication credentials for successful authentication.
    - >
      The credential can be then used for succeeding Ansible tasks which call
      z/OSMF Ansible module or role.
version_added: "1.0.0"
author:
    - Yang Cao (@zosmf-Young)
    - Yun Juan Yang (@zosmf-Robyn)
options:
    zmf_host:
        description:
            - Hostname of the z/OSMF server.
        required: True
        type: str
    zmf_port:
        description:
            - Port number of the z/OSMF server.
        required: False
        type: int
        default: null
    zmf_user:
        description:
            - User name to be used for authenticating with z/OSMF server.
            - Required when I(zmf_crt) and I(zmf_key) are not supplied.
            - >
              If I(zmf_crt) and I(zmf_key) are supplied, I(zmf_user) and
              I(zmf_password) are ignored.
        required: False
        type: str
        default: null
    zmf_password:
        description:
            - Password to be used for authenticating with z/OSMF server.
            - Required when I(zmf_crt) and I(zmf_key) are not supplied.
            - >
              If I(zmf_crt) and I(zmf_key) are supplied, I(zmf_user) and
              I(zmf_password) are ignored.
        required: False
        type: str
        default: null
    zmf_crt:
        description:
            - >
              Location of the PEM-formatted certificate chain file to be used
              for HTTPS client authentication.
            - Required when I(zmf_user) and I(zmf_password) are not supplied.
        required: False
        type: str
        default: null
    zmf_key:
        description:
            - >
              Location of the PEM-formatted file with your private key to be
              used for HTTPS client authentication.
            - Required when I(zmf_user) and I(zmf_password) are not supplied.
        required: False
        type: str
        default: null

"""

EXAMPLES = r"""
- name: Authenticate with z/OSMF server by username/password
  zmf_authenticate:
    zmf_host: "sample.ibm.com"
    zmf_user: "your_username"
    zmf_password: "your_password"

- name: Authenticate with z/OSMF server by HTTPS client authenticate
  zmf_authenticate:
    zmf_host: "sample.ibm.com"
    zmf_crt: "/file_with_your_certificate_chain.crt"
    zmf_key: "/file_with_your_private_key.key"

- name: Authenticate with z/OSMF server by prompting to input username/password
  vars_prompt:
    - name: zmf_user
      prompt: "Enter your zOSMF username"
      private: no
    - name: zmf_password
      prompt: "Enter your zOSMF password"
      private: yes
  tasks:
    - zmf_authenticate:
        zmf_host: "{{ zmf_host }}"
        zmf_port: "{{ zmf_port }}"
        zmf_user: "{{ zmf_user }}"
        zmf_password: "{{ zmf_password }}"
"""

RETURN = r"""
changed:
    description: Indicates if any change is made during the module operation.
    returned: always
    type: bool
ltpa_token_2:
    description:
        - >
          The value of Lightweight Third Party Access (LTPA) token, which
          supports strong encryption.
    returned: on success
    type: str
    sample: "yDS7uJxqrd3h8v5WXq9pf1yPtztQ4JzroZN3XQKF26ZicXgHc7mdzgycMCa......"
jwt_token:
    description: The value of JSON Web token, which supports strong encryption.
    returned: on success
    type: str
    sample: "eyJ0eXAiOiJKV1QiLCJhbGciOiJSUzI1NiJ9.eyJ0b2tlmVhcmVyIiwicie......"
zmf_host:
    description: Hostname of the z/OSMF server.
    returned: on success
    type: str
zmf_port:
    description: Port number of the z/OSMF server.
    returned: on success
    type: int
"""

from ansible.module_utils.basic import AnsibleModule
from ansible_collections.ibm.ibm_zosmf.plugins.module_utils.zmf_util import (
    get_auth_argument_spec,
    get_connect_session
)
from ansible_collections.ibm.ibm_zosmf.plugins.module_utils.zmf_auth_api \
    import call_auth_api
import re


def authenticate(module):
    """
    Authenticate with z/OSMF server.
    Return the authentication token.
    :param AnsibleModule module: the ansible module
    """
    # create session
    session = get_connect_session(module)
    # get authentication token
    response_getAuth = call_auth_api(module, session, 'getAuth')
    if isinstance(response_getAuth, dict):
        if ('Set-Cookie' in response_getAuth
                and ('LtpaToken2' in response_getAuth['Set-Cookie']
                     or 'jwtToken' in response_getAuth['Set-Cookie'])):
            auth = {}
            if 'LtpaToken2' in response_getAuth['Set-Cookie']:
                auth['ltpa_token_2'] = \
                    re.findall('LtpaToken2=(.+?);',
                               response_getAuth['Set-Cookie'])[0]
            if 'jwtToken' in response_getAuth['Set-Cookie']:
                auth['jwt_token'] = \
                    re.findall('jwtToken=(.+?);',
                               response_getAuth['Set-Cookie'])[0]
            auth['zmf_host'] = module.params['zmf_host']
            auth['zmf_port'] = module.params['zmf_port']
            module.exit_json(**auth)
        else:
            module.fail_json(
                msg='Failed to authenticate with z/OSMF server '
                + '---- Cannot obtain the authentication token.')
    else:
        module.fail_json(msg='Failed to authenticate with z/OSMF server ---- '
                         + response_getAuth)


def main():
    argument_spec = get_auth_argument_spec()
    module = AnsibleModule(
        argument_spec=argument_spec,
        supports_check_mode=False
    )
    authenticate(module)


if __name__ == '__main__':
    main()
