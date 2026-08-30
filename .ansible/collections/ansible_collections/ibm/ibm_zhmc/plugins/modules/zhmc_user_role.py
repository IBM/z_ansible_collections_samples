#!/usr/bin/python
# Copyright 2022 IBM Corp. All Rights Reserved.
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#    http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.

from __future__ import (absolute_import, division, print_function)
__metaclass__ = type

# For information on the format of the ANSIBLE_METADATA, DOCUMENTATION,
# EXAMPLES, and RETURN strings, see
# http://docs.ansible.com/ansible/dev_guide/developing_modules_documenting.html

ANSIBLE_METADATA = {
    'metadata_version': '1.1',
    'status': ['stableinterface'],
    'supported_by': 'community',
    'shipped_by': 'other',
    'other_repo_url': 'https://github.com/zhmcclient/zhmc-ansible-modules'
}

DOCUMENTATION = """
---
module: zhmc_user_role
version_added: "2.9.0"
short_description: Create HMC user roles
description:
  - Gather facts about a user role on an HMC of a Z system.
  - Create, delete, or update a user role on an HMC.
author:
  - Andreas Maier (@andy-maier)
requirements:
  - Access to the WS API of the HMC of the targeted Z system
    (see :term:`HMC API`).
  - The targeted Z system can be in any operational mode (classic, DPM)
options:
  hmc_host:
    description:
      - The hostname or IP address of the HMC.
    type: str
    required: true
  hmc_auth:
    description:
      - The authentication credentials for the HMC.
    type: dict
    required: true
    suboptions:
      userid:
        description:
          - The userid (username) for authenticating with the HMC.
        type: str
        required: true
      password:
        description:
          - The password for authenticating with the HMC.
        type: str
        required: true
      ca_certs:
        description:
          - Path name of certificate file or certificate directory to be used
            for verifying the HMC certificate. If null (default), the path name
            in the 'REQUESTS_CA_BUNDLE' environment variable or the path name
            in the 'CURL_CA_BUNDLE' environment variable is used, or if neither
            of these variables is set, the certificates in the Mozilla CA
            Certificate List provided by the 'certifi' Python package are used
            for verifying the HMC certificate.
        type: str
        required: false
        default: null
      verify:
        description:
          - If True (default), verify the HMC certificate as specified in the
            C(ca_certs) parameter. If False, ignore what is specified in the
            C(ca_certs) parameter and do not verify the HMC certificate.
        type: bool
        required: false
        default: true
  name:
    description:
      - The name of the target user role.
    type: str
    required: true
  state:
    description:
      - "The desired state for the HMC user role. All states are fully
         idempotent within the limits of the properties that can be changed:"
      - "* C(absent): Ensures that the user role does not exist."
      - "* C(present): Ensures that the user role exists and has the
         specified properties."
      - "* C(facts): Returns the user role properties."
    type: str
    required: true
    choices: ['absent', 'present', 'facts']
  properties:
    description:
      - "Dictionary with desired properties for the user role.
         Used for C(state=present); ignored for C(state=absent|facts).
         Dictionary key is the property name with underscores instead
         of hyphens, and dictionary value is the property value in YAML syntax.
         Integer properties may also be provided as decimal strings."
      - "The possible input properties in this dictionary are the properties
         defined as writeable in the data model for user role resources
         (where the property names contain underscores instead of hyphens),
         with the following exceptions:"
      - "* C(name): Cannot be specified because the name has already been
         specified in the C(name) module parameter."
      - "* C(associated_system_defined_user_role_uri): Cannot be specified
         because this information is specified using the artificial property
         C(associated_system_defined_user_role_name)."
      - "* C(associated_system_defined_user_role_name): The name of the
         associated system-defined user role."
      - "* C(permissions): Can be specified as if it were writeable."
      - "Properties omitted in this dictionary will remain unchanged when the
         user role already exists, and will get the default value defined
         in the data model for user roles in the :term:`HMC API` when the
         user role is being created."
    type: dict
    required: false
    default: null
    suboptions:
      "{property}":
        description:
          - Any other property defined as writeable in the data model for
            user role resources (where the property names contain underscores
            instead of hyphens), except those excluded in the description above.
        required: false
      associated_system_defined_user_role_name:
        description:
          - "The name of the associated system-defined user role. Specifying it
             requires that the referenced user role exists."
          - "Optional, default: 'hmc-operator-tasks'."
        type: str
        required: false
      permissions:
        description:
          - "The permissions for this user role."
          - "This property is represented different from its description in the
            :term:`HMC API`:
            The property is a list of permissions. Each list item is a
            dictionary that specifies a single permission item, any required
            scoping items, and optional option items."
        type: list
        required: false
        default: []
        elements: dict
        suboptions:
          task:
            description:
              - "Permission item: Task permission to the task with the
                specified name."
            type: str
          view_only:
            description:
              - "Option item only for C(task): Indicates whether the task's
                view-only version is subject of the permission. Only certain
                tasks have a view-only version. Default: true."
            type: bool
          class:
            description:
              - "Permission item: Object permission to all objects of the
                specified resource class (= value of 'class' property)."
            type: str
          group:
            description:
              - "Permission item: Object permission to the group with the
                specified name and optionally to its members."
            type: str
          include_members:
            description:
              - "Option item only for C(group): Indicates whether the group
                members are included in the permission. Default: false."
            type: bool
          cpc:
            description:
              - "Permission item: Object permission to the CPC with the
                specified name."
              - "Scoping item: Specifies the CPC name as a scope for the names
                specified in other permission items."
            type: str
          partition:
            description:
              - "Permission item: Object permission to the partition with the
                specified name on the specified CPC (in DPM mode)."
              - "Requires C(cpc) to be specified as a scoping item."
            type: str
          lpar:
            description:
              - "Permission item: Object permission to the LPAR with the
                specified name on the specified CPC (in classic mode)."
              - "Requires C(cpc) to be specified as a scoping item."
            type: str
          adapter:
            description:
              - "Permission item: Object permission to the adapter with the
                specified name on the specified CPC (in DPM mode)."
              - "Requires C(cpc) to be specified as a scoping item."
            type: str
          storage_group:
            description:
              - "Permission item: Object permission to the storage group with
                the specified name that is associated with the specified CPC
                (in DPM mode)."
              - "Requires C(cpc) to be specified as a scoping item."
            type: str
          storage_group_template:
            description:
              - "Permission item: Object permission to the storage group
                template with the specified name that is associated with the
                specified CPC (in DPM mode)."
              - "Requires C(cpc) to be specified as a scoping item."
            type: str
  log_file:
    description:
      - "File path of a log file to which the logic flow of this module as well
         as interactions with the HMC are logged. If null, logging will be
         propagated to the Python root logger."
    type: str
    required: false
    default: null
  _faked_session:
    description:
      - "An internal parameter used for testing the module."
    required: false
    type: raw
    default: null
"""

EXAMPLES = """
---
# Note: The following examples assume that some variables named 'my_*' are set.

- name: Gather facts about a user role
  zhmc_user_role:
    hmc_host: "{{ my_hmc_host }}"
    hmc_auth: "{{ my_hmc_auth }}"
    name: "{{ my_user_role_name }}"
    state: facts
  register: rule1

- name: Ensure the user role does not exist
  zhmc_user_role:
    hmc_host: "{{ my_hmc_host }}"
    hmc_auth: "{{ my_hmc_auth }}"
    name: "{{ my_user_role_name }}"
    state: absent

- name: Ensure the user role exists and has certain properties
  zhmc_user_role:
    hmc_host: "{{ my_hmc_host }}"
    hmc_auth: "{{ my_hmc_auth }}"
    name: "{{ my_user_role_name }}"
    state: present
    properties:
      description: "Example user role 1"
      permissions:
        - task: "configure-storage-storageadmin"  # Task permission to "configure-storage-storageadmin"
        - task: "hardware-messages"  # Task permission to the view-only version of "hardware-messages"
          view_only: true
        - class: cpc        # Object permission to all CPCs
        - partition: part1  # Object permission to part1 in cpc1
          cpc: cpc1
        - partition: part2  # Object permission to part2 in cpc2
          cpc: cpc2
  register: rule1
"""

RETURN = """
changed:
  description: Indicates if any change has been made by the module.
    For C(state=facts), always will be false.
  returned: always
  type: bool
msg:
  description: An error message that describes the failure.
  returned: failure
  type: str
user_role:
  description:
    - "For C(state=absent), an empty dictionary."
    - "For C(state=present|facts), a
       dictionary with the resource properties of the target user role."
  returned: success
  type: dict
  contains:
    name:
      description: "User role name"
      type: str
    associated-system-defined-user-role-name:
      description: "The name of the associated system-defined user role"
      type: str
    permissions:
      description:
        - "The permissions for this user role."
        - "This property is represented different from its description in the
          :term:`HMC API`:
          The property is a list of permissions. Each list item is a
          dictionary that specifies a single permission item, any needed
          scoping items, and any applicable option items."
      type: list
      elements: dict
      contains:
        task:
          description:
            - "Permission item: Task permission to the task with the
              specified name."
          type: str
        view_only:
          description:
            - "Option item present for C(task): Indicates whether the task's
              view-only version is subject of the permission. Only certain
              tasks have a view-only version, but the option item will be
              present for all tasks."
          type: bool
        class:
          description:
            - "Permission item: Object permission to all objects of the
              specified resource class (= value of 'class' property)."
          type: str
        group:
          description:
            - "Permission item: Object permission to the group with the
              specified name and optionally to its members."
          type: str
        include_members:
          description:
            - "Option item present for C(group): Indicates whether the group
              members are included in the permission. The option item will be
              present for all groups."
          type: bool
        cpc:
          description:
            - "Permission item: Object permission to the CPC with the
              specified name."
            - "Scoping item: Specifies the CPC name as a scope for the names
              specified in other permission items."
          type: str
        partition:
          description:
            - "Permission item: Object permission to the partition with the
              specified name on the specified CPC (in DPM mode)."
            - "C(cpc) will be present as a scoping item."
          type: str
        lpar:
          description:
            - "Permission item: Object permission to the LPAR with the
              specified name on the specified CPC (in classic mode)."
            - "C(cpc) will be present as a scoping item."
          type: str
        adapter:
          description:
            - "Permission item: Object permission to the adapter with the
              specified name on the specified CPC (in DPM mode)."
            - "C(cpc) will be present as a scoping item."
          type: str
        storage_group:
          description:
            - "Permission item: Object permission to the storage group with
              the specified name that is associated with the specified CPC
              (in DPM mode)."
            - "C(cpc) will be present as a scoping item."
          type: str
        storage_group_template:
          description:
            - "Permission item: Object permission to the storage group
              template with the specified name that is associated with the
              specified CPC (in DPM mode)."
            - "C(cpc) will be present as a scoping item."
          type: str
    "{property}":
      description: "Additional properties of the user role, as described
        in the data model of the 'User Role' object in the :term:`HMC API`
         book. The property names have hyphens (-) as described in that book."
  sample:
    {
        "associated-system-defined-user-role-name": "hmc-operator-tasks",
        "associated-system-defined-user-role-uri": "/api/user-roles/e8c098cb-0597-4003-8e5b-e3a63476c2f8",
        "class": "user-role",
        "description": "zhmc test user role 1",
        "is-inheritance-enabled": false,
        "is-locked": false,
        "name": "zhmc_test_role_1",
        "object-id": "3dc87062-f651-11ec-8ea3-00106f25b43c",
        "object-uri": "/api/user-roles/3dc87062-f651-11ec-8ea3-00106f25b43c",
        "parent": "/api/console",
        "permissions": [
            {
                "task": "configure-storage-storageadmin"
            },
            {
                "task": "hardware-messages",
                "view_only": true
            },
            {
                "task": "se-cryptographic-management",
                "view_only": false
            },
            {
                "class": "cpc"
            },
            {
                "cpc": "P000A218",
                "partition": "Test"
            },
            {
                "adapter": "HiSoClassic",
                "cpc": "P000A218"
            }
        ],
        "replication-overwrite-possible": false,
        "type": "user-defined"
    }
"""

import uuid  # noqa: E402
import logging  # noqa: E402
import traceback  # noqa: E402
from ansible.module_utils.basic import AnsibleModule  # noqa: E402

from ..module_utils.common import log_init, Error, ParameterError, \
    get_hmc_auth, get_session, to_unicode, process_normal_property, \
    missing_required_lib, common_fail_on_import_errors  # noqa: E402

try:
    import requests.packages.urllib3
    IMP_URLLIB3_ERR = None
except ImportError:
    IMP_URLLIB3_ERR = traceback.format_exc()

try:
    import zhmcclient
    IMP_ZHMCCLIENT_ERR = None
except ImportError:
    IMP_ZHMCCLIENT_ERR = traceback.format_exc()

# Python logger name for this module
LOGGER_NAME = 'zhmc_user_role'

LOGGER = logging.getLogger(LOGGER_NAME)

# Dictionary of properties of user role resources, in this format:
#   name: (allowed, create, update, eq_func, type_cast)
# where:
#   name: Name of the property according to the data model, with hyphens
#     replaced by underscores (this is how it is or would be specified in
#     the 'properties' module parameter).
#   allowed: Indicates whether it is allowed in the 'properties' module
#     parameter.
#   create: Indicates whether it can be specified for the "Create User Role"
#    operation.
#   update: Indicates whether it can be specified for the "Modify User Role
#     Properties" operation (at all).
#   update_while_active: Not used for this module.
#   eq_func: Equality test function for two values of the property; None means
#     to use Python equality.
#   type_cast: Type cast function for an input value of the property; None
#     means to use it directly. This can be used for example to convert
#     integers provided as strings by Ansible back into integers (that is a
#     current deficiency of Ansible).
ZHMC_USER_ROLE_PROPERTIES = {

    # create+update properties:
    'name': (False, True, True, True, None, None),
    # name: provided in 'name' module parm
    'description': (True, True, True, True, None, to_unicode),
    # Artificial property replacing the ..._uri property:
    'associated_system_defined_user_role_name':
        (True, True, True, True, None, to_unicode),
    'associated_system_defined_user_role_uri': (
        False, False, True, True, None, None),  # via the ..._name property
    'is_inheritance_enabled': (True, True, True, True, None, bool),
    # Artificial property 'permissions' replacing the same-named HMC property:
    'permissions': (True, False, False, True, None, None),

    # read-only properties:
    'object_uri': (False, False, False, True, None, None),
    'object_id': (False, False, False, True, None, None),
    'parent': (False, False, False, True, None, None),
    'class': (False, False, False, True, None, None),
    'replication_overwrite_possible': (False, False, False, True, None, bool),
    'type': (False, False, False, True, None, None),
}


def process_properties(client, urole, params):
    """
    Process the properties specified in the 'properties' module parameter,
    and return two dictionaries (create_props, update_props) that contain
    the properties that can be created, and the properties that can be updated,
    respectively. If the resource exists, the input property values are
    compared with the existing resource property values and the returned set
    of properties is the minimal set of properties that need to be changed.

    - Underscores in the property names are translated into hyphens.
    - The presence of read-only properties, invalid properties (i.e. not
      defined in the data model for user roles), and properties that are
      not allowed because of restrictions or because they are auto-created from
      an artificial property is surfaced by raising ParameterError.

    Parameters:

      urole (zhmcclient.UserRole): User Role object to be updated with
        the full set of current properties, or `None` if it did not previously
        exist.

      params (dict): Module input parameters.

    Returns:
      tuple of (create_props, update_props),
      where:
        * create_props: dict of properties for
          zhmcclient.UserRoleManager.create()
        * update_props: dict of properties for
          zhmcclient.UserRole.update_properties()
        * cur_perms, add_perms, rem_perms: Permission dicts for current, to be
          added and to be removed permissions.

    Raises:
      ParameterError: An issue with the module parameters.
    """
    console = client.consoles.console
    create_props = {}
    update_props = {}
    add_perms = {}  # permission dictionary of perms to be added
    rem_perms = {}  # permission dictionary of perms to be removed

    # handle 'name' property
    urole_name = to_unicode(params['name'])
    if urole is None:
        # User role does not exist yet.
        create_props['name'] = urole_name
    else:
        # User role does already exist.
        # We looked up the user role by name, so we will never have to
        # update the user role name.
        pass

    # handle the other properties
    input_props = params.get('properties', None)
    if input_props is None:
        input_props = {}

    # Get current permissions
    if urole:
        cur_perms = current_perm_dict(
            client, urole.get_property('permissions'))
    else:
        cur_perms = {}

    for prop_name in input_props:

        if prop_name not in ZHMC_USER_ROLE_PROPERTIES:
            raise ParameterError(
                "Property {0!r} is not defined in the data model for "
                "user roles.".format(prop_name))

        allowed, create, update, update_while_active, eq_func, type_cast = \
            ZHMC_USER_ROLE_PROPERTIES[prop_name]

        if not allowed:
            raise ParameterError(
                "Property {0!r} is not allowed in the 'properties' module "
                "parameter.".format(prop_name))

        # Process artificial/special properties allowed in input parameters

        if prop_name == 'associated_system_defined_user_role_name':
            sys_urole_name = input_props[prop_name]
            try:
                sys_urole = console.user_roles.find(name=sys_urole_name)
            except zhmcclient.NotFound:
                raise ParameterError(
                    "Cannot find system-defined user role {0!r} specified in "
                    "the 'associated_system_defined_user_role_name' input "
                    "property.".format(sys_urole_name))
            create_props['associated-system-defined-user-role-uri'] = \
                sys_urole.uri
            continue

        if prop_name == 'permissions':

            tgt_perms = target_perm_dict(client, input_props[prop_name])

            # Mark missing permissions as to be added
            for perm_key in tgt_perms:
                if perm_key not in cur_perms:
                    add_perms[perm_key] = tgt_perms[perm_key]

            # Mark superfluous permissions as to be removed
            for perm_key in cur_perms:
                if perm_key not in tgt_perms:
                    rem_perms[perm_key] = cur_perms[perm_key]

            continue

        # Process a normal (= non-artificial) property
        _create_props, _update_props, _stop = process_normal_property(
            prop_name, ZHMC_USER_ROLE_PROPERTIES, input_props, urole)
        create_props.update(_create_props)
        update_props.update(_update_props)
        if _stop:
            raise AssertionError()

    return create_props, update_props, cur_perms, add_perms, rem_perms


def create_check_mode_urole(client, create_props, update_props):
    """
    Create and return a fake local User Role object.

    This is used when a user role needs to be created in check mode.

    This function must be consistent with the behavior of the
    "Create User Role" operation on the HMC. HTTP errors the HMC would
    return are indicated by raising zhmcclient.HTTPError.
    """
    console = client.consoles.console
    input_props = {}
    input_props.update(create_props)
    input_props.update(update_props)

    # Check required input properties
    missing_props = []
    for pname in ('name',):
        if pname not in input_props:
            missing_props.append(pname)
    if missing_props:
        raise zhmcclient.HTTPError({
            'http-status': 400,
            'reason': 4,
            'message': "Required input properties missing for "
            "Create User Role: {p}".format(p=missing_props),
        })

    optasks_urole = console.user_roles.find(name='hmc-operator-tasks')

    # Defaults for properties
    props = {
        # createable/updateable
        'description': '',
        'associated-system-defined-user-role-uri': optasks_urole.uri,
        'is-inheritance-enabled': False,
        'permissions': [],
        # read-only
        'type': 'user-defined',
        'replication-overwrite-possible': True,
    }

    # Apply specified input properties on top of the defaults
    props.update(input_props)

    urole_oid = 'fake-{0}'.format(uuid.uuid4())
    urole = console.user_roles.resource_object(urole_oid, props=props)

    return urole


def uri_to_object(client, obj_uri):
    """
    Convert the canonical URI of an HMC object to an zhmcclient object
    representing it. The object will have only minimal properties. An existence
    check is performed, so unless zhmcclient.NotFound is raised, the resource
    exists on the HMC.

    Returns:
      zhmcclient.BaseResource: zhmcclient object representing the resource.

    Raises:
      zhmcclient.NotFound
    """
    console = client.consoles.console
    if obj_uri.startswith('/api/cpcs/'):
        obj = client.cpcs.find(**{'object-uri': obj_uri})
    elif obj_uri.startswith('/api/console/tasks/'):
        obj = console.tasks.find(**{'element-uri': obj_uri})
    elif obj_uri.startswith('/api/groups/'):
        raise NotImplementedError(
            "zhmcclient does not support groups")
    elif obj_uri.startswith('/api/partitions/'):
        obj_list = console.list_permitted_partitions(
            filter_args={'object-uri': obj_uri})
        if not obj_list:
            raise zhmcclient.NotFound(
                "Partition with object-uri: {u!r}".format(u=obj_uri))
        obj = obj_list[0]
    elif obj_uri.startswith('/api/logical-partitions/'):
        obj_list = console.list_permitted_lpars(
            filter_args={'object-uri': obj_uri})
        if not obj_list:
            raise zhmcclient.NotFound(
                "LPAR with object-uri: {u!r}".format(u=obj_uri))
        obj = obj_list[0]
    elif obj_uri.startswith('/api/adapters/'):
        for cpc in client.cpcs.list():
            try:
                obj = cpc.adapters.find(**{'object-uri': obj_uri})
                break
            except zhmcclient.NotFound:
                continue
        else:
            raise zhmcclient.NotFound(
                "Adapter with object-uri: {u!r}".format(u=obj_uri))
    elif obj_uri.startswith('/api/storage-groups/'):
        obj = console.storage_groups.find(**{'object-uri': obj_uri})
    elif obj_uri.startswith('/api/storage-templates/'):
        obj = console.storage_group_templates.find(**{'object-uri': obj_uri})
    else:
        raise ParameterError(
            "Resource with URI {u!r} not supported for user "
            "role permissions".format(u=obj_uri))
    return obj


def current_perm_dict(client, hmc_permissions):
    """
    Return the permission dictionary for the specified HMC permissions.

    The permission dictionary can be looked up by resource URI or resource
    class and thus can be used to find out whether a permission exists or
    needs to be added or removed.

    The permission dictionary can have these items:
    - key: resource URI, value: tuple(kwargs, zhmcclient object)
    - key: resource class, value: tuple(kwargs, None)

    Where:
    - kwargs: Optional kwargs for UserRole add_permissions/remove_permissions
      methods.

    Parameters:
      hmc_permissions(list): List of HMC permission-info items

    Returns:
      dict: Permission dictionary

    Raises:
      zhmcclient.NotFound: Resource with that URI was not found on HMC
    """
    cur_perms = {}
    for perm_item in hmc_permissions:
        perm_item2 = dict(perm_item)
        obj_type = perm_item2.pop('permitted-object-type')
        obj_key = perm_item2.pop('permitted-object')
        # This leaves only additional option parms in perm_item2
        opt_kwargs = {}
        for hmc_name, value in perm_item2.items():
            if hmc_name == 'include-members':
                opt_kwargs['include_members'] = value
            elif hmc_name == 'view-only-mode':
                opt_kwargs['view_mode'] = value
            else:
                pass  # The HMC data model does not define any further options
        if obj_type == 'object':
            obj = uri_to_object(client, obj_key)  # May raise NotFound
            cur_perms[obj_key] = (opt_kwargs, obj)
        else:  # 'object-class'
            cur_perms[obj_key] = (opt_kwargs, None)
    # LOGGER.debug("Current permissions on HMC: %r", cur_perms)
    return cur_perms


def target_perm_dict(client, ansi_permissions):
    """
    Return the permission dictionary for the specified Ansible permissions.

    The permission dictionary can be looked up by resource URI or resource
    class and thus can be used to find out whether a permission exists or
    needs to be added or removed.

    The permission dictionary can have these items:
    - key: resource URI, value: tuple(kwargs, zhmcclient object)
    - key: resource class, value: tuple(kwargs, None)

    Where:
    - kwargs: Optional kwargs for UserRole add_permissions/remove_permissions
      methods.

    Parameters:
      ansi_permissions(list): List of permission items formatted as in the
        'permissions' parameter of this Ansible module.

    Returns:
      dict: Permission dictionary

    Raises:
      zhmcclient.NotFound: Resource with that URI was not found on HMC
      ParameterError: Invalid combination of resources
    """
    console = client.consoles.console
    tgt_perms = {}
    for perm_item in ansi_permissions:
        perm_item2 = dict(perm_item)
        keys = set(perm_item2.keys())
        if keys == {'class'}:
            resource_class = perm_item2.pop('class')
            if perm_item2:
                raise ParameterError(
                    "Invalid additional items in permission item for "
                    "resource class {n!r}: {i!r}".
                    format(n=resource_class, i=perm_item2))
            tgt_perms[resource_class] = ({}, None)
        elif keys == {'cpc'}:
            cpc_name = perm_item2.pop('cpc')
            if perm_item2:
                raise ParameterError(
                    "Invalid additional items in permission item for "
                    "CPC {n!r}: {i!r}".
                    format(n=cpc_name, i=perm_item2))
            cpc = client.cpcs.find(name=cpc_name)
            tgt_perms[cpc.uri] = ({}, cpc)
        elif 'task' in keys:
            task_name = perm_item2.pop('task')
            view_only = perm_item2.pop('view_only', None)
            if perm_item2:
                raise ParameterError(
                    "Invalid additional items in permission item for "
                    "task {n!r}: {i!r}".
                    format(n=task_name, i=perm_item2))
            kwargs = {}
            if view_only is not None:
                kwargs['view_only'] = view_only
            task = console.tasks.find(name=task_name)
            tgt_perms[task.uri] = (kwargs, task)
        elif 'group' in keys:
            group_name = perm_item2.pop('group')
            include_members = perm_item2.pop('include_members', None)
            if perm_item2:
                raise ParameterError(
                    "Invalid additional items in permission item for "
                    "group {n!r}: {i!r}".
                    format(n=group_name, i=perm_item2))
            kwargs = {}
            if include_members is not None:
                kwargs['include_members'] = include_members
            group = console.groups.find(name=group_name)
            tgt_perms[group.uri] = (kwargs, group)
        elif keys == {'partition', 'cpc'}:
            cpc_name = perm_item2.pop('cpc')
            part_name = perm_item2.pop('partition')
            if perm_item2:
                raise ParameterError(
                    "Invalid additional items in permission item for "
                    "partition {n!r} on CPC {c!r}: {i!r}".
                    format(n=part_name, c=cpc_name, i=perm_item2))
            cpc = client.cpcs.find(name=cpc_name)
            part = cpc.partitions.find(name=part_name)
            tgt_perms[part.uri] = ({}, part)
        elif keys == {'logical_partition', 'cpc'}:
            cpc_name = perm_item2.pop('cpc')
            lpar_name = perm_item2.pop('logical_partition')
            if perm_item2:
                raise ParameterError(
                    "Invalid additional items in permission item for "
                    "LPAR {n!r} on CPC {c!r}: {i!r}".
                    format(n=lpar_name, c=cpc_name, i=perm_item2))
            cpc = client.cpcs.find(name=cpc_name)
            lpar = cpc.lpars.find(name=lpar_name)
            tgt_perms[lpar.uri] = (perm_item2, lpar)
        elif keys == {'adapter', 'cpc'}:
            cpc_name = perm_item2.pop('cpc')
            adapter_name = perm_item2.pop('adapter')
            if perm_item2:
                raise ParameterError(
                    "Invalid additional items in permission item for "
                    "adapter {n!r} on CPC {c!r}: {i!r}".
                    format(n=adapter_name, c=cpc_name, i=perm_item2))
            cpc = client.cpcs.find(name=cpc_name)
            adapter = cpc.adapters.find(name=adapter_name)
            tgt_perms[adapter.uri] = (perm_item2, adapter)
        elif keys == {'storage_group', 'cpc'}:
            cpc_name = perm_item2.pop('cpc')
            sg_name = perm_item2.pop('storage_group')
            if perm_item2:
                raise ParameterError(
                    "Invalid additional items in permission item for "
                    "storage group {n!r} on CPC {c!r}: {i!r}".
                    format(n=sg_name, c=cpc_name, i=perm_item2))
            cpc = client.cpcs.find(name=cpc_name)
            sg = cpc.storage_groups.find(name=sg_name)
            tgt_perms[sg.uri] = (perm_item2, sg)
        elif keys == {'storage_group_template', 'cpc'}:
            cpc_name = perm_item2.pop('cpc')
            st_name = perm_item2.pop('storage_group_template')
            if perm_item2:
                raise ParameterError(
                    "Invalid additional items in permission item for "
                    "storage group template {n!r} on CPC {c!r}: {i!r}".
                    format(n=st_name, c=cpc_name, i=perm_item2))
            cpc = client.cpcs.find(name=cpc_name)
            st = cpc.storage_group_templates.find(name=st_name)
            tgt_perms[st.uri] = (perm_item2, st)
        else:
            raise ParameterError(
                "Invalid combination of resources for permitted "
                "object for user role: {i!r}".format(i=perm_item2))
    # LOGGER.debug("Target permissions: %r", tgt_perms)
    return tgt_perms


def result_permissions(perm_dict):
    """
    Return the Ansible permissions from a permission dictionary.

    The Ansible permissions are ready to be returned from the module.

    The permission dictionary can have these items:
    - key: resource URI, value: tuple(kwargs, zhmcclient object)
    - key: resource class, value: tuple(kwargs, None)

    Where:
    - kwargs: Optional kwargs for UserRole add_permissions/remove_permissions
      methods. They are at the same time the representations in the Ansible
      result.

    Parameters:
      perm_dict(dict): Permission dictionary

    Returns:
      list: List of permission items formatted for being returned by this
      Ansible module.

    Raises:
      zhmcclient.NotFound: Resource with that URI was not found on HMC
      ParameterError: Invalid permitted object
    """
    ansi_perms = []
    for perm_key in perm_dict:
        opt_kwargs, obj = perm_dict[perm_key]
        if obj is None:  # resource class
            item = {'class': perm_key}
            item.update(opt_kwargs)
        elif isinstance(obj, zhmcclient.Cpc):
            item = {'cpc': obj.name}
            item.update(opt_kwargs)
        elif isinstance(obj, zhmcclient.Task):
            item = {'task': obj.name}
            item.update(opt_kwargs)
        # zhmcclient.Group not implemented:
        elif isinstance(obj, zhmcclient.Partition):
            cpc = obj.manager.parent
            item = {'partition': obj.name, 'cpc': cpc.name}
            item.update(opt_kwargs)
        elif isinstance(obj, zhmcclient.Lpar):
            cpc = obj.manager.parent
            item = {'logical_partition': obj.name, 'cpc': cpc.name}
            item.update(opt_kwargs)
        elif isinstance(obj, zhmcclient.Adapter):
            cpc = obj.manager.parent
            item = {'adapter': obj.name, 'cpc': cpc.name}
            item.update(opt_kwargs)
        elif isinstance(obj, zhmcclient.StorageGroup):
            cpc = obj.manager.parent
            item = {'storage_group': obj.name, 'cpc': cpc.name}
            item.update(opt_kwargs)
        elif isinstance(obj, zhmcclient.StorageGroupTemplate):
            cpc = obj.manager.parent
            item = {'storage_group_template': obj.name, 'cpc': cpc.name}
            item.update(opt_kwargs)
        else:
            raise NotImplementedError(
                "Invalid permitted object: {o!r}".format(o=obj))
        ansi_perms.append(item)
    return ansi_perms


def urole_uri_to_name(console, urole_uri):
    """
    Return the name of a user role with the specified URI.

    Parameters:

      console(zhmcclient.Console): Console object to be used for finding
        the user role object.

      urole_uri(string): Canonical URI of the user role.

    Returns:
      string: Name of the user role.

    Raises:
      zhmcclient.NotFound: Resource with that URI was not found on HMC
    """
    urole = console.user_roles.find(**{'object-uri': urole_uri})
    return urole.name


def ensure_present(params, check_mode):
    """
    Ensure that the user role exists and has the specified properties.

    Raises:
      ParameterError: An issue with the module parameters.
      zhmcclient.Error: Any zhmcclient exception can happen.
    """

    host = params['hmc_host']
    userid, password, ca_certs, verify = get_hmc_auth(params['hmc_auth'])
    urole_name = params['name']
    _faked_session = params.get('_faked_session', None)

    changed = False
    result = {}

    session = get_session(
        _faked_session, host, userid, password, ca_certs, verify)
    try:
        client = zhmcclient.Client(session)
        console = client.consoles.console
        # The default exception handling is sufficient for the above.

        try:
            urole = console.user_roles.find(name=urole_name)
        except zhmcclient.NotFound:
            urole = None

        if urole is None:
            # It does not exist. Create it and update it if there are
            # update-only properties.
            create_props, update_props, cur_perms, add_perms, rem_perms = \
                process_properties(client, urole, params)
            update2_props = {}
            for name, value in update_props.items():
                if name not in create_props:
                    update2_props[name] = value
            if not check_mode:
                urole = console.user_roles.create(create_props)
                if update2_props:
                    urole.update_properties(update2_props)
                # We refresh the properties after the update, in case an
                # input property value gets changed.
                urole.pull_full_properties()
            else:
                # Create a User Role object locally
                urole = create_check_mode_urole(
                    client, create_props, update2_props)
            result = dict(urole.properties)
            changed = True

            for perm_key in rem_perms:
                opt_kwargs, obj = rem_perms[perm_key]
                if obj is None:  # resource class
                    kwargs = dict(permitted_object=perm_key)
                else:
                    kwargs = dict(permitted_object=obj)
                kwargs.update(opt_kwargs)
                LOGGER.debug(
                    "Removing permission %r from user role %r",
                    kwargs, urole_name)
                if not check_mode:
                    urole.remove_permission(**kwargs)
                del cur_perms[perm_key]
            for perm_key in add_perms:
                opt_kwargs, obj = add_perms[perm_key]
                if obj is None:  # resource class
                    kwargs = dict(permitted_object=perm_key)
                else:
                    kwargs = dict(permitted_object=obj)
                kwargs.update(opt_kwargs)
                LOGGER.debug(
                    "Adding permission %r to user role %r",
                    kwargs, urole_name)
                if not check_mode:
                    urole.add_permission(**kwargs)
                cur_perms[perm_key] = add_perms[perm_key]

        else:
            # It exists. Update its properties.
            urole.pull_full_properties()
            result = dict(urole.properties)
            create_props, update_props, cur_perms, add_perms, rem_perms = \
                process_properties(client, urole, params)
            if update_props:
                LOGGER.debug(
                    "Existing user role %r needs to get properties "
                    "updated: %r", urole_name, update_props)
                if not check_mode:
                    urole.update_properties(update_props)
                    # We refresh the properties after the update, in case an
                    # input property value gets changed.
                    urole.pull_full_properties()
                    result = dict(urole.properties)
                else:
                    # Update the local User Role object's properties
                    result.update(update_props)
                changed = True

                for perm_key in rem_perms:
                    opt_kwargs, obj = rem_perms[perm_key]
                    if obj is None:  # resource class
                        kwargs = dict(permitted_object=perm_key)
                    else:
                        kwargs = dict(permitted_object=obj)
                    kwargs.update(opt_kwargs)
                    LOGGER.debug(
                        "Removing permission %r from user role %r",
                        kwargs, urole_name)
                    if not check_mode:
                        urole.remove_permission(**kwargs)
                    del cur_perms[perm_key]
                for perm_key in add_perms:
                    opt_kwargs, obj = add_perms[perm_key]
                    if obj is None:  # resource class
                        kwargs = dict(permitted_object=perm_key)
                    else:
                        kwargs = dict(permitted_object=obj)
                    kwargs.update(opt_kwargs)
                    LOGGER.debug(
                        "Adding permission %r to user role %r",
                        kwargs, urole_name)
                    if not check_mode:
                        urole.add_permission(**kwargs)
                    cur_perms[perm_key] = add_perms[perm_key]

        if not urole:
            raise AssertionError()

        # Process artificial properties

        result['permissions'] = result_permissions(cur_perms)

        sys_urole_uri = result['associated-system-defined-user-role-uri']
        if sys_urole_uri:
            sys_urole_name = urole_uri_to_name(console, sys_urole_uri)
        else:
            sys_urole_name = None
        result['associated-system-defined-user-role-name'] = sys_urole_name

        return changed, result

    finally:
        session.logoff()


def ensure_absent(params, check_mode):
    """
    Ensure that the user role does not exist.

    Raises:
      ParameterError: An issue with the module parameters.
      zhmcclient.Error: Any zhmcclient exception can happen.
    """

    host = params['hmc_host']
    userid, password, ca_certs, verify = get_hmc_auth(params['hmc_auth'])
    urole_name = params['name']
    _faked_session = params.get('_faked_session', None)

    changed = False
    result = {}

    session = get_session(
        _faked_session, host, userid, password, ca_certs, verify)
    try:
        client = zhmcclient.Client(session)
        console = client.consoles.console
        # The default exception handling is sufficient for the above.

        try:
            urole = console.user_roles.find(name=urole_name)
        except zhmcclient.NotFound:
            return changed, result

        if not check_mode:
            urole.delete()
        changed = True

        return changed, result

    finally:
        session.logoff()


def facts(params, check_mode):
    """
    Return facts about a user role.

    Raises:
      ParameterError: An issue with the module parameters.
      zhmcclient.Error: Any zhmcclient exception can happen.
    """

    host = params['hmc_host']
    userid, password, ca_certs, verify = get_hmc_auth(params['hmc_auth'])
    urole_name = params['name']
    _faked_session = params.get('_faked_session', None)

    changed = False
    result = {}

    session = get_session(
        _faked_session, host, userid, password, ca_certs, verify)
    try:
        # The default exception handling is sufficient for this code
        client = zhmcclient.Client(session)
        console = client.consoles.console

        urole = console.user_roles.find(name=urole_name)
        urole.pull_full_properties()

        result = dict(urole.properties)

        # Process artificial properties

        cur_perms = current_perm_dict(client, urole.get_property('permissions'))
        result['permissions'] = result_permissions(cur_perms)

        sys_urole_uri = result['associated-system-defined-user-role-uri']
        if sys_urole_uri:
            sys_urole_name = urole_uri_to_name(console, sys_urole_uri)
        else:
            sys_urole_name = None
        result['associated-system-defined-user-role-name'] = sys_urole_name

        return changed, result

    finally:
        session.logoff()


def perform_task(params, check_mode):
    """
    Perform the task for this module, dependent on the 'state' module
    parameter.

    If check_mode is True, check whether changes would occur, but don't
    actually perform any changes.

    Raises:
      ParameterError: An issue with the module parameters.
      zhmcclient.Error: Any zhmcclient exception can happen.
    """
    actions = {
        "absent": ensure_absent,
        "present": ensure_present,
        "facts": facts,
    }
    return actions[params['state']](params, check_mode)


def main():

    # The following definition of module input parameters must match the
    # description of the options in the DOCUMENTATION string.
    argument_spec = dict(
        hmc_host=dict(required=True, type='str'),
        hmc_auth=dict(
            required=True,
            type='dict',
            options=dict(
                userid=dict(required=True, type='str'),
                password=dict(required=True, type='str', no_log=True),
                ca_certs=dict(required=False, type='str', default=None),
                verify=dict(required=False, type='bool', default=True),
            ),
        ),
        name=dict(required=True, type='str'),
        state=dict(required=True, type='str',
                   choices=['absent', 'present', 'facts']),
        properties=dict(required=False, type='dict', default={}),
        log_file=dict(required=False, type='str', default=None),
        _faked_session=dict(required=False, type='raw'),
    )

    module = AnsibleModule(
        argument_spec=argument_spec,
        supports_check_mode=True)

    if IMP_URLLIB3_ERR is not None:
        module.fail_json(msg=missing_required_lib("requests"),
                         exception=IMP_URLLIB3_ERR)

    requests.packages.urllib3.disable_warnings()

    if IMP_ZHMCCLIENT_ERR is not None:
        module.fail_json(msg=missing_required_lib("zhmcclient"),
                         exception=IMP_ZHMCCLIENT_ERR)

    common_fail_on_import_errors(module)

    log_file = module.params['log_file']
    log_init(LOGGER_NAME, log_file)

    _params = dict(module.params)
    del _params['hmc_auth']
    LOGGER.debug("Module entry: params: %r", _params)

    try:

        changed, result = perform_task(module.params, module.check_mode)

    except (Error, zhmcclient.Error) as exc:
        # These exceptions are considered errors in the environment or in user
        # input. They have a proper message that stands on its own, so we
        # simply pass that message on and will not need a traceback.
        msg = "{0}: {1}".format(exc.__class__.__name__, exc)
        LOGGER.debug(
            "Module exit (failure): msg: %s", msg)
        module.fail_json(msg=msg)
    # Other exceptions are considered module errors and are handled by Ansible
    # by showing the traceback.

    LOGGER.debug(
        "Module exit (success): changed: %r, user_role: %r",
        changed, result)
    module.exit_json(changed=changed, user_role=result)


if __name__ == '__main__':
    main()
