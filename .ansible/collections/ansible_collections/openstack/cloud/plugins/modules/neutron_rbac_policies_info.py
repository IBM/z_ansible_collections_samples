#!/usr/bin/python
# coding: utf-8 -*-

# Copyright: Ansible Project
# (c) 2021, Ashraf Hasson <ahasson@redhat.com>
# GNU General Public License v3.0+ (see COPYING or https://www.gnu.org/licenses/gpl-3.0.txt)

from __future__ import (absolute_import, division, print_function)
__metaclass__ = type

DOCUMENTATION = r'''
---
module: neutron_rbac_policies_info
short_description: Fetch Neutron policies.
author: OpenStack Ansible SIG
description:
  - Get RBAC policies against a network, security group or a QoS Policy for one or more projects.
  - If a C(policy_id) was not provided, this module will attempt to fetch all available policies.
  - Accepts same arguments as OpenStackSDK network proxy C(find_rbac_policy) and C(rbac_policies) functions which are ultimately passed over to C(RBACPolicy)
  - All parameters passed in to this module act as a filter for when no C(policy_id) was provided, otherwise they're ignored.
  - Returns None if no matching policy was found as opposed to failing.

options:
  policy_id:
    description:
      - The RBAC policy ID
      - If provided, all other filters are ignored
    type: str
  object_id:
    description:
      - The object ID (the subject of the policy) to which the RBAC rules applies
      - This would be the ID of a network, security group or a qos policy
      - Mutually exclusive with the C(object_type)
    type: str
  object_type:
    description:
      - Can be one of the following object types C(network), C(security_group) or C(qos_policy)
      - Mutually exclusive with the C(object_id)
    choices: ['network', 'security_group', 'qos_policy']
    type: str
  target_project_id:
    description:
      - Filters the RBAC rules based on the target project id
      - Logically AND'ed with other filters
      - Mutually exclusive with C(project_id)
    type: str
  project_id:
    description:
      - Filters the RBAC rules based on the project id to which the object belongs to
      - Logically AND'ed with other filters
      - Mutually exclusive with C(target_project_id)
    type: str
  project:
    description:
      - Filters the RBAC rules based on the project name
      - Logically AND'ed with other filters
    type: str
  action:
    description:
      - Can be either of the following options C(access_as_shared) | C(access_as_external)
      - Logically AND'ed with other filters
    choices: ['access_as_shared', 'access_as_external']
    type: str

extends_documentation_fragment:
  - openstack.cloud.openstack
'''

EXAMPLES = r'''
# Gather all rbac policies for a project
- name: Get all rbac policies for {{ project }}
  openstack.cloud.neutron_rbac_policies_info:
    project_id: "{{ project.id }}"
'''

RETURN = r'''
# return value can either be plural or signular depending on what was passed in as parameters
policies:
  description:
    - List of rbac policies, this could also be returned as a singular element, i.e., 'policy'
  type: complex
  returned: always
  contains:
    object_id:
      description:
        - The UUID of the object to which the RBAC rules apply
      type: str
      sample: "7422172b-2961-475c-ac68-bd0f2a9960ad"
    target_project_id:
      description:
        - The UUID of the target project
      type: str
      sample: "c201a689c016435c8037977166f77368"
    project_id:
      description:
        - The UUID of the project to which access is granted
      type: str
      sample: "84b8774d595b41e89f3dfaa1fd76932c"
    object_type:
      description:
        - The object type to which the RBACs apply
      type: str
      sample: "network"
    action:
      description:
        -  The access model specified by the RBAC rules
      type: str
      sample: "access_as_shared"
    id:
      description:
        - The ID of the RBAC rule/policy
      type: str
      sample: "4154ce0c-71a7-4d87-a905-09762098ddb9"
    name:
      description:
        - The name of the RBAC rule; usually null
      type: str
      sample: null
    location:
       description:
         - A dictionary of the project details to which access is granted
       type: dict
       sample: >-
            {
                "cloud": "devstack",
                "region_name": "",
                "zone": null,
                "project": {
                    "id": "84b8774d595b41e89f3dfaa1fd76932c",
                    "name": null,
                    "domain_id": null,
                    "domain_name": null
                }
            }
'''

import re
from ansible_collections.openstack.cloud.plugins.module_utils.openstack import OpenStackModule


class NeutronRbacPoliciesInfo(OpenStackModule):
    argument_spec = dict(
        policy_id=dict(),
        object_id=dict(),  # ID of the object that this RBAC policy affects.
        object_type=dict(choices=['security_group', 'qos_policy', 'network']),  # Type of the object that this RBAC policy affects.
        target_project_id=dict(),  # The ID of the project this RBAC will be enforced.
        project_id=dict(),  # The owner project ID.
        project=dict(),
        action=dict(choices=['access_as_external', 'access_as_shared']),  # Action for the RBAC policy.
    )

    module_kwargs = dict(
        supports_check_mode=True,
    )

    def _filter_policies_by(self, policies, key, value):
        filtered = []
        regexp = re.compile(r"location\.project\.([A-Za-z]+)")
        if regexp.match(key):
            attribute = key.split('.')[-1]
            for p in policies:
                if p['location']['project'][attribute] == value:
                    filtered.append(p)
        else:
            for p in policies:
                if getattr(p, key) == value:
                    filtered.append(p)

        return filtered

    def _get_rbac_policies(self):
        object_type = self.params.get('object_type')
        project_id = self.params.get('project_id')
        action = self.params.get('action')

        search_attributes = {}
        if object_type is not None:
            search_attributes['object_type'] = object_type
        if project_id is not None:
            search_attributes['project_id'] = project_id
        if action is not None:
            search_attributes['action'] = action

        try:
            policies = []
            generator = self.conn.network.rbac_policies(**search_attributes)
            for p in generator:
                policies.append(p)
        except self.sdk.exceptions.OpenStackCloudException as ex:
            self.fail_json(msg='Failed to get RBAC policies: {0}'.format(str(ex)))

        return policies

    def run(self):
        policy_id = self.params.get('policy_id')
        object_id = self.params.get('object_id')
        object_type = self.params.get('object_type')
        project_id = self.params.get('project_id')
        project = self.params.get('project')
        target_project_id = self.params.get('target_project_id')

        if self.ansible.check_mode:
            self.exit_json(changed=False)

        if policy_id is not None:
            try:
                policy = self.conn.network.get_rbac_policy(policy_id)
                self.exit_json(changed=False, policy=policy)
            except self.sdk.exceptions.ResourceNotFound:
                self.exit_json(changed=False, policy=None)
            except self.sdk.exceptions.OpenStackCloudException as ex:
                self.fail_json(msg='Failed to get RBAC policy: {0}'.format(str(ex)))
        else:
            if object_id is not None and object_type is not None:
                self.fail_json(msg='object_id and object_type are mutually exclusive, please specify one of the two.')
            if project_id is not None and target_project_id is not None:
                self.fail_json(msg='project_id and target_project_id are mutually exclusive, please specify one of the two.')

        filtered_policies = self._get_rbac_policies()

        if project is not None:
            filtered_policies = self._filter_policies_by(filtered_policies, 'location.project.name', project)
        if object_id is not None:
            filtered_policies = self._filter_policies_by(filtered_policies, 'object_id', object_id)
        if target_project_id is not None:
            filtered_policies = self._filter_policies_by(filtered_policies, 'target_project_id', target_project_id)

        self.exit_json(policies=filtered_policies, changed=False)


def main():
    module = NeutronRbacPoliciesInfo()
    module()


if __name__ == '__main__':
    main()
