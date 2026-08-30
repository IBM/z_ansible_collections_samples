#!/usr/bin/python

# Copyright: Ansible Project
# (c) 2021, Ashraf Hasson <ahasson@redhat.com>
# GNU General Public License v3.0+ (see COPYING or https://www.gnu.org/licenses/gpl-3.0.txt)

DOCUMENTATION = r'''
---
module: neutron_rbac_policy
short_description: Create or delete a Neutron policy to apply a RBAC rule against an object.
author: OpenStack Ansible SIG
description:
  - Create a policy to apply a RBAC rule against a network, security group or a QoS Policy or update/delete an existing policy.
  - If a C(policy_id) was provided but not found, this module will attempt to create a new policy rather than error out when updating an existing rule.
  - Accepts same arguments as OpenStackSDK network proxy C(find_rbac_policy) and C(rbac_policies) functions which are ultimately passed over to C(RBACPolicy)

options:
  policy_id:
    description:
      - The RBAC policy ID
      - Required when deleting or updating an existing RBAC policy rule, ignored otherwise
    type: str
  object_id:
    description:
      - The object ID (the subject of the policy) to which the RBAC rule applies
      - Cannot be changed when updating an existing policy
      - Required when creating a RBAC policy rule, ignored when deleting a policy
    type: str
  object_type:
    description:
      - Can be one of the following object types C(network), C(security_group) or C(qos_policy)
      - Cannot be changed when updating an existing policy
      - Required when creating a RBAC policy rule, ignored when deleting a policy
    choices: ['network', 'security_group', 'qos_policy']
    type: str
  target_project_id:
    description:
      - The project to which access to be allowed or revoked/disallowed
      - Can be specified/changed when updating an existing policy
      - Required when creating or updating a RBAC policy rule, ignored when deleting a policy
    type: str
  project_id:
    description:
      - The project to which the object_id belongs
      - Cannot be changed when updating an existing policy
      - Required when creating a RBAC policy rule, ignored when deleting a policy
    type: str
  action:
    description:
      - Can be either of the following options C(access_as_shared) | C(access_as_external)
      - Cannot be changed when updating an existing policy
      - Required when creating a RBAC policy rule, ignored when deleting a policy
    choices: ['access_as_shared', 'access_as_external']
    type: str
  state:
    description:
      - Whether the RBAC rule should be C(present) or C(absent).
    choices: ['present', 'absent']
    default: present
    type: str

extends_documentation_fragment:
  - openstack.cloud.openstack
'''

EXAMPLES = r'''
# Ensure network RBAC policy exists
- name: Create a new network RBAC policy
  neutron_rbac_policy:
    object_id: '7422172b-2961-475c-ac68-bd0f2a9960ad'
    object_type: 'network'
    target_project_id: 'a12f9ce1de0645e0a0b01c2e679f69ec'
    project_id: '84b8774d595b41e89f3dfaa1fd76932d'

# Update network RBAC policy
- name: Update an existing network RBAC policy
  neutron_rbac_policy:
    policy_id: 'f625242a-6a73-47ac-8d1f-91440b2c617f'
    target_project_id: '163c89e065a94e069064e551e15daf0e'

# Delete an existing RBAC policy
- name: Delete RBAC policy
  openstack.cloud.openstack.neutron_rbac_policy:
    policy_id: 'f625242a-6a73-47ac-8d1f-91440b2c617f'
    state: absent
'''

RETURN = r'''
policy:
  description:
    - A hash representing the policy
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

from ansible_collections.openstack.cloud.plugins.module_utils.openstack import OpenStackModule


class NeutronRbacPolicy(OpenStackModule):
    argument_spec = dict(
        policy_id=dict(),
        object_id=dict(),  # ID of the object that this RBAC policy affects.
        object_type=dict(choices=['security_group', 'qos_policy', 'network']),  # Type of the object that this RBAC policy affects.
        target_project_id=dict(),  # The ID of the project this RBAC will be enforced.
        project_id=dict(),  # The owner project ID.
        action=dict(choices=['access_as_external', 'access_as_shared']),  # Action for the RBAC policy.
        state=dict(default='present', choices=['absent', 'present'])
    )

    module_kwargs = dict(
        supports_check_mode=True,
    )

    def _delete_rbac_policy(self, policy):
        """
        Delete an existing RBAC policy
        returns: the "Changed" state
        """

        if policy is None:
            self.fail_json(msg='Must specify policy_id for delete')

        try:
            self.conn.network.delete_rbac_policy(policy.id)
        except self.sdk.exceptions.OpenStackCloudException as ex:
            self.fail_json(msg='Failed to delete RBAC policy: {0}'.format(str(ex)))

        return True

    def _create_rbac_policy(self):
        """
        Creates a new RBAC policy
        returns: the "Changed" state of the RBAC policy
        """

        object_id = self.params.get('object_id')
        object_type = self.params.get('object_type')
        target_project_id = self.params.get('target_project_id')
        project_id = self.params.get('project_id')
        action = self.params.get('action')

        attributes = {
            'object_id': object_id,
            'object_type': object_type,
            'target_project_id': target_project_id,
            'project_id': project_id,
            'action': action
        }

        if not all(attributes.values()):
            self.fail_json(msg='Missing one or more required parameter for creating a RBAC policy')

        try:
            search_attributes = dict(attributes)
            del search_attributes['object_id']
            del search_attributes['target_project_id']
            policies = self.conn.network.rbac_policies(**search_attributes)
            for p in policies:
                if p.object_id == object_id and p.target_project_id == target_project_id:
                    return (False, p)

            # if no matching policy exists, attempt to create one
            policy = self.conn.network.create_rbac_policy(**attributes)
        except self.sdk.exceptions.OpenStackCloudException as ex:
            self.fail_json(msg='Failed to create RBAC policy: {0}'.format(str(ex)))

        return (True, policy)

    def _update_rbac_policy(self, policy):
        """
        Updates an existing RBAC policy
        returns: the "Changed" state of the RBAC policy
        """

        object_id = self.params.get('object_id')
        object_type = self.params.get('object_type')
        target_project_id = self.params.get('target_project_id')
        project_id = self.params.get('project_id')
        action = self.params.get('action')

        allowed_attributes = {
            'rbac_policy': policy.id,
            'target_project_id': target_project_id
        }

        disallowed_attributes = {
            'object_id': object_id,
            'object_type': object_type,
            'project_id': project_id,
            'action': action
        }

        if not all(allowed_attributes.values()):
            self.fail_json(msg='Missing one or more required parameter for updating a RBAC policy')

        if any(disallowed_attributes.values()):
            self.fail_json(msg='Cannot change disallowed parameters while updating a RBAC policy: ["object_id", "object_type", "project_id", "action"]')

        try:
            policy = self.conn.network.update_rbac_policy(**allowed_attributes)
        except self.sdk.exceptions.OpenStackCloudException as ex:
            self.fail_json(msg='Failed to update the RBAC policy: {0}'.format(str(ex)))

        return (True, policy)

    def _policy_state_change(self, policy):
        state = self.params['state']
        if state == 'present':
            if not policy:
                return True
        if state == 'absent' and policy:
            return True
        return False

    def run(self):
        policy_id = self.params.get('policy_id')
        state = self.params.get('state')

        if policy_id is not None:
            try:
                policy = self.conn.network.get_rbac_policy(policy_id)
            except self.sdk.exceptions.ResourceNotFound:
                policy = None
            except self.sdk.exceptions.OpenStackCloudException as ex:
                self.fail_json(msg='Failed to get RBAC policy: {0}'.format(str(ex)))
        else:
            policy = None

        if self.ansible.check_mode:
            self.exit_json(changed=self._policy_state_change(policy), policy=policy)

        if state == 'absent':
            if policy is None and policy_id:
                self.exit_json(changed=False)
            if policy_id is None:
                self.fail_json(msg='Must specify policy_id when state is absent')
            if policy is not None:
                changed = self._delete_rbac_policy(policy)
                self.exit_json(changed=changed)
        # state == 'present'
        else:
            if policy is None:
                (changed, new_policy) = self._create_rbac_policy()
            else:
                (changed, new_policy) = self._update_rbac_policy(policy)

            self.exit_json(changed=changed, policy=new_policy)


def main():
    module = NeutronRbacPolicy()
    module()


if __name__ == '__main__':
    main()
