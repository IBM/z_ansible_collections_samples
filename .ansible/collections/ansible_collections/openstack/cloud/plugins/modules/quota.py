#!/usr/bin/python
# Copyright (c) 2016 Pason System Corporation
# GNU General Public License v3.0+ (see COPYING or https://www.gnu.org/licenses/gpl-3.0.txt)

DOCUMENTATION = '''
---
module: quota
short_description: Manage OpenStack Quotas
author: OpenStack Ansible SIG
description:
    - Manage OpenStack Quotas. Quotas can be created,
      updated or deleted using this module. A quota will be updated
      if matches an existing project and is present.
options:
    name:
        description:
            - Name of the OpenStack Project to manage.
        required: true
        type: str
    state:
        description:
            - A value of present sets the quota and a value of absent resets the quota to system defaults.
        default: present
        type: str
        choices: ['absent', 'present']
    backup_gigabytes:
        description: Maximum size of backups in GB's.
        type: int
    backups:
        description: Maximum number of backups allowed.
        type: int
    cores:
        description: Maximum number of CPU's per project.
        type: int
    fixed_ips:
        description: Number of fixed IP's to allow.
        type: int
    floating_ips:
        description: Number of floating IP's to allow in Compute.
        aliases: ['compute_floating_ips']
        type: int
    floatingip:
        description: Number of floating IP's to allow in Network.
        aliases: ['network_floating_ips']
        type: int
    gigabytes:
        description: Maximum volume storage allowed for project.
        type: int
    gigabytes_types:
        description:
            - Per driver volume storage quotas.  Keys should be
              prefixed with C(gigabytes_) values should be ints.
        type: dict
    injected_file_size:
        description: Maximum file size in bytes.
        type: int
    injected_files:
        description: Number of injected files to allow.
        type: int
    injected_path_size:
        description: Maximum path size.
        type: int
    instances:
        description: Maximum number of instances allowed.
        type: int
    key_pairs:
        description: Number of key pairs to allow.
        type: int
    loadbalancer:
        description: Number of load balancers to allow.
        type: int
    metadata_items:
       description: Number of metadata items allowed per instance.
       type: int
    network:
        description: Number of networks to allow.
        type: int
    per_volume_gigabytes:
        description: Maximum size in GB's of individual volumes.
        type: int
    pool:
        description: Number of load balancer pools to allow.
        type: int
    port:
        description: Number of Network ports to allow, this needs to be greater than the instances limit.
        type: int
    properties:
        description: Number of properties to allow.
        type: int
    ram:
        description: Maximum amount of ram in MB to allow.
        type: int
    rbac_policy:
        description: Number of policies to allow.
        type: int
    router:
        description: Number of routers to allow.
        type: int
    security_group_rule:
        description: Number of rules per security group to allow.
        type: int
    security_group:
        description: Number of security groups to allow.
        type: int
    server_group_members:
        description: Number of server group members to allow.
        type: int
    server_groups:
        description: Number of server groups to allow.
        type: int
    snapshots:
        description: Number of snapshots to allow.
        type: int
    snapshots_types:
        description:
            - Per-driver volume snapshot quotas.  Keys should be
              prefixed with C(snapshots_) values should be ints.
        type: dict
    subnet:
        description: Number of subnets to allow.
        type: int
    subnetpool:
        description: Number of subnet pools to allow.
        type: int
    volumes:
        description: Number of volumes to allow.
        type: int
    volumes_types:
        description:
            - Per-driver volume count quotas.  Keys should be
              prefixed with C(volumes_) values should be ints.
        type: dict
    project:
        description: Unused, kept for compatability
        type: int

requirements:
    - "python >= 3.6"
    - "openstacksdk >= 0.13.0"
    - "keystoneauth1 >= 3.4.0"

extends_documentation_fragment:
- openstack.cloud.openstack
'''

EXAMPLES = '''
# List a Project Quota
- openstack.cloud.quota:
    cloud: mycloud
    name: demoproject

# Set a Project back to the defaults
- openstack.cloud.quota:
    cloud: mycloud
    name: demoproject
    state: absent

# Update a Project Quota for cores
- openstack.cloud.quota:
    cloud: mycloud
    name: demoproject
    cores: 100

# Update a Project Quota
- openstack.cloud.quota:
    name: demoproject
    cores: 1000
    volumes: 20
    volumes_type:
      - volume_lvm: 10

# Complete example based on list of projects
- name: Update quotas
  openstack.cloud.quota:
    name: "{{ item.name }}"
    backup_gigabytes: "{{ item.backup_gigabytes }}"
    backups: "{{ item.backups }}"
    cores: "{{ item.cores }}"
    fixed_ips: "{{ item.fixed_ips }}"
    floating_ips: "{{ item.floating_ips }}"
    floatingip: "{{ item.floatingip }}"
    gigabytes: "{{ item.gigabytes }}"
    injected_file_size: "{{ item.injected_file_size }}"
    injected_files: "{{ item.injected_files }}"
    injected_path_size: "{{ item.injected_path_size }}"
    instances: "{{ item.instances }}"
    key_pairs: "{{ item.key_pairs }}"
    loadbalancer: "{{ item.loadbalancer }}"
    metadata_items: "{{ item.metadata_items }}"
    per_volume_gigabytes: "{{ item.per_volume_gigabytes }}"
    pool: "{{ item.pool }}"
    port: "{{ item.port }}"
    properties: "{{ item.properties }}"
    ram: "{{ item.ram }}"
    security_group_rule: "{{ item.security_group_rule }}"
    security_group: "{{ item.security_group }}"
    server_group_members: "{{ item.server_group_members }}"
    server_groups: "{{ item.server_groups }}"
    snapshots: "{{ item.snapshots }}"
    volumes: "{{ item.volumes }}"
    volumes_types:
      volumes_lvm: "{{ item.volumes_lvm }}"
    snapshots_types:
      snapshots_lvm: "{{ item.snapshots_lvm }}"
    gigabytes_types:
      gigabytes_lvm: "{{ item.gigabytes_lvm }}"
  with_items:
    - "{{ projects }}"
  when: item.state == "present"
'''

RETURN = '''
openstack_quotas:
    description: Dictionary describing the project quota.
    returned: Regardless if changes where made or not
    type: dict
    sample:
        openstack_quotas: {
            compute: {
                cores: 150,
                fixed_ips: -1,
                floating_ips: 10,
                injected_file_content_bytes: 10240,
                injected_file_path_bytes: 255,
                injected_files: 5,
                instances: 100,
                key_pairs: 100,
                metadata_items: 128,
                ram: 153600,
                security_group_rules: 20,
                security_groups: 10,
                server_group_members: 10,
                server_groups: 10
            },
            network: {
                floatingip: 50,
                loadbalancer: 10,
                network: 10,
                pool: 10,
                port: 160,
                rbac_policy: 10,
                router: 10,
                security_group: 10,
                security_group_rule: 100,
                subnet: 10,
                subnetpool: -1
            },
            volume: {
                backup_gigabytes: 1000,
                backups: 10,
                gigabytes: 1000,
                gigabytes_lvm: -1,
                per_volume_gigabytes: -1,
                snapshots: 10,
                snapshots_lvm: -1,
                volumes: 10,
                volumes_lvm: -1
            }
        }

'''

from ansible_collections.openstack.cloud.plugins.module_utils.openstack import OpenStackModule


class QuotaModule(OpenStackModule):
    argument_spec = dict(
        name=dict(required=True),
        state=dict(default='present', choices=['absent', 'present']),
        backup_gigabytes=dict(required=False, type='int', default=None),
        backups=dict(required=False, type='int', default=None),
        cores=dict(required=False, type='int', default=None),
        fixed_ips=dict(required=False, type='int', default=None),
        floating_ips=dict(required=False, type='int', default=None, aliases=['compute_floating_ips']),
        floatingip=dict(required=False, type='int', default=None, aliases=['network_floating_ips']),
        gigabytes=dict(required=False, type='int', default=None),
        gigabytes_types=dict(required=False, type='dict', default={}),
        injected_file_size=dict(required=False, type='int', default=None),
        injected_files=dict(required=False, type='int', default=None),
        injected_path_size=dict(required=False, type='int', default=None),
        instances=dict(required=False, type='int', default=None),
        key_pairs=dict(required=False, type='int', default=None, no_log=False),
        loadbalancer=dict(required=False, type='int', default=None),
        metadata_items=dict(required=False, type='int', default=None),
        network=dict(required=False, type='int', default=None),
        per_volume_gigabytes=dict(required=False, type='int', default=None),
        pool=dict(required=False, type='int', default=None),
        port=dict(required=False, type='int', default=None),
        project=dict(required=False, type='int', default=None),
        properties=dict(required=False, type='int', default=None),
        ram=dict(required=False, type='int', default=None),
        rbac_policy=dict(required=False, type='int', default=None),
        router=dict(required=False, type='int', default=None),
        security_group_rule=dict(required=False, type='int', default=None),
        security_group=dict(required=False, type='int', default=None),
        server_group_members=dict(required=False, type='int', default=None),
        server_groups=dict(required=False, type='int', default=None),
        snapshots=dict(required=False, type='int', default=None),
        snapshots_types=dict(required=False, type='dict', default={}),
        subnet=dict(required=False, type='int', default=None),
        subnetpool=dict(required=False, type='int', default=None),
        volumes=dict(required=False, type='int', default=None),
        volumes_types=dict(required=False, type='dict', default={})
    )

    module_kwargs = dict(
        supports_check_mode=True
    )

    def _get_volume_quotas(self, project):
        return self.conn.get_volume_quotas(project)

    def _get_network_quotas(self, project):
        return self.conn.get_network_quotas(project)

    def _get_compute_quotas(self, project):
        return self.conn.get_compute_quotas(project)

    def _get_quotas(self, project):
        quota = {}
        try:
            quota['volume'] = self._get_volume_quotas(project)
        except Exception:
            self.warn("No public endpoint for volumev2 service was found. Ignoring volume quotas.")

        try:
            quota['network'] = self._get_network_quotas(project)
        except Exception:
            self.warn("No public endpoint for network service was found. Ignoring network quotas.")

        quota['compute'] = self._get_compute_quotas(project)

        for quota_type in quota.keys():
            quota[quota_type] = self._scrub_results(quota[quota_type])

        return quota

    def _scrub_results(self, quota):
        filter_attr = [
            'HUMAN_ID',
            'NAME_ATTR',
            'human_id',
            'request_ids',
            'x_openstack_request_ids',
        ]

        for attr in filter_attr:
            if attr in quota:
                del quota[attr]

        return quota

    def _system_state_change_details(self, project_quota_output):
        quota_change_request = {}
        changes_required = False

        for quota_type in project_quota_output.keys():
            for quota_option in project_quota_output[quota_type].keys():
                if quota_option in self.params and self.params[quota_option] is not None:
                    if project_quota_output[quota_type][quota_option] != self.params[quota_option]:
                        changes_required = True

                        if quota_type not in quota_change_request:
                            quota_change_request[quota_type] = {}

                        quota_change_request[quota_type][quota_option] = self.params[quota_option]

        return (changes_required, quota_change_request)

    def _system_state_change(self, project_quota_output):
        """
        Determine if changes are required to the current project quota.

        This is done by comparing the current project_quota_output against
        the desired quota settings set on the module params.
        """

        changes_required, quota_change_request = self._system_state_change_details(
            project_quota_output
        )

        if changes_required:
            return True
        else:
            return False

    def run(self):
        cloud_params = dict(self.params)

        # In order to handle the different volume types we update module params after.
        dynamic_types = [
            'gigabytes_types',
            'snapshots_types',
            'volumes_types',
        ]

        for dynamic_type in dynamic_types:
            for k, v in self.params[dynamic_type].items():
                self.params[k] = int(v)

        # Get current quota values
        project_quota_output = self._get_quotas(cloud_params['name'])
        changes_required = False

        if self.params['state'] == "absent":
            # If a quota state is set to absent we should assume there will be changes.
            # The default quota values are not accessible so we can not determine if
            # no changes will occur or not.
            if self.ansible.check_mode:
                self.exit_json(changed=True)

            # Calling delete_network_quotas when a quota has not been set results
            # in an error, according to the sdk docs it should return the
            # current quota.
            # The following error string is returned:
            # network client call failed: Quota for tenant 69dd91d217e949f1a0b35a4b901741dc could not be found.
            neutron_msg1 = "network client call failed: Quota for tenant"
            neutron_msg2 = "could not be found"

            for quota_type in project_quota_output.keys():
                quota_call = getattr(self.conn, 'delete_%s_quotas' % (quota_type))
                try:
                    quota_call(cloud_params['name'])
                except Exception as e:
                    error_msg = str(e)
                    if error_msg.find(neutron_msg1) > -1 and error_msg.find(neutron_msg2) > -1:
                        pass
                    else:
                        self.fail_json(msg=str(e), extra_data=e.extra_data)

            project_quota_output = self._get_quotas(cloud_params['name'])
            changes_required = True

        elif self.params['state'] == "present":
            if self.ansible.check_mode:
                self.exit_json(changed=self._system_state_change(
                    project_quota_output))

            changes_required, quota_change_request = self._system_state_change_details(
                project_quota_output
            )

            if changes_required:
                for quota_type in quota_change_request.keys():
                    quota_call = getattr(self.conn, 'set_%s_quotas' % (quota_type))
                    quota_call(cloud_params['name'], **quota_change_request[quota_type])

                # Get quota state post changes for validation
                project_quota_update = self._get_quotas(cloud_params['name'])

                if project_quota_output == project_quota_update:
                    self.fail_json(msg='Could not apply quota update')

                project_quota_output = project_quota_update

        self.exit_json(
            changed=changes_required, openstack_quotas=project_quota_output)


def main():
    module = QuotaModule()
    module()


if __name__ == '__main__':
    main()
