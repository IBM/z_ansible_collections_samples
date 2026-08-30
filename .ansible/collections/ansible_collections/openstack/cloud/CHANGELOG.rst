=============================================
Openstack Cloud Ansilbe modules Release Notes
=============================================

.. contents:: Topics


v1.10.0
=======

Release Summary
---------------

Enable logging of openstacksdk activities and warn users about incompatible openstacksdk releases when using inventory plugin

Bugfixes
--------

- Add SDK logging option for openstack ansible collections
- Don't use deprecated distutils from python 3.10
- Ensure openstacksdk compatibility in inventory plugin
- Lowered maximum OpenStack SDK version to 0.98.999 in inventory plugin

Known Issues
------------

- For compatibility with OpenStack SDK >= 0.99.0 use Ansible OpenStack collection 2.0.0 or later which is currently under development.
- Release series 1.x.x of this collection is compatible to OpenStack SDK prior to 0.99.0 only.

v1.9.1
======

Release Summary
---------------

Bugfix in keypair module

Bugfixes
--------

- Do not remove trailing spaces when reading public key in keypair module

Known Issues
------------

- For compatibility with OpenStack SDK >= 0.99.0 use Ansible OpenStack collection 2.0.0 or later which is currently under development.
- Release series 1.x.x of this collection is compatible to OpenStack SDK prior to 0.99.0 only.

v1.9.0
======

Release Summary
---------------

This release will enforce openstacksdk<0.99.0, has a dozen modules refactored and several bugs fixed.

Bugfixes
--------

- Added support for specifying a maximum version of the OpenStack SDK
- Constrain filters in compute_service_info to SDK >= 0.53.0
- Drop username from return values of identity_user_info
- Fix logic in routers_info
- Fixed return value disable{d,s}_reason in compute_service_info module
- Fixed return values in compute_service_info module again
- Follow up to bump of minimum required OpenStack SDK release to SDK 0.36.0 (Train)
- Lowered maximum OpenStack SDK version to 0.98.999
- Move dns zone info to use proxy layer
- Refactored catalog_service module
- Refactored endpoint module
- Refactored host_aggregate module
- Refactored identity_domain_info module
- Refactored identity_group_info module
- Refactored identity_role module
- Refactored identity_role_info module
- Refactored identity_user module
- Refactored identity_user_info module
- Refactored image_info module
- Refactored keypair_info module
- Refactored recordset module
- Refactored role_assignment module
- Set owner in image module
- Support description in sg-rule creation
- Warn users about us breaking backward compatibility

Known Issues
------------

- For compatibility with OpenStack SDK >= 0.99.0 use Ansible OpenStack collection 2.0.0 or later which is currently under development.
- Release series 1.x.x of this collection is compatible to OpenStack SDK prior to 0.99.0 only.

v1.8.0
======

Release Summary
---------------

Subnet pool module and bugfixes

Bugfixes
--------

- Add 'all_projects' to server_action module
- Add subnet pool module
- Bumped minimum required OpenStack SDK release to SDK 0.36.0 (Train)
- Changed compute_flavor_info module to use OpenStack SDK's proxy layer
- Dropped deprecated return values in floating_ip_info and assert remaining fields
- Fix ansible-lint issues for the newest version
- Fix assertion after stack deletion
- Handle aggregate host list set to None
- Reenabled check-import.sh which tests imports to Ansible Galaxy
- Remove old, unsupported parameters from documentation in image_info module
- Router - Remove unneeded 'filter' parameter
- Updated return value docs of compute_service_info module

New Modules
-----------

- openstack.cloud.subnet_pool - Create or Delete subnet pools from OpenStack.

v1.7.2
======

Release Summary
---------------

Bugfixes

Bugfixes
--------

- Fix collection guidelines

v1.7.1
======

Release Summary
---------------

Bugfixes

Minor Changes
-------------

- lb_member - Add monitor_[address,port] parameter

Bugfixes
--------

- openstack_inventory - Fix documentation
- quota - Fix description of volumes_types parameter

v1.7.0
======

Release Summary
---------------

New modules for Ironic and bugfixes

Minor Changes
-------------

- openstack_inventory - Adds use_name variable
- port - Add dns_[name,domain] to the port module
- project - Remove project properties tests and support

Bugfixes
--------

- identity_user_info - Fix identity user lookup with a domain
- keystone_domain - Move identity domain to use proxy layer

New Modules
-----------

- openstack.cloud.baremetal_node_info - Retrieve information about Bare Metal nodes from OpenStack an object.
- openstack.cloud.baremetal_port - Create, Update, Remove ironic ports from OpenStack
- openstack.cloud.baremetal_port_info - Retrieve information about Bare Metal ports from OpenStack an object.

v1.6.0
======

Release Summary
---------------

New modules for RBAC and Nova services

Minor Changes
-------------

- quota - Adds metadata_items parameter

New Modules
-----------

- openstack.cloud.compute_service_info - Retrieve information about one or more OpenStack compute services
- openstack.cloud.neutron_rbac_policies_info - Fetch Neutron policies.
- openstack.cloud.neutron_rbac_policy - Create or delete a Neutron policy to apply a RBAC rule against an object.

v1.5.3
======

Release Summary
---------------

Bugfixes

Bugfixes
--------

- Don't require allowed_address_pairs for port
- server_volume - check specified server is found

v1.5.2
======

Release Summary
---------------

Bugfixes

Minor Changes
-------------

- Add documentation links to README.md
- Don't run functional jobs on galaxy.yml change
- Move CI to use Ansible 2.12 version as main

Bugfixes
--------

- Add client and member listener timeouts for persistence (Ex. SSH)
- Added missing warn() used in cloud.openstack.quota
- Fix issue with same host and group names
- Flavor properties are not deleted on changes and id will stay

v1.5.1
======

Release Summary
---------------

Bugfixes for networking modules

Minor Changes
-------------

- Changed minversion in tox to 3.18.0
- Update IRC server in README

Bugfixes
--------

- Add mandatory requires_ansible version to metadata
- Add protocol listener octavia
- Add support check mode for all info modules
- Allow to attach multiple floating ips to a server
- Only add or remove router interfaces when needed
- Wait for pool to be active and online

v1.5.0
======

Release Summary
---------------

New modules for DNS and FIPs and bugfixes.

Minor Changes
-------------

- Add bindep.txt for ansible-builder
- Add check_mode attribute to OpenstackModule
- Migrating image module from AnsibleModule to OpenStackModule
- Switch KeystoneFederationProtocolInfo module to OpenStackModule
- Switch ProjectAccess module to OpenStackModule
- Switch Quota module to OpenStackModule
- Switch Recordset module to OpenStackModule
- Switch ServerGroup module to OpenStackModule
- Switch ServerMetadata module to OpenStackModule
- Switch Snapshot module to OpenStackModule
- Switch Stack module to OpenStackModule
- Switch auth module to OpenStackModule
- Switch catalog_service module to OpenStackModule
- Switch coe_cluster module to OpenStackModule
- Switch coe_cluster_template module to OpenStackModule
- Switch endpoint module to OpenStackModule
- Switch federation_idp module to OpenStackModule
- Switch federation_idp_info module to OpenStackModule
- Switch federation_mapping module to OpenStackModule
- Switch federation_mapping_info module to OpenStackModule
- Switch federation_protocol module to OpenStackModule
- Switch flavor module to OpenStackModule
- Switch flavor_info module to OpenStackModule
- Switch floating_ip module to OpenStackModule
- Switch group_assignment module to OpenStackModule
- Switch hostaggregate module to OpenStackModule
- Switch identity_domain module to OpenStackModule
- Switch identity_domain_info module to OpenStackModule
- Switch identity_group module to OpenStackModule
- Switch identity_group_info module to OpenStackModule
- Switch identity_role module to OpenStackModule
- Switch identity_user module to OpenStackModule
- Switch lb_listener module to OpenStackModule
- Switch lb_member module to OpenStackModule
- Switch lb_pool module to OpenStackModule
- Switch object module to OpenStackModule
- Switch port module to OpenStackModule
- Switch port_info module to OpenStackModule
- Switch project and project_info module to OpenStackModule
- Switch role_assignment module to OpenStackModule
- Switch user_info module to OpenStackModule
- image - Add support to setting image tags

Bugfixes
--------

- Update checks for validate_certs in openstack_cloud_from_module
- compute_flavor - Fix the idempotent of compute_flavor module
- host_aggregate - Fix host_aggregate to tolerate aggregate.hosts being None
- inventory/openstack - Fix inventory plugin on Ansible 2.11
- port - fix update on empty list of allowed address pairs
- setup.cfg Replace dashes with underscores
- subnet - Only apply necessary changes to subnets
- volume - Fail if referenced source image for a new volume does not exist

New Modules
-----------

- openstack.cloud.address_scope - Create or delete address scopes from OpenStack
- openstack.cloud.dns_zone_info - Getting information about dns zones
- openstack.cloud.floating_ip_info - Get information about floating ips

v1.4.0
======

Release Summary
---------------

New object_container module and bugfixes.

Bugfixes
--------

- Add Octavia job for testing Load Balancer
- Add binding profile to port module
- Add execution environment metadata
- Fix CI for latest ansible-test with no_log
- Fix issues with newest ansible-test 2.11
- Prepare for Ansible 2.11 tests
- add option to exclude legacy groups
- security_group_rule add support ipv6-icmp

New Modules
-----------

- openstack.cloud.object_container - Manage Swift container

v1.3.0
======

Release Summary
---------------

New modules and bugfixes.

Minor Changes
-------------

- Fix some typos in readme
- Guidelines Fix links and formatting
- baremetal_node - Add support for new features
- baremetal_node - ironic deprecate sub-options of driver_info
- baremetal_node - ironic stop putting meaningless values to properties
- image_info - Migrating image_info module from AnsibleModule to OpenStackModule
- recordset -  Update recordset docu
- server - Allow description field to be set with os_server
- server_action - Added shelve and unshelve as new server actions

Bugfixes
--------

- port - Fixed check for None in os_port
- project - Fix setting custom property on os_project
- security_group_rule - Remove protocols choice in security rules
- volume_info - Fix volume_info result for SDK < 0.19

New Modules
-----------

- openstack.cloud.identity_role_info - Retrieve information about Openstack Identity roles.
- openstack.cloud.keypair_info - Retrieve information about Openstack key pairs.
- openstack.cloud.security_group_info - Retrieve information about Openstack Security Groups.
- openstack.cloud.security_group_rule_info - Retrieve information about Openstack Security Group rules.
- openstack.cloud.stack_info - Retrieve information about Openstack Heat stacks.

v1.2.1
======

Release Summary
---------------

Porting modules to new OpenstackModule class and fixes.

Minor Changes
-------------

- dns_zone - Migrating dns_zone from AnsibleModule to OpenStackModule
- dns_zone, recordset - Enable update for recordset and add tests for dns and recordset module
- endpoint - Do not fail when endpoint state is absent
- ironic - Refactor ironic authentication into a new module_utils module
- loadbalancer - Refactor loadbalancer module
- network - Migrating network from AnsibleModule to OpenStackModule
- networks_info - Migrating networks_info from AnsibleModule to OpenStackModule
- openstack - Add galaxy.yml to support install from git
- openstack - Fix docs-args mismatch in modules
- openstack - OpenStackModule Support defining a minimum version of the SDK
- router - Migrating routers from AnsibleModule to OpenStackModule
- routers_info - Added deprecated_names for router_info module
- routers_info - Migrating routers_info from AnsibleModule to OpenStackModule
- security_group.py - Migrating security_group from AnsibleModule to OpenStackModule
- security_group_rule - Refactor TCP/UDP port check
- server.py - Improve "server" module with OpenstackModule class
- server_volume - Migrating server_volume from AnsibleModule to OpenStackModule
- subnet - Fix subnets update and idempotency
- subnet - Migrating subnet module from AnsibleModule to OpenStackModule
- subnets_info - Migrating subnets_info from AnsibleModule to OpenStackModule
- volume.py - Migrating volume from AnsibleModule to OpenStackModule
- volume_info - Fix volume_info arguments for SDK 0.19

v1.2.0
======

Release Summary
---------------

New volume backup modules.

Minor Changes
-------------

- lb_health_monitor - Make it possible to create a health monitor to a pool

New Modules
-----------

- openstack.cloud.volume_backup module - Add/Delete Openstack volumes backup.
- openstack.cloud.volume_backup_info module - Retrieve information about Openstack volume backups.
- openstack.cloud.volume_snapshot_info module - Retrieve information about Openstack volume snapshots.

v1.1.0
======

Release Summary
---------------

Starting redesign modules and bugfixes.

Minor Changes
-------------

- A basic module subclass was introduced and a few modules moved to inherit from it.
- Add more useful information from exception
- Added pip installation option for collection.
- Added template for generation of artibtrary module.
- baremetal modules - Do not require ironic_url if cloud or auth.endpoint is provided
- inventory_openstack - Add openstack logger and Ansible display utility
- loadbalancer - Add support for setting the Flavor when creating a load balancer

Bugfixes
--------

- Fix non existing attribuites in SDK exception
- security_group_rule - Don't pass tenant_id for remote group

New Modules
-----------

- openstack.cloud.volume_info - Retrieve information about Openstack volumes.

v1.0.1
======

Release Summary
---------------

Bugfix for server_info

Bugfixes
--------

- server_info - Fix broken server_info module and add tests

v1.0.0
======

Release Summary
---------------

Initial release of collection.

Minor Changes
-------------

- Renaming all modules and removing "os" prefix from names.
- baremetal_node_action - Support json type for the ironic_node config_drive parameter
- config - Update os_client_config to use openstacksdk
- host_aggregate - Add support for not 'purging' missing hosts
- project - Add properties for os_project
- server_action - pass imageRef to rebuild
- subnet - Updated allocation pool checks

Bugfixes
--------

- baremetal_node - Correct parameter name
- coe_cluster - Retrive id/uuid correctly
- federation_mapping - Fixup some minor nits found in followup reviews
- inventory_openstack - Fix constructed compose
- network - Bump minimum openstacksdk version when using os_network/dns_domain
- role_assignment - Fix os_user_role for groups in multidomain context
- role_assignment - Fix os_user_role issue to grant a role in a domain

New Modules
-----------

- openstack.cloud.federation_idp - Add support for Keystone Identity Providers
- openstack.cloud.federation_idp_info - Add support for fetching the information about federation IDPs
- openstack.cloud.federation_mapping - Add support for Keystone mappings
- openstack.cloud.federation_mapping_info - Add support for fetching the information about Keystone mappings
- openstack.cloud.keystone_federation_protocol - Add support for Keystone federation Protocols
- openstack.cloud.keystone_federation_protocol_info - Add support for getting information about Keystone federation Protocols
- openstack.cloud.routers_info - Retrieve information about one or more OpenStack routers.
