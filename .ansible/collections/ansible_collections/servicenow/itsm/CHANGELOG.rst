=============================
servicenow.itsm Release Notes
=============================

.. contents:: Topics

v2.7.0
======

Release Summary
---------------

Add optional aggregation feature to inventory; add ServiceNow Xanadu to integration test matrix

Minor Changes
-------------

- inventory - allow inventory to aggregate multiple hostvars for the same host. (https://github.com/ansible-collections/servicenow.itsm/pull/408)
- tests - Add ServiceNow Xanadu release to test matrix

v2.6.3
======

Release Summary
---------------

Fix docs issue with 2.6.2 release

v2.6.2
======

Release Summary
---------------

Fix issue with service_catalog endpoint and remove SNOW Tokyo from test matrix

Deprecated Features
-------------------

- tests - Drop sanity test override matrix, as the inherited job now has the correct excludes
- tests - Drop testing of Tokyo, as it is no longer supported by ServiceNow

Bugfixes
--------

- Correct submit_order endpoint for issue

v2.6.1
======

Release Summary
---------------

Fix incorrect documentation shipped with 2.6.0

Bugfixes
--------

- docs - Documentation generated for 2.6.0 was incorrect due to maintainer error. This updates the documentation to be correct and consistent.

v2.6.0
======

Release Summary
---------------

Introduce service_catalog modules; fix inventory crash bug and improve performance by handling duplicate records better

Minor Changes
-------------

- Added check for records(sys_id) that are already processed with reference records
- Raise Ansible runtime version to 2.15.0 in accordance with Ansible Lifecycle policy. This implies dropping Python 3.9 from the test matrix as well.
- ServiceNow returns duplicated records causing error at line referenced.pop("sys_id")
- Update authors in galaxy.yml

Bugfixes
--------

- now - Fix crash of inventory when query is present (https://github.com/ansible-collections/servicenow.itsm/issues/361).

New Modules
-----------

- servicenow.itsm.service_catalog - Manage ServiceNow service catalog cart
- servicenow.itsm.service_catalog_info - List ServiceNow service catalogs along with categories and items

v2.5.0
======

Release Summary
---------------

Introduce generic API client, test against all current releases of ServiceNow, and introduce support for Event-Driven Ansible Notification Service (aka EDA NS) application

Minor Changes
-------------

- Added option to allow changing sysparm_limit for table query (https://github.com/ansible-collections/servicenow.itsm/pull/309).
- Included integration tests and instances targeting the following ServiceNow releases: Washington, Vancouver, Utah, Tokyo
- api - allow `api` module to make request outside `Table API` namespace(https://github.com/ansible-collections/servicenow.itsm/pull/314).
- api_info - allow `api_info` module to make request outside `Table API` namespace(https://github.com/ansible-collections/servicenow.itsm/pull/314).
- change_request - allow change_request_mapping for category parameter (https://github.com/ansible-collections/servicenow.itsm/issues/266).
- client - allow user to pass a `object_hook` function to rest client for custom decoding of the json response(https://github.com/ansible-collections/servicenow.itsm/pull/316).
- configuration_item_relations - add module to add and remove relations between configuration items.
- configuration_item_relations_info - add module retrieve relations of a configuration item.
- now - add cache support for the inventory plugin (https://github.com/ansible-collections/servicenow.itsm/pull/315).
- now.py - replace "." in reference field column name to "_" in host variable

Bugfixes
--------

- now - Fix crash when SN_TIMEOUT is set because is it passed as string instead of a number (https://github.com/ansible-collections/servicenow.itsm/pull/348).

New Modules
-----------

- servicenow.itsm.configuration_item_relations - Manage ServiceNow relations between configuration items
- servicenow.itsm.configuration_item_relations_info - Retreive ServiceNow relations of configuration items

v2.4.0
======

Minor Changes
-------------

- Updated release script for servicenow collection.
- api - added custom headers and api path to the given request (https://github.com/ansible-collections/servicenow.itsm/pull/239).
- use get_record_by_sys_id instead of get_record in methods update, delete (https://github.com/ansible-collections/servicenow.itsm/pull/307).

Bugfixes
--------

- change_request - allow query assignment_group by sys_id (https://github.com/ansible-collections/servicenow.itsm/issues/295)
- change_request_task - allow query assignment_group by sys_id (https://github.com/ansible-collections/servicenow.itsm/issues/295)
- change_request_task - remove duplicate option 'testing' from 'type' argument_spec.
- configuration_item_info - allow user to specify limited return fields for the specified configuration item (https://github.com/ansible-collections/servicenow.itsm/pull/208).
- incident - allow incident_mapping for close_code parameter.
- now - added missing SN_SYSPARM_QUERY environment variable (https://github.com/ansible-collections/servicenow.itsm/issues/293).
- table_client - Fix 'KeyError' exception when fetching records by sys_id and add `must_have` arguments (https://github.com/ansible-collections/servicenow.itsm/pull/306)

v2.3.0
======

Release Summary
---------------

This is the minor release of the ``servicenow.itsm`` collection.
This changelog contains all changes to the modules in this collection that
have been added after the release of ``servicenow.itsm`` 2.2.0.

Minor Changes
-------------

- Add validate_certs option to instance (https://github.com/ansible-collections/servicenow.itsm/pull/264).
- Added option to pass OAuth2 access token previously obtained from ServiceNow (https://github.com/ansible-collections/servicenow.itsm/pull/272).

Bugfixes
--------

- Fix issue with attachment_upload module not working properly (https://github.com/ansible-collections/servicenow.itsm/pull/260).
- now - use correct environment variable for SN_CLIENT_SECRET (https://github.com/ansible-collections/servicenow.itsm/issues/261).

v2.2.0
======

Release Summary
---------------

This is the minor release of the ``servicenow.itsm`` collection.
This changelog contains all changes to the modules in this collection that
have been added after the release of ``servicenow.itsm`` 2.1.0.

Minor Changes
-------------

- Added attachment_upload module (https://github.com/ansible-collections/servicenow.itsm/pull/248).

New Modules
-----------

- servicenow.itsm.attachment_upload - Upload attachment to the selected table

v2.1.0
======

Release Summary
---------------

This is the minor release of the ``servicenow.itsm`` collection.
This changelog contains all changes to the modules in this collection that
have been added after the release of ``servicenow.itsm`` 2.0.0.

Minor Changes
-------------

- api - Added parameter query_params to api module (https://github.com/ansible-collections/servicenow.itsm/pull/225).
- inventory plugin - Plugin now supports mapping of reference fields inside 'compose' block.

Bugfixes
--------

- inventory plugin - sysparm_query attribute is taken into account.
- mapping - When creating custom mapping, one can list unknown fields and map them to values. Before the fix there was a bug, where one could only rename fields inside mapping.

v2.0.0
======

Release Summary
---------------

This is the major release of the ``servicenow.itsm`` collection.

Minor Changes
-------------

- Attachment integration tests - Add missing register variables (https://github.com/ansible-collections/servicenow.itsm/pull/194)
- TableClient - Remove hardcoded value of sysparm_exclude_reference_link when querying on table api.
- \*_info modules - Added additional module parameter sysparm_display_value to all info modules, which, if set to either true or all, enables the user to see the values of sys_tags.
- \*_info modules - Added field sysparm_query, which represents an encoded query string used to filter the results as an alternative to C(query) (https://github.com/ansible-collections/servicenow.itsm/pull/190).
- api - Added module api, which essentially codifies the ServiceNow REST API explorer in Ansible-native way for POST, PATCH and DELETE operations.
- api - Enhanced api module with template processing capabilities as an alternative to its data parameter for creating or updating a resource (https://github.com/ansible-collections/servicenow.itsm/pull/201).
- api_info - Added module api_info, which essentially codifies the ServiceNow REST API explorer in Ansible-native way for retrieving records (GET operations).
- attachment integration tests - Adapt integration tests for attachment module due to changes on PR 192 (https://github.com/ansible-collections/servicenow.itsm/pull/193)
- configuration_batch_item - now returns result instead only if something was changed or not.
- configuration_item_info - Added option name to simplify queries based on that parameter.
- module_utils/attachments.py - Add ``get_attachment`` and ``save_attachment`` (https://github.com/ansible-collections/servicenow.itsm/pull/186).
- module_utils/problem.py - Added problem client for requesting problem state updates from the I(API for Red Hat Ansible Automation Platform Certified Content Collection) Scripted REST API Service.
- module_utils/util.py - Added optional Boolean parameter C(implicit) to C(get_mapper) function to provide default values for missing keys in the mapping.
- modules/problem.py - Added module parameters validation to match the mapping specification.
- modules/problem.py - Added optional module parameter C(base_api_path) to control the URI prefix of the endpoint exposed by the I(API for Red Hat Ansible Automation Platform Certified Content Collection) Scripted REST API Service.
- now - Added field sysparm_query, which represents an encoded query string used to filter the results as an alternative to C(query) (https://github.com/ansible-collections/servicenow.itsm/pull/190).
- test_api - Remove unused import which caused sanity error. (https://github.com/ansible-collections/servicenow.itsm/pull/204)

Breaking Changes / Porting Guide
--------------------------------

- configuration_item - Added name as a unique identifier. This means that the idempotence is based on name, while previously there was no idempotence (except for sys_id). When state=present if a configuration item with given name does not exist, the item is created. If it already exists, it is updated. (https://github.com/ansible-collections/servicenow.itsm/pull/192)
- plugins/inventory/now.py - Removed parameters ``ansible_host_source``, ``named_groups`` and ``group_by`` (https://github.com/ansible-collections/servicenow.itsm/pull/213).

Bugfixes
--------

- modules/problem.py - Uses I(API for Red Hat Ansible Automation Platform Certified Content Collection) Scripted REST API Service for transitioning problem state in case of Table API fails.

New Modules
-----------

- servicenow.itsm.api - Manage ServiceNow POST, PATCH and DELETE requests
- servicenow.itsm.api_info - Manage ServiceNow GET requests
- servicenow.itsm.attachment - a module that users can use to download attachment using sys_id

v1.4.0
======

Release Summary
---------------

This is the minor release of the ``servicenow.itsm`` collection.

Minor Changes
-------------

- added ignore.txt for Ansible 2.14 devel branch.
- now - Updated documents to make clear how AND OR queries operate.
- now - fix mapped attributes in now modules.
- now - fix validate-modules errors in now inventory plugins.
- now - inventory plugin updated to support ``refresh_token`` and ``grant_type`` (https://github.com/ansible-collections/servicenow.itsm/issues/168).

v1.3.3
======

Release Summary
---------------

This is the patch release of the ``servicenow.itsm`` collection.

v1.3.2
======

Release Summary
---------------

This is the patch release of the ``servicenow.itsm`` collection.

v1.3.1
======

Release Summary
---------------

This is the patch release of the ``servicenow.itsm`` collection.

v1.3.0
======

Release Summary
---------------

This is the minor release of the ``servicenow.itsm`` collection.
This changelog contains all changes to the modules in this collection that
have been added after the release of ``servicenow.itsm`` 1.2.0.

Minor Changes
-------------

- client - Changed the base URL path of the HTTP client for all requests from `/api/now` to `/`
- now - Enhance inventory with additional groups from CMDB relations (https://github.com/ansible-collections/servicenow.itsm/issues/108).
- table.py - add change_request and configuration item search options.

New Modules
-----------

- servicenow.itsm.change_request_task - Manage ServiceNow change request tasks
- servicenow.itsm.change_request_task_info - List ServiceNow change request tasks
- servicenow.itsm.problem_task - Manage ServiceNow problem tasks
- servicenow.itsm.problem_task_info - List ServiceNow problem tasks

v1.2.0
======

Release Summary
---------------

This is the minor release of the ``servicenow.itsm`` collection.
This changelog contains all changes to the modules in this collection that
have been added after the release of ``servicenow.itsm`` 1.1.0.

Minor Changes
-------------

- attachments - Add a client for attachment management. Add support for attachments in change_request, configuration_item, incident and problem modules, including their info counterparts. (https://github.com/ansible-collections/servicenow.itsm/pull/91)

Deprecated Features
-------------------

- now inventory plugin - deprecate non constructed features (https://github.com/ansible-collections/servicenow.itsm/pull/97).

Bugfixes
--------

- change_request - validates on_hold with its respective field instead of a non-existent "on_hold" state when requiring a hold_reason (https://github.com/ansible-collections/servicenow.itsm/pull/86).
- client - Lowercase all header dict keys on Response initialization for better consistency across Python versions. Fix tests and table client accordingly (https://github.com/ansible-collections/servicenow.itsm/pull/98).
- now - add support for constructed feature in inventory plugin (https://github.com/ansible-collections/servicenow.itsm/issues/35).

New Modules
-----------

- servicenow.itsm.configuration_item_batch - Manage ServiceNow configuration items in batch mode

v1.1.0
======

Release Summary
---------------

v1.1.0 release for ServiceNow ITSM collection.

Minor Changes
-------------

- Added new query module utility to filter results in info modules (https://github.com/ansible-collections/servicenow.itsm/issues/66).
- Added query parameter to change request info module
- Added query parameter to configuration item info module
- Added query parameter to incident info module
- Added query parameter to problem info module
- Added support for ``refresh_token`` in login mechanism (https://github.com/ansible-collections/servicenow.itsm/issues/63).

Bugfixes
--------

- now - check instance host value before making REST call from the Client (https://github.com/ansible-collections/servicenow.itsm/pull/79).

v1.0.0
======

New Plugins
-----------

Inventory
~~~~~~~~~

- servicenow.itsm.now - Inventory source for ServiceNow table records.

New Modules
-----------

- servicenow.itsm.change_request - Manage ServiceNow change requests
- servicenow.itsm.change_request_info - List ServiceNow change requests
- servicenow.itsm.configuration_item - Manage ServiceNow configuration items
- servicenow.itsm.configuration_item_info - List ServiceNow configuration item
- servicenow.itsm.incident - Manage ServiceNow incidents
- servicenow.itsm.incident_info - List ServiceNow incidents
- servicenow.itsm.problem - Manage ServiceNow problems
- servicenow.itsm.problem_info - List ServiceNow problems
