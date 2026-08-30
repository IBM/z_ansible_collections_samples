===========================
Ibm.Ibm_Zosmf Release Notes
===========================

.. contents:: Topics

v1.3.0
======


New Roles
---------

- ibm.ibm_zosmf.zmf_swmgmt_identify_missing_critical_updates - Role identifies missing critical updates
- ibm.ibm_zosmf.zmf_swmgmt_identify_missing_fixcat_updates - Role identifies missing fixcat updates
- ibm.ibm_zosmf.zmf_swmgmt_search_software_updates - Role searches for software updates

v1.2.1
======

Major Changes
-------------

- Removed dependency on Requests library for Python on the control node.

v1.2.0
======

Major Changes
-------------

- Updated module `zmf_sca` which adds support for the security requirements provision.

New Modules
-----------

- ibm.ibm_zosmf.zmf_sca - Automate z/OS security requirements validation and provision

v1.1.0
======

New Modules
-----------

- ibm.ibm_zosmf.zmf_sca - Automate z/OS security requirements validation and provision

New Roles
---------

- ibm.ibm_zosmf.zmf_cpm_create_software_instance - Role creates a z/OS software instance
- ibm.ibm_zosmf.zmf_cpm_get_software_instance - Role get specific z/OS software instance
- ibm.ibm_zosmf.zmf_cpm_list_software_templates - Role lists all published z/OS software templates

v1.0.1
======

New Modules
-----------

- ibm.ibm_zosmf.zmf_authenticate - Authenticate with z/OSMF server
- ibm.ibm_zosmf.zmf_workflow - Operate z/OS workflows

New Roles
---------

- ibm.ibm_zosmf.zmf_cpm_manage_software_instance - Role manages a provisioned z/OS software instance
- ibm.ibm_zosmf.zmf_cpm_provision_software_service - Role provisions a z/OS software service
- ibm.ibm_zosmf.zmf_cpm_remove_software_instance - Role removes a z/OS software instance
- ibm.ibm_zosmf.zmf_workflow_complete - Role completes a z/OS workflow
