==============================
ibm.ibm_zos_cics Release Notes
==============================

.. contents:: Topics


v2.1.0
======

Release Summary
---------------

General Availability of CICS provisioning modules. You can use these Ansible modules to create automation tasks that provision or deprovision, and start or stop a CICS region. Sample playbooks show you how to do this with the latest version of the Ansible IBM z/OS CICS collection. All modules were initially released with Version 1.1.0-beta as noted below. Subsequent Version 1.1.0-beta releases may include enhancements and bugfixes for these modules. Refer to the What's new of Version 1.1.0-beta releases for details.
This release replaces all the previous 1.1.0-beta* releases.
You can use the following modules for provisioning and managing CICS TS data sets
``aux_temp_storage`` for the CICS auxiliary temporary storage data set. This module was initially released as ``auxiliary_temp`` with Version 1.1.0-beta.4. The module is changed to ``aux_temp_storage`` in Version 2.1.0.
``aux_trace`` for the CICS auxiliary trace data sets. This module was initially released as ``trace`` with Version 1.1.0-beta.4. The module is changed to ``aux_trace`` in Version 2.1.0.
``csd`` for the CICS system definition data set. This module was initially released with Version 1.1.0-beta.4.
``global_catalog`` for the CICS global catalog data set. This module was initially released with Version 1.1.0-beta.4.
``local_request_queue`` for the CICS local request queue data set. This module was initially released with Version 1.1.0-beta.3.
``td_intrapartition`` for the CICS transient data intrapartition data set. This module was initially released as ``intrapartition`` with Version 1.1.0-beta.4. The module is changed to ``td_intrapartition`` in Version 2.1.0.
``transaction_dump`` for the CICS transaction dump data sets. This module was initially released with Version 1.1.0-beta.4.
You can use the following modules for CICS startup and shutdown operations
``region_jcl`` - Create a CICS startup JCL data set. This module replaces ``start_cics``, which was released with Version 1.1.0-beta.5. ``region_jcl`` is significantly different from ``start_cics`` in function. ``region_jcl`` creates a data set that contains the startup JCL, but doesn't perform the actual startup processing. ``region_jcl`` also supports definition and allocation of user data sets with the ``user_data_sets`` parameter.
``stop_region`` - Stop a CICS region. This module was initially released as ``stop_cics`` with Version 1.1.0-beta.5. The module is changed to ``stop_region`` in Version 2.1.0. In Version 2.1.0, ``stop_region`` supports a new input parameter, ``job_name`` so that you can use the job name, which is typically the CICS's APPLID, to identify a running CICS region.
The group name for the CICS provisioning modules is ``region``. However, in the Version 1.1.0-beta releases, the group name was ``region_group``.
CICS provisioning modules provide support for all in-service CICS TS releases including the latest CICS TS 6.2.

Deprecated Features
-------------------

- The group name for the CMCI modules is changed to ``cmci`` instead of ``cmci_group``. ``cmci_group`` is deprecated.

New Modules
-----------

- ibm.ibm_zos_cics.aux_temp_storage - Create and remove the CICS auxiliary temporary storage data set
- ibm.ibm_zos_cics.aux_trace - Allocate auxiliary trace data sets
- ibm.ibm_zos_cics.csd - Create, remove, and manage the CICS CSD
- ibm.ibm_zos_cics.global_catalog - Create, remove, and manage the CICS global catalog
- ibm.ibm_zos_cics.local_catalog - Create, remove, and manage the CICS local catalog
- ibm.ibm_zos_cics.local_request_queue - Create and remove the CICS local request queue
- ibm.ibm_zos_cics.region_jcl - Create CICS startup JCL data set
- ibm.ibm_zos_cics.stop_region - Stop a CICS region
- ibm.ibm_zos_cics.td_intrapartition - Create and remove the CICS transient data intrapartition data set
- ibm.ibm_zos_cics.transaction_dump - Allocate transaction dump data sets

v2.0.0
======

Release Summary
---------------

Removed support for Python 2.7

Breaking Changes / Porting Guide
--------------------------------

- Python 2.7 is no longer supported as the managed node runtime

v1.0.6
======

Release Summary
---------------

This release contains one bug fix

Bugfixes
--------

- Allows CPSM Scope and Context to contain the following special characters '$', '@', and '#'

v1.0.5
======

Release Summary
---------------

This release contains one bug fix

Bugfixes
--------

- Missing requirements.txt - requirements.txt was not included in the built collection. Fix removes this from the build_ignore section of the galaxy.yml.

v1.0.4
======

Release Summary
---------------

This release contains a number of new features and bug fixes.

Minor Changes
-------------

- Provide variables for all modules in one go using Ansible's `group module defaults <https://docs.ansible.com/ansible/2.8/user_guide/playbooks_module_defaults.html#module-defaults-groups>`_ support. The group name for the CMCI modules is ``cmci_group``.

Bugfixes
--------

- cmci_get - prevent ``cmci_get`` from failing if no records are found via the ``fail_on_nodata`` option. The default value is ``true`` if not specified.

v1.0.3
======

Release Summary
---------------

This release contains a number of new features and bug fixes.

Minor Changes
-------------

- Added support for CMCI feedback on failed CMCI requests.
- Updated timeout support on requests to be configurable via the timeout option. The default value is 30 seconds if not specified.

Bugfixes
--------

- Improve sanitisation and validation of parameters.

v1.0.1
======

Release Summary
---------------

Fix some documentation issues on Hub, and include some missing documentation about requirements.

v1.0.0
======

Release Summary
---------------

Initial release of the IBM® z/OS® CICS® collection, also referred to as ibm_zos_cics, which is part of the broader offering Red Hat® Ansible® Certified Content for IBM Z®.

This collection can manage CICS and CICSPlex® SM resources and definitions by calling the CMCI REST API, which can be configured in a CICSplex or in a stand-alone region.

New Modules
-----------

- ibm.ibm_zos_cics.cmci_action - Perform actions on CICS and CICSPlex SM resources
- ibm.ibm_zos_cics.cmci_create - Create CICS and CICSPlex SM definitions
- ibm.ibm_zos_cics.cmci_delete - Delete CICS and CICSPlex SM resources
- ibm.ibm_zos_cics.cmci_get - Query CICS and CICSPlex SM resources and definitions
- ibm.ibm_zos_cics.cmci_update - Update CICS and CICSPlex resources and definitions
