================================
ibm.ibm\_zos\_core Release Notes
================================

.. contents:: Topics

v2.0.0
======

Release Summary
---------------

Release Date: '2026-05-27'
This changelog describes all changes made to the modules and plugins included
in this collection. The release date is the date the changelog is created.
For additional details such as required dependencies and availability review
the collections `release notes <https://ibm.github.io/z_ansible_collections_doc/ibm_zos_core/docs/source/release_notes.html>`__

Minor Changes
-------------

- gather_diagnostics - Adds role that gathers comprehensive diagnostic information from both the Ansible control node and target z/OS managed nodes. The role collects system configuration, environment variables, installed packages, ZOAU and Python details, operator command output, and SSH configuration to aid in troubleshooting and support scenarios. (https://github.com/ansible-collections/ibm_zos_core/issues/2168).
- playbook_upgrade_validator - adds a role that scans playbooks in a directory and reports required actions to migrate from v1.x.x to v2.0.0. (https://github.com/ansible-collections/ibm_zos_core/issues/2167)
- zos_backup_restore - Adds ``access`` to specify how the module will access data sets and z/OS UNIX files when performing a backup or restore operation. Adds ``share`` to specify the module allow data set read access to other programs while backing up or restoring. Adds ``auth``  allows you to act as an administrator, where it will disable checking the current users privileges for z/OS UNIX files, data sets and catalogs. (https://github.com/ansible-collections/ibm_zos_core/pull/2320)
- zos_backup_restore - Adds ``compress`` option to enable compression of partitioned data sets using hardware compression if available. Adds ``terse`` option to modify the behavior of executing an AMATERSE step to compress the temporary data set for the backup. (https://github.com/ansible-collections/ibm_zos_core/pull/2259)
- zos_backup_restore - Adds ``disable_automatic_class`` to specify that automatic class selection routines will not be used to determine classes for provide list. Adds ``disable_automatic_storage_class`` to specify the automatic class selection routines will not be used to determine the source data set storage class. Adds ``disable_automatic_management_class``  to specify the automatic class selection routines will not be used to determine the source data set management class. (https://github.com/ansible-collections/ibm_zos_core/pull/2276)
- zos_backup_restore - Adds ``index`` that allows for the backup and restore of all the associated alternate index (AIX®) clusters and paths of a VSAM. (https://github.com/ansible-collections/ibm_zos_core/pull/2259)
- zos_blockinfile - Adds alias ``after`` for option ``insertafter`` and alias ``before`` for option ``insertbefore``. Adds new return values ``stdout_lines`` and ``stderr_lines`` that returns any outstanding stdout or stderr message split in lines. (https://github.com/ansible-collections/ibm_zos_core/pull/2243).
- zos_data_set - Adds `noscratch` option to allow uncataloging a data set without deleting it from the volume's VTOC. (https://github.com/ansible-collections/ibm_zos_core/pull/2210)
- zos_data_set - Adds return value ``data_sets`` which contains the attributes of all data sets created. (https://github.com/ansible-collections/ibm_zos_core/pull/2206)
- zos_data_set - Enhances error messages when creating a Generation Data Group fails. (https://github.com/ansible-collections/ibm_zos_core/pull/2254)
- zos_data_set - Enhances error messages when deleting a Generation Data Group fails. (https://github.com/ansible-collections/ibm_zos_core/pull/2266)
- zos_encode - Adds new return value ``encoding`` with ``from`` and ``to`` encoding values used in the operation. (https://github.com/ansible-collections/ibm_zos_core/pull/2228).
- zos_job_output - Adds support to query SYSIN DDs from a job with new option input. (https://github.com/ansible-collections/ibm_zos_core/pull/2215)
- zos_lineinfile - Adds alias ``after`` for option ``insertafter`` and alias ``before`` for option ``insertbefore``. Adds new return values ``stdout_lines`` and ``stderr_lines`` that returns any outstanding stdout or stderr message split in lines. (https://github.com/ansible-collections/ibm_zos_core/pull/2253).
- zos_mount - New return value ``persistent.state`` that returns the entry state in the persistent data set. New return value ``persistent.name`` that returns the persistent data set name. (https://github.com/ansible-collections/ibm_zos_core/pull/2272).
- zos_operator - New return value ``stdout`` contains the command stdout in raw format. New return value ``stderr`` contains the command stderr in raw format. (https://github.com/ansible-collections/ibm_zos_core/pull/2492).
- zos_stat_by_type - Added new filter plugin to filter the output returned by zos_stat depending on the type of resource that was queried. (https://github.com/ansible-collections/ibm_zos_core/issues/2316).
- zos_tso_command - New return value ``stdout`` contains the command stdout in raw format. New return value ``stderr_lines`` that returns any outstanding stdout message split in lines. (https://github.com/ansible-collections/ibm_zos_core/pull/2245).

Breaking Changes / Porting Guide
--------------------------------

- zos_apf - Option ``data_set_name`` is deprecated in favor of ``target``. New return values ``stdout_lines`` and ``stderr_lines`` are now returned. (https://github.com/ansible-collections/ibm_zos_core/pull/2250).
- zos_archive - Option ``format.format_options`` is deprecated in favor of ``format.options``. Option ``format.format_options.name`` is deprecated in favor of ``format.options.type``. Option ``format.format_options.use_adrdssu`` is deprecated in favor of ``format.options.use_adrdssu``. Option ``format.format_options.terse_pack`` is deprecated in favor of ``format.options.spack`` as a new boolean option. (https://github.com/ansible-collections/ibm_zos_core/pull/2256).
- zos_backup_restore - Add new dictionary option ``sms`` to specify how interact with the storage class. Option ``sms_storage_class`` is deprecated in favor of ``storage_class`` and is under new option ``sms`` being sms.storage_class. Option ``sms_management_class`` is deprecated in favor of ``management_class`` and is under new option ``sms`` being sms.management_class. (https://github.com/ansible-collections/ibm_zos_core/pull/2276)
- zos_backup_restore - Add new module option ``output`` to specify how backup will be restored. Option ``hlq`` is deprecated in favor of ``output.hlq``. (https://github.com/ansible-collections/ibm_zos_core/pull/2412)
- zos_copy - Option ``force_lock`` is deprecated in favor of ``force`` for using datasets on dsp=shr. Option ``force`` is deprecated in favor of ``replace`` for cases you want to replace a dest already exists. Option ``executable`` is deprecated in favor of ``is_executable``. Now return value ``dest_created`` is always return with bool value. (https://github.com/ansible-collections/ibm_zos_core/pull/2232).
- zos_copy - Option ``is_binary`` is replaced in favor of ``binary``. Option ``is_executable`` is renamed to ``executable``. (https://github.com/ansible-collections/ibm_zos_core/pull/2290).
- zos_data_set - Return value ``message`` is deprecated in favor of ``msg``. (https://github.com/ansible-collections/ibm_zos_core/pull/2361).
- zos_fetch - Option ``is_binary`` is replaced in favor of ``binary``. (https://github.com/ansible-collections/ibm_zos_core/pull/2290).
- zos_fetch - Return value ``file`` is replaced by ``src``. Return value ``note`` is deprecated, the messages thrown in ``note`` are now returned in ``msg``. (https://github.com/ansible-collections/ibm_zos_core/pull/2231).
- zos_find - Option value ``pds_pattern`` is deprecated. Previously, excluding members required adding pds_patterns. The updated behavior now allows excluding members by placing them inside parentheses. (https://github.com/ansible-collections/ibm_zos_core/pull/2261).
- zos_job_output - Option name ``ddname`` is substituted for ``dd_name``, but ``ddname`` is kept as an alias. Return value ``changed`` is always returned and return value ``steps`` are no longer under ``ret_code`` but is now included under `jobs`. Return value ``ddnames`` is replaced by ``dds`` and value ``ddname`` under ``ddnames`` is replaced by ``dd_name``. (https://github.com/ansible-collections/ibm_zos_core/pull/2208).
- zos_job_query - Option ``message_filter`` is deprecated in favor of ``msg_filter``. Option ``message_id`` is deprecated in favor of ``msg_id``. Return value ``message_id`` is deprecated in favor of ``msg_id``. Return value ``message_text`` is deprecated in favor of ``msg_txt``. (https://github.com/ansible-collections/ibm_zos_core/pull/2218).
- zos_job_query - Return field ``message`` is deprecated in favor of ``msg``. Return field ``steps`` is no longer under ``ret_code`` but is now included under ``jobs``. (https://github.com/ansible-collections/ibm_zos_core/pull/2204).
- zos_job_submit - Interface value ``location`` is replace for ``remote_src`` in a bool value and ``wait_time_s`` is replaced for ``wait_time``. Return value ``ddnames`` replaced by ``dds`` and value ``ddname`` under ``ddnames`` is replace by ``dd_name``. Now all values are returned under ``jobs`` except ``changed``. (https://github.com/ansible-collections/ibm_zos_core/pull/2208).
- zos_lineinfile - Removes return value ``return_content`` in favor of ``stdout``. (https://github.com/ansible-collections/ibm_zos_core/pull/2253).
- zos_mount - Option ``persistent.data_store`` is deprecated in favor of ``persistent.name``. Option ``persistent.comment`` is deprecated in favor of ``persistent.marker``. (https://github.com/ansible-collections/ibm_zos_core/pull/2244).
- zos_mount - Return values ``persistent.addDataset`` and ``persistent.delDataset`` are deprecated in favor of ``persistent.name`` and ``persistent.state``. (https://github.com/ansible-collections/ibm_zos_core/pull/2272).
- zos_operator - Option ``wait_time_s`` is being deprecated in favor of ``wait_time``. New option ``time_unit`` is being added to select seconds or centiseconds. New return value ``time_unit`` is being added. Return value ``wait_time_s`` is being deprecated in favor of ``wait_time``. (https://github.com/ansible-collections/ibm_zos_core/pull/2230).
- zos_operator - Return value ``content`` is deprecated in favor of ``stdout_lines`` and ``stderr_lines``. (https://github.com/ansible-collections/ibm_zos_core/pull/2492)
- zos_tso_command - Return value ``content`` is replaced by ``stdout_lines``. Return value ``lines`` is replaced by ``line_count``. (https://github.com/ansible-collections/ibm_zos_core/pull/2245).
- zos_unarchive - Option ``format.format_options`` is deprecated in favor of ``format.options``. Option ``format.format_options.name`` is deprecated in favor of ``format.options.type``. Option ``format.format_options.use_adrdssu`` is deprecated in favor of ``format.options.use_adrdssu``. Return values ``stdout_lines`` and ``stderr_lines`` is added. (https://github.com/ansible-collections/ibm_zos_core/pull/2271).

Bugfixes
--------

- zos_backup_restore - Module documentation stated that default ``space_type`` for a backup was ``m`` but module would use bytes instead. Fix now uses the correct default space type. (https://github.com/ansible-collections/ibm_zos_core/pull/2238).
- zos_backup_restore - Updated documentation to clarify `temp_volume` usage and document known issue where operations fail if `temp_volume` matches source volume or is unspecified. (https://github.com/ansible-collections/ibm_zos_core/pull/2437)
- zos_fetch - Previously, running the module with a missing source could raise an ``UnboundLocalError`` because the variable ``is_member`` was referenced before initialization. The fix now ensures ``is_member`` is initialized before dataset existence checks, allowing the module to fail gracefully with a proper error message when the source does not exist. (https://github.com/ansible-collections/ibm_zos_core/pull/2378).
- zos_mount - Previously, using the persistent parameter caused the module to clear the entire member or data set provided even without a pattern match, leaving it empty despite a successful mount. The fix now ensures content is only deleted when a pattern match is detected, preserving existing configuration. (https://github.com/ansible-collections/ibm_zos_core/pull/2347).
- zos_started_task - System logs could not be fetched due to an internal variable name conflict. Fix renames the internal variable to distinguish the boolean control flag from the system_logs output variable. (https://github.com/ansible-collections/ibm_zos_core/issues/2422)

New Plugins
-----------

Filter
~~~~~~

- ibm.ibm_zos_core.generate_data_set_name - Filter HLQs to generate a new random valid data set name.
- ibm.ibm_zos_core.zos_stat_by_type - Filter returned fields from zos\_stat

New Modules
-----------

- ibm.ibm_zos_core.zos_user - Manage z/OS user and group profiles in RACF

v1.16.0
=======

Release Summary
---------------

Release Date: '2026-03-04'
This changelog describes all changes made to the modules and plugins included
in this collection. The release date is the date the changelog is created.
For additional details such as required dependencies and availability review
the collections `release notes <https://ibm.github.io/z_ansible_collections_doc/ibm_zos_core/docs/source/release_notes.html>`__

Minor Changes
-------------

- zos_apf - Adds new alias ``target`` for ``data_set_name`` and a warning message that it will be deprecated on 2.0.0. (https://github.com/ansible-collections/ibm_zos_core/pull/2353).
- zos_archive - Adds new alias ``adrdssu`` for ``use_adrdssu`` and a warning message that it will be deprecated on 2.0.0. (https://github.com/ansible-collections/ibm_zos_core/pull/2353).
- zos_archive - Adds new alias ``options`` for ``format_options`` and a warning message that it will be deprecated on 2.0.0. (https://github.com/ansible-collections/ibm_zos_core/pull/2353).
- zos_archive - Adds new alias ``type`` for ``name`` and a warning message that it will be deprecated on 2.0.0. (https://github.com/ansible-collections/ibm_zos_core/pull/2353).
- zos_backup_restore - Adds ``compress`` option to enable compression of partitioned data sets using hardware compression if available. (https://github.com/ansible-collections/ibm_zos_core/pull/2260)
- zos_backup_restore - Adds ``terse`` option to modify the behavior of executing an AMATERSE step to compress the temporary data set for the backup. (https://github.com/ansible-collections/ibm_zos_core/pull/2260)
- zos_copy - Adds a better error message that gives user a hint that copy issue can be due to a GDS part of ``src`` GDG is being used by another process. (https://github.com/ansible-collections/ibm_zos_core/pull/2220)
- zos_copy - Adds new alias ``binary`` for ``is_binary`` and warning message that will be deprecated on 2.0.0. (https://github.com/ansible-collections/ibm_zos_core/pull/2353).
- zos_copy - Adds new alias ``replace`` for ``force`` and a warning message that it will be deprecated on 2.0.0. (https://github.com/ansible-collections/ibm_zos_core/pull/2353).
- zos_data_set - Adds `noscratch` option to allow uncataloging a data set without deleting it from the volume's VTOC. (https://github.com/ansible-collections/ibm_zos_core/pull/2202)
- zos_data_set - Enhances error messages when creating a Generation Data Group fails. (https://github.com/ansible-collections/ibm_zos_core/pull/2212)
- zos_data_set - Enhances error messages when deleting a Generation Data Group fails. (https://github.com/ansible-collections/ibm_zos_core/pull/2255)
- zos_data_set - Merged `noscratch` functionality into the `scratch` option. This allows uncataloging a data set without deleting it from the volume's VTOC (https://github.com/ansible-collections/ibm_zos_core/pull/2268).
- zos_fetch - Adds new alias ``binary`` for ``is_binary`` and warning message that will be deprecated on 2.0.0. (https://github.com/ansible-collections/ibm_zos_core/pull/2353).
- zos_job_output - Adds new alias ``dd_name`` for ``ddname`` and warning message that will be alias on 2.0.0. (https://github.com/ansible-collections/ibm_zos_core/pull/2353).
- zos_job_output - Adds support to query SYSIN DDs from a job with new option ``sysin_dd``. (https://github.com/ansible-collections/ibm_zos_core/pull/2207)
- zos_job_submit - Adds new alias ``wait_time`` for ``wait_time_s`` and warning message that will be deprecated on 2.0.0. (https://github.com/ansible-collections/ibm_zos_core/pull/2353).
- zos_job_submit - Adds support for jobs with TYPRUN=JCLHOLD and TYPRUN=HOLD. (https://github.com/ansible-collections/ibm_zos_core/pull/2229).
- zos_mount - Adds new alias ``marker`` for ``comment`` and warning message that will be deprecated on 2.0.0. (https://github.com/ansible-collections/ibm_zos_core/pull/2353).
- zos_mount - Adds new alias ``name`` for ``data_store`` and warning message that will be deprecated on 2.0.0. (https://github.com/ansible-collections/ibm_zos_core/pull/2353).
- zos_mvs_raw - Adds new `raw` option under ``dd_data_set`` that lets the MVS program create datasets with its own DCB attributes without the user having to specify them. (https://github.com/ansible-collections/ibm_zos_core/pull/2341).
- zos_operator - Adds new alias ``wait_time`` for ``wait_time_s`` and warning message that will be deprecated on 2.0.0. (https://github.com/ansible-collections/ibm_zos_core/pull/2353).
- zos_unarchive - Adds new alias ``adrdssu`` for ``use_adrdssu`` and warning message that will be deprecated on 2.0.0. (https://github.com/ansible-collections/ibm_zos_core/pull/2353).
- zos_unarchive - Adds new alias ``options`` for ``format_options`` and warning message that will be deprecated on 2.0.0. (https://github.com/ansible-collections/ibm_zos_core/pull/2353).
- zos_unarchive - Adds new alias ``type`` for ``name`` and warning message that will be deprecated on 2.0.0. (https://github.com/ansible-collections/ibm_zos_core/pull/2353).

New Modules
-----------

- ibm.ibm_zos_core.zos_started_task - Perform operations on started tasks.

v1.15.1
=======

Release Summary
---------------

Release Date: '2025-11-21'
This changelog describes all changes made to the modules and plugins included
in this collection. The release date is the date the changelog is created.
For additional details such as required dependencies and availability review
the collections `release notes <https://ibm.github.io/z_ansible_collections_doc/ibm_zos_core/docs/source/release_notes.html>`__

Bugfixes
--------

- zos_backup_restore - Module documentation stated that default ``space_type`` for a backup was ``m`` but module would use bytes instead. Fix now uses the correct default space type. (https://github.com/ansible-collections/ibm_zos_core/pull/2236).
- zos_copy - When a data set name had the maximum length of 44 characters, a check to see if it is an alias would fail. Fix now solves this issue so that any valid data set name can be used as a source or destination. (https://github.com/ansible-collections/ibm_zos_core/pull/2391)
- zos_replace - Module would always write USS files in UTF-8 encoding regardless if an occurrence was found or not. Fix now writes the file with the provided ``encoding`` value only if an occurrence is found. (https://github.com/ansible-collections/ibm_zos_core/pull/2372).

v1.15.0
=======

Release Summary
---------------

Release Date: '2025-09-30'
This changelog describes all changes made to the modules and plugins included
in this collection. The release date is the date the changelog is created.
For additional details such as required dependencies and availability review
the collections `release notes <https://ibm.github.io/z_ansible_collections_doc/ibm_zos_core/docs/source/release_notes.html>`__

Minor Changes
-------------

- zos_archive - Adds support for encoding before archiving files. (https://github.com/ansible-collections/ibm_zos_core/pull/2081)
- zos_archive - Adds support for reverting the encoding of a source's files after archiving them. (https://github.com/ansible-collections/ibm_zos_core/pull/2192)
- zos_archive - Adds support for skipping encoding in archive module. This allows users to skip encoding for certain files before archiving them. (https://github.com/ansible-collections/ibm_zos_core/pull/2116)
- zos_copy - Added support for british pound character usage in file content and data set names for both source and destination when copying. (https://github.com/ansible-collections/ibm_zos_core/pull/2153)
- zos_copy - Adds new option `identical_gdg_copy` in the module. This allows copying GDG generations from a source base to a destination base while preserving generation data set absolute names when the destination base does not exist prior to the copy. (https://github.com/ansible-collections/ibm_zos_core/pull/2100).
- zos_copy - Adds support of using alias names in src and dest parameters for PS, PDS and PDSE data sets. (https://github.com/ansible-collections/ibm_zos_core/pull/2103)
- zos_fetch - Updated the documentation to correctly state what the default behavior of the module is. (https://github.com/ansible-collections/ibm_zos_core/pull/2047).
- zos_find - Adds functionality to find migrated data sets. - Adds functionality to find different types of data sets at the same time. (https://github.com/ansible-collections/ibm_zos_core/pull/2073).
- zos_job_output - Adds new fields cpu_time, origin_node and execution_node to response. (https://github.com/ansible-collections/ibm_zos_core/pull/2056).
- zos_job_query - Adds new fields cpu_time, origin_node and execution_node to response. (https://github.com/ansible-collections/ibm_zos_core/pull/2056).
- zos_job_submit - Adds new fields cpu_time, origin_node and execution_node to response. (https://github.com/ansible-collections/ibm_zos_core/pull/2056).
- zos_mvs_raw  - Before this addition, you could not put anything in columns 1 or 2, were reserved for JCL processing. Change now allows add reserved_cols option and validate that the module get access to modify dd_content option base on the value, if not retain the previous behavior or work. (https://github.com/ansible-collections/ibm_zos_core/pull/2086)
- zos_mvs_raw - Adds support for volume data definition. (https://github.com/ansible-collections/ibm_zos_core/pull/2194)
- zos_stat - Added support to recall migrated data sets and return its attributes. (https://github.com/ansible-collections/ibm_zos_core/pull/2075)
- zos_stat - Adds new fields that describe the type of the resource that was queried. These new fields are `isfile`, `isdataset`, `isaggregate` and `isgdg`. (https://github.com/ansible-collections/ibm_zos_core/pull/2137)
- zos_stat - Adds support to query data sets using their aliases. (https://github.com/ansible-collections/ibm_zos_core/pull/2061)
- zos_stat - Module now returns whether the resource queried exists on the managed node with the `exists` field inside `stat`. (https://github.com/ansible-collections/ibm_zos_core/pull/2137)
- zos_unarchive - Added encoding support for the unarchive module. This allows users to encode the files after unarchiving them in a perticular encoding. (https://github.com/ansible-collections/ibm_zos_core/pull/2105)

Bugfixes
--------

- zos_backup_restore - Return value `backup_name` was empty upon successful result. Fix now returns `backup_name` populated. (https://github.com/ansible-collections/ibm_zos_core/pull/2040).
- zos_data_set - Attempting to create a data set with the same name on a different volume did not work, nor did it report a failure. The fix now informs the user that if the data set is cataloged on a different volume, it needs to be uncataloged before using the data set module to create a new data set on a different volume. (https://github.com/ansible-collections/ibm_zos_core/pull/2057).
- zos_fetch - Previously, the use of `become` would result in a permissions error while trying to fetch a data set or a member. Fix now allows a user to escalate privileges when fetching resources. (https://github.com/ansible-collections/ibm_zos_core/pull/2079)
- zos_lineinfile - Return values ``return_content`` and ``backup_name`` were not always being returned. Fix now ensure that these values are always present in the module's response. (https://github.com/ansible-collections/ibm_zos_core/pull/2120)
- zos_lineinfile - The module would report a false negative when certain special characters where present in the `line` option. Fix now reports the successful operation. (https://github.com/ansible-collections/ibm_zos_core/pull/2080).
- zos_mount - FSUMF168 return in stderror means that the mount dataset wouldn't resolve. While this shows a catalog or volume issue, it should not impact our search for an existing mount. Added handling to the df call, so that FSUMF168 are ignored. (https://github.com/ansible-collections/ibm_zos_core/pull/2060).

New Modules
-----------

- ibm.ibm_zos_core.zos_replace - Replace all instances of a pattern within a file or data set.

v1.14.1
=======

Release Summary
---------------

Release Date: '2025-07-03'
This changelog describes all changes made to the modules and plugins included
in this collection. The release date is the date the changelog is created.
For additional details such as required dependencies and availability review
the collections `release notes <https://ibm.github.io/z_ansible_collections_doc/ibm_zos_core/docs/source/release_notes.html>`__

Bugfixes
--------

- zos_copy - Previously, if the ansible user was not a superuser copying a file into the managed node resulted in permission denied error. Fix now sets the correct permissions for the ansible user for copying to the remote. (https://github.com/ansible-collections/ibm_zos_core/pull/2196)
- zos_job_submit - Previously, if the ansible user was not a superuser copying a file into the managed node resulted in permission denied error. Fix now sets the correct permissions for the ansible user for copying to the remote. (https://github.com/ansible-collections/ibm_zos_core/pull/2196)
- zos_script - Previously, if the ansible user was not a superuser copying a file into the managed node resulted in permission denied error. Fix now sets the correct permissions for the ansible user for copying to the remote. (https://github.com/ansible-collections/ibm_zos_core/pull/2196)
- zos_unarchive - Previously, if the ansible user was not a superuser copying a file into the managed node resulted in permission denied error. Fix now sets the correct permissions for the ansible user for copying to the remote. (https://github.com/ansible-collections/ibm_zos_core/pull/2196)

v1.14.0
=======

Release Summary
---------------

Release Date: '2025-06-30'
This changelog describes all changes made to the modules and plugins included
in this collection. The release date is the date the changelog is created.
For additional details such as required dependencies and availability review
the collections `release notes <https://ibm.github.io/z_ansible_collections_doc/ibm_zos_core/docs/source/release_notes.html>`__

Minor Changes
-------------

- zos_copy - Adds ``large`` as a choice for ``type`` in ``dest_data_set``. (https://github.com/ansible-collections/ibm_zos_core/pull/1938)
- zos_copy - Adds logging of Jinja rendered template content when `use_template` is true and verbosity level `-vvv` is used. (https://github.com/ansible-collections/ibm_zos_core/pull/1968).
- zos_copy - Adds support for copying in asynchronous mode inside playbooks. (https://github.com/ansible-collections/ibm_zos_core/pull/1953).
- zos_copy - Removes the need to allow READ access to MVS.MCSOPER.ZOAU to execute the module by changing how the module checks if a data set is locked. (https://github.com/ansible-collections/ibm_zos_core/pull/1917).
- zos_job_output - Add execution_time return value in the modules response. (https://github.com/ansible-collections/ibm_zos_core/pull/1891).
- zos_job_query - Add execution_time return value in the modules response. (https://github.com/ansible-collections/ibm_zos_core/pull/1891).
- zos_job_query - Loads correct bytes size value for dds when using zoau 1.3.4 or later (https://github.com/ansible-collections/ibm_zos_core/pull/1868).
- zos_job_query - System and Subsystem are now retrieved from JES. (https://github.com/ansible-collections/ibm_zos_core/pull/1900).
- zos_job_submit -  Adds logging of Jinja rendered template content when `use_template` is true and verbosity level `-vvv` is used. (https://github.com/ansible-collections/ibm_zos_core/pull/1962).
- zos_job_submit - Add execution_time return value in the modules response. (https://github.com/ansible-collections/ibm_zos_core/pull/1891).
- zos_job_submit - Loads correct bytes size value for dds when using zoau 1.3.4 or later (https://github.com/ansible-collections/ibm_zos_core/pull/1868).
- zos_script - Adds error message for when remote source does not exist. (https://github.com/ansible-collections/ibm_zos_core/pull/1894).
- zos_script - Adds logging of Jinja rendered template content when `use_template` is true and verbosity level `-vvv` is used. (https://github.com/ansible-collections/ibm_zos_core/pull/1968).
- zos_script - Adds support for running local and remote scripts in asynchronous mode inside playbooks. (https://github.com/ansible-collections/ibm_zos_core/pull/1934).
- zos_script - Support automatic removal of carriage return line breaks [CR, CRLF] when copying local files to USS. (https://github.com/ansible-collections/ibm_zos_core/pull/1954).
- zos_stat - Adds support to query data sets using their aliases. (https://github.com/ansible-collections/ibm_zos_core/pull/2048)
- zos_unarchive - Adds support for unarchiving files in asynchronous mode inside playbooks. (https://github.com/ansible-collections/ibm_zos_core/pull/2020).
- zos_zfs_resize - Adds validations for trace destination dataset used for trace verbose. (https://github.com/ansible-collections/ibm_zos_core/pull/1897).

Bugfixes
--------

- zos_apf - When trying to add a library into the APF list that was already added, the module would fail. Fix now will not fail the module, and will inform the user that the library is already on the APF list. (https://github.com/ansible-collections/ibm_zos_core/pull/1893)
- zos_copy - Previously, if the dataset name included special characters such as $, validation would fail when force_lock was false. This has been changed to allow the use of special characters when force_lock option is false. (https://github.com/ansible-collections/ibm_zos_core/pull/1908)
- zos_copy - Previously, when trying to copy into remote and ansible's default temporary directory was not created before execution the copy task would fail. Fix now creates the temporary directory if possible. (https://github.com/ansible-collections/ibm_zos_core/pull/2109)
- zos_copy - When ``asa_text`` was set to true at the same time as ``force_lock``, a copy would fail saying the destination was already in use. Fix now opens destination data sets up with disposition SHR when ``force_lock`` and ``asa_text`` are set to true. (https://github.com/ansible-collections/ibm_zos_core/pull/1941).
- zos_copy - the carriage return characters were being removed from only first 1024 bytes of a file. Now fixed that issue to support removal of the carriage return characters from the complete file content if the file size is more than 1024 bytes. (https://github.com/ansible-collections/ibm_zos_core/pull/1954).
- zos_data_set - Module would fail when trying to delete a non-existent Generation Data Group. Fix now provides a successful response with `changed=false`. (https://github.com/ansible-collections/ibm_zos_core/pull/1971).
- zos_data_set - Module would fail with TypeError when trying to replace an existing GDG. Fix now allows to replacing a GDG. (https://github.com/ansible-collections/ibm_zos_core/pull/1964).
- zos_job_output - When searching for a job name, module performed a '*' (find all), then filtered the results. Fix now asks for specific job name, making the return faster and more precise. (https://github.com/ansible-collections/ibm_zos_core/pull/1916).
- zos_job_query - When searching for a job name, module performed a '*' (find all), then filtered the results. Fix now asks for specific job name, making the return faster and more precise. (https://github.com/ansible-collections/ibm_zos_core/pull/1916).
- zos_job_submit - Previously, the use of `become` would result in a permissions error while trying to execute a job from a local file. Fix now allows a user to escalate privileges when executing a job transferred from the controller node. (https://github.com/ansible-collections/ibm_zos_core/pull/2109)
- zos_job_submit - When searching for a job name, module performed a '*' (find all), then filtered the results. Fix now asks for specific job name, making the return faster and more precise. (https://github.com/ansible-collections/ibm_zos_core/pull/1916).
- zos_mount - Module failed when using persistent option with a data set that contains non UTF-8 characters. Fix now can use a data set with non UTF-8 characters as data_store. (https://github.com/ansible-collections/ibm_zos_core/pull/1871).

New Modules
-----------

- ibm.ibm_zos_core.zos_stat - Retrieve facts from MVS data sets, USS files, aggregates and generation data groups

v1.13.0
=======

Release Summary
---------------

Release Date: '2025-03-31'
This changelog describes all changes made to the modules and plugins included
in this collection. The release date is the date the changelog is created.
For additional details such as required dependencies and availability review
the collections `release notes <https://ibm.github.io/z_ansible_collections_doc/ibm_zos_core/docs/source/release_notes.html>`__

Minor Changes
-------------

- module_utils/import_handler - When importing a non supported ZOAU version like 1.2.x the module would throw a non user friendly error message. Error message is now explicit about ZOAU not being properly configured for Ansible. (https://github.com/ansible-collections/ibm_zos_core/pull/1804).
- zos_copy - Added new option ``autoescape`` to ``template_parameters``, allowing users to disable autoescaping of common XML/HTML characters when working with Jinja templates. (https://github.com/ansible-collections/ibm_zos_core/pull/1810).
- zos_copy - Adds error message when a PDS/E source member does not exist or is not cataloged. (https://github.com/ansible-collections/ibm_zos_core/pull/1821).
- zos_job_submit - Add deploy and forget capability. Now when wait_time_s is 0, the module will submit the job and will not wait to get the job details or content, returning only the job id. (https://github.com/ansible-collections/ibm_zos_core/pull/1746).
- zos_job_submit - Added new option ``autoescape`` to ``template_parameters``, allowing users to disable autoescaping of common XML/HTML characters when working with Jinja templates. (https://github.com/ansible-collections/ibm_zos_core/pull/1810).
- zos_job_submit - Added support to run zos_job_submit tasks in async mode inside playbooks. (https://github.com/ansible-collections/ibm_zos_core/pull/1786).
- zos_mvs_raw - Added ``max_rc`` option. Now when the user sets ``max_rc``, the module tolerates the failure if the return code is smaller than the ``max_rc`` specified, however, return value ``changed`` will be False if the program return code is not 0. (https://github.com/ansible-collections/ibm_zos_core/pull/1813).
- zos_script - Added new option ``autoescape`` to ``template_parameters``, allowing users to disable autoescaping of common XML/HTML characters when working with Jinja templates. (https://github.com/ansible-collections/ibm_zos_core/pull/1810).

Bugfixes
--------

- zos_copy - Improve module zos_copy error handling when the user does not have universal access authority set to UACC(READ) for SAF Profile 'MVS.MCSOPER.ZOAU' and SAF Class OPERCMDS. The module now handles the exception and returns an informative message. (https://github.com/ansible-collections/ibm_zos_core/pull/1744).
- zos_copy - Previously, if the dataset name included special characters such as $, validation would fail when force_lock was false. This has been changed to allow the use of special characters when force_lock option is false. (https://github.com/ansible-collections/ibm_zos_core/pull/1936)
- zos_copy - Previously, if the dataset name included special characters such as ``$`` and ``asa_text`` option is true, the module would fail. Fix now allows the use of special characters in the data set name when ``asa_text`` option is true. (https://github.com/ansible-collections/ibm_zos_core/pull/1924).
- zos_copy - When ``asa_text`` was set to true at the same time as ``force_lock``, a copy would fail saying the destination was already in use. Fix now opens destination data sets up with disposition SHR when ``force_lock`` and ``asa_text`` are set to true. (https://github.com/ansible-collections/ibm_zos_core/pull/1939).
- zos_fetch - Some relative paths were not accepted as a parameter e.g. C(files/fetched_file). Change now allows the user to use different types of relative paths as a parameter. (https://github.com/ansible-collections/ibm_zos_core/pull/1769).
- zos_find - Module would not find VSAM data and index resource types. Fix now finds the data and index resource types. (https://github.com/ansible-collections/ibm_zos_core/pull/1822).
- zos_find - Module would not find a VSAM cluster resource type if it was in use with DISP=OLD. Fix now finds the VSAM cluster. (https://github.com/ansible-collections/ibm_zos_core/pull/1822).
- zos_job_query - Module was not returning values for system and subsystem. Fix now returns these values. (https://github.com/ansible-collections/ibm_zos_core/pull/1759).
- zos_mvs_raw - If a program failed with a non-zero return code and verbose was false, the module would succeed. Whereas, if the program failed and verbose was true the module would fail. Fix now has a consistent behavior and fails in both cases. (https://github.com/ansible-collections/ibm_zos_core/pull/1774).
- zos_mvs_raw - Module would not populate stderr return value. Fix now populates stderr in return values. (https://github.com/ansible-collections/ibm_zos_core/pull/1808).
- zos_mvs_raw - Module would obfuscate the return code from the program when failing returning 8 instead. Fix now returns the proper return code from the program. (https://github.com/ansible-collections/ibm_zos_core/pull/1774).
- zos_mvs_raw - Module would return the stderr content in stdout when verbose was true and return code was 0. Fix now does not replace stdout content with stderr. (https://github.com/ansible-collections/ibm_zos_core/pull/1794).
- zos_mvs_raw - Option ``tmp_hlq`` was not being used as HLQ when creating backup data sets. Fix now uses ``tmp_hlq`` as HLQ for backup data sets. (https://github.com/ansible-collections/ibm_zos_core/pull/1847).
- zos_script - When the user trying to run a remote script had execute permissions but wasn't owner of the file, the module would fail while trying to change permissions on it. Fix now ensures the module first checks if the user can execute the script and only try to change permissions when necessary. (https://github.com/ansible-collections/ibm_zos_core/pull/1852).

New Modules
-----------

- ibm.ibm_zos_core.zos_zfs_resize - Resize a zfs data set.

v1.12.0
=======

Release Summary
---------------

Release Date: '2024-12-06'
This changelog describes all changes made to the modules and plugins included
in this collection. The release date is the date the changelog is created.
For additional details such as required dependencies and availability review
the collections `release notes <https://ibm.github.io/z_ansible_collections_doc/ibm_zos_core/docs/source/release_notes.html>`__

Minor Changes
-------------

- zos_backup_restore - Redefines the default behavior of module option `hlq`. When option `operation` is set to `restore` and the `hlq` is not provided, the original high level qualifiers in a backup will be used for a restore. (https://github.com/ansible-collections/ibm_zos_core/pull/1632).
- zos_job_output - Added address space type used by jobs in return JSON as `content_type`. (https://github.com/ansible-collections/ibm_zos_core/pull/1673).
- zos_job_query - Added address space type used by jobs in return JSON as `content_type`. (https://github.com/ansible-collections/ibm_zos_core/pull/1673).
- zos_job_submit - Added address space type used by jobs in return JSON as `content_type`. (https://github.com/ansible-collections/ibm_zos_core/pull/1673).
- zos_mvs_raw - Un-mappable chars in stdout/stderr streams are now replaced with the replacement character. (https://github.com/ansible-collections/ibm_zos_core/pull/1634).
- zos_operator - Added new option ``case_sensitive`` to module, allowing users to control how case in a command is handled by it. (https://github.com/ansible-collections/ibm_zos_core/pull/1641)
- zos_script - Un-mappable chars in stdout/stderr streams are now replaced with the replacement character. (https://github.com/ansible-collections/ibm_zos_core/pull/1634).
- zos_tso_command - Un-mappable chars in stdout/stderr streams are now replaced with the replacement character. (https://github.com/ansible-collections/ibm_zos_core/pull/1634).

Bugfixes
--------

- zos_apf - The ``tmp_hlq`` option was previously ignored and default values were used instead. Fix now honors the value set in the module option. (https://github.com/ansible-collections/ibm_zos_core/pull/1695).
- zos_archive - The ``tmp_hlq`` option was previously ignored and default values were used instead. Fix now honors the value set in the module option. (https://github.com/ansible-collections/ibm_zos_core/pull/1695).
- zos_backup_restore - When a recoverable error was encountered and ``recover=True``, the module would ignore the option and fail. Fix now does not fail when a recoverable error is raised when ``recover=True``. (https://github.com/ansible-collections/ibm_zos_core/pull/1643).
- zos_blockinfile - Previously module was not able to delete a block when 'marker_begin' and 'marker_end' were set to the same value. Fix introduces a requirement for 'marker_begin' and 'marker_end' to have different values. (https://github.com/ansible-collections/ibm_zos_core/pull/1684).
- zos_blockinfile - The ``tmp_hlq`` option was previously ignored and default values were used instead. Fix now honors the value set in the module option. (https://github.com/ansible-collections/ibm_zos_core/pull/1695).
- zos_copy - Improve module zos_copy error handling when the user does not have universal access authority set to UACC(READ) for SAF Profile 'MVS.MCSOPER.ZOAU' and SAF Class OPERCMDS. The module now handles the exception and returns an informative message. (https://github.com/ansible-collections/ibm_zos_core/pull/1766).
- zos_copy - Previously, the module ignored the value of ``remote_tmp`` set in Ansible configuration file and used the ``/tmp/`` directory. Fix now uses the value of ``remote_tmp`` or the default value ``~/.ansible/tmp`` if none is given. (https://github.com/ansible-collections/ibm_zos_core/pull/1739).
- zos_copy - The ``tmp_hlq`` option was previously ignored and default values were used instead. Fix now honors the value set in the module option. (https://github.com/ansible-collections/ibm_zos_core/pull/1695).
- zos_data_set - The ``tmp_hlq`` option was previously ignored and default values were used instead. Fix now honors the value set in the module option. (https://github.com/ansible-collections/ibm_zos_core/pull/1695).
- zos_encode - The ``tmp_hlq`` option was previously ignored and default values were used instead. Fix now honors the value set in the module option. (https://github.com/ansible-collections/ibm_zos_core/pull/1695).
- zos_fetch - The ``tmp_hlq`` option was previously ignored and default values were used instead. Fix now honors the value set in the module option. (https://github.com/ansible-collections/ibm_zos_core/pull/1695).
- zos_find - Module would not find VSAM data and index resource types. Fix now finds the data and index resource types. (https://github.com/ansible-collections/ibm_zos_core/pull/1818).
- zos_find - Module would not find a VSAM cluster resource type if it was in use with DISP=OLD. Fix now finds the VSAM cluster. (https://github.com/ansible-collections/ibm_zos_core/pull/1818).
- zos_job_output - RACF user names containing a ``@``, ``$``, or ``#`` raised an invalid argument error. Fix now allows the use of all valid characters for a RACF user. (https://github.com/ansible-collections/ibm_zos_core/pull/1661).
- zos_job_query - Module was not returning values for system and subsystem. Fix now returns these values. (https://github.com/ansible-collections/ibm_zos_core/pull/1761).
- zos_job_query - RACF user names containing a ``@``, ``$``, or ``#`` raised an invalid argument error. Fix now allows the use of all valid characters for a RACF user. (https://github.com/ansible-collections/ibm_zos_core/pull/1661).
- zos_lineinfile - The ``tmp_hlq`` option was previously ignored and default values were used instead. Fix now honors the value set in the module option. (https://github.com/ansible-collections/ibm_zos_core/pull/1695).
- zos_mount - The ``tmp_hlq`` option was previously ignored and default values were used instead. Fix now honors the value set in the module option. (https://github.com/ansible-collections/ibm_zos_core/pull/1695).
- zos_mvs_raw - If a program failed with a non-zero return code and verbose was false, the module would succeed (false positive). Fix now fails the module for all instances where a program has a non-zero return code. (https://github.com/ansible-collections/ibm_zos_core/pull/1780).
- zos_mvs_raw - Module would obfuscate the return code from the program when failing returning 8 instead. Fix now returns the proper return code from the program. (https://github.com/ansible-collections/ibm_zos_core/pull/1780).
- zos_mvs_raw - Module would return the stderr content in stdout when verbose was true and return code was 0. Fix now does not replace stdout content with stderr. (https://github.com/ansible-collections/ibm_zos_core/pull/1800).
- zos_mvs_raw - base64 sub-option for return_content under option for retrieving DD output did not return base64. Fix now returns the base64 encoded contents of the DD. (https://github.com/ansible-collections/ibm_zos_core/pull/1691).
- zos_script - The module would discard command line arguments in a command, except for the first one. Fix now makes sure that all arguments are passed to the remote command that gets executed. (https://github.com/ansible-collections/ibm_zos_core/pull/1698).
- zos_unarchive - The ``tmp_hlq`` option was previously ignored and default values were used instead. Fix now honors the value set in the module option. (https://github.com/ansible-collections/ibm_zos_core/pull/1695).

v1.11.0
=======

Release Summary
---------------

Release Date: '2024-10-01'
This changelog describes all changes made to the modules and plugins included
in this collection. The release date is the date the changelog is created.
For additional details such as required dependencies and availability review
the collections `release notes <https://ibm.github.io/z_ansible_collections_doc/ibm_zos_core/docs/source/release_notes.html>`__

Minor Changes
-------------

- zos_apf - Change input to auto-escape 'library' names containing symbols (https://github.com/ansible-collections/ibm_zos_core/pull/1493).
- zos_archive - Added support for GDG and GDS relative name notation to archive data sets. Added support for data set names with special characters like $, /#, /- and @. (https://github.com/ansible-collections/ibm_zos_core/pull/1511).
- zos_backup_restore - Added support for GDS relative name notation to include or exclude data sets when operation is backup. Added support for data set names with special characters like $, /#, and @. (https://github.com/ansible-collections/ibm_zos_core/pull/1527).
- zos_blockinfile - Added support for GDG and GDS relative name notation to use a data set. And backup in new generations. Added support for data set names with special characters like $, /#, /- and @. (https://github.com/ansible-collections/ibm_zos_core/pull/1516).
- zos_copy - add support for copying generation data sets (GDS) and generation data groups (GDG), as well as using a GDS for backup. (https://github.com/ansible-collections/ibm_zos_core/pull/1564).
- zos_data_set - Added support for GDG and GDS relative name notation to create, delete, catalog and uncatalog a data set. Added support for data set names with special characters like $, /#, /- and @. (https://github.com/ansible-collections/ibm_zos_core/pull/1504).
- zos_encode - add support for encoding generation data sets (GDS), as well as using a GDS for backup. (https://github.com/ansible-collections/ibm_zos_core/pull/1531).
- zos_fetch - add support for fetching generation data groups and generation data sets. (https://github.com/ansible-collections/ibm_zos_core/pull/1519)
- zos_find - added support for GDG/GDS and special characters (https://github.com/ansible-collections/ibm_zos_core/pull/1518).
- zos_job_submit - Improved the copy to remote mechanic to avoid using deepcopy that could result in failure for some systems. (https://github.com/ansible-collections/ibm_zos_core/pull/1561).
- zos_job_submit - add support for generation data groups and generation data sets as sources for jobs. (https://github.com/ansible-collections/ibm_zos_core/pull/1497)
- zos_lineinfile - Added support for GDG and GDS relative name notation to use a data set. And backup in new generations. Added support for data set names with special characters like $, /#, /- and @. (https://github.com/ansible-collections/ibm_zos_core/pull/1516).
- zos_mount - Added support for data set names with special characters ($, /#, /- and @). This is for both src and backup data set names. (https://github.com/ansible-collections/ibm_zos_core/pull/1631).
- zos_mvs_raw - Added support for GDG and GDS relative name notation to use a data set. Added support for data set names with special characters like $, /#, /- and @. (https://github.com/ansible-collections/ibm_zos_core/pull/1525).
- zos_mvs_raw - Added support for GDG and GDS relative positive name notation to use a data set. (https://github.com/ansible-collections/ibm_zos_core/pull/1541).
- zos_mvs_raw - Redesign the wrappers of dd clases to use properly the arguments. (https://github.com/ansible-collections/ibm_zos_core/pull/1470).
- zos_script - Improved the copy to remote mechanic to avoid using deepcopy that could result in failure for some systems. (https://github.com/ansible-collections/ibm_zos_core/pull/1561).
- zos_tso_command - Added support for GDG and GDS relative name notation to use a data set name. Added support for data set names with special characters like $, /#, /- and @. (https://github.com/ansible-collections/ibm_zos_core/pull/1563).
- zos_unarchive - Added support for data set names with special characters like $, /#, /- and @. (https://github.com/ansible-collections/ibm_zos_core/pull/1511).
- zos_unarchive - Improved the copy to remote mechanic to avoid using deepcopy that could result in failure for some systems. (https://github.com/ansible-collections/ibm_zos_core/pull/1561).

Bugfixes
--------

- module_util/data_set.py - DataSet.data_set_cataloged function previously only returned True or False, but failed to account for exceptions which occurred during the LISTCAT. The fix now raises an MVSCmdExecError if the return code from LISTCAT is too high. (https://github.com/ansible-collections/ibm_zos_core/pull/1535).
- zos_copy - a regression in version 1.4.0 made the module stop automatically computing member names when copying a single file into a PDS/E. Fix now lets a user copy a single file into a PDS/E without adding a member in the dest option. (https://github.com/ansible-collections/ibm_zos_core/pull/1570).
- zos_copy - module would use opercmd to check if a non existent destination data set is locked. Fix now only checks if the destination is already present. (https://github.com/ansible-collections/ibm_zos_core/pull/1623).
- zos_job_submit - Was not propagating any error types UnicodeDecodeError, JSONDecodeError, TypeError, KeyError when encountered, now the error message shares the type error. (https://github.com/ansible-collections/ibm_zos_core/pull/1560).
- zos_mvs_raw - DD_output first character from each line was missing. Change now includes the first character of each line. (https://github.com/ansible-collections/ibm_zos_core/pull/1543).

v1.10.0
=======

Release Summary
---------------

Release Date: '2024-06-11'
This changelog describes all changes made to the modules and plugins included
in this collection. The release date is the date the changelog is created.
For additional details such as required dependencies and availability review
the collections `release notes <https://ibm.github.io/z_ansible_collections_doc/ibm_zos_core/docs/source/release_notes.html>`__

Minor Changes
-------------

- zos_apf - Enhanced error messages when an exception is caught. (https://github.com/ansible-collections/ibm_zos_core/pull/1204).
- zos_backup_restore - Add tmp_hlq option to the user interface to override the default high level qualifier (HLQ) for temporary and backup. (https://github.com/ansible-collections/ibm_zos_core/pull/1265).
- zos_copy - Documented `group` and `owner` options. (https://github.com/ansible-collections/ibm_zos_core/pull/1307).
- zos_copy - Improve zos_copy performance when copying multiple members from one PDS/E to another. (https://github.com/ansible-collections/ibm_zos_core/pull/1183).

Breaking Changes / Porting Guide
--------------------------------

- zos_archive - option ``terse_pack`` no longer accepts uppercase choices, users should replace them with lowercase ones. (https://github.com/ansible-collections/ibm_zos_core/pull/1388).
- zos_archive - suboption ``record_format`` of ``dest_data_set`` no longer accepts uppercase choices, users should replace them with lowercase ones. (https://github.com/ansible-collections/ibm_zos_core/pull/1388).
- zos_archive - suboption ``space_type`` of ``dest_data_set`` no longer accepts uppercase choices, users should replace them with lowercase ones. (https://github.com/ansible-collections/ibm_zos_core/pull/1388).
- zos_archive - suboption ``type`` of ``dest_data_set`` no longer accepts uppercase choices, users should replace them with lowercase ones. (https://github.com/ansible-collections/ibm_zos_core/pull/1388).
- zos_backup_restore - option ``space_type`` no longer accepts uppercase choices, users should replace them with lowercase ones. (https://github.com/ansible-collections/ibm_zos_core/pull/1388).
- zos_copy - suboption ``record_format`` of ``dest_data_set`` no longer accepts uppercase choices, users should replace them with lowercase ones. (https://github.com/ansible-collections/ibm_zos_core/pull/1388).
- zos_copy - suboption ``space_type`` of ``dest_data_set`` no longer accepts uppercase choices, users should replace them with lowercase ones. (https://github.com/ansible-collections/ibm_zos_core/pull/1388).
- zos_copy - suboption ``type`` of ``dest_data_set`` no longer accepts uppercase choices, users should replace them with lowercase ones. (https://github.com/ansible-collections/ibm_zos_core/pull/1388).
- zos_data_set - option ``record_format`` no longer accepts uppercase choices, users should replace them with lowercase ones. (https://github.com/ansible-collections/ibm_zos_core/pull/1388).
- zos_data_set - option ``space_type`` no longer accepts uppercase choices, users should replace them with lowercase ones. (https://github.com/ansible-collections/ibm_zos_core/pull/1388).
- zos_data_set - option ``type`` no longer accepts uppercase choices, users should replace them with lowercase ones. (https://github.com/ansible-collections/ibm_zos_core/pull/1388).
- zos_data_set - options inside ``batch`` no longer accept uppercase choices, users should replace them with lowercase ones. (https://github.com/ansible-collections/ibm_zos_core/pull/1388).
- zos_job_submit - option ``location`` no longer accepts uppercase choices, users should replace them with lowercase ones. (https://github.com/ansible-collections/ibm_zos_core/pull/1388).
- zos_mount - option ``automove`` no longer accepts uppercase choices, users should replace them with lowercase ones. (https://github.com/ansible-collections/ibm_zos_core/pull/1388).
- zos_mount - option ``fs_type`` no longer accepts uppercase choices, users should replace them with lowercase ones. (https://github.com/ansible-collections/ibm_zos_core/pull/1388).
- zos_mount - option ``mount_opts`` no longer accepts uppercase choices, users should replace them with lowercase ones. (https://github.com/ansible-collections/ibm_zos_core/pull/1388).
- zos_mount - option ``tag_untagged`` no longer accepts uppercase choices, users should replace them with lowercase ones. (https://github.com/ansible-collections/ibm_zos_core/pull/1388).
- zos_mount - option ``unmount_opts`` no longer accepts uppercase choices, users should replace them with lowercase ones. (https://github.com/ansible-collections/ibm_zos_core/pull/1388).
- zos_mvs_raw - options inside ``dd_concat`` no longer accept uppercase choices, users should replace them with lowercase ones. (https://github.com/ansible-collections/ibm_zos_core/pull/1388).
- zos_mvs_raw - suboption ``record_format`` of ``dd_data_set`` no longer accepts uppercase choices, users should replace them with lowercase ones. (https://github.com/ansible-collections/ibm_zos_core/pull/1388).
- zos_mvs_raw - suboption ``record_format`` of ``dd_unix`` no longer accepts uppercase choices, users should replace them with lowercase ones. (https://github.com/ansible-collections/ibm_zos_core/pull/1388).
- zos_mvs_raw - suboption ``space_type`` of ``dd_data_set`` no longer accepts uppercase choices, users should replace them with lowercase ones. (https://github.com/ansible-collections/ibm_zos_core/pull/1388).
- zos_mvs_raw - suboption ``type`` of ``dd_data_set`` no longer accepts uppercase choices, users should replace them with lowercase ones. (https://github.com/ansible-collections/ibm_zos_core/pull/1388).
- zos_mvs_raw - suboptions ``disposition_normal`` and ``disposition_abnormal`` of ``dd_data_set`` no longer accept ``catlg`` and ``uncatlg`` as choices. This also applies when defining a ``dd_data_set`` inside ``dd_concat``. (https://github.com/ansible-collections/ibm_zos_core/pull/1388).
- zos_unarchive - suboption ``record_format`` of ``dest_data_set`` no longer accepts uppercase choices, users should replace them with lowercase ones. (https://github.com/ansible-collections/ibm_zos_core/pull/1388).
- zos_unarchive - suboption ``space_type`` of ``dest_data_set`` no longer accepts uppercase choices, users should replace them with lowercase ones. (https://github.com/ansible-collections/ibm_zos_core/pull/1388).
- zos_unarchive - suboption ``type`` of ``dest_data_set`` no longer accepts uppercase choices, users should replace them with lowercase ones. (https://github.com/ansible-collections/ibm_zos_core/pull/1388).

Bugfixes
--------

- module_utils/job.py - job output containing non-printable characters would crash modules. Fix now handles the error gracefully and returns a message to the user inside `content` of the `ddname` that failed. (https://github.com/ansible-collections/ibm_zos_core/pull/1261).
- zos_apf - List option only returned one data set. Fix now returns the list of retrieved data sets. (https://github.com/ansible-collections/ibm_zos_core/pull/1204).
- zos_blockinfile - Using double quotation marks inside a block resulted in a false positive result with ZOAU 1.3. Fix now handles this special case to avoid false negatives. (https://github.com/ansible-collections/ibm_zos_core/pull/1340).
- zos_find - Filter size failed if a PDS/E matched the pattern. Fix now gets the correct size for PDS/Es. (https://github.com/ansible-collections/ibm_zos_core/pull/1443).
- zos_job_submit - Was ignoring the default value for location=DATA_SET, now when location is not specified it will default to DATA_SET. (https://github.com/ansible-collections/ibm_zos_core/pull/1220).
- zos_job_submit - when the argument max_rc was different than 0 the changed response returned as false. Fix now return a changed response as true when the rc is not 0 and max_rc is above or equal to the value of the job. (https://github.com/ansible-collections/ibm_zos_core/pull/1345).
- zos_mvs_raw - The module ignored the value of `tmp_hlq` option when creating temporary data sets. Fix now honors the value if provided and uses it as High Level Qualifier for temporary data sets created during the module execution. (https://github.com/ansible-collections/ibm_zos_core/pull/1320).

v1.9.0
======

Release Summary
---------------

Release Date: '2024-03-11'
This changelog describes all changes made to the modules and plugins included
in this collection. The release date is the date the changelog is created.
For additional details such as required dependencies and availability review
the collections `release notes <https://ibm.github.io/z_ansible_collections_doc/ibm_zos_core/docs/source/release_notes.html>`__

Major Changes
-------------

- zos_job_submit - when job statuses were read, were limited to AC (active), CC (completed normally), ABEND (ended abnormally) and ? (error unknown), SEC (security error), JCLERROR (job had a jcl error). Now the additional statuses are supported, CANCELLED (job was cancelled), CAB (converter abend), CNV (converter error), SYS (system failure) and FLU (job was flushed). (https://github.com/ansible-collections/ibm_zos_core/pull/1283).

Minor Changes
-------------

- zos_apf - Improves exception handling if there is a failure parsing the command response when operation selected is list. (https://github.com/ansible-collections/ibm_zos_core/pull/1036).
- zos_copy - Improve zos_copy performance when copying multiple members from one PDS/E to another. (https://github.com/ansible-collections/ibm_zos_core/pull/1176).
- zos_job_output - When passing a job ID and owner the module take as mutually exclusive. Change now allows the use of a job ID and owner at the same time. (https://github.com/ansible-collections/ibm_zos_core/pull/1078).
- zos_job_submit - Improve error messages in zos_job_submit to be clearer. (https://github.com/ansible-collections/ibm_zos_core/pull/1074).
- zos_job_submit - The module had undocumented parameter and uses as temporary file when the location of the file is LOCAL. Change now uses the same name as the src for the temporary file removing the addition of tmp_file to the arguments. (https://github.com/ansible-collections/ibm_zos_core/pull/1091).
- zos_job_submit - The module handling ZOAU import errors obscured the original traceback when an import error ocurred. Fix now passes correctly the context to the user. (https://github.com/ansible-collections/ibm_zos_core/pull/1091).
- zos_mvs_raw - when using the dd_input content option for instream-data, if the content was not properly indented according to the program which is generally a blank in columns 1 & 2, those columns would be truncated. Now, when setting instream-data, the module will ensure that all lines contain a blank in columns 1 and 2 and add blanks when not present while retaining a maximum length of 80 columns for any line. This is true for all content types; string, list of strings and when using a YAML block indicator. (https://github.com/ansible-collections/ibm_zos_core/pull/1057). - zos_mvs_raw - no examples were included with the module that demonstrated using a YAML block indicator, this now includes examples using a YAML block indicator.
- zos_tso_command - add example for executing explicitly a REXX script from a data set. (https://github.com/ansible-collections/ibm_zos_core/pull/1065).

Bugfixes
--------

- module_utils/job.py - job output containing non-printable characters would crash modules. Fix now handles the error gracefully and returns a message to the user inside `content` of the `ddname` that failed. (https://github.com/ansible-collections/ibm_zos_core/pull/1288).
- zos_apf - When operation=list was selected and more than one data set entry was fetched, the module only returned one data set. Fix now returns the complete list. (https://github.com/ansible-collections/ibm_zos_core/pull/1236).
- zos_copy - When copying an executable data set with aliases and destination did not exist, destination data set was created with wrong attributes. Fix now creates destination data set with the same attributes as the source. (https://github.com/ansible-collections/ibm_zos_core/pull/1066).
- zos_copy - When performing a copy operation to an existing file, the copied file resulted in having corrupted contents. Fix now implements a workaround to not use the specific copy routine that corrupts the file contents. (https://github.com/ansible-collections/ibm_zos_core/pull/1064).
- zos_data_set - Fixes a small parsing bug in module_utils/data_set function which extracts volume serial(s) from a LISTCAT command output. Previously a leading '-' was left behind for volser strings under 6 chars. (https://github.com/ansible-collections/ibm_zos_core/pull/1247).
- zos_job_output - When passing a job ID or name less than 8 characters long, the module sent the full stack trace as the module's message. Change now allows the use of a shorter job ID or name, as well as wildcards. (https://github.com/ansible-collections/ibm_zos_core/pull/1078).
- zos_job_query - The module handling ZOAU import errors obscured the original traceback when an import error ocurred. Fix now passes correctly the context to the user. (https://github.com/ansible-collections/ibm_zos_core/pull/1042).
- zos_job_query - When passing a job ID or name less than 8 characters long, the module sent the full stack trace as the module's message. Change now allows the use of a shorter job ID or name, as well as wildcards. (https://github.com/ansible-collections/ibm_zos_core/pull/1078).
- zos_job_submit - Was ignoring the default value for location=DATA_SET, now when location is not specified it will default to DATA_SET. (https://github.com/ansible-collections/ibm_zos_core/pull/1120).
- zos_job_submit - when a JCL error occurred, the ret_code[msg_code] contained JCLERROR followed by an integer where the integer appeared to be a reason code when actually it is a multi line marker used to coordinate errors spanning more than one line. Now when a JCLERROR occurs, only the JCLERROR is returned for property ret_code[msg_code]. (https://github.com/ansible-collections/ibm_zos_core/pull/1283).
- zos_job_submit - when a response was returned, it contained an undocumented property; ret_code[msg_text]. Now when a response is returned, it correctly returns property ret_code[msg_txt]. (https://github.com/ansible-collections/ibm_zos_core/pull/1283).
- zos_job_submit - when typrun=copy was used in JCL it would fail the module with an improper message and error condition. While this case continues to be considered a failure, the message has been corrected and it fails under the condition that not enough time has been added to the modules execution. (https://github.com/ansible-collections/ibm_zos_core/pull/1283).
- zos_job_submit - when typrun=hold was used in JCL it would fail the module with an improper message and error condition. While this case continues to be considered a failure, the message has been corrected and it fails under the condition that not enough time has been added to the modules execution. (https://github.com/ansible-collections/ibm_zos_core/pull/1283).
- zos_job_submit - when typrun=jchhold was used in JCL it would fail the module with an improper message and error condition. While this case continues to be considered a failure, the message has been corrected and it fails under the condition that not enough time has been added to the modules execution. (https://github.com/ansible-collections/ibm_zos_core/pull/1283).
- zos_job_submit - when typrun=scan was used in JCL, it would fail the module. Now typrun=scan no longer fails the module and an appropriate message is returned with appropriate return code values. (https://github.com/ansible-collections/ibm_zos_core/pull/1283).
- zos_job_submit - when wait_time_s was used, the duration would run approximately 5 second longer than reported in the duration. Now the when duration is returned, it is the actual accounting from when the job is submitted to when the module reads the job output. (https://github.com/ansible-collections/ibm_zos_core/pull/1283).
- zos_operator - The module handling ZOAU import errors obscured the original traceback when an import error ocurred. Fix now passes correctly the context to the user. (https://github.com/ansible-collections/ibm_zos_core/pull/1042).
- zos_unarchive - Using a local file with a USS format option failed when sending to remote because dest_data_set option had an empty dictionary. Fix now leaves dest_data_set as None when using a USS format option. (https://github.com/ansible-collections/ibm_zos_core/pull/1045).
- zos_unarchive - When unarchiving USS files, the module left temporary files on the remote. Change now removes temporary files. (https://github.com/ansible-collections/ibm_zos_core/pull/1073).

v1.8.0
======

Release Summary
---------------

Release Date: '2023-12-08'
This changelog describes all changes made to the modules and plugins included
in this collection. The release date is the date the changelog is created.
For additional details such as required dependencies and availability review
the collections `release notes <https://ibm.github.io/z_ansible_collections_doc/ibm_zos_core/docs/source/release_notes.html>`__

Minor Changes
-------------

- module_utils/template - Add validation into path joins to detect unauthorized path traversals. (https://github.com/ansible-collections/ibm_zos_core/pull/1029)
- zos_archive - Add validation into path joins to detect unauthorized path traversals. (https://github.com/ansible-collections/ibm_zos_core/pull/1029)
- zos_archive - Enhanced test cases to use test lines the same length of the record length. (https://github.com/ansible-collections/ibm_zos_core/pull/965)
- zos_copy -  Add validation into path joins to detect unauthorized path traversals. (https://github.com/ansible-collections/ibm_zos_core/pull/962)
- zos_copy - Add new option `force_lock` that can copy into data sets that are already in use by other processes (DISP=SHR). User needs to use with caution because this is subject to race conditions and can lead to data loss. (https://github.com/ansible-collections/ibm_zos_core/pull/980).
- zos_copy - includes a new option `executable` that enables copying of executables such as load modules or program objects to both USS and partitioned data sets. When the `dest` option contains a non-existent data set, `zos_copy` will create a data set with the appropriate attributes for an executable. (https://github.com/ansible-collections/ibm_zos_core/pull/804)
- zos_copy - introduces a new option 'aliases' to enable preservation of member aliases when copying data to partitioned data sets (PDS) destinations from USS or other PDS sources. Copying aliases of text based members to/from USS is not supported. (https://github.com/ansible-collections/ibm_zos_core/pull/1014)
- zos_fetch - Add validation into path joins to detect unauthorized path traversals. (https://github.com/ansible-collections/ibm_zos_core/pull/962)
- zos_job_submit - Change action plugin call from copy to zos_copy. (https://github.com/ansible-collections/ibm_zos_core/pull/951)
- zos_job_submit - Previous code did not return output, but still requested job data from the target system. This changes to honor return_output=false by not querying the job dd segments at all. (https://github.com/ansible-collections/ibm_zos_core/pull/1063).
- zos_operator - Changed system to call 'wait=true' parameter to zoau call. Requires zoau 1.2.5 or later. (https://github.com/ansible-collections/ibm_zos_core/pull/976)
- zos_operator_action_query - Add a max delay of 5 seconds on each part of the operator_action_query. Requires zoau 1.2.5 or later. (https://github.com/ansible-collections/ibm_zos_core/pull/976)
- zos_script - Add support for remote_tmp from the Ansible configuration to setup where temporary files will be created, replacing the module option tmp_path. (https://github.com/ansible-collections/ibm_zos_core/pull/1068).
- zos_tso_command - Add example for executing explicitly a REXX script from a data set. (https://github.com/ansible-collections/ibm_zos_core/pull/1072).
- zos_unarchive -  Add validation into path joins to detect unauthorized path traversals. (https://github.com/ansible-collections/ibm_zos_core/pull/1029)
- zos_unarchive - Enhanced test cases to use test lines the same length of the record length. (https://github.com/ansible-collections/ibm_zos_core/pull/965)

Deprecated Features
-------------------

- zos_blockinfile debug - is deprecated in favor of 'as_json' (https://github.com/ansible-collections/ibm_zos_core/pull/904).

Bugfixes
--------

- zos_copy - Update option limit to include LIBRARY as dest_dataset/suboption value. Documentation updated to reflect this change. (https://github.com/ansible-collections/ibm_zos_core/pull/968).
- zos_copy - When copying an executable data set from controller to managed node, copy operation failed with an encoding error. Fix now avoids encoding when executable option is selected. (https://github.com/ansible-collections/ibm_zos_core/pull/1079).
- zos_copy - When copying an executable data set with aliases and destination did not exist, destination data set was created with wrong attributes. Fix now creates destination data set with the same attributes as the source. (https://github.com/ansible-collections/ibm_zos_core/pull/1067).
- zos_copy - When performing a copy operation to an existing file, the copied file resulted in having corrupted contents. Fix now implements a workaround to not use the specific copy routine that corrupts the file contents. (https://github.com/ansible-collections/ibm_zos_core/pull/1069).
- zos_job_submit - Temporary files were created in tmp directory. Fix now ensures the deletion of files every time the module run. (https://github.com/ansible-collections/ibm_zos_core/pull/951)
- zos_job_submit - The last line of the jcl was missing in the input. Fix now ensures the presence of the full input in job_submit. (https://github.com/ansible-collections/ibm_zos_core/pull/952)
- zos_lineinfile - A duplicate entry was made even if line was already present in the target file. Fix now prevents a duplicate entry if the line already exists in the target file. (https://github.com/ansible-collections/ibm_zos_core/pull/916)
- zos_operator - The last line of the operator was missing in the response of the module. The fix now ensures the presence of the full output of the operator. https://github.com/ansible-collections/ibm_zos_core/pull/918)
- zos_operator - The module was ignoring the wait time argument. The module now passes the wait time argument to ZOAU. (https://github.com/ansible-collections/ibm_zos_core/pull/1063).
- zos_operator_action_query - The module was ignoring the wait time argument. The module now passes the wait time argument to ZOAU. (https://github.com/ansible-collections/ibm_zos_core/pull/1063).
- zos_unarchive - When zos_unarchive fails during unpack either with xmit or terse it does not clean the temporary data sets created. Fix now removes the temporary data sets. (https://github.com/ansible-collections/ibm_zos_core/pull/1054).

Known Issues
------------

- Several modules have reported UTF8 decoding errors when interacting with results that contain non-printable UTF8 characters in the response. This occurs when a module receives content that does not correspond to a UTF-8 value. These include modules `zos_job_submit`, `zos_job_output`, `zos_operator_action_query` but are not limited to this list. This will be addressed in `ibm_zos_core` version 1.10.0-beta.1. Each case is unique, some options to work around the error are below. - Specify that the ASA assembler option be enabled to instruct the assembler to use ANSI control characters instead of machine code control characters. - Add `ignore_errors:true` to the playbook task so the task error will not fail the playbook. - If the error is resulting from a batch job, add `ignore_errors:true` to the task and capture the output into a variable and extract the job ID with a regular expression and then use `zos_job_output` to display the DD without the non-printable character such as the DD `JESMSGLG`. (https://github.com/ansible-collections/ibm_zos_core/issues/677) (https://github.com/ansible-collections/ibm_zos_core/issues/776) (https://github.com/ansible-collections/ibm_zos_core/issues/972)
- With later versions of `ansible-core` used with `ibm_zos_core` collection a warning has started to appear "Module "ansible.builtin.command" returned non UTF-8 data in the JSON response" that is currently being reviewed. There are no recommendations at this point. (https://github.com/ansible-collections/ibm_zos_core/issues/983)

New Modules
-----------

- ibm.ibm_zos_core.zos_script - Run scripts in z/OS

v1.7.0
======

Release Summary
---------------

Release Date: '2023-10-09'
This changelog describes all changes made to the modules and plugins included
in this collection. The release date is the date the changelog is created.
For additional details such as required dependencies and availability review
the collections `release notes <https://ibm.github.io/z_ansible_collections_doc/ibm_zos_core/docs/source/release_notes.html>`__

Major Changes
-------------

- zos_copy - Previously, backups were taken when force was set to false; whether or not a user specified this operation which caused allocation issues with space and permissions. This removes the automatic backup performed and reverts to the original logic in that backups must be initiated by the user. (https://github.com/ansible-collections/ibm_zos_core/pull/896)

Minor Changes
-------------

- Add support for Jinja2 templates in zos_copy and zos_job_submit when using local source files. (https://github.com/ansible-collections/ibm_zos_core/pull/667)
- zos_archive - If destination data set space is not provided then the module computes it based on the src list and/or expanded src list based on pattern provided. (https://github.com/ansible-collections/ibm_zos_core/pull/930).
- zos_archive - When xmit faces a space error in xmit operation because of dest or log data set are filled raises an appropriate error hint. (https://github.com/ansible-collections/ibm_zos_core/pull/930).
- zos_copy - Adds block_size, record_format, record_length, space_primary, space_secondary, space_type and type in the return output when the destination data set does not exist and has to be created by the module. (https://github.com/ansible-collections/ibm_zos_core/pull/773)
- zos_data_set - record format = 'F' has been added to support 'fixed' block records. This allows records that can use the entire block. (https://github.com/ansible-collections/ibm_zos_core/pull/821)
- zos_job_output - zoau added 'program_name' to their field output starting with v1.2.4.  This enhancement checks for that version and passes the extra column through. (https://github.com/ansible-collections/ibm_zos_core/pull/841)
- zos_job_query - Adds new fields job_class, svc_class, priority, asid, creation_datetime, and queue_position to the return output when querying or submitting a job. Available when using ZOAU v1.2.3 or greater. (https://github.com/ansible-collections/ibm_zos_core/pull/778)
- zos_job_query - unnecessary calls were made to find a jobs DDs that incurred unnecessary overhead. This change removes those resulting in a performance increase in job related queries. (https://github.com/ansible-collections/ibm_zos_core/pull/911)
- zos_job_query - zoau added 'program_name' to their field output starting with v1.2.4.  This enhancement checks for that version and passes the extra column through. (https://github.com/ansible-collections/ibm_zos_core/pull/841)
- zos_job_submit - zoau added 'program_name' to their field output starting with v1.2.4.  This enhancement checks for that version and passes the extra column through. (https://github.com/ansible-collections/ibm_zos_core/pull/841)
- zos_unarchive - When copying to remote fails now a proper error message is displayed. (https://github.com/ansible-collections/ibm_zos_core/pull/930).
- zos_unarchive - When copying to remote if space_primary is not defined, then is defaulted to 5M. (https://github.com/ansible-collections/ibm_zos_core/pull/930).

Bugfixes
--------

- module_utils - data_set.py - Reported a failure caused when cataloging a VSAM data set. Fix now corrects how VSAM data sets are cataloged. (https://github.com/ansible-collections/ibm_zos_core/pull/791).
- zos_archive - Module did not return the proper src state after archiving. Fix now displays the status of the src after the operation. (https://github.com/ansible-collections/ibm_zos_core/pull/930).
- zos_blockinfile - Test case generate a data set that was not correctly removed. Changes delete the correct data set not only member. (https://github.com/ansible-collections/ibm_zos_core/pull/840)
- zos_copy - Module returned the dynamic values created with the same dataset type and record format. Fix validate the correct dataset type and record format of target created. (https://github.com/ansible-collections/ibm_zos_core/pull/824)
- zos_copy - Reported a false positive such that the response would have `changed=true` when copying from a source (src) or destination (dest) data set that was in use (DISP=SHR). This change now displays an appropriate error message and returns `changed=false`. (https://github.com/ansible-collections/ibm_zos_core/pull/794).
- zos_copy - Reported a warning about the use of _play_context.verbosity.This change corrects the module action to prevent the warning message. (https://github.com/ansible-collections/ibm_zos_core/pull/806).
- zos_copy - Test case for recursive encoding directories reported a UTF-8 failure. This change ensures proper test coverage for nested directories and file permissions. (https://github.com/ansible-collections/ibm_zos_core/pull/806).
- zos_copy - Zos_copy did not encode inner content inside subdirectories once the source was copied to the destination. Fix now encodes all content in a source directory, including subdirectories. (https://github.com/ansible-collections/ibm_zos_core/pull/772).
- zos_copy - kept permissions on target directory when copy overwrote files. The fix now set permissions when mode is given. (https://github.com/ansible-collections/ibm_zos_core/pull/795)
- zos_data_set - Reported a failure caused when `present=absent` for a VSAM data set leaving behind cluster components. Fix introduces a new logical flow that will evaluate the volumes, compare it to the provided value and if necessary catalog and delete. (https://github.com/ansible-collections/ibm_zos_core/pull/791).
- zos_fetch - Reported a warning about the use of _play_context.verbosity.This change corrects the module action to prevent the warning message. (https://github.com/ansible-collections/ibm_zos_core/pull/806).
- zos_job_output - Error message did not specify the job not found. Fix now specifies the job_id or job_name being searched to ensure more information is given back to the user. (https://github.com/ansible-collections/ibm_zos_core/pull/747)
- zos_operator - Reported a failure caused by unrelated error response. Fix now gives a transparent response of the operator to avoid false negatives. (https://github.com/ansible-collections/ibm_zos_core/pull/762).

New Modules
-----------

- ibm.ibm_zos_core.zos_archive - Archive files and data sets on z/OS.
- ibm.ibm_zos_core.zos_unarchive - Unarchive files and data sets in z/OS.

v1.6.0
======

Release Summary
---------------

Release Date: '2023-06-23'
This changelog describes all changes made to the modules and plugins included
in this collection. The release date is the date the changelog is created.
For additional details such as required dependencies and availability review
the collections `release notes <https://ibm.github.io/z_ansible_collections_doc/ibm_zos_core/docs/source/release_notes.html>`__

Major Changes
-------------

- zos_volume_init - Introduces new module to handle volume (or minidisk) initialization. (https://github.com/ansible-collections/ibm_zos_core/pull/654)

Minor Changes
-------------

- Updated the text converter import from "from ansible.module_utils._text" to "from ansible.module_utils.common.text.converters" to remove warning".. warn Use ansible.module_utils.common.text.converters instead.". (https://github.com/ansible-collections/ibm_zos_core/pull/602)
- module_utils - job.py utility did not support positional wiled card placement, this enhancement uses `fnmatch` logic to support wild cards.
- zos_copy - Fixed a bug where the module would change the mode for a directory when copying into it the contents of another. (https://github.com/ansible-collections/ibm_zos_core/pull/723)
- zos_copy - was enhanced to keep track of modified members in a destination dataset, restoring them to their previous state in case of a failure. (https://github.com/ansible-collections/ibm_zos_core/pull/551)
- zos_data_set - add force parameter to enable member delete while pdse is in use (https://github.com/ansible-collections/ibm_zos_core/pull/718).
- zos_job_query - ansible module does not support positional wild card placement for `job_name1 or `job_id`. This enhancement allows embedded wildcards throughout the `job_name` and `job_id`. (https://github.com/ansible-collections/ibm_zos_core/pull/721)
- zos_lineinfile - would access data sets with exclusive access so no other task can read the data, this enhancement allows for a data set to be opened with a disposition set to share so that other tasks can access the data when option `force` is set to `true`. (https://github.com/ansible-collections/ibm_zos_core/pull/731)
- zos_tso_command - was enhanced to accept `max_rc` as an option. This option allows a non-zero return code to succeed as a valid return code. (https://github.com/ansible-collections/ibm_zos_core/pull/666)

Bugfixes
--------

- Fixed wrong error message when a USS source is not found, aligning with a similar error message from zos_blockinfile "{src} does not exist".
- module_utils - data_set.py - Reported a failure caused when cataloging a VSAM data set. Fix now corrects how VSAM data sets are cataloged. (https://github.com/ansible-collections/ibm_zos_core/pull/816).
- zos_blockinfile - was unable to use double quotes which prevented some use cases and did not display an approriate message. The fix now allows for double quotes to be used with the module. (https://github.com/ansible-collections/ibm_zos_core/pull/680)
- zos_copy - Encoding normalization used to handle newlines in text files was applied to binary files too. Fix makes sure that binary files bypass this normalization. (https://github.com/ansible-collections/ibm_zos_core/pull/810)
- zos_copy - Fixes a bug where files not encoded in IBM-1047 would trigger an error while computing the record length for a new destination dataset. Issue 664. (https://github.com/ansible-collections/ibm_zos_core/pull/743)
- zos_copy - Fixes a bug where the code for fixing an issue with newlines in files (issue 599) would use the wrong encoding for normalization. Issue 678. (https://github.com/ansible-collections/ibm_zos_core/pull/743)
- zos_copy - Reported a warning about the use of _play_context.verbosity.This change corrects the module action to prevent the warning message. (https://github.com/ansible-collections/ibm_zos_core/pull/814).
- zos_copy - kept permissions on target directory when copy overwrote files. The fix now set permissions when mode is given. (https://github.com/ansible-collections/ibm_zos_core/pull/790)
- zos_data_set - Reported a failure caused when `present=absent` for a VSAM data set leaving behind cluster components. Fix introduces a new logical flow that will evaluate the volumes, compare it to the provided value and if necessary catalog and delete. (https://github.com/ansible-collections/ibm_zos_core/pull/816).
- zos_encode - fixes a bug where converted files were not tagged afterwards with the new code set. (https://github.com/ansible-collections/ibm_zos_core/pull/534)
- zos_fetch - Reported a warning about the use of _play_context.verbosity.This change corrects the module action to prevent the warning message. (https://github.com/ansible-collections/ibm_zos_core/pull/814).
- zos_find - fixes a bug where find result values stopped being returned after first value in a list was 'not found'. (https://github.com/ansible-collections/ibm_zos_core/pull/668)
- zos_gather_facts - Fixes an issue in the zoau version checker which prevented the zos_gather_facts module from running with newer versions of ZOAU. (https://github.com/ansible-collections/ibm_zos_core/pull/797)
- zos_lineinfile - Fixed a bug where a Python f-string was used and thus removed to ensure support for Python 2.7 on the controller. (https://github.com/ansible-collections/ibm_zos_core/pull/659)

New Modules
-----------

- ibm.ibm_zos_core.zos_volume_init - Initialize volumes or minidisks.

v1.5.0
======

Release Summary
---------------

Release Date: '2023-04-21'
This changelog describes all changes made to the modules and plugins included
in this collection. The release date is the date the changelog is created.
For additional details such as required dependencies and availability review
the collections `release notes <https://ibm.github.io/z_ansible_collections_doc/ibm_zos_core/docs/source/release_notes.html>`__

Major Changes
-------------

- ibm_zos_core - Updates the entire collection in that the collection no longer depends on the managed node having installed System Display and Search Facility (SDSF). Remove SDSF dependency from ibm_zos_core collection. (https://github.com/ansible-collections/ibm_zos_core/pull/303).

Minor Changes
-------------

- module utility jobs - was updated to remove the usage of REXX and replaced with ZOAU python APIs. This reduces code replication and it removes the need for REXX interpretation which increases performance. (https://github.com/ansible-collections/ibm_zos_core/pull/312).
- module utils backup - updates the module with a new option named tmp_hlq. This allows for a user to specify the data set high level qualifier (HLQ) used in any temporary data set created by the module. Often, the defaults are not permitted on systems, this provides a way to override the defaults. (https://github.com/ansible-collections/ibm_zos_core/pull/341).
- module utils dd_statement- updates the module with a new option named tmp_hlq. This allows for a user to specify the data set high level qualifier (HLQ) used in any temporary data set created by the module. Often, the defaults are not permitted on systems, this provides a way to override the defaults. (https://github.com/ansible-collections/ibm_zos_core/pull/341).
- module utils encode - updates the module with a new option named tmp_hlq. This allows for a user to specify the data set high level qualifier (HLQ) used in any temporary data set created by the module. Often, the defaults are not permitted on systems, this provides a way to override the defaults. (https://github.com/ansible-collections/ibm_zos_core/pull/341).
- zos_apf - updates the module with a new option named tmp_hlq. This allows for a user to specify the data set high level qualifier (HLQ) used in any temporary data set created by the module. Often, the defaults are not permitted on systems, this provides a way to override the defaults. (https://github.com/ansible-collections/ibm_zos_core/pull/341).
- zos_blockinfile - fixes a bug when using double quotes in the block text of the module. When double quotes appeared in block text, the module would error differently depending on the usage of option insertafter. Examples of this error have return code 1 or 16 along with message "ZOAU dmod return content is NOT in json format" and a varying stderr. (https://github.com/ansible-collections/ibm_zos_core/pull/303).
- zos_blockinfile - updates the module with a new option named force. This allows for a user to specify that the data set can be shared with others during an update which results in the data set you are updating to be simultaneously updated by others. (https://github.com/ansible-collections/ibm_zos_core/pull/316).
- zos_blockinfile - updates the module with a new option named indentation. This allows for a user to specify a number of spaces to prepend to the content before being inserted into the destination. (https://github.com/ansible-collections/ibm_zos_core/pull/317).
- zos_blockinfile - updates the module with a new option named tmp_hlq. This allows for a user to specify the data set high level qualifier (HLQ) used in any temporary data set created by the module. Often, the defaults are not permitted on systems, this provides a way to override the defaults. (https://github.com/ansible-collections/ibm_zos_core/pull/341).
- zos_copy - updates the module with a new option named tmp_hlq. This allows for a user to specify the data set high level qualifier (HLQ) used in any temporary data set created by the module. Often, the defaults are not permitted on systems, this provides a way to override the defaults. (https://github.com/ansible-collections/ibm_zos_core/pull/341).
- zos_data_set - Ensures that temporary datasets created by zos_data_set use the tmp_hlq specified. This allows for a user to specify the data set high level qualifier (HLQ) used in any temporary data set created by the module. Often, the defaults are not permitted on systems, this provides a way to override the defaults. (https://github.com/ansible-collections/ibm_zos_core/pull/491).
- zos_encode - updates the module with a new option named tmp_hlq. This allows for a user to specify the data set high level qualifier (HLQ) used in any temporary data set created by the module. Often, the defaults are not permitted on systems, this provides a way to override the defaults. (https://github.com/ansible-collections/ibm_zos_core/pull/341).
- zos_fetch - updates the module with a new option named tmp_hlq. This allows for a user to specify the data set high level qualifier (HLQ) used in any temporary data set created by the module. Often, the defaults are not permitted on systems, this provides a way to override the defaults. (https://github.com/ansible-collections/ibm_zos_core/pull/341).
- zos_gather_facts - is a new module that can discover facts about the managed z/OS target. This module leverages the zinfo utility offered by ZOAU. (https://github.com/ansible-collections/ibm_zos_core/pull/322).
- zos_job_output - was updated to leverage the latest changes that removes the REXX code by calling the module utility jobs. (https://github.com/ansible-collections/ibm_zos_core/pull/312).
- zos_job_query - was updated to leverage the latest changes that removes the REXX code by calling the module utility jobs. (https://github.com/ansible-collections/ibm_zos_core/pull/312).
- zos_job_query - was updated to use the jobs module utility. (https://github.com/ansible-collections/ibm_zos_core/pull/312).
- zos_job_submit - The architecture changed such that the entire modules execution time now captured in the duration time which includes job submission and log collection. If a job does not return by the default 10 sec 'wait_time_s' value, it can be increased up to 86400 seconds. (https://github.com/ansible-collections/ibm_zos_core/issues/389).
- zos_job_submit - behavior changed when a volume is defined in the module options such that it will catalog the data set if it is not cataloged and submit the job. In the past, the function did not catalog the data set and instead performed I/O operations and then submitted the job. This behavior aligns to other module behaviors and reduces the possibility to encounter a permissions issue. (https://github.com/ansible-collections/ibm_zos_core/issues/389).
- zos_job_submit - was updated to include an additional error code condition JCLERR. (https://github.com/ansible-collections/ibm_zos_core/pull/312)
- zos_lineinfile - updates the module with a new option named tmp_hlq. This allows for a user to specify the data set high level qualifier (HLQ) used in any temporary data set created by the module. Often, the defaults are not permitted on systems, this provides a way to override the defaults. (https://github.com/ansible-collections/ibm_zos_core/pull/341).
- zos_mount - updates the module with a new option named tmp_hlq. This allows for a user to specify the data set high level qualifier (HLQ) used in any temporary data set created by the module. Often, the defaults are not permitted on systems, this provides a way to override the defaults. (https://github.com/ansible-collections/ibm_zos_core/pull/341).
- zos_mvs_raw - Ensures that temporary datasets created by DD Statements use the tmp_hlq specified. This allows for a user to specify the data set high level qualifier (HLQ) used in any temporary data set created by the module. Often, the defaults are not permitted on systems, this provides a way to override the defaults. (https://github.com/ansible-collections/ibm_zos_core/pull/414).
- zos_mvs_raw - updates the module with a new option named tmp_hlq. This allows for a user to specify the data set high level qualifier (HLQ) used in any temporary data set created by the module. Often, the defaults are not permitted on systems, this provides a way to override the defaults. (https://github.com/ansible-collections/ibm_zos_core/pull/341).
- zos_operator - added in the response the cmd result (https://github.com/ansible-collections/ibm_zos_core/issues/389).
- zos_operator - added in the response the elapsed time (https://github.com/ansible-collections/ibm_zos_core/issues/389).
- zos_operator - added in the response the wait_time_s set (https://github.com/ansible-collections/ibm_zos_core/issues/389).
- zos_operator - deprecated the wait option, not needed with wait_time_s minor_changes (https://github.com/ansible-collections/ibm_zos_core/issues/389).
- zos_operator - was updated to remove the usage of REXX and replaced with ZOAU python APIs. This reduces code replication and it removes the need for REXX interpretation which increases performance. (https://github.com/ansible-collections/ibm_zos_core/pull/312).

Deprecated Features
-------------------

- zos_encode - deprecates the module options `from_encoding` and `to_encoding` to use suboptions `from` and `to` in order to remain consistent with all other modules. (https://github.com/ansible-collections/ibm_zos_core/pull/345).
- zos_job_submit - Response 'message' property has been deprecated, all responses are now in response property 'msg'. (https://github.com/ansible-collections/ibm_zos_core/issues/389).
- zos_job_submit - The 'wait' option has been deprecated because using option 'wait_time_s' implies the job is going to wait. (https://github.com/ansible-collections/ibm_zos_core/issues/389).

Bugfixes
--------

- zos_copy - Copy failed from a loadlib member to another loadlib member. Fix now looks for error in stdout in the if statement to use -X option. (https://github.com/ansible-collections/ibm_zos_core/pull/641)
- zos_copy - Fixed a bug where the module would change the mode for a directory when copying into it the contents of another. (https://github.com/ansible-collections/ibm_zos_core/pull/746)
- zos_copy - Fixes a bug such that the module fails when copying files from a directory needing also to be encoded. The failure would also delete the `src` which was not desirable behavior. Fixes deletion of src on encoding error. (https://github.com/ansible-collections/ibm_zos_core/pull/321).
- zos_copy - Fixes a bug where copying a member from a loadlib to another loadlib fails. (https://github.com/ansible-collections/ibm_zos_core/pull/640)
- zos_copy - Fixes a bug where files not encoded in IBM-1047 would trigger an error while computing the record length for a new destination dataset. Issue 664. (https://github.com/ansible-collections/ibm_zos_core/pull/725)
- zos_copy - Fixes a bug where if a destination has accented characters in its content, the module would fail when trying to determine if it is empty. (https://github.com/ansible-collections/ibm_zos_core/pull/634)
- zos_copy - Fixes a bug where the code for fixing an issue with newlines in files (issue 599) would use the wrong encoding for normalization. Issue 678. (https://github.com/ansible-collections/ibm_zos_core/pull/725)
- zos_copy - Fixes a bug where the computed record length for a new destination dataset would include newline characters. (https://github.com/ansible-collections/ibm_zos_core/pull/620)
- zos_copy - Fixes wrongful creation of destination backups when module option `force` is true, creating emergency backups meant to restore the system to its initial state in case of a module failure only when force is false. (https://github.com/ansible-collections/ibm_zos_core/pull/590)
- zos_copy - module was updated to correct a bug in the case when the destination (dest) is a PDSE and the source (src) is a Unix Systems File (USS). The module would fail in determining if the PDSE actually existed and try to create it when it already existed resulting in an error that would prevent the module from correctly executing. (https://github.com/ansible-collections/ibm_zos_core/pull/327)
- zos_data_set - Fixes a bug such that the module will delete a catalogued data set over an uncatalogued data set even though the volume is provided for the uncataloged data set. This is unexpected behavior and does not align to documentation; correct behavior is that when a volume is provided that is the first place the module should look for the data set, whether or not it is cataloged. (https://github.com/ansible-collections/ibm_zos_core/pull/325).
- zos_data_set - Fixes a bug where the default record format FB was actually never enforced and when enforced it would cause VSAM creation to fail with a Dynalloc failure. Also cleans up some of the options that are set by default when they have no bearing for batch. (https://github.com/ansible-collections/ibm_zos_core/pull/647)
- zos_fetch - Updates the modules behavior when fetching VSAM data sets such that the maximum record length is now determined when creating a temporary data set to copy the VSAM data into and a variable-length (VB) data set is used. (https://github.com/ansible-collections/ibm_zos_core/pull/350)
- zos_job_output - Fixes a bug that returned all ddname's when a specific ddnamae was provided. Now a specific ddname can be returned and all others ignored. (https://github.com/ansible-collections/ibm_zos_core/pull/334)
- zos_job_query - was updated to correct a boolean condition that always evaluated to "CANCELLED". (https://github.com/ansible-collections/ibm_zos_core/pull/312).
- zos_job_submit - Fixes the issue when `wait_time_s` was set to 0 that would result in a `type` error that a stack trace would result in the response, issue 670. (https://github.com/ansible-collections/ibm_zos_core/pull/683)
- zos_job_submit - Fixes the issue when a job encounters a security exception no job log would would result in the response, issue 684. (https://github.com/ansible-collections/ibm_zos_core/pull/683)
- zos_job_submit - Fixes the issue when a job is configured for a syntax check using TYPRUN=SCAN that it would wait the full duration set by `wait_time_s` to return a response, issue 685. (https://github.com/ansible-collections/ibm_zos_core/pull/683)
- zos_job_submit - Fixes the issue when a job is configured for a syntax check using TYPRUN=SCAN that no job log would result in the response, issue 685. (https://github.com/ansible-collections/ibm_zos_core/pull/683)
- zos_job_submit - Fixes the issue when a job is purged by the system that a stack trace would result in the response, issue 681. (https://github.com/ansible-collections/ibm_zos_core/pull/683)
- zos_job_submit - Fixes the issue when invalid JCL syntax is submitted that a stack trace would result in the response, issue 623. (https://github.com/ansible-collections/ibm_zos_core/pull/683)
- zos_job_submit - Fixes the issue when resources (data sets) identified in JCL did not exist such that a stack trace would result in the response, issue 624. (https://github.com/ansible-collections/ibm_zos_core/pull/683)
- zos_job_submit - Fixes the issue where the response did not include the job log when a non-zero return code would occur, issue 655. (https://github.com/ansible-collections/ibm_zos_core/pull/683)
- zos_mount - Fixes option `tag_ccsid` to correctly allow for type int. (https://github.com/ansible-collections/ibm_zos_core/pull/511)
- zos_mvs_raw - module was updated to correct a bug when no DD statements were provided. The module when no option was provided for `dds` would error, a default was provided to correct this behavior. (https://github.com/ansible-collections/ibm_zos_core/pull/336)
- zos_operator - Fixes case sensitive error checks, invalid, error & unidentifiable (https://github.com/ansible-collections/ibm_zos_core/issues/389).
- zos_operator - Fixes such that specifying wait_time_s would throw an error (https://github.com/ansible-collections/ibm_zos_core/issues/389).
- zos_operator - Fixes the wait_time_s to default to 1 second (https://github.com/ansible-collections/ibm_zos_core/issues/389).
- zos_operator - fixed incorrect example descriptions and updated the doc to highlight the deprecated option `wait`. (https://github.com/ansible-collections/ibm_zos_core/pull/648)
- zos_operator - was updated to correct missing verbosity content when the option verbose was set to True. zos_operator - was updated to correct the trailing lines that would appear in the result content. (https://github.com/ansible-collections/ibm_zos_core/pull/400).

New Modules
-----------

- ibm.ibm_zos_core.zos_gather_facts - Gather z/OS system facts.

v1.4.1
======

Release Summary
---------------

Release Date: '2023-04-18'
This changelog describes all changes made to the modules and plugins included
in this collection. The release date is the date the changelog is created.
For additional details such as required dependencies and availability review
the collections `release notes <https://ibm.github.io/z_ansible_collections_doc/ibm_zos_core/docs/source/release_notes.html>`__

Bugfixes
--------

- zos_copy - Copy failed from a loadlib member to another loadlib member. Fix now looks for error in stdout in the if statement to use -X option. (https://github.com/ansible-collections/ibm_zos_core/pull/640)
- zos_copy - Fixed a bug where the module would change the mode for a directory when copying into it the contents of another. (https://github.com/ansible-collections/ibm_zos_core/pull/742)
- zos_copy - Fixes a bug where files not encoded in IBM-1047 would trigger an error while computing the record length for a new destination dataset. Issue 664. (https://github.com/ansible-collections/ibm_zos_core/pull/732)
- zos_copy - Fixes a bug where the code for fixing an issue with newlines in files (issue 599) would use the wrong encoding for normalization. Issue 678. (https://github.com/ansible-collections/ibm_zos_core/pull/732)
- zos_copy - fixed wrongful creation of destination backups when module option `force` is true, creating emergency backups meant to restore the system to its initial state in case of a module failure only when force is false. (https://github.com/ansible-collections/ibm_zos_core/pull/590)
- zos_copy - fixes a bug where the computed record length for a new destination dataset would include newline characters. (https://github.com/ansible-collections/ibm_zos_core/pull/620)
- zos_job_query - fixes a bug where a boolean was not being properly compared. (https://github.com/ansible-collections/ibm_zos_core/pull/379)

v1.4.0
======

Release Summary
---------------

Release Date: '2022-12-07'
This changelog describes all changes made to the modules and plugins included
in this collection. The release date is the date the changelog is created.
For additional details such as required dependencies and availability review
the collections `release notes <https://ibm.github.io/z_ansible_collections_doc/ibm_zos_core/docs/source/release_notes.html>`__

Major Changes
-------------

- zos_copy was updated to support the ansible.builtin.ssh connection options; for further reference refer to the SSH plugin documentation.
- zos_copy was updated to take into account the record length when the source is a USS file and the destination is a data set with a record length. This is done by inspecting the destination data set attributes and using these attributes to create a new data set.
- zos_copy was updated with the capabilities to define destination data sets from within the zos_copy module. In the case where you are copying to a data set destination that does not exist, you can now do so using the new zos_copy module option destination.
- zos_fetch was updated to support the ansible.builtin.ssh connection options; for further reference refer to the SSH plugin documentation.
- zos_job_output was updated to to include the completion code (CC) for each individual job step as part of the ret_code response.
- zos_job_query was updated to handle when an invalid job ID or job name is used with the module and returns a proper response.
- zos_job_query was updated to support a 7 digit job number ID for when there are greater than 99,999 jobs in the history.
- zos_job_submit was enhanced to check for 'JCL ERROR' when jobs are submitted and result in a proper module response.
- zos_job_submit was updated to fail fast when a submitted job fails instead of waiting a predetermined time.
- zos_operator_action_query response messages were improved with more diagnostic information in the event an error is encountered.
- zos_ping was updated to remove the need for the zos_ssh connection plugin dependency.

Minor Changes
-------------

- zos_copy - enhanced the force option when `force=true` and the remote file or data set `dest` is NOT empty, the `dest` will be deleted and recreated with the `src` data set attributes, otherwise it will be recreated with the `dest` data set attributes. (https://github.com/ansible-collections/ibm_zos_core/pull/306)
- zos_copy - enhanced to optimize how it captures the permission bits state for the `dest`. This change now reviews the source files instead of traversing the entire `dest` path. (https://github.com/ansible-collections/ibm_zos_core/pull/561)
- zos_copy - enhanced to support creating a parent directory when it does not exist in the `dest` path. Prior to this change, if a parent directory anywhere in the path did not exist the task would fail as it was stated in documentation. (https://github.com/ansible-collections/ibm_zos_core/pull/561)
- zos_copy - enhanced to support system symbols in PARMLIB. System symbols are elements that allow different z/OS® systems to share PARMLIB definitions while retaining unique values in those definitions. This was fixed in a future release through the use of one of the ZOAU dependency but this version of `ibm_zos_core` does not support that dependency version so this support was added. (https://github.com/ansible-collections/ibm_zos_core/pull/566)
- zos_copy - fixes a bug that when a directory is copied from the controller to the managed node and a mode is set, the mode is applied to the directory on the managed node. If the directory being copied contains files and mode is set, mode will only be applied to the files being copied not the pre-existing files. (https://github.com/ansible-collections/ibm_zos_core/pull/306)
- zos_copy - fixes a bug where options were not defined in the module argument spec that will result in error when running `ansible-core` v2.11 and using options `force` or `mode`. (https://github.com/ansible-collections/ibm_zos_core/pull/496)
- zos_copy - introduced an updated creation policy referred to as precedence rules such that if `dest_data_set` is set, this will take precedence. If `dest` is an empty data set, the empty data set will be written with the expectation its attributes satisfy the copy. If no precedent rule has been exercised, `dest` will be created with the same attributes of `src`. (https://github.com/ansible-collections/ibm_zos_core/pull/306)
- zos_copy - introduced new computation capabilities such that if `dest` is a nonexistent data set, the attributes assigned will depend on the type of `src`. If `src` is a USS file, `dest` will have a Fixed Block (FB) record format and the remaining attributes will be computed. If `src` is binary, `dest` will have a Fixed Block (FB) record format with a record length of 80, block size of 32760, and the remaining attributes will be computed. (https://github.com/ansible-collections/ibm_zos_core/pull/306)
- zos_copy - option `dest_dataset` has been deprecated and removed in favor of the new option `dest_data_set`. (https://github.com/ansible-collections/ibm_zos_core/pull/306)
- zos_copy - was enhanced for when `src` is a directory and ends with "/", the contents of it will be copied into the root of `dest`. It it doesn't end with "/", the directory itself will be copied. (https://github.com/ansible-collections/ibm_zos_core/pull/496)

Deprecated Features
-------------------

- zos_copy and zos_fetch option sftp_port has been deprecated. To set the SFTP port, use the supported options in the ansible.builtin.ssh plugin. Refer to the `SSH port <https://docs.ansible.com/ansible/latest/collections/ansible/builtin/ssh_connection.html#parameter-port>`__ option to configure the port used during the modules SFTP transport.
- zos_copy module option model_ds has been removed. The model_ds logic is now automatically managed and data sets are either created based on the src data set or overridden by the new option destination_dataset.
- zos_ssh connection plugin has been removed, it is no longer required. You must remove all playbook references to connection ibm.ibm_zos_core.zos_ssh.

Bugfixes
--------

- zos_copy - fixes a bug that did not create a data set on the specified volume. (https://github.com/ansible-collections/ibm_zos_core/pull/306)
- zos_copy - fixes a bug where a number of attributes were not an option when using `dest_data_set`. (https://github.com/ansible-collections/ibm_zos_core/pull/306)
- zos_job_output - fixes a bug that returned all ddname's when a specific ddname was provided. Now a specific ddname can be returned and all others ignored. (https://github.com/ansible-collections/ibm_zos_core/pull/507)
- zos_job_output was updated to correct possible truncated responses for the ddname content. This would occur for jobs with very large amounts of content from a ddname.
- zos_mount - fixed option `tag_ccsid` to correctly allow for type int. (https://github.com/ansible-collections/ibm_zos_core/pull/502)
- zos_operator - enhanced to allow for MVS operator `SET` command, `SET` is equivalent to the abbreviated `T` command. (https://github.com/ansible-collections/ibm_zos_core/pull/501)
- zos_ssh - connection plugin was updated to correct a bug in Ansible that
    would result in playbook task retries overriding the SSH connection
    retries. This is resolved by renaming the zos_ssh option
    retries to reconnection_retries. The update addresses users of
    ansible-core v2.9 which continues to use retries and users of
    ansible-core v2.11 or later which uses reconnection_retries.
    This also resolves a bug in the connection that referenced a deprecated
    constant. (https://github.com/ansible-collections/ibm_zos_core/pull/328)

New Modules
-----------

- ibm.ibm_zos_core.zos_mount - Mount a z/OS file system.

v1.3.6
======

Release Summary
---------------

Release Date: '2022-10-07'
This changelog describes all changes made to the modules and plugins included
in this collection. The release date is the date the changelog is created.
For additional details such as required dependencies and availability review
the collections `release notes <https://ibm.github.io/z_ansible_collections_doc/ibm_zos_core/docs/source/release_notes.html>`__ 

Minor Changes
-------------

- zos_copy - was enhanced for when `src` is a directory and ends with "/", the contents of it will be copied into the root of `dest`. If it doesn't end with "/", the directory itself will be copied. (https://github.com/ansible-collections/ibm_zos_core/pull/515)

Bugfixes
--------

- jobs.py - fixes a utility used by module `zos_job_output` that would truncate the DD content. (https://github.com/ansible-collections/ibm_zos_core/pull/462)
- zos_copy - fixes a bug that when a directory is copied from the controller to the managed node and a mode is set, the mode is now applied to the directory on the controller. If the directory being copied contains files and mode is set, mode will only be applied to the files being copied not the pre-existing files.(https://github.com/ansible-collections/ibm_zos_core/pull/462)
- zos_copy - fixes a bug where options were not defined in the module argument spec that will result in error when running `ansible-core` 2.11 and using options `force` or `mode`. (https://github.com/ansible-collections/ibm_zos_core/pull/462)
- zos_fetch - fixes a bug where an option was not defined in the module argument spec that will result in error when running `ansible-core` 2.11 and using option `encoding`. (https://github.com/ansible-collections/ibm_zos_core/pull/462)
- zos_job_submit - fixes a bug where an option was not defined in the module argument spec that will result in error when running `ansible-core` 2.11 and using option `encoding`. (https://github.com/ansible-collections/ibm_zos_core/pull/462)
- zos_ssh - fixes connection plugin which will error when using `ansible-core` 2.11 with an `AttributeError module 'ansible.constants' has no attribute 'ANSIBLE_SSH_CONTROL_PATH_DIR'`. (https://github.com/ansible-collections/ibm_zos_core/pull/462)
- zos_ssh - fixes connection plugin which will error when using `ansible-core` 2.11 with an `AttributeError module 'ansible.constants' has no attribute 'ANSIBLE_SSH_CONTROL_PATH_DIR'`. (https://github.com/ansible-collections/ibm_zos_core/pull/513)

v1.3.5
======

Release Summary
---------------

Release Date: '2022-03-06'
This changlelog describes all changes made to the modules and plugins included
in this collection.
For additional details such as required dependencies and availablity review
the collections `release notes <https://ibm.github.io/z_ansible_collections_doc/ibm_zos_core/docs/source/release_notes.html>`__ 

Bugfixes
--------

- zos_ssh - connection plugin was updated to correct a bug in Ansible that
    would result in playbook task retries overriding the SSH connection
    retries. This is resolved by renaming the zos_ssh option
    retries to reconnection_retries. The update addresses users of
    ansible-core v2.9 which continues to use retries and users of
    ansible-core v2.11 or later which uses reconnection_retries.
    This also resolves a bug in the connection that referenced a deprecated
    constant. (https://github.com/ansible-collections/ibm_zos_core/pull/328)

v1.3.3
======

Release Summary
---------------

Release Date: '2022-26-04'
This changlelog describes all changes made to the modules and plugins included
in this collection.
For additional details such as required dependencies and availablity review
the collections `release notes <https://ibm.github.io/z_ansible_collections_doc/ibm_zos_core/docs/source/release_notes.html>`__ 

Bugfixes
--------

- zos_copy was updated to correct deletion of all temporary files and unwarranted deletes. - When the module would complete, a cleanup routine did not take into account that other processes had open temporary files and thus would error when trying to remove them. - When the module would copy a directory (source) from USS to another USS directory (destination), any files currently in the destination would be deleted. The modules behavior has changed such that files are no longer deleted unless the force option is set to true. When **force=true**, copying files or a directory to a USS destination will continue if it encounters existing files or directories and overwrite any corresponding files.
- zos_job_query was updated to correct a boolean condition that always evaluated to "CANCELLED". - When querying jobs that are either **CANCELLED** or have **FAILED**, they were always treated as **CANCELLED**.

v1.3.1
======

Release Summary
---------------

Release Date: '2022-27-04'
This changlelog describes all changes made to the modules and plugins included
in this collection.
For additional details such as required dependencies and availablity review
the collections `release notes <https://ibm.github.io/z_ansible_collections_doc/ibm_zos_core/docs/source/release_notes.html>`__ 

Bugfixes
--------

- zos_ping was updated to support Automation Hub documentation generation.
- zos_ssh connection plugin was updated to prioritize the execution of modules written in REXX over other implementations such is the case for zos_ping.

Known Issues
------------

- When executing programs using zos_mvs_raw, you may encounter errors that originate in the implementation of the programs. Two such known issues are noted below of which one has been addressed with an APAR. - zos_mvs_raw module execution fails when invoking Database Image Copy 2 Utility or Database Recovery Utility in conjunction with FlashCopy or Fast Replication. - zos_mvs_raw module execution fails when invoking DFSRRC00 with parm "UPB,PRECOMP", "UPB, POSTCOMP" or "UPB,PRECOMP,POSTCOMP". This issue is addressed by APAR PH28089.

v1.3.0
======

Release Summary
---------------

Release Date: '2021-19-04'
This changlelog describes all changes made to the modules and plugins included
in this collection.
For additional details such as required dependencies and availablity review
the collections `release notes <https://ibm.github.io/z_ansible_collections_doc/ibm_zos_core/docs/source/release_notes.html>`__ 

`New Playbooks <https://github.com/IBM/z_ansible_collections_samples>`__
  - Authorize and synchronize APF authorized libraries on z/OS from a configuration file cloned from GitHub
  - Automate program execution with copy, sort and fetch data sets on z/OS playbook.
  - Automate user management with add, remove, grant permission, generate
    passwords, create zFS, mount zFS and send email notifications when deployed
    to Ansible Tower or AWX with the manage z/OS Users Using Ansible playbook.
  - Use the configure Python and ZOAU Installation playbook to scan the
    **z/OS** target to find the latest supported configuration and generate
    inventory and a variables configuration.
  - Automate software management with SMP/E Playbooks

Minor Changes
-------------

- All modules support relative paths and remove choice case sensitivity.
- zos_data_set added support to allocate and format zFS data sets.
- zos_operator supports new options **wait** and **wait_time_s** such that you can specify that zos_operator wait the full **wait_time_s** or return as soon as the first operator command executes.

Bugfixes
--------

- Action plugin zos_copy was updated to support Python 2.7.
- Job utility is an internal library used by several modules. It has been updated to use a custom written parsing routine capable of handling special characters to prevent job related reading operations from failing when a special character is encountered.
- Module zos_copy was updated to fail gracefully when a it encounters a non-zero return code.
- Module zos_copy was updated to support copying data set members that are program objects to a PDSE. Prior to this update, copying data set members would yield an error; - FSUM8976 Error writing <src_data_set_member> to PDSE member <dest_data_set_member>
- Module zos_job_submit referenced a non-existent option and was corrected to **wait_time_s**.
- Module zos_job_submit was updated to remove all trailing **\r** from jobs that are submitted from the controller.
- Module zos_tso_command support was added for when the command output contained special characters.
- Playbook zos_operator_basics.yaml has been updated to use end in the WTO reply over the previous use of cancel. Using cancel is not a valid reply and results in an execution error.

Known Issues
------------

- When executing programs using zos_mvs_raw, you may encounter errors that originate in the implementation of the programs. Two such known issues are noted below of which one has been addressed with an APAR. - zos_mvs_raw module execution fails when invoking Database Image Copy 2 Utility or Database Recovery Utility in conjunction with FlashCopy or Fast Replication. - zos_mvs_raw module execution fails when invoking DFSRRC00 with parm "UPB,PRECOMP", "UPB, POSTCOMP" or "UPB,PRECOMP,POSTCOMP". This issue is addressed by APAR PH28089.

New Modules
-----------

- ibm.ibm_zos_core.zos_apf - Add or remove libraries to Authorized Program Facility (APF)
- ibm.ibm_zos_core.zos_backup_restore - Backup and restore data sets and volumes
- ibm.ibm_zos_core.zos_blockinfile - Manage block of multi-line textual data on z/OS
- ibm.ibm_zos_core.zos_data_set - Manage data sets
- ibm.ibm_zos_core.zos_find - Find matching data sets

v1.2.1
======

Release Summary
---------------

Release Date: '2020-10-09'
This changlelog describes all changes made to the modules and plugins included
in this collection.
For additional details such as required dependencies and availablity review
the collections `release notes <https://ibm.github.io/z_ansible_collections_doc/ibm_zos_core/docs/source/release_notes.html>`__.

Beginning this release, all playbooks previously included with the collection
will be made available on the `playbook repository <https://github.com/IBM/z_ansible_collections_samples>`__.

Minor Changes
-------------

- Documentation related to configuration has been migrated to the `playbook repository <https://github.com/IBM/z_ansible_collections_samples>`__
- Python 2.x support

Bugfixes
--------

- zos_copy - fixed regex support, dictionary merge operation fix
- zos_encode - removed TemporaryDirectory usage.
- zos_fetch - fix quote import

New Modules
-----------

- ibm.ibm_zos_core.zos_lineinfile - Manage textual data on z/OS

v1.1.0
======

Release Summary
---------------

Release Date: '2020-26-01'
This changlelog describes all changes made to the modules and plugins included
in this collection.
For additional details such as required dependencies and availablity review
the collections `release notes <https://ibm.github.io/z_ansible_collections_doc/ibm_zos_core/docs/source/release_notes.html>`__

Minor Changes
-------------

- Documentation updates
- Improved error handling and messages
- New Filter that will filter a list of WTOR messages based on message text.

New Modules
-----------

- ibm.ibm_zos_core.zos_encode - Perform encoding operations.
- ibm.ibm_zos_core.zos_fetch - Fetch data from z/OS
- ibm.ibm_zos_core.zos_mvs_raw - Run a z/OS program.
- ibm.ibm_zos_core.zos_operator - Execute operator command
- ibm.ibm_zos_core.zos_operator_action_query - Display messages requiring action
- ibm.ibm_zos_core.zos_ping - Ping z/OS and check dependencies.
- ibm.ibm_zos_core.zos_tso_command - Execute TSO commands

v1.0.0
======

Release Summary
---------------

Release Date: '2020-18-03'
This changlelog describes all changes made to the modules and plugins included
in this collection.
For additional details such as required dependencies and availablity review
the collections `release notes <https://ibm.github.io/z_ansible_collections_doc/ibm_zos_core/docs/source/release_notes.html>`__ 

Minor Changes
-------------

- Documentation updates
- Module zos_data_set catalog support added

Security Fixes
--------------

- Improved test, security and injection coverage
- Security vulnerabilities fixed

New Modules
-----------

- ibm.ibm_zos_core.zos_copy - Copy data to z/OS
- ibm.ibm_zos_core.zos_job_output - Display job output
- ibm.ibm_zos_core.zos_job_query - Query job status
- ibm.ibm_zos_core.zos_job_submit - Submit JCL
