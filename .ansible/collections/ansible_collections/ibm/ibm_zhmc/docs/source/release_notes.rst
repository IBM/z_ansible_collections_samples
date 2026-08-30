.. Copyright 2017-2020 IBM Corp. All Rights Reserved.
..
.. Licensed under the Apache License, Version 2.0 (the "License");
.. you may not use this file except in compliance with the License.
.. You may obtain a copy of the License at
..
..    http://www.apache.org/licenses/LICENSE-2.0
..
.. Unless required by applicable law or agreed to in writing, software
.. distributed under the License is distributed on an "AS IS" BASIS,
.. WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
.. See the License for the specific language governing permissions and
.. limitations under the License.
..


.. _`Releases`:

Releases
========


Version 1.2.1
-------------

Released: 2022-12-06

Availability: `AutomationHub`_, `Galaxy`_, `GitHub`_

**Bug fixes:**

* Unpinned Ansible again. It was pinned in version 1.2.0 on each Python version
  to a different Ansible version in order to broaden the test coverage. The
  test coverage across Ansible versions is now defined separately from the
  Ansible versions required for installing the collection.


Version 1.2.0
-------------

This version contains all fixes up to version 1.1.1.

Released: 2022-12-06

Availability: `AutomationHub`_, `Galaxy`_, `GitHub`_

**Bug fixes:**

* Fixed that every module invocation created an additional log handler, thus
  duplicating log entries. This only affected the end2end tests, but not when
  used in Ansible playbooks. (issue #552)

* In the zhmc_partition module, fixed that the artificial property
  'boot-storage-volume-name' was not included in the result.
  (related to issue #550)

* In the zhmc_partition module, fixed the support for check mode and added
  tests. (issue #550)

* In the zhmc_partition module, added missing z14, z15 and z16 input properties:
  'boot_storage_volume', 'boot_storage_volume_name', 'boot_load_parameters',
  'permit_ecc_key_import_functions', 'ssc_ipv6_gateway', 'secure_boot',
  'secure_execution', 'storage_group_uris', 'tape_link_uris',
  'partition_link_uris', 'available_features_list'. (related to issue #550)

* Test: Added missing z14 partition properties to the mock definition file
  tests/end2end/mocked_hmc_z14.yaml. (related to issue #550)

* Fixed a flake8 AttributeError when using importlib-metadata 5.0.0 on
  Python >=3.7, by pinning importlib-metadata to <5.0.0 on these Python
  versions.

* Temporarily disabled the sanity tests on all Ansible 7 (ansible-core 2.14)
  test environments. See issue #579 for the overall issue.

* Improved error handling when the zhmcclient_mock module is missing.
  (issue #574)

* Made the zhmc_adapter module tolerant against unconfigured FICON adapters
  to avoid HTTP error 404,4 "Get for Storage Port Properties is not supported
  for this card type". (issue #580)

* Made the zhmc_user module tolerant against unusual cases such as local
  auth without password rule. (issue #564)

* Updated the set of supported Ansible versions listed in the Installation
  section of the documentation to add recent Ansible versions up to Ansible 7.

**Enhancements:**

* Added a new 'zhmc_partition_list' Ansible module for listing partitions on
  CPCs in DPM mode. This speeds up execution time compared to obtaining them
  from the facts returned by 'zhmc_cpc'. (issue #526)

* Added support for Ansible 6.0.0 by adding an ignore-2.13.txt file to the
  sanity tests. (issue #533)

* Added a new make target 'end2end_mocked' that runs the end2end
  tests against mock environments defined with a new HMC inventory file
  (mocked_inventory.yaml) and a new HMC vault file (mocked_vault.yaml),
  and new mock files mocked_z14_classic.yaml and mocked_z14_dpm.yaml.
  (part of issue #396)

* Increased the minimum version of zhmcclient to 1.3.3, in order to pick
  up fixes. (part of issue #396)

* Added a new module 'zhmc_password_rule' that supports creating/updating,
  deleting, and gathering facts of a password rule on the HMC. (issue #363)

* Added a new module 'zhmc_password_rule_list' that supports listing the names
  of password rules on the HMC. (issue #363)

* Added the end2end_mocked tests to the coverage data reported to coveralls.io.

* Added a new module 'zhmc_user_role' that supports creating/updating,
  deleting, and gathering facts of a user role on the HMC. (issue #362)

* Added a new module 'zhmc_user_role_list' that supports listing the names
  of user roles on the HMC. (issue #362)

* Merged function tests into end2end tests to remove duplicate test cases.

* Removed the restriction that the zhmc_partition_list and zhmc_lpar_list
  modules were supported only with HMC versions 2.14.0 and newer. These modules
  are now supprted with all HMC versions (issue #549)

* Removed the restriction that the 'se-version' property in the result of the
  zhmc_partition_list and zhmc_lpar_list modules was provided only with HMC
  versions 2.14.1 and newer. The property is now provided with all HMC versions.
  (issue #549)

* Added support for 'reset_clear' and 'reset_normal' state in the zhmc_lpar
  module to support the "Reset Clear" and "Reset Normal" HMC operations.
  Along with that, added support for a new optional 'os_ipl_token' input
  parameter to support the respective HMC operation parameter.
  (issue #556)

* Added a new 'zhmc_adapter_list' Ansible module for listing adapters on
  CPCs in DPM mode. This speeds up execution time compared to obtaining them
  from the facts returned by 'zhmc_cpc'. (issue #576)

* Improved the error handling of the zhmc_user module when specified
  user roles, user patterns, password rules, or LDAP server definitions
  do not exist. (related to issue #564)

* Increased the set of tested Ansible versions to now include all major versions
  that are supported, from Ansible 2.9 to Ansible 7.

* Added tests for Python 3.11.

* Simplified the publishing of the collection.


* Stated support for the classic-mode only machine generations z196 / z114 /
  zEC12 / zBC12.

* Stated support for machine generation z16 / LinuxONE 4.

* Upgraded zhmcclient to 1.5.0 to pick up fixes.


**Cleanup:**

* Clarified the description of input parameters of the zhmc_lpar module.
  (part of issue #556)


Version 1.1.0
-------------

This version contains all fixes up to version 1.0.3.

Released: 2022-06-01

Availability: `AutomationHub`_, `Galaxy`_, `GitHub`_

**Bug fixes:**

* Added a tag 'infrastructure' to the collection metadata (tags field in
  galaxy.yml) - Ansible Automation Hub requires at least one tag from a
  standard tag list to be specified.

* Added "make check" for running "flake8" since the "pep8" that is run as
  part of the ansible sanity test does not find some issues.
  Resolved those new issues.

* Removed the "tools" directory from the temporary archive built for the sanity
  test, and removed the ignore statements for "tools/os_setup.sh" from the
  ignore files because the sanity test on AutomationHub tests against the
  uploaded archive which does not have that script.

* Fixed the use of incorrectly named attributes and methods in the zhmc_user
  module, and made the module result in check mode consistent with non-check
  mode. (issue #507)

* Test: Added missing env.vars in the pytest invocation for end2end tests.

* Test: Added missing optional module parameters in the end2end tests.

* Test: Added support for specifying 'hmc_auth.ca_certs' and 'hmc_auth.verify'
  from the 'hmc_verify_cert' parameter in the HMC definition file in
  end2end test cases for zhmc_partition and zhmc_user.

* Docs: Fixed incorrect input property names in zhmc_user module.
  (part of issue #514)

* Test: Fixed failure of sanity test on Python 3.6 due to new
  CryptographyDeprecationWarning raised by ansible, by pinning cryptography
  to <37.0.0 on Python 3.6. (issue #518)

* 'zhmc_user' module: Fixed an error for users with LDAP authentication.

* 'zhmc_user' module: Fixed incorrect default properties for users created in
  check mode.

* Increased minimum version of zhmcclient from 1.2.0 to 1.3.0 in order to
  pick up fixes and new functionality.

**Enhancements:**

* Test: Made end2end testing compatible with zhmcclient.testutils support using
  an Ansible compatible HMC inventory file and an Ansible compatible HMC vault
  file.
  The default HMC inventory file is now ~/.zhmc_inventory.yaml and can be
  changed using the TESTINVENTORY env. var.
  The default HMC vault file is now ~/.zhmc_vault.yaml and can be
  changed using the TESTVAULT env. var.
  The default HMC or group to run the end2end tests against is now 'default'
  and can be changed using the TESTHMC env. var.

* Test: Added support for a TESTCASES env.var for filtering testcases with the
  pytest -k option.

* Added support for specifying user roles as input in the zhmc_user module.
  User roles can now be specified with their names. They had been displayed
  on users before. (issue #514)

* Removed check in zhmc_user module for required input properties 'type' and
  'authentication_type' because for updating existing users they are not
  needed, and for creating new users, the HMC checks these.
  (part of issue #514)


Version 1.0.0
-------------

This version contains all fixes up to version 0.10.1.

Released: 2022-04-08

Availability: `Galaxy`_, `GitHub`_

**Bug fixes:**

* Fixed new Pylint issues reported by Pylint 2.9 and 2.10.

* Improved handling of exceptions when creation of zhmcclient.Session fails.
  (issue #451)

* Added support for Python 3.10, but needed to exclude the Ansible sanity
  test for the time being, since it does not yet support Python 3.10.

* Increased the minimum versions of the requests, cryptography, and PyYAML
  packages due to fixes requires for Python 3.10, and also due to the new
  package dependency resolver in Pip.

* Added support for Ansible 5.0.

* Increased minimum version of zhmcclient from 0.31.0 to 1.2.0 in order to
  pick up fixes and new functionality.

* Docs: Increased minimum version of Sphinx to 4.1.0 to fix an issue with
  renamed filters in Jinja2 3.1.0.

* Docs/dev: Pinned voluptous to <0.13.0 on Python 2.7. Increased sphinx-versions
  to 1.1.3.post-am2 for fix for Click 8.1.0. (issue #488)

**Enhancements:**

* Added a new zhmc_lpar Ansible module for managing LPARs on CPCs in classic
  mode. (issue #418)

* Added state values 'active' and 'inactive' to the zhmc_cpc Ansible module
  for activating/starting and deactivating/stopping CPCs in their current
  operational mode. (issue #418)


Version 0.10.0
--------------

This version contains all fixes up to version 0.9.2.

Released: 2021-06-17

Availability: `Galaxy`_, `GitHub`_

**Incompatible changes:**

* The new support for verifying HMC certificates will by default verify the
  HMC certificate using the "Mozilla CA Certificate List" provided by the
  'certifi' Python package, causing self-signed HMC certificates to be
  rejected. The verification behavior can be controlled with the new
  'ca_certs' and 'verify' sub-parameters of the 'hmc_auth' module parameter
  of each module.

**Bug fixes:**

* Docs: In the development section of the docs, fixes and improvements for the
  descriptions of releasing a version and starting a new version (issues #344
  and #345).

* Docs: The docs is now always built from the master branch, and the versions
  to be generated is now automatically determined from the Git tags and branches.
  This fixes a possible inconsistency in the versions included and build
  parameters used, between stable branch and master branch (issue #350).

* Mitigated the coveralls HTTP status 422 by pinning coveralls-python to
  <3.0.0.

* Fixed the condition for whether to run the Ansible sanity test and fixed
  issues reported by it. (issue #377 and others)

* Docs: Fixed the text for the Ansible Module Index in the bibliography to
  state it applies to Ansible 2.9 and fixed the link to reference the 2.9
  version instead of the latest version. Added a bibliography entry for the
  Ansible Collection Index for Ansible 2.10 and later.

* Docs: Pinned Sphinx to <4.0 to circumvent the issue that sphinx-versions
  uses the deprecated Sphinx.add_stylesheet() method that was removed in
  Sphinx 4.0. (issue #402)

* Test: Added sanity test ignore file for ansible-core 2.11 and fixed some
  Pylint issues to pass the test.

* Docs: Fixed link to ibm_zhmc samples playbooks.

* Docs: Fixed error during automatic docs build when two PRs are merged to
  master shortly one after another. The last one finishing the docs build now
  wins. Since PRs are merged in the order earlier first, their docs build should
  also finish first. (issue #417)

* Docs: Fixed instructions to release a version to cover for the case where
  the docs build does not show the new verison in the release notes.

**Enhancements:**

* Docs: The idempotency of each module and possible limitations are now
  described for each module. (issue #375)

* Increased minimum version of zhmcclient to 0.31.0 in order to have
  the support for certificate verification and to pick up fixes.

* Added support for verifying HMC certificates by adding module sub-parameters
  'ca_certs' and 'verify' to the 'hmc_auth' module parameter of all modules.
  (issue #401)

* Changed module input parameter 'hmc_auth.userid' to no longer be hidden in
  logs, for better debugging. The password is still hidden in any logs.

* Docs: Stated that ansible-core 2.11 is supported.

* Increased the minimum version of zhmcclient to 0.31.0.

**Cleanup:**

* Renamed "Bibliography" page to "Resources" and removed common Ansible links
  from that page to better fit the unified documentation for the IBM Z
  collections.

* Accomodated the immutable properties introduced with zhmcclient 0.31.0.

* Docs: The documentation is now built for all versions since 0.9.0 and for
  the master branch. This change added the update versions before the latest
  update version within each minor version, and removed the latest stable branch
  stable_M.N.


Version 0.9.0
-------------

This version contains all fixes up to version 0.8.3.

Released: 2020-12-14

Availability: `Galaxy`_, `GitHub`_

**Incompatible changes:**

* Starting with version 0.9.0, the zhmc Ansible modules are no longer distributed
  as the
  `zhmc-ansible-modules package on Pypi <https://pypi.org/project/zhmc-ansible-modules/>`_,
  but as the
  `ibm.ibm_zhmc collection on Ansible Galaxy <https://galaxy.ansible.com/ibm/ibm_zhmc/>`_.
  The installation of the zhmc Ansible modules is now done with::

    ansible-galaxy collection install ibm.ibm_zhmc

  Playbooks using the zhmc Ansible modules do not need to be changed, other
  than adding a "collections" property that includes the "ibm.ibm_zhmc"
  collection::

    ---
    - hosts: localhost
      collections:
      - ibm.ibm_zhmc
      tasks:
      - ...

* Fixed the 'version_added' field in the module description to no longer
  indicate the version of this module collection package, but instead the
  minimum Ansible version supported, consistent with the definition of that
  field. Since Ansible Galaxy supports Ansible 2.9 and above, the field
  now shows 2.9 for all modules.

**Bug fixes:**

* Increased minimum version of flake8 to 3.7.0 due to difficulties with
  recognizing certain 'noqa' statements. This required explicitly specifying
  its dependent pycodestyle and pyflakes packages with their minimum versions,
  because the dependency management did not work with our minimum
  package versions.

* Fixed issues with parameters in exception messages raised in
  zhmc_storage_group and zhmc_user.

* Fixed AttributeError when using the zhmc_adapter module to create a
  HiperSockets adapter. (see issue #141)

* Fixed ParameterError raised when creating NICs on CNA adapter ports.

* Docs: In the description of the module return data, added samples and
  fixed errors in the described structure of return data for the modules
  `zhmc_adapter`, `zhmc_cpc`, `zhmc_storage_group` and `zhmc_user`.

**Enhancements:**

* Added end2end test support, against real HMCs.

* Added a new module `zhmc_user` for managing users on the HMC.

* Dropped the use of pbr for this package.

* Added support for Python 3.7 and 3.8, dropped support for Python 3.4.
  Removed old circumventions for Travis issues.

* Updated maintainer list.

* Promoted package from Alpha to Beta and status of modules from preview to
  stable.

* In the zhmc_nic module, updated the definition of NIC properties to the z15
  machine generation. This makes the 'mac_address' property writeable, and adds
  the 'vlan_type', 'function_number' and 'function_range' properties.

* Added support in the zhmc_crypto_attachment module for specifying crypto
  adapters by name instead of just their count. (See issue #187)

* Migrated from Travis and Appveyor to GitHub Actions. This required several
  changes in package dependencies for development.

* Clarified that the zhmc_cpc module can be used for CPCs in any operational
  mode. Previously, the documentation stated DPM mode as a prerequisite.
  Added support to the zhmc_cpc module for updating several classic-mode-only
  properties.

**Cleanup:**

* Removed the page describing common return values, because all return values
  are specifically described on the module pages without referencing any
  common return value type.

* zhmc_cpc: Added an artificial property 'storage-groups' to the output
  that shows the storage groups attached to the partition, with only a subset
  of their properties.

* zhmc_partition: Added an artificial property 'storage-groups' to the output
  that shows the storage groups attached to the partition, with all of their
  properties and artificial properties as in the result of zhmc_storage_group.
  This is enabled by the new boolean input parameter 'expand_storage_groups'.

* zhmc_partition: Added an artificial property 'crypto-adapters' to the
  'crypto-configuration' property, showing the adapter properties of the
  crypto adapters attached to the partition, with all of their properties and
  artificial properties as in the result of zhmc_adapter. This is enabled by
  the new boolean input parameter 'expand_crypto_adapters'.

* zhmc_partition: Added artificial properties to the 'nics' property:

  * 'adapter-name': Name of the adapter backing the NIC
  * 'adapter-port': Port index on the adapter backing the NIC
  * 'adapter-id': Adapter ID (PCHID) of the adapter backing the NIC

* Examples: Added an example playbook 'get_cpc_io.yml' which retrieves
  information about a CPC in DPM mode and its I/O configuration and
  creates a markdown file showing the result.

* Dev: Changed make targets and adjusted to directory structure compatible with
  Ansible collections, and for publishing on Ansible Galaxy.

* Moved the sample playbooks to the common IBM Z Ansible Collection Samples
  repository: https://github.com/IBM/z_ansible_collections_samples/


Version 0.8.0
-------------

Released: 2019-04-02

Availability: `Pypi`_, `GitHub`_

**Bug fixes:**

* Fixed an issue in the zhmc_crypto_attachment module where the incorrect
  crypto adapter was picked, leading to a subsequent crypto conflict
  when starting the partition. See issue #112.

**Enhancements:**

* Improved the quaity of error messages in the zhmc_crypto_attachment module.


Version 0.7.0
-------------

Released: 2019-02-20

Availability: `Pypi`_, `GitHub`_

**Incompatible changes:**

* Temporarily disabled the retrieval of full properties in the result data
  of the zhmc_adapter module.

**Bug fixes:**

* Docs: Fixed change log of 0.6.0 (see the 0.6.0 section below).

**Enhancements:**

* Renovated the logging:
  - Added support for the log_file parameter to all modules.
  - Changed the format of the log lines.
  - Set log level also when no log_file is specified, causing the logs to be propagated to the root logger.


Version 0.6.0
-------------

Released: 2019-01-07

Availability: `Pypi`_, `GitHub`_

Fixed this change log in 0.6.1 and 0.7.0

**Bug fixes:**

* Fixed dependency to zhmcclient package to be >=0.20.0, instead
  of using its master branch from the github repo.

* Updated the 'requests' package to 2.20.0 to fix the following vulnerability:
  https://nvd.nist.gov/vuln/detail/CVE-2018-18074

* Added support for Python 3.7. This required increasing the minimum version
  of Ansible from 2.2.0.0 to 2.4.0.0.
  This also removes the dependency on the 'pycrypto' package, which has
  vulnerabilities and is no longer maintained since 2013. Ansible uses the
  'cryptography' package, instead.  See issue #66.

* The `crypto_number` property of Adapter is an integer property, and thus the
  Ansible module `zhmc_adapter` needs to change the string passed by Ansible
  back to an integer. It did that correctly but only for the `properties`
  input parameter, and not for the `match` input parameter. The type conversions
  are now applied for all properties of Adapter also for the `match` parameter.

* The dictionary to check input properties for the `zhmc_cpc` module had the
  `acceptable_status` property written with a hyphen instead of underscore.
  This had the effect that it was rejected as non-writeable when specifying
  it as input.

**Enhancements:**

* Added support for managing CPCs by adding a `zhmc_cpc` Ansible module.
  The module allows setting writeable properties of a CPC in an idempotent way,
  and to gather facts for a CPC (i.e. all of its properties including a few
  artificial ones). See issue #82.

* Added support for managing adapters by adding a `zhmc_adapter` Ansible
  module. The module allows setting writeable properties of an adapter,
  changing the adapter type for FICON Express adapters, and changing the
  crypto type for Crypto Express adapters, all in an idempotent way.
  It also allows gathering facts for an adapter (i.e. all of its properties#
  including a few artificial ones).
  See issue #83.

* Added a `zhmc_crypto_attachment` Ansible module, which manages the attachment
  of crypto adapters and of crypto domains to partitions in an idempotent way.
  This was already supported in a less flexible and non-idempotent way by the
  `zhmc_partition` Ansible module.

* Added support for adjusting the value of the `ssc_ipv4_gateway` input property
  for the `zhmc_partition` module to `None` if specified as the empty string.
  This allows defaulting the value more easily in playbooks.

* Docs: Improved and fixed the documentation how to release a version
  and how to start a new version.


Version 0.5.0
-------------

Released: 2018-10-24

Availability: `Pypi`_, `GitHub`_

**Incompatible changes:**

* Changed 'make setup' back to 'make develop' for consistency with the other
  zhmcclient projects.

**Bug fixes:**

* Several fixes in the make process and package dependencies.

* Synced package dependencies with zhmcclient project.

**Enhancements:**

* Added support for DPM storage groups, attachments and volumes, by adding
  new modules 'zhmc_storage_group', 'zhmc_storage_group_attachment', and
  'zhmc_storage_volume'. Added several playbooks as examples.


Version 0.4.0
-------------

Availability: `Pypi`_, `GitHub`_

Released: 2018-03-15

**Bug fixes:**

* Fixed the bug that a TypeError was raised when setting the 'ssc_dns_servers'
  property for a Partition. The property value is a list of strings, and
  lists of values were not supported previously. Extended the function test
  cases for partitions accordingly. (Issue #34).

* Fixed that the "type" property for Partitions could not be specified.
  It is valid for Partition creation, and the only restriction is that
  its value cannot be changed once the Partition exists. Along with fixing
  the logic for such create-only properties, the same issue was also fixed
  for the adapter port related properties of HBAs. (Issue #31).

* Improved the logic for handling create+update properties in case
  the resource does not exist, such that they are no longer updated
  in addition to being set during creation. The logic still supports
  updating as an alternative if the resource does not exist, for
  update-only properties (e.g. several properties in Partitions).
  (Fixed as part of issue #31).

* Fixed the issue that a partition in "terminated" or "paused" status
  could not be made absent (i.e. deleted). Now, the partition is
  stopped which should bring it into "stopped" status, and then
  deleted. (Issue #29).

**Enhancements:**

* Added get_facts.py script to examine usage of the Ansible 2.0 API.

* Added support for gathering partition and child facts.
  The fact support is invoked by specifying state=facts.
  The fact support is implemented by returning the partition properties
  in the result. The returned partition properties are enriched by adding
  properties 'hbas', 'nics', 'virtual-functions' that are a list
  of the properties of the respective child elements of that partition.
  (Issue #32).


Version 0.3.0
-------------

Released: 2017-08-16

Availability: `Pypi`_, `GitHub`_

**Incompatible changes:**

**Deprecations:**

**Bug fixes:**

**Enhancements:**

* Added support for specifying integer-typed and float-typed
  properties of Partitions, NICs, HBAs, and VFs also as decimal
  strings in the module input.

* Specifying string typed properties of Partitions, NICs, HBAs,
  and VFs with Unicode characters no longer performs an unnecessary
  property update.

**Dependencies:**

* Increased minimum Ansible release from 2.0.0.1 to 2.2.0.0.

* Upgraded zhmcclient requirement to 0.15.0


Version 0.2.0
-------------

Released: 2017-07-20

Availability: `Pypi`_, `GitHub`_

This is the initial release.


.. .............................................................................
.. Links to available distributions of the zhmc collection
.. .............................................................................

.. _GitHub:
   https://github.com/zhmcclient/zhmc-ansible-modules/releases
.. _Galaxy:
   https://galaxy.ansible.com/ibm/ibm_zhmc
.. _AutomationHub:
   https://console.redhat.com/ansible/automation-hub/repo/published/ibm/ibm_zhmc
.. _Pypi:
   https://pypi.org/project/zhmc-ansible-modules/
