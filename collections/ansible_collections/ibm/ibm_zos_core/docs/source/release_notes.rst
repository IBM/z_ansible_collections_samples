.. ...........................................................................
.. © Copyright IBM Corporation 2020                                          .
.. ...........................................................................

========
Releases
========

Version 1.2.1
=============

Notes
-----

* Update required
* Module changes

  * Noteworthy Python 2.x support

    * encode - removed TemporaryDirectory usage.
    * zos_copy - fixed regex support, dictionary merge operation fix
    * zos_fetch - fix quote import

* Collection changes

  * Beginning this release, all sample playbooks previously included with the
    collection will be made available on the `samples repository`_. The
    `samples repository`_ explains the playbook concepts,
    discusses z/OS administration, provides links to the samples support site,
    blogs and other community resources.

* Documentation changes

  * In this release, documentation related to playbook configuration has been
    migrated to the `samples repository`_. Each sample contains a README that
    explains what configurations must be made to run the sample playbook.

.. _samples repository:
   https://github.com/IBM/z_ansible_collections_samples/blob/master/README.md

Availability
------------

* `Automation Hub`_
* `Galaxy`_
* `GitHub`_

Reference
---------

* Supported by IBM Open Enterprise Python for z/OS: 3.8.2 or later
* Supported by IBM Z Open Automation Utilities 1.0.3 PTF UI70435
* Supported by z/OS V2R3
* The z/OS® shell

Known issues
------------

* Modules

  * When executing programs using ``zos_mvs_raw``, you may encounter errors
    that originate in the programs implementation. Two such known issues are
    noted below of which one has been addressed with an APAR.

    #. ``zos_mvs_raw`` module execution fails when invoking
       Database Image Copy 2 Utility or Database Recovery Utility in conjunction
       with FlashCopy or Fast Replication.
    #. ``zos_mvs_raw`` module execution fails when invoking DFSRRC00 with parm
       "UPB,PRECOMP", "UPB, POSTCOMP" or "UPB,PRECOMP,POSTCOMP". This issue is
       addressed by APAR PH28089.


Version 1.2.0
=============

Notes
-----

* Update recommended
* Collection changes

  * Beginning this release, all sample playbooks previously included with the
    collection will be made available on the `samples repository`_. The
    `samples repository`_ explains the playbook concepts,
    discusses z/OS administration, provides links to the samples support site,
    blogs and other community resources.

* Documentation changes

  * In this release, documentation related to playbook configuration has been
    migrated to the `samples repository`_. Each sample contains a README that
    explains what configurations must be made to run the sample playbook.

.. _samples repository:
   https://github.com/IBM/z_ansible_collections_samples/blob/master/README.md

Availability
------------

* `Galaxy`_
* `GitHub`_

Reference
---------

* Supported by IBM Open Enterprise Python for z/OS: 3.8.2 or later
* Supported by IBM Z Open Automation Utilities 1.0.3 PTF UI70435
* Supported by z/OS V2R3
* The z/OS® shell

Known issues
------------

* Modules

  * When executing programs using ``zos_mvs_raw``, you may encounter errors
    that originate in the programs implementation. Two such known issues are
    noted below of which one has been addressed with an APAR.

    #. ``zos_mvs_raw`` module execution fails when invoking
       Database Image Copy 2 Utility or Database Recovery Utility in conjunction
       with FlashCopy or Fast Replication.
    #. ``zos_mvs_raw`` module execution fails when invoking DFSRRC00 with parm
       "UPB,PRECOMP", "UPB, POSTCOMP" or "UPB,PRECOMP,POSTCOMP". This issue is
       addressed by APAR PH28089.


Version 1.2.0-beta.4
====================

Notes
-----

* Update recommended
* Bugfix

  * Fixes a bug for `zos_data_set` module where some parameters were not
    getting passed correctly because python considers integer value of 0
    to be false.
  * Fixes documentation in module `zos_job_submit` where **wait_time_s** should
    have been written as **duration_s**.
  * Fixes requirements version in sample playbook hosts-setup.yaml

* Module changes

  * Module ``zos_copy`` can now use wildcards to copy multiple PDS/PDSE members
    to another PDS/PDSE

Availability
------------

* `Galaxy`_
* `GitHub`_

Reference
---------

* Supported by IBM Open Enterprise Python for z/OS: 3.8.2 or later
* Supported by IBM Z Open Automation Utilities 1.0.3 PTF UI70435
* Supported by z/OS V2R3
* The z/OS® shell

Known issues
------------

* Modules

  * When executing programs using ``zos_mvs_raw``, you may encounter errors
    that originate in the programs implementation. Two such known issues are
    noted below of which one has been addressed with an APAR.

    #. ``zos_mvs_raw`` module execution fails when invoking
       Database Image Copy 2 Utility or Database Recovery Utility in conjunction
       with FlashCopy or Fast Replication.
    #. ``zos_mvs_raw`` module execution fails when invoking DFSRRC00 with parm
       "UPB,PRECOMP", "UPB, POSTCOMP" or "UPB,PRECOMP,POSTCOMP". This issue is
       addressed by APAR PH28089.

Version 1.2.0-beta.3
====================

Notes
-----

* Update recommended
* Bugfix

  * Fixes a bug which causes action plugins to fail when collections are
    referenced using fully qualified collection names instead of playbook
    level imports

Availability
------------

* `Galaxy`_
* `GitHub`_

Reference
---------

* Supported by IBM Open Enterprise Python for z/OS: 3.8.2 or later
* Supported by IBM Z Open Automation Utilities 1.0.3 PTF UI70435
* Supported by z/OS V2R3
* The z/OS® shell

Known issues
------------

* Modules

  * When executing programs using ``zos_mvs_raw``, you may encounter errors
    that originate in the programs implementation. Two such known issues are
    noted below of which one has been addressed with an APAR.

    #. ``zos_mvs_raw`` module execution fails when invoking
       Database Image Copy 2 Utility or Database Recovery Utility in conjunction
       with FlashCopy or Fast Replication.
    #. ``zos_mvs_raw`` module execution fails when invoking DFSRRC00 with parm
       "UPB,PRECOMP", "UPB, POSTCOMP" or "UPB,PRECOMP,POSTCOMP". This issue is
       addressed by APAR PH28089.

Version 1.2.0-beta.2
====================

Notes
-----

* Update recommended
* Module changes

  * Update zos_fetch and zos_copy to allow for user specified SFTP transfer
    port.
  * Refactor module option **backup_file** to **backup_name** in modules
    ``zos_copy``, ``zos_lineinfile``, ``zos_encode``.
  * Fix ``zos_copy`` record format.
  * Fix ``zos_job_submit`` allowable characters for data sets.
  * Update ``zos_fetch`` and ``zos_copy`` with option **ignore_sftp_stderr**
    to alter module behavior.
  * Fix ``zos_operator_action_query`` so that all outstanding messages are
    returned.
  * Update ``zos_mvs_raw`` with verbose option.
* Documentation

  * Update documentation in support of `centralized content`_.
* New playbook to aid in generating **group_vars**

Availability
------------

* `Galaxy`_
* `GitHub`_

Reference
---------

* Supported by IBM Open Enterprise Python for z/OS: 3.8.2 or later
* Supported by IBM Z Open Automation Utilities 1.0.3 PTF UI70435
* Supported by z/OS V2R3
* The z/OS® shell

Known issues
------------

* Modules

  * When executing programs using ``zos_mvs_raw``, you may encounter errors
    that originate in the programs implementation. Two such known issues are
    noted below of which one has been addressed with an APAR.

    #. ``zos_mvs_raw`` module execution fails when invoking
       Database Image Copy 2 Utility or Database Recovery Utility in conjunction
       with FlashCopy or Fast Replication.
    #. ``zos_mvs_raw`` module execution fails when invoking DFSRRC00 with parm
       "UPB,PRECOMP", "UPB, POSTCOMP" or "UPB,PRECOMP,POSTCOMP". This issue is
       addressed by APAR PH28089.

.. _centralized content:
   https://ibm.github.io/z_ansible_collections_doc/index.html


Version 1.2.0-beta.1
====================

Notes
-----

* Update recommended
* New modules

  * zos_copy
  * zos_lineinfile
  * zos_mvs_raw

* Bug fixes
* Documentation updates
* New samples
* Module enhancements:

  * zos_data_set - includes full multi-volume support for data set creation,
    addition of secondary space option, improved SMS support with storage,
    data, and management classes

Availability
------------

* Galaxy
* GitHub

Reference
---------

* Supported by IBM Open Enterprise Python for z/OS: 3.8.2 or later
* Supported by IBM Z Open Automation Utilities 1.0.3 PTF UI70435
* Supported by z/OS V2R3
* The z/OS® shell


Version 1.1.0
=============

Notes
-----
* Update recommended
* New modules

  * zos_fetch
  * zos_encode
  * zos_operator_action_query
  * zos_operator
  * zos_tso_command
  * zos_ping

* New filter
* Improved error handling and messages
* Bug fixes
* Documentation updates
* New samples

Availability
------------

* Automation Hub
* Galaxy
* GitHub

Reference
---------

* Supported by IBM Open Enterprise Python for z/OS: 3.8.2 or later
* Supported by IBM Z Open Automation Utilities: 1.0.3 PTF UI70435
* Supported by z/OS V2R3
* The z/OS® shell


Version 1.1.0-beta1
===================

Notes
-----

* Update recommended
* New modules

  * zos_fetch, zos_encode, zos_operator_action_query, zos_operator,
    zos_tso_command, zos_ping
* New filter
* Improved error handling and messages
* Bug fixes
* Documentation updates
* New samples

Availability
------------

* Galaxy
* GitHub

Reference
---------

* Supported by IBM Z Open Automation Utilities: 1.0.2 or 1.0.3 PTF UI70435

Version 1.0.0
=============

Notes
-----

* Update recommended
* Security vulnerabilities fixed
* Improved test, security and injection coverage
* Module zos_data_set catalog support added
* Documentation updates

Availability
------------

* Automation Hub
* Galaxy
* GitHub

Reference
---------

* Supported by IBM Z Open Automation Utilities: 1.0.1 PTF UI66957 through
  1.0.3 PTF UI70435


Version 0.0.4
=============

Notes
-----

* Update recommended
* Includes fixes to modules zos_job_output and zos_job_submit
* Improved buffer utilization
* Optimized JSON response
* Functional test cases for all modules
* Updated document references

Availability
------------

* Galaxy
* GitHub

Reference
---------

* Supported by IBM Z Open Automation Utilities: 1.0.1 PTF UI66957 through
  1.0.3 PTF UI70435


Version 0.0.3
=============

Notes
-----

* Update recommended
* Includes updates to README.md for a malformed URL and product direction
* Includes fixes for zos_data_set module

Availability
------------

* Galaxy
* GitHub

Reference
---------

* Supported by IBM Z Open Automation Utilities: 1.0.1 PTF UI66957 through
  1.0.3 PTF UI70435

Version 0.0.2
=============

Notes
-----

* Update not required
* Updates to the README and included docs

Availability
------------

* Galaxy
* GitHub

Reference
---------

* Supported by IBM Z Open Automation Utilities: 1.0.1 PTF UI66957 through
  1.0.3 PTF UI70435


Version 0.0.1
=============

Notes
-----

* Initial beta release of IBM Z core collection, referred to as ibm_zos_core
  which is part of the broader offering
  Red Hat® Ansible Certified Content for IBM Z.

Availability
------------

* Galaxy
* GitHub

Reference
---------

* Supported by IBM Z Open Automation Utilities: 1.0.1 PTF UI66957 through
  1.0.3 PTF UI70435

.. .............................................................................
.. Global Links
.. .............................................................................

.. _GitHub:
   https://github.com/ansible-collections/ibm_zos_core

.. _Galaxy:
   https://galaxy.ansible.com/ibm/ibm_zos_core

.. _Automation Hub:
   https://www.ansible.com/products/automation-hub
