*******************************
Ansible z/OS Collection Samples
*******************************
This repository is for managing and storing all sample playbooks related to the Red Hat Ansible Certified Content for IBM Z.



* `Requirements`_

  * `Control node`_
  * `Managed node`_
  * `Python on z/OS`_
  * `ZOAU`_

* `Installation`_

  * `Ansible Galaxy`_
  * `Automation Hub and Private Galaxy server`_
  * `Local build`_

* `Quickstart`_

* `Sample playbooks`_

* `Blogs`_

* `Releases`_

  * `Version 1.1.0-beta1`_
  * `Version 1.0.0`_
  * `Version 0.0.4`_
  * `Version 0.0.3`_
  * `Version 0.0.2`_
  * `Version 0.0.1`_


.. ...........................................................................
.. © Copyright IBM Corporation 2020                                          .
.. ...........................................................................

Requirements
============

A control node is any machine with Ansible installed. From the control node,
you can run commands and playbooks from a laptop, desktop, or server.
However, you cannot run **IBM z/OS core collection** on a Windows system.

A managed node is often referred to as a target node, or host, and it is managed
by Ansible. Ansible need not be installed on a managed node, but SSH must be
enabled.

The nodes listed below require these specific versions of software:

Control node
------------

* `Ansible version`_: 2.9 or later
* `Python`_: 2.7 or later
* `OpenSSH`_

.. _Ansible version:
   https://docs.ansible.com/ansible/latest/installation_guide/intro_installation.html
.. _Python:
   https://www.python.org/downloads/release/latest
.. _OpenSSH:
   https://www.openssh.com/


Managed node
------------

* `Python on z/OS`_: 3.6 or later
* `z/OS`_: V02.02.00 or later
* `IBM Z Open Automation Utilities`_ (ZOAU)

   * IBM z/OS core collections are dependent on specific versions of ZOAU.
     For information about the required version of ZOAU, review the
     `release notes`_.
* `z/OS OpenSSH`_

.. _z/OS:
   https://www.ibm.com/support/knowledgecenter/SSLTBW_2.2.0/com.ibm.zos.v2r2/zos-v2r2-home.html

.. _IBM Z Open Automation Utilities:
   `ZOAU`_

.. _z/OS OpenSSH:
   https://www.ibm.com/support/knowledgecenter/SSLTBW_2.2.0/com.ibm.zos.v2r2.e0za100/ch1openssh.htm

.. _release notes:
   `Releases`_


Python on z/OS
--------------

If the Ansible target is z/OS, you must install a Python distribution ported
for this platform. Rocket Software is currently the preferred version for z/OS.

**Installation**

* Visit the `Rocket Software homepage`_ and create a required account in the
  `Rocket Customer Portal`_.
* Click Downloads on the top left portion the page.
* Select the category z/OpenSource on the left panel.
* Scroll and select Python.
* Download the binaries, installation files, and the README.ZOS onto an x86
  machine.
* Transfer the zipped tarball (tar.gz) file to the target z/OS system and
  extract it according to the instructions in the installation files.
* Follow the additional setup instructions as described in the README.ZOS file.

.. _Rocket Software homepage:
   https://www.rocketsoftware.com/zos-open-source
.. _Rocket Customer Portal:
   https://my.rocketsoftware.com/


ZOAU
----

IBM Z Open Automation Utilities provide support for executing automation tasks
on z/OS. With ZOAU, you can run traditional MVS commands, such as IEBCOPY,
IDCAMS, and IKJEFT01, as well as perform a number of data set operations
in the scripting language of your choice.

**Installation**

* For the ZOAU FMID, program directory, fix list, latest PTF, installation
  and configuration instructions, refer to the ZOAU `product page`_.

.. _product page:
   https://www.ibm.com/support/knowledgecenter/en/SSKFYE_1.0.0/welcome_zoautil.html


Installation
============

You can install the **IBM z/OS core collection** using one of these options:
Ansible Galaxy, Ansible Automation Hub, or a local build.

For more information on installing collections, see `using collections`_.

.. _using collections:
   https://docs.ansible.com/ansible/latest/user_guide/collections_using.html

Ansible Galaxy
--------------
Galaxy enables you to quickly configure your automation project with content
from the Ansible community.

Galaxy provides prepackaged units of work known as collections. You can use the
`ansible-galaxy`_ command with the option ``install`` to install a collection on
your system (control node) hosted in Galaxy. If you have installed a prior
version, you must overwrite an existing collection with the ``--force`` option.

Here are a few examples of installing the **IBM z/OS core collection**:

.. code-block:: sh

   $ ansible-galaxy collection install ibm.ibm_zos_core\
   $ ansible-galaxy collection install -f ibm.ibm_zos_core
   $ ansible-galaxy collection install --force ibm.ibm_zos_core

By default, the `ansible-galaxy`_ command installs the latest available
collection, but you can add a version identifier to install a specific version.
Before installing a collection from Galaxy, review all the available versions.
Periodically, new releases containing enhancements and features you might be
interested in become available.

Here's an example command for installing the **IBM z/OS core collection** for
a specific version.

.. code-block:: sh

   $ ansible-galaxy collection install ibm.ibm_zos_core::1.0.0

The collection installation progress will be output to the console. Note the
location of the installation so that you can review other content included with
the collection, such as the sample playbook. By default, collections are
installed in ``~/.ansible/collections``; see the sample output.

.. _ansible-galaxy:
   https://docs.ansible.com/ansible/latest/cli/ansible-galaxy.html

.. code-block:: sh

   Process install dependency map
   Starting collection install process
   Installing 'ibm.ibm_zos_core:1.0.0' to '/Users/user/.ansible/collections/ansible_collections/ibm/ibm_zos_core'

After installation, the collection content will resemble this hierarchy: :

.. code-block:: sh

   ├── collections/
   │  ├── ansible_collections/
   │      ├── ibm/
   │          ├── ibm_zos_core/
   │              ├── docs/
   │              ├── playbooks/
   │              ├── plugins/
   │                  ├── action/
   │                  ├── connection/
   │                  ├── module_utils/
   │                  ├── modules/
   │                  └── filter/


You can use the `-p` option with `ansible-galaxy` to specify the installation
path, such as:

.. code-block:: sh

   $ ansible-galaxy collection install ibm.ibm_zos_core -p /home/ansible/collections

For more information on installing collections with Ansible Galaxy,
see `installing collections`_.

.. _installing collections:
   https://docs.ansible.com/ansible/latest/user_guide/collections_using.html#installing-collections-with-ansible-galaxy

Automation Hub and Private Galaxy server
----------------------------------------
Configuring access to a private Galaxy server follows the same instructions
that you would use to configure your client to point to Automation Hub. When
hosting a private Galaxy server or pointing to Hub, available content is not
always consistent with what is available on the community Galaxy server.

You can use the `ansible-galaxy`_ command with the option ``install`` to
install a collection on your system (control node) hosted in Automation Hub
or a private Galaxy server.

By default, the ``ansible-galaxy`` command is configured to access
``https://galaxy.ansible.com`` as the server when you install a
collection. The `ansible-galaxy` client can be configured to point to Hub or
other servers, such as a privately running Galaxy server, by configuring the
server list in the ``ansible.cfg`` file.

Ansible searches for ``ansible.cfg`` in the following locations in this order:

   * ANSIBLE_CONFIG (environment variable if set)
   * ansible.cfg (in the current directory)
   * ~/.ansible.cfg (in the home directory)
   * /etc/ansible/ansible.cfg

To configure a Galaxy server list in the ansible.cfg file:

  * Add the server_list option under the [galaxy] section to one or more
    server names.
  * Create a new section for each server name.
  * Set the url option for each server name.

For Automation Hub, you additionally need to:

  * Set the auth_url option for each server name.
  * Set the API token for each server name. For more information on API tokens,
    see `Get API token from the version dropdown to copy your API token`_.

.. _Get API token from the version dropdown to copy your API token:
   https://cloud.redhat.com/ansible/automation-hub/token/

The following example shows a configuration for Automation Hub, a private
running Galaxy server, and Galaxy:

.. code-block:: yaml

   [galaxy]
   server_list = automation_hub, galaxy, private_galaxy

   [galaxy_server.automation_hub]
   url=https://cloud.redhat.com/api/automation-hub/
   auth_url=https://sso.redhat.com/auth/realms/redhat-external/protocol/openid-connect/token
   token=<hub_token>

   [galaxy_server.galaxy]
   url=https://galaxy.ansible.com/

   [galaxy_server.private_galaxy]
   url=https://galaxy-dev.ansible.com/
   token=<private_token>

For more configuration information, see
`configuring the ansible-galaxy client`_ and `Ansible Configuration Settings`_.

.. _configuring the ansible-galaxy client:
   https://docs.ansible.com/ansible/latest/user_guide/collections_using.html#configuring-the-ansible-galaxy-client

.. _Ansible configuration Settings:
   https://docs.ansible.com/ansible/latest/reference_appendices/config.html


Local build
-----------

You can use the ``ansible-galaxy collection install`` command to install a
collection built from source. To build your own collection, you must clone the
Git repository, build the collection archive, and install the collection. The
``ansible-galaxy collection build`` command packages the collection into an
archive that can later be installed locally without having to use Hub or
Galaxy.

To build a collection from the Git repository:

   1. Clone the sample repository:

      .. note::
         * Collection archive names will change depending on the release version.
         * They adhere to this convention **<namespace>-<collection>-<version>.tar.gz**, for example, **ibm-ibm_zos_core-1.0.0.tar.gz**


   2. Build the collection by running the ``ansible-galaxy collection build``
   command, which must be run from inside the collection:

      .. code-block:: sh

         cd ibm_zos_core
         ansible-galaxy collection build

      Example output of a locally built collection:

      .. code-block:: sh

         $ ansible-galaxy collection build
         Created collection for ibm.ibm_zos_core at /Users/user/git/ibm/zos-ansible/ibm_zos_core/ibm-ibm_zos_core-1.0.0.tar.gz

      .. note::
         * If you build the collection with Ansible version 2.9 or earlier, you will see the following warning that you can ignore.
         * [WARNING]: Found unknown keys in collection galaxy.yml at '/Users/user/git/ibm/zos-ansible/ibm_zos_core/galaxy.yml': build_ignore


   3. Install the locally built collection:

      .. code-block:: sh

         $ ansible-galaxy collection install ibm-ibm_zos_core-1.0.0.tar.gz

      In the output of collection installation, note the installation path to access the sample playbook:

      .. code-block:: sh

         Process install dependency map
         Starting collection install process
         Installing 'ibm.ibm_zos_core:1.0.0' to '/Users/user/.ansible/collections/ansible_collections/ibm/ibm_zos_core'

      You can use the ``-p`` option with ``ansible-galaxy`` to specify the
      installation path, for example, ``ansible-galaxy collection install ibm-ibm_zos_core-1.0.0.tar.gz -p /home/ansible/collections``.

      For more information, see `installing collections with Ansible Galaxy`_.

      .. _installing collections with Ansible Galaxy:
         https://docs.ansible.com/ansible/latest/user_guide/collections_using.html#installing-collections-with-ansible-galaxy





.. ...........................................................................
.. © Copyright IBM Corporation 2020                                          .
.. ...........................................................................

Quickstart
==========

After you install the collection outlined in the  `Installation`_ guide, you
can access the collection and the ansible-doc covered in the following topics:


ibm_zos_core
------------

After the collection is installed, you can access the collection content for a
playbook by referencing the namespace ``ibm`` and the collection's fully
qualified name ``ibm_zos_core``. For example:

.. code-block:: yaml

    - hosts: all

    tasks:
    - name: Query submitted job 'HELLO'
        ibm.ibm_zos_core.zos_job_query:
        job_name: HELLO


In Ansible 2.9, the ``collections`` keyword was added to reduce the need
to refer to the collection repeatedly. For example, you can use the
``collections`` keyword in your playbook:

.. code-block:: yaml

    - hosts: all
      collections:
      - ibm.ibm_zos_core

      tasks:
      - name: Query submitted job 'HELLO'
        zos_job_query:
            job_name: HELLO


z/OS Connection Plugin
----------------------

Since EBCDIC encoding is used on z/OS, custom plugins are required to determine
the correct transport method when targeting a z/OS system. The zos_ssh.py
connection plugin is a fork of the default ssh.py plugin with the added
functionality to check if a module is written in REXX.

Since REXX scripts are required to be in EBCDIC encoding to run, they must be
handled differently during transfer. If the string
``__ANSIBLE_ENCODE_EBCDIC__`` is found in the first line of the module, the
module is transferred to the target system using SCP. Otherwise, SFTP is used.
SCP treats files as text, automatically encoding as EBCDIC at transfer time.
SFTP treats files as binary, performing no encoding changes.

**REXX Module Configuration**:

* Ensure a REXX modules first line is a comment containing the case insensitive keyword ``rexx``
* Followed by the case sensitive value ``__ANSIBLE_ENCODE_EBCDIC__``


**Example REXX module**:

.. code-block:: sh

   /* rexx  __ANSIBLE_ENCODE_EBCDIC__  */
   x = 55
   SAY '{"SYSTEM_VERSION":"' x '"}'
   RETURN 0


ansible-doc
-----------

Modules included in this collection provide additional documentation that is
similar to a UNIX, or UNIX-like operating system man page (manual page). This
documentation can be accessed from the command line by using the
``ansible-doc`` command.

Here's how to use the ``ansible-doc`` command after you install the
**IBM z/OS core collection**: ``ansible-doc ibm.ibm_zos_core.zos_data_set``

.. code-block:: sh

    > ZOS_DATA_SET    (/Users/user/.ansible/collections/ansible_collections/ibm/ibm_zos_core/plugins/modules/zos_data_set.py)

            Create, delete and set attributes of data sets. When forcing data set replacement, contents will not be
            preserved.

    * This module is maintained by The Ansible Community
    OPTIONS (= is mandatory):

    - batch
            Batch can be used to perform operations on multiple data sets in a single module call.
            Each item in the list expects the same options as zos_data_set.
            [Default: (null)]
            type: list
            version_added: 2.9

    - data_class
            The data class name (required for SMS-managed data sets)
            [Default: (null)]
            type: str
            version_added: 2.9

For more information on using the ``ansible-doc`` command, refer
to `Ansible guide`_.

.. _Ansible guide:
   https://docs.ansible.com/ansible/latest/cli/ansible-doc.html#ansible-doc









Sample playbooks
================

* `Data set operations <https://github.com/IBM/z_ansible_collections_samples/blob/master/playbooks/data-set-operations-sample.yaml>`_

* `Data transfer <https://github.com/IBM/z_ansible_collections_samples/blob/master/playbooks/data-transfer-sample.yaml>`_

* `Encoding <https://github.com/IBM/z_ansible_collections_samples/blob/master/playbooks/encoding-sample.yaml>`_

* `Job submission <https://github.com/IBM/z_ansible_collections_samples/blob/master/playbooks/job-submission-sample.yaml>`_

* `z/OS operator commands <https://github.com/IBM/z_ansible_collections_samples/blob/master/playbooks/zos-operator-sample.yaml>`_




Blogs
=====
* `Job Submission on z/OS Made Easy with Ansible <https://community.ibm.com/community/user/ibmz-and-linuxone/blogs/asif-mahmud1/2020/06/10/job-submission-on-zos-made-easy-with-ansible>`_
* `Simplified Approach to Copying Data Between z/OS and Local Machine Using Ansible <https://>`_

.. ...........................................................................
.. © Copyright IBM Corporation 2020                                          .
.. ...........................................................................

Releases
========

Version 1.1.0-beta1
-------------------

Notes
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
  * Galaxy
  * GitHub

Reference
  * Supported by IBM Z Open Automation Utilities: 1.0.2 or later

Version 1.0.0
-------------
Notes
   * Update recommended
   * Security vulnerabilities fixed
   * Improved test, security and injection coverage
   * Module zos_data_set catalog support added
   * Documentation updates

Availability
  * Automation Hub
  * Galaxy
  * GitHub

Reference
  * Supported by IBM Z Open Automation Utilities: 1.0.1 (PTF UI66957 or later)

Version 0.0.4
-------------

Notes
  * Update recommended
  * Includes fixes to modules zos_job_output and zos_job_submit
  * Improved buffer utilization
  * Optimized JSON response
  * Functional test cases for all modules
  * Updated document references

Availability
  * Galaxy
  * GitHub

Reference:
  * Supported by IBM Z Open Automation Utilities: 1.0.1 (PTF UI66957 or later)

Version 0.0.3
-------------
Notes
  * Update recommended
  * Includes updates to README.md for a malformed URL and product direction
  * Includes fixes for zos_data_set module

Availability
  * Galaxy
  * GitHub

Reference
  * Supported by IBM Z Open Automation Utilities: 1.0.1 (PTF UI66957 or later)

Version 0.0.2
-------------
Notes
  * Update not required
  * Updates to the README and included docs

Availability
  * Galaxy
  * GitHub

Reference
  * Supported by IBM Z Open Automation Utilities: 1.0.1 (PTF UI66957 or later)

Version 0.0.1
-------------
Notes
  * Initial beta release of IBM Z core collection, referred to as ibm_zos_core
    which is part of the broader offering
    Red Hat® Ansible Certified Content for IBM Z.

Availability
  * Galaxy
  * GitHub

Reference
  * Supported by IBM Z Open Automation Utilities: 1.0.1 (PTF UI66957 or latera