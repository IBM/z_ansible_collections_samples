.. ...........................................................................
.. © Copyright IBM Corporation 2020                                          .
.. ...........................................................................

=========
Playbooks
=========

An `Ansible playbook`_ consists of organized instructions that define work for
a managed node (host) to be managed with Ansible.

.. _Ansible playbook:
   https://docs.ansible.com/ansible/latest/user_guide/playbooks_intro.html#playbooks-intro

Sample playbooks that demonstrate how to use the collection content are **included**
in the **IBM z/OS IMS collection**. You can find the samples on
`Github`_ or in the collections playbooks directory included with the
installation. For more information about the collections installation and
hierarchy, refer to the RedHat Ansible Certified Content `installation`_ documentation.

.. _Github:
   https://github.com/ansible-collections/ibm_zos_ims/tree/master/playbooks

.. _installation:
   https://ibm.github.io/z_ansible_collections_doc/installation/installation.html

The sample playbook can be run with the ``ansible-playbook`` command with some
modification to the **inventory**, **ansible.cfg** and **group_vars**.

.. toctree::
   :maxdepth: 3
   :caption: Configuration

   playbook_config_setup

.. toctree::
   :maxdepth: 1
   :caption: Inventory

   playbook_inventory

.. toctree::
   :maxdepth: 1
   :caption: group_vars

   playbook_group_vars

.. toctree::
   :maxdepth: 1
   :caption: Run a playbook

   playbook_run
