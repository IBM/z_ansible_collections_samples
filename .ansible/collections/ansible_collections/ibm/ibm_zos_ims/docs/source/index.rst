.. ...........................................................................
.. © Copyright IBM Corporation 2020                                          .
.. ...........................................................................

========================
IBM z/OS IMS collection
========================

The **IBM z/OS IMS collection**, also represented as **ibm\_zos\_ims**
in this document, is part of the broader offering **Red Hat® Ansible
Certified Content for IBM Z**. The IBM z/OS IMS collection supports tasks
such as generating IMS Database Descriptors (DBD), Program Specification
Blocks (PSB), Application Control Blocks (ACB), running IMS commands
(type-1, type-2, DBRC), and interacting with the IMS Catalog.

The **IBM z/OS IMS collection** works closely with offerings such as the
`IBM z/OS core collection`_ to deliver a solution that will enable you to
automate tasks on z/OS.

.. _IBM z/OS core collection:
   https://galaxy.ansible.com/ibm/ibm_zos_core

Red Hat Ansible Certified Content for IBM Z
===========================================

**Red Hat® Ansible Certified Content for IBM Z** provides the ability to
connect IBM Z® to clients' wider enterprise automation strategy through the
Ansible Automation Platform ecosystem. This enables development and operations
automation on Z through a seamless, unified workflow orchestration with
configuration management, provisioning, and application deployment in one
easy-to-use platform.

**The IBM z/OS IMS collection**, as part of the broader offering
**Red Hat® Ansible Certified Content for IBM Z**, is available on both Galaxy
as a community supported offering and on Automation Hub with
enterprise support.

Features
========

The IBM z/OS IMS collection includes `modules`_,
`sample playbooks`_, and ansible-doc to automate tasks on z/OS.


.. _modules:
    https://github.com/ansible-collections/ibm_zos_ims/tree/master/plugins/modules/
.. _sample playbooks:
    https://github.com/ansible-collections/ibm_zos_ims/tree/master/playbooks/

.. toctree::
   :maxdepth: 1
   :caption: Getting Started

   installation
   requirements-single
   playbooks-single

.. toctree::
   :maxdepth: 1
   :caption: Ansible Content

   modules

.. toctree::
   :maxdepth: 1
   :caption: Release Notes

   release_notes

.. .. toctree::
..    :maxdepth: 1
..    :caption: FAQs
..
..    faqs

.. toctree::
   :maxdepth: 1
   :caption: Reference

   community_guides
   license
