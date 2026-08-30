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

====================
IBM Z HMC collection
====================

The **IBM Z HMC collection** provides `Ansible`_ modules that can manage platform
resources on `IBM Z`_ and `LinuxONE`_ machines.

The goal of this collection is to be able to utilize the power and ease of use
of Ansible for the management of IBM Z platform resources.

The IBM Z platform resources that can be managed include for example partitions,
I/O adapters, the Z system itself, or various resources on its Hardware
Management Console (HMC).

The Ansible modules in this collection are fully
`idempotent <https://docs.ansible.com/ansible/latest/reference_appendices/glossary.html#term-Idempotency>`_,
following an important principle for Ansible modules.
The idempotency of a module allows Ansible playbooks to specify the desired end
state for a resource, regardless of what the current state is. For example, an
IBM Z partition can be specified to have ``state=active`` which means that
it must exist and be in the active operational status. Depending on the current
state of the partition, actions will be taken by the module to reach this
desired end state: If the partition does not exist, it will be created and
started. If it exists but is not active, it will be started. If it is already
active, nothing will be done. Other initial states including transitional
states such as starting or stopping also will be taken care of.
The idempotency of modules makes Ansible playbooks restartable: If an error
happens and some things have been changed already, the playbook can simply be
re-run and will automatically do the right thing, because the initial state
does not matter for reaching the desired end state.

The Ansible modules in this collection are written in Python and interact with
the Web Services API of the Hardware Management Console (HMC) of the Z machines
to be managed.

Note: Before version 0.9.0, the Ansible modules in this collection have been
distributed as the `zhmc-ansible-modules package on Pypi`_.
Starting with version 0.9.0, the Ansible modules are no longer distributed on
Pypi, but as the `ibm.ibm_zhmc collection on Ansible Galaxy`_.
Starting with version 1.0.0, the Ansible modules are additionally distributed
as the `ibm.ibm_zhmc collection on Ansible AutomationHub`_.

Features
========

The **IBM Z HMC collection** includes :ref:`modules` to automate the management
of IBM Z platform resources. There are :ref:`sample playbooks` that show how to
invoke these modules in playbook tasks.

.. toctree::
   :maxdepth: 4
   :caption: Getting Started

   installation

.. toctree::
   :maxdepth: 3
   :caption: References

   modules
   playbooks

.. toctree::
   :maxdepth: 1
   :caption: Community Guides

   community_guides
   development

.. toctree::
   :maxdepth: 3
   :caption: Requirements

   requirements

.. toctree::
   :maxdepth: 1
   :caption: Appendices

   release_notes
   versioning
   bibliography

Copyright
=========

Copyright IBM Corporation 2016-2020

License
=======

This collection is licensed under the `Apache 2.0 License`_.

Author Information
==================

The **IBM Z HMC collection** is maintained by the IBM Z development team.

.. _`Reporting issues`:

Reporting issues
================

If you encounter any problem with this collection, or if you have questions of
any kind related to this collection (even when they are not about a problem),
please open an issue in the `issue tracker`_.

.. _`issue tracker`:
   https://github.com/zhmcclient/zhmc-ansible-modules/issues
.. _Apache 2.0 License:
   https://opensource.org/licenses/Apache-2.0
.. _ibm.ibm_zhmc collection on Ansible Galaxy:
   https://galaxy.ansible.com/ibm/ibm_zhmc/
.. _ibm.ibm_zhmc collection on Ansible AutomationHub:
   https://console.redhat.com/ansible/automation-hub/repo/published/ibm/ibm_zhmc/
.. _zhmc-ansible-modules package on Pypi:
   https://pypi.org/project/zhmc-ansible-modules/
.. _Ansible:
   https://www.ansible.com/
.. _IBM Z:
   https://www.ibm.com/it-infrastructure/z
.. _LinuxONE:
   https://www.ibm.com/it-infrastructure/linuxone/
