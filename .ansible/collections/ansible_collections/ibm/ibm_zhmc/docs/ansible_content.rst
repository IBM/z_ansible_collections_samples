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

=====
Z HMC
=====

The **IBM Z® HMC collection**, also represented as
`ibm_zhmc`_ in this document, is  part of the broader
initiative to bring Ansible® Automation to IBM Z through the offering
**Red Hat® Ansible Certified Content for IBM Z**.

The **IBM Z HMC collection** provides `Ansible`_ modules that can manage
platform resources on `IBM Z`_ and `LinuxONE`_ machines.
The IBM Z platform resources that can be managed include Partitions, CPU,
memory, virtual switches, I/O adapters and various platform resources,
and covers lifecycle and configuration operations.

The Ansible modules in this collection are written in Python and interact with
the Web Services API of the Hardware Management Console (HMC) of the Z machines
to be managed.

.. toctree::
   :maxdepth: 1
   :caption: Collection Content

   source/modules

.. _ibm_zhmc:
   https://galaxy.ansible.com/ibm/ibm_zhmc/
.. _Ansible:
   https://www.ansible.com/
.. _IBM Z:
   http://www.ibm.com/it-infrastructure/z/
.. _LinuxONE:
   http://www.ibm.com/it-infrastructure/linuxone/
