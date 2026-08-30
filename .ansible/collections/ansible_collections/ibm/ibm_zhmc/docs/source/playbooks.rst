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


.. _`Playbooks`:

Playbooks
=========

An `Ansible playbook`_ consists of organized instructions that define work for
a managed node (hosts) to be managed with Ansible. In case of the
**IBM Z HMC collection**, the managed node is the local host, and the IP address
of the HMC is specified as an input parameter to the Ansible modules.

.. _`Sample Playbooks`:

Sample Playbooks
----------------

The **IBM Z HMC collection** provides sample playbooks in the
`IBM Z Ansible Collection Samples`_ repository.

The starting point for reading about them is `IBM Z HMC Sample Playbooks`_.

.. _Ansible playbook:
   https://docs.ansible.com/ansible/latest/user_guide/playbooks_intro.html#playbooks-intro
.. _IBM Z Ansible Collection Samples:
   https://github.com/IBM/z_ansible_collections_samples/
.. _IBM Z HMC Sample Playbooks:
   https://github.com/IBM/z_ansible_collections_samples/tree/master/z_systems_administration/zhmc
