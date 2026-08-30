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


.. _`Requirements`:

Requirements
============

Ansible playbook tasks using modules from the **IBM Z HMC collection** must
be configured to run on the control node (= local host). From there, the
modules communicate remotely with the WS API of the targeted HMCs.

There are multiple approaches on how this can be configured:

* The recommended approach uses an Ansible inventory and delegation of the tasks
  to localhost, by specifying ``connection: local`` for the playbook or
  ``ansible_connection: local`` in the inventory, and
  ``delegate_to: localhost`` for each task that uses a module of the
  **IBM Z HMC collection**.

* A simpler but more limited approach uses no Ansible inventory and specifies
  ``connection: local`` and ``hosts: localhost`` for the entire playbook.

For more details on these approaches, see the description and sample playbooks
in `IBM Z HMC Sample Playbooks`_.

.. _IBM Z HMC Sample Playbooks:
   https://github.com/IBM/z_ansible_collections_samples/tree/master/z_systems_administration/zhmc

Control node
============

Requirements for the Ansible control node are:

* The control node must have network connectivity to the targeted HMC.

.. toctree::
   :maxdepth: 3

   requirements_managed
