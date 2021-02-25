.. Copyright 2020 IBM Corp. All Rights Reserved.
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

Samples for the IBM Z HMC Collection
====================================

The `IBM Z HMC Collection <https://github.com/zhmcclient/zhmc-ansible-modules>`_
provides sample playbooks.

Downloading the sample playbooks
--------------------------------

The easiest way to use the samples playbooks is to clone the Git repository
to your local system. This does not require any special authorization for the
repository:

.. code-block:: bash

    $ git clone https://github.com/zhmcclient/zhmc-ansible-modules.git
    $ cd zhmc-ansible-modules/zhmc/playbooks

Creating a vault file
---------------------

All sample playbooks include a vault file named ``vault.yml`` that defines
variables that are used in the sample playbooks.

In order to ensure that the vault file used by developers of this repository
is not stored in the repository by mistake, the ``vault.yml`` file is excluded
from Git, and the Git repository provides that file under the name:

* ``vault_example.yml``

To use the sample playbooks, you need to copy that file to ``vault.yml`` and
adjust the content of the copy as needed.

Running the playbooks
---------------------

To run any of the sample playbooks, it is recommended to first look at them and
to adjust any parameters as documented in the playbook files.

You use the
`ansible-playbook <https://docs.ansible.com/ansible/latest/cli/ansible-playbook.html>`_
command to run playbooks.

Sample playbooks
----------------

The sample playbooks whose filename starts with ``module_`` are very simple
playbooks that demonstrate the use of a single module.
They are described in :ref:`Sample module playbooks`.

The sample playbooks whose filename starts with ``usecase_`` are more complex
and implement a particular use case. They are described in
:ref:`Sample use case playbooks`.

.. toctree::
   :maxdepth: 2

   module_playbooks
   usecase_playbooks
