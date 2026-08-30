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

.. _`Modules`:

Modules
=======

Ansible® modules can be used from the command line, in a playbook or in
a role.

The **IBM Z® HMC collection** provides several modules. Reference material for
each module contains documentation on what parameters the module accepts and
what will returned.

Modules targeting the HMC (i.e. not a specific CPC):

.. toctree::
   :maxdepth: 1
   :glob:

   modules/zhmc_user
   modules/zhmc_password_rule
   modules/zhmc_password_rule_list
   modules/zhmc_user_role
   modules/zhmc_user_role_list

Modules supported with CPCs in any operational mode:

.. toctree::
   :maxdepth: 1
   :glob:

   modules/zhmc_cpc
   modules/zhmc_cpc_list

Modules supported only with CPCs in DPM operational mode:

.. toctree::
   :maxdepth: 1
   :glob:

   modules/zhmc_adapter
   modules/zhmc_adapter_list
   modules/zhmc_crypto_attachment
   modules/zhmc_hba
   modules/zhmc_nic
   modules/zhmc_partition
   modules/zhmc_partition_list
   modules/zhmc_storage_group
   modules/zhmc_storage_group_attachment
   modules/zhmc_storage_volume
   modules/zhmc_virtual_function

Modules supported only with CPCs in classic operational mode:

.. toctree::
   :maxdepth: 1
   :glob:

   modules/zhmc_lpar
   modules/zhmc_lpar_list

You can also access the documentation of each module from the command line by
using the `ansible-doc`_ command, for example:

.. code-block:: sh

   $ ansible-doc ibm.ibm_zhmc.zhmc_adapter

.. _ansible-doc:
   https://docs.ansible.com/ansible/latest/cli/ansible-doc.html#ansible-doc
