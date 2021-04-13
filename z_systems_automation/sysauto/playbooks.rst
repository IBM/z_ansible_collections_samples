.. ...........................................................................
.. © Copyright IBM Corporation 2020                                          .
.. ...........................................................................

=======================
Playbooks
=======================

An `Ansible playbook`_ consists of organized instructions that define work for
a managed node (host) to be managed with Ansible.

Sample playbooks that demonstrate how to use the collection content are **included**
in the **IBM Z System Automation collection**. You can find the samples on `Github`_ or in the collections playbooks
directory included with the installation.
For more information about the collections installation and hierarchy, refer to the `Installation`_ documentation of this collection.

The sample playbooks can be run with the ``ansible-playbook`` command with some
modifications to the **inventory** and variable files.


Ansible Configuration
=====================

Ansible configuration file ``ansible.cfg`` can override nearly all ``ansible-playbook`` configurations. 

You can modify the following configuration statement to refer to your own installation path for the collection:

.. code-block:: yaml

   collections_paths = ~/.ansible/collections:/usr/share/ansible/collections

For more information about available configurations for ``ansible.cfg``, see `Ansible Configuration Settings`_.

Inventory
=========

Ansible works with multiple managed nodes (hosts) at the same time, using a list or group of lists known as an `inventory`_.
Once the inventory is defined, you can use `patterns`_ to select the hosts, or groups, you want Ansible to run against.

Included in the `playbooks directory`_ is a sample inventory file `inventory.yaml`_ that with little modification
can be used to manage the target z/OS systems. This inventory file should be included when running the sample playbook.

.. code-block:: yaml

   sample:
     hosts:
       sampleHost1:
       sampleHost2:

* **sample**: An example of host grouping.

   * **sampleHost1**: Nickname for the target z/OS system. You can modify it to refer to your own z/OS system.
   
Host Vars
=========

You can supply host variables in either the inventory file or the separate variable file. Storing separate host and group
variables files may help you organize your variable values more easily.

Included in the `playbooks directory`_ are variable files in the directory `host_vars`_ one for each host nickname (provided in the hosts inventory file).

* `sampleHost1.yaml`_: It contains the variables for host ``sampleHost1``:

   .. code-block:: yaml

      sa_service_hostname: your.host.name
      sa_service_port: port_number
      sa_service_protocol: http or https

   * **sa_service_hostname**: The value of this property identifies the host name of the IBM Z System Automation Operations REST server.
     
   * **sa_service_port**: The value of this property identifies the port number of the IBM Z System Automation Operations REST server.
   
   * **sa_service_protocol**: The value (http or https) of this property identifies if you have configured your IBM Z System Automation Operations REST server to use SSL.
   
   Refer to the `configuration`_ of the System Automation Operations REST Server for details about these settings.

Variables
=========

We supply sample variables in a variables file. Storing separate host and variables files may help you organize your variable values more easily.

Included in the `playbooks directory`_ is a sample variables file in the directory `vars`_.

* `vars.yaml`_: It contains the variables for the playbooks:

   .. code-block:: yaml

      templateName: name_of_the_template
      subsystem: subsystem_name
      system: system_name
      job: jobname
      # procedure: procedureName
      # comment: comment
      # group: group
      # sdesc: "a short description"
      
   * **templateName**: The value of this property specifies the template name that will be used to create the dynamic resource. This parameter is mandatory.
   
   * **subsystem**: The value of this property specifies the subsystem name of the new resource. This parameter is mandatory.
   
   * **system**: The value of this property specifies the system where the resource will be created. This parameter is mandatory.
   
   * **job**: The value of this property specifies the job name of the new resource. This parameter is mandatory.
   
   * **procedure**: The value of this property specifies the procedure name used by the new resource. This parameter is optional.
   
   * **comment**: The value of this property specifies a comment to be associated with the creation of the new resource. This parameter is optional.
   
   * **group**: The value of this property specifies the automation name of the application group (APG) that will host the new resource. This parameter is optional.
   
   * **sdesc**: The value of this property specifies a short description of the new resource. This parameter is optional.
   
Sample Playbooks
================

.. toctree::
   :maxdepth: 1
   :glob:

   playbooks/sample_pb_create_dynres
   playbooks/sample_pb_delete_dynres

Run the Playbooks
=================

The sample playbooks must be run from the playbooks directory of the installed collection: ``~/.ansible/collections/ansible_collections/ibm/ibm_zos_sysauto/playbooks/``

Use the `ansible-playbook`_ command to run a sample playbook. The command syntax is:

.. code-block:: sh

   $ ansible-playbook [-i INVENTORY] sample_pb_*.yaml
   
for example:

.. code-block:: sh

   $ ansible-playbook -i inventory.yaml sample_pb_create_dynres.yaml
   

To adjust the logging verbosity, include the ``-v`` option with `ansible-playbook`_ command. You can append more letter ``v``'s, for example, ``-v``, ``-vv``, ``-vvv``, or ``-vvvv``, to obtain more details in case a connection failed.
Each letter ``v`` increases the logging verbosity similar to the traditional logging levels, such as INFO, WARN, ERROR, or DEBUG.

   
.. _Ansible Playbook:
   https://docs.ansible.com/ansible/latest/user_guide/playbooks_intro.html#playbooks-intro
.. _Github:
   https://github.com/ansible-collections/ibm_zos_sysauto/tree/main/playbooks
.. _playbooks directory:   
   https://github.com/ansible-collections/ibm_zos_sysauto/blob/main/playbooks 
.. _Installation:
   installation.html
.. _ansible.cfg:
   https://github.com/ansible-collections/ibm_zos_sysauto/blob/main/playbooks/ansible.cfg
.. _Ansible Configuration Settings:
   https://docs.ansible.com/ansible/latest/reference_appendices/config.html#ansible-configuration-settings-locations
.. _inventory:
   https://docs.ansible.com/ansible/latest/user_guide/intro_inventory.html
.. _patterns:
   https://docs.ansible.com/ansible/latest/user_guide/intro_patterns.html#intro-patterns
.. _inventory.yaml:
   https://github.com/ansible-collections/ibm_zos_sysauto/blob/main/playbooks/inventory.yaml
.. _host_vars:
   https://github.com/ansible-collections/ibm_zos_sysauto/blob/main/playbooks/host_vars/
.. _vars:
   https://github.com/ansible-collections/ibm_zos_sysauto/blob/main/playbooks/vars/
.. _vars.yaml:   
   https://github.com/ansible-collections/ibm_zos_sysauto/blob/main/playbooks/vars/vars.yaml
.. _sampleHost1.yaml:   
   https://github.com/ansible-collections/ibm_zos_sysauto/blob/main/playbooks/host_vars/sampleHost1.yaml
.. _ansible-playbook:
   https://docs.ansible.com/ansible/latest/cli/ansible-playbook.html
.. _configuration:
   https://www.ibm.com/support/knowledgecenter/de/SSWRCJ_4.2.0/com.ibm.safos.doc_4.2/InstallPlan/JCL_procedure_embeded.html