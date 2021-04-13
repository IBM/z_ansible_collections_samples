.. ...........................................................................
.. © Copyright IBM Corporation 2020, 2021                                          .
.. ...........................................................................

sample_pb_create_dynres
=======================

For configuration and setup, see `Playbook Documentation`_.

This `sample playbook`_ shows how to create and resume a dynamic resource using the role `sa_create_dynamic_resource`_.

You are prompted for your NetView credentials during execution of the playbook. The environment variables "username_value"
and "password_value" are taken as default values for the credentials.

.. code-block:: yaml

   - name: create and resume a dynamic resource in SA
     hosts: sampleHost1
     gather_facts: no
     collections:
       - ibm.ibm_zos_sysauto
     vars_files:
       - ./vars/vars.yaml
	 vars_prompt:
       - name: username
         prompt: "Enter your username"
		 default: "{{ lookup('env', 'username_value') }}"
         private: no

       - name: password
         prompt: "Enter your password"
		 default: "{{ lookup('env', 'password_value') }}"
         private: yes
     tasks:
       - include_role:
           name: sa_create_dynamic_resource

.. note::

  The inventory file `hosts`_ and host vars (`sampleHost1.yaml`_) need to be updated to identify the
  target IBM Z System Automation Operations REST server end points.
  For more details refer to the documentation of `inventory`_ and `host vars`_.


.. _Playbook Documentation:
   ../playbooks.html
.. _sample playbook:
   https://github.com/ansible-collections/ibm_zos_sysauto/blob/main/playbooks/sample_pb_create_dynres.yaml
.. _hosts:
   https://github.com/ansible-collections/ibm_zos_sysauto/blob/main/playbooks/hosts
.. _sampleHost1.yaml:
   https://github.com/ansible-collections/ibm_zos_sysauto/blob/main/playbooks/host_vars/sampleHost1.yaml
.. _sa_create_dynamic_resource:
   ../roles/sa_create_dynamic_resource.html
.. _inventory:
   ../playbooks.html#inventory
.. _host vars:
   ../playbooks.html#host-vars
