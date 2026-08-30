.. ...........................................................................
.. © Copyright IBM Corporation 2020                                          .
.. ...........................................................................

Inventory
---------

Ansible works with multiple managed nodes (hosts) at the same time, using a
list or group of lists known as an `inventory`_. Once the inventory is defined,
you can use `patterns`_ to select the hosts or groups that you want Ansible to
run against.

Included in the `playbooks directory`_ is a `sample inventory file`_ that can be
used to manage your nodes with a little modification. This inventory file
should be included when running the sample playbook.

.. code-block:: yaml

   zsystem:
     hosts:
       zvm:
         ansible_host: zos_target_address
         ansible_user: zos_target_username
         ansible_python_interpreter: path_to_python_interpreter_binary_on_zos_target


The value for the property **ansible_host** is the hostname of the managed node;
for example, ``ansible_host: ec33017A.vmec.svl.ibm.com``

The value for the property **zos_target_username** is the user name to use when
connecting to the host; for example, ``ansible_user: omvsadm``.

The value for the property **ansible_python_interpreter** is the target host
Python path. This is useful for systems with more than one Python installation,
or when Python is not installed in the default location **/usr/bin/python**;
for example,
``ansible_python_interpreter: /usr/lpp/IBM/cyp/v3r8/pyz/bin/python3``

For more information on Python configuration requirements on z/OS, refer to
Ansible `FAQ`_.

Behavioral inventory parameters such as ``ansible_port`` which allows you
to set the port for a host can be reviewed in the
`behavioral inventory parameters`_.

.. _inventory:
   https://docs.ansible.com/ansible/latest/user_guide/intro_inventory.html
.. _patterns:
   https://docs.ansible.com/ansible/latest/user_guide/intro_patterns.html#intro-patterns
.. _sample inventory file:
   https://github.com/ansible-collections/ibm_zos_ims/blob/master/playbooks/inventory
.. _FAQ:
   https://docs.ansible.com/ansible/latest/reference_appendices/faq.html#running-on-z-os
.. _behavioral inventory parameters:
   https://docs.ansible.com/ansible/latest/user_guide/intro_inventory.html#connecting-to-hosts-behavioral-inventory-parameters
.. _playbooks directory:
   https://github.com/ansible-collections/ibm_zos_ims/tree/master/playbooks
