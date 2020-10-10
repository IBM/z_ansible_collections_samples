.. ...........................................................................
.. © Copyright IBM Corporation 2020                                          .
.. ...........................................................................

=========
z/OS Core
=========

The **IBM z/OS core collection**, also represented as **ibm\_zos\_core**
in this document, is part of the broader offering **Red Hat® Ansible
Certified Content for IBM Z**. IBM z/OS core collection supports tasks
such as creating data sets, submitting jobs, querying jobs,
retrieving job output, encoding data sets, fetching data sets, operator
commands, TSO commands, ping and querying operator actions.

The **IBM z/OS core collection** serves as a dependency for other collections
under the **Red Hat® Ansible Certified Content for IBM Z** umbrella and
works closely with offerings such as `IBM z/OS IMS collection`_ to deliver
a solution that will enable you to automate tasks on z/OS subsystems such
as IMS.

Modules include documentation that is similar to a UNIX, or UNIX-like operating
system man page (manual page). This documentation can be accessed from the
command line by using the ``ansible-doc`` command documented in the
`Ansible guide`_.

Here's how to use the ``ansible-doc`` command after you install the
**IBM z/OS core collection**: ``ansible-doc ibm.ibm_zos_core.zos_data_set``

.. code-block:: sh

   $ansible-doc ibm.ibm_zos_core.zos_data_set

   > ZOS_DATA_SET    (/Users/user/.ansible/collections/ansible_collections/ibm/ibm_zos_core/plugins/modules/zos_data_set.py)

      Create, delete and set attributes of data sets. When forcing data set replacement, contents will not be preserved.

    OPTIONS (= is mandatory):

    - batch
            Batch can be used to perform operations on multiple data sets in a single module call.
            Each item in the list expects the same options as zos_data_set.
            type: list

    - data_class
            The data class name (required for SMS-managed data sets)
            type: str


.. _Ansible guide:
   https://docs.ansible.com/ansible/latest/cli/ansible-doc.html#ansible-doc

.. _IBM z/OS IMS collection:
   https://github.com/ansible-collections/ibm_zos_ims


.. toctree::
   :maxdepth: 1
   :caption: Reference

   source/plugins
   source/modules
   source/filters
