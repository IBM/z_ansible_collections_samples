.. ...........................................................................
.. © Copyright IBM Corporation 2020                                          .
.. ...........................................................................

Filters
=======

Filters in Ansible are from Jinja2, and are used to transform data inside
a template expression. The templates operate on the Ansible controller, and not
on the target host. Therefore, filters execute on the controller as they augment
the data locally.

Jinja2 ships with many filters as does Ansible, and also allows users to add
their own custom filters.

The **IBM z/OS core collection** includes filters and their usage in sample
playbooks. Unlike collections that can be identified at the top level using the
collections keyword, filters must always be specified in the playbook with their
fully qualified name even when included in a collection.

Filters usage follows this pattern:

   .. note::
         * <namespace>.<collection>.<filter>
         * ibm.ibm_zos_core.filter_wtor_messages('IEE094D SPECIFY OPERAND')

For more details on filters, review the filters and documentation under
the `filter`_ directory included in the collection.

.. _filter:
   https://github.com/ansible-collections/ibm_zos_core/tree/master/plugins/filter/






