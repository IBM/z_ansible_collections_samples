.. ...........................................................................
.. © Copyright IBM Corporation 2020                                          .
.. ...........................................................................

Run the playbook
----------------

Access the sample Ansible playbook and ensure that you are within the collection
playbooks directory where the sample files are included:
``~/.ansible/collections/ibm/ibm_zos_ims/playbooks/``.

Use the Ansible command ``ansible-playbook`` to run the sample playbook.  The
command syntax is ``ansible-playbook -i <inventory> <playbook>``; for example,
``ansible-playbook -i inventory zos-collection-sample.yaml``.

This command assumes that the controller's public SSH key has been shared with
the managed node. If you want to avoid entering a username and password each
time, copy the SSH public key to the managed node using the ``ssh-copy-id``
command; for example, ``ssh-copy-id -i ~/.ssh/mykey.pub user@<hostname>``.

Alternatively, you can use the ``--ask-pass`` option to be prompted for the
user's password each time a playbook is run; for example,
``ansible-playbook -i inventory zos-collection-sample.yaml --ask-pass``.

.. note::
   * Using ``--ask-pass`` is not recommended because it will hinder performance.
   * Using ``--ask-pass`` requires ``sshpass`` be installed on the controller.
     For further reference, see the `ask-pass documentation`_.

Optionally, you can configure the console logging verbosity during playbook
execution. This is helpful in situations where communication is failing and
you want to obtain more details. To adjust the logging verbosity, append more
letter `v`'s; for example, `-v`, `-vv`, `-vvv`, or `-vvvv`. Each letter `v`
increases logging verbosity similar to traditional logging levels INFO, WARN,
ERROR, DEBUG.

.. note::
   It is a good practice to review the playbook samples before executing them.
   It will help you understand what requirements in terms of space, location,
   names, authority, and artifacts will be created and cleaned up. Although
   samples are always written to operate without the need for the user's
   configuration, flexibility is written into the samples because it is not
   easy to determine if a sample has access to the host's resources.
   Review the playbook notes sections for additional details and
   configuration.

   Sample playbooks often submit JCL that is included with this collection
   under the `files directory`_. Review the sample JCL for necessary edits to
   allow for submission on the target system. The most common changes are to
   add a CLASS parameter and change the NOTIFY user parameter. For more details,
   see the JCL notes section included in the collection.

.. _ask-pass documentation:
   https://linux.die.net/man/1/sshpass

.. _files directory:
   https://github.com/ansible-collections/ibm_zos_ims/tree/dev/playbooks/files

