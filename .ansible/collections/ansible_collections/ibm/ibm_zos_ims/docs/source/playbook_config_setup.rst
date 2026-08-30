.. ...........................................................................
.. © Copyright IBM Corporation 2020                                          .
.. ...........................................................................

Configuration
=============

Each release of Ansible provides options in addition to the ones identified in
the sample configurations that are included with this collection. These options
allow you to customize how Ansible operates in your environment. Ansible
supports several sources to configure its behavior and all sources follow the
Ansible `precedence rules`_.

The Ansible configuration file `ansible.cfg` can override almost all
``ansible-playbook`` configurations. Included in the `playbooks directory`_ is a
sample `ansible.cfg`_ that can supplement ``ansible-playbook`` with a
little modification.

In the `ansible.cfg`_, the only required configuration is ``pipelining = True``.
Setting the ``pipelining = True`` is **required** because it overrides the
default behavior which is to transfer Ansible modules to the target in binary
via SFTP, however this will fail with the error:

.. warning::
   SyntaxError: Non-UTF-8 code starting with '\x83' in file
   /a/user1/.ansible/tmp/ansible-tmp-1548232945.35-274513842609025/
   AnsiballZ_stat.py on line 1, but no encoding declared;
   see https://python.org/dev/peps/pep-0263/ for details

Setting ``pipelining = True`` in `ansible.cfg`_ will prevent this error.

You can specify the SSH port used by Ansible and instruct Ansible where to
write the temporary files on the target. This can be easily done by adding the
options to your inventory or `ansible.cfg`.

An example of adding these options to `ansible.cfg` is shown below. For more
details, see the sample `ansible.cfg`_ notes.

.. code-block:: yaml

   [defaults]
   forks = 25
   remote_tmp = /u/ansible/tmp
   remote_port = 2022

For more information about available configurations for ``ansible.cfg``, read
the Ansible documentation on `Ansible configuration settings`_.

.. _ansible.cfg:
   https://github.com/ansible-collections/ibm_zos_ims/blob/master/playbooks/ansible.cfg
.. _Ansible configuration settings:
   https://docs.ansible.com/ansible/latest/reference_appendices/config.html#ansible-configuration-settings-locations
.. _precedence rules:
   https://docs.ansible.com/ansible/latest/reference_appendices/general_precedence.html#general-precedence-rules
.. _playbooks directory:
   https://github.com/ansible-collections/ibm_zos_ims/tree/master/playbooks
