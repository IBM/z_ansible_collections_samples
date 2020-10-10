.. ...........................................................................
.. © Copyright IBM Corporation 2020                                          .
.. ...........................................................................

Plugins
=======

Plugins that come with the **IBM z/OS core collection** augment Ansible's core
functionality. Ansible uses a plugin architecture to enable a rich, flexible
and expandable feature set.

Action
------

* ``zos_ping``: A fork of Ansible `normal.py`_ action plugin that is modified to allow a conditional shebang line in REXX modules.

* ``zos_job_submit``: Used to `submit a job`_ from the controller and optionally monitor the job completion.

.. _normal.py:
   https://github.com/ansible/ansible/blob/devel/lib/ansible/plugins/action/normal.py
.. _submit a job:
   modules/zos_job_submit.html

Connection
----------

* ``zos_ssh``: Enables the Ansible controller to communicate with a z/OS target machine by using SSH, with the added support to transfer ASCII as EBCDIC when transferring REXX modules. This connection plugin was forked from the Ansible `ssh.py`_ connection plugin.
* For further reference, see **z/OS Connection Plugin**.

.. _ssh.py:
        https://github.com/ansible/ansible/blob/devel/lib/ansible/plugins/connection/ssh.py

z/OS Connection Plugin
----------------------

Since EBCDIC encoding is used on z/OS, custom plugins are required to determine
the correct transport method when targeting a z/OS system. The zos_ssh.py
connection plugin is a fork of the default ssh.py plugin with the added
functionality to check if a module is written in REXX.

Since REXX scripts are required be in EBCDIC encoding to run, they must be
handled differently during transfer. If the string
``__ANSIBLE_ENCODE_EBCDIC__`` is found in the first line of the module, the
module is transferred to the target system using SCP. Otherwise, SFTP is used.
SCP treats files as text, automatically encoding as EBCDIC at transfer time.
SFTP treats files as binary, performing no encoding changes.

**REXX Module Configuration**:

* Ensure a REXX modules first line is a comment containing the case insensitive keyword ``rexx``
* Followed by the case sensitive value ``__ANSIBLE_ENCODE_EBCDIC__``


**Example REXX module**:

.. code-block:: sh

   /* rexx  __ANSIBLE_ENCODE_EBCDIC__  */
   x = 55
   SAY '{"SYSTEM_VERSION":"' x '"}'
   RETURN 0


