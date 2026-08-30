.. ...........................................................................
.. © Copyright IBM Corporation 2020                                          .
.. ...........................................................................

Group_vars
----------

Although you can store variables in the inventory file, storing separate host
and group variables files may help you organize your variable values more
easily. Included with the sample playbook is a sample variables
file `all.yml`_.

The value for the property **_BPXK_AUTOCVT** must be configured to ``ON``, for
example; ``_BPXK_AUTOCVT: "ON"``.

The value for the property **ZOAU_HOME** is the ZOA Utilities install root path;
for example, ``ZOAU_HOME: "/usr/lpp/IBM/zoautil"``.

The value for the property **PYTHONPATH** is the ZOA Utilities Python library
path; for example, ``PYTHONPATH: "/usr/lpp/IBM/zoautil/lib/"``.

The value for the property **LIBPATH** is both the path to the
**Python libraries** on the target and the **ZOA Utilities Python library**
path separated by colons ``:``; for example,
``LIBPATH: "/usr/lpp/IBM/zoautil/lib/:/usr/lpp/IBM/cyp/v3r8/pyz/lib:/lib:/usr/lib:."``.

The value for the property **PATH** is the **ZOA utilities BIN path** and the
**Python BIN path**; for example,
``PATH: "/usr/lpp/IBM/zoautil/bin:/usr/lpp/IBM/cyp/v3r8/pyz/bin:/bin"``.

The value for the property **_CEE_RUNOPTS** is the invocation Language
Environment® runtime options for programs and used by Python; for example;
``_CEE_RUNOPTS: "FILETAG(AUTOCVT,AUTOTAG) POSIX(ON)"``.

The value for properties **__TAG_REDIR_ERR**, **_TAG_REDIR_IN**,
**_TAG_REDIR_OUT** are ``txt`` and used by the shell; for example,

.. code-block:: sh

  _TAG_REDIR_ERR: "txt"
  _TAG_REDIR_IN: "txt"
  _TAG_REDIR_OUT: "txt"

The value for the property **LANG** is the name of the default locale; the value
**C** specifies the POSIX locale. For example, ``LANG: "C"``.

The included **all.yml** sample variables file contents are:

.. code-block:: yaml

   environment_vars:
     _BPXK_AUTOCVT: "ON"
     ZOAU_HOME: "/usr/lpp/IBM/zoautil"
     PYTHONPATH: "/usr/lpp/IBM/zoautil/lib"
     LIBPATH: "/usr/lpp/IBM/zoautil/lib/:/usr/lpp/IBM/cyp/v3r8/pyz/lib:/usr/lib:/lib:."
     PATH: "/usr/lpp/IBM/zoautil/bin:/usr/lpp/IBM/cyp/v3r8/pyz/bin:/bin"
     _CEE_RUNOPTS: "FILETAG(AUTOCVT,AUTOTAG) POSIX(ON)"
     _TAG_REDIR_ERR: "txt"
     _TAG_REDIR_IN: "txt"
     _TAG_REDIR_OUT: "txt"
     LANG: "C"

.. note::
   In ZOAU 1.0.2 and later, the property **ZOAU_ROOT** is no longer supported
   and must be replaced with the property **ZOAU_HOME**. If you are using ZOAU
   version 1.0.1 or lower, you must continue to use the property
   **ZOAU_ROOT** which is the ZOA Utilities install root path required for
   ZOAU; for example, ``/usr/lpp/IBM/zoautil``.

.. _all.yml:
   https://github.com/ansible-collections/ibm_zos_ims/blob/master/playbooks/group_vars/all.yml


A reusable approach to storing your group variables is to create top level
dependency variables and rely on variable expansion to substitute the values.
This is preferred, because it tends to reduce misconfiguration when copying
dependency paths. In this example, the top level dependency variables ``PYZ``
for Python and ``ZOAU`` have been added and used through the configuration.

.. code-block:: yaml

   PYZ: "/usr/lpp/IBM/cyp/v3r8/pyz"
   ZOAU: "/usr/lpp/IBM/zoautil"

   environment_vars:
     _BPXK_AUTOCVT: "ON"
     ZOAU_HOME: "{{ ZOAU }}"
     PYTHONPATH: "{{ ZOAU }}/lib"
     LIBPATH: "{{ ZOAU }}/lib:{{ PYZ }}/lib:/lib:/usr/lib:."
     PATH: "{{ ZOAU }}/bin:{{ PYZ }}/bin:/bin:/var/bin:/usr/lpp/java/J8.0/bin"
     _CEE_RUNOPTS: "FILETAG(AUTOCVT,AUTOTAG) POSIX(ON)"
     _TAG_REDIR_ERR: "txt"
     _TAG_REDIR_IN: "txt"
     _TAG_REDIR_OUT: "txt"
     LANG: "C"

.. note::

   Currently, IBM Open Enterprise Python for z/OS is the supported and
   recommended Python distribution for use on z/OS with Ansible and ZOAU. If
   Rocket Python is the only available python on the target, please review the
   suggested environment variables below for use with Rocket Python.

.. code-block:: yaml

   ########################################
   # Rocket suggested environment variables
   ########################################
   PYZ: "/usr/lpp/rsusr/python36"
   ZOAU: "/usr/lpp/IBM/zoautil"

   environment_vars:
     ZOAU_ROOT: "{{ ZOAU }}"
     ZOAU_HOME: "{{ ZOAU }}"
     PYTHONPATH: "{{ ZOAU }}/lib:{{ PYZ }}:/lib:/usr/lib"
     _BPXK_AUTOCVT: "ON"
     PATH: "{{ ZOAU }}/bin:/bin:/var/bin:{{ PYZ }}/bin"
     LIBPATH: "{{ ZOAU }}/lib:{{ PYZ }}/lib:/lib:/usr/lib:."