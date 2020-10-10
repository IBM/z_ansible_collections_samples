.. ...........................................................................
.. © Copyright IBM Corporation 2020                                          .
.. ...........................................................................

Prerequisites
=============

The following prerequisites must be met before you develop and run test cases
for z/OS Ansible modules.

Control node
------------

* `Ansible version`_: 2.9 or later
* `Python`_: 3 or later
* `OpenSSH`_

.. _Ansible version:
   https://docs.ansible.com/ansible/latest/installation_guide/intro_installation.html
.. _Python:
   https://www.python.org/downloads/release/latest
.. _OpenSSH:
   https://www.openssh.com/


Managed node
------------

* `IBM Open Enterprise Python for z/OS`_
* z/OS `V2R3`_ or `later`_
* `IBM Z Open Automation Utilities`_ (ZOAU)
* The z/OS® shell

   * IBM z/OS collections are dependent on specific versions of ZOAU. When
     configuring the dependencies, refer to the collections requirements
     and dependencies to ensure proper execution.
* `z/OS OpenSSH`_


.. _IBM Open Enterprise Python for z/OS:
   http://www.ibm.com/products/open-enterprise-python-zos

.. _V2R3:
   https://www.ibm.com/support/knowledgecenter/SSLTBW_2.3.0/com.ibm.zos.v2r3/en/homepage.html

.. _later:
   https://www.ibm.com/support/knowledgecenter/SSLTBW

.. _IBM Z Open Automation Utilities:
   https://www.ibm.com/support/knowledgecenter/en/SSKFYE

.. _z/OS OpenSSH:
   https://www.ibm.com/support/knowledgecenter/SSLTBW_2.2.0/com.ibm.zos.v2r2.e0za100/ch1openssh.htm


Configuration and Dependencies
==============================

This section explains how to configure the environment and which dependencies to
install to run both the functional and unit test cases.

Install dependencies
--------------------

Install the requirements using the supplied ``requirements.txt`` with ``pip3``.
In the root folder of the collection, run the command:

.. code-block:: sh

   $ pip3 install -r tests/requirements.txt


If the command responds with ``ERROR: Could not install packages due to an EnvironmentError: [Errno 13] Permission denied``
run the command with the additional ``-user`` option:

.. code-block:: sh

   $ pip3 install --user -r tests/requirements.txt


SSH Keys
--------

Generate and add a new SSH key using `ssh-keygen`_ and `ssh-add`_.

.. _ssh-keygen:
   https://www.ssh.com/ssh/keygen/

.. _ssh-add:
   https://www.ssh.com/ssh/add


.. image:: https://zenhub.ibm.com/images/5c75c71e85b6d5070636e1d8/8bd18b60-7517-4301-b3d4-17857e3a5e49


Copy the public key to a target host, using `ssh-copy-id`_.

.. _ssh-copy-id:
   https://www.ssh.com/ssh/copy-id


.. image:: https://zenhub.ibm.com/images/5c75c71e85b6d5070636e1d8/63102702-1dd4-4578-8539-33beb496bf69



Configuration and Arguments
---------------------------

Create a YAML file containing the required information to run the functional tests.

+-------------+------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------+----------+---------+
| Argument    | Description                                                                                                                                                                                                                                                      | Required | Aliases |
+=============+==================================================================================================================================================================================================================================================================+==========+=========+
| host        | The z/OS target host to connect to.                                                                                                                                                                                                                              | True     |         |
+-------------+------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------+----------+---------+
| user        | The username for authentication with host.                                                                                                                                                                                                                       | True     |         |
+-------------+------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------+----------+---------+
| python_path | The absolute path to the python interpreter on the z/OS target host.                                                                                                                                                                                             | True     |         |
+-------------+------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------+----------+---------+
| environment | A list of key-value pairs containing all environment variables that must be set on the z/OS target host before running Python/Ansible. It is important to add two sets of quotes when quotations are desired in the environment variable _export_ statement.     | False    |         |
+-------------+------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------+----------+---------+


Example YAML configuration:

.. code-block:: yaml

   host: some.host.name
   user: myuser
   python_path: /usr/lpp/IBM/cyp/v3r8/pyz
   environment:
     _BPXK_AUTOCVT: "ON"
     _CEE_RUNOPTS: "'FILETAG(AUTOCVT,AUTOTAG) POSIX(ON)'"
     _TAG_REDIR_ERR: txt
     _TAG_REDIR_IN: txt
     _TAG_REDIR_OUT: txt
     LANG: C
     ZOAU_ROOT: "/usr/lpp/IBM/zoautil"
     ZOAU_HOME: "/usr/lpp/IBM/zoautil"
     LIBPATH: "/usr/lpp/IBM/zoautil/lib:/usr/lpp/IBM/cyp/v3r8/pyz/lib:/lib:/usr/lib:."
     ZOAUTIL_DIR: "/usr/lpp/IBM/zoautil"
     PYTHONPATH: "/usr/lpp/IBM/zoautil/lib"
     PATH: "/usr/lpp/IBM/zoautil/bin:/usr/lpp/IBM/cyp/v3r8/pyz/bin:/bin:/var/bin"

Run Functional and Unit Tests
=============================

After completing the steps in "Configuration and Dependencies", you can run the
functional and unit tests. Running the functional tests requires access to a
z/OS system; the unit tests do not require z/OS access.


Run Functional Tests
--------------------

To run the functional tests, you will need a Python 3 environment.
Pass the YAML configuration to  command ``pytest`` with the option ``--zinventory``
or ``-z``. By default, ``pytest`` looks for the YAML configuration in the local
directory as **test-config.yaml**. In this example, we assume that the path to the
YAML configuration is **/home/myuser/test_config.yml**.

If the environment is using Python 2, or Python 3 is not present in the host PATH:

.. code-block:: yaml

   $ python3 -m pytest --host-pattern=all --zinventory=/home/myuser/test_config.yml


If the environment is configured with Python 3:

.. code-block:: yaml

   $ pytest --host-pattern=all --zinventory=/home/myuser/test_config.yml


Run Unit Tests
--------------

The unit tests require minimal configuration to get started and do not require
access to a z/OS system to run. To run the unit tests, navigate to the
unit tests folder:


.. code-block:: sh

    ansible_collections_ibm_zos_core
    └── tests
        └── units


If the environment is using Python 2 or Python 3 is not in the host PATH:

.. code-block:: yaml

   $ python3 -m pytest


If the environment is configured with Python 3:

.. code-block:: yaml

   $ pytest


CLI Arguments
-------------

By default, the ``pytest`` options ``--zinventory``or ``-z`` are used to provide
the absolute path to the configuration YAML file. Additionally, certain select
arguments from `pytest-ansible`_ can be used. These can be passed as command
line options or provided in the YAML configuration file
**/home/myuser/test_config.yml**.

.. _pytest-ansible:
   https://github.com/ansible/pytest-ansible


Some options noted below with **NOT SUPPORTED**, may work with additional
testing.

.. code-block:: sh

   pytest \
       NOT SUPPORTED [--inventory <path_to_inventory>] \
       REQUIRED [--host-pattern <host-pattern>] \
       [--connection <plugin>] \
       BROKEN [--module-path <path_to_modules] \
       [--user <username>] \
       [--become] \
       [--become-user <username>] \
       [--become-method <method>] \
       NOT SUPPORTED [--limit <limit>] \
       [--check]


If you choose to use the options in the command over **test_config.yaml**, a
command line example would look like:

.. code-block:: sh

   pytest --host-pattern=all --user guest --become-user root --zinventory=/home/myuser/test_config.yml


Add module directory to ANSIBLE_LIBRARY
---------------------------------------

Typically, ``module_path`` in the YAML configuration, or ``--module-path`` on the CLI would be
a valid parameter which would function as an alternative to setting the
ANSIBLE_LIBRARY environment variable. However, this option is not functional in the
**2.9.5** Ansible release.

If the modules are in:

.. code-block:: sh

   /
   └── Users
       └── myuser
           └── ansible_collections_ibm_zos_core
               └── plugins
                   └── modules


Use this command to add the module directory to **ANSIBLE_LIBRARY** :

.. code-block:: sh

   $ export ANSIBLE_LIBRARY=/Users/myuser/ansible_collections_ibm_zos_core/plugins/modules


Now that the module directory has been exported, navigate to the functional
tests folder to run the test with the added module directory.

Navigate to the functional tests folder:

.. code-block:: sh

   ansible_collections_ibm_zos_core
   └── tests
       └── functional


If the environment is using Python 2 or Python 3 is not in the host PATH:

.. code-block:: sh

   $ python3 -m pytest --host-pattern=all --zinventory=/home/myuser/test_config.yml


If the environment is configured with Python 3:

.. code-block:: yaml

   $ pytest --host-pattern=all --zinventory=/home/myuser/test_config.yml



Develop Unit Tests with pytest
==============================

This section provides an overview of unit and functional testing z/OS Ansible
modules. Multiple `pytest fixtures`_ are provided to simplify the testing
process.

.. _pytest fixtures:
   https://docs.pytest.org/en/latest/fixture.html


Mocking z/OS Packages, Methods, and Functions
-------------------------------------------------------
Unit tests do not require access to a z/OS system for execution. When
functions or methods are dependent on a missing z/OS Python package such
as `zoautil_py`, the imports of both the package and direct calls to the
missing library should be "mocked".

Unit Testing Resources
----------------------
* `unittest.mock`_ - allows you to replace parts of your system under test with
  mock objects and make assertions about how they have been used.

  ``pip`` can be used to install ``mock`` in python 2 and 3. In Python 3,
  ``unittest.mock`` is included with the Python installation.

  .. note::
     While examples may reference ``unittest.mock``, or ``mock``
     directly for certain purposes, the ``pytest`` framework is the recommended
     testing framework. The use of the ``unittest`` framework is not recommended.

* `pytest-mock`_ - installs a ``mocker`` fixture which is a thin-wrapper around
  the patching API provided by the mock package, but with the benefit of not
  having to worry about undoing patches at the end of a test.

.. _unittest.mock:
   https://docs.python.org/3/library/unittest.mock.html

.. _pytest-mock:
   https://github.com/pytest-dev/pytest-mock


zos_import_mocker
-----------------
The `zos_import_mocker`_ pytest fixture is designed to simplify mocking import
dependencies without causing side-effects.

.. _zos_import_mocker:
    https://github.com/ansible-collections/ibm_zos_core/blob/dev/tests/conftest.py#L57

`zos_import_mocker`_ returns two items when it is provided to a test:

#. A mocker object from `pytest-mock`_.

   From the docs:

    .. code-block:: none

       a thin-wrapper around the patching API provided by the mock package, but
       with the benefit of not having to worry about undoing patches at the end
       of a test


   Behind the scenes, ``zos_import_mocker`` uses the mocker fixture to patch
   the ``zoautil_py`` package with a MagicMock object. With ``zoautil_py``
   patched, modules that depend on ``zoautil_py`` can be imported and used
   without raising exceptions.

#. The ``perform_imports()`` function.

   The ``perform_imports()`` function accepts a single import string or a list
   of import strings as arguments. Imports that depend on ``zoautil_py`` should
   utilize this function to simplify imports.

   It returns the imports for use by the test cases. If a single import string
   was provided, a single import is returned. If a list of import strings was
   provided, a list of imports is returned.

   The example code assigns the name ``importer()`` to the returned
   ``perform_imports()`` function.

``zos_import_mocker`` uses function scope. Therefore, the mock of
``zoautil_py`` and any operations performed with the provided mocker will be
cleared after each function.

.. _pytest-mock:
    https://github.com/pytest-dev/pytest-mock


Example ``zos_import_mocker`` Usage
------------------------------------

.. code-block:: python

   from __future__ import (absolute_import, division)
   __metaclass__ = type

   from ansible.module_utils.basic import AnsibleModule
   import pytest
   import sys
   from mock import call

   # The IMPORT_NAME import string is passed to importer() in each test case
   IMPORT_NAME = 'ansible_collections_ibm_zos_core.plugins.module   s.datasets.zos_dataset'

   # Tests for create_dataset()
   dummy_dict = {
       'type': 'pds',
       'size': '50M'
   }

   test_data = [
       ('test1.tester.test', dummy_dict, 0, True),
       ('test1.tester.test', {}, 0, True),
       (None, {}, 1, False),
       ('test1.tester.test', None, 0, True),
       ('test1.tester.test', dummy_dict, 1, False)
   ]

   @pytest.mark.parametrize("dsname,args,return_value,expected", test_data)
   def test_create_dataset_various_args(zos_import_mocker, dsname, args, return_value, expected):
       mocker, importer = zos_import_mocker
       ds = importer(IMPORT_NAME)
       passed = True
       mocker.patch('zoautil_py.Datasets.create',
                   create=True, return_value=return_value)
       try:
           ds.create_dataset(dsname, args)
       except ds.DatasetCreateError:
           passed = False
       except TypeError as e:
           # MagicMock throws TypeError when input args is None
           # But if it gets that far we consider it passed
           if 'MagicMock' not in str(e):
               passed = False
       assert passed == expected

   def test_create_dataset_missing_all_args(zos_import_mocker):
       mocker, importer = zos_import_mocker
       ds = importer(IMPORT_NAME)
       mocker.patch('zoautil_py.Datasets.create', create=True)
       with pytest.raises(TypeError):
           ds.create_dataset()

   def test_create_dataset_missing_second_arg(zos_import_mocker):
       mocker, importer = zos_import_mocker
       ds = importer(IMPORT_NAME)
       patched_method = mocker.patch(
           'zoautil_py.Datasets.create', create=True, return_value=0)
       ds.create_dataset('testname')
       patched_method.assert_called_with('testname')

   def test_create_dataset_arg_expansion(zos_import_mocker):
       mocker, importer = zos_import_mocker
       ds = importer(IMPORT_NAME)
       item1 = 'value1'
       item2 = 'value2'
       item3 = 'value3'
       to_expand = {
           'item1': item1,
           'item2': item2,
           'item3': item3
       }
       patched_method = mocker.patch(
           'zoautil_py.Datasets.create', create=True, return_value=0)
       ds.create_dataset('testname', to_expand)
       patched_method.assert_called_with(
           'testname', item1=item1, item2=item2, item3=item3)

   def test_create_dataset_exception_receiving_name(zos_import_mocker):
       mocker, importer = zos_import_mocker
       ds = importer(IMPORT_NAME)
       mocker.patch('zoautil_py.Datasets.create', create=True, return_value=1)
       ds_name = 'testdsn'
       patched_method = mocker.patch.object(
           ds.DatasetCreateError, '__init__', return_value=None)
       try:
           ds.create_dataset('testdsn')
       except ds.DatasetCreateError:
           pass
       patched_method.assert_called_with(ds_name)


Develop Functional Tests with pytest
====================================

Functional tests should execute modules on the z/OS target node and validate
return values are valid and desired action(s) are achieved.

Functional Testing Resources
----------------------------
* `pytest-ansible`_ - contains a plugin for ``pytest`` which adds several
  fixtures for running ansible modules, or inspecting ansible_facts.

.. _pytest-ansible:
   https://github.com/ansible/pytest-ansible

The plugin from ``pytest-ansible`` is used by one of our own pytest fixtures.

`pytest-ansible` does not work out of the box for z/OS Ansible modules due
to `pytest-ansible` using the `adhoc` command to drive module testing behind
the scenes. `adhoc` commands do not support setting environment variables on
the target host, which is needed to run z/OS Python.

The `ansible_zos_module`_ pytest fixture reads the `YAML configuration`_,
provisions a temporary python interpreter, and sets up the ``ansible`` plugin
from ``pytest-ansible``.

``ansible_zos_module`` returns the equivalent of the
`pytest-ansible adhoc fixture`_ and can be used similarly.


.. _YAML configuration:
   zos_ansible_module_testing.html#configuration-and-arguments

.. _pytest-ansible adhoc fixture:
   https://github.com/ansible/pytest-ansible#fixture-ansible_adhoc

.. _ansible_zos_module:
   https://github.com/ansible-collections/ibm_zos_core/blob/dev/tests/conftest.py#L37


Details
-------
``ansible_zos_module`` uses the ``request`` and `z_python_interpreter`_
fixtures.

.. _z_python_interpreter:
   https://github.com/ansible-collections/ibm_zos_core/blob/dev/tests/conftest.py

The ``z_python_interpreter`` fixture performs the following actions:

#. Parses the YAML config.
#. Builds our new interpreter by wrapping the provided Python interpreter with
   needed environment configuration.
#. Builds a dictionary containing all of the arguments from the YAML
   configuration that should be passed to the ``ansible pytest-ansible`` plugin.

The ``z_python_interpreter`` fixture returns two values:

#. **interpreter** - the temporary interpreter string.

   ``ansible_zos_module`` injects the interpreter string into the default
   interpreter path variable used by the ``ansible pytest-ansible`` plugin so
   the temporary interpreter string is used when running modules.

    The `interpreter` variable is a string containing all of the environment
    variable exports followed by the python interpreter. The export statements
    are required, otherwise USS will not execute the interpreter.
#. **inventory** - a dictionary containing all of the arguments to provide
   during ``ansible pytest-ansible`` plugin initialization.

   ``ansible_zos_module`` handles the plugin initialization.

.. note::
   ``z_python_interpreter`` shouldn't be used directly. It is solely
   used as a fixture to ``ansible_zos_module``.

``ansible_zos_module`` is scoped to the test session. Therefore, only one
temporary Python interpreter is used each time Pytest is run.

Example ``ansible_zos_module`` usage
------------------------------------

.. code-block:: python

   from __future__ import absolute_import, division

   import os
   import sys
   import warnings

   import ansible.constants
   import ansible.errors
   import ansible.utils
   import pytest

   __metaclass__ = type

   def test_dataset_creation(ansible_zos_module):
       hosts = ansible_zos_module
       # * hosts.all.zos_dataset tells adhoc to run the zos_datset module against all provided hosts.
       # * In our case, there is only a single host. This may change in the future.
       results = hosts.all.zos_dataset(name='imsbank.ims1.test01', state='present', replace=True)
       # * results.contacted contains a list where each list item represents a host on which the module ran
       for result in results.contacted.values():
           assert result['state'] == 'present'
           assert result['changed'] == True
           assert result.get('module_stderr') == None


Additional Development Tips
===========================

When using ``ansible_zos_module``, it may be beneficial to view the structure
output by **results.contacted** or other objects.

The `pprint`_ Python module can be used to format output.

.. _pprint:
   https://docs.python.org/3/library/pprint.html

By using ``pprint.pprint(vars(someobject))``, we can print content from any
object containing ``__dict__``.

.. code-block:: python

   from __future__ import absolute_import, division

   import os
   import sys
   import warnings

   import ansible.constants
   import ansible.errors
   import ansible.utils
   import pytest
   from pprint import pprint

   __metaclass__ = type

   def test_dataset_creation(ansible_zos_module):
       hosts = ansible_zos_module
       # * hosts.all.zos_dataset tells adhoc to run the zos_datset module against all provided hosts.
       # * In our case, there is only a single host. This may change in the future.
       results = hosts.all.zos_dataset(name='imsbank.ims1.test01', state='present', replace=True)
       # * results.contacted contains a list where each list item represents a host on which the module ran
       pprint(vars(results))
       for result in results.contacted.values():
           assert result['state'] == 'present'
           assert result['changed'] == True
           assert result.get('module_stderr') == None


To get debug output when running pytest, use the `-s` flag.
Assuming the absolute path of the YAML configuration file is
**/home/myuser/test_config.yml**

The the environment is using Python 2 or Python 3 is not in the host PATH:

.. code-block:: sh

   python3 -m pytest -s --host-pattern=all --zinventory=/home/myuser/test_config.yml


The the environment is configured with Python 3:

.. code-block:: sh

   pytest -s --host-pattern=all --zinventory=/home/myuser/test_config.yml
