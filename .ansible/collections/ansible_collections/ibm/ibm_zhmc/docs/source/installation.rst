.. Copyright 2017-2020 IBM Corp. All Rights Reserved.
..
.. Licensed under the Apache License, Version 2.0 (the "License");
.. you may not use this file except in compliance with the License.
.. You may obtain a copy of the License at
..
..    http://www.apache.org/licenses/LICENSE-2.0
..
.. Unless required by applicable law or agreed to in writing, software
.. distributed under the License is distributed on an "AS IS" BASIS,
.. WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
.. See the License for the specific language governing permissions and
.. limitations under the License.
..


.. _`Installation`:

Installation
============

The system Ansible is installed on is called the "control system". This is
where Ansible commands (such as ``ansible-playbook``) are invoked.

Ansible is written in Python but invokes Ansible modules always as executables,
even when they are also written in Python. Therefore, Ansible modules
implemented in Python are run as Python scripts and are not imported as Python
modules.

The standard installation is that Ansible is installed as an operating system
package and uses the existing system Python. The Ansible modules then also use
the system Python.

As an alternative to the standard installation, it is possible to use a
`virtual Python environment`_ for Ansible itself and for Ansible modules
written in Python. Using a virtual Python environment has the main advantages
that it minimizes the risk of incompatibilities between Python packages because
the virtual environment contains only the packages that are needed for the
specific use case, and that it does not pollute your system Python installation
with other Python packages, keeping the risk of incompatibilities away from
your system Python.

.. _`virtual Python environment`: https://docs.python-guide.org/dev/virtualenvs/

The following sections describe these two installation methods.


Standard installation with system Python
----------------------------------------

All commands shown are to be executed in a terminal or command prompt on the
control system. The instructions are written for a bash shell.

.. _`Installing Ansible on specific operating systems`: https://docs.ansible.com/ansible/latest/installation_guide/intro_installation.html#installing-ansible-on-specific-operating-systems

1.  Install Ansible as an operating system package on the control system.

    For details, see `Installing Ansible on specific operating systems`_.

2.  Install the **IBM Z HMC collection** from Ansible Galaxy:

    .. code-block:: sh

        $ ansible-galaxy collection install ibm.ibm_zhmc

    This will install the collection to your local Ansible collections tree,
    which is by default ``$HOME/.ansible/collections/ansible_collections``.
    It does not install any dependent Python packages.

3.  Install the dependent Python packages into your system Python:

    Double check where the ``ibm.ibm_zhmc`` collection got installed:

    .. code-block:: sh

        $ ansible-galaxy collection list ibm.ibm_zhmc
        # /Users/johndoe/.ansible/collections/ansible_collections
        Collection   Version
        ------------ -------
        ibm.ibm_zhmc 1.0.0

    Set a variable to the installation directory shown in the command output:

    .. code-block:: sh

        $ anco_dir=/Users/johndoe/.ansible/collections/ansible_collections

    Using the ``requirements.txt`` file in the installation directory of the
    ``ibm.ibm_zhmc`` collection, install dependent Python packages into your
    system Python:

    .. code-block:: sh

        $ sudo pip install -r $anco_dir/ibm/ibm_zhmc/requirements.txt


Alternative installation with virtual Python environment
--------------------------------------------------------

.. _virtualenv: https://virtualenv.pypa.io/

This section describes the installation of Ansible and the ibm.ibm_zhmc Ansible
Galaxy collection into a virtual Python environment that is set
up using `virtualenv`_.

This installation method utilizes the ability of Ansible to configure the
Python environment it uses, and configures it to use the active Python (which
can be a virtual Python environment or the system Python).

All commands shown are to be executed in a terminal or command prompt on the
control system. The instructions are written for a bash shell.

1.  Create a virtual Python environment and activate it:

    .. code-block:: sh

        $ mkvirtualenv myenv

    Note: Using the command shown requires the ``virtualenvwrapper`` package.

    For details, see `virtualenv`_.

2.  Install Ansible as a Python package on the control system:

    .. code-block:: sh

        $ pip install ansible

    This will install Ansible into the active Python, i.e. into the virtual
    Python environment. Note that an OS-level Ansible and a Python-level
    Ansible have shared configuration files, e.g. in ``/etc/ansible``.

3.  Create a shell script that invokes the active Python.

    Adjust the file name and path for the shell script in the ``python_script``
    variable as needed, the only requirement is that the shell script must be
    found in the PATH:

    .. code-block:: sh

        $ python_script=$HOME/local/bin/env_python

        $ cat >$python_script <<'EOT'
        #!/bin/bash
        py=$(which python)
        $py "$@"
        EOT

        $ chmod 755 $python_script

4.  Configure Ansible to invoke Python via the new shell script (using the
    ``python_script`` variable from the previous step):

    .. code-block:: sh

        $ sudo tee -a /etc/ansible/hosts >/dev/null <<EOT
        [local:vars]
        ansible_python_interpreter=$python_script
        EOT

5.  Install the **IBM Z HMC collection** from Ansible Galaxy:

    .. code-block:: sh

        $ ansible-galaxy collection install ibm.ibm_zhmc

    This will install the collection to your local Ansible collections tree,
    which is by default ``$HOME/.ansible/collections/ansible_collections``.
    It does not install any dependent Python packages.

6.  Install the dependent Python packages into the active Python:

    Double check where the ``ibm.ibm_zhmc`` collection got installed:

    .. code-block:: sh

        $ ansible-galaxy collection list ibm.ibm_zhmc
        # /Users/johndoe/.ansible/collections/ansible_collections
        Collection   Version
        ------------ -------
        ibm.ibm_zhmc 1.0.0

    Set a variable to the installation directory shown in the command output:

    .. code-block:: sh

        $ anco_dir=/Users/johndoe/.ansible/collections/ansible_collections

    Using the ``requirements.txt`` file in the installation directory of the
    ``ibm.ibm_zhmc`` collection, install dependent Python packages into your
    active Python:

    .. code-block:: sh

        $ pip install -r $anco_dir/ibm/ibm_zhmc/requirements.txt


.. _`Supported environments`:

Supported environments
----------------------

The following versions of Python are supported:

- Python 2.7
- Python 3.5 and higher

The following operating systems are supported:

- Linux
- macOS (OS-X)
- Windows

The following versions of Ansible are supported:

- Ansible 2.9 (only up to Python 3.8)
- Ansible 2.10 (only up to Python 3.8)
- Ansible 3 (ansible-base 2.10) (only up to Python 3.8)
- Ansible 4 (ansible-core 2.11) (only up to Python 3.9)
- Ansible 5 (ansible-core 2.12)
- Ansible 6 (ansible-core 2.13)
- Ansible 7 (ansible-core 2.14)

The following Z and LinuxONE machine generations are supported:

- z196 / z114
- zEC12 / zBC12
- z13 / z13s / Emperor / Rockhopper
- z14 / Emperor II / Rockhopper II
- z15 / LinuxONE III
- z16 / LinuxONE 4
