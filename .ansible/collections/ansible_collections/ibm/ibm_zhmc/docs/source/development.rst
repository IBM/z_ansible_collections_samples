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


.. _`Development`:

Development
===========

This section only needs to be read by developers of this project. People that
want to make a fix or develop some extension, and people that want to test the
project are also considered developers for the purpose of this section.


.. _`Repository`:

Repository
----------

The repository for the **IBM Z HMC collection** is on GitHub:

https://github.com/zhmcclient/zhmc-ansible-modules


.. _`Setting up the development environment`:

Setting up the development environment
--------------------------------------

The development environment is pretty easy to set up.

Besides having a supported operating system with a supported Python version
(see :ref:`Supported environments`), it is recommended that you set up a
`virtual Python environment`_.

.. _virtual Python environment: https://docs.python-guide.org/dev/virtualenvs/

Then, with a virtual Python environment active, clone the Git repo of this
project and prepare the development environment with ``make develop``:

.. code-block:: sh

    $ git clone git@github.com:zhmcclient/zhmc-ansible-modules.git
    $ cd zhmc-ansible-modules
    $ make develop

This will install all prerequisites the project needs for its development.

Generally, this project uses Make to do things in the currently active
Python environment. The command ``make help`` (or just ``make``) displays a
list of valid Make targets and a short description of what each target does.


.. _`Building the documentation`:

Building the documentation
--------------------------

The documentation for the **IBM Z HMC collection** is published
on GitHub Pages at https://zhmcclient.github.io/zhmc-ansible-modules/.

That web site represents a defined set of versions of this collection and
automatically gets updated whenever a pull request gets merged into the
repository branch that corresponds to the version. The automatic update
mechanism is implemented in the GitHub Actions workflow file
``.github/workflows/docs.yml``.

The versions to be represented on that site are defined in ``docs/source/conf.py``
in the section for "sphinx-versioning".

In order to build this "versioned" documentation locally, issue:

.. code-block:: sh

    $ make docs

The top-level document to open with a web browser will be
``docs_build/index.html``. Note that the versioned documentation is built from
the defined branches, so it does not include the content of your Git work
directory.

In order to see the effects of some change in your Git work directory, there
is a second documentation build that builds an "unversioned" documentation
from the content of your Git work directory:

.. code-block:: sh

    $ make docslocal

The top-level document to open with a web browser will be
``docs_local/index.html``; it is opened automatically when the documentation
has been built successfully.


.. _`Testing`:

Testing
-------

Again, an invocation of Make runs against the currently active Python environment.

There are four kinds of tests currently, available as make targets:

* ``make linkcheck`` - Check links in documentation
* ``make test`` - Run unit and function tests with test coverage
* ``make sanity`` - Run Ansible sanity tests (includes flake8, pylint, validate-modules)
* ``make end2end`` - Run end2end tests (against a real environment)

For the unit and function tests, the testcases and options for pytest
can be specified via the environment variable ``TESTOPTS``, as shown in these
examples:

.. code-block:: sh

    $ make test                                      # Run all unit and function tests
    $ TESTOPTS='-vv' make test                       # Specify -vv verbosity for pytest
    $ TESTOPTS='-k test_partition.py' make test      # Run only this test source file

The automated tests performed by Github Actions run on a standard set of test
environments when a PR is created, and on the full set of test environments when
a release is prepared and in addition on a weekly basis. See the
``.github/workflows/test.yml`` file for details.

These automated tests use all Ansible versions that are supported, but not in
all combinations with all Python versions. See the
``requirements.txt`` and ``minimum-constraint.txt`` files for details.

The following table shows which Ansible versions are tested on which Python
versions. In addition, this project tests on a well-defined set of minimum
versions of Python packages, and a set of latest versions of Python packages
for each Python version.

======  ========  =======  ============
Python  Packages  ansible  ansible-core
------  --------  -------  ------------
2.7     minimum   2.9      2.9
3.5     minimum   2.9      2.9
3.6     minimum   2.9      2.9
3.7     minimum   2.9      2.9
3.8     minimum   2.9      2.9
3.9     minimum   4.0      2.11
3.10    minimum   5.0      2.12
3.11    minimum   7.0      2.14
2.7     latest    2.9      2.9
3.5     latest    2.9      2.9
3.6     latest    2.10     2.10
3.7     latest    4.x      2.11
3.8     latest    5.x      2.12
3.9     latest    6.x      2.13
3.10    latest    7.x      2.14
3.11    latest    7.x      2.14
======  ========  =======  ============


.. _`Releasing a version`:

Releasing a version
-------------------

This section shows the steps for releasing a version to `Ansible Galaxy
<https://galaxy.ansible.com/>`_.

It covers all variants of versions that can be released:

* Releasing a new major version (Mnew.0.0) based on the master branch
* Releasing a new minor version (M.Nnew.0) based on the master branch
* Releasing a new update version (M.N.Unew) based on the stable branch of its
  minor version

This description assumes that you are authorized to push to the remote repo
at https://github.com/zhmcclient/zhmc-ansible-modules and that the remote repo
has the remote name ``origin`` in your local clone.

Any commands in the following steps are executed in the main directory of your
local clone of the zhmc-ansible-modules Git repo.

1.  Set shell variables for the version that is being released and the branch
    it is based on:

    * ``MNU`` - Full version M.N.U that is being released
    * ``MN`` - Major and minor version M.N of that full version
    * ``BRANCH`` - Name of the branch the version that is being released is
      based on

    When releasing a new major version (e.g. ``1.0.0``) based on the master
    branch:

    .. code-block:: sh

        MNU=1.0.0
        MN=1.0
        BRANCH=master

    When releasing a new minor version (e.g. ``0.9.0``) based on the master
    branch:

    .. code-block:: sh

        MNU=0.9.0
        MN=0.9
        BRANCH=master

    When releasing a new update version (e.g. ``0.8.1``) based on the stable
    branch of its minor version:

    .. code-block:: sh

        MNU=0.8.1
        MN=0.8
        BRANCH=stable_${MN}

2.  Create a topic branch for the version that is being released:

    .. code-block:: sh

        git checkout ${BRANCH}
        git pull
        git checkout -b release_${MNU}

3.  Edit the Galaxy metadata file:

    .. code-block:: sh

        vi galaxy.yml

    and set the 'version' parameter to the version that is being released:

    .. code-block:: yaml

        version: M.N.U

4.  Edit the change log:

    .. code-block:: sh

        vi docs/source/release_notes.rst

    and make the following changes in the section of the version that is being
    released:

    * Finalize the version.
    * Change the release date to today's date.
    * Make sure that all changes are described.
    * Make sure the items shown in the change log are relevant for and
      understandable by users.
    * In the "Known issues" list item, remove the link to the issue tracker and
      add text for any known issues you want users to know about.
    * Remove all empty list items.

5.  Commit your changes and push the topic branch to the remote repo:

    .. code-block:: sh

        git commit -asm "Release ${MNU}"
        git push --set-upstream origin release_${MNU}

6.  On GitHub, create a Pull Request for branch ``release_M.N.U``.

    Important: When creating Pull Requests, GitHub by default targets the
    ``master`` branch. When releasing based on a stable branch, you need to
    change the target branch of the Pull Request to ``stable_M.N``.

    The PR creation will cause the "test" workflow to run. That workflow runs
    tests for all defined environments, since it discovers by the branch name
    that this is a PR for a release.

7.  On GitHub, once the checks for that Pull Request have succeeded, merge the
    Pull Request (no review is needed). This automatically deletes the branch
    on GitHub.

    If the PR did not succeed, fix the issues.

8.  On GitHub, close milestone ``M.N.U``.

    Verify that the milestone has no open items anymore. If it does have open
    items, investigate why and fix.

9.  Publish the collection to Ansible Galaxy

    .. code-block:: sh

        git checkout ${BRANCH}
        git pull
        git branch -D release_${MNU}
        git branch -D -r origin/release_${MNU}
        git tag -f ${MNU}
        git push -f --tags

    Pushing the new tag will cause the "publish" workflow to run. That workflow
    builds the collection, publishes it on Ansible Galaxy, creates a release for
    it on Github, and finally creates a new stable branch on Github if the master
    branch was released.

10. Verify the publishing

    * Verify that the new version is available on Ansible Galaxy at
      https://galaxy.ansible.com/ibm/ibm_zhmc/

      If the new version is not shown there, verify that the import on Ansible
      Galaxy succeeded, by checking the status at
      https://galaxy.ansible.com/my-imports (you need to log in).

    * Verify that the new version has a release on Github at
      https://github.com/zhmcclient/python-zhmcclient/releases

    * Verify that the new version has documentation on Github pages at
      https://zhmcclient.github.io/zhmc-ansible-modules/release_notes.html

11. Publish the collection to Ansible AutomationHub

    This needs to be done in addition to the prior publish step, and it
    has not successfully been automated as of today.

    You need to have an account on https://console.redhat.com, and your
    userid there needs to be authorized to modify the 'ibm' namespace.

    * Open https://console.redhat.com/ansible/automation-hub/repo/published/ibm
      and log in to your account.

    * Click on the "Upload Collection" button at the top right of the page,
      and in the file selection dialog that pops up, select the distribution
      archive for the version you want to upload. That archive has been built
      during the `make upload` in the previous step, and its file name is:

      .. code-block:: text

          dist/ibm-ibm_zhmc-{M}.{N}.{U}.tar.gz

    **Attention!!** This only works once for each version. You cannot
    re-release the same version more than once.

    Verify that the import on Ansible AutomationHub succeeded, by checking the
    status at
    https://console.redhat.com/ansible/automation-hub/my-imports?namespace=ibm
    (you need to log in).

    After the import succeeded, the release must still be approved by RedHat
    before it is published, so the approval status should now show
    "waiting for approval".

    The RedHat team should approve the release within a day or so. Once it has
    been approved, the new version will be visible on Ansible AutomationHub at
    https://console.redhat.com/ansible/automation-hub/repo/published/ibm/ibm_zhmc .


.. _`Starting a new version`:

Starting a new version
----------------------

This section shows the steps for starting development of a new version.

These steps may be performed right after the steps for
:ref:`releasing a version`, or independently.

This section covers all variants of new versions:

* Starting a new major version (Mnew.0.0) based on the master branch
* Starting a new minor version (M.Nnew.0) based on the master branch
* Starting a new update version (M.N.Unew) based on the stable branch of its
  minor version

This description assumes that you are authorized to push to the remote repo
at https://github.com/zhmcclient/zhmc-ansible-modules and that the remote repo
has the remote name ``origin`` in your local clone.

Any commands in the following steps are executed in the main directory of your
local clone of the zhmc-ansible-modules Git repo.

1.  Set shell variables for the version that is being started and the branch it
    is based on:

    * ``MNU`` - Full version M.N.U that is being started
    * ``MN`` - Major and minor version M.N of that full version
    * ``BRANCH`` -  Name of the branch the version that is being started is
      based on

    When starting a new major version (e.g. ``1.0.0``) based on the master
    branch:

    .. code-block:: sh

        MNU=1.0.0
        MN=1.0
        BRANCH=master

    When starting a new minor version (e.g. ``0.9.0``) based on the master
    branch:

    .. code-block:: sh

        MNU=0.9.0
        MN=0.9
        BRANCH=master

    When starting a new minor version (e.g. ``0.8.1``) based on the stable
    branch of its minor version:

    .. code-block:: sh

        MNU=0.8.1
        MN=0.8
        BRANCH=stable_${MN}

2.  Create a topic branch for the version that is being started:

    .. code-block:: sh

        git checkout ${BRANCH}
        git pull
        git checkout -b start_${MNU}

3.  Edit the change log:

    .. code-block:: sh

        vi docs/source/release_notes.rst

    and insert the following section before the top-most section, and update
    the version to a draft version of the version that is being started:

    .. code-block:: text

        Version M.N.U-dev1
        ------------------

        This version contains all fixes up to version M.N-1.x.

        Released: not yet

        Availability: `AutomationHub`_, `Galaxy`_, `GitHub`_

        **Incompatible changes:**

        **Deprecations:**

        **Bug fixes:**

        **Enhancements:**

        **Cleanup:**

        **Known issues:**

        * See `list of open issues`_.

        .. _`list of open issues`: https://github.com/zhmcclient/zhmc-ansible-modules/issues

4.  Edit the Galaxy metadata file:

    .. code-block:: sh

        vi galaxy.yml

    and update the version to a draft version of the version that is being
    started:

    .. code-block:: yaml

        version: M.N.U-dev1

    Note: The version must follow the rules for semantic versioning 2.0
    including the description of development/alpha/etc suffixes, as described
    in https://semver.org/

5.  Commit your changes and push them to the remote repo:

    .. code-block:: sh

        git status  # Double check the changed files
        git commit -asm "Start ${MNU}"
        git push --set-upstream origin start_${MNU}

6.  On GitHub, create a Pull Request for branch ``start_M.N.U``.

    Important: When creating Pull Requests, GitHub by default targets the
    ``master`` branch. When starting a version based on a stable branch, you
    need to change the target branch of the Pull Request to ``stable_M.N``.

7.  On GitHub, create a milestone for the new version ``M.N.U``.

    You can create a milestone in GitHub via Issues -> Milestones -> New
    Milestone.

8.  On GitHub, go through all open issues and pull requests that still have
    milestones for previous releases set, and either set them to the new
    milestone, or to have no milestone.

9.  On GitHub, once the checks for the Pull Request for branch ``start_M.N.U``
    have succeeded, merge the Pull Request (no review is needed). This
    automatically deletes the branch on GitHub.

10. Update and clean up the local repo:

    .. code-block:: sh

        git checkout ${BRANCH}
        git pull
        git branch -d start_${MNU}
