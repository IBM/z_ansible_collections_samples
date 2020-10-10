.. ...........................................................................
.. © Copyright IBM Corporation 2020                                          .
.. ...........................................................................

============
Contributing
============

We are not currently accepting community contributions. However, we encourage
you to open `git issues`_ for bugs, comments or feature requests.

Review this content periodically to learn when and how to make
contributions in the future. For the latest information on open issues, see:
`git issues`_.

.. _git issues:
   https://github.com/ansible-collections/ibm_zos_core/issues


Development
===========

z/OS Ansible Module Testing
---------------------------

This section outlines the processes to develop and run test cases for z/OS
Ansible modules.

.. toctree::
   :maxdepth: 1
   :glob:

   community_guides_docs/zos_ansible_module_testing

Parsing with BetterArgParser
----------------------------

BetterArgParser goes beyond typical option value parsing and serves as an
alternative to parsers such as argparse. It has been designed to validate
values often used on z/OS to avoid unnecessary failures on the target
such as incorrectly providing a data set name or type. For cases that require
further validation, BetterArgParser accepts a BetterArg object that allows for
further customization.

It is recommended to use BetterArgParser in conjunction with Ansible's
module argument parser.

This section outlines the features of BetterArgParser, explains how to define arguments,
provides examples of dependencies and more:

.. toctree::
   :maxdepth: 1
   :glob:

   community_guides_docs/better_arg_parser
