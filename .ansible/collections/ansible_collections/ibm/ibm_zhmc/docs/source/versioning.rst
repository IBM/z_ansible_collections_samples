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


.. _`Versioning`:

Versioning
==========

This collection uses the rules of `Semantic Versioning 2.0.0`_ for its version.

.. _Semantic Versioning 2.0.0: https://semver.org/spec/v2.0.0.html

This documentation may have been built from a development level of the
package. You can recognize a development version of this package by the
presence of a ".devD" suffix in the version string.


.. _`Compatibility`:

Compatibility
-------------

For Ansible modules, compatibility is always seen from the perspective of an
Ansible playbook using it. Thus, a backwards compatible new version of an
Ansible Galaxy collection means that the user can safely upgrade to that new
version without encountering compatibility issues in any Ansible playbooks
using the modules in the collection.

This collection uses the rules of `Semantic Versioning 2.0.0`_ for compatibility
between package versions, and for :ref:`deprecations <Deprecations>`.

The public interface of the collection that is subject to the semantic
versioning rules (and specificically to its compatibility rules) are the Ansible
module interfaces described in this documentation.

Violations of these compatibility rules are described in section
:ref:`Releases`.


.. _`Deprecations`:

Deprecations
------------

Deprecated functionality is marked accordingly in this documentation and in the
:ref:`Releases` section.
