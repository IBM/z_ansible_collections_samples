Send templates
=========

This role is a utility used by other roles to send entire directory of template files to z/OS system.

Requirements
------------

* None

Role Variables
--------------

- ### ** path **
   The path of the directory to send.


Dependencies
------------

None

Example Playbook
----------------

```yaml
  - include_role: 
      name: send-templates
        path: <local directory containing files to send>

```

## Copyright

© Copyright IBM Corporation 2020

License
-------

Copyright (c) IBM Corporation 2020 Apache License, Version 2.0 (see https://opensource.org/licenses/Apache-2.0)
