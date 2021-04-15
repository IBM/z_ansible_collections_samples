Send a template file
=========

This role is a utility used by other roles to send a template file to z/OS system.

Requirements
------------

* None

Role Variables
--------------

- ### ** path **
   The path of the file to be sent.

Dependencies
------------

None

Example Playbook
----------------

```yaml
  - include_role: 
      name: send-template
        path: <local-file-to-send>

```

## Copyright

© Copyright IBM Corporation 2020

License
-------

Copyright (c) IBM Corporation 2020 Apache License, Version 2.0 (see https://opensource.org/licenses/Apache-2.0)
