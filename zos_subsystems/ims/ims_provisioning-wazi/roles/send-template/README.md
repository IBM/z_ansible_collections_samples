send-template
=========

Send a template file to USS.

Requirements
------------

None

Role Variables
--------------

* path: contains the full path to the template file to send.

Dependencies
------------

None.

Example Playbook
----------------

```yaml
        - include_role:
            name: send-template
          vars:
            path: '/home/template.j2'
```

## Copyright

© Copyright IBM Corporation 2022

## License
Licensed under
[Apache License, Version 2.0](https://opensource.org/licenses/Apache-2.0).

