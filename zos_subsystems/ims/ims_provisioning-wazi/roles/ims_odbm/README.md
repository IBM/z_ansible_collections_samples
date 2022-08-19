Role Name
=========

Start or stop Object Database Manager (ODBM)

Requirements
------------
* IBM z/OS core collection 1.4.0
* IBM z/OS IMS collection 1.2.0

Role Variables
--------------
| Variable                           | Definition                                                                                                                                                          |
| ---------------------------------- | ------------------------------------------------------------------------------------------------------------------------------------------------------------------- |                                                                                                                       
| odbm               | If odbm = "start": start ODBM. Or odbm = "stop": stop ODBM                                                                                                                     |                                                                                                                          |

Dependencies
------------

None

Example Playbook
----------------
```yaml
    - include_role:
        name: ims_odbm
      vars:
        odbm: start

```

## Copyright

© Copyright IBM Corporation 2022

## License
Licensed under
[Apache License, Version 2.0](https://opensource.org/licenses/Apache-2.0).
