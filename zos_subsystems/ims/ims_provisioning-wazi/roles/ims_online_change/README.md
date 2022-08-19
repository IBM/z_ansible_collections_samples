Role Name
=========

Enable On-line Change (OLC) utility and copy staging library into active library.


Requirements
------------
* IBM z/OS core collection 1.4.0
* IBM z/OS IMS collection 1.2.0


Role Variables
--------------
| Variable                           | Definition                                                                                                                                                          |
| ---------------------------------- | ------------------------------------------------------------------------------------------------------------------------------------------------------------------- |                                                                                                                       
| enable_olc               | If it's true, OLC will be enabled.                                                                                                                          |                                                                                                                          |
| active_libs               | If it's true, copy staging library into active library                                                                                                                            |


Dependencies
------------

None

Example Playbook
----------------
```yaml
    - include_role:
        name: ims_online_change
      vars:
        enable_olc: true
        active_libs: true

```

## Copyright

© Copyright IBM Corporation 2022

## License
Licensed under
[Apache License, Version 2.0](https://opensource.org/licenses/Apache-2.0).
