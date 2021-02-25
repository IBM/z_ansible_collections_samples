zcee_zcon_setup
=========

Run zcon setup.

Requirements
------------

None

Role Variables
--------------

| Variable                    | Definition                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    |
| --------------------------- | --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| softwareServiceInstanceName | The unique name for the zCEE instance.                                                                                                                                                                                                                                                                                                                            |
| zcon_install_dir            | The path to the z/OS Connect EE installation directory on zFS.                                                                                                                                                                                                                                                                                                                                                                                                                                |


Dependencies
------------

None

Example Playbook
----------------

```yaml
- include_role:
    name: zcee_zcon_setup
```

License
-------

Copyright (c) IBM Corporation 2020 Apache License, Version 2.0 (see https://opensource.org/licenses/Apache-2.0)

Author Information
------------------

Dipti Gandhi (ddgandhi@us.ibm.com)
