zcee_create_server
=========

Create a z/OS Connect EE server based on the z/OS Connect EE template. 

Requirements
------------

None.

Role Variables
--------------

| Variable                    | Definition                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    |
| --------------------------- | --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| zcon_install_dir            | The path to the z/OS Connect EE installation directory on zFS.                                                                                                                                                                                                                                                                                                                                                                                                                       |
| zcon_zfs_mountpoint         | The path to the zCEE mount point.                                                                                                                                                                                                                                                                                                                                                                                                                                                        |
| softwareServiceInstanceName | The unique name for the instance of zCEE.                                                                                                                                                                                                                                                                                                                                                            |
| zcee_config_path            | The path to the base zCEE configuration directory.                                                                                                                                                                                                                                                                                                                                                                                                                                                 |

Dependencies
------------

None.

Example Playbook
----------------

```yaml
- include_role:
    name: zcee_create_server
    tasks_from: default 
- include_role:
    name: zcee_create_server
    tasks_from: ims_sp
- include_role:
    name: zcee_create_server
    tasks_from: cics_sp
- include_role:
    name: zcee_create_server
    tasks_from: mq_sp    
```

License
-------

Copyright (c) IBM Corporation 2020 Apache License, Version 2.0 (see https://opensource.org/licenses/Apache-2.0)

Author Information
------------------

Dipti Gandhi (ddgandhi@us.ibm.com)
