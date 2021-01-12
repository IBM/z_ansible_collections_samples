zcee_mount_zfs
=========

Mount a z/OS file system (zFS).

Requirements
------------

create-format-zfs role. A zFS data set must be created and formatted before it can be mounted.

Role Variables
--------------

| Variable                    | Definition                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    |
| --------------------------- | --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| softwareServiceInstanceName | The unique name for the instance of zCEE.                                                                                                                                                                                                                                                                                                                                                                                                        |
| zcon_install_dir            | The path to the z/OS Connect EE installation directory on zFS.                                                                                                                                                                                                                                                                                                                                                                                                          |
| zcon_zfs_mountpoint         | The path to the zCEE mount point.                                                                                                                                                                                                                                                                                                                                         |
| ZCON_FILE_SYSTEM_HLQ        | Specifies the high-level qualifier for the zFS data sets that are defined during the provisioning of the z/OS Connect EE instance.                                                                                                                                                                                                                                                                                                                                                                                                                                
| zcee_config_path            | The path to the base zCEE configuration directory.                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                         

Dependencies
------------

None

Example Playbook
----------------

```yaml
- include_role:
    name: zcee_mount_zfs
```

License
-------

Copyright (c) IBM Corporation 2020 Apache License, Version 2.0 (see https://opensource.org/licenses/Apache-2.0)

Author Information
------------------

Dipti Gandhi (ddgandhi@us.ibm.com)
