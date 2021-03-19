zcee_create_format_zfs
=========

Create and format a z/OS file system (zFS).

Requirements
------------

Ansible Collection ibm.ibm_zos_core

Role Variables
--------------

| Variable                    | Definition                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    |
| --------------------------- | --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| softwareServiceInstanceName      | The unique name for the zCEE instance.                                                                                                                                                                                                                                                                                                              |
| zcon_install_dir                 |   The path to the z/OS Connect EE installation directory on zFS.                                                                                                                                                                                                                                                                                                                                                                                                                                            |
| zcon_zfs_mountpoint              | The path to the zCEE mount point.                                                                                                                                                                                                                                                                                                                                                                                                    |
| zcon_file_system_hlq             | Specifies the high-level qualifier for the zFS data sets that are defined during the provisioning of the z/OS Connect EE instance.                                                                                                                                                                                                                                                                                                                                                                                                                                                               |
| zcon_zos_vsam_volume             |VOLUME value that is used in the creation of the zFS data sets; for example, SYSDA. If the value is specified as SMS (Storage Management Subsystem), the VOLUME parameter is omitted from the VSAM creation.
| zcee_config_path                 | The path to the base zCEE configuration directory                                                                                                                                                                                                                                                                                                                                                                                                                                             |                                                                                                                                                                                                                                                                                                                                                    
| zcon_zfs_dataclass               |  A data class that is used to define a zFS data set for the z/OS Connect EE instance.                                                                                                                                                                                                                                                                                                                                                                                                         |                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              


Dependencies
------------

None

Example Playbook
----------------

```yaml
- include_role:
    name: create-format-zfs
```

License
-------

Copyright (c) IBM Corporation 2020 Apache License, Version 2.0 (see https://opensource.org/licenses/Apache-2.0)

Author Information
------------------

Dipti Gandhi (ddgandhi@us.ibm.com)
