zcee_configure_artifacts
=========

Configure zCEE artifacts.

Requirements
------------

This role requires the IBM z/OS core collection, ibm.ibm_zos_core from the Red Hat® Ansible Certified Content for IBM Z.

Role Variables
--------------

| Variable                    | Definition                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    |
| --------------------------- | --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| zcee_config_path            | The path to the base zCEE configuration directory.                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                         |
| zcon_zfs_mountpoint         | The path to the zCEE mount point.                                                                                                                                                                                                                                                                                                                                                                                                                                                              |
| softwareServiceInstanceName | The unique name for the zCEE instance.                                                                                                                                                                                                                                                                                                                                                                                                                                                      |
| files_to_copy               | A list of files on the control node that should be copied to the z/OS host. For more information, see [**Configuration Artifacts**]( https://github.ibm.com/IBMZSoftware/ansible-zcee/blob/dev/roles/zcee_configure_artifacts/defaults/main.yml)                                                                                                                                                                                                                                                                                                                                                                                                                                     |
| target_charset              | Specifies the character set any copied templates and files should be converted to.                                                                                                                                                                                                                                                                                                                                                                                                        |


Dependencies
------------

None.

Example Playbook
----------------

```yaml
- include_role:
    name: zcee_configure_artifacts
```

License
-------

Copyright (c) IBM Corporation 2020 Apache License, Version 2.0 (see https://opensource.org/licenses/Apache-2.0)


Author Information
------------------

Dipti Gandhi (ddgandhi@us.ibm.com)
