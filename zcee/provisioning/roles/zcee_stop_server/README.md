zcee_stop_server
=========

Stop a z/OS Connect EE server.

Requirements
------------

Ansible Collection ibm.ibm_zos_core

Role Variables
--------------

| Variable                    | Definition                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    |
| --------------------------- | --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| zcon_zfs_mountpoint          | The path to the zCEE mount point.                                                                                                                                                                                                                                                                                                                                                                                                                              |
| softwareServiceInstanceName | The unique name for the zCEEE instance.                                                                                                                                                                                                                                                                                                                                                                                                                                                         |                                                                                                                                                                                                                                                                                                                                                                                                                                   


Dependencies
------------

None.

Example Playbook
----------------

```yaml
- include_role:
    name: zcee_stop_server
  vars:
    server_name: '{{ softwareServiceInstanceName }}'
```

License
-------

Copyright (c) IBM Corporation 2020 Apache License, Version 2.0 (see https://opensource.org/licenses/Apache-2.0)

Author Information
------------------

Dipti Gandhi (ddgandhi@us.ibm.com)
