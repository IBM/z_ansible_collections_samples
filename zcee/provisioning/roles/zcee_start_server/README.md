zcee_start_server
=========

Start a z/OS Connect EE server.

Requirements
------------

Ansible Collection ibm.ibm_zos_core

Role Variables
--------------

| Variable                    | Definition                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    |
| --------------------------- | --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| zcon_install_dir            | The path to the z/OS Connect EE installation directory on zFS.                                                                                                                                                                                                                                                                                                                                                                                                                                |
| softwareServiceInstanceName | The unique name for the instance of zCEE.                                                                                                                                                                                                                                                                                                                                                                                                                                                         |
| zcon_zos_proclib            | The PROCLIB holding zCEE related procedures.                                                                                                                                                                                                                                                                                                                                                                                                           |
| user_private_proclib        | The user private proclib to hold the proc.                                                                                                                                                                                                                                                                                                                                                                                                                                       |


Dependencies
------------

zCEE server procedure must be present on the control node or on the user's private proclib.

Example Playbook
----------------

```yaml
- include_role:
    name: zcee_start_server
  vars:
    server_name: '{{ softwareServiceInstanceName }}'
```

License
-------

Copyright (c) IBM Corporation 2020 Apache License, Version 2.0 (see https://opensource.org/licenses/Apache-2.0)

Author Information
------------------

Dipti Gandhi (ddgandhi@us.ibm.com)
