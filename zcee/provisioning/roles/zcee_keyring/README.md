zcee_keyring
=========

Define a z/OS Connect EE keyring.

Requirements
------------

Ansible Collection ibm.ibm_zos_core

Role Variables
--------------

| Variable                    | Definition                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    |
| --------------------------- | --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| softwareServiceInstanceName | The unique name for the instance of zCEE.  
| zcon_admin_server           | User ID under which the provisioned instance of z/OS Connect EE runs.                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                       |                                                                                                                                                                                                                                                                                           
| define_keyring              | Boolean used to determine if the keyring should be defined.                                                                                                                                                                                                                                                                                                                                                                                                                                        | 
| zcee_config_path            | The path to the base zCEE configuration directory.


Dependencies
------------

None

Example Playbook
----------------

```yaml
- include_role:
    name: zcee_keyring
```

License
-------

Copyright (c) IBM Corporation 2020 Apache License, Version 2.0 (see https://opensource.org/licenses/Apache-2.0)

Author Information
------------------

Dipti Gandhi (ddgandhi@us.ibm.com)
