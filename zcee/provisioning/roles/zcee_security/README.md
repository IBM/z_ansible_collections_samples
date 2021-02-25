zcee_security
=========

Define z/OS Connect EE security.

Requirements
------------

This role requires the IBM z/OS core collection, ibm.ibm_zos_core.

Role Variables
--------------

| Variable                    | Definition                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    |
| --------------------------- | --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| workflowOwner               | The ID of the owner of the zCEE application.  
| softwareServiceInstanceName | The unique name for the zCEE instance. 
| define_security             | Boolean used to determine if security should be defined.
| zcon_saf_authentication     |Vvalue of 'TRUE' or 'FALSE' specifying if z/OS connect should require authentication.                                                                                                                                                                                                                                                                                                                                                                                                            |
| zcon_tls                    | Value of 'YES' or 'NO' used to specify if z/OS connect should accept SSL connections.                                                                                                                                                                                                                                                                                                                                                                                                           |
| zcon_admin_server           | User ID under which the provisioned instance of z/OS Connect EE runs.                                                                                                                                                                                                                                                                                                                                                                                                                         |
| zcon_zos_guest_user         | User ID to get unauthenticated READ access to profile in APPL class.
| hostname                    | The hostname for the z/OS system.      
| zcon_http_port              | HTTP port for zCEE, may be determined at runtime. 
| zcon_https_port             | HTTPS port for zCEE, may be determined at runtime. 
| zcee_config_path            | The path to the base zCEE configuration directory.                                                                                                                                                                                                                                                                                                                                                                                                                                             |

Dependencies
------------

None

Example Playbook
----------------

```yaml
- include_role:
    name: zcee_security
```

License
-------

Copyright (c) IBM Corporation 2020 Apache License, Version 2.0 (see https://opensource.org/licenses/Apache-2.0)

Author Information
------------------

Dipti Gandhi (ddgandhi@us.ibm.com)
