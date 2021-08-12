security
=========

Define STARTED profiles for angel process and enable z/OS authorized services. The enabled z/OS authorized services can be customized based on boolean value variables in the role defaults file.

Requirements
------------

Ansible Collection: ibm.ibm_zos_core

Role Variables
--------------

| Variable      | Definition                             | Set                                              |
| ------------- | ---------------------------------------|--------------------------------------------------|
| PROC_LIB      | The name of procedure library | `host_vars/zos_host.yml`                         |
| SERVER_PROC   | The unique name for the STARTED command to run the server | `host_vars/zos_host.yml`        |
| ANGEL_PROC    | The unique name for the STARTED command to run the angel process | `host_vars/zos_host.yml` | 
| TARGET_USER   | USERID for the angel process | `host_vars/zos_host.yml` | 
| USER_GROUP    | Group for TARGET_USER | `host_vars/zos_host.yml` | 
| SAFCRED       | Boolean value for enabling SAF authorized user registry services and SAF authorization services | [defaults/main.yml](defaults/main.yml)| 
| ZOSWLM        | Boolean value for enabling WLM services | [defaults/main.yml](defaults/main.yml)| 
| TXRSS         | Boolean value for enabling  RRS Transaction Services | [defaults/main.yml](defaults/main.yml)| 
| ZOSDUMP       | Boolean value for enabling ZOSDUMP | [defaults/main.yml](defaults/main.yml)| 
| LOCAL_ADAPTER | Boolean value for enabling optimized local adapter services | [defaults/main.yml](defaults/main.yml)| 
| PRODMGR       | Boolean value for enabling the IFAUSAGE services | [defaults/main.yml](defaults/main.yml)| 
| ZOSAIO        | Boolean value for enabling AsyncIO services | [defaults/main.yml](defaults/main.yml)| 

Dependencies
------------

None 

Example Playbook
----------------
```yaml
    - include_role:
        name: security
```

Copyright
---------

© Copyright IBM Corporation 2021

License
-------

Licensed under Apache License, Version 2.0

Author Information
------------------

Stephanie Lieu - @stephanie-lieu or @steph-lieu

Support
-------

Please refer to the [support section](https://github.com/IBM/z_ansible_collections_samples/blob/master/README.md#support) for more details.
