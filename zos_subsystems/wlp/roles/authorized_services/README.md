authorized_services
=========

Enable z/OS&reg; authorized services. The enabled z/OS&reg; authorized services can be customized based on the boolean value of the variables in the [role defaults file](./defaults).

Requirements
------------

[IBM&reg; z/OS&reg; core collection 1.3.0 or later](https://galaxy.ansible.com/ibm/ibm_zos_core)

Role Variables
--------------

| Variable      | Definition                             | Set                                              |
| ------------- | ---------------------------------------|--------------------------------------------------|
| TARGET_USER   | User id of the angel process | `host_vars/zos_host.yml` |
| USER_GROUP    | Group of the target user | `host_vars/zos_host.yml` |
| SAFCRED       | Boolean value for enabling SAF authorized user registry services and SAF authorization services | [defaults/main.yml](defaults/main.yml)|
| ZOSWLM        | Boolean value for enabling WLM services | [defaults/main.yml](defaults/main.yml)|
| TXRSS         | Boolean value for enabling  RRS Transaction Services | [defaults/main.yml](defaults/main.yml)|
| ZOSDUMP       | Boolean value for enabling z/OS&reg; dumps | [defaults/main.yml](defaults/main.yml)|
| LOCAL_ADAPTER | Boolean value for enabling optimized local adapter services | [defaults/main.yml](defaults/main.yml)|
| PRODMGR       | Boolean value for enabling IFAUSAGE services | [defaults/main.yml](defaults/main.yml)|
| ZOSAIO        | Boolean value for enabling AsyncIO services | [defaults/main.yml](defaults/main.yml)|

Dependencies
------------

None

Example Playbook
----------------
```yaml
    - include_role:
        name: authorized_services
```

Copyright
---------

© Copyright IBM Corporation 2021

License
-------

Licensed under [Apache License, Version 2.0](https://opensource.org/licenses/Apache-2.0)

Author Information
------------------

Stephanie Lieu - @stephanie-lieu or @steph-lieu

Support
-------

Please refer to the [support section](https://github.com/IBM/z_ansible_collections_samples/blob/master/README.md#support) for more details.
