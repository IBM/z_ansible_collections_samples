create_zFS
=========

Create and format a z/OS&reg; file system (zFS).

Requirements
------------

[IBM&reg; z/OS&reg; core collection 1.3.0 or later](https://galaxy.ansible.com/ibm/ibm_zos_core)

Role Variables
--------------

| Variable      | Definition                             | Set                                              |
| ------------- | ---------------------------------------|--------------------------------------------------|
| data_set_name | Name of the data set to be mounted | `host_vars/zos_host.yml`                         |


Dependencies
------------

None

Example Playbook
----------------
```yaml
    - include_role:
        name: create_zFS

```
Copyright
---------

© Copyright IBM Corporation 2021

License
-------

Licensed under [Apache License, Version 2.0](https://opensource.org/licenses/Apache-2.0)

Author Information
------------------

Stephanie Lieu - @Stephanie-Lieu or @steph-lieu

Support
-------

Please refer to the [support section](https://github.com/IBM/z_ansible_collections_samples/blob/master/README.md#support) for more details.
