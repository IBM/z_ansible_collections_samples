allocate_datasets
=========

Allocate log data sets for managed ACBs by using the `zos_data_set` module in the `ibm_zos_core` Ansible collection.

Requirements
------------

[IBM&reg; z/OS&reg; core collection 1.4.0 or later](https://galaxy.ansible.com/ibm/ibm_zos_core)

[IBM&reg; z/OS&reg; IMS collection 1.2.0](https://galaxy.ansible.com/ibm/ibm_zos_ims)

Role Variables
--------------

None

Dependencies
------------

None

Example Playbook
----------------
```yaml
    - include_role:
        name: roles/mACB/allocate_datasets
```
Copyright
---------

© Copyright IBM Corporation 2023

License
-------

Licensed under [Apache License, Version 2.0](https://opensource.org/licenses/Apache-2.0)

Author Information
------------------

Stephanie Lieu - @Stephanie-Lieu or @steph-lieu

Support
-------

Refer to the [support section](https://github.com/IBM/z_ansible_collections_samples/blob/master/README.md#support) for more details.
