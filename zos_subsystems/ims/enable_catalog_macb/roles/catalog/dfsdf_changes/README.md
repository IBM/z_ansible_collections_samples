dfsdf_changes
=========

Creates a backup of the `DFSDFxxx` member and makes necessary changes to `<SECTION=CATALOG>`. 

The playbook contains two sections: changes **before** running the populate utility and changes **after** running the populate utility. 
- Before running the populate utility, if the CATALOG section does not exist in the `DFSDFxxx` member, it will be created. The `CATALOG` attribute is set to `N`. 
- After running the populate utitity, the `CATALOG` attribute is changed to `Y` to enable the IMS catalog.  

Requirements
------------

[IBM&reg; z/OS&reg; core collection 1.4.0 or later](https://galaxy.ansible.com/ibm/ibm_zos_core)

[IBM&reg; z/OS&reg; IMS collection 1.2.0](https://galaxy.ansible.com/ibm/ibm_zos_ims)

Role Variables
--------------
- [`BACKUP`](host_vars/zos_host.yml) - Boolean value that determines if a backup copy of the `DFSDFxxx` member will be created

- [`DFSDF_BACKUP_NAME`](host_vars/zos_host.yml)- Name of the backup member

- `catalog`
  - when `catalog` = before, it adds `<SECTION=CATALOG>` to the DFSDFxxx member. 
  - when `catalog` = after, it changes the `CATALOG` attribute to `Y`. 

Dependencies
------------

None

Example Playbook
----------------
```yaml
    - include_role:
        name: roles/catalog/dfsdf_changes
      vars: 
        catalog: before

```
Copyright
---------

© Copyright IBM Corporation 2023

License
-------

Licensed under [Apache License, Version 2.0](https://opensource.org/licenses/Apache-2.0)

Author Information
------------------

Stephanie Lieu - @steph-lieu

Support
-------

Refer to the [support section](https://github.com/IBM/z_ansible_collections_samples/blob/master/README.md#support) for more details.
