dfsdf_changes
=========

Creates a backup of the `DFSDFxxx` member and makes necessary changes to `<SECTION=CATALOG>`. The playbook contains two sections: changes **before** running the populate utility and changes **after** running the populate utility. 

Before running the populate utility:
  - Since IMS catalog is a pre-requisite for managed ACBs, this playbook checks for the resources in the following modules that are necessary for IMS catalog to run:
    - `DFSCD000`
    - `DFSCP000` 
    - `DFSCP001`
    - `DFSCX000`
    - `DFSCPL00`
  - This playbook also ensures the `ACBMGMT` variable is set to `ACBLIB`.

After running the populate utility:
  - This playbook changes the `ACBMGMT` variable to `CATALOG` to enable managed ACBs. 

Requirements
------------

[IBM&reg; z/OS&reg; core collection 1.4.0 or later](https://galaxy.ansible.com/ibm/ibm_zos_core)

[IBM&reg; z/OS&reg; IMS collection 1.2.0](https://galaxy.ansible.com/ibm/ibm_zos_ims)

Role Variables
--------------
- [`BACKUP`](host_vars/zos_host.yml) - Boolean value that determines if a backup copy of the `DFSDFxxx` member will be created

- [`ACB_DFSDF_BACKUP_NAME`](host_vars/zos_host.yml)- Name of the backup member

- `macb`
  - when `macb` = before, it checks for necessary resources for IMS catalog to be present and check for the `ACBMGMT` attribute in `<SECTION=CATALOG>`. 
  - when `macb` = after, it changes the `ACBMGMT` attribute under the section to `CATALOG`. 

Dependencies
------------

None

Example Playbook
----------------
```yaml
    - include_role:
        name: roles/mACB/dfsdf_changes
      vars: 
        macb: before

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
