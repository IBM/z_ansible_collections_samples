restart
=========

Restart the IMS instance by bringing up the Common Service Layer (CSL) address spaces and issuing the `NRESTART` command. More specifically, the Operations Manager (OM), Structured Call Interface (SCI), and Resource Manager (RM) address spaces of the CSL will be brought up. This role also attempts to restart IMS Connect, but errors will be ignored if IMS Connect is not configured. 

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
        name: restart

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
