ims_cobol_copy_acblib_to_inactive_acb
=========

Copy ACBLIB to an inactive ACB library.

Requirements
------------

Ansible Collection `ibm.ibm_zos_core`

Role Variables
--------------

| Variable           | Definition                                                                             |
|--------------------|----------------------------------------------------------------------------------------|
| INACTIVE_ACBLIB    | Name of the inactive ACBLIB, dynamically overridden during app deployment
| ROUTE              | SSID of the IMS instance                                                           |
| acblib             | A list of variabless with the name attribute referencing members of the ACBLIB. The defaults are sufficient.                                                                     |


```yaml
 acblib:
  - name: ACCOUNT
  - name: ACCTYPE
  - name: CUSTACCS
  - name: CUSTOMER
  - name: CUSTTYPE
  - name: HISTORY
  - name: TSTAT
  - name: TSTATTYP
  - name: TTYPE
  - name: IBACSUM
  - name: IBLOAD
  ```

Dependencies
------------

Roles:

* ims_cobol_check_acblib

Example Playbook
----------------

```yaml
    - include_role:
        name: ims_cobol_copy_acblib_to_inactive_acb
```

Copyright
---------

© Copyright IBM Corporation 2021

License
-------

Licensed under Apache License, Version 2.0

Author Information
------------------

Dipti Gandhi - @ddgandhi

Support
-------

Please refer to the [support section](https://github.com/IBM/z_ansible_collections_samples/blob/master/README.md#support) for more details.
