ims_cobol_run_gens
=========

Perform DBDGEN, PSBGEN, and ACBGEN operations

Requirements
------------

Ansible Collection `ibm.ibm_zos_ims`

Role Variables
--------------

None

Dependencies
------------

Roles:

ims_cobol_execute_permissions_and_copy_ussDatasets

Example Playbook
----------------

```yaml
  - include_role:
      name: ims_cobol_run_gens
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
