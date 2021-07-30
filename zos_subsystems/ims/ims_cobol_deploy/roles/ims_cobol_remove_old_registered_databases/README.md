ims_cobol_remove_old_registered_databases
=========

Perform delete of the old registered databases for the COBOL application.

Requirements
------------

Ansible Collection `ibm.ibm_zos_core`

Role Variables
--------------

| Variable                   | Definition                                                                                                                                                          |
|----------------------------|---------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| datasets                   | List of data sets and their attributes to create during application deployment                             |
| dbds                       | List of variables with the name attribute referencing members of DBDLIB. Defaults are sufficient.                                                                       |
| loads                      | List of data sets and their attributes needed to load the application data                                                                                                  |
| psbs                       | List of variables with the name attribute referencing members of PSBLIB. Defaults are sufficient.                                                                       |


  ```yaml
  datasets:
  - name: DBD
    type: library
    primary_size: 2
    secondary_size: 1
    record_format: fb
    logical_record_length: 80
    block_size: 32720
    dir_blocks: 10
  ```
  
  ```yaml
    loads:
    - name: ACCOUNT
      type: basic
      primary_size: 3
      secondary_size: 1
      record_format: fb
      logical_record_length: 200
      block_size: 27800
  ```

Dependencies
------------

None.

Example Playbook
----------------

```yaml
  - include_role:
      name: ims_cobol_remove_old_registered_databases
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
