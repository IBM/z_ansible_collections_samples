ims_cobol_create_app_datasets
=========

Create application data sets required for IMS COBOL application deployment.

Requirements
------------

Ansible Collection `ibm.ibm_zos_core`

Role Variables
--------------

| Variable                   | Definition                                                                                                                                                          |
|----------------------------|---------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| datasets                   | List of data sets and their attributes to create during application deployment                                      |
| loads                      | List of data sets and their attributes needed to load the application data

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
      name: ims_cobol_create_app_datasets
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
