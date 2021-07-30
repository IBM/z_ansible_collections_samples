ims_cobol_execute_permissions_and_copy_ussDatasets
=========

Execute permissions for the copied data and copy the data from USS to MVS data sets.

Requirements
------------

Ansible Collection `ibm.ibm_zos_core`

Role Variables
--------------

| Variable                   | Definition                                                                                                                                                          |
|----------------------------|---------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| PROJECT_NAME               | Project name to use when defining the IMS COBOL application                                                                                                          |
| SAMPLE_FILE_DIR            | Location to place sample application files                                                                                                               |
| loadData                   | Database names used to copy source from USS to MVS                                                                                     |


  ```yaml
  loadData:
  - name: ACCOUNT
  - name: CUSTACCS
  - name: CUSTOMER
  - name: HISTORY
  - name: TSTAT
  ```

Dependencies
------------

Roles:

* ims_cobol_copy_extract_files
* ims_cobol_create_app_datasets

Example Playbook
----------------

```yaml
  - include_role:
      name: ims_cobol_execute_permissions_and_copy_ussDatasets
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
