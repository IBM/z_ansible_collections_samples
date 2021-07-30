ims_cobol_compile_link_cobol
=========

Compile and link COBOL and load programs.

Requirements
------------

COBOL must be installed and the PATH variable should contain the COBOL directory with the cob2 executable.

Role Variables
--------------

| Variable                   | Definition                                                                                                                                                          |
|----------------------------|---------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| COBOL_COMP_LIB_HLQ         | The 1st qualifier of the COBOL compiler library      |                                                                                                      |
| SAMPLE_COBOL_DIR           | Location to place the COBOL program files   |
|
| COBOL_PROGRAMS           | A list of the location of the COBOL program files      |
|

Dependencies
------------

Roles:

* ims_cobol_copy_extract_files
* ims_cobol_create_app_datasets
* ims_cobol_execute_permissions_and_copy_ussDatasets
* ims_cobol_check_acblib

Example Playbook
----------------

```yaml
  - include_role:
      name: ims_cobol_compile_link_cobol
```

Copyright
---------

© Copyright IBM Corporation 2021

License
-------

Licensed under [Apache License, Version 2.0](https://opensource.org/licenses/Apache-2.0)

Author Information
------------------

Dipti Gandhi - @ddgandhi

Support
-------

Please refer to the [support section](https://github.com/IBM/z_ansible_collections_samples/blob/master/README.md#support) for more details.
