System Definitions Service
=========

This role defines pre-processor macros and stage 1 & 2 data sets

Requirements
------------

* Ansible Collection: `ibm.ibm_zos_core` and `ibm.ibm_zos_ims`


Role Variables
--------------

- ### **preprocess**

  If preprocess is true, creates pre-processor macros.

- ### **stage1**

  If stage1 is true, creates stage 1 data definitions.

- ### **stage2**

  If stage2 is true, creates stage 2 data definitions.


Dependencies
------------

None

Example Playbook:
----------------

```yaml 
        - include_role:
            name: ims_sysdef
          vars:
            preprocess: true
            stage1: true
            stage2: true

```


## Copyright

© Copyright IBM Corporation 2020

License
-------

Copyright (c) IBM Corporation 2020 Apache License, Version 2.0 (see https://opensource.org/licenses/Apache-2.0)


