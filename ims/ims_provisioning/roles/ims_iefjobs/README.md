Job Services
=========

This role creates IEF job files for IMS services: OM, RM, SC, CTL, etc ...

Requirements
------------

* Ansible Collection: `ibm.ibm_zos_core` and `ibm.ibm_zos_ims`


Role Variables
--------------

- ### **ief_jobs**

  Create or delete IEF job files:
  - 'create': create job files.
  - 'delete': delete job files.



Dependencies
------------

None

Example Playbook: How to create IEF jobs
----------------

```yaml 
       - include_role:
            name: ims_iefjobs
          vars:
            ief_jobs: create

```

Example Playbook: How to delete IEF jobs
----------------

```yaml 
       - include_role:
            name: ims_iefjobs
          vars:
            ief_jobs: delete

```


## Copyright

© Copyright IBM Corporation 2020

License
-------

Copyright (c) IBM Corporation 2020 Apache License, Version 2.0 (see https://opensource.org/licenses/Apache-2.0)


