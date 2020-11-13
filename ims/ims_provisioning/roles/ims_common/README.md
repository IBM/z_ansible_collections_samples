Common Services
=========

Provide the following services to IMS:
* Start and stop IMS Control region and IMS Connect
* Cold start IMS
* Wait for IMS ready


Requirements
------------

* Ansible Collection: `ibm.ibm_zos_core` and `ibm.ibm_zos_ims`

Role Variables
--------------
- ### **icon**

  Specifies to start or stop IMS Connect
  - 'start': start IMS Connect service
  - 'stop': stop IMS Connect service

- ### **ctl**

  Specifies to start or stop IMS Control Region
  - 'start': start IMS Control region
  - 'stop': stop IMS Control region

- ### **cold-start**

  When cold-start is true, IMS will be cold-started.


- ### **wait_for_job**

  When wait_for_job is true, the play book waits for all IMS services to come up.



Dependencies
------------

None

Example Playbook: How to start IMS Connection, Control region, 
IMS (cold) and wait for all services to be up
----------------

```yaml
        - include_role:
            name: ims_common
            tasks_from: ims_common
          vars:
            icon: "start"
            ctl: "start"
            wait_for_job: true
            cold_start: true

```


## Copyright

© Copyright IBM Corporation 2020

License
-------

Copyright (c) IBM Corporation 2020 Apache License, Version 2.0 (see https://opensource.org/licenses/Apache-2.0)

Author Information
------------------

An Lam (an.lam@ibm.com)
