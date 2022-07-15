Common Queue Services Role
=========

CQS role defines CQS component and query its status.

Requirements
------------

* Ansible Collection: ibm.ibm_zos_core and ibm.ibm_zos_ims

Role Variables
--------------

| Variable                           | Definition                                                                                                                                                          |
| ---------------------------------- | ------------------------------------------------------------------------------------------------------------------------------------------------------------------- |                                                                                                                       
| cqs                | If cqs = "config", CQS component will be allocated.  If cqs = "query", CQS status will be returned                                                                                                                           |

                                                                                          

Dependencies
------------

None

Example Playbook
----------------

```yaml
        - include_role:
            name: common_queue_services
            tasks_from: common_queue_services
          vars:
            cqs: config

```

License
-------

Copyright (c) IBM Corporation 2020 Apache License, Version 2.0 (see https://opensource.org/licenses/Apache-2.0)

Author Information
------------------

An Lam (an.lam@ibm.com)
