Common Queue Services
=========

CQS role allocates CQS component and query its status.

Requirements
------------

* Ansible Collection: `ibm.ibm_zos_core` and `ibm.ibm_zos_ims`

Role Variables
--------------

- ### **cqs**

  Specifies to allocate or query CQS
  - 'config': allocate CQS component
  - 'query': query CQS status

                                                       

Dependencies
------------

None

Example Playbook: How to allocate CQS
----------------

```yaml
        - include_role:
            name: common_queue_services
            tasks_from: common_queue_services
          vars:
            cqs: "config"

```

Example Playbook: How to query CQS
----------------

```yaml
        - include_role:
            name: common_queue_services
            tasks_from: common_queue_services
          vars:
            cqs: "query"

```



License
-------

Copyright (c) IBM Corporation 2020 Apache License, Version 2.0 (see https://opensource.org/licenses/Apache-2.0)

Author Information
------------------

An Lam (an.lam@ibm.com)
