Common Queue Services Role
=========

CQS role defines CQS component and query its status.

Requirements
------------
* IBM z/OS core collection 1.4.0
* IBM z/OS IMS collection 1.2.0


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
          vars:
            cqs: config

```

## Copyright
© Copyright IBM Corporation 2022

## License
Licensed under
[Apache License, Version 2.0](https://opensource.org/licenses/Apache-2.0).

