Resource Manager
=========

Configure, query, start or stop Resource Manager (RM)


Requirements
------------
* IBM z/OS core collection 1.4.0
* IBM z/OS IMS collection 1.2.0

Role Variables
--------------

* rm:
  - "config": configure RM
  - "start": start RM
  - "stop": stop RM
  - "query": query RM status


Dependencies
------------

None

Example Playbook
----------------

```yaml
    - include_role:
        name: ims_resource_manager
      vars:
        rm: start

```


## Copyright

© Copyright IBM Corporation 2022

## License
Licensed under
[Apache License, Version 2.0](https://opensource.org/licenses/Apache-2.0).
