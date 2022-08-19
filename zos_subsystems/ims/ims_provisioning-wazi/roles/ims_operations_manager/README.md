Operatrions Manager
=========

Configure, query, start or stop Operations Manager (OM)


Requirements
------------
* IBM z/OS core collection 1.4.0
* IBM z/OS IMS collection 1.2.0

Role Variables
--------------

* om:
  - "config": configure OM
  - "start": start OM
  - "stop": stop OM
  - "query": query OM status


Dependencies
------------

None

Example Playbook
----------------

```yaml
    - include_role:
        name: ims_operations_manager
      vars:
        om: start

```


## Copyright

© Copyright IBM Corporation 2022

## License
Licensed under
[Apache License, Version 2.0](https://opensource.org/licenses/Apache-2.0).
