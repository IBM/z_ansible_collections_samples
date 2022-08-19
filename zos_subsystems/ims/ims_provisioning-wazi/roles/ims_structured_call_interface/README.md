Structured Call Interface
=========

Configure, query, start or stop Structured Call Interface (SCI)


Requirements
------------
* IBM z/OS core collection 1.4.0
* IBM z/OS IMS collection 1.2.0

Role Variables
--------------

* sci:
  - "config": configure SCI
  - "start": start SCI
  - "stop": stop SCI
  - "query": query SCI status


Dependencies
------------

None

Example Playbook
----------------

```yaml
    - include_role:
        name: ims_structured_call_interface
      vars:
        sci: start

```


## Copyright

© Copyright IBM Corporation 2022

## License
Licensed under
[Apache License, Version 2.0](https://opensource.org/licenses/Apache-2.0).
