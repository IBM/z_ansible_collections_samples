add_certificate
=========

Add a digital certificate to a key ring and activate it

Requirements
------------

- Ansible Collection `ibm.ibm_zos_core`

Role Variables
--------------

- ### **ansible_user**

  Specifies the userID that owns the Key Ring

Example Playbook
----------------

```yaml
- hosts: zos_host
  collections:
    - ibm.ibm_zos_core
  gather_facts: no
  environment: "{{ environment_vars }}"
  vars:

  roles:
    - role: add_certificate
```

License
-------

Copyright (c) IBM Corporation 2021 Apache License, Version 2.0 (see https://opensource.org/licenses/Apache-2.0)

Author Information
------------------

- Andy Nguyen andy.nguyen@ibm.com

Copyright
---------

© Copyright IBM Corporation 2021
