install_zconbt
=========

This role installs z/OS Connect Build Toolkit onto the target host if not already present.

Requirements
------------

Requires a zconbt tar file.

Role Variables
--------------

* `zconbt_compressed_name`: file path to compressed version of z/OS Connect Build Toolkit tar file.


Example Playbook
----------------

```yaml
- hosts: all
  gather_facts: no
  environment: "{{ environment_vars }}"

  tasks:
    - name: Install z/OS Connect Build Toolkit
      include_role:
        name: install_zconbt
```

License
-------

Copyright (c) IBM Corporation 2020 Apache License, Version 2.0 (see https://opensource.org/licenses/Apache-2.0)

Author Information
------------------

- Omar Elbarmawi omar@ibm.com, [@oelbarmawi](https://github.com/oelbarmawi)

Copyright
---------

© Copyright IBM Corporation 2020
