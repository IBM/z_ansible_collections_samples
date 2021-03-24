prep_zconbt
=========

This role ensures the target host environment is prepared to generate and deploy service archives, API archives, or API requester archives. Includes the `install_zconbt` role and other Ansible modules to prepare the environment.

Files
--------------

* `ara_demo.tar`: Sample project directory for generating API Requester archives.

* `demofiles.tar`: Sample project directory for generating Service and API archives.

Example Playbook
----------------

```yaml
- hosts: all
  gather_facts: no
  environment: "{{ environment_vars }}"

  tasks:
    - name: Prep target host environment
      include_role:
        name: prep_zconbt
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
