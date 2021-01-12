git_clone_private_repo
=========

This role utilizes ssh forwarding to copy source code from a private remote repository on Github to the target host.

Requirements
------------

Requires that the ssh key to access the remote repository be specified within [default variables yaml](./defaults/main.yml).

Role Variables
--------------

* `secret_key`: Github secret key that can be found on localhost to access github repository

* `TARGET_USERNAME`: This should always be overridden by higher prescedence variable

* `GIT_REPO`: Git repository to be cloned. Must be SSH (not Https)

* `GIT_BRANCH`: Github branch to clone from.

* `CLONE_DEST`: Directory where the repository will be cloned


Example Playbook
----------------

```yaml
- hosts: all
  gather_facts: no
  environment: "{{ environment_vars }}"

  tasks:
    - name: Clone private repoistory from Github
      include_role:
        name: git_clone_private_repo
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
