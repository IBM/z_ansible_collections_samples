install-bzip2
=========

Deploys _bzip2_ to a z/OS system.

Requirements
------------

* The compressed archive for _bzip2_ needs to be acquired from Rocket Software. Decompress and unpack the _bzip2_ binary and place it in this role's _files_ folder.

Role Variables
--------------

- ### **  uss_utilities_path **
   where MVSUTILS/MVSCMD and other needed tools/scripts are installed on z/OS target node.


Dependencies
------------

None

Example Playbook
----------------

```yaml
  - include_role: 
      name: install-bzip2
```

## Copyright

© Copyright IBM Corporation 2020

License
-------

Copyright (c) IBM Corporation 2020 Apache License, Version 2.0 (see https://opensource.org/licenses/Apache-2.0)
