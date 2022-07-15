install-bzip2
=========

This role is used to deploy _bzip2_ to a z/OS system.

Requirements
------------

The compressed archive for _bzip2_ needs to be acquired from Rocket Software. Decompress and unpack the _bzip2_ binary and place it in this role's _files_ folder.

Role Variables
--------------

| Variable           | Definition                                                                             |
|--------------------|----------------------------------------------------------------------------------------|
| uss_utilities_path | where MVSUTILS/MVSCMD and other needed tools/scripts are installed on z/OS target node |

Dependencies
------------

None

Example Playbook
----------------

```yaml
  - include_role: 
      name: install-bzip2
```

License
-------

BSD

Author Information
------------------

Blake Becker (blake.becker@ibm.com)