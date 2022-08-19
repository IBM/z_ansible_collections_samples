send-templates
=========

This role performs the following actions:

1. Interpret a folder of templates on the Ansible controller
2. Archive and compress the finished templates
3. Copy the archive to z/OS target node
4. Decompress the archive
5. Encode all files to correct encoding

Requirements
------------

Bzip2 needs to be present at *uss_utilities_path*

Role Variables
--------------

| Variable            | Definition                                                                             |
|---------------------|----------------------------------------------------------------------------------------|
| template_path       | The path on Ansible control node to the directory to search for Jinja templates.       |
| uss_file_path       | the path where JCL and other scripts will be stored                                    |
| uss_utilities_path  | where MVSUTILS/MVSCMD and other needed tools/scripts are installed on z/OS target node |

Dependencies
------------

None.

Example Playbook
----------------

```yaml
 - include_role:
     name: send-templates
   vars:
     template_path: '{{ playbook_path }}/templates/step1/*'
```

## Copyright

© Copyright IBM Corporation 2022

## License
Licensed under
[Apache License, Version 2.0](https://opensource.org/licenses/Apache-2.0).
