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

A list of other roles hosted on Galaxy should go here, plus any details in regards to parameters that may need to be set for other roles, or variables that are used from other roles.

Example Playbook
----------------

```yaml
 - include_role:
     name: send-templates
   vars:
     template_path: '{{ playbook_path }}/templates/step1/*'
```

License
-------

BSD

Author Information
------------------

Blake Becker (blake.becker@ibm.com)