submit-rexx
=========

Used for the submission of rexx jobs that need to run in TSO/E address space.
This role runs the program `IKJEFT01`
By default, the REXX script to run should be located at {{ DFS_AUTH_LIB_HLQ1 }}.{{ DFS_AUTH_LIB_HLQ2 }}.{{ TSO_REXX_HLQ3 }}( {{ rexx_name }} )

Requirements
------------

Must be run as system administrator

Role Variables
--------------

| Variable      | Definition                                          |
|---------------|-----------------------------------------------------|
| rexx_name     | The name of the rexx script to run in z/OS USS      |
| uss_file_path | the path where JCL and other scripts will be stored |

Dependencies
------------

Depends on the following roles:

* submit-tso-rexx
* check-job-status
* save-as-dataset

Example Playbook
----------------
```yaml
- name: Submit REXX
  include_role:
    name: submit-tso-rexx
    public: yes
  vars:
    rexx_name: 'WAITJOB'
```
License
-------

BSD

Author Information
------------------

Blake Becker (blake.becker@ibm.com)