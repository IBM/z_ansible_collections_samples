save-templates-to-datasets
=========

Save all files in

Requirements
------------

Any pre-requisites that may not be covered by Ansible itself or the role should be mentioned here. For instance, if the role uses the EC2 module, it may be a good idea to mention in this section that the boto package is required.

Role Variables
--------------

| Variable          | Definition                                                                                                                                                          |
| ----------------- | ------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| DFS_AUTH_LIB_HLQ1 | The first HLQ to be used to store middleware (IMS, zCEE, etc) related datasets. these include procs, OLDS, WADS, PROCLIB, JOBS, and application specific datasets.  |
| DFS_AUTH_LIB_HLQ2 | The second HLQ to be used to store middleware (IMS, zCEE, etc) related datasets. these include procs, OLDS, WADS, PROCLIB, JOBS, and application specific datasets. |
| uss_file_path     | the path where JCL and other scripts will be stored                                                                                                                 |
| datasets          | A dictionary containing file name to copy and desired dataset location to copy to. Example below.                                                                   |

```yaml
datasets:
  provision:
    - file_name: DFSE302J
      ds_member: IV3E302J
      hlq: INSTALL
    - file_name: DFSALSY1
      ds_member: IV3E101J
      hlq: INSTALL
    - file_name: DFSALCAT
      ds_member: IV3E319J
      hlq: INSTALL
    - file_name: DFSALSY2
      ds_member: IV3C101J
      hlq: INSTALL
    - file_name: DFSSETPG
      ds_member: IV3D201T
      hlq: INSTALL
    - file_name: DFS4CPYP
      ds_member: IV3E312J
      hlq: INSTALL
    - file_name: DFSE313J
      ds_member: IV3E313J
      hlq: INSTALL
    - file_name: DFSCPYST
      ds_member: IV3E318J
      hlq: INSTALL
    - file_name: DFSE304J
      ds_member: IV3E304J
      hlq: INSTALL
    - file_name: DFSE303J
      ds_member: IV3E303J
      hlq: INSTALL
    - file_name: DFSDYNAL
      ds_member: IV3E301J
      hlq: INSTALL
    - file_name: DFSENOLC
      ds_member: IV3E317J
      hlq: INSTALL
    - file_name: DFS4ACTV
      ds_member: IV3E318J
      hlq: INSTALL
    - file_name: DFSGNPRE
      ds_member: IV3C202J
      hlq: INSTALL
    - file_name: DFSE315J
      ds_member: IV3E315J
      hlq: INSTALL
    - file_name: DFSE314J
      ds_member: IV3E314J
      hlq: INSTALL
    - file_name: DFSC105J
      ds_member: IV3C105J
      hlq: INSTALL
    - file_name: DFS4STG1
      ds_member: IV3C203J
      hlq: INSTALL
    - file_name: DFSDFDRC
      ds_member: IV3E207J
      hlq: INSTALL
  deprovision:
    - file_name: DFSUNMNT
      ds_member: UNMOUNT
      hlq: INSTALL
```

Dependencies
------------

A list of other roles hosted on Galaxy should go here, plus any details in regards to parameters that may need to be set for other roles, or variables that are used from other roles.

Example Playbook
----------------

Including an example of how to use your role (for instance, with variables passed in as parameters) is always nice for users too:

    - hosts: servers
      roles:
         - { role: username.rolename, x: 42 }

License
-------

BSD

Author Information
------------------

Blake Becker (blake.becker@ibm.com)