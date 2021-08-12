create_server
=========

Create an instance of a WebSphere Liberty server. 

Requirements
------------

None 

Role Variables
--------------

| Variable      | Definition                             | Set                                              |
| ------------- | ---------------------------------------|--------------------------------------------------|
| server_instance_name | The unique name for the instance of Liberty server | `host_vars/zos_host.yml`       |
| liberty_bin_path | The path to the Liberty installation bin folder | `host_vars/zos_host.yml`            |

Dependencies
------------

None 

Example Playbook
----------------
```yaml
  - include_role:
          name: create_server
```
License
-------

BSD

Author Information
------------------

An optional section for the role authors to include contact information, or a website (HTML is not allowed).
