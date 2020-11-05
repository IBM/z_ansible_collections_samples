zcee_reserve_ports
=========

Reserve ports for z/OS Connect EE.

Requirements
------------

None

Role Variables
--------------

| Variable                    | Definition                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    |
| --------------------------- | --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| tcpip_profile               | The location of the TCPIP profile information, used to allocate ports.                                                                                                                                                                                                                                                                                                                                                                                                                         |
| DYNAMICALLY_RESERVE_PORTS   | If DYNAMICALLY_RESERVE_PORTS=yes, playbooks will attempt to reserve ports.                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              |
| RESERVE_SPECIFIC_PORT       | If DYNAMICALLY_RESERVE_PORTS=yes and RESERVE_SPECIFIC_PORT=no, playbook will reserve an open port within the default range of the Ansible zport module.                                                                                                                                                                                                                                                                                                                                        |
| zcon_http_port              | HTTP port for zCEE, may be determined at runtime.                                                                                                                                                                                                                                                                                                                                                                                                                                              |
| zcon_https_port             | HTTPS port for zCEE, may be determined at runtime.                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           |
| zcon_hostname               | The host name of the system running zCEE.                                                                                                                                                                                                                                                                                                                                                                                                                                                                     |


Dependencies
------------

This role is dependent on the zport module. zPort dynamically allocates a port on TCP and/or UDP using Unix System Services on z/OS.

Example Playbook
----------------

```yaml
- include_role:
    name: zcee_reserve_ports
```

License
-------

Copyright (c) IBM Corporation 2020 Apache License, Version 2.0 (see https://opensource.org/licenses/Apache-2.0)

Author Information
------------------

Dipti Gandhi (ddgandhi@us.ibm.com)
