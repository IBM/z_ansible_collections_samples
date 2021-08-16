# Configure Ansible Inventory
Ansible inventory file is a list of all the managed host names that Ansible
will connect. Also part of the inventory is the user that Ansible should
connect to over SSH to the managed host.

## Description of the properties used in this configuration
* Property `ansible_host` is the z/OS managed node (target), e.g, `ansible_host: "zvm1.vmec.svl.ibm.com"`
* Property `ansible_user` is the z/OS managed user to connect and run as over SSH,  e.g, `ansible_user: "zosadm"`

Configure `ansible_host` and `ansible_user` in the included inventory.

For example:

```yaml
source_system:
  hosts:
    zos_host:
      ansible_host: "zvm1.vmec.svl.ibm.com"
      ansible_user: "zosadm"
```

If you are unfamiliar with playbook configuration practices and would like to
learn more, you can review our [detailed configuration guide](configuration_guide.md).