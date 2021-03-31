# Apply ptf using Using Ansible

This project provides sample playbooks and roles which can be used to perform SMPE operations.

It is a good practice to review the playbook sample contents before executing
them. It will help you understand the requirements in terms of space, location,
names, authority, and the artifacts that will be created and cleaned up.
Although samples are written to operate without the need for the user’s
configuration, flexibility is written into the samples because it is not easy
to determine if a sample has access to the host’s resources. Review the
playbook notes sections for additional details and configuration.

## Playbook Summary

- [**setup_internet_retrieval.yml**](setup_internet_retrieval.yml) - Set up IBM Internet Service Retrieval by adding digital certificates to RACF database.
- [**order.yml**](order.yml) - Handles order a PTF from IBM and receive it into the Global zone using IBM Internet Service Retrieval.
- [**install.yml**](install.yml) - Handles APPLY process for a PTF.
- [**uninstall.yml**](uninstall.yml) - Handles RESTORE process for a PTF.
- [**accept.yml**](accept.yml) - Handles ACCEPT process for a PTF.
- [**reject.yml**](reject.yml) - Handles REJECT process for a PTF.
- [**query_csi.yml**](query_csi.yml) - Query CSI data.

## Role Summary

- [**add_certificate**](roles/add_certificate/README.md) - Holds tasks related to adding certificates to RACF.
- [**order_ptf**](roles/order_ptf/README.md) - Holds tasks related to order a PTF from IBM.
- [**apply_ptf**](roles/apply_ptf/README.md) - Holds tasks related to APPLY a PTF.
- [**restore_ptf**](roles/restore_ptf/README.md) - Holds tasks related to RESTORE a PTF.
- [**reject_ptf**](roles/reject_ptf/README.md) - Holds tasks related to REJECT a PTF.
- [**accept_ptf**](roles/accept_ptf/README.md) - Holds tasks related to ACCEPT a PTF.
- [**query_csi**](roles/query_csi/README.md) - Holds tasks related to QUERY data from CSI.

## Ansible Collection Requirement

   IBM z/OS core collection 1.3.0 or later

## Getting Started

If you are unfamiliar with playbooks, you can review our
[detailed configuration guide](../../../docs/share/configuration_guide.md) or
continue with getting started below.

Optionally, you can use the sample
[host_setup](../../../zos_administration/host_setup/README.md)
to discover and create your **inventory** and **host_vars** artifacts. It should
be noted that when you use the **host_setup** it will generate a configuration
for the most common dependencies, some playbooks require more customized
configurations, in this case, you can review the sample documentation and
add the additional required variables.

## Getting Started: CLI

If you are unfamiliar with playbooks, you can review our
[detailed configuration guide](../../../docs/share/configuration_guide.md) or
continue with getting started below.

### 1. Update [inventory.yml](inventory.yml) with the information about your system(s)

```yaml
zsystem:
  hosts:
    zos:
      ansible_host: zos_target_address
      ansible_user: zos_target_username
      ansible_python_interpreter: path_to_python_interpreter_binary_on_zos_target
```

### 2. Update the environment variables for the z/OS system in [host_vars/zos_host.yml](host_vars/zos_host.yml)

```yaml
# the path to the root of IBM python installation
PYZ: "/usr/lpp/IBM/cyp/v3r8/pyz"

# the path to root of ZOAU installation
ZOAU: "/usr/lpp/IBM/zoautil"
```

### 3. Update the playbook specific variables in [host_vars/zos_host.yml](host_vars/zos_host.yml) based on desired behavior

### 4. Run desired playbook

```bash
ansible-playbook -i inventory.yml <playbook-name>
```

# Copyright

© Copyright IBM Corporation 2021

# License

Licensed under [Apache License,
Version 2.0](https://opensource.org/licenses/Apache-2.0)

# Support

Please refer to the [support section](../../../README.md#support) for more
details.
