# Apply ptf using Using Ansible

This project provides sample playbooks and roles which can be used to perform certificate renewal using RACF.

It is a good practice to review the playbook sample contents before executing
them. It will help you understand the requirements in terms of space, location,
names, authority, and the artifacts that will be created and cleaned up.
Although samples are written to operate without the need for the user’s
configuration, flexibility is written into the samples because it is not easy
to determine if a sample has access to the host’s resources. Review the
playbook notes sections for additional details and configuration.

## Playbook Summary

- [**health_checker_security**](health_checker_security.yml) - Set up security profile for accessing Health Checker functions
- [**create_SSL_cert.yml**](create_SSL_cert.yml) - Create a sample SSL certificate
- [**create_TN3270.cert.yml**](create_TN3270_cert.yml) - Create a sample SITE certificate
- [**search_and_renew.yml**](search_and_renew.yml) - Search and a renew a matching certificate found in the RACF_CERTIFCATE_EXPIRATION health check report

## Role Summary

- [**issue_operator_cmd**](roles/issue_operator_cmd/README.md) - Issue an operator command
- [**issue_racf_cmd**](roles/issue_racf_cmd/README.md) - Isuue RACF command(s)
- [**print_hc_buffer**](roles/print_hc_buffer/README.md) - Pull data from Health Checker
- [**send-template**](roles/send-template/README.md) - send template to a zOS host

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
