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
- [**create_CERTAUTH_cert_raw.yml**](create_CERTAUTH_cert_raw.yml) - Create a CERTAUTH certificate using zos_mvs_raw module
- [**create_CERTAUTH_cert_tsocmd.yml**](create_CERTAUTH_cert_tsocmd.yml) - Create a CERTAUTH certificate using zos_tso_command module
- [**create_SITE_cert_raw.yml**](create_SITE_cert_raw.yml) - Create a SITE certificate using zos_mvs_raw module
- [**create_SITE_cert_tsocmd.yml**](create_SITE_cert_tsocmd.yml) - Create a SITE certificate using zos_tso_command module
- [**create_USER_cert_raw.yml**](create_USER_cert_raw.yml) - Create a USER certificate using zos_mvs_raw module
- [**create_USER_cert_tsocmd.yml**](create_USER_cert_tsocmd.yml) - Create a USER certificate using zos_tso_command module
- [**delete_cert_raw.yml**](delete_cert_raw.yml) - Delete a certificate using zos_mvs_raw module
- [**delete_cert_tsocmd.yml**](delete_cert_tsocmd.yml) - Delete a certificate using zos_tso_command module
- [**delete_keyring_raw.yml**](delete_keyring_raw.yml) - Delete a keyring using zos_mvs_raw module
- [**delete_keyring_tsocmd.yml**](delete_keyring_tsocmd.yml) - Delete a keyring using zos_tso_command module
- [**list_cert_raw.yml**](list_cert_raw.yml) - Display a certificate's details using zos_mvs_raw module
- [**list_cert_tsocmd.yml**](list_cert_tsocmd.yml) - Display a certificate details using zos_tso_command module
- [**search_and_renew_raw.yml**](search_and_renew_raw.yml) - Search and a renew a matching certificate found in the RACF_CERTIFCATE_EXPIRATION health check report using zos_mvs_raw module
- [**search_and_renew_tsocmd.yml**](search_and_renew_tsocmd.yml) - Search and a renew a matching certificate found in the RACF_CERTIFCATE_EXPIRATION health check report using zos_tso_command module

## Role Summary

- [**issue_operator_cmd**](roles/issue_operator_cmd/README.md) - Issue an operator command
- [**issue_racf_cmd**](roles/issue_racf_cmd/README.md) - Isuue RACF command(s)
- [**issue_tso_cmd**](roles/issue_operator_cmd/README.md) - Issue TSO command(s)
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
