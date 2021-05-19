# Copy Directory to PDS, Edit member and Submit Job

This sample focuses on processing the result of a z/OS® job submission response
using a number of Ansible® constructs such as `when`, `with_sequence`, `loop`,
`set_fact`, `debug`, `tags`, etc, all available as part of the Ansible engine.

Because this sample is aimed at helping users become familiar with the various
Ansible constructs, it includes a response from a submitted job in both JSON
and YAML format (both equally the same) such that you can develop against
the JSON/YAML responses to avoid having to submit a job for every playbook
edit. Optionally the playbook can be run in production mode where the playbook
will need to connect to an actual z/OS host (`hosts`) and the playbook task
`zos_job_submit` must have JCL it can submit.

It is a good practice to review the playbook sample contents before executing
them. It will help you understand the requirements in terms of space, location,
names, authority, and the artifacts that will be created and cleaned up.
Although samples are written to operate without the need for the user’s
configuration, flexibility is written into the samples because it is not easy
to determine if a sample has access to the host’s resources. Review the
playbook notes sections for additional details and configuration.

## Requirements

  - [IBM® z/OS® core collection 1.0.0 or later](https://galaxy.ansible.com/ibm/ibm_zos_core)
  - [Ansible® 2.10 or later](https://docs.ansible.com/ansible/latest/installation_guide/intro_installation.html)
  - [jmespath](https://pypi.org/project/jmespath/)

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


### Update [inventory.yml](inventory.yml) with the information about your system(s)

```yaml
# the system where the data should be copied to
source_system:
  hosts:
    zos_host:
      ansible_host: zos_target_address
      ansible_user: zos_target_username
      ansible_python_interpreter: path_to_python_interpreter_binary_on_zos_target
```

### Update the environment variables for each z/OS system in [host_vars/zos_host.yml](host_vars/zos_host.yml)

```yaml
# the path to the root of IBM python installation
PYZ: "/usr/lpp/IBM/cyp/v3r8/pyz"

# the path to root of ZOAU installation
ZOAU: "/usr/lpp/IBM/zoautil"
```

### Run desired playbook with the supported Ansible `--tags``

```bash
Usage:
 ansible-playbook -i <inventory> <playbook> --tags "....."
```

```bash
  1) Run in development mode using JSON cached input
        ansible-playbook -i inventory zos_job_submit_ansible_constructs.yaml --tags "mode_development_json"
  2) Run in development mode using YAML cached input
        ansible-playbook -i inventory zos_job_submit_ansible_constructs.yaml --tags "mode_development_yaml"
  3) Run in development mode using JSON cached input with verbose
        ansible-playbook -i inventory zos_job_submit_ansible_constructs.yaml --tags "mode_development_json,mode_verbose"
  4) Run in development mode using YAML cached input with verbose
        ansible-playbook -i inventory zos_job_submit_ansible_constructs.yaml --tags "mode_development_yaml,mode_verbose"
  5) Run in production mode on z/OS target
        ansible-playbook -i inventory zos_job_submit_ansible_constructs.yaml --tags "mode_production"
  6) Run in production mode on z/OS target with verbose
        ansible-playbook -i inventory zos_job_submit_ansible_constructs.yaml --tags "mode_production,mode_verbose"
```

# Copyright

© Copyright IBM Corporation 2020

# License

Licensed under [Apache License,
Version 2.0](https://opensource.org/licenses/Apache-2.0).

# Support

Please refer to the [support section](../../../README.md#support) for more
details.