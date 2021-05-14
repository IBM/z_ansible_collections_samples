# Using the z/OS collection to interact with MQ for z/OS local queues

These sample playbooks demonstrate how to use the `zos_operator` module from the `ibm_zos_core` collection to run MQSC
commands against an MQ for z/OS queue manager. 

The samples demonstrate how to:

- Create a local queue: mq_define_qlocal.yml
- Display a local queue: mq_display_qlocal.yml
- Alter a local queue: mq_alter_qlocal.yml
- Delete a local queue: mq_delete_qlocal.yml

As most MQSC commands follow the same pattern, these samples can easily be altered to interact with other
MQ objects such as channels, or topics.

## Requirements

- Python 2.7+
- Ansible 2.9+

## Getting Started

If you are unfamiliar with playbooks, you can review our
[detailed configuration guide](../../../../docs/share/configuration_guide.md) or
continue with getting started below.

Optionally, you can use the sample
[host_setup](../../../../zos_administration/host_setup/README.md)
to discover and create your **inventory** and **host_vars** artifacts. It should
be noted that when you use the **host_setup** it will generate a configuration
for the most common dependencies, some playbooks require more customized
configurations, in this case, you can review the sample documentation and
add the additional required variables.

### Update [inventory.yml](inventory.yml) with the information about your system(s)

```yaml
# the target z/OS system
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
PYZ: "/python/v3r8"

# the path to root of ZOAU installation
ZOAU: "/usr/lpp/IBM/zoautil"
```

### Update the host variables for which queue manager and queue to interact with

```yaml
# Queue manager command prefix
mq_cpf: "!MQ21"

# Queue to be altered
mq_queue_name: TEST.Q
```

### Run the samples
For example:

```bash
ansible-playbook -i inventory.yml mq_define_qlocal.yml
```

## What next?

Look at the [other samples](../..) to find examples of what else you can do with MQSC commands.

# Support

Please refer to the [support section](../../../../README.md/#support) for more details.

# License

Licensed under [Apache License, Version 2.0](https://opensource.org/licenses/Apache-2.0).

# Copyright

© Copyright IBM Corporation 2021.
