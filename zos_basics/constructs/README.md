# Job Submit Response Parsing and Constructs
This sample focuses on processing the result of a z/OS® job submission response
using a number of Ansible® constructs such as
[when](https://docs.ansible.com/ansible/latest/user_guide/playbooks_conditionals.html#conditionals),
[with_sequence](https://docs.ansible.com/ansible/latest/user_guide/playbooks_loops.html#loops),
[loop](https://docs.ansible.com/ansible/latest/user_guide/playbooks_loops.html#loops),
[set_fact](https://docs.ansible.com/ansible/latest/collections/ansible/builtin/set_fact_module.html),
[debug](https://docs.ansible.com/ansible/latest/collections/ansible/builtin/debug_module.html),
[tags](https://docs.ansible.com/ansible/latest/user_guide/playbooks_tags.html),
etc, all available as part of the Ansible engine.

Because this sample is aimed at helping users become familiar with the various
Ansible constructs, it includes a response from a submitted job in both JSON
and YAML format (both equally the same) such that you can develop against
the JSON/YAML responses to avoid having to submit a job for every playbook
edit. Optionally the playbook can be run in production mode where the playbook
will need to connect to an actual z/OS host (`hosts`) and the playbook task
`zos_job_submit` must have JCL it can submit.

Although playbooks are written to operate without the need for the user’s
configuration, flexibility is written into the samples because it can't always
be determined if a sample has access to the host’s resources. Review the
playbook for additional details and configuration.

> [!IMPORTANT]
> The release of `ibm_zos_core` collection version 1.10.0 introduced case-sensitive
> changes to modules, specifically that module choices must be lower case. See the porting
> guide in the release notes
> ([here](https://ibm.github.io/z_ansible_collections_doc/ibm_zos_core/docs/source/release_notes.html#porting-guide))
> to understand the full scope of these changes.
>
> These changes are not compatible with earlier versions of the `ibm_zos_core` collection, 
> and since the earlier versions do not reach their end of life until April 2025, 
> the sample playbook compatible with the older versions will remain available.
>
> To support both the latest and prior versions of the collection: 
> * The `main` branch will reflect playbooks compatible with `ibm_zos_core` version 1.10.0 or later.
>   * Find this playbook on the `main` branch: [here](https://github.com/IBM/z_ansible_collections_samples/tree/main/zos_basics/constructs).
> * The `ibm_zos_core-v1.9.x-samples` branch will reflect playbooks compatible with `ibm_zos_core` version 1.9.x or earlier.
>   * Find this playbook on the `ibm_zos_core-v1.9.x-samples` branch: [here](https://github.com/IBM/z_ansible_collections_samples/tree/ibm_zos_core-v1.9.x-samples/zos_basics/constructs).
> 
> The `ibm_zos_core-v1.9.x-samples` branch will remain available until April 2025 but will not receive updates.
>
>

## Playbook Requirements
- [IBM® z/OS® core collection 1.0.0 or later](https://galaxy.ansible.com/ibm/ibm_zos_core)
- [Ansible® 2.11 or later](https://docs.ansible.com/ansible/latest/installation_guide/intro_installation.html)
- [jmespath](https://pypi.org/project/jmespath/)
    - `pip install jmespath`

## Configuration
- Configure the included [inventory.yml](inventories/inventory.yml) with the
  information from the managed z/OS host.
  - Review [inventory documentation](../../docs/share/zos_core/configure_inventory.md)
- Configure the included **host_vars** [zos_host.yml](inventories/host_vars/zos_host.yml)
  with the information from your z/OS system.
  - Review [host_vars documentation](../../docs/share/zos_core/configure_host_vars.md)
    and any additional noted variables in the configuration.

### Run desired playbook with the supported Ansible --tags
This project has included a `site.yml` playbook that serves as the primary playbook
that provides additional prerequisite checks then it invokes the `zos_job_submit_ansible_constructs.yml`
playbook.

If you want to run the primary playbook `site.yml` it will check that your environment
has the correct version of Ansible as well as the collection needed to execute
correctly. To run the primary playbook, use command:

```bash
ansible-playbook -i inventory.yml site.yml
```

You can skip the prerequisite check and run the `zos_job_submit_ansible_constructs.yml` with
command:

```bash
ansible-playbook -i inventory.yml zos_job_submit_ansible_constructs.yml
```

Additional ways to run this playbook using --tags listed below control how the
playbook is executed.

```bash
Usage:
 ansible-playbook -i <inventory> <playbook> --tags "....."
```

```
  1) Run in development mode using JSON cached input
        ansible-playbook -i inventory.yml zos_job_submit_ansible_constructs.yml --tags "mode_development_json"
  2) Run in development mode using YAML cached input
        ansible-playbook -i inventory.yml zos_job_submit_ansible_constructs.yml --tags "mode_development_yaml"
  3) Run in development mode using JSON cached input with verbose
        ansible-playbook -i inventory.yml zos_job_submit_ansible_constructs.yml --tags "mode_development_json,mode_verbose"
  4) Run in development mode using YAML cached input with verbose
        ansible-playbook -i inventory.yml zos_job_submit_ansible_constructs.yml --tags "mode_development_yaml,mode_verbose"
  5) Run in production mode on z/OS target
        ansible-playbook -i inventory.yml zos_job_submit_ansible_constructs.yml --tags "mode_production"
  6) Run in production mode on z/OS target with verbose
        ansible-playbook -i inventory.yml zos_job_submit_ansible_constructs.yml --tags "mode_production,mode_verbose"
```

# Changelog
All changes are maintained chronologically by date found in the
[changelog](changelog.yml).

# Copyright
© Copyright IBM Corporation 2021

# License
Licensed under [Apache License,
Version 2.0](https://opensource.org/licenses/Apache-2.0).

# Support
Please refer to the [support section](../../../README.md#support) for more
details.
