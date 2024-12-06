# Using module_defaults to install a program definition

This sample demonstrates using Ansible [module defaults](https://docs.ansible.com/ansible/latest/playbook_guide/playbooks_module_defaults.html) to provide variable information to each of the tasks that use modules in the `cmci_group`, allowing the playbook to be simplified. The playbook creates and installs a CICS program definition, and then deletes the program and the program definition.

## Requirements

- Python 3.8+
- Ansible-core 2.12+
- IBM z/OS CICS Ansible collection 1.0.4+

## Getting Started

You will need to have set up the CMCI REST API in your CICS environment. You
can enable the CMCI REST API in either CICSPlex SM environments, or in
stand-alone CICS regions. The `cmci_*` modules use the *CMCI REST API* to
interact with your CICS environment.

For detailed installation instructions please consult
[the documentation](https://ibm.github.io/z_ansible_collections_doc/installation/installation.html).

You can install the IBM z/OS CICS collection from Ansible Galaxy by using the
`ansible-galaxy` CLI, which is supplied with your Ansible installation:

```bash
ansible-galaxy collection install ibm.ibm_zos_cics
```

For more information about the CMCI REST API, see the
[CMCI overview in the CICS TS documentation](https://www.ibm.com/docs/en/cics-ts/latest?topic=fundamentals-cics-management-client-interface-cmci).

Because this playbook only uses the CMCI REST API, it can be run on the control node directly, without having to
configure an inventory. Generally you'll be able to use this trick with any of the CMCI modules. In this example, we
run multiple `cmci_` modules on `localhost`, i.e. the Ansible control node, by setting the target host to localhost.
Running the CMCI modules on the control node can be a good idea, because you don't have to deal with the complexity of
an unnecessary SSH connection, and you don't have to install the modules' dependencies on the remote host.

The `cmci_*` modules have pre-requisites that need to be installed into the Python environment in which the module
executes. In this case, all `cmci_` modules will be executed on `localhost`, i.e. the Ansible control
node. The playbook demonstrates how you can ensure the pre-requisites are installed (wherever the module runs) before
the initial `cmci_get` module is executed. More information about the `cmci_*` module pre-requisites can be found in the
[documentation](https://ibm.github.io/z_ansible_collections_doc/ibm_zos_cics/docs/source/requirements_managed.html).

## Run [program_lifecycle.yml](program_lifecycle.yml)

You can run the playbook without modification:

```bash
ansible-playbook program_lifecycle.yml
````

The playbook will prompt for required parameters. After parameters have been supplied, the playbook installs the
CMCI module dependencies to the python environment. The playbook then creates and checks CICS resources before deleting them. Specifically, it creates and updates a program definition, installs it and checks the program is installed, and finally disables the program and deletes it along with the program definition.

The `module_defaults` section at the top of the playbook allows some parameters to be passed to a group, in this case the group `group/ibm.ibm_zos_cics.cmci_group` so any modules that are part of that group inherit those parameters. This means the rest of the playbook's `cmci_group` tasks can omit CMCI connection information as they will get that info from the module_defaults.

## What next?

- To avoid being prompted for parameters, you can try supplying the input parameters on the command line directly:

  ```bash
  ansible-playbook -e "context=MYCTXT cmci_host=example.com" program_lifecycle.yml
  ```
  
  Parameters specified in this way won't be prompted for. Have a look at the `vars_prompt` section of the playbook to
  work out the names of the variables to use on the CLI.

  You can also edit the playbook to switch the `vars_prompt` for a `vars` section, to stop Ansible prompting for the
  values when executing the playbook. You will still be able to override any default values you set with the `-e`
  command line argument. For information about how to set variables, see
  [the Ansible documentation](https://docs.ansible.com/ansible/latest/user_guide/playbooks_variables.html).

- Look at the [other samples](../..) to find examples of what else you can do with the CICS collection.

# Support

Please refer to the [support section](../../../../README.md/#support) for more details.

# License

Licensed under [Apache License, Version 2.0](https://opensource.org/licenses/Apache-2.0).

# Copyright

© Copyright IBM Corporation 2023.
