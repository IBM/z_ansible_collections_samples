# Restarting CICS bundles

This sample playbook demonstrates how to use the `cmci_update` and `cmci_get`
modules from the `ibm_zos_cics` collection to restart a set of CICS bundles.

The sample shows a mechanism of disabling, waiting for these asynchronous
resources to reach a disabled state, enabling and again waiting for them to
reach an enabled state.

This sample additionally shows how to automate installation of pre-requisites
for the `cmci_*` modules.

## Requirements

- Python 2.7+
- Ansible 2.9+

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
run the `cmci_get` module on `localhost`, i.e. the Ansible control node, by setting the target host to localhost.
Running the CMCI modules on the control node can be a good idea, because you don't have to deal with the complexity of
an unnecessary SSH connection, and you don't have to install the modules' dependencies on the remote host.

The `cmci_*` modules have pre-requisites that need to be installed into the Python environment in which the module
executes. In this case, the `cmci_get` module will be executed on `localhost`, i.e. the Ansible control
node. The playbook demonstrates how you can ensure the pre-requisites are installed (wherever the module runs) before
the `cmci_get` module is executed. More information about the `cmci_*` module pre-requisites can be found in the
[documentation](https://ibm.github.io/z_ansible_collections_doc/ibm_zos_cics/docs/source/requirements_managed.html).

## Run [restart_bundles.yml](restart_bundles.yml)

You can run the playbook without modification:

```bash
ansible-playbook restart_bundles.yml
````

The playbook will prompt for required parameters. After parameters have been supplied, the playbook installs the
CMCI module dependencies to the python environment. The playbook makes a series of CMCI GET and PUT requests to
regions in the supplied CICSPlex SM context.

## What next?

- To avoid being prompted for parameters, you can try supplying the input parameters on the command line directly:

  ```bash
  ansible-playbook -e "context=MYCTXT cmci_host=example.com" restart_bundles.yml
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
