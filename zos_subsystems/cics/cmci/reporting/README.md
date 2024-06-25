# Retrieving operational data from running CICS regions

This sample playbook demonstrates how to use the `cmci_get` module from the
`ibm_zos_cics` collection to retrieve operational data from running CICS
regions.

This example retrieves information corresponding to the `CICSRGN`
resource table, but can be adapted to retrieve information about any of the
resource tables supported by CICSPlex SM. The retrieved information is written
to a CSV file, which you can open as a spreadsheet.

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
[CMCI overview in the CICS TS documentation](https://www.ibm.com/docs/en/cics-ts/5.6?topic=environment-cics-management-client-interface-cmci).

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

## Run [report.yml](report.yml)

You can run the playbook without modification:

```bash
ansible-playbook report.yml
```

The playbook will prompt for required parameters. After parameters have been supplied, the playbook installs the
CMCI module dependencies to the python environment. The playbook makes a CMCI GET request to regions in the supplied
CICSPlex SM context, and writes the file `report.csv` to the current directory with a report on a subset of
attributes for each region. You can open this file in a text editor, or as a spreadsheet to look at
the results of the report.

## What next?

- To avoid being prompted for parameters, you can try supplying the input parameters on the command line directly:

  ```bash
  ansible-playbook -e "context=MYCTXT cmci_host=example.com" report.yml
  ```
  
  Parameters specified in this way won't be prompted for. Have a look at the `vars_prompt` section of the playbook to
  work out the names of the variables to use on the CLI.

  You can also edit the playbook to switch the `vars_prompt` for a `vars` section, to stop Ansible prompting for the
  values when executing the playbook. You will still be able to override any default values you set with the `-e`
  command line argument. For information about how to set variables, see
  [the Ansible documentation](https://docs.ansible.com/ansible/latest/user_guide/playbooks_variables.html).

- Try editing `report.yml` to change the attributes included in the report.
  
  You may want to uncomment the debug task to see the full response from CICSPlex SM, which includes all the attribute
  names applicable to the target resource. In this example, the returned attributes will correspond to what's listed in the [CICSRGN resource table](https://www.ibm.com/docs/en/cics-ts/5.6?topic=tables-cicsrgn-resource-table). 
  
- Try adding a `filter` argument to the `cmci_get` task, to restrict the report to a subset of your CICS regions. For
  information on how to set a filter for the `cmci_get` task, see [the IBM z/OS CICS modules documentation](https://ibm.github.io/z_ansible_collections_doc/ibm_zos_cics/docs/source/modules/cmci_get.html).
  
- Try supplying a different resource for the `type` argument of the `cmci_get` task, to request attributes for a different type of resource. You can find
  available CMCI resource names at
  [CMCI resource names](https://www.ibm.com/docs/en/cics-ts/5.6?topic=reference-cmci-resource-names).

- Look at the [other samples](../..) to find examples of what else you can do with the CICS collection.

# Support

Please refer to the [support section](../../../../README.md/#support) for more details.

# License

Licensed under [Apache License, Version 2.0](https://opensource.org/licenses/Apache-2.0).

# Copyright

© Copyright IBM Corporation 2021.
