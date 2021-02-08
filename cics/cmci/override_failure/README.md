# Retrieving operational data from running CICS regions

This sample playbook demonstrates how to override the default failure criteria
of the CMCI modules.  In this sample, the `cmci_get` module from the
`ibm_zos_cics` collection is used to retrieve a `PROGRAM` from a specified
CICS region.  In the event that the module fails because the program doesn't
exist, this failure is ignored, and any subsequent tasks are allowed to
proceed. Failures resulting from other conditions will still cause the playbook
to fail.  Try running this sample with the name of a `PROGRAM` that does exist,
and one that doesn't, to see how the `failed_when` criteria prevents module
failure.

This sample additionally shows how to automate installation of
pre-requisites for the `cmci_*` modules.

## Requirements
   - Python 2.7+
   - Ansible 2.9+
   - IBM z/OS CICS Ansible collection 1.0.0+

## Getting Started
You will need to have set up the CMCI REST API in your CICS environment. You
can enable the CMCI REST API in either CICSPlex SM environments, or in
independent CICS regions. The `cmci_*` modules use the *CMCI REST API* to
interact with your CICS environment. To use the `cmci_*` modules you
will need to have set up the CMCI REST API in your CICS environment. You can
enable the CMCI REST API in either CICSplex SM environments, or in independent
CICS regions.

For detailed installation instructions please consult
[the documentation](https://ibm.github.io/z_ansible_collections_doc/installation/installation.html).

You can install the IBM z/OS CICS collection from Ansible Galaxy by using the
`ansible-galaxy` CLI, which is supplied with your Ansible installation:

```bash
ansible-galaxy collection install ibm.ibm_zos_cics
```

For more information about the CMCI REST API, see the
[CMCI overview in the CICS TS documentation](https://www.ibm.com/support/knowledgecenter/SSGMCP_5.6.0/fundamentals/cpsm/cpsm-cmci-overview.html).

Because this playbook only uses the CMCI REST API, it can be run on the control
node directly, without having to configure an inventory. Generally you'll be
able to use this trick with any of the CMCI modules. In this example, we run
the `cmci_get` module on `localhost`, i.e. the Ansible control node, by setting
the target host to localhost. Running the CMCI modules on the control node can
be a good idea, because you don't have to deal with the complexity of an
unneessary SSH connection, and you don't have to install the modules'
dependencies on the remote host.

The `cmci_*` modules have pre-requisites that need to be installed into the
Python environment in which the module executes.  In this case, the `cmci_get`
module will be executed on `localhost`, i.e. the Ansible control node.
The playbook demonstrates how you can ensure the pre-requisites are installed
(wherever the module runs) before the `cmci_get` module is executed.  More
information about the `cmci_*` module pre-requisites can be found in the
[documentation](todo)

## Run [override_failure.yml](override_failure.yml)

You can run the playbook without modification:
```bash
ansible-playbook override_failure.yml
````

The playbook will prompt for required parameters. After parameters have been
supplied, the playbook will install the CMCI module dependencies to the
python environment. A CMCI GET request will be made to regions in the
supplied CICSPlex SM context, and a file `report.csv` will be written to the
current directory with a report on a subset of attributes for each region.
You should be able to open this file in a text editor, or as a spreadsheet to
look at the results of the report.