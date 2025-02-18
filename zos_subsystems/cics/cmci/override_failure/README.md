# Customising when a CMCI module should fail

This sample playbook demonstrates how to override the default failure criteria
of the CMCI modules.

In this sample, the `cmci_get` module from the `ibm_zos_cics` collection is used
to retrieve a `PROGRAM` from a specified CICS region. In the event that the
module fails because the program doesn't exist, this failure is ignored, and any
subsequent tasks are allowed to proceed. Failures resulting from other conditions
will still cause the playbook to fail.

Try running this sample with the name of a `PROGRAM` that does exist, and one that
doesn't, to see how the `failed_when` criteria prevents module failure.

This sample additionally shows how to automate installation of
pre-requisites for the `cmci_*` modules.

## Requirements

- Python 2.7+
- Ansible 2.9+

## Getting Started

You will need to have set up the CMCI REST API in your CICS environment. You
can enable the CMCI REST API in either CICSPlex SM environments, or in
stand-alone CICS regions. The `cmci_*` modules use the *CMCI REST API* to
interact with your CICS environment. To use the `cmci_*` modules you
will need to have set up the CMCI REST API in your CICS environment. You can
enable the CMCI REST API in either CICSplex SM environments, or in stand-alone
CICS regions.

For detailed installation instructions please consult
[the documentation](https://ibm.github.io/z_ansible_collections_doc/installation/installation.html).

You can install the IBM z/OS CICS collection from Ansible Galaxy by using the
`ansible-galaxy` CLI, which is supplied with your Ansible installation:

```bash
ansible-galaxy collection install ibm.ibm_zos_cics
```

For more information about the CMCI REST API, see the
[CMCI overview in the CICS TS documentation](https://www.ibm.com/docs/en/cics-ts/latest?topic=fundamentals-cics-management-client-interface-cmci).

Because this playbook only uses the CMCI REST API, it can be run on the control
node directly, without having to configure an inventory. Generally you'll be
able to use this trick with any of the CMCI modules. In this example, we run
the `cmci_get` module on `localhost`, i.e. the Ansible control node, by setting
the target host to localhost. Running the CMCI modules on the control node can
be a good idea, because you don't have to deal with the complexity of an
unnecessary SSH connection, and you don't have to install the modules'
dependencies on the remote host.

The `cmci_*` modules have pre-requisites that need to be installed into the
Python environment in which the module executes. In this case, the `cmci_get`
module will be executed on `localhost`, i.e. the Ansible control node.
The playbook demonstrates how you can ensure the pre-requisites are installed
(wherever the module runs) before the `cmci_get` module is executed. More
information about the `cmci_*` module pre-requisites can be found in the
[documentation](https://ibm.github.io/z_ansible_collections_doc/ibm_zos_cics/docs/source/requirements_managed.html).

## Run [override_failure.yml](override_failure.yml)

You can run the playbook without modification:

```bash
ansible-playbook override_failure.yml
````

The playbook will prompt for required parameters. After parameters have been
supplied, the playbook installs the CMCI module dependencies to the
python environment. The playbook makes a CMCI GET request to inquire about
`PROGRAM` resources using the supplied name in the supplied CICSPlex SM context
and scope, and if found prints out information about them. If not found, the
playbook continues, without failing.

## How does this work?

In the playbook, the `cmci_get` module has a custom `failed_when` condition.

```yml
failed_when: >
  'cpsm_response' not in result or result.cpsm_response not in ['OK', 'NODATA']
```

Instead of using the module's usual logic as to when a response fails (such as
a non-OK HTTP response) this condition instead inspects the content of the
payload to determine whether the module has failed. By specifying that
`cpsm_response` codes of both `OK` and `NODATA` are OK, the playbook ignores
failures due to finding no programs.

## What next?

- Look at the [other samples](../..) to find examples of what else you can do with the CICS collection.

# Support

Please refer to the [support section](../../../../README.md/#support) for more details.

# License

Licensed under [Apache License, Version 2.0](https://opensource.org/licenses/Apache-2.0).

# Copyright

© Copyright IBM Corporation 2021.
