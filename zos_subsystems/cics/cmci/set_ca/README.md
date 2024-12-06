# Retrieving operational data from running CICS regions

This sample playbook demonstrates how to use the `cmci_get` module from the
`ibm_zos_cics` collection when using an HTTPS connection to a server that is
using a certificate issued by a self-signed CA.

This example retrieves information corresponding to the `CICSPLX`
resource table, but can be adapted to retrieve information about any of the
resource tables supported by CICSPlex SM.  The CICSplex names are printed to
the log.

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

To run this playbook, you will need to have local access to the CA certificate in
PEM format.  Extract your certificate and convert it to PEM format, saving it along
side the playbook, with the name `ca_certs.pem`.

For more information about the CMCI REST API, see the
[CMCI overview in the CICS TS documentation](https://www.ibm.com/docs/en/cics-ts/latest?topic=fundamentals-cics-management-client-interface-cmci).

Because this playbook requires local access to the `ca_certs.pem` file, and only uses
the CMCI REST API, it can be run on the control node directly, without having to
configure an inventory. Generally you'll be able to use this technique with any of the
CMCI modules. In this example, we run the `cmci_get` module on `localhost`, i.e. the
Ansible control node, by setting the target host to localhost. Running the CMCI modules
on the control node can be a good idea, because you don't have to deal with the complexity
of an unnecessary SSH connection, and you don't have to install the modules' dependencies
on the remote host.

The `cmci_*` modules have pre-requisites that need to be installed into the Python
environment in which the module executes. In this case, the `cmci_get` module will be
executed on `localhost`, i.e. the Ansible control node. The playbook demonstrates how
you can ensure the pre-requisites are installed (wherever the module runs) before
the `cmci_get` module is executed. More information about the `cmci_*` module
pre-requisites can be found in the
[documentation](https://ibm.github.io/z_ansible_collections_doc/ibm_zos_cics/docs/source/requirements_managed.html).

## Run [set_ca.yml](set_ca.yml)

You can run the playbook without modification, once you have prepared your
`ca_certs.pem` file:

```bash
ansible-playbook set_ca.yml
````

The playbook will prompt for required parameters. After parameters have been supplied,
the playbook installs the CMCI module dependencies to the python environment. The
playbook makes a CMCI GET request to retrieve information about CICSplexes, and
writes the CICSplex names to the log.

## What next?

- Look at the [other samples](../..) to find examples of what else you can do with the CICS collection.

# Support

Please refer to the [support section](../../../../README.md/#support) for more details.

# License

Licensed under [Apache License, Version 2.0](https://opensource.org/licenses/Apache-2.0).

# Copyright

© Copyright IBM Corporation 2022.
