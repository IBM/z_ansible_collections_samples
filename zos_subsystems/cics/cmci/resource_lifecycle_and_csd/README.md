# Resource lifecycle and CSD

CMCI's access to CSD is through a named CICS region, so in addition to
supplying the CMCI connection details as parameters, you will also need to
supply a context and scope which identifies a CICS region that's using the
appropriate CSD. You'll also be prompted for the name of a CSD group to use
temporarily. The created definition will be deleted at the end of the
sample. The sample will create a `PROGDEF` in the named CSD group,
update the definition, install it into the target CICS region, disable the
`PROGRAM`, discard the `PROGRAM`, and delete `PROGDEF`, leaving the system
in the same state as it started.

Also of note, install is an asynchronous action in CMCI. This playbook also
demonstrates how to use Ansible to wait until the install has completed
successfully before proceeding.

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
the `cmci_*` modules on `localhost`, i.e. the Ansible control node, by delegating
module execution to localhost. Running the CMCI modules on the control node can
be a good idea, because you don't have to deal with the complexity of an
unnecessary SSH connection, and you don't have to install the modules'
dependencies on the remote host.

The `cmci_*` modules have pre-requisites that need to be installed into the
Python environment in which the module executes. In this case, the `cmci_*`
modules will be executed on `localhost`, i.e. the Ansible control node.
The playbook demonstrates how you can ensure the pre-requisites are installed
(wherever the module runs) before the `cmci_*` modules are executed. More
information about the `cmci_*` module pre-requisites can be found in the
[documentation](https://ibm.github.io/z_ansible_collections_doc/ibm_zos_cics/docs/source/requirements_managed.html).

## Run [resource_lifecycle_and_csd.yml](resource_lifecycle_and_csd.yml)

You can run the playbook without modification:

```bash
ansible-playbook resource_lifecycle_and_csd.yml
````

## Highlights

The playbook shows which parameters you need to set to create definitions in CSD,
update definitions in CSD, install definitions from CSD and delete definitions
from CSD. Of note is that when creating you must specify that you'd like to
create the definition in CSD in addition to supplying a scope:

```yaml
create_parameters:
  - name: 'CSD'
```

When working with that definition, e.g. for installing, in addition to
identifying the resource by name, you also need to specify which `CSDGROUP` the
definition is in. For example from installing the definition:

```yaml
resources:
  filter:
    NAME: '{{ program }}'
  get_parameters:
    - name: 'CSDGROUP'
      value: '{{ csdgroup }}'
```

Also of note is that when installing definitions from CSD, the correct action
name to use is `CSDINSTALL`:

```yaml
ibm.ibm_zos_cics.cmci_action:
  action_name: 'CSDINSTALL'
```

You can cross-reference this with the documentation for the
[`PROGDEF` resource table](https://www.ibm.com/docs/en/cics-ts/latest?topic=tables-progdef-resource-table).

# Support

Please refer to the [support section](../../../../README.md/#support) for more details.

# License

Licensed under [Apache License, Version 2.0](https://opensource.org/licenses/Apache-2.0).

# Copyright

© Copyright IBM Corporation 2021.
