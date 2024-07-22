# Samples for the IBM Z HMC Collection

The [IBM Z HMC Collection](https://github.com/zhmcclient/zhmc-ansible-modules)
provides sample playbooks.

## Downloading the sample playbooks

Follow the instructions in
[using the samples repository](../../meta/samples_repository/README.md)
to clone this Git repository to your local system.

The sample playbooks for the IBM Z HMC Collection are located in the
directory ``z_systems_administration/zhmc/playbooks`` of the cloned repository.

The sample playbooks make use of an inventory file and a vault file that are
briefly described in the next sections.

## Creating an inventory file

The sample playbooks of the IBM Z HMC Collection are designed to run with an
Ansible inventory file that specifies the HMCs to run against.

The format of Ansible inventory files is described in the Ansible documentation
in section
[Inventory basics: formats, hosts, and groups](https://docs.ansible.com/ansible/latest/inventory_guide/intro_inventory.html#inventory-basics-formats-hosts-and-groups) and allows
many flexibilities.

The following data structure in the inventory file is sufficient for purposes
of the sample playbooks:

```
---
default:  # A group nickname

  hosts:

    myhmc13:  # An HMC nickname (must match the nickname in the vault file)
      ansible_host: 10.11.12.13

    # ... more HMC entries for this group, as needed

# ... more groups, as needed
```

The sample playbooks by default run against the HMCs defined in the group named
``default``. They can run against any other group or single HMC by setting the
``hmc`` variable to the nickname of the group or HMC. This can be done for
example by using the ``-e hmc=nickname`` option of the ``ansible-playbook``
command.

In order to ensure that the inventory file used by developers of this repository
is not stored in the repository by mistake, the ``inventory.yml`` file is excluded
from Git, and the playbook directory contains an example copy of that file under
the name:

* ``inventory_example.yml``

To use the sample playbooks for the IBM Z HMC Collection, you need to copy that
example inventory file to e.g. ``inventory.yml`` and and adjust its content to
your HMC environment.

## Creating a vault file

The sample playbooks of the IBM Z HMC Collection use a vault file named
``vault.yml`` that defines credentials for the HMCs you want to use the sample
playbooks with.

From an Ansible perspective, the vault file is just a YAML file with arbitrary
content that is included into the playbooks with the ``vars_files`` directive.

The sample playbooks assume the following data structure in the vault file:

```
---
hmc_auth:

  myhmc13:  # An HMC nickname (must match the nickname in the inventory file)
    userid: myuserid
    password: mypassword
    verify: false   # optional, only if you need to disable certificate validation

  # ... more HMC entries, as needed
```

The vault file can store multiple entries for HMCs that are identified with a
nickname. Each HMC entry specifies HMC userid and password to use. Optionally,
validation of the server certificate can be disabled using ``verify: false``.

In order to ensure that the vault file used by developers of this repository
is not stored in the repository by mistake, the ``vault.yml`` file is excluded
from Git, and the playbook directory contains an example vault file:

* ``vault_example.yml``

To use the sample playbooks for the IBM Z HMC Collection, you need to copy that
example vault file to ``vault.yml`` and adjust its content to your HMC
environment.

You can encrypt the vault file using the ``ansible-vault`` command, and if you
do, you either need to specify the ``--ask-vault-password`` option when
running the playbooks using the ``ansible-playbook`` command, or use a vault
password file (see Ansible documentation).

## Running the playbooks

To run any of the sample playbooks of the IBM Z HMC Collection, it is
recommended to first look at them and to adjust any parameters as documented in
the playbook files.

You use the
[ansible-playbook](https://docs.ansible.com/ansible/latest/cli/ansible-playbook.html)
command to run playbooks:

```
$ ansible-playbook -i inventory.yml playbook.yml
```

You can set variables or override variables defined in the playbook by using
the ``-e name=value`` option. For example, the following command sets the ``hmc``
variable to an HMC nickname ``myhmc42`` defined in the inventory file (and vault
file) to run the playbook against that specific HMC:

```
$ ansible-playbook -i inventory.yml -e hmc=myhmc42 playbook.yml
```

If you have encrypted the vault file, you can have Ansible prompt for the
vault password by using the ``--ask-vault-password`` option:

```
$ ansible-playbook -i inventory.yml --ask-vault-password playbook.yml
```

## Sample playbooks

The sample playbooks whose filename starts with ``module_`` are very simple
playbooks that demonstrate the use of a single module.

The sample playbooks whose filename starts with ``usecase_`` are more complex
and implement a particular use case.

* [Single module playbooks](docs/module_playbooks.md)
* [Use case playbooks](docs/usecase_playbooks.md)

## Details on Ansible connection to HMC

Since the HMC does not provide an SSH login but only a remotely accessible WS API,
Ansible must be configured to run the HMC related tasks of the playbook on the
control node (= local host), from where the zhmc modules then connect to the HMC
based on their input parameters.

The sample playbooks are designed to use delegation to localhost in the zhmc
tasks, for example:

```
- . . .
  tasks:
    - name: "Get facts for CPC {{ cpc }}"
      delegate_to: localhost                # <-- delegates this task to run on localhost
      zhmc_cpc:
        hmc_host: "{{ ansible_host }}"
        hmc_auth: "{{ hmc_auth[inventory_hostname] }}"
        name: "{{ cpc }}"
        state: facts
```
