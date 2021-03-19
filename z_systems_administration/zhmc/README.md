# Samples for the IBM Z HMC Collection

The [IBM Z HMC Collection](https://github.com/zhmcclient/zhmc-ansible-modules)
provides sample playbooks.

## Downloading the sample playbooks

Follow the instructions in
[using the samples repository](../meta/samples_repository/README.md)
to clone this Git repository to your local system.

The sample playbooks for the IBM Z HMC Collection are located in the
directory ``zhmc/playbooks`` of the cloned repository.

## Creating a vault file

The sample playbooks use a vault file named ``vault.yml`` that defines
variables that are used in the sample playbooks.

In order to ensure that the vault file used by developers of this repository
is not stored in the repository by mistake, the ``vault.yml`` file is excluded
from Git, and the playbook directory contains an example copy of that file under
the name:

* ``vault_example.yml``

To use the sample playbooks for the IBM Z HMC Collection, you need to copy that
file to ``vault.yml`` and adjust the content of the copy as needed.

## Running the playbooks

To run any of the sample playbooks, it is recommended to first look at them and
to adjust any parameters as documented in the playbook files.

You use the
[ansible-playbook](https://docs.ansible.com/ansible/latest/cli/ansible-playbook.html)
command to run playbooks.

## Sample playbooks

The sample playbooks whose filename starts with ``module_`` are very simple
playbooks that demonstrate the use of a single module.

The sample playbooks whose filename starts with ``usecase_`` are more complex
and implement a particular use case.

* [Single module playbooks](docs/module_playbooks.md)
* [Use case playbooks](docs/usecase_playbooks.md)
