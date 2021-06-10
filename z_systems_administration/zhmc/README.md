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
HMC authentication data and other variables that are used in the sample playbooks.

In order to ensure that the vault file used by developers of this repository
is not stored in the repository by mistake, the ``vault.yml`` file is excluded
from Git, and the playbook directory contains an example copy of that file under
the name:

* ``vault_example.yml``

To use the sample playbooks for the IBM Z HMC Collection, you need to copy that
file to ``vault.yml`` and adjust the content of the copy as needed.

## Creating an inventory file

The sample playbooks are designed to run with a certain sample host group in
order to show how running against multiple HMCs can be achieved. That requires
using an inventory file that defines that host group.

In order to ensure that the inventory file used by developers of this repository
is not stored in the repository by mistake, the ``inventory.yml`` file is excluded
from Git, and the playbook directory contains an example copy of that file under
the name:

* ``inventory_example.yml``

To use the sample playbooks for the IBM Z HMC Collection, you need to copy that
file to ``inventory.yml`` and adjust the content of the copy as needed.

## Running the playbooks

To run any of the sample playbooks, it is recommended to first look at them and
to adjust any parameters as documented in the playbook files.

You use the
[ansible-playbook](https://docs.ansible.com/ansible/latest/cli/ansible-playbook.html)
command to run playbooks.

Since the sample playbooks are designed to run with a certain sample host group,
the inventory file needs to be specified in the command, e.g.:

```
$ ansible-playbook -i inventory.yml playbook.yml
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

There are multiple ways how this can be achieved in Ansible. The sample playbooks
are designed to use delegation to localhost in the zhmc tasks. The key elements
in that design are:

* There is an inventory file that specifies a host group with the HMCs to target,
  where each HMC has its IP addresses (or hostnames) defined and specifies that
  the connection to that HMC is local, e.g.:

  File ``inventory.yml``:

  ```
  my_hmc_list:    # name of the host group
    hosts:
      myhmc13:   # Ansible name for the HMC
        ansible_host: 10.11.12.13    # IP address or hostname of the HMC
        ansible_connection: local    # connection is local (for this host, on all playbook tasks)
  ```

* There is a vault file that specifies the authentication data for each HMC,
  using the same Ansible names for the HMCs as defined in the inventory file,
  e.g.:

  File ``vault.yml``:

  ```
  hmc_auth:
    myhmc13:   # Ansible name for the HMC
      userid: myuserid
      password: mypassword
      verify: false
  ```

* The playbook references the host group defined in the inventory file,
  loads the variables from the vault file, delegates each zhmc task to localhost,
  and uses magic Ansible variables for obtaining the HMC IP address and
  HMC authentication data, e.g.:

  File ``module_facts_cpc.yml``:

  ```
  - hosts:
      - my_hmc_list    # references the host group defined in the inventory file
    vars_files:
      - vault.yml    # loads the variables from the vault file
    vars:
      cpc_name: CPCA
    collections:
      - ibm.ibm_zhmc
    tasks:
      - name: "Get facts for CPC {{ cpc_name }}"
        delegate_to: localhost    # delegates this task to run on localhost
        zhmc_cpc:
          hmc_host: "{{ ansible_host }}"    # uses Ansible magic variable for HMC IP address
          hmc_auth: "{{ hmc_auth[inventory_hostname] }}"    # uses Ansible magic variable for HMC auth.data
          name: "{{ cpc_name }}"
          state: facts
  ```

The examples above show how the sample playbooks are designed. There are
alternative ways to define the HMC hosts, the HMC authentication data, and
to ensure that the Ansible tasks run locally on the control node.

One alternative approach is not to use an Ansible inventory and to define
only a single HMC to work with, and local connection for all tasks of the
playbook:

* The vault file specifies the IP address / hostname of the HMC and the
  authentication data for the HMC, e.g.:

  File ``alternative_vault.yml``:

  ```
  hmc_host: 10.11.12.13
  hmc_auth:
    userid: myuserid
    password: mypassword
    verify: false
  ```

* The playbook defines only localhost as a host, and defines the connection
  to be local for the entire playbook. Non-zhmc tasks that need to run on
  a specific other host can still do that by using the Ansible ``delegate_to``
  attribute, but the host will be a single host and not defined by an inventory.

  The playbook uses variables defined in the vault file for obtaining the HMC
  IP address and HMC authentication data, e.g.:

  File ``alternative_module_facts_cpc.yml``:

  ```
  - hosts: localhost    # runs all tasks on localhost
    connection: local   # connection is local for all tasks
    vars_files:
      - alternative_vault.yml    # loads the variables from the vault file
    vars:
      cpc_name: CPCA
    collections:
      - ibm.ibm_zhmc
    tasks:
      - name: "Get facts for CPC {{ cpc_name }}"
        zhmc_cpc:
          hmc_host: "{{ hmc_host }}"    # uses variable from vault file
          hmc_auth: "{{ hmc_auth }}"    # uses variable from vault file
          name: "{{ cpc_name }}"
          state: facts
  ```
