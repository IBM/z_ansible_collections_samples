# CICS Provisioning

The CICS provisioning playbook samples demonstrate how to configure and allocate the required data sets to provision and start a CICS region (with or without SMSS support), using the [latest Beta](https://galaxy.ansible.com/ui/repo/published/ibm/ibm_zos_cics/?version=1.1.0-beta.5) of the CICS Ansible collection. The deprovisioning sample, shows how to stop a running region and delete all the associated data sets.

It is a recommended you review the playbook contents and the [Beta documentation](https://galaxy.ansible.com/ui/repo/published/ibm/ibm_zos_cics/docs/?version=1.1.0-beta.5) before executing them.
 

## Playbook Summary

[full_provision.yml](./full_provision.yml) contains an Ansible playbook that can create all the required data sets and start a CICS region. Specify the `applid` using the vars at the top of the playbook, and check the CICS data sets in module defaults and start match those in your system. For more information on the different overrides and options available, see the [Beta documentation](https://galaxy.ansible.com/ui/repo/published/ibm/ibm_zos_cics/docs/?version=1.1.0-beta.5) 

[full_provision_smss.yml](./full_provision_smss.yml) contains an Ansible playbook that can create all the required data sets and start a CICS region with CMCI support. Specify the applid and port using the vars at the top of the playbook and check the CICS data sets in module defaults and start match those in your system. For more information on the different overrides and options available, see the [Beta documentation](https://galaxy.ansible.com/ui/repo/published/ibm/ibm_zos_cics/docs/?version=1.1.0-beta.5) 

[deprovision.yml](./deprovision.yml) contains an Ansible playbook that stops a running CICS region and removes all its data sets. Specify the `applid` and `job_id` (as output by the provisioning script) belonging to the running CICS region using the vars at the top of the playbook, and check the CICS data sets in module defaults match those in your provisioning playbook. For more information on the different overrides and options available, see the [Beta documentation](https://galaxy.ansible.com/ui/repo/published/ibm/ibm_zos_cics/docs/?version=1.1.0-beta.5) 

[inventory.yml](./inventories/inventory.yml) contains an ansible inventory for your provisioning. Change the ansible_host to your own LPAR/system to provision a CICS region on. It also contains the user which Ansible connects over SSH to that lpar as. Therefore change this to the user you wish to connect as, and ensure you have already setup ssh key authentication for this user on the machine you run the `ansible-playbook` command from.

[variables.yml](./host_vars/variables.yml) contains the required environment variables needed by the z/OS Ansible collections (including the CICS one) to execute the ansible payload on z/OS Unix. They also include the path to the Python and ZOAU installations on z/OS Unix


## Requirements
- Python 3.9 or later
- Ansible 2.15 or later
- ZOAU 1.3 or later
- IBM z/OS Core Ansible collection 1.9.0 or later
- IBM z/OS CICS Ansible collection 1.1.0-beta.5



## Getting Started

Before running the playbooks you must:

- Ensure you have Ansible installed already locally
- Ensure you have Python and [ZOAU](https://www.ibm.com/docs/en/zoau/1.3.x?topic=installing-zoau) installed on z/OS
- Install the latest beta version of the [CICS ansible collection](https://galaxy.ansible.com/ui/repo/published/ibm/ibm_zos_cics/)
- Install the latest version of the [z/OS Core collection](https://galaxy.ansible.com/ui/repo/published/ibm/ibm_zos_core/)
- Ensure all steps you normally undertake before provisioning a CICS region, such as setting up RACF permissions and activating VTAM nodes for the region have been completed
- Update the `inventories/inventory.yml` and `host_var/variables.yml` to match the values for your system

To run the playbook to fully provision a standalone CICS region, use the following from this directory:
```bash
ansible-playbook full_provision.yml -i ./inventories/inventory.yml
```

To run the playbook to fully provision a managed standalone CICS region, use the following from this directory:
```bash
ansible-playbook full_provision_smss.yml -i ./inventories/inventory.yml
```

To run the playbook to fully deprovision a CICS region you have provisioned, use the following from this directory:
```bash
ansible-playbook deprovision.yml -i ./inventories/inventory.yml
```


## License

Licensed under [Apache License, Version 2.0](https://opensource.org/licenses/Apache-2.0).

