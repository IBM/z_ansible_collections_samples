# Ansible Playbooks for Different Use Cases

The playbooks in this directory cover multiple use cases that might arise when
working with ACC and appliances.

## Preparation

- Ensure that ACC and the appliances under consideration are activated and
  working as expected.
- Both ACC-admin and appliance-owners must
  [install ansible](https://docs.ansible.com/ansible/latest/installation_guide/intro_installation.html) on
  their respective control nodes.
  - For example, on your laptop that will connect with ACC, install `ansible` via:
    ```bash
    pip install ansible
    ```
- Download all these `yaml` files to a directory on the control node (e.g., a
  laptop) which will connect with ACC.

## Upgrade or Concurrently Updating the Appliances

An appliance can be upgraded (wiping the complete disk and replacing it with the
new image) or concurrently updated by the appliance-owner. For that, perform the
following actions as appliance-owner.

- `cd` to the directory `other_usecases_ansible`.
- Download the appliance image you want to install to your control node (e.g., 
  your laptop).
- Modify the variables in the file `owner_vars.yaml`.
  - Change the `acc_ip` in the `owner_vars.yaml` file to point to the right
    IP address.
    - Currently, it is set to a dummy IP 9.152.150.224, and port 8081.
    - Change this IP to the one that should be serving ACC APIs.
- Modify the location of that image file for upgrade in the `owner_vars.yaml` file.
  - Specifically, change the `image_path` variable.
  - This image is used for both upgrade and concurrent update.
  - The variable `image_type` denotes the kind of update:
    - Set the `image_type` variable to to `image` for upgrade.
      - Run the playbook for appliance upgrade:
        ```bash
        ansible-playbook 01_upgrade_flow.yaml
        ```
    - Set the `image_type` variable to `fix` for concurrent update.
      - Run the playbook for appliance concurrent :
        ```bash
        ansible-playbook 04_managed_appliance_update.yaml
        ```

## Pull Logs from ACC and other Appliances

- `cd` to the directory `other_usecases_ansible`.
- Run the playbook to get logs from ACC, decryptable only by IBM:
  ```bash
  ansible-playbook 03_pull_scc_logs.yaml
  ```

This playbook with interactively ask for the IP, username and password of the
appliance from which logs are gathered. This means that this playbook can also be
used for gathering logs from other appliances (e.g., SSA appliance).


## ACC Appliance Update

- `cd` to the directory `other_usecases_ansible`.
- Run the playbook to update the ACC appliance:
  ```bash
  ansible-playbook 06_acc_appliance_update.yaml
  ```

## Pull SSA logs and Check Health Status

- Run the playbook, for health checking and pulling SSA logs:
  ```bash
  ansible-playbook 05_managed_appliance_health_and_pull_logs.yaml
  ```

## SSA Installation (End-to-End)

To install 2 SSAs after a fresh install of ACC, you should:

- `cd` to the directory `other_usecases_ansible`.
- Export your HMC username and password on a terminal in your control node
  (laptop):
  ```bash
  export HMC_USER=<enter_HMC_username>
  export HMC_PASSWORD=<enter_HMC_password>
  ```
- Download and store the SSA installation image to your control node.
- Modify the `env_vars.yaml` file.
  - Modify the variables for the LPAR that appliance-owner will use for installing
    the appliance.
- Modify the `07_ssa_install_e2e.yaml` playbook if required. For example:
  - Check if you have to remove a task in the playbook (e.g.,
    updating the password).
  - Check if you use FCP disk instead of a dasd. This means you have to
    modify the task `Assign resources to the SSA owner`.
  - Check if you use FIDs instead of `chpid`. This means you have to modify the
    task `Assign resources to the SSA owner`.
  - If your setup uses a `vlan_id`, ensure that the variable is enabled in the configuration:
        - Uncomment the `vlan_id` entry in `env_vars.yaml`.
        - Modify the task `Assign resources to the SSA owner` by uncommenting the `vlan_id` reference in that task.

- Run the playbook to install SSA via:
  ```bash
  ansible-playbook 07_ssa_install_e2e.yaml
  ```

This playbook will set up ACC, upload the images, initiate the install and then
check the status of the install. The installation itself can take more than 15
mins. Check the status of installation on HMC.

## ACC and 2-SSAs Installation Sanity-Check

If you have installed ACC and 2-SSAs, you can do a quick check to see if the
appliances are functional. For this purpose:

- `cd` to the directory `other_usecases_ansible`.
- Modify the file `acc_ssa_install_check_vars.yaml`.
- Run the playbook to check the sanity of the installation via:
  ```bash
  ansible-playbook 08_acc_ssa_install_check.yaml
  ```

## Set HMC Credentials (Daily Task)

The administrator must run the following Ansible playbook once every day to refresh and set the HMC credentials:
  ```
    ansible-playbook 09_insert_hmc_creds.yaml
  ```

## Restart ACC

If the ACC admin wants to restart ACC, then `admin` can use this playbook:

```
ansible-playbook 10_restart_acc.yaml
```

This playbook will help user to restart ACC.
Note: This playbook will not deactivate and then activate the ACC LPAR. It will just send a restart signal to ACC appliance (similar to `reboot`).

## Trigger Disruptive dumps and Collect Logs from Appliance

This playbook automates the process of triggering a disruptive dump on an SSC appliance and collecting diagnostic logs after the reboot.
- `cd` to the directory `other_usecases_ansible`.
- Run the playbook `11_get_disruptive_dumps.yaml`.

  ```bash
  ansible-playbook 11_get_disruptive_dumps.yaml
  ```

This playbook with interactively ask for the IP, username, password, reason for downloading dumps and file path where to download for gathering logs from any appliances (e.g., SSA appliance).

## Unlock Appliances

This playbook allows the ACC Owner to manually unlock one or more locked appliances in their resource package.
Each appliance will be processed one at a time, prompting the user for credentials specific to that appliance.
Run this playbook as the ACC Owner.
The file `13_unlock_each_appliance.yaml` must be available in the same directory.
This file is automatically included by the main playbook (`12_unlock_appliances.yaml`) for each appliance.

```
ansible-playbook 12_unlock_appliances.yaml
```