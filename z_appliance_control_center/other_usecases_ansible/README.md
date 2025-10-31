# Ansible Playbooks for Different Use Cases

The playbooks in this directory cover multiple use cases that might arise when
working with ACC and appliances.

These playbooks can be used for:

- Pulling ACC logs
- Updating ACC image
- End-to-end installation of the appliance
- Upgrading and updating SSA image
- Pulling SSA logs and Checking health status of SSA
- Syncing LPARs
- Install checks for ACC and SSA
- Inserting HMC credentials
- Unlocking appliances
- Restarting ACC

### Note

The use cases here cover the default mode of ACC, without using MFA. If MFA
is enabled, then you will need to modify the playbooks a little:

- Add a task at the start of the playbook to ask the user for a TOTP. For example:
  ```
  - name: Wait for user to generate OTP using Owner mfa secret
    pause:
      prompt: "Enter OTP generated from the owner mfa_secret:"
    register: mfa_otp_input
  ```
- Change the token generation to also send TOTP from the previous step:
  ```
  - name: 00 - Get authentication token from the ACC as appliance-owner
    tags: owner, install
    ansible.builtin.uri:
      url: "{{ acc_ip }}/user/token"
      method: POST
      body:
        username: "{{ cc_owner_user }}"
        password: "{{ cc_owner_password }}"
        otp: "{{ mfa_otp_input.user_input }}"
      body_format: json
      return_content: true
      validate_certs: false
    register: auth_response
  ```

The rest of the playbook should be not change. For reference and example of using
MFA, check the `appliance_deploy_default_mfa_ansible/04_install_flow.yaml` playbook.

## Preparation

- Almost all the playbooks here require that ACC and the appliances under consideration are activated and
  working as expected.
- Both ACC-admin and appliance-owners must
  [install ansible](https://docs.ansible.com/ansible/latest/installation_guide/intro_installation.html) on
  their respective control nodes.
  - For example, on your laptop that will connect with ACC, install `ansible` via:
    ```bash
    pip install ansible
    ```
- Download this repository on the control node (e.g., a
  laptop) which will connect with ACC.
- The playbooks that are run by an appliance-owner require to export the owner password
  on a terminal in your control node (laptop):
  ```bash
  export ACC_OWNER_PASSWORD=<owner_password>
  ```
- `cd` to the directory `other_usecases_ansible`.
- Modify the variables in the file `owner_vars.yaml`.
  - Change the `acc_ip` in the `owner_vars.yaml` file to point to the right
    IP address. Change this IP and port to the one that should be serving ACC APIs.

## Updating the Appliances - 01_upgrade_flow.yaml | 04_managed_appliance_update.yaml

An appliance can be upgraded (wiping the complete disk and replacing it with the
new image) or concurrently updated by the appliance-owner. For that, perform the
following actions as appliance-owner.

- Download the appliance image you want to install to your control node (e.g., 
  your laptop).
- Modify the location of that image file for upgrade in the `owner_vars.yaml` file.
  - Specifically, change the `image_path` variable.
  - This image is used for both upgrade and concurrent update.
  - The variable `image_type` denotes the kind of update:
    - Set the `image_type` variable to `image` for upgrade.
      - Run the playbook for appliance upgrade:
        ```bash
        ansible-playbook 01_upgrade_flow.yaml
        ```
    - Set the `image_type` variable to `fix` for concurrent update.
      - Run the playbook for appliance concurrent :
        ```bash
        ansible-playbook 04_managed_appliance_update.yaml
        ```

## Pull Logs from ACC and other Appliances | 03_pull_scc_logs.yaml

- Run the playbook to get logs from ACC:
  ```bash
  ansible-playbook 03_pull_scc_logs.yaml
  ```

This playbook will interactively ask for the IP, username and password of the
SSC LPAR of the appliance, from which logs are gathered. This means that this playbook can also be
used for gathering logs from other appliances (e.g., SSA appliance).

## Pull SSA logs and Check Health Status | 05_managed_appliance_health_and_pull_logs.yaml

- Run the playbook for health checking and pulling appliance (like SSA) logs:
  ```bash
  ansible-playbook 05_managed_appliance_health_and_pull_logs.yaml
  ```

## ACC Appliance Update | 06_acc_appliance_update.yaml

- Run the playbook to update the ACC appliance:
  ```bash
  ansible-playbook 06_acc_appliance_update.yaml
  ```

## SSA Installation (End-to-End) | 07_ssa_install_e2e.yaml

To install 2 SSAs after a fresh install of ACC, you can run this playbook. For
running this playbook, it is expected that ACC is just installed using
`appliance_deploy_default_ansible/00_acc_install.yaml` playbook.

### Pre-requisites:

- Please ensure that all SSA LPARs are **deactivated** from the HMC before
  executing the SSA appliance installation playbook `07_ssa_install_e2e.yaml`.
- The playbooks is configured in such a way that both SSA LPARs will use the
  same credentials.
- If the playbook must be re-run due to task failures, be careful re-running the
  the following tasks:
  ```
    00 - Initialize ACC
    01 - Update ACC-admin password
    05 - Create an appliance-owner for both SSAs
    06 - Assign resources to the SSA owner
    07 - Update the password of SSA appliance-owner
    09 - Upload the SSA appliance image to the ACC
  ```
  If you need to re-run this playbook, the tasks above may fail during
  subsequent playbook executions. To prevent these failures, comment out the
  applicable tasks listed above that were successfully completed during previous
  playbook runs.
- **Note:** If you receive an error during or after execution of task
  `09 - Upload the SSA appliance image to the ACC`, you can use the tag `retry`
  while executing the playbook to automatically skip all the above tasks. For example:
  ```bash
  ansible-playbook 07_ssa_install_e2e.yaml -t retry
  ```  

### Procedure

- Export the username and password in a terminal on your control node
  (laptop), via which the ACC will communicate with the HMC:
  ```bash
  export HMC_USER=<enter_HMC_username>
  export HMC_PASSWORD=<enter_HMC_password>
  ```
- Export the owner variables in a terminal on your control node (laptop):
  ```bash
  export SSA_OWNER_DEFAULT_PASSWORD=<owner_default_password>
  export SSA_OWNER_PASSWORD=<owner_new_password>
  ```
- Export the SSA appliances credentials in a terminal on your control node (laptop):
  ```bash
  export SSA_APP_USER=<ssa_username>
  export SSA_APP_PASSWORD=<ssa_password>
  ```
- Download and store the SSA installation image to your control node.
- Update the variables to the required values in `env_vars.yaml`.
  - Do not comment out any variables, even if they are not supposed to be used
    in your infrastructure.
- Run the playbook `07_ssa_install_e2e.yaml`  to install SSA via:
  ```bash
  ansible-playbook 07_ssa_install_e2e.yaml
  ```

This playbook will set up ACC, upload the images, initiate the install and then
check the status of the install. The installation itself can take more than 15
mins. Check the status of installation on HMC.

## ACC and 2-SSAs Installation Sanity-Check | 08_acc_ssa_install_check.yaml

If you have installed ACC and 2-SSAs, you can do a quick check to see if the
appliances are functional. For this purpose:

- Export the SSA appliances credentials in a terminal on your control node (laptop):
  ```bash
  export SSA1_APP_USER=<ssa1_username>
  export SSA1_APP_PASSWORD=<ssa1_password>
  export SSA2_APP_USER=<ssa2_username>
  export SSA2_APP_PASSWORD=<ssa2_password>
  ```
- Modify the file `acc_ssa_install_check_vars.yaml`.
- Run the playbook to check the sanity of the installation via:
  ```bash
  ansible-playbook 08_acc_ssa_install_check.yaml
  ```

## Set HMC Credentials (Daily Task) | 09_insert_hmc_creds.yaml

The administrator must run the following Ansible playbook once every day to
refresh and set the HMC credentials in ACC.

For this purpose:

- Export the username and password in a terminal on your control node
  (laptop), via which the ACC will communicate with the HMC:
  ```bash
  export HMC_USER=<enter_HMC_username>
  export HMC_PASSWORD=<enter_HMC_password>
  ```
- Run the playbook:
  ```bash
    ansible-playbook 09_insert_hmc_creds.yaml
  ```

## Restart ACC | 10_restart_acc.yaml

If the ACC-admin wants to restart ACC, then ACC-admin can use this playbook:

```
ansible-playbook 10_restart_acc.yaml
```

This playbook will help user to restart ACC, which can be helpful in certain conditions.

Note: This playbook will not deactivate and then activate the ACC LPAR. It will
just send a restart signal to ACC appliance (similar to `reboot`).

## Trigger and Collect Disruptive dumps from Appliance | 11_get_disruptive_dumps.yaml

This playbook automates the process of triggering a disruptive dump on an SSC appliance
and collecting diagnostic logs after the reboot.  Handle with care, as you might lose unsaved data
on your appliances.

For starting and collecting disruptive dumps, run the playbook `11_get_disruptive_dumps.yaml`.

```bash
ansible-playbook 11_get_disruptive_dumps.yaml
```

This playbook with interactively ask for the IP, username, password, reason for
downloading dumps and file path where to download for gathering logs from any
appliances (e.g., SSA appliance).

## Unlock Appliances | 12_unlock_appliances.yaml | 13_unlock_each_appliance.yaml

This playbook allows the appliance-owner to unlock one or more locked appliances in their resource package.
Each appliance will be processed one at a time, prompting the user for credentials specific to that appliance.

Run this playbook as the appliance-owner.
```bash
ansible-playbook 12_unlock_appliances.yaml
```

To successfully run the above command, the file `13_unlock_each_appliance.yaml` must
be available in the same directory. This file is included by the main playbook
(`12_unlock_appliances.yaml`) for each appliance.
