# Ansible Playbooks for Appliance Installation in Default Mode without MFA

These sample playbooks:

- Install IBM Appliance Control Center (ACC) and
- Install a single appliance, or install 2 appliances.

## Installing ACC - Preparations

**Note**: The following steps are tested and verified on MacOS terminal.
Windows users can either use Windows Subsystem for Linux (WSL) or run equivalent
commands of the ones described below. Linux users can use terminal to
run the commands.

- Download the ACC installation image from Fix Central and store it on your
  control node (i.e., your laptop).
- Ensure the control node (your laptop) and ACC IP address have sufficient
  authority and are added to the appropriate HMC whitelist to allow for proper
  communication to the HMC.
- Ensure that in the HMC, the Secure Service Container (SSC) based ACC LPAR
  activation profile is created and is updated with correct values of network
  settings. (`chpid`, `prefix`, `fid`, etc.) and storage (initial 16 GB storage is required for ACC).
- Both ACC-admin and appliance-owners must
  [install ansible](https://docs.ansible.com/ansible/latest/installation_guide/intro_installation.html) on
  their respective control nodes.
  - For example, on your laptop that will connect with ACC, install `ansible` via:
    ```bash
    pip install ansible
    ```
- Download this directory and the `acc_install_ansible` directory on 
  the control node (e.g., a laptop), which will connect with ACC.
- Install the following software on your control node.

### System Requirements

  - Ansible (must already be installed)
  - Python 3.x
  - sshpass

**Note**:

- The ansible playbook does not automatically install `packages`.
- If `sshpass` is missing, the playbook will stop with an error. Install it manually depending on your OS.

### Python Packages

The following python packages are required to be installed within a python virtual environment for successful execution of the ACC installation playbook:

  - click
  - click_shell
  - zhmcclient
  - urllib3

To setup a python virtual environment and install the required packages, please execute the following procedure:

- `cd` to the `../acc_install_ansible` directory on your control node (laptop).
- Run the command:
  ```bash
  python3 -m venv venv
  ```
- Afterwards, run the following commands to enter the python virtual environment, install the required python packages, and exit the virtual environment:
  ```bash
  source venv/bin/activate
  pip install click click_shell zhmcclient urllib3
  deactivate
  ```

## ACC Appliance Installation - 00_acc_install.yaml

To set up the ACC, the following actions must be performed by the ACC-admin.

- Export your HMC username and password in a terminal on your laptop:
  ```bash
  export HMC_USER=<enter_HMC_username>
  export HMC_PASSWORD=<enter_HMC_password>
  ```
- Export ACC-admin username and default password and new password on a terminal in your laptop:
  ```bash
  export ACC_ADMIN_USER=<lpar_username>
  export ACC_ADMIN_DEFAULT_PASSWORD=<lpar_password>
  ```
  `lpar_username` and `lpar_password` are the ACC's SSC LPAR's credentials. These credentials
  are used by ACC-admin later as well, when initializing ACC.
- Update the variables in the file (`appliance_deploy_default_ansible/acc_env_vars.yaml`):
  - Change the `IMAGE_PATH` in the `acc_env_vars.yaml` file.
  - Change the `CPC`, `LPAR`, `LPAR_IP`, `DISK_ID` in the `acc_env_vars.yaml` file.
  - Update other values accordingly to point to the right LPAR for installation.
    - If you use FCP disk instead of a dasd to install ACC, ensure that the variables are enabled in the configuration, set `IS_FCP` to `true` in the `acc_env_vars.yaml` file.
- Run the following playbook to install ACC:
  ```bash
  ansible-playbook ./appliance_deploy_default_ansible/00_acc_install.yaml
  ```

If required, the ansible playbook will create and activate a
`Python virtual environment` at `INSTALL_SCRIPT_PATH` location. However, if the
the above python virtual environment and packages within the virtual environment are missing, the playbook
will display an error message and stop.

The above step will take time (about 15 mins) to complete. The scripts
will first set up the LPAR in the right mode, then upload the ACC image and
afterwards, install the ACC image on the disk.

Check the status of ACC LPAR on the HMC for successful installation. After this
step is completed, ACC will be installed and running.

## Setting Up ACC - 01_admin_actions.yaml

To set up the ACC, the following actions must be performed by the ACC-admin on
their control node.

- Export HMC username and password on a terminal in your control node (laptop):
  ```bash
  export HMC_USER=<enter_HMC_username>
  export HMC_PASSWORD=<enter_HMC_password>
  ```
  Here, ACC-admin should use the credentials with which ACC will access the HMC.
- Export ACC-admin username and default password and new password on a terminal in your laptop:
  ```bash
  export ACC_ADMIN_USER=<admin_username>
  export ACC_ADMIN_DEFAULT_PASSWORD=<admin_old_password>
  export ACC_ADMIN_PASSWORD=<admin_new_password>
  export ACC_OWNER_DEFAULT_PASSWORD=<owner_default_password>
  ```
  The `admin_username` is the ACC's SSC LPAR username, and the `admin_old_password` is the
  ACC's SSC LPAR password. `admin_new_password` is the new password that is set by the
  ACC-admin. `owner_default_password` is the default password set by the ACC-admin for
  the appliance-owner.
- `cd` to the directory `appliance_deploy_default_ansible`.
- Modify the variables in the file `admin_vars.yaml`.
  - Change the `acc_ip` in the `admin_vars.yaml` file to point to the right IP address.
    Change the IP to the IP of the ACC's SSC LPAR. Keep the port to 8081.
- Check if you have to remove an already executed task in the playbook (e.g.,
  updating the password).
  - Some of the tasks might fail if executed twice.
- Run the playbook via:
  ```bash
  ansible-playbook 01_admin_actions.yaml
  ```

This will perform one time operations and will configure ACC. Moreover, it will
create an appliance-owner.

## Adding Resource Packages - 02a_assign_1_lpar.yaml | 02b_assign_2_lpar.yaml

To assign the resources to appliance-owners, the following steps must be taken by
an ACC-admin on the control node.

The directory provides sample playbooks for 2 scenarios:

- `02a_assign_1_lpar.yaml`:
  - This should be used to assign one LPAR to the appliance-owner. This playbook will not
    activate the LPAR.
  - The appliance-owner will then be able to install only one appliance.
- `02b_assign_2_lpar.yaml`:
  - This should be used to assign two LPARs to the appliance-owner. This playbook will not
    activate the LPAR.
  - The appliance-owner will then be able to install two appliances at the same time.

As an ACC-admin, you should **only run one of the above scripts**.

The information about one or both appliances should be entered in the
`admin_vars.yaml` file.

Moreover, export ACC-admin username and password on a terminal in your laptop:
```bash
export ACC_ADMIN_USER=<admin_username>
export ACC_ADMIN_PASSWORD=<admin_password>
```
The `admin_username` is the ACC's SSC LPAR username, and `admin_password` is 
the password.

As an ACC-admin, run the appropriate playbook depending on the number of LPARs:

- **For one LPAR**, update the playbook `02a_assign_1_lpar.yaml` to match your infrastructure. For example:
  - If your setup uses a `vlan_id`, ensure that the variable is enabled in the configuration:
      - Uncomment the `vlan_id1` entry in `admin_vars.yaml`.
      - Uncomment the line with `vlan_id` in the task `As ACC-admin, assign single LPAR to the owner`.
  - If you use FCP disk instead of a dasd, ensure that the variables are enabled in the configuration:
      - Set `is_fcp1` to `true` in `admin_vars.yaml`.
      - Uncomment the `wwpn1` and `lun1` entries in `admin_vars.yaml`.
      - Uncomment the lines with `wwpn` and `lun` in the task `As ACC-admin, assign single LPAR to the owner`.
  - If you use `chpid` instead of `fid`, ensure that the variables are enabled in the configuration:
      - Uncomment the `chpid1` and `zport1` entries in `admin_vars.yaml`.
      - Uncomment the lines with `chpid` and `port` in the task `As ACC-admin, assign single LPAR to the owner`.
      - Comment-out the line with `fid` in the task `As ACC-admin, assign single LPAR to the owner`.
  - Run the playbook:
    ```bash
    ansible-playbook 02a_assign_1_lpar.yaml
    ```
- **For two LPARs**, update the playbook `02b_assign_2_lpar.yaml` to match your infrastructure. For example:
  - If your setup uses a `vlan_id`, ensure that the variables are enabled in the configuration:
      - Uncomment the `vlan_id1` and `vland_id2` entries in `admin_vars.yaml`.
      - Uncomment the line with `vlan_id` in the task `As ACC-admin, assign two LPARs to the owner`.
  - If you use FCP disk instead of a dasd, ensure that the variables are enabled in the configuration:
      - Set `is_fcp1` and `is_fcp2` to `true` in `admin_vars.yaml`.
      - Uncomment the `wwpn1`, `wwpn2`, `lun1` and `lun2` entries in `admin_vars.yaml`.
      - Uncomment the lines with `wwpn` and `lun` in the task `As ACC-admin, assign two LPARs to the owner`.
  - If you use `chpid` instead of `fid`, ensure that the variables are enabled in the configuration:
      - Uncomment the `chpid1`,  `chpid2`, `zport1` and `zport2` entries in `admin_vars.yaml`.
      - Uncomment the lines with `chpid` and `port` in the task `As ACC-admin, assign two LPARs to the owner`.
      - Comment-out the line with `fid` in the task `As ACC-admin, assign two LPARs to the owner`.
  - Run the playbook:
    ```bash
    ansible-playbook 02b_assign_2_lpar.yaml
    ```

This action will just assigned LPARs to the appliance-owner. The LPARs will not be
activated. Now, the appliance-owner must install and activate the appliances
on these LPARs.

## Appliance Installation - Preparations - 03_owner_action.yaml

The appliance must be installed by the appliance-owner. For that, perform the
following actions as appliance-owner.

- Export default password and new password on a terminal in your control node (laptop):
  ```bash
  export ACC_OWNER_DEFAULT_PASSWORD=<owner_default_password>
  export ACC_OWNER_PASSWORD=<owner_new_password>
  ```
  `owner_default_password` is the default password set by the ACC-admin for
  the appliance-owner. `owner_new_password` is the password created by the
  appliance-owner, which will replace the default password.
- `cd` to the directory `appliance_deploy_default_ansible`.
- Download the appliance image you want to install to your control node.
- Modify the variables in the file `owner_vars.yaml`.
  - Change the `acc_ip` in the `owner_vars.yaml` file to point to the right IP.
    - Change this IP and port to the one that should be serving ACC APIs.
- Modify the location of that image file in the `owner_vars.yaml` file.
  - Specifically, change the `image_path` variable.
- Run the`03_owner_action.yaml` playbook to update owner credentials and upload
  the appliance image:
  ```bash
  ansible-playbook 03_owner_action.yaml
  ```

The above playbook will update the passwords of the appliance-owner and upload the
appliance image to ACC. 

Ideally, you should never re-run this script unless you know what you are doing.
For example, this playbook will update the password of the owner, which is
required only once. If you want to re-run this playbook, then remove
the password change task in the playbook.

## Installing Appliances - 04_install_flow.yaml

Once the appliance-owner has performed the actions above, the appliance-owner can
install the appliance. For that, perform the following actions as appliance-owner.

- In case the admin has only assigned a single LPAR using the
  `02a_assign_1_lpar.yaml` playbook, then comment the second LPAR's details in
  task `01 - As owner, install and activate the image` of `04_install_flow.yaml` 

  For example, comment out these lines:
  ```bash
  {
      "name": "{{ lpar_name2 }}",
      "execution_action": "{{ execution_action }}",
      "install": "{{ install }}"
  }
  ```
- Export password on a terminal in your control node (laptop):
  ```bash
  export ACC_OWNER_PASSWORD=<owner_new_password>
  ```
- Export the username and password of the appliance in a terminal on your control node (laptop):
  ```bash
  export APP_USERNAME=<appliance_username>
  export APP_PASSWORD=<appliance_password>
  ```
  `appliance_username` and `appliance_passwords` are the credentials of the SSC LPAR (appliance)
  that will be installed by ACC.
- `cd` to the directory `appliance_deploy_default_ansible`.
- Modify the variables in the file `owner_vars.yaml`.
- Run the playbook:
  ```bash
  ansible-playbook 04_install_flow.yaml
  ```

The above playbook with send the install command to ACC. ACC will take up to
20 mins to install the appliance. Check the status of the appliances on HMC.

Note that to pull logs for SSA, concurrent updates for SSA, upgrade, health check
status, or ACC concurrent updates, use the playbooks located in:
`other_usecases_ansible` directory.

#### NOTE

Make sure to run `03_owner_action.yaml` before executing `04_install_flow.yaml`.
The `03_owner_action.yaml` playbook appends the `image_id`: <id> entry to the end of the `owner_vars.yaml` file.
If you skip running `03_owner_action.yaml`, you may encounter the following error:

```bash
"msg": "The task includes an option with an undefined variable. 'image_id' is undefined"
```

You can also manually add `image_id` variable to the `owner_vars.yaml` file, in case you
do not want to re-run `03_owner_action.yaml`.
