# Ansible Playbooks for Appliance Installation in Default Mode without MFA

These example playbooks let the user:

- Install IBM Appliance Control Center (ACC) and
- Install a single appliance, or install 2 appliances.

## Installing ACC - Preparations

- Download the ACC installation image from Fix Central and store it on your
  control node (i.e., your laptop).
- Ensure that in the HMC, the Secure Service Container (SSC) based ACC LPAR
  activation profile is created and has updated with correct values of network settings. (e.g. `chpid`, `prefix`, etc.) and storage (initial 16 GB storage is required for ACC).
- Both ACC-admin and appliance-owners must
  [install ansible](https://docs.ansible.com/ansible/latest/installation_guide/intro_installation.html) on
  their respective control nodes.
  - For example, on your laptop that will connect with ACC, install `ansible` via:
    ```bash
    pip install ansible
    ```
- Download this directory and the `acc_install_ansible` directory on 
  the control node (e.g., a laptop), which will connect with ACC.

## ACC Appliance Installation

To set up the ACC, the following actions must be performed by the ACC-admin.

Note that the following steps are tested and verified on mac/Linux laptops.
Windows users can either use Windows Subsystem for Linux (WSL) or run equivalent
commands of the ones described below.

- Export your HMC username and password on a terminal in your laptop:
  ```
  export HMC_USER=<enter_HMC_username>
  export HMC_PASSWORD=<enter_HMC_password>
  ```

- Update the variables in the file (`appliance_deploy_default_ansible/acc_env_vars.yaml`):
  - Change the `IMAGE_PATH` and `INSTALL_SCRIPT_PATH` in the `acc_env_vars.yaml`
    file to point to the right one.
  - Change the `CPC`, `LPAR`, `LPAR_IP`, `DISK_ID` in the `acc_env_vars.yaml` file.
  - Update other values accordingly to point to the right LPAR for installation.
  - **For FCP Disk Uploads**:
    When uploading an image using an **FCP disk**, perform the following updates:
      - Uncomment the `lun` and `wwpn` parameters, and set `IS_FCP` to **true** in the `acc_env_vars.yaml` file.
      - Also, uncomment the `lun` and `wwpn` environment variables in the` 00_acc_install.yaml` file.


- Run the following playbook to install ACC via:
  ```
  ansible-playbook ./appliance_deploy_default_ansible/00_acc_install.yaml
  ```

The above step will take time (about 15 mins) to complete. The ansible playbook
will first set up the LPAR in the right mode, then upload the ACC image and
afterwards, install the ACC image on the disk.

Check the status of ACC LPAR on the HMC for successful installation. After this
step is completed, ACC will be installed and running.

## Setting Up ACC

To set up the ACC, the following actions must be performed by the ACC-admin on
their control node.

- Export your HMC username and password on a terminal in your laptop:
  ```
  export HMC_USER=<enter_HMC_username>
  export HMC_PASSWORD=<enter_HMC_password>
  ```
- Export Admin username and default password and new password on a terminal in your laptop:
  ```
  export ACC_ADMIN_USER=<your_admin_username>
  export ACC_ADMIN_DEFAULT_PASSWORD=<your_admin_old_password>
  export ACC_ADMIN_PASSWORD=<your_admin_old_password>
  ```
- `cd` to the directory `appliance_deploy_default_ansible`.
- Modify the variables in the file `admin_vars.yaml`.
  - Change the `acc_ip` in the `admin_vars.yaml` file to point to the right IP address.
    - Currently, it is set to a dummy IP 9.152.150.224, and port 8081.
    - Change this IP to the one that is used with ACC installation. Keep the port
      to 8081.
- Modify the variables of the LPAR that appliance-owner will use for installing
  the appliance.
  - For example, change `z_machine_lpar` to denote on which LPAR the appliance
    will be installed.
  - Also, assign the LPAR an IP address using the `app_ip` variable.
- Run the playbook via:
  ```
  ansible-playbook 01_admin_actions.yaml
  ```

This will perform one time operations and will configure ACC. Ideally, you should
never require a re-run of this script.

## Adding Resource Packages

To assign the resources to appliance-owners, the following steps must be taken by
an ACC-admin on their control node.

The directory provides 2 example playbooks:

- `02a_assign_1_lpar.yaml`:
  - This should be used to assign one LPAR to the appliance-owner.
  - The appliance-owner will then install only one appliance.
- `02b_assign_2_lpar.yaml`:
  - This should be used to assign two LPARs to the appliance-owner.
  - The appliance-owner will then install two appliances at the same time.

As an ACC-admin, you should **only run one of the above scripts**.

The information about one or both appliances should be entered in the
`admin_vars.yaml` file.

As an ACC-admin, run the appropriate playbook depending on the number of LPARs:

- For one LPAR:
  - Fix the playbook `02a_assign_1_lpar.yaml` if required. For example:
    - Check if you have to remove a task in the playbook (e.g.,
      updating the password).
    - If your setup uses a `vlan_id`, ensure that the variable is enabled in the configuration:
        - Uncomment the `vlan_id` entry in `admin_vars.yaml`.
        - Modify the task `10 - As ACC-admin, assign single resources to the owner` by uncommenting the `vlan_id` reference in that task.
    - Check if you use `vlan_id`, Please uncomment in vlan_id in `admin_vars.yaml` . This means you have to 
      modify the task `10 - As ACC-admin, assign single resources to the owner`(uncomment `vlan_id` in the task).
    - Check if you use FCP disk instead of a dasd. This means you have to
      modify the task `10 - As ACC-admin, assign single resources to the owner`.
      You will have to set the variable `is_fcp` to `true` and use the values for
      `wwpn` and `lun`.
    - Check if you use `chpid` instead of `fid`. This means you have to modify the
      task `10 - As ACC-admin, assign single resources to the owner`.
    - Check if you use FIDs instead of `chpid`. This means you have to modify the
      task `10 - As ACC-admin, assign single resources to the owner`.
    - Run the playbook:
      ```bash
      ansible-playbook 02a_assign_1_lpar.yaml
      ```
- For two LPARs:
  - Fix the playbook `02b_assign_2_lpar.yaml` if required. For example:
    - Check if you have to remove a task in the playbook (e.g.,
      updating the password).
    - If your setup uses a `vlan_id`, ensure that the variable is enabled in the configuration:
        - Uncomment the `vlan_id` entry in `admin_vars.yaml`.
        - Modify the task `12- As ACC-admin, assign two lpar to the owner` by uncommenting the `vlan_id` reference in that task.
    - Check if you use FCP disk instead of a dasd. This means you have to
      modify the task `12- As ACC-admin, assign two lpar to the owner`.
      You will have to set the variable `is_fcp` to `true` and use the values for
      `wwpn` and `lun`.
    - Check if you use `chpid` instead of `fid`. This means you have to modify the
      task `12- As ACC-admin, assign two lpar to the owner`.
    - Check if you use FIDs instead of `chpid`. This means you have to modify the
      task `12- As ACC-admin, assign two lpar to the owner`.
    - Run the playbook:
      ```bash
      ansible-playbook 02b_assign_2_lpar.yaml
      ```

This action has just assigned LPARs to the appliance-owner. The LPARs are not
activated. Now, the appliance-owner must install and activate the appliances
on these LPARs.

## Appliance Installation - Preparations

The appliance must be installed by the appliance-owner. For that, perform the
following actions as appliance-owner.

- `cd` to the directory `appliance_deploy_default_ansible`.
- Download the appliance image you want to install to your control node.
- Modify the variables in the file `owner_vars.yaml`.
  - Change the `acc_ip` in the `admin_vars.yaml` file to point to the right IP.
    - Currently, it is set to a dummy IP 9.152.150.224, and port 8081.
    - Change this IP to the one that should be serving ACC APIs.
- Modify the location of that image file in the `owner_vars.yaml` file.
  - Specifically, change the `image_path` variable.
- Run the`03_owner_action.yaml` playbook to update owner credentials and upload
  the appliance image:
  ```
  ansible-playbook 03_owner_action.yaml
  ```

The above playbook will update the passwords of the appliance-owner and upload the
appliance image to ACC. Ideally, you should never re-run this script unless you know what you are doing.

For example, this playbook will update the password of the owner, which is
required only once. If you want to re-run this playbook, then remove
the password change task in the playbook.

## Installing Appliances

Once the appliance-owner has performed the actions above, the appliance-owner can
install the appliance by running the following command on their control node.

```
ansible-playbook 04_install_flow.yaml
```

The above playbook with send the install command to ACC. ACC will take up to
20 mins to install the appliance. Check the status of the appliances on HMC.

Note that to pull logs for SSA, concurrent updates for SSA, upgrade, Health check
status, or ACC concurrent updates, use the playbooks located in:
`other_ansible_usecases_scripts` directory.

#### NOTE
Make sure to run 03_owner_action.yaml before executing 04_install_flow.yaml.
The 03_owner_action.yaml script appends the image_id: <id> entry to the end of the owner variable file.
If you skip running 03_owner_action.yaml, you may encounter the following error:

  `"msg": "The task includes an option with an undefined variable. 'image_id' is undefined"`
