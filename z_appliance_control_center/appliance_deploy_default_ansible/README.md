# Ansible Playbooks for Appliance Installation in Default Mode without MFA

These sample playbooks:

- Install a single appliance, or install 2 appliances.

### Notes

- It is assumed that the ACC-admin has already installed ACC using the
  `../acc_install_ansible/00_acc_install.yaml` playbook, or
  using the HMC.
- The ACC-admin has the HMC credentials.
- The ACC-admin has the credentials of the ACC's SSC LPAR.
- ACC can communicate with the HMC over the network.
- MFA is disabled in ACC by the ACC-admin.

## Setting Up ACC - 01_admin_actions.yaml

To set up the ACC, the following actions must be performed by the ACC-admin on
their control node.

- Export HMC username and password on a terminal in your control node (laptop):

  ```bash
  export HMC_USER=<enter_HMC_username>
  export HMC_PASSWORD=<enter_HMC_password>
  ```

  Here, ACC-admin should use the credentials with which ACC will access the HMC.
- Export the ACC-admin username and default password. Additionally, set and
  export a new password for the ACC admin and a default password for the ACC
  owner. Keep in mind, the ACC API will be used to set these new passwords for
  the ACC admin and owner, please be sure you are following the password
  validation rules in the [ACC API](https://www.ibm.com/docs/en/systems-hardware/zsystems/9175-ME1?topic=reference-user-management#api_reference__title__3):

  ```bash
  export ACC_ADMIN_USER=<admin_username>
  export ACC_ADMIN_DEFAULT_PASSWORD=<admin_password>
  export ACC_ADMIN_PASSWORD=<admin_new_password>
  export ACC_OWNER_DEFAULT_PASSWORD=<owner_default_password>
  ```

  The `admin_username` is the ACC LPAR's username, and the `admin_password` is the
  current ACC LPAR's password. The `admin_new_password` is the new password to be set for the
  ACC-admin. The `owner_default_password` is the default password for the appliance-owner.
- `cd` to the directory `appliance_deploy_default_ansible`.
- Modify the variables in the file `admin_vars.yaml`.
  - When reviewing the `admin_vars` file from top to bottom, all variables up to and including `z_machine_name` must be updated to run this playbook. The remaining variables will be modified later when running the [Adding Resource Package playbooks](#adding-resource-packages---02a_assign_1_lparyaml--02b_assign_2_lparyaml).
    - **Note**: Be sure the `acc_ip` in the `admin_vars.yaml` file points to the IP of the ACC LPAR. Keep the port to 8081.
- Check if you have to remove an already executed tasks in the playbook (e.g.,
  updating the password).
  - Some of the tasks might fail if executed twice.
- Run the playbook via:

  ```bash
  ansible-playbook 01_admin_actions.yaml
  ```

This playbook will perform one time operations and will configure ACC. Moreover, it will
create an appliance-owner.

## Adding Resource Packages - 02a_assign_1_lpar.yaml | 02b_assign_2_lpar.yaml

To assign the resources to appliance-owners, the steps outlined in this section must be taken by
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

Any remaining admin variables not updated during [01 playbook execution](#setting-up-acc---01_admin_actionsyaml)
must be configured in the `admin_vars.yaml` file. This includes all information related to one or both appliances.

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
    - Uncomment the line with `vlan_id` in the task `As ACC-admin, assign single LPAR to the owner`
      in the playbook `02a_assign_1_lpar.yaml`.
  - If you use FCP disk instead of a dasd, ensure that the variables are enabled in the configuration:
    - Set `is_fcp1` to `true` in `admin_vars.yaml`.
    - Uncomment the `wwpn1` and `lun1` entries in `admin_vars.yaml`.
    - Uncomment the lines with `wwpn` and `lun` in the task `As ACC-admin, assign single LPAR to the owner`
      in the playbook `02a_assign_1_lpar.yaml`.
      - **Note**: Keep in mind, when using FCP the `disk_id` variable represents the FCP device number used to communicate with the `lun` on the storage controller.
  - If you use `chpid` instead of `fid`, ensure that the variables are enabled in the configuration:
    - Uncomment the `chpid1` and `zport1` entries in `admin_vars.yaml`.
    - Uncomment the lines with `chpid` and `port` in the task `As ACC-admin, assign single LPAR to the owner`
      in the playbook `02a_assign_1_lpar.yaml`.
    - Comment-out the line with `fid` in the task `As ACC-admin, assign single LPAR to the owner`
      in the playbook `02a_assign_1_lpar.yaml`.

  - Run the playbook:

    ```bash
    ansible-playbook 02a_assign_1_lpar.yaml
    ```

- **For two LPARs**, update the playbook `02b_assign_2_lpar.yaml` to match your infrastructure. For example:
  - If your setup uses a `vlan_id`, ensure that the variables are enabled in the configuration:
    - Uncomment the `vlan_id1` and `vland_id2` entries in `admin_vars.yaml`.
    - Uncomment the line with `vlan_id` in the task `As ACC-admin, assign two LPARs to the owner`
        in the playbook `02b_assign_2_lpar.yaml`.
  - If you use FCP disk instead of a dasd, ensure that the variables are enabled in the configuration:
    - Set `is_fcp1` and `is_fcp2` to `true` in `admin_vars.yaml`.
    - Uncomment the `wwpn1`, `wwpn2`, `lun1` and `lun2` entries in `admin_vars.yaml`.
    - Uncomment the lines with `wwpn` and `lun` in the task `As ACC-admin, assign two LPARs to the owner`
      in the playbook `02b_assign_2_lpar.yaml`.
      - **Note**: Keep in mind, when using FCP the `disk_id` variable represents the FCP device number used to communicate with the `lun` on the storage controller.
  - If you use `chpid` instead of `fid`, ensure that the variables are enabled in the configuration:
    - Uncomment the `chpid1`,  `chpid2`, `zport1` and `zport2` entries in `admin_vars.yaml`.
    - Uncomment the lines with `chpid` and `port` in the task `As ACC-admin, assign two LPARs to the owner`
        in the playbook `02b_assign_2_lpar.yaml`.
    - Comment-out the line with `fid` in the task `As ACC-admin, assign two LPARs to the owner`
        in the playbook `02b_assign_2_lpar.yaml`.

  - Run the playbook:

    ```bash
    ansible-playbook 02b_assign_2_lpar.yaml
    ```

This action will just assigned LPARs to the appliance-owner. The LPARs will not be
activated. Now, the appliance-owner must install and activate the appliances
on these LPARs.

## Appliance Installation - Preparations - 03_owner_actions.yaml

The appliance must be installed by the appliance-owner. For that, perform the
following actions as appliance-owner.

- Export the previously set default appliance-owner password and additionally set and export
  a new password for the appliance-owner. Keep in mind, the ACC API will be used to set the
  new password for appliance-owner. Therefore, be sure you are following the password
  validation rules in the [ACC API](https://www.ibm.com/docs/en/systems-hardware/zsystems/9175-ME1?topic=reference-user-management#api_reference__title__3).

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
  - When reviewing the `owner_vars` file from top to bottom, all variables up to and including `image_type` must be updated to run this playbook. The remaining variables will be modified later when running the [Installing Appliances playbooks](#installing-appliances---04_install_flowyaml).
    - **Note**: Be sure the `acc_ip` in the `owner_vars.yaml` file points to the IP of the ACC LPAR. Keep the port to 8081.
- Modify the location of that image file in the `owner_vars.yaml` file.
  - Specifically, change the `image_path` variable.
- Run the`03_owner_actions.yaml` playbook to update owner credentials and upload
  the appliance image:

  ```bash
  ansible-playbook 03_owner_actions.yaml
  ```

The above playbook will update the passwords of the appliance-owner and upload the
appliance image to ACC.

Ideally, you should never re-run this script unless you know what you are doing.
For example, this playbook will update the password of the owner, which is
required only once. If you want to re-run this playbook, then remove
the password change task in the playbook.

## Installing Appliances - 04_install_flow.yaml

**Note**: At this point, the appliance-owner should make sure that the LPARs that are
assigned to the appliance-owner are **deactivated**.

Once the appliance-owner has performed the actions above, the appliance-owner can
install the appliance. For that, perform the following actions as appliance-owner.

- In case the admin has only assigned a single LPAR using the
  `02a_assign_1_lpar.yaml` playbook, then remove the second LPAR's details in
  task `01 - As owner, install and activate the image` of `04_install_flow.yaml`.

  For example, remove these lines:

  ```bash
  ,
  {
      "name": "{{ lpar_name2 }}",
      "execution_action": "{{ execution_action }}",
      "install": "{{ install }}"
  }
  ```

  Note that you will have to remove any trailing commas in the JSON object.
- Export the newly set appliance-owner password in a terminal on your control node (laptop):

  ```bash
  export ACC_OWNER_PASSWORD=<owner_new_password>
  ```

- Export and set a new username and password to be used for the appliance LPAR(s):

  ```bash
  export APP_USERNAME=<appliance_username>
  export APP_PASSWORD=<appliance_password>
  ```

  `appliance_username` and `appliance_passwords` are the credentials of the SSC LPAR (appliance)
  that will be installed by ACC.
- `cd` to the directory `appliance_deploy_default_ansible`.
- Modify the remaining owner variables not updated during [03 playbook execution](#appliance-installation---preparations---03_owner_actionsyaml) in the file `owner_vars.yaml`.

- Run the playbook:

  ```bash
  ansible-playbook 04_install_flow.yaml
  ```

The above playbook with send the install command to ACC. ACC will take up to
20 mins to install the appliance. Check the status of the appliances on HMC.

Note that to pull logs for appliances, concurrent updates for appliances, upgrade, health check
status, or ACC concurrent updates, use the playbooks located in:
`other_usecases_ansible` directory.

#### NOTE

Make sure to run `03_owner_actions.yaml` before executing `04_install_flow.yaml`.
The `03_owner_actions.yaml` playbook appends the `image_id`: <id> entry to the end of the `owner_vars.yaml` file.
If you skip running `03_owner_actions.yaml`, you may encounter the following error:

```bash
"msg": "The task includes an option with an undefined variable. 'image_id' is undefined"
```

You can also manually add `image_id` variable to the `owner_vars.yaml` file, in case you
do not want to re-run `03_owner_actions.yaml`.
