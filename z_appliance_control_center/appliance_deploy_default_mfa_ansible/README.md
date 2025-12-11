# Ansible Playbooks for Appliance Installation in Default Mode with MFA

These sample playbooks:

- Allow to use MFA with Appliance Control Center (ACC) and
- Install a single appliance, or install 2 appliances.

### Notes

- It is assumed that the ACC-admin has already installed ACC using the
  `../acc_install_ansible/00_acc_install.yaml` playbook, or
  using the HMC.
- The ACC-admin has the HMC credentials.
- The ACC-admin has the credentials of the ACC's SSC LPAR.
- ACC can communicate with the HMC over the network.
- In these playbooks, the MFA enablement done during the
  initialization of ACC.

## Setting Up ACC - 01_admin_actions.yaml

To set up the ACC, the following actions must be performed by the ACC-admin on
their control node.

- Export your HMC username and password on a terminal in your laptop:

  ```bash
  export HMC_USER=<enter_HMC_username>
  export HMC_PASSWORD=<enter_HMC_password>
  ```

- Export the ACC-admin username and default password. Additionally, set and
  export a new password for the ACC admin and a default password for the ACC
  owner. Keep in mind, the ACC API will be used to set these new passwords for
  the ACC admin and owner, please be sure you are following the password
  validation rules in the ACC API:

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
  Moreover, the passwords must adhere to the following rules:
  - Length: 15-128 characters.
  - Valid characters: letters, digits, special characters (`-_#!@$%&?`).
  - Must have at least one lower case, one upper case, one digit, one special
    character.
  - Must not include the username.
- `cd` to the directory `appliance_deploy_default_mfa_ansible`.
- Modify the variables in the file `admin_vars.yaml`.
  - **Note**: Be sure the `acc_ip` in the `admin_vars.yaml` file points to the IP of the ACC LPAR. Keep the port to 8081.
  - Set the `mfa_enabled` parameter to `true` in the file `admin_vars.yaml` when
    enabling MFA. When `mfa_enabled` is set to `true` at initialization of ACC:
    - The playbook first generates a TOTP secret for updating the admin password.
    - Use this TOTP secret to generate an TOTP using any authenticator app.
    - Once the password is updated, another MFA secret is generated (called
      `mfa_secret` in the remainder of the text) for the respective user.
      - **!! Important !!:** Save this `mfa_secret` for future reference, as it is
        displayed only once.
    - Use this secret to generate TOTP, which will be required to obtain the
      access token.
- Check if you have to remove an already executed task in the playbook (e.g.,
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

The information about one or both appliances should be entered in the
`admin_vars.yaml` file.

Moreover, export ACC-admin username and password on a terminal in your laptop:

```bash
export ACC_ADMIN_USER=<admin_username>
export ACC_ADMIN_PASSWORD=<admin_password>
```

The `admin_username` is the ACC's SSC LPAR username, and `admin_password` is
the password.

To generate the access token for the admin user, provide the OTP when prompted in
the command line.
Use the `mfa_secret` to generate this OTP through your authenticator app.
While running the playbook, if the command prompt requests an OTP, enter the TOTP
generated using the saved `mfa_secret`.

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

- Export admin username and password on a terminal in your control node (laptop):

  ```bash
  export ACC_ADMIN_USER=<admin_username>
  export ACC_ADMIN_PASSWORD=<admin_password>
  ```

  **Note**: This step will no longer be required in the next version of the
  playbooks. These credentials are used to create MFA secret for the appliance-
  owner.
- Export the previously set default appliance-owner password and additionally set and export
  a new password for the appliance-owner. Keep in mind, the ACC API will be used to set the
  new password for appliance-owner. Therefore, be sure you are following the password
  validation rules in the ACC API.

  ```bash
  export ACC_OWNER_DEFAULT_PASSWORD=<owner_default_password>
  export ACC_OWNER_PASSWORD=<owner_new_password>
  ```

  `owner_default_password` is the default password set by the ACC-admin for
  the appliance-owner. `owner_new_password` is the password created by the
  appliance-owner, which will replace the default password.
- `cd` to the directory `appliance_deploy_default_mfa_ansible`.
- Download the appliance image you want to install to your control node.
- Modify the variables in the file `owner_vars.yaml`.
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

Note: To generate the access token for the owner, provide the OTP when prompted in the command line.
Use the last saved mfa_secret of owner to generate this OTP through your authenticator app.
When the command prompt requests an OTP, enter the OTP generated using the saved mfa_secret.

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
- `cd` to the directory `appliance_deploy_default_mfa_ansible`.
- Modify the variables in the file `owner_vars.yaml`.

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
The `03_owner_actions.yaml` playbook appends the `image_id`: <id> entry to the end of the owner variable file.
If you skip running 03_owner_actions.yaml, you may encounter the following error:

```bash
  "msg": "The task includes an option with an undefined variable. 'image_id' is undefined"`
```

You can also manually add `image_id` variable to the `owner_vars.yaml` file, in case you
do not want to re-run `03_owner_actions.yaml`.
