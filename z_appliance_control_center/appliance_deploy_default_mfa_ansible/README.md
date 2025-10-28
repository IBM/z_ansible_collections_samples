# Ansible Playbooks for Appliance Installation in Default Mode with MFA

These example playbooks let the user:

- Install a single appliance, or install 2 appliances.
- MFA will be enabled at the Initialization Level.

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
- `cd` to the directory `appliance_deploy_default_mfa_ansible`.
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

## For MFA Configuration

**Note:** MFA enablement is supported only during the initialization of ACC, not in between.

- Set the `mfa_enabled` parameter to `true` when enabling or proceeding with MFA in the file `admin_vars.yaml`.

- When `mfa_enabled` is set to `true` at initialization of ACC:
    - The system first generates a `TOTP` secret for updating the admin password.
    - Use this` TOTP secret` to generate an `OTP` using any `authenticator app`.
   - Once the password is updated, an `MFA secret (mfa_secret)` is generated for the respective user `(admin/owner)`.
  - **!! Important !!:** Save this `mfa_secret` for future reference, as it is displayed only once.
  - Use this mfa_secret to generate an `OTP`, which will be required to obtain the access token.

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

As an ACC-admin, run the appropriate playbook depending on the number of LPARs:-

  ```bash
  To generate the access token for the admin user, provide the OTP when prompted in the command line.
  Use the last saved mfa_secret to generate this OTP through your authenticator app.
  When the command prompt requests an OTP, enter the OTP generated using the saved mfa_secret.
  ```
- For one LPAR:
  - Fix the playbook `02a_assign_1_lpar.yaml` if required. For example:
    - Check if you have to remove a task in the playbook (e.g.,
      updating the password).
    - If your setup uses a `vlan_id`, ensure that the variable is enabled in the configuration:
        - Uncomment the `vlan_id` entry in `admin_vars.yaml`.
        - Modify the task `10 - As ACC-admin, assign single resources to the owner` by uncommenting the `vlan_id` reference in that task.
    - Check if you use `FCP disk` instead of a dasd. This means you have to
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
    - Check if you use` FCP disk` instead of a dasd. This means you have to
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

- `cd` to the directory `appliance_deploy_default_mfa_ansible`.
- Download the appliance image you want to install to your control node.
- Modify the variables in the file `owner_vars.yaml`.

- Update the admin credentials in the `owner_vars.yaml` file.
    - This is required because generating the access token for enabling 2FA for the owner (via mfa/secret/owner/) requires the admin’s access token.

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

 ```bash
  To generate the access token for the owner, provide the OTP when prompted in the command line.
  Use the last saved mfa_secret of owner to generate this OTP through your authenticator app.
  When the command prompt requests an OTP, enter the OTP generated using the saved mfa_secret.
  ```
```
ansible-playbook 04_install_flow.yaml
```

The above playbook with send the install command to ACC. ACC will take up to
20 mins to install the appliance. Check the status of the appliances on HMC.

Note that to pull logs for SSA, concurrent updates for SSA, upgrade, jealth check
status, or ACC concurrent updates, use the playbooks located in:
`other_usecases_ansible` directory.

#### NOTE
Make sure to run 03_owner_action.yaml before executing 04_install_flow.yaml.
The 03_owner_action.yaml script appends the image_id: <id> entry to the end of the owner variable file.
If you skip running 03_owner_action.yaml, you may encounter the following error:

  `"msg": "The task includes an option with an undefined variable. 'image_id' is undefined"`
