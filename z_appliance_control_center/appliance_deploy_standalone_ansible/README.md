# Ansible Playbooks for Appliance Installation in Standalone Mode without MFA

These example playbooks let the user:

- Install a single appliance, or install 2 appliances.

## Setting Up ACC

To set up the ACC, the following actions must be performed by the ACC-admin on
their control node.

- `cd` to the directory `appliance_deploy_standalone_ansible`.
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
    - Check if you use FCP disk instead of a dasd. This means you have to
      modify the task `10 - As ACC-admin, assign single resources to the owner`.
      You will have to set the variable `is_fcp` to `true` and use the values for
      `wwpn` and `lun`.
    - Check if you use FIDs instead of `chipid`. This means you have to modify the
      task `10 - As ACC-admin, assign single resources to the owner`.
    - Run the playbook:
      ```bash
      ansible-playbook 02a_assign_1_lpar.yaml
      ```
- For two LPARs:
  - Fix the playbook `02b_assign_2_lpar.yaml` if required. For example:
    - Check if you have to remove a task in the playbook (e.g.,
      updating the password).
    - Check if you use FCP disk instead of a dasd. This means you have to
      modify the task `12- As ACC-admin, assign two lpar to the owner`.
      You will have to set the variable `is_fcp` to `true` and use the values for
      `wwpn` and `lun`.
    - Check if you use FIDs instead of `chipid`. This means you have to modify the
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

- `cd` to the directory `appliance_deploy_standalone_ansible`.
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

You can install lpar with 2 types of **execution action** with standalone mode:
  - 1️. **appliance_only** → when execution action is **appliance_only** lpar should be in `installer` mode for activation in HMC and must include `install` parameter in task (18 - As owner, install and activate the image).
  - 2️. **switch_to_installer**   → when execution action is **switch_to_installer** LPAR should be in `appliance` mode for activation in HMC and comment `install` parameter in task (18 - As owner, install and activate the image)
   
The above playbook will send the install command to ACC. ACC will take up to
20 mins to install the appliance. Check the status of the appliances on HMC.

Note that to pull logs for SSA, concurrent updates for SSA, upgrade, Health check
status, or ACC concurrent updates, use the playbooks located in:
`other_ansible_usecases_scripts` directory.

