# Ansible Playbooks for Deploying SSA

The Spyre Support Appliance (SSA) can either be deployed using the `appliance_deploy_*`
directories, or one can use the playbooks provided here. The difference is that
the playbooks in this directory are tested and adapted for SSA installation.
Moreover, the purpose of these playbooks is to address an end-to-end install
and configuration of SSA.

**Note**: A single playbook here is used to do both ACC-admin and
appliance-owner actions. That is, this playbook covers the actions in the
playbooks `01_admin_actions.yaml`, `02b_assign_2_lpar.yaml`,
`03_owner_actions.yaml` and `04_install_flow.yaml`. These actions are:

- Initialize/configure ACC.
- Creates an appliance-owner.
- Assigns 2 LPARs to the appliance-owner.
- Uploads SSA image.
- Install the SSA image on the 2 LPARs.
- Unlocks the SSA if installed via the HMC.

If you have a clear separation of roles in your organization, it is better to
use the playbooks in the `appliance_deploy_*` directories.

## SSA Installation (Default Mode) | 01_ssa_install_e2e_default.yaml

To install 2x SSAs after a fresh install of ACC, you can run this playbook.
This helps you to avoid the other playbooks in the `appliance_deploy_*`
directory.

This playbook is specifically configured for installing SSAs with the default
mode of ACC, where ACC can communicate with the HMC.

### Pre-requisites:

- It is expected that the ACC is installed using
  `../acc_install_ansible/00_acc_install.yaml` playbook prior to
  running this playbook.
- Please ensure that all SSA LPARs are **deactivated** from the HMC before
  executing the SSA appliance installation playbook `01_ssa_install_e2e_default.yaml`.
- The playbooks is configured in such a way that both SSA LPARs will use the
  same credentials.
- If the playbook must be re-run due to task failures, be careful re-running the
  the following tasks:

  ```linux
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
  ansible-playbook 01_ssa_install_e2e_default.yaml -t retry
  ```

### Procedure

- Export the username and password in a terminal on your control node
  (laptop), via which the ACC will communicate with the HMC:

  ```bash
  export HMC_USER=<enter_HMC_username>
  export HMC_PASSWORD=<enter_HMC_password>
  ```

- Export the previously set default appliance-owner password and additionally set and export
  a new password for the appliance-owner. Keep in mind, the ACC API will be used to set the
  new password for appliance-owner. Therefore, be sure you are following the password
  validation rules in the [ACC API](https://www.ibm.com/docs/en/systems-hardware/zsystems/9175-ME1?topic=reference-user-management#api_reference__title__3).

  ```bash
  export ACC_OWNER_DEFAULT_PASSWORD=<owner_default_password>
  export ACC_OWNER_PASSWORD=<owner_new_password>
  ```

- Export the SSA appliance credentials in a terminal on your control node (laptop):

  ```bash
  export APP_USERNAME=<appliance_username>
  export APP_PASSWORD=<appliance_password>
  ```

- Download and store the SSA installation image to your control node.
- Update the variables to the required values in `env_vars.yaml`.
  - Do not comment out any variables, even if they are not supposed to be used
    in your infrastructure.
- Run the playbook `01_ssa_install_e2e_default.yaml` to install SSA via:

  ```bash
  ansible-playbook 01_ssa_install_e2e_default.yaml
  ```

### Important

#### Select Correct Network Card Type (OSA/NETH)

When running the playbook `01_ssa_install_e2e_default.yaml` under `ssa_deploy_ansible`, the playbook prompts the user to select the network card type for each SSA LPAR:

```linux
Enter network card type to be used for LPAR <LPAR_NAME>: (OSA/NETH)
```

Please ensure that the correct network type is selected based on the system configuration:

- OSA -> use when you have CHPID, Port
- NETH -> use when you have FID

Additionally, the playbook will prompt the user to indicate whether the network being used by the LPAR
leverages VLAN tagging:

```Linux
Does the LPAR <LPAR_NAME> need VLAN configuration? (yes/no)
```

Ensure the appropriate `vlan*_id` variables are set in the `env_vars` file if VLAN tagging is required.

Selecting the wrong option will update the HMC network configuration incorrectly and may lead to the LPAR becoming unreachable.
To avoid such issues, always verify the correct network type in HMC before executing the playbook.

#### Select Correct Disk Information (FCP/DASD)

Please ensure that the correct disk type based on the system configuration:

- For FCP disk, ensure that valid values for 'wwpn1' and 'lun1' are provided
  - **Note**: When using FCP, the `disk*_id` variable represents the FCP device number used to communicate with the `lun` on the storage controller.
- For DASD disk, the 'wwpn1' and 'lun1' values are not required

This playbook will set up ACC, upload the images, initiate the install and then
check the status of the install. The installation itself can take more than 15
mins. Check the status of installation on HMC.

## SSA Installation (Standalone Mode) | 02_ssa_install_e2e_standalone.yaml

To install 2x SSAs after a fresh install of ACC, you can run this playbook.
This helps you to avoid the other playbooks in the `appliance_deploy_*`
directory. For example, this single playbook replaces `01_admin_actions.yaml`,
`02*_assign_*_lpar.yaml`, `03_owner_actions.yaml` and `04_install_flow.yaml`.

This playbook is specifically configured for installing SSAs with the standalone
mode of ACC, where ACC cannot communicate with the HMC.

### Pre-requisites:

- It is expected that the ACC is installed using
  `../acc_install_ansible/00_acc_install.yaml` playbook prior to
  running this playbook.
- Please ensure that all SSA LPARs are in **SSC Installer mode** on the HMC
  before executing the SSA appliance installation playbook
  `02_ssa_install_e2e_standalone.yaml`.
- The playbooks is configured in such a way that both SSA LPARs will use the
  same credentials.
- If the playbook must be re-run due to task failures, be careful re-running the
  the following tasks:

  ```linux
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
  ansible-playbook 02_ssa_install_e2e_standalone.yaml -t retry
  ```

### Procedure

- Export the previously set default appliance-owner password and additionally set and export
  a new password for the appliance-owner. Keep in mind, the ACC API will be used to set the
  new password for appliance-owner. Therefore, be sure you are following the password
  validation rules in the [ACC API](https://www.ibm.com/docs/en/systems-hardware/zsystems/9175-ME1?topic=reference-user-management#api_reference__title__3).

  ```bash
  export ACC_OWNER_DEFAULT_PASSWORD=<owner_default_password>
  export ACC_OWNER_PASSWORD=<owner_new_password>
  ```

- Export the SSA appliance credentials in a terminal on your control node (laptop):

  ```bash
  export APP_USERNAME=<appliance_username>
  export APP_PASSWORD=<appliance_password>
  ```

- Download and store the SSA installation image to your control node.
- Update the variables to the required values in `env_vars.yaml`.
  - Do not comment out any variables, even if they are not supposed to be used
    in your infrastructure.
  - Take special care of variables that are required in the standalone mode.
    For example, `cpc_ifls`.
- Run the playbook `02_ssa_install_e2e_standalone.yaml` to install SSA via:

  ```bash
  ansible-playbook 02_ssa_install_e2e_standalone.yaml
  ```

### Important


#### Select Correct Network Card Type (OSA/NETH)

When running the playbook `02_ssa_install_e2e_standalone.yaml` under `ssa_deploy_ansible`, the playbook prompts the user to select the network card type for each SSA LPAR:

```linux
Enter network card type to be used for LPAR <LPAR_NAME>: (OSA/NETH)
```

Please ensure that the correct network type is selected based on the system configuration:

- OSA -> use when you have CHPID, Port
- NETH -> use when you have FID

Additionally, the playbook will prompt the user to indicate whether the network being used by the LPAR
leverages VLAN tagging:

```Linux
Does the LPAR <LPAR_NAME> need VLAN configuration? (yes/no)
```

Ensure the appropriate `vlan*_id` variables are set in the `env_vars` file if VLAN tagging is required.

Selecting the wrong option will update the HMC network configuration incorrectly and may lead to the LPAR becoming unreachable.
To avoid such issues, always verify the correct network type in HMC before executing the playbook.


#### Select Correct Disk Information (FCP/DASD)

Please ensure that the correct disk type based on the system configuration:

- For FCP disk, ensure that valid values for 'wwpn1' and 'lun1' are provided
  - **Note**: When using FCP, the `disk*_id` variable represents the FCP device number used to communicate with the `lun` on the storage controller.
- For DASD disk, the 'wwpn1' and 'lun1' values are not required

This playbook will set up ACC, upload the images, initiate the install and then
check the status of the install. The installation itself can take more than 15
mins. Check the status of installation on HMC.

## ACC and 2x SSAs Installation Sanity-Check | 03_acc_ssa_install_check.yaml

If you have installed ACC and 2x SSAs, you can do a quick check to see if the
appliances are functional. For this purpose:

- Export the SSA appliance credentials in a terminal on your control node (laptop). These are the SSA LPAR's credentials used to install the 2 SSAs.

  ```bash
  export SSA1_APP_USER=<ssa1_username>
  export SSA1_APP_PASSWORD=<ssa1_password>
  export SSA2_APP_USER=<ssa2_username>
  export SSA2_APP_PASSWORD=<ssa2_password>
  ```

- Modify the file `env_vars.yaml`.
- Run the playbook to check the sanity of the installation via:

  ```bash
  ansible-playbook 03_acc_ssa_install_check.yaml
  ```

**Note**: Allow sufficient time for the SSA installation to fully complete before running this playbook. Premature playbook
execution may lead to inconsistent or incomplete results.

## 2x SSAs Management after HMC Install | 04_unlock_ssa_after_hmc_install.yaml

This playbook helps to bring the management of SSA appliances under ACC, in case
the 2 SSAs are installed using the HMC's UI (instead of the playbooks provided in
this repo). This way, even though the SSAs are installed using the HMC's UI,
they can still be managed by ACC. For example, after running this playbook,
ACC can pull health status from the 2 SSAs, ACC can pull logs out of the SSAs,
ACC can install updates on the SSAs etc.

Specifically, this playbook is referring to `Unlock the SSA appliances in ACC`,
first figure of Chapter 3 in the
[Spyre Accelerator Guide](https://www.ibm.com/docs/de/module_1721331501652/pdf/GC28-7071-00.pdf).

### Pre-requisites

Before running this playbook, you should:

- Install ACC.
- Run the `01_admin_actions.yaml` playbook for performing admin actions.
  - Use the `01_admin_actions.yaml` playbook in the applicable `appliance_deploy*`
    directory. For example, use the `appliance_deploy_default_*` for default
    mode ACC, and `appliance_deploy_standalone_*` for standalone mode ACC.
- Assign 2 LPARs as an admin to the appliance-owner using the playbook
  `02b_assign_2_lpar.yaml`.
- Install SSAs using the HMC's UI, without using the ansible playbooks
  provided in this repo.
- The SSA LPARs must be active before calling this playbook.

### Procedure

- Export the default
  password for the appliance-owner that is previously set in the `01_admin_actions.yaml`
  playbook. Also set a new password for the appliance-owner. Keep in mind, the ACC API will be used to set these new passwords for
  the ACC admin and owner, please be sure you are following the password
  validation rules in the [ACC API](https://www.ibm.com/docs/en/systems-hardware/zsystems/9175-ME1?topic=reference-user-management#api_reference__title__3):

  ```bash
  export ACC_OWNER_DEFAULT_PASSWORD=<owner_default_password>
  export ACC_OWNER_PASSWORD=<owner_new_password>
  ```
  The `owner_default_password` is the default password set by the
  ACC-admin for the appliance-owner. The `owner_new_password` is the password
  set by the appliance-owner for logging into ACC.
  Moreover, the password must adhere to the following rules:
  - Length: 15-128 characters.
  - Valid characters: letters, digits, special characters (`-_#!@$%&?`).
  - Must have at least one lower case, one upper case, one digit, one special
    character.
- Update the `env_vars.yaml` file in this directory.
- Run the playbook `04_unlock_ssa_after_hmc_install.yaml` via:
  ```bash
  ansible-playbook 04_unlock_ssa_after_hmc_install.yaml
  ```
- In the playbook, when unlocking the appliances, provide these credentials.
  These credentials represent the username and password of the SSA LPAR
  appliances, which were inserted when activating the LPAR using the HMC's
  UI.
  