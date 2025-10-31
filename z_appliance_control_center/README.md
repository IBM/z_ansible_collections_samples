# IBM Z Appliance Control Center

**Note that these playbooks are constantly improved, and can be treated as
a draft. It is strongly advised that users always check for latest changes.**

[IBM Z Appliance Control Center](https://www.ibm.com/docs/en/systems-hardware/zsystems/9175-ME1?topic=library-appliance-control-center-z-linuxone-users-guide)
(ACC) is an IBM product to manage Secure Service Container (SSC) based appliances
on IBM Z or LinuxONE systems. You can use ACC to manage appliances like
Spyre Support Appliance (SSA).

To work efficiently with ACC, it is recommended to do the following in order:

- Read the ACC user guide.
- Use the ansible scripts.
- Check the ACC's UI.

This set of directories contains ansible playbooks that you can use against ACC to
manage appliances. These playbooks should be used as a sample. The user is
expected to understand the infrastructure of ACC and other appliances, as well
as have some understanding of setting up environment variables.

All these playbooks are tested on MacOS.

## ACC User Guide

For a detailed information about ACC, its concepts and working principles,
read the [ACC user guide](https://www.ibm.com/docs/en/module_1721331501652/pdf/GC28-7073-00.pdf).

## Playbook Directory Structure

The playbooks are organized into multiple directories, based upon the way ACC
is deployed, configured and used.

| Directory Name | Purpose |
|:------------|:--------|
| appliance_deploy_default_ansible | Use these playbooks to deploy (e.g., install and activate) ACC and appliances, and when ACC is configured to communicate with the HMC |
| appliance_deploy_standalone_ansible | Use these playbooks to deploy (e.g., install and activate) ACC and appliances, and when ACC is not configured to communicate with the HMC |
| appliance_deploy_default_mfa_ansible | Use these playbooks to deploy (e.g., install and activate) ACC with MFA, and appliances, and when ACC is configured communicate with the HMC |
| appliance_deploy_standalone_mfa_ansible | Use these playbooks to deploy (e.g., install and activate) ACC with MFA and appliances, and when ACC is not configured to communicate with the HMC |
| other_usecases_ansible | Use these playbooks to use ACC for other use cases like updates, gathering logs, inserting HMC credentials |
| acc_install_ansible | A directory with scripts for installing ACC, normally you will not modify these playbooks |

Therefore, you must consider the right directory before using these playbooks with ACC.
Refer to the user guide for more information.

Within these directories, you will find different examples and use-cases for:

- Installing ACC
- Performing admin actions (e.g., initialize ACC, update admin password, set HMC, get cpc details)
- Assigning LPARs (1 or 2 LPARs)
- Owner actions (e.g., update password, upload image)
- Installation flows (e.g., install appliance for one or two LPARs)
- Pulling ACC logs
- Updating ACC image
- End-to-end installation of the appliance
- Upgrading and updating SSA image
- Pulling SSA logs and Checking health status of SSA
- Syncing LPARs
- Install checks for ACC and SSA

Please read the README.md file in the same directory, before proceeding with the playbooks.

### File Structure of appliance_deploy_* 

These directories are used to deploy appliances.

| File | Purpose |
|:---------|:--------|
| `acc_env_vars.yaml`| This file contains variables used by the ACC administrator to install ACC |
| `admin_vars.yaml` | This file contains variables used by the ACC administrator to initialize ACC and assign resources to the appliance owners |
| `owner_vars.yaml` | This file contains variables used by the appliance owners to install their appliances |
| `00_acc_install.yaml` | ACC administrator can use this playbook to install ACC, and it will use the scripts in `acc_ansible_install_scripts` directory |
| `01_admin_actions.yaml` | ACC administrator can use this playbook to initialize ACC |
| `02a_assign_1_lpar.yaml` | ACC administrator can use this playbook to assign a single LPAR to an appliance owner |
| `02b_assign_2_lpar.yaml` | ACC administrator can use this playbook to assign two LPARs ot an appliance owner |
| `03_owner_action.yaml` | Appliance owner can use this playbook to initialize the user on ACC |
| `04_install_flow.yaml` | Appliance owner can use this playbook to install and activate an appliance |

### File Structure of other_usecases_ansible

This directory is used for other use cases associated with ACC and appliances.

| File | Purpose |
|:---------|:--------|
| `env_vars.yaml` | This file contains variables used by the environment, e.g., to install 2x SSAs |
| `owner_vars.yaml` | This file contains variables used by the appliance owners regarding their appliances |
| `acc_ssa_install_check_vars.yaml` | This file contains variable used for checking ACC and 2x SSA installations |
| `00_resource_scan.yaml`| Appliance owner can use this playbook to gather information about the resources assigned and consumed by the owner |
| `01_upgrade_flow.yaml` | Appliance owner can use this playbook to upgrade an appliance, which will format the disk and install a new appliance |
| `02_sync_demo.yaml` | Appliance owner can use this playbook to observe how syncing works between ACC and the HMC in the default ACC mode |
| `03_pull_scc_logs.yaml` | ACC administrator can use this playbook to pull logs out of an SSC appliance like ACC |
| `04_managed_appliance_update.yaml` | Appliance owner can use this playbook to update currently running appliances |
| `05_managed_appliance_health_and_pull_logs.yaml` | Appliance owner can use this playbook to gather health status of the appliances and pull their logs |
| `06_acc_appliance_update.yaml` | ACC administrator can use this playbook to update ACC |
| `07_ssa_install_e2e.yaml` | This playbook can be use end-to-end, for initializing ACC, and installing and activating 2x SSAs |
| `08_acc_ssa_install_check.yaml` | This playbook can be used to check and validate that the ACC and 2x SSAs are active and reachable |
| `09_insert_hmc_creds.yaml` | ACC administrator can use this playbook to insert HMC credentials into ACC when ACC is in the default mode |

### File Structure of acc_install_ansible

This directory contains scripts that help with installation of ACC. The user will just
call the playbook for installing ACC, and the playbook will call these scripts.
Therefore, it is not expected for the user to modify or deeply understand the
process behind these scripts. However, it is recommended to read the `README.md` file
before continuing with the installation of ACC.

## ACC Features and Limitations

It is recommended to read the release notes of each ACC release for
up-to-date information about ACC and the list of features, limitations,
and future plans.

We provide a brief overview of these aspects but they might not be
up-to-date.

### List of ACC Features

- Appliance management (install, activate, deactivate, update) on DPM and non-DPM mode machine.
- Working in default mode (ACC connected to the HMC) and standalone mode (ACC not connected to the HMC).
- Uploading appliances for installation.
- Basic sanity checks before installation.
- Appliance upgrade (complete image replacement) and updates.
- Appliance cluster management actions (install, activate, deactivate, update).
- Downloading logs of appliances.
- Health monitoring of appliances.
- ACC user management.
- Sync with HMC on LPARs running appliances in default mode.
- Rest APIs and sample ansible scripts for managing appliances.
- Basic UI functions for managing appliance.
- Multifactor authentication.
- Providing an opportunity to the ACC-Admin to upload certificates.

### Current Limitations of ACC

See the notes [here](https://www.ibm.com/docs/en/module_1721331501652/pdf/SC28-7067-00.pdf).

## Troubleshooting

As a result of executing the playbooks in this repository, the ACC LPAR appliance may occasionally enter a failed or unresponsive state. You can use the [ACC API](https://www.ibm.com/docs/en/systems-hardware/zsystems/9175-ME1?topic=guide-api-reference) to restore the appliance to a healthy condition.

Refer to the [Troubleshooting](https://www.ibm.com/docs/en/systems-hardware/zsystems/9175-ME1?topic=guide-troubleshooting)
chapter of the user-guide.

Here are some useful examples:

### Obtaining a Token

You will need a token to communicate with ACC. To generate the token, you can use
the following API example.

```bash
curl -k -X 'POST' \
  "https://${ACC_IP}:${ACC_PORT}/api/user/token" \
  -H 'accept: application/json' \
  -H 'Content-Type: application/json' \
  -d '{
  "username": "your_cc_owner_username",
  "password": "you_cc_owner_user_password"
}
```

This will return a JSON object, with `access_token` field containing the token.
You can copy the `access_token` into a variable like `TOKEN` and use it later.

### Deleting the Resource Package

If as an ACC-admin, you have assigned wrong resources to the appliance-owner,
you must delete and re-create the resource package.

For that purpose, use the following resource package deletion API example below.

```bash
curl -k -X 'DELETE' \
  "https://$ACC_IP:$ACC_PORT/api/resource/pkgs/${RESOURCE_PKG}?owner=${ACC_OWNER1_USERID}" \
  -H 'accept: */*' \
  -H "Authorization: Bearer $ADMIN_TOKEN" \
  -H 'Content-Type: application/json'
```

If the resource package cannot be deleted because of already activated LPARs,
then use the `force=true` parameter like:

```bash
/api/resource/pkgs/${RESOURCE_PKG}?owner=${ACC_OWNER1_USERID}&force=true
```

Afterwards, you can re-create the resource package for the owner.
