# Appliance Control Center Installer Ansible Script

## Preparation

- Ensure LPAR is activated and has updated the activation profile with correct values of network settings 
(e.g. `chpid`, `prefix`, etc.) and storage (initial 16 GB storage is required for ACC).

- Both ACC-admin and appliance-owners must
  [install ansible](https://docs.ansible.com/ansible/latest/installation_guide/intro_installation.html) on
  their respective control nodes.
  - For example, on your laptop that will connect with ACC, install `ansible` via:
  
    ```
    pip install ansible
    ```
- Download all these `yaml` files to a directory on the control node (e.g., a laptop) which
  will connect with ACC.

## Appliance Installation

To set up the ACC, the following actions must be performed by the ACC-admin.

- Export your HMC username and password on a terminal in your laptop:
  ```
  export HMC_USER=<enter_HMC_username>
  export HMC_PASSWORD=<enter_HMC_password>
  ```

- Update the variables in the file (`sample_ssa/acc_env_vars.yaml`):

  - Change the `IMAGE_PATH` and `INSTALL_SCRIPT_PATH` in the `acc_env_vars.yaml` file to point to the right one
  - Change the `CPC, LPAR, LPAR_IP, DISK_ID` in the `acc_env_vars.yaml` file to point to the right IP address
  - Update other values accordingly to point to the right LPAR

- Modify the variables for the LPAR that appliance-owner will use for installing the appliance.


- Run the playbook via:
  ```
  ansible-playbook ./sample_ssa/acc_install.yaml
  ```
