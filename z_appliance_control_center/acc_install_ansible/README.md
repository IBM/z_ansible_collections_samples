# Appliance Control Center Installer Ansible Script

## Preparation

#### Verify OpenSSL Installation
  Before proceeding, ensure that OpenSSL is installed and correctly linked to your Python environment
  Activate your virtual environment and check the OpenSSL version with the following commands:

  source /path/to/venv/bin/activate
  python -c "import ssl; print(ssl.OPENSSL_VERSION)"

  You should see output similar to:
  ```
  OpenSSL 3.5.1 1 Jul 2025
  ```

  If OpenSSL is not installed or an error appears (e.g., ModuleNotFoundError: No module named 'ssl'), follow these steps:
  Install OpenSSL via Homebrew (on macOS):
  ```
  brew install openssl@3
  ```

  Reinstall Python ensuring it is correctly linked to OpenSSL (you can use pyenv or system installation).
  Recreate your virtual environment:
  ```
  python -m venv /path/to/venv
  ```

#### Activate and Configure the LPAR
- Ensure LPAR is activated and has updated the activation profile with correct values of network settings 
(e.g. `chpid`, `prefix`, etc.) and storage (initial 16 GB storage is required for ACC).

#### Install Ansible on Control Nodes
- Both ACC-admin and appliance-owners must
  [install ansible](https://docs.ansible.com/ansible/latest/installation_guide/intro_installation.html) on
  their respective control nodes.
  - For example, on your laptop that will connect with ACC, install `ansible` via:
  
    ```
    pip install ansible
    ```
  
#### Download Installation Scripts
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
