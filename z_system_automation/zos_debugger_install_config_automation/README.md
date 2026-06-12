# Debug-Config-Automation

Using Ansible to install and configure the IBM z/OS Debugger

## 📚 Documentation

For comprehensive installation instructions, configuration details, troubleshooting guides, and use case scenarios, please refer to the detailed documentation available in multiple formats:

- **[User Guide (Markdown)](doc/User_Guide.md)** - Easy to read in GitHub/VS Code
- **[User Guide (Word Document)](doc/User's%20Guide.docx)** - Downloadable DOCX format for offline reference

### What's in the User Guide?

The User Guide (available in both Markdown and DOCX formats) provides extensive documentation including:

- **Complete Prerequisites** - Detailed software requirements, compatibility matrices, and system requirements
- **Step-by-Step Installation** - Comprehensive setup instructions with examples
- **Configuration Reference** - Complete variable descriptions and configuration templates
- **Use Case Scenarios** - Recovery, upgrades, multi-region setups, and rollback procedures
- **Post-Installation Verification** - Detailed verification checklists and functional testing
- **Troubleshooting** - Common issues, solutions, and diagnostic commands
- **Appendices** - Variable reference tables, port mappings, dataset conventions, JCL templates, and quick reference commands

### Quick Start

For a quick start, follow the instructions below. For detailed information, consult the [User Guide](doc/User_Guide.md).

## Prerequisites

Before running the automation scripts, make sure the following software and versions are installed:

1. **Ansible** >= 2.18.7
2. **Ansible IBM z/OS Core** >= 1.14.1
3. **Python** >= 3.1.x

## Setup

### 1. **Download Required Installation Files**

   ⚠️ **IMPORTANT:** The IBM z/OS Debugger installation files are NOT included in this repository.

   Before proceeding, you must:

   1. **Download the required files:**
      - `HADRXXX.pax.Z` - Base product RELFILES
      - `IBM.HADRXXX.PTF####` - Latest PTF package (or newer version)

   2. **Place the files** in the `files/` directory of this repository

   3. **Update `variables.yml`** with the correct file names:
      ```yaml
      ptf_file: IBM.HADRXXX.PTF####    # Update if using different PTF
      SMP_file: HADRXXX.pax.Z          # Update if filename differs
      ```

   📖 **For detailed download instructions and file requirements, see [files/README.md](files/README.md)**

### 2. **Install and Configure Ansible.**

   Install the Ansible IBM z/OS Core collection:

   ```
   ansible-galaxy collection install ibm.ibm_zos_core
   ```

### 2. **Establish SSH Connectivity to z/OS**

   To enable SSH connectivity to the z/OS server, you'll need to add your public key to the z/OS machine for passwordless login. Use the following command to copy your SSH key to the z/OS server:

   ```
   ssh-copy-id -p <<port>> <<user>>@<<host>>
   ```
   * Replace << user >> with the appropriate username for the z/OS server.
   * Replace << host >> with the z/OS server hostname.
   * Replace << port >> with the SSHD port for the z/OS server.

### 3. **Verify SSH Connection**

   Ensure that the SSH connection is established without requiring a password or private key:

   ```
   ssh -p <<port>> <<user>>@<<host>>
   ```
   If the connection is successful, you should be able to log in without being prompted for a password.

### 4. **Update Configuration Files**

   Update the following configuration files according to your environment:

   - **`variables.yml`** — Configure installation paths, file names, ports, and product settings
     - ⚠️ **CRITICAL:** Ensure `ptf_file` and `SMP_file` match your downloaded file names
   
   - `./inventories/group_vars/all.yml` — Update global variables that are applicable to all hosts

   - `./inventories/host_vars/zos_host.yml` — Update host-specific variables for the z/OS machine

   - `./inventories/inventory.yml` — Define your host inventory, specifying the z/OS machine and any other necessary hosts

## Execution Instructions

### Step 1: Verify File Setup
   - ✅ Confirm installation files are in the `files/` directory (see [files/README.md](files/README.md))
   - ✅ Verify `variables.yml` has correct file names matching your downloaded files

### Step 2: Configure Variables
   - Customize `variables.yml` according to your installation preferences
   - Update all paths, HLQs, ports, and environment-specific settings

### Step 3: Surrogate User as Root (if necessary)

   - If you need to surrogate your user as root, first run the `initialise.yaml` playbook. This will prepare your environment for running the RDS and DPS automation.
   - Run the following command:

     ```bash
     ansible-playbook initialise.yaml
     ```

### Step 4: Choose Your Installation Method

After completing the surrogate user setup (if required), you have two options to proceed:

#### **Option A: Complete Automated Installation (Recommended)**

Execute all installation and configuration steps automatically in the correct order using the main playbook:

```bash
ansible-playbook site.yml -i inventories/inventory.yml -vvv
```

This will automatically execute all 9 steps in sequence:
1. **Cleanup** - Remove previous installation artifacts
2. **SMP Install** - Install base product from RELFILES
3. **PTF Install** - Apply latest PTF fixes
4. **Configuration** - Register product and install SVC
5. **DPS Config** - Configure Debug Profile Service
6. **CICS Config** - Configure CICS integration
7. **Keystore Creation** - Create SSL/TLS keystores
8. **RDS Config** - Configure Remote Debug Service
9. **IMS Config** - Configure IMS integration

#### **Option B: Run Individual Playbooks Step-by-Step**

If you prefer more control or need to execute specific tasks only, you can run individual playbooks as per your preference:

#### **Cleanup (Step 1):**

   - To remove previous installation and start fresh:

     ```bash
     ansible-playbook Cleanup.yml -i inventories/inventory.yml -vvv
     ```
     This will delete the SMP files, the Debugger installation data sets, plus the Debugger files in zFS.

#### **SMP Installation (Step 2):**

   - To trigger the SMP Installation process:

     ```bash
     ansible-playbook SMPInstall.yml -i inventories/inventory.yml -vvv
     ```
     This will install the `files/HADRXXX.pax.Z` RELFILES to your environment.
     
     ⚠️ **Prerequisite:** Ensure `HADRXXX.pax.Z` exists in the `files/` directory (see [files/README.md](files/README.md))

#### **Apply Latest PTF (Step 3):**

   - To trigger the PTF Installation process:

     ```bash
     ansible-playbook PTFInstall.yml -i inventories/inventory.yml -vvv
     ```
     This will install the `files/IBM.HADRXXX.PTF####` PTF to your environment.
     
     ⚠️ **Prerequisite:** Ensure the PTF file exists in the `files/` directory and `variables.yml` has the correct filename

#### **Debugger Configuration (Step 4):**

   - To trigger the Debugger Configuration process:

     ```bash
     ansible-playbook Configuration.yml -i inventories/inventory.yml -vvv
     ```
     This will register the product, install the SVC, and run the COBOL V6.x IVP program to verify the installation is working.

#### **DPS Configuration (Step 5):**

   - To configure and start the Debug Profile Service:

     ```bash
     ansible-playbook DPSConfig.yml -i inventories/inventory.yml -vvv
     ```
     This will create the STCEQA server id, add the necessary RACF security definitions, customize the DPS configuration files, and start the EQAPROF server.

#### **CICS Configuration (Step 6):**

   - To configure CICS integration:

     ```bash
     ansible-playbook CICSConfig.yml -i inventories/inventory.yml -vvv
     ```
     This will configure the z/OS Debugger for CICS environments.

#### **Keystore Creation (Step 7):**

   - To create SSL/TLS keystores:

     ```bash
     ansible-playbook KeystoreCreation.yml -i inventories/inventory.yml -vvv
     ```
     This will generate the necessary keystores for secure communication.

#### **RDS Configuration (Step 8):**

   - To configure and start the Remote Debug Service:

     ```bash
     ansible-playbook RDSConfig.yml -i inventories/inventory.yml -vvv
     ```
     This will configure the RDS server for remote debugging capabilities.

#### **IMS Configuration (Step 9):**

   - To configure IMS integration:

     ```bash
     ansible-playbook IMSConfig.yml -i inventories/inventory.yml -vvv
     ```
     This will configure the z/OS Debugger for IMS environments.

## Troubleshooting

For common issues and detailed troubleshooting steps, please refer to the **[Troubleshooting Section](doc/User_Guide.md#troubleshooting)** in the User Guide.

### Quick Troubleshooting Tips

- **SSH Connectivity Issues:** Ensure your SSH public key is correctly added to the z/OS machine using `ssh-copy-id`, or manually add the public key to the `.ssh/authorized_keys` file

- **Permission Issues:** Make sure the necessary files have the correct permissions and that the user running the scripts has appropriate access

- **Job Failures:** Check job output on z/OS using SDSF or z/OSMF, and review JCL for errors

- **Port Conflicts:** Use `netstat -a | grep <port>` to check if ports are already in use

For detailed diagnostic commands and solutions, see the [User Guide](doc/User_Guide.md#troubleshooting).

## Additional Resources

- **[IBM z/OS Debugger Documentation](https://www.ibm.com/docs/en/developer-for-zos)** - Official product documentation
- **[Ansible Documentation](https://docs.ansible.com/)** - Ansible automation platform
- **[IBM z/OS Core Collection](https://galaxy.ansible.com/ibm/ibm_zos_core)** - Ansible collection for z/OS

## Installation Time

- **Complete Automated Installation:** 30-45 minutes
- **Manual Installation (without automation):** 4-6 hours

## Support

For issues, questions, or contributions, please refer to the [User Guide](doc/User_Guide.md) or contact the development team.

---

**Developed By:** Edison Kwok, Sindhu Prakash, Pavithra Velusamy

**Version:** 1.0

**Compatible with:** IBM z/OS Debugger VHR0M0
