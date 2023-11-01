# IMS Provisioning for Wazi environment

The IMS provisioning playbook samples demonstrate how to allocate the required
data sets and configure them to provision IMS and related services.

It is a good practice to review the playbook sample contents before executing
them. It will help you understand the requirements in terms of space, location,
names, authority, and the artifacts that will be created and cleaned up.
Although samples are written to operate without the need for the user’s
configuration, flexibility is written into the samples because it is not easy
to determine if a sample has access to the host’s resources. Review the
playbook notes sections for additional details and configuration.


## Playbook Summary

- [**provision-tmdb-wazi.yml**](provision-tmdb-wazi.yml)  - allocating required data sets and kicking off many IMS services.
- [**deprovision-tmdb-wazi.yml**](deprovision-tmdb-wazi.yml)  - deallocating data sets and stopping IMS services.
- [**query-ims.yml**](query-ims.yml) - provides examples of how to query status of different IMS services.  These examples utilize the roles defined below.

## Role Summary

This project uses roles to provide an object-oriented model to provision IMS.  Each role is responsible for a specific area.  These roles can be re-used in different playbooks.

- [**check_duplicate_ims**](roles/check_duplicate_ims) - Check if the IMSID requested is already up
- [**check_duplicate_imsplex**](roles/check_duplicate_imsplex) - Check if the IMSPlex requested is already up
- [**check_inputs**](roles/check_inputs) - Check if the IMSID, IMSPlex, and port numbers are valid
- [**check_port**](roles/check_port) - Check if port numbers requested are reserved or in use
- [**deprovsion_ims**](roles/deprovision_ims) - deprovisions IMS: shutdown and delete all datasets created by IMS provision playbook
- [**ims_apf**](roles/ims_apf) - adds authorization of IMS datasets to zOS
- [**ims_catalog**](roles/ims_catalog) - allocates, loads, and deletes IMS catalog
- [**ims_common**](roles/ims_common) - provides services such as start/stop IMS control regions and IMS Connection
- [**ims_common_queue**](roles/ims_common_queue) - provides dataset definition and query or CQS
- [**ims_dataset**](roles/ims_dataset) - creates and deletes IMS definition data sets and libraries
- [**ims_dbrc**](roles/ims_dbrc) - sets up DBRC defaults and prepares DBRC
- [**ims_exit**](roles/ims_exit) - prepares security and connection exit for IMS
- [**ims_gen**](roles/ims_gen) - prepares DBDGEN, PSBGEN, and ACBGEN
- [**ims_iefjobs**](roles/ims_iefjobs) - creates IEF jobs
- [**ims_initialize**](roles/ims_initialize) - reserves ICON ports and sends PROCLIB templates to zOS
- [**ims_online_change**](roles/ims_online_change) - enables the online change utility
- [**ims_operations_manager**](roles/ims_operations_manager) - defines, starts, stops, and queries operations manager
- [**ims_proclib**](roles/ims_proclib) - defines BPE configuration and copies PROCLIB
- [**ims_racf**](roles/ims_racf) - prepares RACF security for IMS
- [**ims_region**](roles/ims_region) - starts IMS regions
- [**ims_resource_manager**](roles/ims_resource_manager) - defines, starts, stops and queries RM
- [**ims_structured_call_interface**](roles/ims_structured_call_interface) - defines, starts, stops, and queries SCI
- [**ims_sysdef**](roles/ims_sysdef) - prepares system definition data set
- [**ims_tls**](roles/ims_tls) - configure AT-TLS ports
- [**install-bzip2**](roles/install-bzip2) - copies and installs utility zip file on zOS
- [**monitor_ims**](roles/monitor_ims) - checks to see if IMS is up or down.
- [**provision_ims**](roles/provision_ims) - this is the main role to provision IMS.  It utilizes other roles to allocate datasets, prepare datasets and start all IMS services
- [**save-templates-to-datasets**](roles/save-templates-to-datasets) - copies templates from USS to zOS data sets
- [**send-template**](roles/send-template) - sends a template file from the local host to zOS
- [**send-templates**](roles/send-templates)  - sends multiple templates in a directory from local host to zOS
- [**set_up_run_environment**](roles/set_up_run_environment)  - setting up the initial environment for playbook execution
- [**stop_jmp**](roles/stop_jmp) - stop JMP (Java) region
- [**stop_mpp**](roles/stop_mmp) - stop MPP region
- [**submit-rexx**](roles/submit-rexx) - runs a REXX script on zOS


Inside each role:

* `./tasks` contains tasks that can be performed by the role.
  Sub-folder names describe the use of the contained files.
* `./tasks/main.yml` contains the default tasks performed by the role.


## Requirements
* IBM z/OS core collection 1.5.0
* IBM z/OS IMS collection 1.2.0
* IBM® Wazi Sandbox 2.4 or IBM® Extended z/OS® ADCD for Z Development and Test Environment built upon the general release of ADCD z/OS® V2R5 December Edition of 2022 or later

## Getting Started

If you are unfamiliar with playbooks, you can review our
[detailed configuration guide](../../docs/share/configuration_guide.md) or
continue with getting started below.



## Getting Started: CLI

A few settings may need to be changed to ensure compatibility with your z/OS target.

For more information on python configuration requirements on z/OS, refer to [Ansible FAQ: Running on z/OS](https://docs.ansible.com/ansible/latest/reference_appendices/faq.html).


1. Update [inventories/zosendpoint](inventories/zosendpoint) contains the information needed to connect to our target. We must specify the following information about our target system:
     * ansible_host: either an IP or URL to the target system.
     * ansible_user: the username used to login with SSH.
     * ansible_python_interpreter: the path on the target to the python interpreter.
     * ansible_port: the port number to login with SSH if it's not the default port 2022 for Wazi systems.
   * An example is below, where `zosendpoint` will be the name used to reference our target:

    ```yaml
    all:
      hosts:
        zosendpoint:
          ansible_host: your_host
          ansible_user: ibmuser
          ansible_port: 2022
          ansible_python_interpreter: /usr/lpp/IBM/cyp/v3r9/pyz/bin/python3

    ```
2. Update the playbook specific variables in [vars_wazi/ims-dbdc.yml](vars_wazi/ims-dbdc.yml) based on desired behaviors. Some of the commonly changed variables are:
    1. **DFS_IMS_SSID** for IMSID
    2. **ODBM_PORTIDPort** and **ODBM_SSLPORTID** for ODBM ports
    3. **IMS_CONNECT_PORT** and **IMS_CONNECT_SSLPORT** for IMS Connect ports
    4. **DFS_IMSPlex** for IMSPlex name


## Run the playbook

1. Run provisioning IMS playbook, type the following from the root of this repository:

`ansible-playbook -i inventories/zosendpoint provision-tmdb-wazi.yml`

1. Run query IMS services, type the following from the root of this repository:

`ansible-playbook -i inventories/zosendpoint query-ims.yml`

1. Run de-provisioning IMS playbook, type the following from the root of this repository:

`ansible-playbook -i inventories/zosendpoint deprovision-tmdb-wazi.yml`

## Copyright

© Copyright IBM Corporation 2023

## License
Licensed under [Apache License](https://opensource.org/licenses/Apache-2.0).

