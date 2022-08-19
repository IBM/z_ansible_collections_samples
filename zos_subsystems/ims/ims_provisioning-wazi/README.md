# IMS Provisioning

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

- [**ims-tmdb.yml**](ims-tmdb.yml)  - handles both allocating and deallocating required data sets and kicking off many IMS services.
- [**query-ims.yml**](query-ims.yml) - provides examples of how to query status of different IMS services.  These examples utilize the roles defined below.

## Role Summary

This project uses roles to provide an object-oriented model to provision IMS.  Each role is responsible for a specific area.  These roles can be re-used in different playbooks.

- [**deprovsion_ims**](roles/deprovision_ims/README.md) - deprovisions IMS: shutdown and delete all datasets created by IMS provision playbook
- [**ims_apf**](roles/ims_apf/README.md) - adds authorization of IMS datasets to zOS

- [**ims_catalog**](roles/ims_catalog/README.md) - allocates, loads, and deletes IMS catalog
- [**ims_common**](roles/ims_common/README.md) - provides services such as start/stop IMS control regions and IMS Connection
- [**ims_common_queue**](roles/ims_common_queue/README.md) - provides dataset definition and query or CQS
- [**ims_dataset**](roles/ims_dataset/README.md) - creates and deletes IMS definition data sets and libraries
- [**ims_dbrc**](roles/ims_dbrc/README.md) - sets up DBRC defaults and prepares DBRC
- [**ims_exit**](roles/ims_exit/README.md) - prepares security and connection exit for IMS
- [**ims_gen**](roles/ims_gen/README.md) - prepares DBDGEN, PSBGEN, and ACBGEN
- [**ims_iefjobs**](roles/ims_iefjobs/README.md) - creates IEF jobs
- [**ims_initialize**](roles/ims_initialize/README.md) - reserves ICON ports and sends PROCLIB templates to zOS
- [**ims_online_change**](roles/ims_online_change/README.md) - enables the online change utility
- [**ims_operations_manager**](roles/ims_operations_manager/README.md) - defines, starts, stops, and queries operations manager
- [**ims_proclib**](roles/ims_proclib/README.md) - defines BPE configuration and copies PROCLIB
- [**ims_racf**](roles/ims_racf/README.md) - prepares RACF security for IMS
- [**ims_region**](roles/ims_region/README.md) - starts IMS regions
- [**ims_resource_manager**](roles/ims_resource_manager/README.md) - defines, starts, stops and queries RM
- [**ims_structured_call_interface**](roles/ims_structured_call_interface/README.md) - defines, starts, stops, and queries SCI
- [**ims_sysdef**](roles/ims_sysdef/README.md) - prepares system definition data set
- [**install-bzip2**](roles/install-bzip2/README.md) - copies and installs utility zip file on zOS
- [**monitor_ims**](roles/monitor_ims/README.md) - checks to see if IMS is up or down.
- [**provision_ims**](roles/provision_ims/README.md) - this is the main role to provision IMS.  It utilizes other roles to allocate datasets, prepare datasets and start all IMS services
- [**save-templates-to-datasets**](roles/save-templates-to-datasets/README.md) - copies templates from USS to zOS data sets
- [**send-template**](roles/send-template/README.md) - sends a template file from the local host to zOS
- [**send-templates**](roles/send-templates/README.md)  - sends multiple templates in a directory from local host to zOS
- [**stop_jmp**](roles/stop_jmp/README.md) - stop JMP (Java) region
- [**stop_mpp**](roles/stop_mmp/README.md) - stop MPP region
- [**submit-rexx**](roles/submit-rexx/README.md) - runs a REXX script on zOS


Inside each role:

* `./tasks` contains tasks that can be performed by the role.
  Sub-folder names describe the use of the contained files.
* `./tasks/main.yml` contains the default tasks performed by the role.


## Ansible Collection Requirements
* IBM z/OS core collection 1.4.0
* IBM z/OS IMS collection 1.2.0

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
     * ansible_port: the port number to login with SSH if it's not the default port 22.
   * An example is below, where `zosendpoint` will be the name used to reference our target:

    ```yaml
    all:
      hosts:
        zosendpoint:
          ansible_host: ec0000a.vmec.svl.ibm.com
          ansible_user: ibmuser
          ansible_python_interpreter: /usr/lpp/IBM/cyp/v3r9/pyz/bin/python3
          ansible_port: 2022 

    ```

2. Update the environment variables for the z/OS system in [host_vars/zosendpoint.yml](host_vars/zosendpoint.yml)
    ```yaml
    # the path to the root of IBM python installation
    PYZ: "/usr/lpp/IBM/cyp/v3r9/pyz"

    # the path to root of ZOAU installation
    ZOAU: "/usr/lpp/IBM/zoautil"
    ```

3. Update the playbook specific variables in [host_vars/zosendpoint.yml](host_vars/zosendpoint.yml)based on desired behavior


## Run the playbook

1. Run provisioning IMS playbook, type the following from the root of this repository:

`ansible-playbook -i inventories/zosendpoint ims-tmdb.yml `

2. Run query IMS services, type the following from the root of this repository:

`ansible-playbook -i inventories/zosendpoint query-ims.yml`

3. Run de-provisioning IMS playbook, type the following from the root of this repository:

`ansible-playbook -i inventories/zosendpoint ims-dbdc.yml -e "{ ims_function: delete }" `

## Copyright

© Copyright IBM Corporation 2022

## License
Licensed under [Apache License](https://opensource.org/licenses/Apache-2.0).

