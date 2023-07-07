# Provision IMS Catalog and IMS Management of Application Control Blocks (ACBs)

This project provides a sample playbook and roles, which can be used to set up [IMS catalog](https://www.ibm.com/docs/en/ims/15.3.0?topic=catalog-overview-ims) and [IMS-managed ACBs](https://www.ibm.com/docs/en/ims/15.3.0?topic=tailoring-ims-management-acbs) onto an existing IMS instance.

It is a good practice to review the playbook sample contents before executing
them. It will help you understand the requirements in terms of space, location,
names, authority, and the artifacts that will be created and cleaned up.
Although samples are generally written to operate with as little of the user's 
configuration as possible, flexibility is written into the samples because it 
is not easy to determine if a sample has access to the host’s resources. 
Review the playbook notes sections for additional details and configuration.

## Playbook Summary

- [**provision_catalog.yml**](provision_catalog.yml) - Handles the following tasks:
  - Shutting down the existing IMS instance
  - Cleaning up any existing conflicting IMS catalog data sets
  - Allocating the necessary directory and partitioned data sets for IMS catalog
  - Copying the necessary database descriptions (DBDs) and program specification blocks (PSBs) to the DBDLIB and PSBLIB data sets
  - Running the access control blocks generator (ACBGEN) and activating the ACB library 
  - Registering the IMS catalog to the IMS Database Recovery Control facility (DBRC)
  - Adding the `CATALOG` section to the `DFSDFxxx` member
  - Loading IMS catalog with the IMS Catalog Populate utility (DFS3PU00)
  - Create [image copies](https://www.ibm.com/docs/en/ims/15.3.0?topic=copies-image-ims-image-copy-utilities) by using the [Database Image Copy utility (DFSUDMP0)](https://www.ibm.com/docs/en/ims/15.3.0?topic=utilities-database-image-copy-utility-dfsudmp0). 
    - This maintains the integrity of IMS catalog if you provision IMS-managed ACBs.
  - Changing the `CATALOG` attribute of the `DFSDFxxx` member from `N` to `Y`
  - Restarting IMS
  - Running a test application program to verify the provisioning of IMS catalog

- [**provision_macb.yml**](provision_macb.yml) - Handles the following tasks:
  - Shutting down the existing IMS instance
  - Making necessary changes to the `DFSDFxxx` member  
  - Allocating log data sets
  - Updating access to the IMS catalog by using the IMS Catalog Populate utility (DFS3PU00)
  - Changing the `ACBMGMT` attribute of the `DFSDFxxx` member from `ACBLIB` to `CATALOG`
  - Restarting IMS
  - Verifying the enablement of IMS-managed ACBs through the `QUERY MEMBER` command

## Role Summary
- [**shut_down**](roles/shut_down/README.md)- Contains tasks to shut down IMS.
- [**restart**](roles/restart/README.md)- Contains tasks restart IMS.

### catalog
- [**clean_up**](roles/catalog/clean_up/README.md)- Contains tasks to:
  - Clean up the IMS instance data sets to prepare for the provision of IMS catalog 
  - Clean up the environment after IMS catalog is provisioned.
- [**allocate_datasets**](roles/catalog/allocate_datasets/README.md)- Allocate necessary data sets for IMS catalog.
- [**copy_dbd_psb**](roles/catalog/copy_dbd_psb/README.md)- Copy DBDs and PSBs.
- [**acbgen_activate**](roles/catalog/acbgen_activate/README.md)- Perform ACBGEN and activate the ACB library.
- [**register_with_dbrc**](roles/catalog/register_with_dbrc/README.md)- Register IMS catalog with DBRC.
- [**dfsdf_changes**](roles/catalog/dfsdf_changes/README.md)- Make changes to the `DFSDFxxx` member.
- [**populate_utility**](roles/catalog/populate/README.md)- Run the IMS Catalog Populate utility (DFS3PU00).
- [**image_copy**](roles/catalog/image_copy/README.md)- Create image copies of the loaded IMS catalog data sets.
- [**confirm**](roles/catalog/confirm/README.md)- Verify the provision of IMS catalog through running a test application program.

### mACB
- [**allocate_datasets**](roles/catalog/allocate_datasets/README.md)- Allocate log data sets for IMS-managed ACBs.
- [**dfsdf_changes**](roles/mACB/dfsdf_changes/README.md)- Make changes to the `DFSDFxxx` member.
- [**populate_utility**](roles/mACB/populate/README.md)- Run the IMS Catalog Populate utility (DFS3PU00).

## Requirements 

IBM Open Enterprise SDK for Python: 3.9

ansible-core: 2.11.1

[Z Open Automation Utilities](https://www.ibm.com/docs/en/wdfrhcw/1.4.0?topic=components-z-open-automation-utilities): 1.1.1

[IBM&reg; z/OS&reg; core collection 1.4.0 or later](https://galaxy.ansible.com/ibm/ibm_zos_core)

[IBM&reg; z/OS&reg; IMS collection 1.2.0](https://galaxy.ansible.com/ibm/ibm_zos_ims)

## Getting Started

If you are unfamiliar with playbooks, you can review our
[detailed configuration guide](https://github.com/IBM/z_ansible_collections_samples/blob/master/docs/share/zos_core/configuration_guide.md) or continue with the steps below.


1. Update [inventories/inventory.yml](inventories/inventory.yml) with your system information.

```yaml
zsystem:
  hosts:
    zos_host:
      ansible_host: zos_target_address
      ansible_user: zos_target_username
      ansible_python_interpreter: /python/usr/lpp/IBM/cyp/v3r8/pyz/bin/python3

```

2. Update the environment variables for the z/OS&reg; system in [host_vars/zos_host.yml](host_vars/zos_host.yml)

 
```yaml
# The path to the root where IBM python is installed
PYZ: "path_to_python_installation_on_zos_target"


# The path to root where ZOAU is installed
ZOAU: "path_to_zoau_installation_on_zos_target"
```

3. Update the user information for job submissions in [host_vars/zos_host.yml](host_vars/zos_host.yml)

```yaml
TARGET_USERNAME: target_user
```

4. Update the IMS information in [host_vars/zos_host.yml](host_vars/zos_host.yml) 

```yaml
# The SSID of the IMS system to be provisioned
DFS_IMS_SSID: IMS1

# IMSPlex name
DFS_IMS_PLEX: PLEX1

# The first HLQ to be used to store IMS related data sets
DFS_AUTH_LIB_HLQ1: IMSTESTL

# The fully qualified data set name for the IMS PROCLIB
DFS_IMS_PROCLIB: '{{ DFS_AUTH_LIB_HLQ1 }}.{{ DFS_AUTH_LIB_HLQ2 }}.PROCLIB'

# Data set volume
DFS_DS_VOLUME1: USER01

# Data set unit
DFS_IMS_UNIT: SYSALLDA

# The DFSDFxxx member that the IMSPB points to
DFS_MEMBER_SUFFIX: '000'

# Buffer pool member
BUFFER_POOL_PARAMETER: DFSVSMHP
```

5. Update IMS address spaces in [host_vars/zos_host.yml](host_vars/zos_host.yml)

```yaml
DFS_IMS_SCI_PROC: IMS1SCI
DFS_IMS_OM_PROC: IMS1OM
DFS_IMS_RM_PROC: IMS1RM
DFS_IMS_CTL_PROC: IMS1CTL
DFS_HWS_PROC: IMS1HWS1
```

6. Update the IMS Connect Job ID in [host_vars/zos_host.yml](host_vars/zos_host.yml)

> **_NOTE:_** This playbook assumes that IMS Connect is configured and running. It will attempt to stop and restart IMS Connect in the [**shut_down**](roles/shut_down/README.md) and [**restart**](roles/restart/README.md) roles, but errors will be ignored if IMS Connect is not configured. 

```yaml
DFS_HWS_SSID: IMS1HWS
```


7. Update the image copy information in [host_vars/zos_host.yml](host_vars/zos_host.yml)

```yaml
# The name of the template JCL for the image copy
GENJCL_INPUT: ICJCL

# List of dictionaries for data sets that need an image copy
to_image_copy:
  - { dataset: DFSCD000, dbd: DFSCD01, dsn: '{{ DFS_IMS_HLQGROUP }}.DFSCD000.A00001', ddn: DFSCD01A, icdsn: IMSTESTL.DFSCD01.DFSCD01A.IC.CD01A }
  - { dataset: DFSCD000, dbd: DFSCD01, dsn: '{{ DFS_IMS_HLQGROUP }}.DFSCD000.B00001', ddn: DFSCD01B, icdsn: IMSTESTL.DFSCD01.DFSCD01B.IC.CD01B }
  - { dataset: DFSCD000, dbd: DFSCD01, dsn: '{{ DFS_IMS_HLQGROUP }}.DFSCD000.C00001', ddn: DFSCD01C, icdsn: IMSTESTL.DFSCD01.DFSCD01C.IC.CD01C }
...
```

8. Update DFSDF related variables in [host_vars/zos_host.yml](host_vars/zos_host.yml)

```yaml
# Boolean value that determines if a backup of DFSDFxxx member will be created 
BACKUP: true 

# Name of the backup file for IMS catalog
DFSDF_BACKUP_NAME: DFSDFBAC

# Name of the backup file for mACB
ACB_DFSDF_BACKUP_NAME: DFSDFBAB
```

9. Run the playbook

To run the **catalog** playbook:
```bash
ansible-playbook -i inventories/inventory.yml provision_catalog.yml
```

To run the **mACB** playbook:
```bash
ansible-playbook -i inventories/inventory.yml provision_macb.yml
```
## Expected Output 

### Catalog 
This playbook uses a [DL/I Test Application Program](https://www.ibm.com/docs/en/ims/15.3.0?topic=reference-dli-test-program-dfsddlt0) to verify the success of the provisioning of IMS catalog. It uses the [STATUS statement](https://www.ibm.com/docs/en/ims/15.3.0?topic=reference-status-statement) to print the `COMMENTS, CALL, COMPARE, PCB, and SEGMENT DATA` of one call. 

A successful playbook invocation is indicated by a return code of `0` from the test application program. This shows that the test application was able to access and query the IMS catalog.

Example of succesful output with verbosity set to `-vvv` can be found at [successful_catalog_example.txt](successful_catalog_example.txt).



### Managed ACB 
This playbook uses the `QUERY MEMBER` [command](https://www.ibm.com/docs/en/ims/15.3.0?topic=commands-query-member-command) to verify the success of the provision of managed ACBs. 

```yaml
QUERY MEMBER TYPE(IMS) SHOW(ATTRIB)
```
A successful playbook invocation includes the term `DIRECTORY` in the output of the `QUERY MEMBER` command. 

Example of successful output with verbosity set to `-vvv` can be found at [successful_macb_example.txt](successful_macb_example.txt) or pasted as follows. 
```yaml
 'NO-STM,DYNMODBLKS,DIRECTORY'
```


## Copyright
© Copyright IBM Corporation 2023

## License
Licensed under [Apache License, Version 2.0](https://opensource.org/licenses/Apache-2.0)

## Author Information
Stephanie Lieu - @steph-lieu

## Support
Refer to the [support section](https://github.com/IBM/z_ansible_collections_samples/blob/master/README.md#support) for more details.
