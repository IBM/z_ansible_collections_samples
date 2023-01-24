# IMS COBOL Application Deployment

This project provides the required sample playbooks and roles to deploy an IMS COBOL application.

It is a good practice to review the playbook sample contents before executing
them. It will help you understand the requirements in terms of space, location,
names, authority, and the artifacts that will be created and cleaned up.
Although samples are written to operate without the need for the user’s
configuration, flexibility is written into the samples because it is not easy
to determine if a sample has access to the host’s resources. Review the
playbook notes sections for additional details and configuration.

## Playbook Summary

- [**app-deploy.yml**](app-deploy.yml) - Handles copying the sample COBOL files to USS, creating application data sets, copy the contents to USS data sets, configure middleware resources, run gens (DBD,PSB,ACB), compile and link a COBOL application, load data, create IMS resources - DBs, TRANs and PGMs and start these resources, create a region and start the region.
- [**app-deprovision.yml**](app-deprovision.yml) - Handles stopping the IMS resources, deleting them, and stopping the region.
- [**app-redeploy.yml**](app-redeploy.yml) - Handles compiling and linking the COBOL application, refreshing IMS resources -  PGMs, TRANs and MPP region. Can rebuild and update ACB if specified via the `UPDATE_ACB` flag. Can also refresh or reload data if specified via the `UPDATE_DATA` flag.

## Role Summary

- [**ims_cobol_copy_extract_files**](roles/ims_cobol_copy_extract_files/README.md) - Holds tasks for copying the tar file from the control node to the managed node and extracting it.
- [**ims_cobol_create_app_datasets**](roles/ims_cobol_create_app_datasets/README.md) - Holds tasks for creating the application data sets.
- [**ims_cobol_execute_permissions_and_copy_ussDatasets**](roles/ims_cobol_execute_permissions_and_copy_ussDatasets/README.md) - Holds tasks to execute the permissions to the copied programs and copy the contents to USS data sets.
- [**ims_cobol_run_gens**](roles/ims_cobol_run_gens/README.md) - Holds tasks to run the gens - DBD, PSB and ACBs.
- [**ims_cobol_check_acblib**](roles/ims_cobol_check_acblib/README.md) - Holds tasks to check for the inactive ACBLIB.
- [**ims_cobol_copy_acblib_to_inactive_acb**](roles/ims_cobol_copy_acblib_to_inactive_acb/README.md) - Holds tasks to copy the members from the active to inactive ACBLIB.
- [**ims_cobol_compile_link_cobol**](roles/ims_cobol_compile_link_cobol/README.md) - Holds tasks to compile and link the COBOL and load programs.
- [**ims_cobol_create_dynamic_alloc**](roles/ims_cobol_create_dynamic_alloc/README.md) - Holds tasks to create dynamic allocation.
- [**ims_cobol_load_accounts**](roles/ims_cobol_load_accounts/README.md) - Holds tasks to load accounts.
- [**ims_cobol_load_customer_accounts**](roles/ims_cobol_load_customer_accounts/README.md) - Holds tasks to load customer accounts.
- [**ims_cobol_load_customer_data**](roles/ims_cobol_load_customer_data/README.md) - Holds tasks to load customer data.
- [**ims_cobol_load_history**](roles/ims_cobol_load_history/README.md) - Holds tasks to load history.
- [**ims_cobol_load_tsta**](roles/ims_cobol_load_tsta/README.md) - Holds tasks to tsta.
- [**ims_cobol_register_db_to_recons**](roles/ims_cobol_register_db_to_recons/README.md) - Holds tasks to register databases to recons.
- [**ims_cobol_define_app_resources**](roles/ims_cobol_define_app_resources/README.md) - Holds tasks to create and start IMS transactions and programs.
- [**ims_cobol_define_ims_databases**](roles/ims_cobol_define_ims_databases/README.md) - Holds tasks to create and start IMS databases.
- [**ims_cobol_define_mpp_region**](roles/ims_cobol_define_mpp_region/README.md) - Holds tasks to create and start an MPP region.
- [**ims_cobol_remove_old_registered_databases**](roles/ims_cobol_remove_old_registered_databases/README.md) - Holds tasks to delete the old registered databases.
- [**ims_cobol_stop_app_resources**](roles/ims_cobol_stop_app_resources/README.md) - Holds tasks to stop and delete IMS transactions and programs.
- [**ims_cobol_stop_ims_databases**](roles/ims_cobol_stop_ims_databases/README.md) - Holds tasks to stop and delete IMS databases.
- [**ims_cobol_stop_mpp_region**](roles/ims_cobol_stop_mpp_region/README.md) - Holds tasks to stop the MPP region.
- [**ims_cobol_refresh_ims_resources**](roles/ims_cobol_refresh_ims_resources/README.md) - Holds tasks to refresh IMS resources associated with the application (PGM, TRAN and MPP region).

## Ansible Collection Requirement

- IBM z/OS core collection 1.3.0 or later
- IBM z/OS IMS collection 1.1.0 or later

## Other Requirements

You must run the [IMS Provisioning playbook](https://github.com/IBM/z_ansible_collections_samples/tree/master/zos_subsystems/ims/ims_provisioning) before you run the COBOL Application deployment playbooks.

## Getting Started

If you are unfamiliar with playbooks, you can review our
[detailed configuration guide](https://github.com/IBM/z_ansible_collections_samples/blob/master/docs/share/configuration_guide.md) or
continue with getting started below.

Optionally, you can use the sample
[host_setup](https://github.com/IBM/z_ansible_collections_samples/blob/master/zos_administration/host_setup/README.md)
to discover and create your **inventory** and **host_vars** artifacts. Please
note that when you use **host_setup**, it generates a configuration
for the most common dependencies. Some playbooks require more customized
configurations; in this case, you can review the sample documentation and
add the additional required variables.

## Getting Started: CLI

A few settings may need to be changed to ensure compatibility with your z/OS target.

For more information on python configuration requirements on z/OS, refer to [Ansible FAQ: Running on z/OS](https://docs.ansible.com/ansible/latest/reference_appendices/faq.html).


1. Update [inventories/zvm](inventories/zvm) contains the information needed to connect to our target. We must specify the following information about our target system:
     * ansible_host: either an IP or URL to the target system.
     * ansible_user: the username used to login with SSH.
     * ansible_python_interpreter: the path on the target to the python interpreter.
   * An example is below, where `zsystem` will be the name used to reference our target:

    ```yaml
    zsystem:
      hosts:
        zvm:
          ansible_host: ec00000a.vmec.svl.ibm.com
          ansible_user: omvsadm
          ansible_python_interpreter: /python/usr/lpp/IBM/cyp/v3r8/pyz/bin/python3
    ```

2. Update the environment variables for the z/OS system in [host_vars/zvm.yml](host_vars/zvm.yml)

```yaml
# the path to the root of IBM python installation
PYZ: "/usr/lpp/IBM/cyp/v3r8/pyz"

# the path to root of ZOAU installation
ZOAU: "/usr/lpp/IBM/zoautil"
```

3. Update the playbook specific variables in [vars/ims_cobol.yml](vars/ims_cobol.yml), based on the behavior that you want.


## Run the playbook

1. Run the IMS COBOL deployment playbook, type the following from the root of this repository:

`ansible-playbook -i inventories/zvm app-deploy.yml`

2. Run the IMS COBOL redeployment playbook, type the following from the root of this repository:

`ansible-playbook -i inventories/zvm app-redeploy.yml`

3. Run the de-provisioning IMS COBOL playbook and type the following from the root of this repository:

`ansible-playbook -i inventories/zvm app-deprovision.yml`


# Copyright

© Copyright IBM Corporation 2021

# License

Licensed under [Apache License,
Version 2.0](https://opensource.org/licenses/Apache-2.0)

# Support

Please refer to the [support section](https://github.com/IBM/z_ansible_collections_samples/blob/master/README.md#support) for more
details.

