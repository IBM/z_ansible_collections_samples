# Samples for IBM z/OS CICS collection

[Documentation site](https://ibm.github.io/z_ansible_collections_doc/ibm_zos_cics/docs/ansible_content.html) | [CICS collection on Galaxy](https://galaxy.ansible.com/ibm/ibm_zos_cics) | [CICS collection on Automation Hub](https://cloud.redhat.com/ansible/automation-hub/repo/published/ibm/ibm_zos_cics)

This repository provides several CICS Ansible playbooks that show how to use the CICS collection for real-life use cases. Samples that relate to resource management activities are provided in the **cmci** folder. Samples that relate to CICS region provisioning operations are provided in the **provisioning** folder.

## Playbooks summary

1. [Retrieving operational data from running CICS regions](cmci/reporting)

    The `reporting` sample gives a good overview of how to get started with the Ansible tasks provided by the CICS collection.
    
    This sample shows how to retrieve data from running CICS regions and format the data for use in dashboarding or ad hoc analysis. Because it uses only the HTTP-based CMCI REST API, you can run the sample easily from a workstation without any setup on z/OS.

1. [Deploying a program to a CICS region](cmci/deploy_program)

    The `deploy_program` sample shows how you can copy a load module from a build data set to a library used by CICS, and then NEWCOPY the program in CICS.

    This sample uses the z/OS core collection in concert with the CICS collection, within one playbook.

1. [Customising when a CMCI module should fail](cmci/override_failure)

    The `override_failure` sample shows how to override the default error when searching for a program that doesn't exist in CICS.

    The Ansible tasks provided by the CICS collection have automatic awareness of failure criteria, such as HTTP errors or actions being applied but with no matching resources. However, sometimes you want to override that behaviour and carry on despite a failure, as this sample shows.

1. [Resource lifecycle and CSD](cmci/resource_lifecycle_and_csd)

    The `resource_lifecycle_and_csd` sample shows how to perform a full CICS resource lifecycle with the CMCI modules, including creating and installing a definition from the CSD.

1. [Custom CA for HTTPS connections](cmci/set_ca)

    The `set_ca` sample shows how to use the CMCI modules with a custom CA bundle, typically useful when you have an internal CA.

1. [Refresh tags](cmci/refresh_tags)

    The `refresh_tags` sample shows how to instruct a CICS region to refresh
    its tags by re-reading the tags file on USS.  This is achieved by using
    the `cmci_action` module.  This sample also documents how to determine
    the appropriate parameters for the `cmci_action` module for the action
    you're trying to run.

1. CICS region provisioning samples

    Detailed instructions are provided in this [documentation](provisioning/), including the requirements and prerequisites you must satisfy. These samples show how to use the modules in the CICS collection to provision a CICS region from scratch:

   * [full_provision.yml](provisioning/full_provision.yml) is an Ansible playbook that can create all the required data sets and start a stand-alone CICS region. 
   * [full_provision_smss.yml](provisioning/full_provision_smss.yml) is an Ansible playbook that can create all the required data sets and start a CICS region that has the CMCI feature.
   * [deprovision.yml](provisioning/deprovision.yml) is an Ansible playbook that stops a running CICS region and removes all its data sets. 

   In addition, the following artifacts are supplied in support for these samples: 

   * [inventory.yml](provisioning/inventories/inventory.yml) contains an Ansible inventory for your provisioning. 
   * [variables.yml](provisioning/host_vars/variables.yml) contains the required environment variables needed by the z/OS Ansible collections (including the CICS collection) to execute the Ansible payload on z/OS UNIX. They also include the path to the Python and ZOAU installations on z/OS UNIX.

---

These CICS samples are just some of the samples available for the Red Hat Ansible Certified Content for IBM Z. You can find samples covering other aspects at the [root of the repository](https://github.com/IBM/z_ansible_collections_samples).
