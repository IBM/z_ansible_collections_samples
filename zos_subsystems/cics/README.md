# Samples for the IBM z/OS CICS collection

[Documentation site](https://ibm.github.io/z_ansible_collections_doc/ibm_zos_cics/docs/ansible_content.html) | [CICS collection on Galaxy](https://galaxy.ansible.com/ibm/ibm_zos_cics) | [CICS collection on Automation Hub](https://cloud.redhat.com/ansible/automation-hub/repo/published/ibm/ibm_zos_cics)

This repository provides a number of samples that show how to use the CICS collection for real-life use cases:

1. [Retrieving operational data from running CICS regions](cmci/reporting)

    The `reporting` sample gives a good overview of how to get started with the tasks provided by the CICS collection.
    
    This sample shows how to retrieve data from running CICS regions and format it for use in dashboarding or ad hoc analysis.

    Because this sample only uses the HTTP-based CMCI REST API, it can easily be run from a workstation rather than needing any setup on z/OS.

1. [Deploying a program to a CICS region](cmci/deploy_program)

    The `deploy_program` sample shows how you can copy a load module from a build data set to a library used by CICS, and then NEWCOPY the program in CICS.

    This sample uses the z/OS core collection in concert with the CICS collection, within the one playbook.

1. [Customising when a CMCI module should fail](cmci/override_failure)

    The `override_failure` sample shows how to override the default error when searching for a program that doesn't exist in CICS.

    The tasks provided by the CICS collection have automatic awareness of failure criteria, such as HTTP errors or actions being applied but zero resources matching the criteria. However, sometimes you want to override that behaviour and carry on despite a failure, as this sample shows.

1. [Resource lifecycle and CSD](cmci/resource_lifecycle_and_csd)

    The `resource_lifecycle_and_csd` sample shows how to perform full CICS resource lifecycle with the CMCI modules, creating and installing a definition from CSD.

1. [Custom CA for HTTPS connections](cmci/set_ca)

    The `set_ca` sample shows how to use the CMCI modules with a custom CA bundle, typically useful when you have an internal CA.

---

These CICS samples are just some of the samples available for the Red Hat Ansible Certified Content for IBM Z. You can find samples covering other aspects at the [root of the repository](https://github.com/IBM/z_ansible_collections_samples).
