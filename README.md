# Ansible Z Playbook Repository
This repository provides playbooks that can be tailored to automate often
repeated IBM Z tasks, links to blogs, and other related media.

An [Ansible® playbook](https://docs.ansible.com/ansible/latest/user_guide/playbooks_intro.html#playbooks-intro)
consists of organized instructions that define work for a managed node to be
automated with Ansible. The playbooks in this
repository are written using the collections from
**Red Hat® Ansible® Certified Content for IBM Z®** offering. For a detailed
overview of the collections, refer to the
[documentation](https://ibm.github.io/z_ansible_collections_doc/index.html).

The collections can be found on
[Ansible Galaxy](https://galaxy.ansible.com/search?deprecated=false&keywords=ibm_z&order_by=-relevance&page=1)
and [Automation Hub](https://www.ansible.com/products/automation-hub).
For additional content, review our [community page](http://ibm.biz/BdfsTR).

This repository is frequently updated to refresh content. We recommend that you clone 
the repository and configure your GitHub
[notifications and subscriptions](https://docs.github.com/en/github/managing-subscriptions-and-notifications-on-github/about-notifications#notifications-and-subscriptions)
to receive ongoing updates about specific activity on GitHub.

## Contribute Ansible Playbooks

We are accepting Ansible playbooks from the community. These playbooks
should relate to **IBM Z**, and should either leverage the
**Red Hat Ansible Certified Content for IBM Z** collections, or highlight
additional capabilities and use-cases for **Z automation**. Check out the
[contribution guidelines](docs/share/contribution-guidelines.md) for more
information.

## Playbooks
Playbooks are organized by topics that align to tasks and use cases that are
often repeated and in need of automation.

### Z Topics
- Infrastructure Provisioning
    - [IBM Cloud Infrastructure Center](z_infra_provisioning/cloud_infra_center)
      - [Installing Red Hat OpenShift Container Platform](z_infra_provisioning/cloud_infra_center/ocp_upi/README.md)
- Systems Administration
  - [zHMC](z_systems_administration/zhmc)
    - [Gather facts for CPC and its adapters and partitions](z_systems_administration/zhmc/docs/usecase_playbooks.md)
    - [zHMC Concepts](z_systems_administration/zhmc/docs/module_playbooks.md)

### z/OS® Topics
- Administration
  - [Set Up Ansible Host Variables for Python and ZOAU](zos_administration/host_setup)
  - [Manage z/OS Users Using Ansible](zos_concepts/user_management/add_remove_user) - \[[Playback](https://mediacenter.ibm.com/media/Managing+z+OS+Users+with+Ansible+/1_bnud4enw)]
  - [Ansible Fact Gathering for z/OS](zos_basics/factgathering)
  - [Unarchive Pax and Set Custom Facts](zos_basics/unarchive_pax_and_set_custom_facts)
  - [Synchronize APF authorized libraries from a configuration cloned from GitHub](zos_concepts/program_authorization/git_apf) - \[[Playback](https://mediacenter.ibm.com/media/Using+Ansible+to+synchronize+z+OS+APF+libraries+from+a+GitHub+configuration+file/1_e6qsiwmg)]
  - [Manage z/OS Certificates](zos_concepts/certificate_management#readme)
- Basic Operations
  - [Data Set Basics](zos_concepts/data_sets/data_set_basics)
  - [Copy Directory to PDS, Edit member and Submit Job](zos_concepts/data_sets/copy_edit_submit)
  - [Operator Basics](zos_concepts/zos_operator/zos_operator_basics)
  - [Convert Encoding](zos_concepts/encoding/convert_encoding)
  - [Job Submit Response Parsing and Constructs](zos_basics/constructs)
  - [Initialize DASD volume, Copy JCL to PDS, and Submit Job](zos_concepts/volume_management/volume_initialization/init_dasd_vol_and_run_sample_jcl)
- Concepts
  - [Copy and Fetch Data sets](zos_concepts/data_transfer/copy_fetch_data_set)
  - [Copy, Sort and Fetch Data Sets on z/OS using Ansible](zos_concepts/data_transfer/copy_sort_fetch)- \[[Playback](https://mediacenter.ibm.com/media/Copy%2C+sort%2C+and+fetch+data+on+z+OS+using+Ansible/1_ah4qhyvu)]
  - [Terse Data Set and Fetch](zos_concepts/data_transfer/terse_fetch_data_set)
  - [Dump, Archive, Fetch, Copy and Unarchive Data Sets](zos_concepts/data_transfer/archive_copy_unarchive_restore)
  - [Transfer, Dump and Unpack Data Sets](zos_concepts/data_transfer/dump_pack_ftp_unpack_restore)
  - [Grow ZFS aggregates](zos_concepts/zfsadm/zfs_grow_aggr)
  - [Copy templates to z/OS](zos_concepts/templates/copy_template)
  - [Use templated job cards and JCL to submit jobs](zos_concepts/templates/submit_job_template)
  - [Use templates for compiling and linking libraries](zos_concepts/templates/loadlib)
  - [Run local and remote scripts inside Unix System Services in z/OS](zos_concepts/zos_script)
  - [Run REXX scripts and CLISTs with TSO commands](zos_concepts/tso_commands/scripts)
- Integrating Existing Automation
  - [Job Control Language](zos_concepts/jobs) (JCL)
    - [Submit Batch Jobs, Query and Retrieve Job Output](zos_concepts/jobs/submit_query_retrieve)
  - [z/OSMF Workflows](zos_management/zosmf_workflows)
  - [System Automation](z_system_automation)
    - [Managing Dynamic Resources](z_system_automation/dynamic_resources/)
  - [Invoking REST APIs using the URI module](zos_concepts/rest_apis)
- Provisioning
  - [IMS](zos_subsystems/ims)
    - [Deploy an IMS Cobol App](zos_subsystems/ims/ims_cobol_deploy)
    - [Provision a new IMS TMDB subsystem](zos_subsystems/ims/ims_provisioning)
    - [Provision a new IMS TMDB subsystem for Wazi](zos_subsystems/ims/ims_provisioning_wazi)
  - [ZCEE](zos_subsystems/zcee)
    - [Provision a new z/OS Connect EE subsystem](zos_subsystems/zcee/provisioning)
  - [WLP](zos_subsystems/wlp)
    - [Provision a new Websphere Application Server Liberty Profile](zos_subsystems/wlp)
  - [z/OSMF Cloud Provisioning and Management](zos_management/zosmf_cloud_provisioning_and_management)
- Application & Service Deployment
  - [ZCEE](zos_subsystems/zcee)
    - [Generate and deploy z/OS Connect EE API, service, and API requester artifacts](zos_subsystems/zcee/api_deployment)
- Subsystems
  - [CICS](zos_subsystems/cics)
    - [Retrieve operational data from running CICS regions](zos_subsystems/cics/cmci/reporting)
    - [Deploy a program to a CICS region](zos_subsystems/cics/cmci/deploy_program)
    - [Customize when a CMCI module should fail](zos_subsystems/cics/cmci/override_failure)
    - [Resource life cycle and CSD](zos_subsystems/cics/cmci/resource_lifecycle_and_csd)
    - [Using module_defaults](zos_subsystems/cics/cmci/module_defaults)
  - [MQ](zos_subsystems/mq)
    - [Interacting with local queues](zos_subsystems/mq/mqsc/qlocal)
	- [Backing up an application structure](zos_subsystems/mq/mqsc/cfstruct)
  - [Db2](zos_subsystems/db2)
    - [Provision a Db2 schema instance](zos_subsystems/db2/db2_schema_provisioning)
    - [Discover registered Db2 applications](zos_subsystems/db2/db2_discover_applications)
    - [Discover registered Db2 subsystems](zos_subsystems/db2/db2_discover_subsystems)
  - [IMS](zos_subsystems/ims)
    - [Enable IMS catalog and managed ACBs](zos_subsystems/ims/enable_catalog_macb)
- Software Installation
  - [SMP/E Playbooks](https://github.com/IBM/z_ansible_collections_samples/tree/master/zos_concepts/software_management)
- Security
  - [Security validation](zos_management/zosmf_security_configuration_assistant)
  - [Security audit](zos_management/zosmf_security_configuration_assistant)
  - [Security provision](zos_management/zosmf_security_configuration_assistant)


## Blogs
Don't have time to explore a collection, particular module or a playbook?  
Learn all about the offerings from our [blogs](https://community.ibm.com/community/user/ibmz-and-linuxone/groups/topic-home/blog-entries?communitykey=ce54fe94-0145-4832-a0ef-4ea81d6062cc&tab=blog-entries).  
Don't know which one to read first? Well, here is a list of some of our recent blogs:   

- [Job Submission on z/OS Made Easy with Ansible](https://community.ibm.com/community/user/ibmz-and-linuxone/blogs/asif-mahmud1/2020/08/04/how-to-run-batch-jobs-on-zos-without-jcl-using-ans) - \[[Playbook](zos_concepts/jobs/submit_query_retrieve)\]
- [Running Batch Jobs on z/OS using Ansible](https://community.ibm.com/community/user/ibmz-and-linuxone/blogs/asif-mahmud1/2020/08/04/how-to-run-batch-jobs-on-zos-without-jcl-using-ans) - \[[Playbook](zos_concepts/data_transfer/copy_sort_fetch)\]
- [Simplified Approach to Copying Data Between z/OS and Local Machine Using Ansible](https://community.ibm.com/community/user/ibmz-and-linuxone/blogs/asif-mahmud1/2020/06/11/simplified-approach-to-copying-data-between-zos-an) - \[[Playbook](zos_concepts/data_transfer/copy_fetch_data_set)\]
- [z/OS User Management With Ansible](https://community.ibm.com/community/user/ibmz-and-linuxone/blogs/blake-becker1/2020/09/03/zos-user-management-with-ansible) - \[[Playbook](zos_concepts/user_management/add_remove_user)\]

## Playbacks
Want to experience Z automation with Ansible without rolling up your sleeves?
Take a look at some of our playbacks using our own playbooks available
in this repository.

- [Copy, Sort and Fetch Data Sets on z/OS using Ansible](https://mediacenter.ibm.com/media/Copy%2C+sort%2C+and+fetch+data+on+z+OS+using+Ansible/1_ah4qhyvu)
- [Manage z/OS Users Using Ansible](https://mediacenter.ibm.com/media/Managing+z+OS+Users+with+Ansible+/1_bnud4enw)
- [Synchronize APF authorized libraries from a configuration cloned from GitHub](https://mediacenter.ibm.com/media/Using+Ansible+to+synchronize+z+OS+APF+libraries+from+a+GitHub+configuration+file/1_e6qsiwmg)

## How playbooks are organized in this repository
All playbooks in this repository can run independently from any other playbook.
Each playbook in this repository includes a README with a brief description,
licensing and instructions on how to configure the playbook. If configuration is
required, a default configuration will be included with the playbook.

For further reading on how playbook projects are organized in this repository,
review the following [documentation](./docs/share/contribution-guidelines.md#playbook-structure).

## Copyright
© Copyright IBM Corporation 2020 - 2024

## License
Licensed under
[Apache License, Version 2.0](https://opensource.org/licenses/Apache-2.0).

## Support
This project is supported by the community though
[Git issues](https://github.com/IBM/z_ansible_collections_samples/issues).

The repository admins and content owners will engage directly with users on
issues reported.

Playbooks are contributed by IBM and the broader Ansible Z community. Therefore,
it may be helpful to review who contributed a playbook as well as its
requirements before opening a Git issue. You can view who the contributor was by
looking at the playbooks commit history as well as notes in the playbook.
