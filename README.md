# Ansible Z Playbook Repository

This repository provides playbooks that can be tailored to automate often
repeated Z tasks, links to blogs, and other related media. An [Ansible playbook](https://docs.ansible.com/ansible/latest/user_guide/playbooks_intro.html#playbooks-intro)
consists of organized instructions that define work for a managed node to be
automated with Ansible. The playbooks in this
repository are written using the collections from
**Red Hat® Ansible® Certified Content for IBM Z®** offering. For a detailed
overview of the playbooks, refer to the
[documentation](https://ibm.github.io/z_ansible_collections_doc/index.html).

The collections can be found on
[Ansible Galaxy](https://galaxy.ansible.com/search?deprecated=false&keywords=ibm_z&order_by=-relevance&page=1)
and [Automation Hub](https://www.ansible.com/products/automation-hub).
For additional content, review our [community page](ibm.biz/BdfsTR).

This repository is frequently updated to refresh content. We recommend that you clone the
the repository and configure your GitHub
[notifications and subscriptions](https://docs.github.com/en/github/managing-subscriptions-and-notifications-on-github/about-notifications#notifications-and-subscriptions)
to receive ongoing updates about specific activity on GitHub.

## Playbooks

Playbooks are organized by topics that align to tasks and use cases that are
often repeated and in need of automation.

### Z Topics
* [Z Systems Administration](z_systems_administration)
  * [zHMC](z_systems_administration/zhmc)
    * [Gather facts for CPC and its adapters and partitions](z_systems_administration/zhmc/docs/usecase_playbooks.md)
    * [zHMC Concepts](z_systems_administration/zhmc/docs/module_playbooks.md)

### z/OS® Topics
* [z/OS Administration](zos_administration/)
   * [Set Up Ansible Host Variables for Python and ZOAU](zos_administration/host_setup)
* [z/OS Concepts](zos_concepts/)
   * [Convert Encoding](zos_concepts/encoding/convert_encoding)
   * [Copy and Fetch Data sets](zos_concepts/data_transfer/copy_fetch_data_set)
   * [Copy Directory to PDS, Edit member and Submit Job](zos_concepts/data_sets/copy_edit_submit)
   * [Copy, Sort and Fetch Data Sets on z/OS using Ansible](zos_concepts/data_transfer/copy_sort_fetch)- \[[Playback](https://mediacenter.ibm.com/media/Copy%2C+sort%2C+and+fetch+data+on+z+OS+using+Ansible/1_ah4qhyvu)]
   * [Data Set Basics](zos_concepts/data_sets/data_set_basics)
   * [Manage z/OS Users Using Ansible](zos_concepts/user_management/add_remove_user) - \[[Playback](https://mediacenter.ibm.com/media/Managing+z+OS+Users+with+Ansible+/1_bnud4enw)]
   * [Operator Basics](zos_concepts/zos_operator/zos_operator_basics)
   * [Submit Batch Jobs, Query and Retrieve Job Output](zos_concepts/jobs/submit_query_retrieve)
   * [Synchronize APF authorized libraries from a configuration cloned from GitHub](zos_concepts/program_authorization/git_apf) - \[[Playback](https://mediacenter.ibm.com/media/Using+Ansible+to+synchronize+z+OS+APF+libraries+from+a+GitHub+configuration+file/1_e6qsiwmg)]
   * [Terse Data Set and Fetch](zos_concepts/data_transfer/terse_fetch_data_set)
   * [Transfer, Dump and Unpack Data Sets](zos_concepts/data_transfer/dump_pack_ftp_unpack_restore)
* [z/OS Subsystems](zos_subsystems)
   * [CICS](zos_subsystems/cics)
     * [Retrieve operational data from running CICS regions](zos_subsystems/cics/cmci/reporting)
     * [Deploy a program to a CICS region](zos_subsystems/cics/cmci/deploy_program)
     * [Customize when a CMCI module should fail](zos_subsystems/cics/cmci/override_failure)
   * [IMS](zos_subsystems/ims)
   * [ZCEE](zos_subsystems/zcee)
* z/OS Automated Operations   
   * [IBM Z System Automation](z_system_automation)
     * Managing Dynamic Resources](z_system_automation/dynamic_resources/)

## Blogs
Don't have time to explore a collection, particular module or a playbook? Learn all about the offerings 
from our
[blogs](https://community.ibm.com/community/user/ibmz-and-linuxone/groups/topic-home/blog-entries?communitykey=ce54fe94-0145-4832-a0ef-4ea81d6062cc&tab=blog-entries). Don't know
which one to read first? Well, here is a list of some of our recent blogs:

* [Job Submission on z/OS Made Easy with Ansible](https://community.ibm.com/community/user/ibmz-and-linuxone/blogs/asif-mahmud1/2020/08/04/how-to-run-batch-jobs-on-zos-without-jcl-using-ans) - \[[Playbook](zos_concepts/jobs/submit_query_retrieve)\]
* [Running Batch Jobs on z/OS using Ansible](https://community.ibm.com/community/user/ibmz-and-linuxone/blogs/asif-mahmud1/2020/08/04/how-to-run-batch-jobs-on-zos-without-jcl-using-ans) - \[[Playbook](zos_concepts/data_transfer/copy_sort_fetch)\]
* [Simplified Approach to Copying Data Between z/OS and Local Machine Using Ansible](https://community.ibm.com/community/user/ibmz-and-linuxone/blogs/asif-mahmud1/2020/06/11/simplified-approach-to-copying-data-between-zos-an) - \[[Playbook](zos_concepts/data_transfer/copy_fetch_data_set)\]
* [z/OS User Management With Ansible](https://community.ibm.com/community/user/ibmz-and-linuxone/blogs/blake-becker1/2020/09/03/zos-user-management-with-ansible) - \[[Playbook](zos_concepts/user_management/add_remove_user)\]

## Playbacks
Want to experience Z automation with Ansible without rolling up your sleeves?
Take a look at some of our playbacks using our own playbooks available
in this repository.

* [Copy, Sort and Fetch Data Sets on z/OS using Ansible](https://mediacenter.ibm.com/media/Copy%2C+sort%2C+and+fetch+data+on+z+OS+using+Ansible/1_ah4qhyvu)
* [Manage z/OS Users Using Ansible](https://mediacenter.ibm.com/media/Managing+z+OS+Users+with+Ansible+/1_bnud4enw)
* [Synchronize APF authorized libraries from a configuration cloned from GitHub](https://mediacenter.ibm.com/media/Using+Ansible+to+synchronize+z+OS+APF+libraries+from+a+GitHub+configuration+file/1_e6qsiwmg)

## How playbooks are organized in this repository

All playbooks in this repository can run independently from any other playbook.
Each playbook in this repository includes a README with a brief description,
licensing and instructions on how to configure the playbook. If configuration is
required, a default configuration will be included with the playbook.

The playbooks in the repository is organized as follows:

    ├── topic/
    │  └── use_case/
    │      └── playbook_name/
    │          ├── host_vars/
    │              └── zos_host.yml
    │          ├── ansible.cfg
    │          ├── inventory
    │          ├── playbook_name.yml
    │          └── README
    ├── LICENSE
    └── README

## Copyright
© Copyright IBM Corporation 2020, 2021

## License
Licensed under
[Apache License, Version 2.0](https://opensource.org/licenses/Apache-2.0).

## Support
Support for all playbooks, roles, and filters are provided by the community
and all issues are tracked through a
[Git issue](https://github.com/IBM/z_ansible_collections_samples/issues).
The repository admins and content owners will engage directly with users on issues reported on the
Git issue.

Playbooks are contributed both by IBM as well as the broader Ansible community including IBM partners and the Z community. 
Therefore, it may be helpful to review who contributed a sample as well as its requirements. You
can view who the contributor was by looking at the playbooks commit history as
well as notes in the playbook.

