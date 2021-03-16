# Ansible Z Playbook Samples

An [Ansible playbook](https://docs.ansible.com/ansible/latest/user_guide/playbooks_intro.html#playbooks-intro)
consists of organized instructions that define work for a managed node to be
managed with Ansible.

This repository contains sample playbooks, blogs and media related to the Red Hat Ansible Certified Content for IBM Z.
Refer to the [documentation site](https://ibm.github.io/z_ansible_collections_doc/index.html) for a detailed overview of requirements,
installation and how to get started.

This repository will have frequent updates, we recommend you clone the
repository and use the GitHub **watch** option to receive update notifications;
this is outlined in the
[using the samples repository](meta/samples_repository/README.md) documentation.

## Topics

* [z/OS Concepts](zos_concepts/)
   * [Convert Encoding](zos_concepts/encoding/convert_encoding)
   * [Copy and Fetch Data sets](zos_concepts/data_transfer/copy_fetch_data_set)
   * [Copy Directory to PDS, Edit member and Submit Job](zos_concepts/data_sets/copy_edit_submit)
   * [Copy, Sort and Fetch Data Sets on z/OS using Ansible](zos_concepts/data_transfer/copy_sort_fetch)- \[[Playback](media/copy_sort_and_fetch_data_sets_on_z_OS_using_ansible.m4v)]
   * [Data Set Basics](zos_concepts/data_sets/data_set_basics)
   * [Manage z/OS Users Using Ansible](zos_concepts/user_management/add_remove_user) - \[[Playback](media/managing-zos-user-with-ansible-presentation-demo.m4v)]
   * [Submit Batch Jobs, Query and Retrieve Job Output](zos_concepts/jobs/submit_query_retrieve)
   * [Synchronize APF authorized libraries on z/OS from a configuration file cloned from GitHub](zos_concepts/program_authorization/git_apf) - \[[Playback](media/synchronize_APF_authorized_libraries_from_a_gitHub_configuration.m4v)]
   * [Terse Data Set and Fetch](zos_concepts/data_transfer/terse_fetch_data_set)
   * [Transfer, Dump and Unpack Data Sets](zos_concepts/data_transfer/dump_pack_ftp_unpack_restore)
   * [z/OS Operator Basics](zos_concepts/zos_operator/zos_operator_basics)

* [z/OS Administration](zos_administration/)
   * [Set Up Host Vars by Configuring Python and ZOAU Installation](zos_administration/host_setup)

* [CICS samples](cics/)
   * [Retrieve operational data from running CICS regions](cics/cmci/reporting/)
   * [Deploy a program to a CICS region](cics/cmci/deploy_program/)
   * [Customize when a CMCI module should fail](cics/cmci/override_failure/)

## Blogs

* [Job Submission on z/OS Made Easy with Ansible](https://community.ibm.com/community/user/ibmz-and-linuxone/blogs/asif-mahmud1/2020/06/10/job-submission-on-zos-made-easy-with-ansible) - \[[Sample playbook](zos_concepts/jobs/submit_query_retrieve)\]

* [Running Batch Jobs on z/OS using Ansible](https://community.ibm.com/community/user/ibmz-and-linuxone/blogs/asif-mahmud1/2020/08/04/how-to-run-batch-jobs-on-zos-without-jcl-using-ans) - \[[Sample playbook](zos_concepts/data_transfer/copy_sort_fetch)\]

* [Simplified Approach to Copying Data Between z/OS and Local Machine Using Ansible](https://community.ibm.com/community/user/ibmz-and-linuxone/blogs/asif-mahmud1/2020/06/11/simplified-approach-to-copying-data-between-zos-an) - \[[Sample playbook](zos_concepts/data_transfer/copy_fetch_data_set)\]

* [z/OS User Management With Ansible](https://community.ibm.com/community/user/ibmz-and-linuxone/blogs/blake-becker1/2020/09/03/zos-user-management-with-ansible) - \[[Sample playbook](zos_concepts/user_management/add_remove_user)\]

## Playbacks
* [Copy, Sort and Fetch Data Sets on z/OS using Ansible](media/copy_sort_and_fetch_data_sets_on_z_OS_using_ansible.m4v)
* [Manage z/OS Users Using Ansible](media/managing-zos-user-with-ansible-presentation-demo.m4v)
* [Synchronize APF authorized libraries on z/OS from a configuration file cloned from GitHub](media/synchronize_APF_authorized_libraries_from_a_gitHub_configuration.m4v)
  
## How sample playbooks are packaged

The sample playbook repository is organized as follows:

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


When new playbooks are contributed, they are associated to a use case and placed
under the appropriate topic. If the use case does not correspond to an existing
topic, a new topic will be added, outlined above.  

Each playbook will also include a README with a brief description, licensing and
copyright information.

## Copyright

© Copyright IBM Corporation 2020

## License

Licensed under [Apache License,
Version 2.0](https://opensource.org/licenses/Apache-2.0).

## Support

Support for all sample playbooks, roles and filters are done so by the community
and managed by opening a [Git issue](https://github.com/IBM/z_ansible_collections_samples/issues).
The repository admins and content owners will engage users on issues reported in
Git.  

In the future, samples may be community contributed, therefore it may be
helpful to review who contributed the sample as well as the requirements. You
can view who the contributor was by looking at the playbooks commit history as
well as notes in the playbook.  

Playbooks contributed by IBM will be identified with the following header:

``` {.yaml}
###############################################################################
# © Copyright IBM Corporation 2020
# Contributed by the Ansible Content for IBM Z Team
```
