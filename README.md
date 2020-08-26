# Ansible z/OS Collection Samples

This repository contains sample playbooks and blogs related to the Red Hat Ansible Certified Content for IBM Z.
Refer to the [documentation site](https://ibm.github.io/z_ansible_collections_doc/index.html) for a detailed overview of requirements,
installation and how to get started.


## Index

* [z/OS Concepts](https://github.com/IBM/z_ansible_collections_samples/ibm_zos_core)
   * [Data Set Basics](https://github.com/IBM/z_ansible_collections_samples/zos_concepts/data_sets/data_set_basics)
   * [Copy and Fetch Data sets](https://github.com/IBM/z_ansible_collections_samples/zos_concepts/data_transfer/copy_fetch_data_set)
   * [Copy, Sort and Fetch Data Sets on z/OS using Ansible](https://github.com/IBM/z_ansible_collections_samples/zos_concepts/data_transfer/copy_sort_fetch)
   * [Transfer, Dump and Unpack Data Sets](https://github.com/IBM/z_ansible_collections_samples/zos_concepts/data_transfer/dump_pack_ftp_unpack_restore)
   * [Convert Encoding](https://github.com/IBM/z_ansible_collections_samples/zos_concepts/encoding/convert_encoding)
   * [Submit Batch Jobs, Query and Retrieve Job Output](https://github.com/IBM/z_ansible_collections_samples/zos_concepts/jobs/submit_query_retrieve)
   * [Manage z/OS Users Using Ansible](https://github.com/IBM/z_ansible_collections_samples/zos_concepts/user_management/add_remove_user)
   * [z/OS Operator Basics](https://github.com/IBM/z_ansible_collections_samples/zos_concepts/zos_operator/zos_operator_basics)


## Blogs

* [Job Submission on z/OS Made Easy with Ansible](https://community.ibm.com/community/user/ibmz-and-linuxone/blogs/asif-mahmud1/2020/06/10/job-submission-on-zos-made-easy-with-ansible) - \[[Sample playbook](https://github.com/IBM/z_ansible_collections_samples/zos_concepts/jobs/submit_query_retrieve)\]

* [Running Batch Jobs on z/OS using Ansible](https://community.ibm.com/community/user/ibmz-and-linuxone/blogs/asif-mahmud1/2020/08/04/how-to-run-batch-jobs-on-zos-without-jcl-using-ans) - \[[Sample playbook](https://github.com/IBM/z_ansible_collections_samples/zos_concepts/data_transfer/copy_sort_fetch)\]

* [Simplified Approach to Copying Data Between z/OS and Local Machine Using Ansible](https://community.ibm.com/community/user/ibmz-and-linuxone/blogs/asif-mahmud1/2020/06/11/simplified-approach-to-copying-data-between-zos-an) - \[[Sample playbook](https://github.com/IBM/z_ansible_collections_samples/zos_concepts/data_transfer/copy_fetch_data_set)\]


## How to contribute

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


When contributing new sample playbooks, they should be placed under the appropriate topic and use case.
If the playbook does not correspond to an existing use case, a new use case can be added that conforms to the
structure outlined above.
Each playbook should also include a README with a brief description, licensing and copyright information.


## Copyright

© Copyright IBM Corporation 2020

## License

Licensed under [Apache License,
Version 2.0](https://opensource.org/licenses/Apache-2.0).

## Support

All IBM certified sample playbooks, roles and filters are supported as part of
the Red Hat® Ansible Certified Content for IBM Z offering. Support for samples
is managed through the repositories git issues:
https://github.com/IBM/z_ansible_collections_samples/issues
