<!---
Copyright 2017-2020 IBM Corp. All Rights Reserved.

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at
   http://www.apache.org/licenses/LICENSE-2.0
Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.
-->

<!---
Note: Details on the "Version on Galaxy" badge, below:

Shields.io allows defining dynamic badge creation using data in the result of
an HTTP GET on a JSON based REST API, as follows:

https://img.shields.io/badge/dynamic/json?
  url=<URL>&
  label=<LABEL>&
  query=<$.DATA.SUBDATA>&
  color=<COLOR>&
  prefix=<PREFIX>&
  suffix=<SUFFIX>
plus the standard query parameters (style, color, ...)

Ansible Galaxy returns the latest version of a collection using an HTTP GET on
https://galaxy.ansible.com/api/v2/collections/<NS>/<COLL>/ as follows:

{
    . . .
    "latest_version": {
        "version": "1.0.0",
    }
}

Ansible AutomationHub returns the latest version of a collection using an HTTP GET on
https://console.redhat.com/api/automation-hub/v3/collections/<NS>/<COLL>/ as follows:

{
    . . .
    "highest_version": {
        "version": "1.0.0",
    }
}

That would result in the following markup:

[![Version on AutomationHub](https://img.shields.io/badge/dynamic/json?style=flat&label=hub&prefix=v&url=https://console.redhat.com/api/automation-hub/v3/collections/ibm/ibm_zhmc/&query=highest_version.version)](https://console.redhat.com/ansible/automation-hub/repo/published/ibm/ibm_zhmc/ "Version on AutomationHub")

However, for now this does not work, so it has been removed again from the README page.
For details, see the discussion at https://github.com/ansible-collections/overview/discussions/202
-->

[![Version on Galaxy](https://img.shields.io/badge/dynamic/json?style=flat&label=galaxy&prefix=v&url=https://galaxy.ansible.com/api/v2/collections/ibm/ibm_zhmc/&query=latest_version.version)](https://galaxy.ansible.com/ibm/ibm_zhmc/ "Version on Galaxy")
[![Test status (master)](https://github.com/zhmcclient/zhmc-ansible-modules/workflows/test/badge.svg?branch=master)](https://github.com/zhmcclient/zhmc-ansible-modules/actions?query=workflow%3Atest "Test status (master)")
[![Docs status (master)](https://github.com/zhmcclient/zhmc-ansible-modules/workflows/docs/badge.svg?branch=master)](https://github.com/zhmcclient/zhmc-ansible-modules/actions?query=workflow%3Adocs "Docs status (master)")
[![Test coverage (master)](https://img.shields.io/coveralls/zhmcclient/zhmc-ansible-modules.svg)](https://coveralls.io/github/zhmcclient/zhmc-ansible-modules "Test coverage (master)")

IBM Z HMC collection
====================

The **IBM Z HMC collection** is part of the broader initiative to bring Ansible
Automation to IBM Z® through the offering
**Red Hat® Ansible Certified Content for IBM Z®**. The
**IBM Z HMC collection** supports automation tasks such as creating, updating or
deleting partitions and other resources that can be managed with the
IBM Z Hardware Management Console (HMC).

Red Hat Ansible Certified Content for IBM Z
===========================================

**Red Hat® Ansible Certified Content for IBM Z** provides the ability to
connect IBM Z® to clients' wider enterprise automation strategy through the
Ansible Automation Platform ecosystem. This enables development and operations
automation on Z through a seamless, unified workflow orchestration with
configuration management, provisioning, and application deployment in
one easy-to-use platform.

The **IBM Z HMC collection** is following the
**Red Hat® Ansible Certified Content for IBM Z®** method of distributing
content. Collections will be developed in the open, and when content is ready
for use it is released to
[Ansible Galaxy](https://galaxy.ansible.com/search?keywords=ibm_z&order_by=-relevance&deprecated=false&type=collection&page=1)
for community adoption. Once contributors review community usage, feedback,
and are satisfied with the content published, the collection will then be
released to [Ansible Automation Hub](https://www.ansible.com/products/automation-hub)
as **certified** and **IBM supported** for
**Red Hat® Ansible Automation Platform subscribers**.

For guides and reference, please review the
[documentation](https://zhmcclient.github.io/zhmc-ansible-modules/)
or the Z HMC section in the
[unified documentation](https://ibm.github.io/z_ansible_collections_doc/).

Features
========

The **IBM Z HMC collection** provides Ansible modules and
[sample_playbooks](https://github.com/IBM/z_ansible_collections_samples/tree/master/z_systems_administration/zhmc)
for automating management tasks on the Hardware Management Console (HMC) of
[IBM Z](https://www.ibm.com/it-infrastructure/z/) and
[LinuxONE](https://www.ibm.com/it-infrastructure/linuxone/) machines, such as
creating, updating or deleting partitions and other resources.

Copyright
=========

© Copyright IBM Corporation 2016-2021.

License
=======

This collection is licensed under
[Apache License, Version 2.0](https://opensource.org/licenses/Apache-2.0).
