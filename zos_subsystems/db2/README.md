# Db2 Ansible playbooks

The following Db2 Ansible playbooks demonstrate how to prompt registered instantiable applications<sup>[1](#footnote1)</sup>, Db2 subsystems and create new instances of an application by exploiting IBM Db2 DevOps Experience for z/OS (DOE) APIs.

## Db2 DevOps Experience

The IBM Db2 DevOps Experience (DOE) delivers Db2 for z/OS DBaaS as an integrated set of features. This technology enables a development team to implement DevOps practices by automating and standardizing tasks that would normally require interactions with an operations team. Therefore, it helps bridge the gap between application developers and database administrators.

It provides collaborations between roles in an organization in the following practices:
- Developers can easily provision a version of an application, make changes, test, and promote those changes for approval.
- Quality Engineers can easily automate the creation of Db2 test environments to speed and improve testing activities.
- Administrators can set the site rules for Db2 schema standards and usage rules, review, and approve changes.

These practices decrease development time, improve quality, and better integrate into multi-platform development practices.

## DOE APIs

The services and capabilities of the Db2 DevOps Experience can be used through a browser-based interface called the IBM Unified Experience for z/OS or by scripting the REST APIs.

DOE provides a rich set of REST APIs that provide all of the functions that you need to drive Db2 for z/OS schema changes. The sample playbooks are examples of how DOE APIs can be embedded in any scripting language.

You can access the Swagger API documentation by browsing https://<*host*>:<*port*>/ws/swagger-ui.html where <*host*> is the DNS name or IP address
of the z/OS system where you installed DOE and <*port*> is the port number that you specified for Swagger (12023 by default).

For more information: [Integrating Db2 for z/OS database changes into a CI/CD pipeline](https://www.redbooks.ibm.com/Redbooks.nsf/RedpieceAbstracts/redp5646.html?Open)

## Playbooks Summary
* [**Provision**](db2_provisioning/db2-schema-provision.yml) - Provision a Db2 schema instance of an application
* [**Deprovision**](db2_provisioning/db2-schema-deprovision.yml) - Deprovision a Db2 schema instance of an application
* [**Discovery of Db2 applications**](db2_applications/db2-get-applications.yml) - Prompt all registered "applications" that can be instantiated
* [**Discovery of Db2 system information**](db2_discovery/db2-discovery.yml) - Prompt the information on all registered Db2 systems

## Requirements:
* IBM Unified Management Server for V1.1.0.4 or later
* IBM Db2 DevOps Experience for z/OS V1.2.0.1 or later
* These samples requires Ansible 2.9.0 later.

## Released Version
* Version 1.0 - Release Notes: (Released mm dd, 2021)

### Note:
<sup>[1](#footnote1)</sup> An application in DOE is a set of objects, such as tablespaces, tables, and indexes that are grouped to be managed and provisioned as a single unit for the use of an application program or a set of application programs. 

# Copyright

© Copyright IBM Corporation 2021

# License

Licensed under [Apache License,
Version 2.0](https://opensource.org/licenses/Apache-2.0)

# Support

Please refer to the [support section](https://github.com/IBM/z_ansible_collections_samples/blob/master/README.md#support) for more
details.
