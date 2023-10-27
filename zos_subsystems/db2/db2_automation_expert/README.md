# IBM Db2 Automation Expert

The following playbooks demonstrate how to run an ad-hoc utility on one or more Db2 objects.
For example, you want to provide self-service Db2 utilities for developers in non-production environments.

The playbooks exploit DBaaS services delivered by IBM Db2 Automation Expert. The APIs are hosted
in IBM Unified Management Server. For more details on the available APIs see the Swagger documentation
hosted by the UMS server: `https://<ums server hostname>:<ums server port>/ws/swagger-ui.html`.

## Playbooks summary

### Supporting playbooks for common UMS requests
* **Authenticate** - Authenticate with the UMS server. 
    * [Sample playbook](ums_login.yml) 
    * [Documentation](roles/ums_login)
* **Refresh access token** - Refresh access token for UMS server (if the original access token has expired). 
    * [Sample playbook](ums_refresh.yml)
    * [Documentation](roles/ums_refresh)

### Db2 Automation Expert playbooks
* **List supported utilities** - The APIs support a subset of the available Db2 utilities. Use this playbook to obtain the list. Initially, REORG, COPY, and RUNSTATS are supported. 
    * [Sample playbook](daj_list_utilities.yml)
    * [Documentation](roles/daj_list_utilities)
* **List profiles** - retrieve list of available profiles for the specified Db2 subsystem, LPAR, and utility.
    * [Sample playbook](daj_list_profiles.yml)
    * [Documentation](roles/daj_list_profiles)
* **Run ad-hoc utility** - Run ad-hoc utilities for a list of objects. 
    * [Sample playbook](daj_util.yml) 
    * [Documentation](roles/daj_util)



# Copyright

© Copyright Rocket Software 2023

# License

Licensed under [Apache License,
Version 2.0](https://opensource.org/licenses/Apache-2.0)
