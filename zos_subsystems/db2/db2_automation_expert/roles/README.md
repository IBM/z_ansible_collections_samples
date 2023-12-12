# IBM Db2 Automation Expert Roles

These roles exploit DBaaS services delivered by IBM Db2 Automation Expert. The APIs are hosted
in IBM Unified Management Server. For more details on the available APIs see the Swagger documentation
hosted by the UMS server: https://<ums server hostname>:<ums server port>/ws/swagger-ui.html.

## Roles summary

### Supporting roles for common UMS requests
* **ums_login** - Authenticate with the UMS server. 
    * [Sample playbook](../ums_login.yml) 
    * [Documentation](ums_login)
* **ums_refresh** - Refresh access token for UMS server (if the original access token has expired). 
    * [Sample playbook](../ums_refresh.yml)
    * [Documentation](ums_refresh)

### Db2 Automation Expert roles
* **daj_list_utilities** - List supported utilities
    * [Sample playbook](../daj_list_utilities.yml)
    * [Documentation](daj_list_utilities)
* **daj_list_profiles** - retrieve list of available profiles for the specified Db2 subsystem, LPAR, and utility.
    * [Sample playbook](../daj_list_profiles.yml)
    * [Documentation](daj_list_profiles)
* **daj_util** - Run ad-hoc utilities for a list of objects. 
    * [Sample playbook](../daj_util.yml) 
    * [Documentation](ums_util)



# Copyright

© Copyright Rocket Software 2023

# License

Licensed under [Apache License,
Version 2.0](https://opensource.org/licenses/Apache-2.0)
