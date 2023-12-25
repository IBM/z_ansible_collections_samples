# IBM Db2 Automation Expert Role: daj_list_profiles

The [daj_util](../daj_util) role accepts an optional Db2 profile as input. This allows you to define
multiple Db2 utility profiles for, say, REORG. For example: you could have one profile for small tables,
another for large tables, and a third for tables with LOBs.

This role returns a list of Db2 Automation Expert profiles available for the Db2 subsystem, LPAR, and utility.

For more details see the API documented in UMS Swagger for the API `/plugin/automation-expert/utility/{lpar}/{subsystem}/{utilityId}/profiles`.

Sample ansible code to show the available REORG profiles for Db2 subsystem DSN1 on LPAR SYS1:
```
    - name: Get list of profiles for utility
      ansible.builtin.include_role:
        name: daj_list_profiles
      vars:
        daj_util_ssid: DSN1
        daj_util_sysname: SYS1
        daj_util_name: REORG
```
Sample output:
```
TASK [daj_list_profiles : Show list of profiles] *******************************
ok: [localhost] => {
    "msg": [
        {
            "createdTimeStamp": "05/17/2023-22.48.40",
            "createdUserIdentifier": "USER1",
            "creator": "USER1",
            "description": "",
            "lastUserTimeStamp": "04/17/2023-01.47.36",
            "lastUserUpdate": "USER1",
            "name": "%USER1 0329 GUI",
            "updateOption": "N"
        },
        {
            "createdTimeStamp": "04/12/2023-18.18.54",
            "createdUserIdentifier": "USER2",
            "creator": "USER2",
            "description": "A TEST FOR",
            "lastUserTimeStamp": "01/24/2023-03.52.31",
            "lastUserUpdate": "USER2",
            "name": "%USER2 0329 GUI%",
            "updateOption": "U"
        },
[...]
```


# Copyright

© Copyright Rocket Software 2023

# License

Licensed under [Apache License,
Version 2.0](https://opensource.org/licenses/Apache-2.0)
