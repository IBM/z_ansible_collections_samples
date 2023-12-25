# IBM Db2 Automation Expert Role: ums_list_utilities

List the Db2 utilities supported by IBM Db2 Automation Expert APIs.
The APIs support a subset of possible Db2 utilities. Use this playbook to retrieve the list
supported by your installation.

For more details see the UMS Swagger documentation for API `/plugin/automation-expert/utility/{lpar}/{subsystem}/utilities`.

Sample output:
```
TASK [daj_list_utilities : Show list of supported utilities] ******************************************************
ok: [localhost] => {
    "msg": [
        {
            "utilityName": "COPY"
        },
        {
            "utilityName": "REORG"
        },
        {
            "utilityName": "RUNSTATS"
        }
    ]
}
```


# Copyright

© Copyright Rocket Software 2023

# License

Licensed under [Apache License,
Version 2.0](https://opensource.org/licenses/Apache-2.0)
