# Fact Gathering for z/OS

The `zos_gather_facts` module expands on Ansible's built in fact gathering to
provide various facts to the Ansible playbook author.

# Run the sample playbook

```
ansible-playbook -i <inventory> gather-facts.yml
```

The provided `gather-facts.yml` playbook runs the zos fact gathering module
`zos_gather_facts` several times with different subsets and filter settings to
demonstrate the available parameters.

- First `zos_gather_facts` is called with the `ipl` gather_subset and a filter
    set to `master*`. This matches 2 available facts both of which are printed
    in the following step through the built in debug module. That next step
    shows how collected facts can be accessed in the context of a playbook by
    calling on the `ansible_facts` variable.
- Then the `zos_gather_facts` module is called with two gather_subsets specified
    `cpu` and `iodf` and once more the built in debug module is called to print
    out collected facts. The print step shows an alternate way to access those
    facts.
- The final step in the playbook prints out all the collected facts which have
    been added to the `ansible_facts` variable.

# Caching facts

Collected facts can be cached into a database and accessed later without taking
a performance hit from repeatedly running fact gathering.

### How to enable local JSON caching

To set up caching into a local json file, 2 variables have to be configured.
These can be configured in an `ansible.cfg` file, as regular environment
variables, or in the command line where the playbook is called.

In ansible.cfg, set values for:
```
[inventory]
cache_plugin=jsonfile
fact_caching_connection=<some_folder_location>
```

As an environment variables:
- `export ANSIBLE_INVENTORY_CACHE_PLUGIN=jsonfile`
- `export ANSIBLE_CACHE_PLUGIN_CONNECTION='<some_folder_location>'`

In the command line, set values for:
- `ANSIBLE_CACHE_PLUGIN=jsonfile`
- `ANSIBLE_CACHE_PLUGIN_CONNECTION='<some_folder_location>'`

Example command to run playbook and output `ansible_files` to a local json file:
```
ANSIBLE_CACHE_PLUGIN=jsonfile ANSIBLE_CACHE_PLUGIN_CONNECTION='<some_folder_location>' ansible-playbook -i inventories gather-facts.yml
```

## Additional resources for Ansible caching
Caching `ansible_facts` is achieved through Ansible cache plugins.
There are a number of cache plugins provided by Ansible and a custom
cache plugin can also be specified. The documentation for calling upon
cache plugins can be found
[here](https://docs.ansible.com/ansible/latest/plugins/cache.html).

Some plugins implemented by Ansible 2.8:

- json - [here](https://docs.ansible.com/ansible/2.8/plugins/cache/jsonfile.html)
- redis - [here](https://docs.ansible.com/ansible/2.8/plugins/cache/redis.html)
- mongo - [here](https://docs.ansible.com/ansible/2.8/plugins/cache/mongodb.html)

The full list of cache plugins can be found by following the steps
listed [here](https://docs.ansible.com/ansible/2.8/plugins/cache.html#plugin-list).


# Changelog
All changes are maintained chronologically by date found in the
[changelog](changelog.yml).

# Copyright
© Copyright IBM Corporation 2022

# License
Licensed under [Apache License,
Version 2.0](https://opensource.org/licenses/Apache-2.0).

# Support
Please refer to the [support section](../../README.md#support) for
more details.