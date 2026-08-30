# Configuration management database

The ServiceNow ITSM Ansible Collection contains two modules for querying and
modifying the configuration management database (CMDB).


## Adding information to the CMDB

The `servicenow.itsm.configuration_item` module allows us to insert new items
into CMDB tables (tables with names starting with `cmdb_ci_`). And this
capability comes real handy when coupled with things like virtual machine (VM)
provisioning.

For example, this is how we would create a new VM and add its information to
the CMDB:

    ---
    - name: Demonstrate CMDB management
      hosts: localhost
      gather_facts: false

      tasks:
        - name: Simulate VM creation
          ansible.builtin.set_fact:
            instance:
              name: some-name
              id: vm1234567890
              public_ip_address: 1.2.3.4

        - name: Register the newly-created VM instance
          servicenow.itsm.configuration_item:
            name: "{{ instance.name }}"
            sys_class_name: cmdb_ci_vm_instance
            ip_address: "{{ instance.public_ip_address }}"
            other:
              vm_inst_id: "{{ instance.id }}"

(We simulated the VM creation with a simple `set_fact` task because this allows
us to run the playbook without creating a VM instance.)

Do note that we had to specify the `sys_class_name` parameter in our module
invocation. This parameter tells the configuration item module into which table
to insert the information. In our case, we added the information to the
`cmdb_ci_vm_instance` table.


## Updating and deleting CMDB items

Updating an existing configuration item is quite similar to creating a new one.
There are two significant differences:

 1. We need to provide a `sys_id` parameter value if we want to update or
    delete an existing configuration item.
 2. We can omit the `sys_class_name` parameter because the module can
    auto-detect this value.

We can delete the configuration from CMDB by adding `state: absent` to the task
parameters. In this case, the only required parameter is `sys_id`.

    ---
    - name: Demonstrate CMDB item update and deletion
      hosts: localhost
      gather_facts: false

      tasks:
        - name: Update the VM state
          servicenow.itsm.configuration_item:
            sys_id: 1234567
            install_status: in_maintenance

        - name: Remove item from CMDB
          servicenow.itsm.configuration_item:
            sys_id: 1234567
            state: absent


## Listing configuration items

The `servicenow.itsm.configuration_item_info` module allows us to fetch the
information about existing CMDB items. Information retrieval is of crucial
importance, for example, when handling incidents that reference a specific
configuration item.

    ---
    - name: Demonstrate CMDB item info retrieval
      hosts: localhost
      gather_facts: false

      tasks:
        - name: Retrieve information about CMDB item
          servicenow.itsm.configuration_item_info:
            # In the next line, we assumed that the incident variable contains
            # information about an incident.
            sys_id: "{{ incident.record.cmdb_ci }}"

We can also retrieve information about all configuration items in the selected
table, but this is something that we would seldom need in real-world scenarios.
