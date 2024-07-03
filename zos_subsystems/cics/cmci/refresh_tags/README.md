# Using the CICS collection to refresh a region's tags

This sample playbook demonstrates how to use the `cmci_action` module from the `ibm_zos_cics` collection to run an action against a CICS resource.
Additionally, this readme documents how to determine the appropriate Ansible
module configuration, if you're trying to automate something you know how
to do in CICS Explorer

## Requirements

- Python 3.5+
- Ansible 2.15+
- IBM z/OS CICS Ansible collection 2.1.0+

## Getting Started

As this sample only uses the `cmci_action` module, no inventory is required, as
it can execute directly on the Ansible control node.

Variables must be supplied for the CMCI connection details.  Dummy variables are
provided in `group_vars/all.yml`, which you must edit to supply the CMCI 
connection details:

- `cmci_host` - Target CMCI hostname
- `cmci_port` - Target CMCI port
- `cmci_scheme` - CMCI scheme (http or https)
- `context` - Target CICSplex SM context
- `scope` - Target CICSplex SM scope

The playbook will prompt for the CMCI user name and password when it executes.

## Usage
```bash
ansible-playbook refresh_tags.yml
```

## How to determine CMCI module parameters

You can use CICS Explorer to tell a CICS region to re-read its tags file 
by right-clicking on a row in the "Regions" view and selecting "Refresh tags".

From this, we can determine that CMCI request runs against a CICS Region.
In order to use the `cmci_action` module to execute the same request we 
need to determine the CMCI resource name, and the action name.

The Regions view corresponds to the `CICSRGN` resource table.  Consulting the
[documentation](https://www.ibm.com/docs/en/cics-ts/latest?topic=tables-cicsrgn-resource-table) for that table, we can find the 
"External resource name (CMCI)" information, which is `CICSRegion`.  This is
the value we've filled in for the `type` parameter of the `cmci_action` module.

The action names can be found on the same page.  By looking at the available
actions we can identify that `TAGSREFR` is the name of the action we're looking
to run.  We've filled that in for the `action_name` parameter of the module.

As this action has no further parameters, we're done!  If the action did have parameters, they're also documented on the same page.