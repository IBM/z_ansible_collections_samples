# Retrieving operational data from running CICS regions

This sample playbook demonstrates how to use the `cmci_get` module from the `ibm_zos_cics` collection
to retrieve operational data from running CICS regions.  This example retrieves information corresponding to the
`CICSRGN` resource table, but can be adapted to retrieve information about any of the resource tables supported by CPSM.
The retrieved information is written to a csv file, suitable for opening as a spreadsheet.  This sample additionally
shows how to automate installation of pre-requisites for the CMCI modules.

## Ansible Collection Requirement

   IBM z/OS CICS collection 1.0.0 or greater
   
## Getting Started

The `cmci_*` modules use the *CICS Management Client Interface* or CMCI, to interact with your CICS environment.  To use
the CMCI modules you will need to have set up the CMCI HTTP API in your CICS environment.  You can enable the CMCI API
in both CICSplex environments, and in independent CICS regions.

For more information about CMCI, see the
[overview in the CICS TS knowledge center](https://www.ibm.com/support/knowledgecenter/SSGMCP_5.6.0/fundamentals/cpsm/cpsm-cmci-overview.html)

Because this playbook only uses the CMCI HTTP API, it can be run on the control node directly, without having to
configure an inventory.  Generally you'll be able to use this trick with any of the CMCI modules.  In this example, we
run the `cmci_get` module on `localhost`, i.e. the Ansible control node, by setting the target host to localhost.
Running the CMCI modules on the control node can be a good idea, because you don't have to deal with the complexity of
an unneessary SSH connection, and you don't have to install the modules' dependencies on the remote host.

## Run [report.yaml](report.yaml)

You can run the playbook without modification:
```bash
ansible-playbook report.yaml
````

The playbook will prompt for required parameters.  After parameters have been supplied, the playbook will install the
CMCI module dependencies to the python environment.  A CMCI GET request will be made to regions in the supplied CPSM
context, and a file `report.csv` will be written to the current directory with a report on a subset of attributes for
each region.  You should be able to open this file in a text editor, or as a spreadsheet to look at the results of the
report.

## What next?

- Try supplying the input parameters on the command line directly:

  ```bash
  ansible-playbook -e "context=MYCTXT cmci_host=example.com" report.yaml
  ```
  
  Parameters specified in this way won't be prompted for.

- Try editing `report.yaml` to change the attributes included in the report
  
  You may want to uncomment the debug task, to see the full respose from CPSM, to help you figure out the attribute
  names.
  
- Try adding a `filter` argument to the `cmci_get` task, to restrict the report to a subset of your CICS regions
  
- Try changing the playbook to request attributes for a different type of resource entirely.  You will have to use the
  CMCI resource name, which can be looked up
  [here](https://www.ibm.com/support/knowledgecenter/SSGMCP_5.6.0/reference-system-programming/cmci/clientapi_resources.html)

