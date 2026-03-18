# z/OSMF Software Update Samples

Welcome to the z/OSMF Software Update Samples!

This set of files is meant to serve as a set of examples to get you started with the Software Update portion of our z/OSMF Ansible collection and new Software Update APIs

We'll walkthrough, provide, and demonstrate a few things for you such as the following:

- The installation of the update to the z/OSMF Ansible collection.
- Getting set up to run with our sample playbooks.
- Walkthroughs on the running of our sample playbooks.
- Locating the roles so that you can build your own enhanced playbooks!
- Nuances and gotchas that may occur along the way!

## Installing the New z/OSMF Collection

Installing the new collection updates should be pretty simple considering you already have ansible installed.

You need to have our collection `ibm.ibm_zosmf >= 1.6.0` installed.

```bash
ansible-galaxy collection install ibm.ibm_zosmf
```

That should install the collection in `~/.ansible/collections` under the `ansible_collections` directory. That is the default that Ansible chooses as a location to house your collections.

That's it! Pretty simple!

**Note: if you have a previous version installed (1.5.0 and below) this should work no problem. If for any reason that doesn't work, because there happens to be difficulties with your environment. You can run with the force option as shown below.*

```bash
ansible-galaxy collection install ibm.ibm_zosmf --force
```

If you're trying to see if you've set up everything correctly you can run the provided `example-playbooks/missing_crit.yaml` playbook and fill in the variables for it in the `example-variables` folder. While it's not a part of the update roles it will let you know we're connected correctly.

## Getting Set Up to Run with our Sample Playbooks and Locating the Roles

When running with the software update roles we're connecting to z/OSMF and driving work through that interface. Because of that we're running a little differently than a regular Ansible setup and want to explain some of those nuances here!

When we connect with our provided roles we're going to use some special variables that we've defined. Below we'll provide the roles and linked web pages on what variables can be used with what roles.

The new roles to look at are the following:

- [**zmf_swupdate_cancel**](https://ibm.github.io/z_ansible_collections_doc/ibm_zosmf/docs/source/roles/zmf_swupdate_cancel.html) – Cancel a Software Update Process
- [**zmf_swupdate_copy**](https://ibm.github.io/z_ansible_collections_doc/ibm_zosmf/docs/source/roles/zmf_swupdate_copy.html) – Copy a Software Update Process
- [**zmf_swupdate_resume**](https://ibm.github.io/z_ansible_collections_doc/ibm_zosmf/docs/source/roles/zmf_swupdate_resume.html) – Resume a Software Update Process
- [**zmf_swupdate_retrieve**](https://ibm.github.io/z_ansible_collections_doc/ibm_zosmf/docs/source/roles/zmf_swupdate_retrieve.html) – Retrieve the Status of a Software Update Process
- [**zmf_swupdate_retrieve_all**](https://ibm.github.io/z_ansible_collections_doc/ibm_zosmf/docs/source/roles/zmf_swupdate_retrieve_all.html) – Retrieve the Status of all Software Update Processes
- [**zmf_swupdate_start**](https://ibm.github.io/z_ansible_collections_doc/ibm_zosmf/docs/source/roles/zmf_swupdate_start.html) – Start a Software Update Process

We recommend using the `-e` parameter with these ansible playbooks. That equates out to the `extra-vars` flag. That basically allows us to make the playbooks runnable without having to hardcode the needed variables. We can pass those variables along at runtime.

## zOSMF Software Update Examples

In this set of example files we have a few sample playbooks that we've written for you to try out prefabricated upgrade scenarios. Those are as follows:

- A Software Update playbook where we **retrieve the status of the latest software update process for a software instance**.
- A Software Update playbook where we **retrieve the status of all software update processes for a software instance**.
- A Software Update playbook where we **start a software update and go through the entire process resolving all system holds**.
- A Software Update playbook where we **start a software update and wait for user intervention to do things like handle holds**.
- A Software Update playbook dedicated to **resuming after manually resolving the aforementioned holds**.
- A Software Update playbook dedicated to **cancelling a software update process**.

## Walkthrough on the running of our sample playbooks

Running our sample playbooks should be pretty simple. The way that our playbooks are built they're designed to be reusable. We're going to alter what we're looking to run software update on through a series of variable files written in `yaml`. In the `/example-variables` repository we have variable files that you can fill in. They will serve as interchangeable pieces that we'll use in our commands. You can copy the samples and swap them in and out at runtime with the command provided below.

***Note: This is our recommended way of setting up these playbooks. Feel free to set them up however you see fit.**

The command to run our playbooks will look something like this. From the root of our project you'd run the following. As mentioned before we're using the `-e` parameter to include the aforementioned variable files.

***Note: Notice how when using a file with the `-e` parameter we have an `@` symbol preceding the file path. That is needed by ansible. Without that ansible doesn't register the value as a file path and the playbook will fail.**

***Note: We're providing a sample inventory because it's required by Ansible. Ansible can be run locally or remotely using the `hosts` parameter in the notebook; `nodes` in our variable files. Most users want to run their playbooks from the machine they're on which would be `localhost`. For our usage, we're using REST services for our API calls, providing `localhost` as the `host/node` is typically preferred. If you'd like to use an alternate `host/node` feel free to do so.**

```bash
ansible-playbook -i example-inventory/inventory.yaml example-playbooks/software_management_swu_start.yml -e "@./example-variables/start_variables.yaml"
```
