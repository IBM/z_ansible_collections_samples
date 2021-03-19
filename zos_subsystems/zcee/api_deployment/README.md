# API Deployment

This sample playbook demonstrates basic use cases for generating and deploying
API archives, service archives, and API requester archives.


It is a good practice to review the playbook sample contents before executing
them. It will help you understand the requirements in terms of space, location,
names, authority, and the artifacts that will be created and cleaned up.
Although samples are written to operate without the need for the user’s
configuration, flexibility is written into the samples because it is not easy
to determine if a sample has access to the host’s resources. Review the
playbook notes sections for additional details and configuration.

## Prerequisites

* Provisioned instance of z/OS Connect EE Server

## Playbook Summary

- [**api-deployment.yml**](api-deployment.yml) - Lays out the entire process of utilizing the z/OS Connect EE roles in this repository to deploy APIs and Services to zCEE.

## Role Summary

- [**git_clone_private_repo**](roles/git_clone_private_repo/README.md) - Contains the process for copying remote private repository onto target host.
- [**install_zconbt**](roles/install_zconbt/README.md) - Installs the z/OS Connect Build Toolkit onto the target host if not present.
- [**prep_zconbt**](roles/prep_zconbt/README.md) - Prepares the environment to use the `zcee_generate_*` roles by creating proper folders and encoding to the proper configuration.
- [**zcee_generate_aar**](roles/zcee_generate_aar/README.md) - Generates the API Archive file given the project directory and archive file name.
- [**zcee_generate_ara**](roles/zcee_generate_ara/README.md) - Generates the API Requester Archive given the properties file and archive file name.
- [**zcee_generate_sar**](roles/zcee_generate_sar/README.md) - Generates the Service Archive file given the project directory and archive file name.

## Getting Started

If you are unfamiliar with playbooks, you can review our
[detailed configuration guide](https://github.com/IBM/z_ansible_collections_samples/blob/master/docs/share/configuration_guide.md) or
continue with getting started below.

Optionally, you can use the sample
[host_setup](https://github.com/IBM/z_ansible_collections_samples/blob/master/zos_administration/host_setup/README.md)
to discover and create your **inventory** and **host_vars** artifacts. It should
be noted that when you use the **host_setup** it will generate a configuration
for the most common dependencies, some playbooks require more customized
configurations, in this case, you can review the sample documentation and
add the additional required variables.

### Update [inventories/zos_host](inventories/zos_host) with the information about your system(s)

```yaml
# the system where the data should be copied to
source_system:
  hosts:
    zos_host:
      ansible_host: zos_target_address
      ansible_user: zos_target_username
      ansible_python_interpreter: path_to_python_interpreter_binary_on_zos_target
```

### Update the environment variables for each z/OS system in [host_vars/zos_host.yml](host_vars/zos_host.yml)

```yaml
# the path to the root of IBM python installation
PYZ: "/usr/lpp/IBM/cyp/v3r8/pyz"

# the path to root of ZOAU installation
ZOAU: "/usr/lpp/IBM/zoautil"
```

### Update the [default variables](roles/git_clone_private_repo/defaults/main.yml) for the *git_clone_private_repo* role with their corresponding values

Using this role, you are able to pull down an existing project from your private git repository onto your target host.

```yaml
# Github secret key that can be found on localhost to access github repository
secret_key: ~/path/to/secret/key

# This should always be overridden by higher prescedence variable
TARGET_USERNAME: user001

# Git repository to be cloned. Must be SSH (not Https)
GIT_REPO: git@github.com:MyCompany/my-zcee-app.git

# Specify the branch in your repository
GIT_BRANCH: api-deploy-branch

# Directory where the repository will be cloned
CLONE_DEST: "~/dest/to/my-zcee-app"
```

### Update the variables file [vars/api_deploy.yml](vars/api_deploy.yml)

This variables file is used to handle variables across all roles use in the `api-deployment` playbook.
```yaml
.
.
.

# Server name used to access the z/OS Connect EE REST API
ZCEE_SERVER: http://myzceeserver.com

# Port specified to access the z/OS Connect EE REST API
ZCEE_PORT: zcee_port

# Username used to access the z/OS Connect EE REST API
USERNAME: Fred

.
.
.
```

### Run desired playbook

```bash
ansible-playbook -i inventories/zos_host api-deployment.yml
```

# Copyright

© Copyright IBM Corporation 2020

# License

Licensed under [Apache License,
Version 2.0](https://opensource.org/licenses/Apache-2.0).

# Support

Please refer to the [support section](https://github.com/IBM/z_ansible_collections_samples/blob/master/README.md#support) for more
details.