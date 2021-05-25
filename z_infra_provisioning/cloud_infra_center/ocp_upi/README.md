# IBM® Cloud Infrastructure Center

IBM® Cloud Infrastructure Center is an advanced infrastructure management offering, including on-premises cloud deployments of IBM z/VM®-based and KVM based Linux® virtual machines on the IBM Z® and IBM LinuxONE platforms.

More details: https://www.ibm.com/docs/en/cic/1.1.3

# About this playbook

The purposes of this playbook:

1. Use IBM® Cloud Infrastructure Center to install Red Hat® OpenShift® Container Platform with user-provisioned infrastructure (UPI)

2. Enhance the existing UPI process, such as 

- Rhcos images can be automatically downloaded and uploaded.

- Nodes’ IPs can be allocated instead of be fixed.

- Worker nodes’ CSR can be automatically approved.

- Additional scripts are provided: 

  1. Add or remove a new OpenShift compute node 

  2. Configure image registry. 

  3. Requirements pre-check before installation

**Note**: This playbook support version of IBM® Cloud Infrastructure Center now is only 1.1.3 . And support version of OpenShift Container Platform is 4.6, 4.7 for z/VM and 4.7 for KVM.

# Installing OpenShift on IBM® Cloud Infrastructure Center with user-provisioned infrastructure (UPI)

The User-Provisioned Infrastructure (UPI) process installs OpenShift in stages, providing opportunities for modifications or integrating with existing infrastructure.

It contrasts with the fully-automated Installer-Provisioned Infrastructure (IPI) which creates everything in one go.

With UPI, creating the cloud (OpenStack) resources (e.g. Nova servers, Neutron ports, security groups) is the responsibility of the person deploying OpenShift.

The installer is still used to generate the ignition files and monitor the installation process.

This provides a greater flexibility at the cost of a more explicit and interactive process.

Below is a step-by-step guide to a UPI installation that mimics an automated IPI installation; prerequisites and steps described below should be adapted to the constraints of the target infrastructure.

Please be aware of the [Known Issues](known-issues.md#known-issues-specific-to-user-provisioned-installations)
of this method of installation.

## Prerequisites

The file `inventory.yaml` contains the variables most likely to need customisation.

For a successful IBM® Cloud Infrastructure Center UPI installation (3 worker nodes) it is required:

Security Groups: 3
Security Group Rules: 60
Subnets: 1
Server Groups: 1
RAM: 112 GB
vCPUs: 28
Volume Storage: 175 GB
Instances: 7
Depending on the type of image registry backend an additional 100 GB volume.

- IBM® Cloud Infrastructure Center virtual machines type ("kvm" or "zvm")
  - inventory: `vm_type`
- IBM® Cloud Infrastructure Center disk type ("dasd" or "scsi")
  - inventory: `disk_type`
- OpenShift version
  - inventory: `openshift_version` and `openshift_minor_version`
- Nova flavors
  - inventory: `os_flavor_master` and `os_flavor_worker`
- The `openshift-install` binary
- A subnet range for the Nova servers / OpenShift Nodes, that does not conflict with your existing network, and 
  - inventory: `os_subnet_range`
- The RHCOS image to use, the default value is "rhcos"
  - the default value is "rhcos" 
  - inventory: `os_image_rhcos`
- A DNS zone you can configure
  - it must be the resolver for the base domain, for the installer and for the end-user machines
  - it will host two records: for API and apps access
  - inventory: `vars.os_dns_domain`
- Bastion node
  - If you don't have your own DNS server and Load Balancer, you can choose to configure them on a RHEL8 machine, which called a "Bastion Node".
  - If using bastion node, ensure that you can ssh to bastion node via root user without password.
  - If using bastion node, uncomment `bastion` and `vars.cluster_nodes` part in inventory.yaml and fill the information of bastion node and OpenShift cluster.
  - inventory: `bastion`, `vars.cluster_domain_name`, `vars.cluster_nodes`

## Install Ansible

This repository contains Ansible playbooks to deploy OpenShift on IBM® Cloud Infrastructure Center.

**Requirements:**

* Python
* Ansible
* Python modules required in the playbooks. Namely:
  * openstackclient
  * openstacksdk
  * netaddr

### RHEL

From a RHEL 8 box, make sure that the repository origins are all set:

```sh
sudo subscription-manager register # if not done already
sudo subscription-manager attach --pool=$YOUR_POOLID # if not done already
sudo subscription-manager repos --disable=* # if not done already

## TODO replace repos to s390x repos
sudo subscription-manager repos \
  --enable=rhel-8-for-s390x-baseos-rpms \
  --enable=openstack-16-tools-for-rhel-8-s390x-rpms \
  --enable=ansible-2.8-for-rhel-8-s390x-rpms \
  --enable=rhel-8-for-s390x-appstream-rpms
```

Then install the packages:
```sh
sudo yum install python3-openstackclient ansible python3-openstacksdk python3-netaddr jq
```

Make sure that `python` points to Python3:
```sh
sudo alternatives --set python /usr/bin/python3
```

## OpenShift Configuration Directory

All the configuration files, logs and installation state are kept in a single directory:

```sh
$ mkdir -p icic-upi
$ cd icic-upi
```

## Prepare the configuration before installation

### OpenShift installer and CLI

Config the `openshift_version` and `openshift_minor_version` in inventory.yaml, the ansible will download the specified openshift installer and CLI binary files.

### Red Hat Enterprise Linux CoreOS (RHCOS) image

A proper [RHCOS](https://docs.openshift.com/container-platform/4.7/architecture/architecture-rhcos.html) image in the IBM® Cloud Infrastructure Center cluster or project is required for successful installation.

To use Red Hat Enterprise Linux CoreOS (RHCOS) images to provision virtual machines, ansible playbooks will download the images from the RHCOS [image mirror](https://mirror.openshift.com/pub/openshift-v4/s390x/dependencies/rhcos/). The images then can be uploaded to IBM® Cloud Infrastructure Center to provision a virtual machine.

**z/VM**

Supported versions: 4.6, 4.7 .

Two types of RHCOS images are supported.

- RHCOS DASD images
- RHCOS SCSI images

Config the `vm_type` as "zvm" and `disk_type` as "dasd" or "scsi" in inventory.yaml

**KVM**

Supported version: 4.7 .

Config the `vm_type` as "kvm" and `disk_type` as "" in inventory.yaml

### Prepare configuration and ignition files before installation

The ansible playbooks has automated origin complex UPI mannul process, such as correcting configuration, creating manifests, creating ignition files and uploading ignition files to glance. 

### Config HAproxy and DNS on a bastion node (default)

If there isn't your own loadbalancer and DNS server, you can deploy a bastion node and use this playbooks to config an HAproxy and DNS server on bastion node. 

1. Deploy a bastion node which ansible installed and remote ssh-key configure.

2. Update bastion node info, dns domain and OCP nodes IP addresses in inventory.yaml

```
    # Bastion DNS and HAProxy settings (Only for fixed IP address)
    bastion:
      ansible_ssh_host: 172.26.103.100
      ansible_ssh_common_args: '-o StrictHostKeyChecking=no'

      # Configure the IP address of your private subnet
      # These default values will work by default
      # with the default configuration defined in
      # tony-hypervisor-setup
      bastion_private_ip_address: 172.26.103.100
      gateway_ip: 172.26.0.1
      bastion_subnet_prefix_reverse: 103.26.172
      cluster_subnet_prefix: 172.26.103
      bastion_public_ip_address: "{{ ansible_default_ipv4.address }}"
  
  vars:
    os_dns_domain: "172.26.103.100"
    cluster_domain_name: "openshift.example.com"
```

### Modify other user-provided values in inventory.yaml

1. Allocated IP or Fixed IP:

The auto_allocated_ip default value is true, then IPs will be allocated from subnet range. 
If you need specify IP addresses, set auto_allocated_ip to false.

```
auto_allocated_ip: true
```

### Prepare the install config file

Prepare the install-config.yaml as below: 

```
apiVersion: v1
baseDomain: example.com
compute:
- architecture: s390x
  hyperthreading: Enabled
  name: worker
  platform: {}
  replicas: 0
controlPlane:
  architecture: s390x
  hyperthreading: Enabled
  name: master
  platform: {}
  replicas: 3
metadata:
  creationTimestamp: null
  name: openshift
networking:
  clusterNetwork:
  - cidr: 10.128.0.0/14
    hostPrefix: 23
  machineNetwork:
  - cidr: 10.0.0.0/16
  networkType: OpenShiftSDN
  serviceNetwork:
  - 172.30.0.0/16
platform:
  {}
publish: External
pullSecret: xxxxx
sshKey: xxxxxx
```
Most of these are self-explanatory. *metadata.name* and *baseDomain* will together form the fully qualified domain name which the API interface will expect to the called, and the default name with which OpenShift will expose newly created applications. And it should be the same as `cluster_domain_name` in inventory.yaml.

Afterwards, you should have `install-config.yaml` in your current directory:

```sh
$ tree
.
└── install-config.yaml
```

**Note**:

## The step to use this ansible playbooks

Configure IBM® Cloud Infrastructure Center environment variable

```sh
$ source /opt/ibm/icic/icicrc <user> <password>
```

### Check requirements for enviroment before installation

You can run below command to check whether your enviroment meet the minimum requirements:

```
$ ansible-playbook -i inventory.yaml configure-pre-check.yaml
```

### Prepare configuration and ignition files

Before OpenShift installation, some special configuration should be modified, such as configure machine network, generate and upload igntions.

```sh
$ ansible-playbook -i inventory.yaml main.yaml
```

### Security Groups

```sh
$ ansible-playbook -i inventory.yaml configure-security-groups.yaml
```

The playbook creates one Security group for the Control Plane and one for the Compute nodes, then attaches rules for enabling communication between the nodes.

### Network, Subnet and external router

```sh
$ ansible-playbook -i inventory.yaml configure-network.yaml
```

The playbook creates a network and a subnet. The subnet obeys `os_subnet_range`. By default, if the `os_subnet_range` is '172.26.0.0/16', the allocation pools will be '172.26.0.10-172.26.255.254'. The allocation pool can be configured by `allocation_pool_start` and `allocation_pool_end`. Finally, the playbook creates the nodes' ports.

**Use existing network**

If you want to use an existing netowrk instead of creating a new network, you can specify the 
network name and subnet UUID, for example:

```
$ ansible-playbook -i inventory.yaml configure-network.yaml -e os_network="openshift-knjws-network" -e os_subnet="26715757-6e9c-4434-af2f-65de972078f0"
```

### Configure bastion

```sh
$ ansible-playbook -i inventory.yaml configure-bastion-properties.yaml
$ ansible-playbook -i inventory.yaml configure-dns.yaml
$ ansible-playbook -i inventory.yaml configure-haproxy.yaml
```

The nodes' hostnames and IP addresses will be read from nodes' port and configured in bastion node's DNS and Haproxy.

### OpenShift Bootstrap

```sh
$ ansible-playbook -i inventory.yaml configure-bootstrap.yaml
```

The playbook sets the *allowed address pairs* on each port attached to our OpenShift nodes.

After the bootstrap server is active, you can check the console log to see that it is getting the ignition correctly:

```sh
$ openstack console log show "$INFRA_ID-bootstrap"
```

You can also SSH into the server (using its floating IP address) and check on the bootstrapping progress:

```sh
$ ssh core@203.0.113.24
[core@openshift-qlvwv-bootstrap ~]$ journalctl -b -f -u bootkube.service
```

### OpenShift Control Plane

```sh
$ ansible-playbook -i inventory.yaml configure-control-plane.yaml
```

Our control plane will consist of three nodes. The servers will be passed the `master-?-ignition.json` files prepared earlier.

The playbook places the Control Plane in a Server Group with "soft anti-affinity" policy.

The master nodes should load the initial Ignition and then keep waiting until the bootstrap node stands up the Machine Config Server which will provide the rest of the configuration.

### Wait for the Control Plane to Complete

When that happens, the masters will start running their own pods, run etcd and join the "bootstrap" cluster. Eventually, they will form a fully operational control plane.

You can monitor this via the following command:

```sh
$ ./openshift-install wait-for bootstrap-complete
```

Eventually, it should output the following:

```plaintext
INFO API v1.14.6+f9b5405 up
INFO Waiting up to 30m0s for bootstrapping to complete...
```

This means the masters have come up successfully and are joining the cluster.

Eventually, the `wait-for` command should end with:

```plaintext
INFO It is now safe to remove the bootstrap resources
```

### Access the OpenShift API

You can use the `oc` or `kubectl` commands to talk to the OpenShift API. The admin credentials are in `auth/kubeconfig`:

```sh
$ export KUBECONFIG="$PWD/auth/kubeconfig"
$ oc get nodes
$ oc get pods -A
```

**NOTE**: Only the API will be up at this point. The OpenShift UI will run on the compute nodes.

### Delete the Bootstrap Resources

```sh
$ ansible-playbook -i inventory.yaml down-bootstrap.yaml
```

The teardown playbook deletes the bootstrap port, server and floating IP address.

If you haven't done so already, you should also disable the bootstrap Ignition URL.

### OpenShift Compute Nodes

```sh
$ ansible-playbook -i inventory.yaml configure-compute-nodes.yaml
```

This process is similar to the masters, but the workers need to be approved before they're allowed to join the cluster.

The workers need no ignition override.

### Configure the storage backend of image registry 

```sh
$ ansible-playbook -i inventory.yaml configure-image-registry.yaml
```

The Image Registry Operator is not initially available for platforms that do not provide default storage. We set the image registry to an empty directory for non-production clusters.

And the image registry will be exposed, this allows you to log in to the registry from outside the cluster using the route address, and to tag and push images using the route host.

Log in with podman: 

```sh
$ HOST=$(oc get route default-route -n openshift-image-registry --template='{{ .spec.host }}')
$ podman login -u $(oc whoami) -p $(oc whoami -t) --tls-verify=false $HOST
```

### Wait for the OpenShift Installation to Complete

Run the following command to verify the OpenShift cluster is fully deployed:

```sh
$ ./openshift-install --log-level debug wait-for install-complete
```

Upon success, it will print the URL to the OpenShift Console (the web UI) as well as admin username and password to log in.

### Add a new OpenShift compute node

Allocated IP:

```sh
$ ansible-playbook -i inventory.yaml add-new-compute-node.yaml 
```

Fixed IP:

```sh
$ ansible-playbook -i inventory.yaml add-new-compute-node.yaml -e ip=172.26.104.34
```

You can add a new compute node after installation, and specify the hostname index and ip address.  

## Destroy the OpenShift Cluster

```sh
$ ansible-playbook -i inventory.yaml  \
	down-bootstrap.yaml      \
	down-control-plane.yaml  \
	down-compute-nodes.yaml  \
	down-network.yaml        \
	down-security-groups.yaml
```

Then, remove the `api` and `*.apps` DNS records.

## Copyright
© Copyright IBM Corporation 2021

## License
Licensed under
[Apache License, Version 2.0](https://opensource.org/licenses/Apache-2.0).

## Support
Support for all playbooks, roles, and filters are provided by the community
and all issues are tracked through a
[Git issue](https://github.com/IBM/z_ansible_collections_samples/issues).
The repository admins and content owners will engage directly with users on issues reported on the
Git issue.

Playbooks are contributed both by IBM as well as the broader Ansible community including IBM partners and the Z community. 
Therefore, it may be helpful to review who contributed a sample as well as its requirements. You
can view who the contributor was by looking at the playbooks commit history as
well as notes in the playbook.

