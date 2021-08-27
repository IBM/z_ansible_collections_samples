# IBM® Cloud Infrastructure Center

IBM® Cloud Infrastructure Center is an advanced infrastructure management product, providing on-premises cloud deployments of IBM z/VM®-based and KVM based Linux® virtual machines on the IBM Z® and IBM LinuxONE platforms.

Before you get started with Ansible, familiarizing yourself with the basics of OpenShift and ICIC is recommended. The following provides a few good links for basic information. 

 [OpenShift Container Platform installation and update](https://docs.openshift.com/container-platform/4.8/architecture/architecture-installation.html#architecture-installation)

[IBM Cloud Infrastructure Center](https://www.ibm.com/docs/en/cic/1.1.4)

# About this playbook

The purposes of this playbook:

1. Use IBM® Cloud Infrastructure Center to install Red Hat® OpenShift® Container Platform via user-provisioned infrastructure (UPI)

2. Enhance the existing UPI process, such as 

- Rhcos images can be automatically downloaded and uploaded.

- Nodes’ IPs can be allocated instead of be fixed.

- Worker nodes’ CSR can be automatically approved.

- Additional scripts are provided: 

  1. Add or remove a new OpenShift compute node 

  2. Configure image registry. 

  3. Requirements pre-check before installation

**Note**: This playbook supports IBM® Cloud Infrastructure Center version 1.1.4 and RH OpenShift Container Platform version 4.6 and, 4.7, 4.8 for z/VM and version 4.7, 4.8 for KVM.

# Installing OpenShift on IBM® Cloud Infrastructure Center via user-provisioned infrastructure (UPI)

The User-Provisioned Infrastructure (UPI) process installs OpenShift in stages, providing opportunities for modifications or integrating with existing infrastructure.

It contrasts with the fully-automated Installer-Provisioned Infrastructure (IPI) which creates everything in one go.

With UPI, creating the cloud (OpenStack) resources (e.g. Nova servers, Neutron ports, security groups) is the responsibility of the person deploying OpenShift.

The installer is still used to generate the ignition files and monitor the installation process.

This provides a greater flexibility at the cost of a more explicit and interactive process.

Below is a step-by-step guide to a UPI installation that mimics an automated IPI installation; 

Please be aware of the [Known Issues](https://github.com/openshift/installer/blob/master/docs/user/openstack/known-issues.md#known-issues-specific-to-user-provisioned-installations)
of this method of installation.

# Workflow
![ocp_upi_ansible](https://media.github.ibm.com/user/95263/files/44cafe00-0743-11ec-98da-799a14c8ce9e)

This is the overall workflow running Ansible playbook, mainly has the following steps:
- [Prepare](#prepare)
- [Create Cluster](#create-cluster)

After above steps, you will get one ready OpenShift Platform Cluster on ICIC infrastructure. 
- [Destroy Cluster](#destroy-cluster)
- [Day2 Operation](#day2-operation)

# Quickstart

## Prepare

### 1. Prepare Servers

- **(Required)** Linux server, the machine that runs Ansible. It is typically your laptop.
    - Tested on RHEL8.
    - Ansible >= 2.8
    - This server **must not** be any ICIC nodes
    - You can use a single LPAR server or virtual machine if you want
      - Disk with at least 20GiB
- **(Optional)** Bastion server, the machine that used to configure DNS and Load Balancer.
    - This server **is not** mandatory, you can use your external DNS server or existing DNSHAProxy server instead of it.
    - You can also use the same Linux Server as bastion server if you don't have a separate bastion machine. 

### 2. Install packages on linux server

**Packages:**

* Python3
* Ansible >=2.8
* jq
* wget
* git
* tar
* gzip
* firewalld
* Python modules required in the playbooks. Namely:
  * openstackclient 
  * openstacksdk >= 0.57.0
  * netaddr

**Register:**

Use the following command to register the system, then automatically associate any available subscription matching that system:
```sh
sudo subscription-manager register --username <username> --password <password> --auto-attach
```
After registration, use the following command to enable ansible repository, or use newer version. 

**Note:** Our scenario is only tested for Ansible 2.8.18 on RHEL 8.2. 
```sh
sudo subscription-manager repos --enable=ansible-2.8-for-rhel-8-s390x-rpms 
```

**Install:**

Install the packages from the repository in the system:
```sh
sudo dnf install python3 ansible jq wget git firewalld tar gzip -y
```
Make sure that `python` points to Python3
```sh
sudo alternatives --set python /usr/bin/python3
```
Upgrade the pip package and dnf:
```sh
sudo -H pip3 install --upgrade pip
sudo dnf update -y
```
Install the required package through dnf:
```sh
sudo dnf install redhat-rpm-config gcc libffi-devel python3-devel openssl-devel cargo -y
```
Then create the requirements file and use pip3 to install the python modules:

**Note**: The requirements.txt are tested for python-openstackclient =5.5.0.
```sh
cat <<'EOF' >> requirements.txt 
# The order of packages is significant, because pip processes them in the order
# of appearance. Changing the order has an impact on the overall integration
# process, which may cause wedges in the gate later.
pbr!=2.1.0,>=2.0.0 # Apache-2.0

cliff>=3.5.0 # Apache-2.0
iso8601>=0.1.11 # MIT
openstacksdk>=0.57.0 # Apache-2.0
osc-lib>=2.3.0 # Apache-2.0
oslo.i18n>=3.15.3 # Apache-2.0
oslo.utils>=3.33.0 # Apache-2.0
python-keystoneclient>=3.22.0 # Apache-2.0
python-novaclient>=17.0.0 # Apache-2.0
python-cinderclient>=3.3.0 # Apache-2.0
stevedore>=2.0.1 # Apache-2.0
openstacksdk==0.57.0
netaddr==0.8.0
python-openstackclient==5.2.0
EOF

sudo pip3 install -r requirements.txt python-openstackclient --ignore-installed
``` 

**Verification:**
```sh
openstack
(openstack)
```

### 2. Setting ICIC environment variables on your linux server

Check [setting environment variables](https://www.ibm.com/docs/en/cic/1.1.4?topic=descriptions-setting-environment-variables) for more details.

1. If your linux server does not have SSH key, use below command-line SSH to generate a key pair: 
```sh
ssh-keygen -t rsa
```

2. Copy the key to ICIC management and bastion server:
```sh
ssh-copy-id user@host
```

3. Test the new key:
```sh
ssh user@host
```
The login should now complete without asking for a password. 

4. Copy the `icicrc` file from ICIC management node to your user's home directory:
```sh
scp -r user@host:/opt/ibm/icic/icicrc /opt/ibm/icic/icicrc
```

5. Copy the `icic.crt` file from ICIC management node to your certs directory:
```
scp -r user@host:/etc/pki/tls/certs/icic.crt /etc/pki/tls/certs/
```

6. Run source `icicrc` to set the environment variables:
```
source /opt/ibm/icic/icicrc
 Please input the username: admin
 Please input the password of admin:
```

7. Verify the linux server is connected to ICIC successfully.
```sh
openstack project list
+----------------------------------+-------------+
| ID                               | Name        |
+----------------------------------+-------------+
| 9c301e5e7b3a4e48a45e21690357c1be | icicvm      |
| 9deef79a109440c3b84e522296285904 | service     |
| bb7cf9dc985046048ccd39059ebfd6bc | ibm-default |
+----------------------------------+-------------+
```

8. Verify the ICIC all components are working as desired.
- Login ICIC command line and confirm all ICIC services status are running.
```sh
icic-services status
```
- Login ICIC web console and go to Home > Environment Checker, click Run Environment Checker button to confirm the cluster does not have any failed message.

If you meet any **not running** service or **failed** message, please according to ICIC [Troubleshooting](https://www.ibm.com/docs/en/cic/1.1.4?topic=troubleshooting) doc to fix it before running ansible playbook.


### 3. Download this playbook on your linux server

All the configuration files, logs and installation state are kept in a single directory:
```sh
git clone https://github.com/IBM/z_ansible_collections_samples.git
cp -r z_ansible_collections_samples/z_infra_provisioning/cloud_infra_center/ocp_upi ocp_upi
cd ocp_upi
```

### 4. Configure your settings in inventory.yaml

Update your settings based on the samples. The following propeties are **required**:

| Property | Default | Description | 
| ------ | ------ | ------ |
| `use_network_name` | \<network name from icic\> |`openstack network list -c Name -f value`|
| `use_network_subnet` | \<subnet id from network name in icic\> |`openstack network list -c Subnets -f value`|
| `vm_type` | kvm| 'kvm' or 'zvm'| |
| `disk_type` | dasd|'dasd' or 'scsi' | |
| `openshift_version` |4.7| '4.6' or '4.7' or '4.8'| |
| `openshift_minor_version` |7|'7' or '13' | |
| `auto_allocated_ip` |true| 'true' or 'false', if false, IPs will be allocated from `allocation_pool_start` and `allocation_pool_end` |
| `os_subnet_range` |\<subnet-range\> | If the os_subnet_range is `172.26.0.0/16`, the allocation pools will be `172.26.0.10-172.26.255.254` | |
| `os_flavor_master` | medium| `openstack flavor list` | |
| `os_flavor_worker` | medium| `openstack flavor list` | |
| `os_control_nodes_number` |3| Number of provisioned Control Plane nodes| |
| `os_compute_nodes_number` |3| Number of provisioned Compute nodes| |
| `availability_zone` |''| The availability zone in which to create the server, default value is '', which means to use the default availability zone.|
| `pullsecret` | \<pull-secret\> |  Get from [cloud.redhat.com](https://console.redhat.com/openshift/install/ibmz/user-provisioned)|
| `sshkey` | \<ssh-key\>| The SSH public key for the core user in RHEL CoreOS |
| `os_dns_domain` | \<external DNS ip addr\> or \<bastion ip addr\>|If you want to use your external or existing DNS server set `os_dns_domain` to use it, others set bastion machine ip address|
| `cluster_domain_name` |openshift.example.com|The cluster name and base domain that used in DNS forwarding| 

Others are **optional**, you don't have to update value or enable them if you don't need it.

> **Please note**: If you want to use a bastion server, you have to set correct value for bastion proprties.

| Property| <div style="width:180px">Default</div> | Description                           |
| --------------------------------------- | ------------------------------------- |:-----|
| `ansible_ssh_host` | \<linux server ip addr\> | 'x.x.x.x'<br> **required** when use bastion server
| `bastion_private_ip_address` | \<bastion ip addr\>      |IP address of your private subnet<br>**required** when use bastion server
| `cluster_subnet_range` |\<cluster subnet range\>       |The queries from IPs in `cluster_subnet_range` will be allowed, we suggest setting the same as `os_subnet_range`  <br>**required** when use bastion server
| `dns_forwarder` | \<upstream DNS ip addr\> |For nameserver where requests should be forwarded for resolution.<br>**required** when use bastion server
| `allocation_pool_start` |\<ip range start\> |'x.x.x.x'
| `allocation_pool_end` |\<ip range end\> |'x.x.x.x'
| `os_bootstrap_ip` | \<bootstrap ip addr\> |'x.x.x.x'
| `os_master_ip` | \<master ip list\>|'[x.x.x.x, x.x.x.x, x.x.x.x]'
| `os_infra_ip` |\<infra ip list\>|'[x.x.x.x, x.x.x.x, x.x.x.x]'


## Create Cluster

- `$ ansible-playbook -i inventory.yaml 01-preparation.yaml`

> Skip to run bastion.yaml if you want to use your existing DNS/HAProxy or external DNS/HAProxy, you can refer [Add-DNS-HAProxy](docs/add-dns-haproxy.md) to update it.

- `ansible-playbook -i inventory.yaml bastion.yaml`
- `ansible-playbook -i inventory.yaml 02-create-cluster-control.yaml`
- `ansible-playbook -i inventory.yaml 03-create-cluster-compute.yaml`

## Destroy Cluster

### Something is wrong? clean up environment
Depending on different cases to choose suitable destroying.
+ You might get some generic errors during playbook `01-preparation.yaml` and `bastion.yaml`,please use:

`ansible-playbook -i inventory.yaml 04-destroy.yaml`

+ You might get some ICIC create control errors during playbook `02-create-cluster-control.yaml`,please use:

`ansible-playbook -i inventory.yaml 04-destroy.yaml`

+ You might get some ICIC create compute server error during playbook `02-create-cluster-compute.yaml`,please use:

`ansible-playbook -i inventory.yaml destroy-computes.yaml`

## Day2 Operation

### Add a new compute node
If you want to add new compute node as allocated IP:
```sh
ansible-playbook -i inventory.yaml add-new-compute-node.yaml 
```
If you want to add new compute node as fixed IP:
```sh
ansible-playbook -i inventory.yaml add-new-compute-node.yaml -e ip=x.x.x.xs
```

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
