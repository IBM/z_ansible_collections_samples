# IBM® Cloud Infrastructure Center

IBM® Cloud Infrastructure Center is an advanced infrastructure management product, providing on-premises cloud deployments of IBM z/VM®-based and KVM based Linux® virtual machines on the IBM Z® and IBM LinuxONE platforms.

Before you get started with Ansible, familiarize yourself with the basics of Red Hat® OpenShift® and IBM Cloud Infrastructure Center. The following links provide basic information and an overview. 

 [Red Hat OpenShift Container Platform installation and update](https://docs.openshift.com/container-platform/4.8/architecture/architecture-installation.html#architecture-installation)

[IBM Cloud Infrastructure Center](https://www.ibm.com/docs/en/cic/1.1.4)

# About this playbook

The playbook contains the following topics:

1. How to use the IBM Cloud Infrastructure Center to install Red Hat OpenShift Container Platform via user-provisioned infrastructure (UPI)

2. Enhance the existing UPI process, such as 

- Red Hat OpenShift images can be automatically downloaded and uploaded.

- Nodes’ IPs can be allocated instead of being fixed.

- Worker nodes’ CSR can be automatically approved.

- Additional scripts are provided: 

  1. Add or remove a new Red Hat OpenShift compute node 

  2. Configure the image registry

  3. Requirements pre-check before the installation

**Note**: This playbook supports IBM® Cloud Infrastructure Center version 1.1.4 and RH OpenShift Container Platform version 4.6 and, 4.7, 4.8 for z/VM and version 4.7, 4.8 for KVM.

# Installing Red Hat OpenShift on the IBM Cloud Infrastructure Center via user-provisioned infrastructure (UPI)

The User-Provisioned Infrastructure (UPI) process installs Red Hat OpenShift in steps, providing the opportunity for modifications or integration into the existing infrastructure.

Contrary to the fully-automated Installer-Provisioned Infrastructure (IPI) that creates everything in one go.

Creating the cloud (OpenStack) resources (e.g. Nova servers, Neutron ports, security groups) is the responsibility of the person deploying Red Hat OpenShift using the UPI.

The UPI installer is used to generate the ignition files and monitor the installation process. 

This provides a greater flexibility at the cost of a more explicit and interactive process.

Below is a step-by-step guide how to do a UPI installation that mimics an automated IPI installation;

Check the [Known Issues](https://github.com/openshift/installer/blob/master/docs/user/openstack/known-issues.md#known-issues-specific-to-user-provisioned-installations)
of this method of installation.

# Workflow
![ocp_upi_ansible](https://media.github.ibm.com/user/95263/files/febe2900-0efe-11ec-82e9-b0d714a4a51c)

This is the overall workflow running Ansible playbook, mainly has the following steps:
- [Prepare](#prepare)
- [Create Cluster](#create-cluster)
  - [Something is Wrong](#something-is-wrong)

After you performed the previous steps successfully, you get one ready OpenShift Container Platform on the IBM Cloud Infrastructure Center. 
- [Day2 Operation](#day2-operation)
- [Uninstall Red Hat OpenShift Container Platform](#uninstall-red-hat-openshift-container-platform)

# Quickstart

## Prerequisites
-  You have installed the IBM Cloud Infrastructure Center

## Prepare

### 1. Preparation of the servers

- **(Required)** A Linux server, the machine that runs Ansible.
    - RHEL8 is the operation system version we tested
    - Ansible >= 2.8
    - This server **must not** be any of the IBM Cloud Infrastructure Center nodes
    - You can use a single LPAR server or virtual machine
      - Disk with at least 20 GiB
- **(Optional)** A Bastion server, a machine that is used to configure DNS and Load Balancer for the Red Hat OpenShift installation
    - If you use your own existing DNS server and Load Balancer for the Red Hat OpenShift installation, the bastion server **is not** required.
    - If you have external or existing DNS server, but no Load Balancer for the Red Hat Openshift installation, please set `os_dns_domain` property, and then use a separate YAML `configure-haproxy` to configure the HAProxy in bastion server.
    - If you have existing Load Balancer, but no DNS server for the Red Hat Openshift installation, you can use a separate YAML `configure-dns` to configure the DNS server in bastion server.
    - If you don't have any existing DNS server or Load Balancer for the Red Hat Openshift installation, you need to create one Linux server as the bastion server and run playbook to configure DNS server and Load Balancer. You can also use the same Linux server that runs Ansible.

### 2. Installation of packages on a Linux server

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

Use the following command to register the Linux server, then automatically associate any available subscription matching that system:
```sh
sudo subscription-manager register --username <username> --password <password> --auto-attach
```
After registration, use the following command to enable ansible repository, or use a newer version of your installed systems. 

**Note:** Our scenario is only tested for Ansible 2.8.18 on RHEL 8.2. 
```sh
sudo subscription-manager repos --enable=ansible-2.8-for-rhel-8-s390x-rpms 
```

**Installation:**

Install the packages from the repository in the Linux server:
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

### 3. Setting the IBM Cloud Infrastructure Center environment variables on your Linux server

Check’ [setting environment variables](https://www.ibm.com/docs/en/cic/1.1.4?topic=descriptions-setting-environment-variables) for more details.

1. If your Linux server does not have SSH key, use the following command-line SSH to generate a key pair: 
```sh
ssh-keygen -t rsa
```

2. Copy the key to the IBM Cloud Infrastructure Center management node and bastion server:
```sh
ssh-copy-id user@host
```

3. Test the new key:
```sh
ssh user@host
```
The login should now complete without asking for a password. 

4. Copy the `icicrc` file from the IBM Cloud Infrastructure Center management node to your user's `/opt/ibm/icic/icicrc` directory:
```sh
scp -r user@host:/opt/ibm/icic/icicrc /opt/ibm/icic/icicrc
```

5. Copy the `icic.crt` file from the IBM Cloud Infrastructure Center management node to your certs directory `/etc/pki/tls/certs/`:
```
scp -r user@host:/etc/pki/tls/certs/icic.crt /etc/pki/tls/certs/
```

6. Run the source `icicrc` to set the environment variables:
```
source /opt/ibm/icic/icicrc
 Please input the username: admin
 Please input the password of admin:
```

7. Verify the Linux server is connected to the IBM Cloud Infrastructure Center successfully.
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

8. Verify that all components in the IBM Cloud Infrastructure Center all components are working as desired.
- Login to the IBM Cloud Infrastructure Center command line and confirm all the IBM Cloud Infrastructure Center services are in status running.
```sh
icic-services status
```
- Login to the IBM Cloud Infrastructure Center web console and select Home > Environment Checker, click the Run Environment Checker button to confirm the cluster does not have any failed messages.

If you meet any **not running** service or **failed** message, check the IBM Cloud Infrastructure Center [Troubleshooting](https://www.ibm.com/docs/en/cic/1.1.4?topic=troubleshooting) document to fix before running the ansible playbook.


### 4. Download this playbook on your Linux server

All the configuration files, logs and installation state are kept in a single directory:
```sh
git clone https://github.com/IBM/z_ansible_collections_samples.git
cp -r z_ansible_collections_samples/z_infra_provisioning/cloud_infra_center/ocp_upi ocp_upi
cd ocp_upi
```

### 5. Configure your settings in inventory.yaml

Update your settings based on the samples. The following propeties are **required**:

| Property | Default | Description | 
| ------ | ------ | ------ |
| `use_network_name` | \<network name from icic\> |`openstack network list -c Name -f value`|
| `use_network_subnet` | \<subnet id from network name in icic\> |`openstack network list -c Subnets -f value`|
| `vm_type` | kvm| The operation system of OpenShift Container Platform, <br>supported: `kvm` or `zvm`| |
| `disk_type` | dasd|The disk storage of OpenShift Container Platform, <br>supported: `dasd` or `scsi` | |
| `openshift_version` |4.7| The product version of OpenShift Container Platform, <br>such as `4.6` or `4.7` or `4.8`| |
| `openshift_minor_version` |7| The minor version of Openshift Container Platform, <br>such as `7` or `13` | 
| `auto_allocated_ip` |true|(Boolean) true or false, if false, <br>IPs will be allocated from `allocation_pool_start` and `allocation_pool_end` |
| `os_subnet_range` |\<subnet-range\> | If the os_subnet_range is `172.26.0.0/16`, <br>the allocation pools will be `172.26.0.10-172.26.255.254` | |
| `os_flavor_bootstrap` | medium| `openstack flavor list`, Minimum flavor size >= 35 GiB  | |
| `os_flavor_master` | medium| `openstack flavor list`, Minimum flavor size >= 35 GiB | |
| `os_flavor_worker` | medium| `openstack flavor list`, Minimum flavor size >= 35 GiB  | |
| `os_control_nodes_number` |3| (Integer) Number of Red Hat Openshift provisioned Control Plane nodes| |
| `os_compute_nodes_number` |3| (Integer) Number of Red Hat Openshift provisioned Compute nodes| |
| `create_server_zone` |''| The zone you can select which host instances are launched on and which roles can boot instances on this host, the value format is `ZONE:HOST:NODE`, HOST and NODE are optional parameters, in such cases, use the `ZONE::NODE`, `ZONE:HOST` or `ZONE`. <br>Default value is '', which means to use the default availability zone. <br>[ **ZONE** is `Zone Name` column from `openstack availability zone list`; **HOST** is `Host Name` column from `openstack host list`; **NODE** is `Hypervisor Hostname` column from `openstack hypervisor list`]|
| `pullsecret` | \<pull-secret\> |  Get from [cloud.redhat.com](https://console.redhat.com/openshift/install/ibmz/user-provisioned)|
| `sshkey` | \<ssh-key\>| The SSH public key for the core user in RHEL CoreOS |
| `os_dns_domain` | \<external DNS ip addr\> or \<bastion ip addr\>|If you want to use your external or existing DNS server set `os_dns_domain` to use it, others set bastion machine ip address|
| `cluster_name` | \<cluster-name\> |The name of the cluster, such as `openshift`.| 
| `base_domain` | \<cluster-base-domain\> |The base domain of the cluster, the base domain is used to create routes to your OpenShift Container Platform cluster components, such as `example.com`| 

If you need the Ansible playbook to help configure DNS server or HAProxy server on bastion server, you need to configure correct bastion properties.
| Property| <div style="width:220px">Default</div> | Description                           |
| --------------------------------------- | ------------------------------------- |:-----|
| `ansible_ssh_host` | \<linux server ip addr\> | 'x.x.x.x'<br> **required** when use bastion server, give the IP address of bastion server.
| `bastion_private_ip_address` | \<bastion ip addr\>      |IP address of your bastion node<br>**required** when use bastion server, give the IP address of bastion server.
| `cluster_subnet_range` |\<cluster subnet range\>       |The queries from IPs in `cluster_subnet_range` will be allowed, we suggest setting the same as `os_subnet_range`  <br>**required** when use bastion server
| `dns_forwarder` | \<upstream DNS ip addr\> |For nameserver where requests should be forwarded for resolution.<br>**required** when use bastion server

Others are **optional**, you can enable them and update value if you need more specified settings.

| Property| <div style="width:220px">Default</div> | Description                           |
| --------------------------------------- | ------------------------------------- |:-----|
| `allocation_pool_start` |\<ip range start\> |'x.x.x.x'
| `allocation_pool_end` |\<ip range end\> |'x.x.x.x'
| `os_bootstrap_ip` | \<bootstrap ip addr\> |'x.x.x.x, <br>**required** when `auto_allocated_ip` is false
| `os_master_ip` | \<master ip list\>|'[x.x.x.x, x.x.x.x, x.x.x.x], <br>**required** when `auto_allocated_ip` is false
| `os_infra_ip` |\<infra ip list\>|'[x.x.x.x, x.x.x.x, x.x.x.x], <br>**required** when `auto_allocated_ip` is false
| `use_proxy` |false|(Boolean) true or false, if true then Openshft Container Platform will use the proxy setting
| `http_proxy` |\<http-proxy\>| `http://<username>:<pswd>@<ip>:<port>`, a proxy URL to use for creating HTTP connections outside the cluster. <br>**required** when `use_proxy` is true
| `https_proxy` |\<https-proxy\>| `http://<username>:<pswd>@<ip>:<port>`, a proxy URL to use for creating HTTPS connections outside the cluster <br>**required** when `use_proxy` is true
| `no_proxy` |\<https-proxy\>| A comma-separated list of destination domain names, domains, IP addresses, or other network CIDRs to exclude proxying. Preface a domain with . to include all subdomains of that domain. Use * to bypass proxy for all destinations. <br>Such as: `'127.0.0.1,169.254.169.254,172.26.0.0/17,172.30.0.0/16,10.0.0.0/16,10.128.0.0/14,localhost,.api-int.,.example.com.'`

## Creation of the cluster

1. **Step1**:

```sh
ansible-playbook -i inventory.yaml 01-preparation.yaml
```

2. **Step2**:

**Note**: This step is optional.

> Skip this step if you want to use your external or existing DNS and Load Balancer, you can refer [Add-DNS-HAProxy](docs/add-dns-haproxy.md) to update it.

> If you use your external or existing DNS server, but no Load Balancer, you can refer [Add-DNS-HAProxy](docs/add-dns-haproxy.md) to update DNS server part, and use this playbook to configure HAProxy in your bastion server.
```sh
ansible-playbook -i inventory.yaml configure-haproxy.yaml
```

> If you don't have any existing DNS server or Load Balancer, please use this playbook to configure DNS server and HAProxy in your bastion server. 
```sh
ansible-playbook -i inventory.yaml bastion.yaml
```

3. **Step3**:
```sh
ansible-playbook -i inventory.yaml 02-create-cluster-control.yaml
```

4. **Step4**:
```sh
ansible-playbook -i inventory.yaml 03-create-cluster-compute.yaml
```

After above steps, you will get one ready OpenShift Container Platform on the IBM Cloud Infrastructure Center.

### Something is wrong?

#### Any above steps failed, clean up environment and then rerun the installation steps.

+ Failed on `01-preparation.yaml` ,`bastion.yaml` and `02-create-cluster-control.yaml`.
  
  - use: `ansible-playbook -i inventory.yaml 04-destroy.yaml`

+ Failed on `02-create-cluster-compute.yaml`.

  - use: `ansible-playbook -i inventory.yaml destroy-computes.yaml`

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

## Uninstall Red Hat OpenShift Container Platform

`ansible-playbook -i inventory.yaml 04-destroy.yaml`


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
