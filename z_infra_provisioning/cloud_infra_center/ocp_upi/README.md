# IBM® Cloud Infrastructure Center

IBM® Cloud Infrastructure Center is an advanced infrastructure management product, providing on-premises cloud deployments of IBM z/VM®-based and KVM based Linux® virtual machines on the IBM Z® and IBM LinuxONE platforms.

Before you get started with Ansible, familiarize yourself with the basics of Red Hat® OpenShift® and IBM Cloud Infrastructure Center. The following links provide basic information and an overview. 

 [Red Hat OpenShift Container Platform installation and update](https://docs.openshift.com/container-platform/4.14/architecture/architecture-installation.html#architecture-installation)

[IBM Cloud Infrastructure Center](https://www.ibm.com/docs/en/cic/1.2.1)

# About this playbook

> The current version of this playbook: [version](./version.md)

> The instruction of [checkout to specific version](./checkout-to-version.md#Checkout-to-specific-version)

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

**Note**: This playbook supports IBM® Cloud Infrastructure Center version 1.2.0, 1.2.1 and RH OpenShift Container Platform version 4.12, 4.13 and 4.14 for z/VM and version 4.12, 4.13 and 4.14 for KVM.

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
- [Create Cluster](#creation-of-the-cluster)
  - [Something is Wrong](#something-is-wrongtrouble-shooting)

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
    - Ansible == 2.9
    - This server **must not** be any of the IBM Cloud Infrastructure Center nodes
    - You can use a single LPAR server or virtual machine
      - Disk with at least 20 GiB
- **(Optional)** A Bastion server, a machine that is used to configure DNS and Load Balancer for the Red Hat OpenShift installation.
    - If you have external or existing DNS server, but no Load Balancer for the Red Hat Openshift installation, please set `os_dns_domain` property, and then use a separate YAML `configure-haproxy` to configure the HAProxy in bastion server.
    - If you have existing Load Balancer, but no DNS server for the Red Hat Openshift installation, you can use a separate YAML `configure-dns` to configure the DNS server in bastion server.
    - If you don't have any existing DNS server or Load Balancer for the Red Hat Openshift installation, you need to create one Linux server as the bastion server and run playbook to configure DNS server and Load Balancer. You can also use the same Linux server that runs Ansible.
    - If you want to deploy multiple Red Hat Openshift, please do not use the same bastion server to configure multiple Load Balancer, otherwise you may encounter x509 error.
    - The firewalld service should be enabled and running in bastion server.


### 2. Installation of packages on a Linux server

**Packages:**

* Python3
* Ansible 
* jq
* wget
* git
* tar
* gzip
* firewalld
* Python modules required in the playbooks. Namely:
  * openstackclient 
  * openstacksdk 
  * netaddr

**Register:**

Use the following command to register the Linux server, then automatically associate any available subscription matching that system:
```sh
sudo subscription-manager register --username <username> --password <password> --auto-attach
```
After registration, use the following command to enable ansible repository, or use a newer version of your installed systems. 

**Note:** Our scenario is only tested for Ansible 2.9.20 on RHEL8.6. 
```sh
sudo subscription-manager repos --enable=ansible-2.9-for-rhel-8-s390x-rpms
yum install ansible-2.9.20
```

**Installation:**

Install the packages from the repository in the Linux server:
```sh
sudo dnf install python3 jq wget git firewalld tar gzip redhat-rpm-config gcc libffi-devel python3-devel openssl-devel cargo -y
```
Make sure that `python` points to Python3
```sh
sudo alternatives --set python /usr/bin/python3
```
Upgrade the pip package and dnf:
```sh
sudo -H pip3 install --upgrade pip
```

Then create the requirements file and use pip3 to install the python modules:

**Note**: 
1. The requirements.txt are tested for python 3.6.8.
2. You can use openstacksdk==0.46.0 and corresponding openstack.cloud collection version 1.10.0. Also you can use openstacksdk==1.0.1 and corresponding openstack.cloud collection version 2.0.0.

```sh
cat <<'EOF' >> requirements.txt 
# The order of packages is significant, because pip processes them in the order
# of appearance. Changing the order has an impact on the overall integration
# process, which may cause wedges in the gate later.
pbr
cliff==3.1.0
iso8601==0.1.12
openstacksdk==0.46.0
osc-lib==2.0.0
oslo.i18n==4.0.1
oslo.utils==3.42.1
python-keystoneclient==4.0.0
python-cinderclient==7.0.0
python-novaclient==17.0.0
stevedore==1.32.0
dogpile-cache
netaddr==0.7.19
python-openstackclient==5.2.2
cryptography==3.2.1
EOF

sudo pip3 install -r requirements.txt --ignore-installed
``` 
Install two collections from ansible galaxy:
With openstacksdk==0.46.0 installed:
```sh
ansible-galaxy collection install openstack.cloud:1.10.0
ansible-galaxy collection install ansible.posix:1.5.1
``` 
With openstacksdk==1.0.1 installed:
```sh
ansible-galaxy collection install openstack.cloud:2.0.0
ansible-galaxy collection install ansible.posix:1.5.1
```

**Verification:**
```sh
openstack
(openstack)
```

### 3. Setting the IBM Cloud Infrastructure Center environment variables on your Linux server

Check’ [setting environment variables](https://www.ibm.com/docs/en/cic/1.1.6?topic=descriptions-setting-environment-variables) for more details.

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
mkdir -p /opt/ibm/icic
scp user@host:/opt/ibm/icic/icicrc /opt/ibm/icic/
```

5. Copy the `icic.crt` file from the IBM Cloud Infrastructure Center management node to your certs directory `/etc/pki/tls/certs/`:
```
scp user@host:/etc/pki/tls/certs/icic.crt /etc/pki/tls/certs/
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

If you meet any **not running** service or **failed** message, check the IBM Cloud Infrastructure Center [Troubleshooting](https://www.ibm.com/docs/en/cic/1.1.6?topic=troubleshooting) document to fix before running the ansible playbook.

9. Set Subnet DNS

**Note**:
>1.Network type `DHCP` is mandatory for KVM, but when deploying OCP with RoCE networking, DHCP is not a requirement.
> 
>2.This step is required for KVM (not required for RoCE networking), z/VM is optional.

During deployment, the OpenShift nodes will need to be able to resolve public name records to download the OpenShift images and so on. They will also need to resolve the OpenStack API endpoint.

The default resolvers are often set up by the OpenStack administrator in Neutron. However, some deployments do not have default DNS servers set, meaning the servers are not able to resolve any records when they boot.

If you are in this situation, you can add resolvers to your Neutron subnet. These will be put into /etc/resolv.conf on your servers post-boot.

For example, if you want to use your own or external bastion server: `198.51.100.86`, you can run this command to add resolvers to your specified subnet, but be aware that if your subnet already has one DNS nameserver, please remove the old and reset bastion ip as primary DNS nameserver:
```sh
$ openstack subnet set --no-dns-nameservers <use_network_subnet>
$ openstack subnet set --dns-nameserver 198.51.100.86 <use_network_subnet>
```


10. Support to deploy multiple OCPs with multiple networks

Now we support the following scenarios for multiple ocps :
- Deploy multiple OCPs on ZVM multi-vlan network. You can refer ICIC doc to [create ZVM multiple vlan networks](https://www.ibm.com/docs/en/cic/1.2.1?topic=mfvc-zvm-multiple-vlan-networks-pass-through-different-zvm-vswitchesthe-first-time-configuration) then choose one network and set the network in the related inventory.yaml file section.

- Deploy multiple OCPs on ZVM multi-flat network. You can refer ICIC doc to [create ZVM multiple flat networks](https://www.ibm.com/docs/en/cic/1.2.1?topic=configuration-zvm-multiple-flat-networksthe-first-time) then choose one network and set the network in the related inventory.yaml file section.

- Deploy multiple OCPs on KVM multi-vlan network. You can refer ICIC doc to [create KVM multiple vlan networks](https://www.ibm.com/docs/en/cic/1.2.1?topic=mfvc-kvm-multiple-vlan-networks-that-pass-through-different-osasthe-first-time-configuration) then choose one network and set the network in the related inventory.yaml file section.

- Deploy multiple OCPs on KVM multi-flat network. You can refer ICIC doc to [create KVM multiple flat networks](https://www.ibm.com/docs/en/cic/1.2.1?topic=configuration-kvm-multiple-flat-networksthe-first-time) then choose one network and set the network in the related inventory.yaml file section.

- Deploy multiple OCPs on KVM flat+vlan network. You can refer ICIC doc to [create KVM flat+vlan networks](https://www.ibm.com/docs/en/cic/1.2.1?topic=mfvc-kvm-multiple-flat-vlan-networks-pass-through-different-osasconfigured-already) then choose one network and set the network in the related inventory.yaml file section.

- Deploy multiple OCPs on ZVM in one project use single network by custom DNS

  you can set different custom DNS IP values in the related inventory.yaml file then to deploy multiple OCPs.


### 4. Download this playbook on your Linux server

All the configuration files, logs and installation state are kept in a single directory:
```sh
git clone https://github.com/IBM/z_ansible_collections_samples.git
cp -r z_ansible_collections_samples/z_infra_provisioning/cloud_infra_center/ocp_upi ocp_upi
cd ocp_upi
```

### 5. Configure your settings in inventory.yaml

Update your settings based on the samples. The following propeties are **required**:

| Property                  | Default                                         | Description                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           | 
|---------------------------|-------------------------------------------------|-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| `use_network_name`        | \<network name from icic\>                      | `openstack network list -c Name -f value`                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             |
| `use_network_subnet`      | \<subnet id from network name in icic\>         | `openstack network list -c Subnets -f value`                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          |
| `vm_type`                 | kvm                                             | The operation system of OpenShift Container Platform, <br>supported: `kvm` or `zvm`                                                                                                                                                                                                                                                                                                                                                                                                                                                                   | |
| `disk_type`               | dasd                                            | The disk storage of OpenShift Container Platform, <br>supported: `dasd` or `scsi`                                                                                                                                                                                                                                                                                                                                                                                                                                                                     | |
| `openshift_version`       | 4.12                                            | The product version of OpenShift Container Platform, <br>such as `4.12`,`4.13` or `4.14`. <br> And the rhcos is not updated for every single minor version. User can get available openshift_version from [here](https://mirror.openshift.com/pub/openshift-v4/s390x/dependencies/rhcos/)                                                                                                                                                                                                                                                             | |
| `openshift_minor_version` | 3                                               | The minor version of Openshift Container Platform, <br>such as `3`.Support to use `latest` tag to install the latest minor version under`openshift_version` <br> And User can inspect what minor releases are available by checking [here](https://mirror.openshift.com/pub/openshift-v4/s390x/clients/ocp/) to see whats there                                                                                                                                                                                                                       | 
| `auto_allocated_ip`       | true                                            | (Boolean) true or false, if false, <br>IPs will be allocated from `allocation_pool_start` and `allocation_pool_end`                                                                                                                                                                                                                                                                                                                                                                                                                                   |
| `os_flavor_bootstrap`     | medium                                          | `openstack flavor list`, Minimum flavor disk size >= 35 GiB                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           | |
| `os_flavor_master`        | medium                                          | `openstack flavor list`, Minimum flavor disk size >= 35 GiB                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           | |
| `os_flavor_worker`        | medium                                          | `openstack flavor list`, Minimum flavor disk size >= 35 GiB                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           | |
| `os_control_nodes_number` | 3                                               | (Integer) Number of Red Hat Openshift provisioned control server nodes                                                                                                                                                                                                                                                                                                                                                                                                                                                                                | |
| `os_compute_nodes_number` | 3                                               | (Integer) Number of Red Hat Openshift provisioned compute server nodes                                                                                                                                                                                                                                                                                                                                                                                                                                                                                | |
| `create_server_zone`      | ''                                              | The zone you can select which host instances are launched on and which roles can boot instances on this host, the value format is `ZONE:HOST:NODE`, HOST and NODE are optional parameters, in such cases, use the `ZONE::NODE`, `ZONE:HOST` or `ZONE`. <br>With openstacksdk==0.46.0 installed, the default value is '', which means to use the default availability zone. With openstacksdk==1.0.1 installed, the value must be set.<br>[ **ZONE** is `Zone Name` column from `openstack availability zone list`; **HOST** is `Host Name` column from `openstack host list`; **NODE** is `Hypervisor Hostname` column from `openstack hypervisor list`] |
| `pullsecret`              | \<pull-secret\>                                 | Get from [cloud.redhat.com](https://console.redhat.com/openshift/install/ibmz/user-provisioned)                                                                                                                                                                                                                                                                                                                                                                                                                                                       |
| `sshkey`                  | \<ssh-key\>                                     | The SSH public key for the core user in RHEL CoreOS                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   |
| `os_dns_domain`           | \<external DNS ip addr\> or \<bastion ip addr\> | If you want to use your external or existing DNS server set `os_dns_domain` to use it, others set bastion machine ip address                                                                                                                                                                                                                                                                                                                                                                                                                          |
| `cluster_name`            | \<cluster-name\>                                | The name of the cluster, such as `openshift`.                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                         |
| `base_domain`             | \<cluster-base-domain\>                         | The base domain of the cluster, the base domain is used to create routes to your OpenShift Container Platform cluster components, such as `example.com`      <br/>                                                                                                                                                                                                                                                                                                                                                                                    | 
| `bootstrap_name_prefix`            | \<bootsrap-node-name\>                                | The name and hostname of the bootstrap node, such as `ocp4zkboot`. If specific name is not required, leave the default value `` here.                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                         |
| `master_name_prefix`            | \<master-node-name\>                                | The name and hostname of the master nodes, such as `ocp4zkm`. If specific name is not required, leave the default value `` here.                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                         |
| `worker_name_prefix`            | \<worker-node-name\>                                | The name and hostname of the worker nodes, such as `ocp4zkw`. If specific name is not required, leave the default value `` here.                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                         |
| `use_internal_bastion`    | true                                            | (Boolean) true or false, if true then nodes information under /var/named/\<clustername>\.zone and /etc/haproxy/haproxy.cfg will update and remove automatically                                                                                                                                                                                                                                                                                                                                                                                       | 

If you need the Ansible playbook to help configure DNS server or HAProxy server on bastion server, you need to configure correct bastion properties:

| Property                     | Default                  | Description                                                                                                     |
|------------------------------|--------------------------|:----------------------------------------------------------------------------------------------------------------|
| `ansible_ssh_host`           | \<linux server ip addr\> | 'x.x.x.x'<br> **required** when use bastion server, give the IP address of bastion server.                      |
| `bastion_private_ip_address` | \<bastion ip addr\>      | IP address of your bastion node<br>**required** when use bastion server, give the IP address of bastion server. |
| `dns_forwarder`              | \<upstream DNS ip addr\> | For nameserver where requests should be forwarded for resolution.<br>**required** when use bastion server       |

Others are **optional**, you can enable them and update value if you need more specified settings.

| Property                  | <div style="width:220px">Default</div>                  | Description                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      |
|---------------------------|---------------------------------------------------------|:---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| `allocation_pool_start`   | \<ip range start\>                                      | 'x.x.x.x'                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        |
| `allocation_pool_end`     | \<ip range end\>                                        | 'x.x.x.x'                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        |
| `os_bootstrap_ip`         | \<bootstrap ip addr\>                                   | 'x.x.x.x, <br>**required** when `auto_allocated_ip` is false                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     |
| `os_master_ip`            | \<master ip list\>                                      | '[x.x.x.x, x.x.x.x, x.x.x.x], <br>**required** when `auto_allocated_ip` is false                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 |
| `os_infra_ip`             | \<infra ip list\>                                       | '[x.x.x.x, x.x.x.x, x.x.x.x], <br>**required** when `auto_allocated_ip` is false                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 |
| `volume_type_id`          | \<storage template id\>                                 | The bootable volume type from backend storage provider. Get it from `openstack volume type list -c ID -f value`                                                                                                                                                                                                                                                                                                                                                                                                                                                                  |
| `use_proxy`               | false                                                   | (Boolean) true or false, if true then Openshft Container Platform will use the proxy setting, get detail from [doc.openshift.com](https://docs.openshift.com/container-platform/4.9/installing/installing_bare_metal/installing-bare-metal.html#installation-configure-proxy_installing-bare-metal)                                                                                                                                                                                                                                                                              |
| `http_proxy`              | \<http-proxy\>                                          | `http://<username>:<pswd>@<ip>:<port>`, a proxy URL to use for creating HTTP connections outside the cluster. <br>**required** when `use_proxy` is true                                                                                                                                                                                                                                                                                                                                                                                                                          |
| `https_proxy`             | \<https-proxy\>                                         | `http://<username>:<pswd>@<ip>:<port>`, a proxy URL to use for creating HTTPS connections outside the cluster <br>**required** when `use_proxy` is true                                                                                                                                                                                                                                                                                                                                                                                                                          |
| `no_proxy`                | \<https-proxy\>                                         | A comma-separated list of destination domain names, domains, IP addresses, or other network CIDRs to exclude proxying. Preface a domain with . to include all subdomains of that domain. Use * to bypass proxy for all destinations. <br>Such as: `'127.0.0.1,169.254.169.254,172.26.0.0/17,172.30.0.0/16,10.0.0.0/16,10.128.0.0/14,localhost,.api-int.,.example.com.'`                                                                                                                                                                                                          |
| `use_localreg`            | false                                                   | (Boolean) true or false, if true then Openshift Container Platform will use local packages to download                                                                                                                                                                                                                                                                                                                                                                                                                                                                           |
| `localreg_mirror`         | \<local-mirror-registry\>                               | The name of local mirror registry to use for mirroring the required container images of OpenShift Container Platform for disconnected installations. Following [guide](https://docs.openshift.com/container-platform/4.12/installing/disconnected_install/installing-mirroring-installation-images.html) to setup mirror registry, and we offer temporary script to setup registry and mirror images, you can get scripts from [mirror-registry](tools/mirror-registry/), please update the correct `PULL_SECRET` and `VERSION` in `01-mirror-registry.sh` script before use it. |
| `local_openshift_install` | \<local-openshift-install-url\>                         | This is always the latest installer download [link](https://mirror.openshift.com/pub/openshift-v4/clients/ocp/latest/openshift-install-linux.tar.gz), use an SSH or HTTP client to store the Openshift installation package, and put the link here                                                                                                                                                                                                                                                                                                                               |
| `local_openshift_client`  | \<local-openshift-client-url\>                          | This is always the latest client download [link](https://mirror.openshift.com/pub/openshift-v4/clients/ocp/latest/openshift-client-linux.tar.gz), use an SSH or HTTP client to store the Openshift client package, and put the link here                                                                                                                                                                                                                                                                                                                                         |
| `local_rhcos_image`       | \<local-rhcos-image-url\>                               | This is all rhcos images download [link](https://mirror.openshift.com/pub/openshift-v4/s390x/dependencies/rhcos/latest/), download the name that corresponds with KVM or z/VM images, and use an SSH or HTTP client to store it, put the link here                                                                                                                                                                                                                                                                                                                               |
| `additional_certs`        | `{{ lookup('file', '/opt/registry/certs/domain.crt')}}` | The local mirror registry repo additionally need SSL certificated to be accessed, those can be added cert file via the `additional_certs` variable.                                                                                                                                                                                                                                                                                                                                                                                                                              |                                                                                                                                       |    |                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                         |                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              | indent (width=2) }}`                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             | The local mirror registry repo additionally need SSL certificated to be accessed, those can be added cert file via the `additional_certs` variable.
| `create_server_timeout`   | 10                                                      | Default is 10 minutes that used to create instances and volumes from backend storage provider                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    |


**Note**: Check [Disconnected installation](./Disconnected_Installation.md) for setting up a disconnected installation of the OpenShift Container Platform (OCP).

**Note**: Check [OpenShift on a single node](./SNO.md) for how to define single node cluster.

## Creation of the cluster

1. **Step1**:

```sh
ansible-playbook -i inventory.yaml 01-preparation.yaml
```

**Please notice:**: 
>1. We will default treat bastion server and localhost server as a same one.
>2. If you want to use your external or existing DNS and Load Balancer, please change `use_internal_bastion: true` to `use_internal_bastion: false` in inventory.yaml. Also refer [Add-DNS-HAProxy](docs/add-dns-haproxy.md) to update DNS and Load Balancer records manually.

2. **Step2**:

```sh
ansible-playbook -i inventory.yaml 02-create-cluster-control.yaml
```

3. **Step3**:

```sh
ansible-playbook -i inventory.yaml 03-create-cluster-compute.yaml
```

After above steps, you will get one ready OpenShift Container Platform on the IBM Cloud Infrastructure Center.

### Something is wrong/trouble shooting?

#### Any above steps failed, clean up environment and then rerun the installation steps.

+ Failed on `01-preparation.yaml` and `02-create-cluster-control.yaml`.
  
  - use: `ansible-playbook -i inventory.yaml 04-destroy.yaml`

+ Failed on `03-create-cluster-compute.yaml`.

  - use: `ansible-playbook -i inventory.yaml destroy-computes.yaml`

## Day2 Operation

### Add compute node
Use this playbook to add a new compute node as allocated IP:
```sh
ansible-playbook -i inventory.yaml add-new-compute-node.yaml 
```
Use this playbook to add a new compute node as fixed IP:
```sh
ansible-playbook -i inventory.yaml add-new-compute-node.yaml -e ip=x.x.x.x
```
Use this playbook to add multiple compute nodes as allocated IP:
```sh
ansible-playbook -i inventory.yaml add-new-compute-node.yaml -e worker_number=3 
```
Use this playbook to add multiple compute nodes as fixed IP, separate the IP list with commas:
```sh
ansible-playbook -i inventory.yaml add-new-compute-node.yaml -e ip=x.x.x.x,x.x.x.x -e worker_number=2
```


## Uninstall Red Hat OpenShift Container Platform
`ansible-playbook -i inventory.yaml 04-destroy.yaml`

## Remove RHCOS images
In order to save image space, our playbook will not delete the uploaded image automatically, user can use this individual playbook to remove it.
`ansible-playbook -i inventory.yaml destroy-images.yaml`

And we store the SHA256 value into image properties to verify downloading images, the SHA256 comes from the `gz` packages.
```
| owner_specified.openstack.object | images/rhcos                                                                     |
| owner_specified.openstack.sha256 | fc265b2d5b6c9f6d175e8b15f672aba78f6e4707875f9accaa2cb74e3d90d27b
```

## Copyright
© Copyright IBM Corporation 2023

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
