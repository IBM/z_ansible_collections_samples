This guide provides step-by-step instructions for setting up a disconnected installation of the OpenShift Container Platform (OCP) on IBM Cloud Infrastructure Center. Follow these instructions to prepare your environment, set up the necessary infrastructure, and configure OCP for a successful installation.

### 1. Prepare Two Machines

- **(Ansible Host)** : This machine is optional but can be used for running Ansible Playbook.

- **(Bastion Host)** : This machine is required and must have internal network connectivity to communicate with your OCP nodes.

Check [Preparation of the servers](./README.md#1-preparation-of-the-servers) for how to prepare them.

### 2. Pre-requisite Packages

First, download the required packages from an online server, and copy those packages to the Ansible Host, then install packages on Ansible Host.

Check [Installation of packages on a Linux server](./README.md#2-installation-of-packages-on-a-linux-server).

### 3. Set Up Mirror Registry

To set up a mirror registry, use for mirroring the required container images of OpenShift Container Platform for disconnected installations. Following [guide](https://docs.openshift.com/container-platform/4.12/installing/disconnected_install/installing-mirroring-installation-images.html) to setup mirror registry.

And we offer temporary script to setup registry and mirror images, you can get scripts from [mirror-registry](tools/mirror-registry/)

Update the following variables in the scripts as needed:
- **01-mirror-registry.sh**:

    - PULL_SECRET: (required)The secret to download openshift images
    - VERSION: (required)The openshift version, such as: `4.8.14`
    - CLIENT_ARCH: (required)The architecture of running script machine, such as: `x86_64` or `s390x`
    - IMAGE_ARCH: (required)The architecture for openshift, such as: `s390x`
- **00-setup-registry.sh**:
    - LOCAL_REGISTRY_USERNAME: Credentials for your registry, such as: `icic`
    - LOCAL_REGISTRY_PASSWORD: Credentials for your registry, such as: `icic`
    - LOCAL_REGISTRY_HOSTNAME: Repository name for storing the mirrored images, such as: `image.registry.icic.ocp.com`
    - LOCAL_REGISTRY_PORT: Repository port for storing the mirrored images, such as: `5008`

**Note**: Copy the mirror registry `/opt/registry/certs/domain.crt` into Ansible Host:/opt/registry/certs/

### 4. Add Mirror Registry Domain

After running `bastion.yaml`, we will remove the existing domain record in the named configuration. Please remember to add it again after running `bastion.yaml` to avoid any missing data. Follow these steps to re-add the domain `image.registry.icic.ocp.com`:

1. Edit the named.conf:
Add the zone information for `image.registry.icic.ocp.com` in named.conf file on Bastion Host.
```
vi /etc/named.conf

zone "image.registry.icic.ocp.com" {
        type master;
        file "image.registry.icic.ocp.com.zone";
        allow-query { any; };
        allow-transfer { none; };
        allow-update { none; };
}
```

2. Create the Zone File:
Create the zone file /var/named/image.registry.icic.ocp.com.zone with the DNS records.
```
$TTL 1D
@       IN SOA  @ rname.invalid. (
                                        0       ; serial
                                        1D      ; refresh
                                        1H      ; retry
                                        1W      ; expire
                                        3H )    ; minimum
        NS      @
        A       <Mirror_Registry_Server_IP_Address>
```

3. Restart named-chroot:
Restart the DNS service to apply the changes.
```
systemctl restart named-chroot
```

4. Verify the DNS Configuration:
Use nslookup to ensure the domain is correctly configured and resolvable.
```
nslookup image.registry.icic.ocp.com
```

5. Test Mirror Registry Access:
Verify that the mirror registry is accessible from the bastion host using curl.
```
curl -u icic:icic --insecure https://image.registry.icic.ocp.com:5008/v2/_catalog
```

### 5. Prepare Openshift Client and RHCOS images

Update the inventory.yaml file with the URLs or file paths for the OpenShift client and RHCOS images. 
Example entry:
```
      use_localreg: true # true or false
      localreg_mirror: "image.registry.icic.ocp.com:5008/icic/openshift4"

      local_openshift_install: '/ocp_upi/openshift-install-linux.tar.gz'
      local_openshift_client: '/ocp_upi/openshift-client-linux.tar.gz'
      local_rhcos_image: '/ocp_upi/rhcos-4.8.14-s390x-openstack.s390x.qcow2.gz'

      additional_certs: "{{ lookup('file', '/opt/registry/certs/domain.crt') | indent (width=2) }}"
```

### 6. Openshift Deployment
Then follow the other steps of Openshift deployment, finally you will get a cluster with your mirror registry:

```
[root@XXX ocp_upi]# ./oc get imagecontentsourcepolicy -o yaml
apiVersion: v1
items:
- apiVersion: operator.openshift.io/v1alpha1
  kind: ImageContentSourcePolicy
  metadata:
    creationTimestamp: "2024-07-11T11:02:21Z"
    generation: 1
    name: image-policy-0
    resourceVersion: "931"
    uid: a005cc44-edeb-4bda-a6f2-1f57a69667e9
  spec:
    repositoryDigestMirrors:
    - mirrors:
      - image.registry.icic.ocp.com:5008/icic/openshift4
      source: quay.io/openshift-release-dev/ocp-release
- apiVersion: operator.openshift.io/v1alpha1
  kind: ImageContentSourcePolicy
  metadata:
    creationTimestamp: "2024-07-11T11:02:22Z"
    generation: 1
    name: image-policy-1
    resourceVersion: "964"
    uid: e1eb4641-8ffa-4bcb-9454-634141613dfb
  spec:
    repositoryDigestMirrors:
    - mirrors:
      - image.registry.icic.ocp.com:5008/icic/openshift4
      source: quay.io/openshift-release-dev/ocp-v4.0-art-dev
kind: List
metadata:
  resourceVersion: ""
  selfLink: ""
```
