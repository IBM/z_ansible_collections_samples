For development and test purpose, you can define a [openshift on a single node, SNO](https://docs.openshift.com/container-platform/4.13/installing/installing_sno/install-sno-installing-sno.html) cluster with following steps:

update inventory.yaml from default 3 control nodes and 3 compute nodes to 1 control and compute node. This apply to both z/VM and KVM.
```
os_control_nodes_number: 1
os_compute_nodes_number: 0
```

Then follow the other steps of Openshift deployment, finally you will get a cluster like following:

```
# oc get node
NAME                         STATUS   ROLES                         AGE   VERSION
openshiftji-dm5dv-master-0   Ready    control-plane,master,worker   48m   v1.25.4+a34b9e9

# oc get co
NAME                                       VERSION   AVAILABLE   PROGRESSING   DEGRADED   SINCE   MESSAGE
authentication                             4.12.3    True        False         False      24m
baremetal                                  4.12.3    True        False         False      45m
cloud-controller-manager                   4.12.3    True        False         False      47m
cloud-credential                           4.12.3    True        False         False      48m
cluster-autoscaler                         4.12.3    True        False         False      44m
config-operator                            4.12.3    True        False         False      45m
console                                    4.12.3    True        False         False      19m
control-plane-machine-set                  4.12.3    True        False         False      45m
csi-snapshot-controller                    4.12.3    True        False         False      25m
dns                                        4.12.3    True        False         False      42m
etcd                                       4.12.3    True        False         False      37m
image-registry                             4.12.3    True        False         False      28m
ingress                                    4.12.3    True        False         False      45m
insights                                   4.12.3    True        False         False      37m
kube-apiserver                             4.12.3    True        False         False      37m
kube-controller-manager                    4.12.3    True        False         False      41m
kube-scheduler                             4.12.3    True        False         False      41m
kube-storage-version-migrator              4.12.3    True        False         False      45m
machine-api                                4.12.3    True        False         False      45m
machine-approver                           4.12.3    True        False         False      45m
machine-config                             4.12.3    True        False         False      43m
marketplace                                4.12.3    True        False         False      45m
monitoring                                 4.12.3    True        False         False      23m
network                                    4.12.3    True        False         False      46m
node-tuning                                4.12.3    True        False         False      45m
openshift-apiserver                        4.12.3    True        False         False      37m
openshift-controller-manager               4.12.3    True        False         False      27m
openshift-samples                          4.12.3    True        False         False      23m
operator-lifecycle-manager                 4.12.3    True        False         False      45m
operator-lifecycle-manager-catalog         4.12.3    True        False         False      45m
operator-lifecycle-manager-packageserver   4.12.3    True        False         False      37m
service-ca                                 4.12.3    True        False         False      45m
storage                                    4.12.3    True        False         False      45m
```
