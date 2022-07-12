# Get all available versions and check current version
Get all available versions and check current version by:
```
cat z_infra_provisioning/cloud_infra_center/ocp_upi/version
```

# Checkout to specific version
Checkout to specific version by:
```
OCP_UPI_PLAYBOOK_VERSION=<version>; git log --oneline z_infra_provisioning/cloud_infra_center/ocp_upi/ | grep "Tag version ${OCP_UPI_PLAYBOOK_VERSION}" | awk '{print $1}' | xargs git checkout
```
Where `<version>` is the version that you want to checkout.
