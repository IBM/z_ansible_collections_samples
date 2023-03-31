# Get all available versions and check current version
Get all available versions and check current version commit id by:
```
cat z_infra_provisioning/cloud_infra_center/ocp_upi/version.md |grep "current version" | awk '{print $4}' 
```

# Checkout to specific version
Checkout to specific version by:
```
git checkout <version_commit_id>
```
Where `<version_commit_id>` is the commit id of the version that you want to checkout.
