# Configure Ansible `host_vars`

Ansible `host_vars` are a list of environment variables for a particular z/OS
managed host that Ansible collections will use once the SSH connection is
established.

## Description of the environment variables used in this configuration
* Environment variable `PYZ` is the Python home installation path on the
  z/OS managed node (target), e.g, pyz: "/usr/lpp/IBM/cyp/v3r8/pyz"
* Environment variable `ZOAU` is the ZOAU installation home on the z/OS managed
  node (target), e.g, zoau: "/usr/lpp/IBM/zoautil"
* Environment variable `ansible_python_interpreter` is the Python binary
  installation path on the z/OS managed node (target),
  e.g, ansible_python_interpreter: "{{pyz}}/bin/python3.8"

Configure `PYZ` and `ZOAU` in the included **group_vars** configuration.

For example:

```yaml
PYZ: "/u/oeusr01/python/pyz_3_8_2/usr/lpp/IBM/cyp/v3r8/pyz"
ZOAU: "/usr/lpp/IBM/zoautil"
ansible_python_interpreter: "{{ PYZ }}/bin/python3.8"
```

**Note**, `ansible_python_interpreter` will not need configuration because it is
configured to use variable expansion based on the `PYZ` environment variable.

If you are unfamiliar with playbook configuration practices and would like to
learn more, you can review our [detailed configuration guide](configuration_guide.md).
