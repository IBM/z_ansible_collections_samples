
# Configuration

Although each sample includes default configurations, it also provides
a playbook [host-setup](https://github.com/ansible-collections/ibm_zos_core/blob/dev/playbooks/host-setup.yaml)
that attempts to locate Ansible dependencies on a z/OS system and
use them to generate a valid **inventory** and **host_vars** configuration.
While the playbook executes, it prints out all the dependencies it finds, 
should you be interested in overriding the generated configurations with other
dependencies.

```

Ensure that you configure your playbook with the necessary `environment: "{{ environment_vars }}"` to
allow the variables to be available to the playbooks runtime. A complete example configuration is presented in the
[Variables section](https://github.com/IBM/z_ansible_collections_samples/blob/master/docs/share/configuration_guide.md#variables).
```

## Ansible Config

The Ansible configuration file **ansible.cfg** can override almost all
`ansible-playbook` configurations. The configuration file includes a sample **ansible.cfg**
that can supplement `ansible-playbook` with a little modification. The only required
configuration is present in **ansible.cfg** `pipelining = True`.
Setting the `pipelining = True` overrides the default behavior which is to
transfer Ansible modules to the target in binary via SFTP.

Setting `pipelining = True` will prevent this error:
```
SyntaxError: Non-UTF-8 code starting with \'x83\' in file
/a/user1/.ansible/tmp/ansible-tmp-1548232945.35-274513842609025/AnsiballZ_stat.py
on line 1, but no encoding declared;
see <https://python.org/dev/peps/pep-0263/> for details
```

You can specify the **SSH** port used by Ansible and instruct Ansible where
to write the **temporary** files on the target. This can be easily done by
adding the options to your **inventory** or to **ansible.cfg**

For example, to set the port and temporary writes, use these options:
``` {.yaml}
[defaults]
forks = 25
remote_tmp = /u/ansible/tmp
remote_port = 2022
```

For more information about available configurations for `ansible.cfg`,
read the Ansible documentation on
[Ansible configuration settings](https://docs.ansible.com/ansible/latest/reference_appendices/config.html#ansible-configuration-settings-locations).

## Inventory

Ansible works with multiple managed nodes (hosts) at the same time,
using a list or group of lists known as an
[inventory](https://docs.ansible.com/ansible/latest/user_guide/intro_inventory.html).
Once the inventory is defined, you can use
[patterns](https://docs.ansible.com/ansible/latest/user_guide/intro_patterns.html#intro-patterns)
to select the hosts or groups that you want Ansible to run on.

The sample includes **inventory** that can be used to manage your nodes
with a little modification. This inventory file should be included when running
the sample playbook.

``` {.yaml}
zsystem:
  hosts:
    zvm:
      ansible_host: zos_target_address
      ansible_user: zos_target_username
      ansible_python_interpreter: path_to_python_interpreter_binary_on_zos_target
```

The value for the property **ansible_host** is the hostname of the
managed node; for example, `ansible_host: ec33017A.vmec.svl.ibm.com`

The value for the property **zos_target_username** is the user name to
use when connecting to the host; for example, `ansible_user: omvsadm`.

The value for the property **ansible_python_interpreter** is the target
host Python path. This is useful for systems with more than one Python
installation, or when Python is not installed in the default location
**/usr/bin/python**; for example,
`ansible_python_interpreter: /usr/lpp/IBM/cyp/v3r8/pyz/bin/python3`

Behavioral inventory parameters such as `ansible_port` allow you
to set the port for a host can be viewed in the behavioral inventory parameters
[documentation](https://docs.ansible.com/ansible/latest/user_guide/intro_inventory.html#connecting-to-hosts-behavioral-inventory-parameters).

## Variables

Although you can store variables in the **inventory** file, storing them in
separate configurations such as **host_vars** or **group_vars** files help
you organize your variable values. 

The sample includes a **host_vars** file that can be easily customized;
the relevant variables are explained below:

The value for the property **\_BPXK_AUTOCVT** must be configured to
`ON`, for example; `_BPXK_AUTOCVT: "ON"`.

The value for the property **ZOAU_HOME** is the ZOA Utilities install
root path; for example, `ZOAU_HOME: "/usr/lpp/IBM/zoautil"`.  
**Note**, if you are using ZOAU version 1.0.1 or older, you must use property **ZOAU_ROOT**
instead of **ZOAU_HOME**.

The value for the property **PYTHONPATH** is the ZOA Utilities Python
library path; for example, `PYTHONPATH: "/usr/lpp/IBM/zoautil/lib/"`.

The value for the property **LIBPATH** is both the path to the
**Python libraries** on the target and the **ZOA Utilities Python library**
path separated by colons `:`; for example,
`LIBPATH: "/usr/lpp/IBM/zoautil/lib/:/usr/lpp/IBM/cyp/v3r8/pyz/lib:/lib:/usr/lib:."`.

The value for the property **PATH** is the **ZOA utilities BIN path**
and the **Python BIN path**; for example,
`PATH: "/usr/lpp/IBM/zoautil/bin:/usr/lpp/IBM/cyp/v3r8/pyz/bin:/bin"`.

The value for the property **\_CEE_RUNOPTS** is the invocation Language
Environment® runtime options for programs and used by Python; for
example; `_CEE_RUNOPTS: "FILETAG(AUTOCVT,AUTOTAG) POSIX(ON)"`.

The value for properties **\_\_TAG_REDIR_ERR**, **\_TAG_REDIR_IN**,
**\_TAG_REDIR_OUT** are `txt` and used by the shell; for example,

``` {.sh}
_TAG_REDIR_ERR: "txt"
_TAG_REDIR_IN: "txt"
_TAG_REDIR_OUT: "txt"
```

The value for the property **LANG** is the name of the default locale;
the value **C** specifies the POSIX locale. For example, `LANG: "C"`.

Using top level dependency variables that rely on variable expansion to
substitute values is preferred, because it tends to reduce
misconfiguration when copying dependency paths. In this example, the top
level dependency variables `PYZ` for Python and `ZOAU` have been added
and used through the configuration.

A complete configuration is depicted below:

``` {.yaml}
PYZ: "/usr/lpp/IBM/cyp/v3r8/pyz"
ZOAU: "/usr/lpp/IBM/zoautil"

environment_vars:
  _BPXK_AUTOCVT: "ON"
  ZOAU_HOME: "{{ ZOAU }}"
  PYTHONPATH: "{{ ZOAU }}/lib"
  LIBPATH: "{{ ZOAU }}/lib:{{ PYZ }}/lib:/lib:/usr/lib:."
  PATH: "{{ ZOAU }}/bin:{{ PYZ }}/bin:/bin:/var/bin:/usr/sbin"
  _CEE_RUNOPTS: "FILETAG(AUTOCVT,AUTOTAG) POSIX(ON)"
  _TAG_REDIR_ERR: "txt"
  _TAG_REDIR_IN: "txt"
  _TAG_REDIR_OUT: "txt"
  LANG: "C"
```

### Variables for Rocket Python

Currently, IBM Open Enterprise Python for z/OS is the supported and
recommended Python distribution for use on z/OS with Ansible and ZOAU.
If Rocket Python is the only available Python library on the target, please
review the suggested environment variables below for use with Rocket
Python.

``` {.yaml}
########################################
# Rocket suggested environment variables
########################################
PYZ: "/usr/lpp/rsusr/python36"
ZOAU: "/usr/lpp/IBM/zoautil"

environment_vars:
  ZOAU_ROOT: "{{ ZOAU }}"
  ZOAU_HOME: "{{ ZOAU }}"
  PYTHONPATH: "{{ ZOAU }}/lib:{{ PYZ }}:/lib:/usr/lib"
  _BPXK_AUTOCVT: "ON"
  PATH: "{{ ZOAU }}/bin:/bin:/var/bin:{{ PYZ }}/bin"
  LIBPATH: "{{ ZOAU }}/lib:{{ PYZ }}/lib:/lib:/usr/lib:."
```

### Variables for Bash shell

Currently, only ``z/OS® shell`` is supported. Using ``ansible_shell_executable``
to change the default shell is discouraged. For more information,
see [Ansible documentation](https://docs.ansible.com/ansible/2.7/user_guide/intro_inventory.html).

Shells such as ``bash`` are **not supported** because they handle the reading
and writing of untagged files differently. ``bash`` added enhanced ASCII support
in version 4.3 and thus differs from 4.2. If ``bash`` shell is the only shell
available, you must control how the new and existing files are tagged and
encoded. This can be controlled by setting both, "_ENCODE_FILE_NEW" and
"_ENCODE_FILE_EXISTING".

For example,
``` {.sh}
_ENCODE_FILE_NEW: "IBM-1047"
_ENCODE_FILE_EXISTING: "IBM-1047"
```

Please review the README.ZOS guide included with the ported ``bash`` shell for
further configurations.

## Run the playbook

Access the sample Ansible playbook and ensure that you are within the
playbook directory where the sample files are included

It is a good practice to review the playbook samples before executing
them. It will help you understand the requirements in terms of space,
location, names, authority, and the artifacts will be created and cleaned
up. Although samples are always written to operate without the need for
the user's configuration, flexibility is written into the samples
because it is not easy to determine if a sample has access to the
host's resources. Review the playbook notes sections for additional
details and configuration.

### Ansible command
Use the Ansible command `ansible-playbook` to run the sample playbook.
The command syntax is `ansible-playbook -i <inventory> <playbook>`; for
example, `ansible-playbook -i inventory sample.yaml`.

This command assumes that the controller's public SSH key has been
shared with the managed node. If you want to avoid entering a username
and password each time, copy the SSH public key to the managed node
using the `ssh-copy-id` command; for example,
`ssh-copy-id -i ~/.ssh/mykey.pub user@<hostname>`.

### Password prompt
Alternatively, you can use the `--ask-pass` option to be prompted for
the user\'s password each time a playbook is run; for example,
`ansible-playbook -i inventory zos-collection-sample.yaml --ask-pass`.

Using `--ask-pass` is not recommended because it will hinder
performance. Using `--ask-pass` requires `sshpass` be installed on
the controller. For further reference, see the
[ask-pass documentation](https://linux.die.net/man/1/sshpass).

## Debugging

Optionally, you can configure the console logging verbosity during
playbook execution. This is helpful in situations where communication is
failing and you want to obtain more details. To adjust the logging
verbosity, append more letter `v`'s; for example,
`-v`, `-vv`, `-vvv`, or `-vvvv`. Each letter `v` increases logging
verbosity similar to traditional logging levels INFO, WARN, ERROR, DEBUG.


