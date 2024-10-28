
# Configuration Guide
This document outlines the various Ansible configurations used in the playbook
projects.

# Configuration
Although each playbook includes default configurations, they do require some
additional configuration. These requirements are all identified in the playbook
projects README, where this documents aims to provide a more comprehensive explanation
of how all the various configurations enable a Ansible playbooks to manage a z/OS
target.

## Ansible Configuration
The Ansible configuration file **ansible.cfg** can override almost all
`ansible-playbook` configurations. The playbook projects include an **ansible.cfg**
that aid in instructing Ansible how to operate. For example, setting
`pipelining = True` overrides the default behavior which is to
transfer Ansible modules to the target in binary via SFTP and reduces the number
of network operations required to execute a module on the remote server, by
executing many Ansible modules without actual file transfer. Also, in early
Python releases it would prevent this below error.

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

These playbook projects include **inventory** that can be used to manage your
nodes with a little modification. This inventory file should be included when
running the playbook.

``` {.yaml}
zsystem:
  hosts:
    zvm:
      ansible_host: zos_target_address
      ansible_user: zos_target_username
```

The value for the property **ansible_host** is the hostname of the
managed node; for example, `ansible_host: zvm1.vmec.svl.ibm.com`

The value for the property **ansible_host** is the z/OS managed user to
connect and run as over SSH; for example, `ansible_user: zosadm`.

Behavioral inventory parameters such as `ansible_port` allow you
to set the port for a host and can be viewed in the behavioral inventory parameters
[documentation](https://docs.ansible.com/ansible/latest/user_guide/intro_inventory.html#connecting-to-hosts-behavioral-inventory-parameters).


# Environment Variables 
```
Ensure that you configure your playbook with the necessary `environment: "{{ environment_vars }}"` to
allow the variables to be available to the playbooks runtime. A complete example configuration is presented in the
[Variables section](https://github.com/IBM/z_ansible_collections_samples/blob/master/docs/share/configuration_guide.md#variables).
```

## Variables

Although you can store variables in the **inventory** file, storing them in
separate configurations such as **host_vars** or **group_vars** files help
you organize your variable values. 

These environment variables are sourced by the playbook thus it is important
that the playbook be configured to find them, often this is overlooked. Thus,
ensure that your playbook is configured with `environment: "{{ environment_vars }}"`
to allow the variables to be available to the playbooks runtime. For example,

```yaml
---
- hosts: zos_host
  collections:
    - ibm.ibm_zos_core
  environment: "{{ environment_vars }}"
```

## Group Variables

Group variables are environment variables referred and found in a `group_vars/`
directory under the inventories directory. They don't necessarily have to be
located under `inventories/`, but it makes for a more universal experience when
using tools such as [ansible](https://docs.ansible.com/ansible/latest/cli/ansible.html),
[ansible-console](https://docs.ansible.com/ansible/latest/cli/ansible-console.html)
will only look for `group_vars/` and `host_vars/` in the inventory directory.

`group_vars/` provides organization such that they are variables that are going
to be consistent for all the managed nodes. The playbook projects include a
`all.yml` that is located in the `group_vars/` directory. The environment variables
in `all.yml` usually don't require any changes because they are using variable
expansion to complete their configuration but it is still important to understand
what each of the variables should be configured to and how they aid in Ansible's
execution.

`group_vars` environment variables:

- Property `BPXK_AUTOCVT` must be configured to `"ON"`; e.g., `_BPXK_AUTOCVT: "ON"`
- Property `ZOAU_HOME` is the **Z Open Automation Utilities (ZOAU)** install root path;
  e.g., `ZOAU_HOME: "/usr/lpp/IBM/zoautil"`
- Property `PYTHONPATH` is the **Z Open Automation Utilities (ZOAU)** Python library path;
  e.g., `PYTHONPATH: "/usr/zoau-wheel/"`
- Property `LIBPATH` is **both** the path to the **Python libraries** on the target
  and the **Z Open Automation Utilities (ZOAU) Python library path** separated by semi-colons; e.g.,
  `LIBPATH: "/usr/lpp/IBM/zoautil/lib/:/usr/lpp/IBM/cyp/v3r12/pyz/lib:/usr/lib:/lib:."`
- Property `PATH` is the ZOA utilities **BIN** path and **Python interpreter** path, e.g.,
  `PATH: "/usr/lpp/IBM/zoautil/bin:/usr/lpp/IBM/cyp/v3r12/pyz/bin:/bin"`
- Property `_CEE_RUNOPTS` is the invocation Language Environment® runtime
  options for programs and used by Python. e.g.,
  `_CEE_RUNOPTS: "FILETAG(AUTOCVT,AUTOTAG) POSIX(ON)"`
- Properties `__TAG_REDIR_ERR`, `_TAG_REDIR_IN`, `_TAG_REDIR_OUT` are txt and used
  by the shell; e.g.,
     ``` {.yaml}
     _TAG_REDIR_ERR: "txt"
     _TAG_REDIR_IN: "txt"
     _TAG_REDIR_OUT: "txt"
     ```
- Property `LANG` is the name of the default locale; value
  C specifies the POSIX locale; for example: ``LANG: "C"``.
- Property `PYTHONSTDINENCODING` should be set to the encoding Unix System Services
  is configured as, supported encodings are ASCII or EBCDIC. This environment
  variable is used to instruct Ansible which encoding it will *pipe* content to
  Python's STDIN (standard in) when `pipelining=true` is set in `ansible.cfg` .
  This environment variable will only apply when using IBM Enterprise Python
  3.10 or later, otherwise, it is ignored.
  For example:`PYTHONSTDINENCODING: "cp1047"`.

A complete example of `all.yml` is:

```{.yaml}
environment_vars:
  _BPXK_AUTOCVT: "ON"
  ZOAU_HOME: "{{ ZOAU }}"
  PYTHONPATH: "{{ ZOAU_PYTHON_LIBRARY_PATH }}"
  LIBPATH: "{{ ZOAU }}/lib:{{ PYZ }}/lib:/lib:/usr/lib:."
  PATH: "{{ ZOAU }}/bin:{{ PYZ }}/bin:/bin:/var/bin:/usr/sbin"
  _CEE_RUNOPTS: "FILETAG(AUTOCVT,AUTOTAG) POSIX(ON)"
  _TAG_REDIR_ERR: "txt"
  _TAG_REDIR_IN: "txt"
  _TAG_REDIR_OUT: "txt"
  LANG: "C"
  PYTHONSTDINENCODING: "cp1047"
```

## Host Variables

Host variables are environment variables referred and located in a `host_vars/`
directory under the `inventories/` directory. They don't necessarily have to be
located under `inventories/`, but it makes for a more universal experience when
using tools such as [ansible](https://docs.ansible.com/ansible/latest/cli/ansible.html),
[ansible-console](https://docs.ansible.com/ansible/latest/cli/ansible-console.html)
will only look for `group_vars/` and `host_vars/` in the inventory directory.

`host_vars/` provides organization such that they are unique to a the z/OS
managed node. The playbook projects include a `*.yml` usually named
`zos_host.yml` that is located in the `host_vars/` directory. These variables
are used in combination with the variables defined in `group_vars/all.yml` such
complete the configuration.

`host_vars` environment variables:

- Property `PYZ` is the python installation home path on the z/OS managed node
  (target), e.g, `pyz: "/usr/lpp/IBM/cyp/v3r12/pyz"`
- Property `ZOAU` is the ZOAU installation home on the z/OS managed node
  (target), e.g, `zoau: "/usr/lpp/IBM/zoautil"`
- Property `ansible_python_interpreter` is the z/OS managed node (target) Python
    binary installation path which generally does not need configuration because
    it using variable expansion to complete the path,
    e.g, `ansible_python_interpreter: "{{PYZ}}/bin/python3"`

A complete example of `zos_host.yml` is:

``` {.yaml}
PYZ: "/usr/lpp/IBM/cyp/v3r12/pyz"
ZOAU: "/usr/lpp/IBM/zoautil"
ZOAU_PYTHON_LIBRARY_PATH: "{{ path_to_wheel_installation_directory }}"

ansible_python_interpreter: "{{ PYZ }}/bin/python3"
```

### Variables for Rocket Python

Currently, **IBM Open Enterprise Python for z/OS** is the supported and
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

Playbook project contain a number of Ansible playbooks. Each project is an
independent use case and its contents provided support that use case. When
executing a playbook, ensure you have either provided the relative (or absolute)
path to the playbook when using the command line command `ansible-playbook`.

When running a playbook, review the comments on how the playbook will
interact with your target, ensure you have the required authority and
permissions such as writing the the target directories or creating data sets.

### Ansible command
Use the Ansible command `ansible-playbook` to run the sample playbook.
The command syntax is `ansible-playbook -i <inventory> <playbook>`; for
example, `ansible-playbook -i inventories sample.yml`.

This command assumes that the controller's public SSH key has been
shared with the managed node. If you want to avoid entering a username
and password each time, copy the SSH public key to the managed node, see the
section [Install SSH key on remote host](#install-ssh-key-on-remote-host).

#### Install SSH key on remote host

The `ssh-copy-id` command is a tool that allows you to install an SSH key in a
remote server’s authorized keys. This command facilitates the SSH key login
often referred to as password-less SSH, which removes the need for a password for
each login thus providing an automatic login process. The `ssh-copy-id` command
is part of OpenSSH that performs remote system administrations using encrypted
SSH connections. Depending on the operating system the Ansible controller is
installed on, installation of `ssh-copy-id` can vary. For example,
- on Debian `sudo apt-get update && sudo apt-get install openssh-client`
- on MacOS `brew install ssh-copy-id`

Using the `ssh-copy-id` command; for example,
`ssh-copy-id -i ~/.ssh/mykey.pub user@<hostname>`.

If you are unable to use `ssh-copy-id` you can perform this manually by using
`ssh-keygen` and answer the prompts that appear. Afterwards you will be left
with a public key `~/.ssh/id_rsa.pub` that needs to be copied to your z/OS target.

```
ssh-keygen
cat ~/.ssh/id_rsa.pub
```

On the z/OS target:
```
mkdir ~/.ssh/
chmod 700 ~/.ssh
touch ~/.ssh/authorized_keys
chmod 600 ~/.ssh/authorized_keys
```
finally, append the contents from `cat ~/.ssh/id_rsa.pub` to `~/.ssh/authorized_keys`

### Password prompt
Alternatively, you can use the `--ask-pass` option to be prompted for
the user\'s password each time a playbook is run; for example,
`ansible-playbook -i <inventory> <playbook>.yml --ask-pass`.

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


