# Best practices using the samples repository

There are several ways to access the playbooks included in the samples GitHub
repository. You can access them using  a web browser, graphical interface (GUI),
command line interface (CLI() or an ansible playbook, to name a few. We
recommend that you fork or clone the repository locally to the ansible
controller, this creates copies of all the playbooks allowing you to try
them out and customize as necessary.

This document requires that you have
[Git installed](https://git-scm.com/book/en/v2/Getting-Started-Installing-Git)
or [download](https://git-scm.com/downloads) the distribution of Git where you
will clone the repository.

## Watching the GitHub repository
The [samples repository](https://github.com/IBM/z_ansible_collections_samples/)
is a collection of samples that will frequently have new playbooks contributed.


In order to know when updates are made, you can **watch** the repository
and by doing so, you will receive notifications when changes
are made. In the upper right hand corner you will see the **watch** toggle:

<img width="325" alt="image" src="https://media.github.ibm.com/user/15905/files/97b26800-f2dc-11ea-8e96-f7723979c03e">

## Cloning the repository

Cloning the repository will clone the GitHub repository locally such that you
will have your own copy and can customize and tailor the samples to your
specific requirements.

There are a number of ways to clone a repo, command line interface, graphical
interfaces and even playbooks. In this document, we will discuss using the
command line interface and a playbook, when run will either clone or pull the
latest changes from the repository.

### Playbook

A moderately simple playbook has been included that performs a number of Git
operations:

  * evaluating the `repo_root_dest` directory exists and is managed by Git
  * updating the repository
  * cloning the repository

This playbook has been written specifically for this repository so it can be
copied and run as a standalone playbook without having to have cloned the
repository first.

Thus, you only need to configure or override the variable `repo_root_dest` noted
in the playbook.

To edit the playbook:
```ymal
    repo_root_dest: "/tmp"
```

To override the variable from the command line (Ansible ad-hoc), set the value
for `-e repo_root_dest`:

```sh
  ansible-playbook clone-repo.yaml -e repo_root_dest=~/
```

After executing this playbook, either you will have cloned the repository or
pulled the repositories latest changes.

### CLI

If you would prefer to clone or update the samples repository using a command line
interface, below are the commands you can use to perform those operations.

If you have not cloned the samples repository, choose or create a directory and
execute the `git clone` command.

```sh
$ cd ~/home
$ mkdir github
$ cd github
$ git clone git@github.com:IBM/z_ansible_collections_samples.git
```

Result:
```
Cloning into 'z_ansible_collections_samples'...
remote: Enumerating objects: 536, done.
remote: Counting objects: 100% (536/536), done.
remote: Compressing objects: 100% (218/218), done.
remote: Total 1135 (delta 272), reused 509 (delta 261), pack-reused 599
Receiving objects: 100% (1135/1135), 4.01 MiB | 4.07 MiB/s, done.
Resolving deltas: 100% (527/527), done.
```

If you have already cloned the samples repository, you can execute the
`git pull` command from within the repository and get the latest changes.

```sh
$ cd ~/home/z_ansible_collections_samples
$ git pull
```

Result:
```
  Already up to date.
```


