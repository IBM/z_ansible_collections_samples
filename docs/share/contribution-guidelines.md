# Contribution Guidelines for the Ansible for IBM Z Playbook Repository

This document captures the general guidelines for contributing to the
**Ansible® for IBM Z® Playbooks repository**. All playbooks contributed to this
repository are required to follow these guidelines.

**Quick links**  
[License](#license)  
[Developer Certificate of Origin](#developer-certificate-of-origin)  
[Community Guidelines](#community-guidelines)  
[Playbook Development Guidelines](#playbook-development-guidelines)  
[Obtaining the Source Code](#obtaining-the-source-code)  

## License
All contributions must be made under the Apache 2 license. For reference review
the [Apache 2 license](../../LICENSE)

## Developer Certificate of Origin
The **Ansible for IBM Z playbook repository** requires the use of the
[Developer’s Certificate of Origin 1.1 (DCO)](https://developercertificate.org)
which is the same mechanism that the Linux® Kernel and many other communities
use to manage code contributions.

### Sign Your Commit
All authors to who contribute to this project retain the copyright to their work.
However, to ensure that authors are only submitting work that they have rights
to, we require everyone to acknowledge this by signing their work. The sign-off
is done during a commit where a line is added as part of the commit message to
indicate you authored this change.

Here is an example of a **Signed-off-by** line, which indicates that the
submitter accepts the DCO:

  `Signed-off-by: John Doe <john.doe@hisdomain.com>`

You can include this automatically when you commit a change to your local
Git repository using `git commit -s`. For example:

  `git commit -s -m "Updated playbook to support some feature."`

An example of a signed commit from Git history:

```
commit 64162df2514e2f9ec51a986e64a93c3568935d45
Author: John Doe <john.doe@hisdomain.com>
Date:   Sun Jul 11 13:10:01 2021 -0700

    Updated playbook with new variables

    Signed-off-by: John Doe <john.doe@hisdomain.com>

```

By doing this, you state that you certify the following (from https://developercertificate.org):

```
Developer Certificate of Origin
Version 1.1

Copyright (C) 2004, 2006 The Linux Foundation and its contributors.
1 Letterman Drive
Suite D4700
San Francisco, CA, 94129

Everyone is permitted to copy and distribute verbatim copies of this
license document, but changing it is not allowed.


Developer's Certificate of Origin 1.1

By making a contribution to this project, I certify that:

(a) The contribution was created in whole or in part by me and I
    have the right to submit it under the open source license
    indicated in the file; or

(b) The contribution is based upon previous work that, to the best
    of my knowledge, is covered under an appropriate open source
    license and I have the right under that license to submit that
    work with modifications, whether created in whole or in part
    by me, under the same open source license (unless I am
    permitted to submit under a different license), as indicated
    in the file; or

(c) The contribution was provided directly to me by some other
    person who certified (a), (b) or (c) and I have not modified
    it.

(d) I understand and agree that this project and the contribution
    are public and that a record of the contribution (including all
    personal information I submit with it, including my sign-off) is
    maintained indefinitely and may be redistributed consistent with
    this project or the open source license(s) involved.
```

### What if you forget to sign off on a commit?

To sign old commits:

  `git rebase --exec 'git commit --amend --no-edit --signoff' -i <commit-hash>`

where commit hash is one before your first commit in history

### Tools that can help with sign-offs

There are a number of tools available to manage DCO sign-offs if you prefer to
use them.

- [Git commit](https://git-scm.com/docs/git-commit#Documentation/git-commit.txt---signoff)
- [DCO command line tool](https://github.com/coderanger/dco ), that can perform
   a sign-off for an entire repository.
- [GitHub browser extensions](https://github.com/scottrigby/dco-gh-ui)
  - [Chrome](https://github.com/scottrigby/dco-gh-ui#chrome)
  - [Firefox](https://github.com/scottrigby/dco-gh-ui#firefox)
- Shell script
  - Use a shell script to automatically apply signing. Add the snippet into your
    `.bashrc` file.

```sh
git() {
    if [[ $1 == "commit" ]]; then
        shift
        echo "Executing git commit -s $@"
        command git commit -s "$@"
    else
        command git "$@"
    fi
}
```

## Community Guidelines
This list outlines the general conventions, guidelines and practices for
contributing to this repository.

- Consider opening a feature request where you can describe your idea even if
  you are not able to implement. Ideas and use cases are welcome and may
  inspire someone to contribute or may have something already in the works.
- Before opening a feature request, review the playbooks in the repository that
  offer the same function or could be extended.
- Communicate frequently with the project members whether its before a feature
  request or during a pull request, this is the best way to ensure everyone's
  interest are met.
- Contributions must be complete and functional Ansible playbook that adheres to
  the [playbook structure outline](#playbook-structure).
- When a pull request is opened, it must include the complete STDOUT log
  as certification the playbook is functional.

## Playbook Development Guidelines

### General
- If you have any third party dependencies such a PIP library, include a
  `requirements.txt` that should reside next to the playbook within the same
  folder.
- Keep playbooks and roles small and independent without cross dependencies.
  Rather than a single long and difficult to follow playbook, break them up into
  logical playbooks that might run stand alone. For example, if you are
  contributing a playbook that manages users, you might want to separate
  add_user, delete_user, update_user, etc.

### Playbooks
The purpose of this section is to provide some helpful reference that you can
follow up that may aid you in developing Ansible playbooks.
- [Ansible Concepts](https://docs.ansible.com/ansible/latest/user_guide/basic_concepts.html#ansible-concepts)
- [Introduction to playbooks](https://docs.ansible.com/ansible/latest/user_guide/playbooks_intro.html#intro-to-playbooks)
- [Working with playbooks](https://docs.ansible.com/ansible/latest/playbook_guide/playbooks.html#working-with-playbooks)
- [Complete Ansible User Guide](https://docs.ansible.com/ansible/latest/user_guide/index.html#user-guide)

### Roles
If in your playbook you begin to notice your it is becoming overly
complex and difficult to maintain, think about introducing a role. Playbooks
invoke roles instead of tasks such that you can still group tasks together then
reuse these roles in other playbooks as often as needed.

Roles offer a level of abstraction that let you collect templates, static files
and variables along with your tasks in one structured reusable format. Roles
essentially give you the opportunity to reduce a complex playbook into smaller
pieces that are reusable. Imagine you are writing a playbook to install
a webserver such as Liberty onto many LPARs, you may not want to copy and paste
that task over and over and instead allow the focus of the playbook to simply
connect, invoke a reusable role that does all the micro operations to install
liberty with a few arguments such as the install path. This would allow the
playbook developer to focus on the task at hand which is to connect, install
and validate.

Roles do require a particular directory structure which is
[documented here](https://docs.ansible.com/ansible/latest/user_guide/playbooks_reuse_roles.html#roles)
and you can even view some of samples in this repository that include roles
like our
[Manage z/OS Users Using Ansible](https://github.com/IBM/z_ansible_collections_samples/tree/master/zos_concepts/user_management/add_remove_user)

### Playbook Structure
Our playbooks follow a common directory structure, this provides our users with
consistency and what to expect. The directory structure might change on if you
are including a role in your playbook.

For example, if a new playbook called `new_playbook` was delivered under the
z/OS `concepts`topic, we might recommend you place your `new_playbook` under the
`<repo>/zos_concept/` folder and if it included a few roles, this structure
would look like:

```py
<repo>/zos_concept/new_playbook/
├── README.md             # This playbooks main documentation file
│
├── runtime-requirements  # This lists the playbooks requirements
│
├── site.yml              # The main project playbook
├── another.yml           # another.yml, more.yml playbooks that can be imported by site.yml or not
├── more.yml
│
├── ansible.cfg           # Custom Ansible configuration if needed
├── inventories/          # inventories folder
│   └── host_vars/        # Assign variables to particular systems
│       └── zos_host.yml
│   └── group_vars/       # Assign variables to particular groups
│       └── all.yml
│   ├── inventory.yml     # Inventory
│
├── files/                # Additional files your playbook might require
│
├── docs/                 # Additional docs needed for your playbook/workflow
│   ├── *.md
│   ├──/images
│
├── roles/              # Roles your playbook is using
│   ├── role1/
│       ├──tasks/
│       ├──handlers/
│       ├──library/
│       ├──files/
│       ├──templates/
│       ├──vars/
│       ├──defaults/
│       ├──meta/
│   ├── role2/
│   └── .../
```

An Ansible role has a defined directory structure with a number of standard
directories. You must include at least one of these directories in each role.
You can omit any directories the role does not use. For more information on
roles, see the
[documentation](https://docs.ansible.com/ansible/latest/user_guide/playbooks_reuse_roles.html#role-directory-structure)

### IDE and plugins
IDE's if used to develop playbooks each often have a number of plugins available
to assist with formatting and syntax highlighting. Since there are so many
IDEs available, brief mention that many use VSCode.

- Some popular plugins for VSCode for YAML an Ansible are:
  - [Ansible Language](https://marketplace.visualstudio.com/items?itemName=redhat.ansible)
    from Red Hat® which supports displaying violations identified by
    ansible-lint and yaml-lint
  - [YAML](https://marketplace.visualstudio.com/items?itemName=redhat.vscode-yaml)
    from Red Hat which offers YAML validation, outlining, auto completion, hover
    text and formatting.

### Code Style Guidelines
Ansible allows much freedom when it comes to writing a playbook. Our best advice
is to review how other playbooks are written and incorporate those practices
into your playbook. To aid in getting you started with some of the most common
practices, we will discuss them below briefly. You may find it helpful to review
[The Inside Playbook](https://www.ansible.com/blog/ansible-best-practices-essentials)
essentials and best practices.

Consider:
- Always name your tasks and plays, it reads easier when users are watching the
  output of a playbook
  ```
  - hosts: production
    name: Copy and fetch data from z/OS
    tasks:
      - name: Copy /etc/profile from Unix System Services
        ibm.ibm_zos_core.zos_fetch:
        .....
  ```
- Use well defined and easily readable variable names
  ```
  lpar_etc_profile: /etc/profile
  ```
- Avoid `key=value` shorthand where content is placed on one line
  `state=present src=/etc/profile dest=/tmp force=true`
  instead:
  ```
  state=present
  src=/etc/profile
  dest=/tmp force=true
  ```
- Always look to use modules before resorting to commands executing in
  `shell`, `raw`, `command`
- If your playbook has many debug statements consider using the `verbosity`
  parameter
  ```
  - debug:
   msg: "This message always appears on the console."

  - debug:
   msg: "This message only appears on the console with ansible-playbook -vv+"
   verbosity: 2
  ```
- Enforcing desired state
  - What it means is that a playbook can run more than once (two times in a row)
    and the second time it will not break anything. You can you use many of the
    Ansible constructs such as
    [conditionals](https://docs.ansible.com/ansible/latest/user_guide/playbooks_conditionals.html#conditionals)
    to achieve this desired state.

    For example, consider the below command will always execute regardless of state:
    ```
    - name: Create an operator action (WTOR) "DUMP COMM=('test dump')" for
      system {{ system_name }}
      ibm.ibm_zos_core.zos_operator:
        cmd: "DUMP COMM=('test dump')"
    ```
    where as in this snippet we are checking for a specific boolean with the
    `when` operator before continuing.
    ```
    - name: Create an operator action (WTOR) "DUMP COMM=('test dump')" for
      system {{ system_name }}
      ibm.ibm_zos_core.zos_operator:
        cmd: "DUMP COMM=('test dump')"
      register: result_zos_operator
      when: bool_zos_operator_action_continue
    ```
- Fully qualified collection names
  - Since Ansible 2.10, many collections have been moved to collections into
    Galaxy. To ensure future portability its is best to use the fully qualified
    name for the module.
    For example prior to Ansible 2.10 you could reference the command module
    with syntax `command: ls -la` where now its recommended you do so as
    `ansible.builtin.command: ls -la`. For the index of all modules and fully
    qualified names, review the
    [documentation](https://docs.ansible.com/ansible/latest/collections/index_module.html)

#### Linting
A bot is currently in place to enforce syntax and style guidelines for
playbooks, roles, and collections as defined by [ansible-lint](https://ansible.readthedocs.io/projects/lint/)


#### Indentation, breaking long lines, comments, etc
Ansible Playbooks are written in YAML and therefore playbooks inherit
[YAML rules](https://yamllint.readthedocs.io/en/latest/rules.html). It's a good
idea to review the YAML rules such as
[line-length](https://yamllint.readthedocs.io/en/latest/rules.html#module-yamllint.rules.line_length),
[trailing-spaces](https://yamllint.readthedocs.io/en/latest/rules.html#module-yamllint.rules.trailing_spaces),
[indentation](https://yamllint.readthedocs.io/en/latest/rules.html#module-yamllint.rules.indentation),
[comments](https://yamllint.readthedocs.io/en/latest/rules.html#module-yamllint.rules.comments)
just to name a few.

#### Keywords
Familiarize yourself with the keywords available to playbooks. You may find it
helpful to use them in your development and avoid using the protected keywords
as your own variables.

Follow the
[playbook keywords guide](https://docs.ansible.com/ansible/latest/reference_appendices/playbooks_keywords.html#playbook-keywords)
for more details.

## Playbook Documentation Guidelines

### General
All playbooks require minimally one README.md located at the same level as the
playbook in its own project. The idea here is that one playbook project (folder)
can stand on its own sort of like its own project, thus all its folder contents
should be all a user needs to focus on.

### Readme
The contents of the README should be uniform or as close as possible to uniform
to all other playbook READMEs. They should try to contain the following topics:

- Title
  - Include a title at the top of the README, usually the tile is the name of
    the sample you are delivering. For example, if I am contributing a playbook
    that demonstrates working with data sets, I might title it "Data Set Basics".
- Requirements
  - Include a **Requirements** section and list any obvious requirements a user
    would need to run this playbook. It could be PIP libs, other collections,
    etc; make it obvious, don't rely on users having to wade through source to
    figure this out.
- Getting Started
  - Include a **Getting Started** section and list out the instructions a user
    needs to follow to configure and run this playbook. It might be they need to
    configure the sample inventory file you are including, update the vars, how
    to install something, etc.
- License
  - Include a license section and this content:
    ```
    Licensed under [Apache License,
    Version 2.0](https://opensource.org/licenses/Apache-2.0).
    ```
- Support
  - Include a support section and this content:
    ```
    Please refer to the [support section](../../../README.md#support) for more
    details.
    ```

## Obtaining the Source Code
Obtaining the source to contribute is best done by forking the repository. A
fork is a copy of a repository that allows you to freely make changes without
impacting the original project.

There are [many guides](https://docs.github.com/en/get-started/quickstart/fork-a-repo)
available that discuss the GitHub forking workflow model. Although it can seem
intimidating, once you learn it, it's always the same and can be applied
everywhere.

To summarize the flow:
- Fork the repository
  - This is done through GitHub by logging into your account and clicking the
    Fork button
- Locally clone your newly forked repository replacing your GIT_ID
  - `git clone git@github.com:GIT_ID/z_ansible_collections_samples.git`
- Track the original repository as a remote of the fork
  - `git remote add --track master upstream git@github.com:IBM/z_ansible_collections_samples.git`
  - `git fetch upstream`
- Crete a new branch for your changes
  - `git checkout -b new-playbook upstream/master`
- Make your changes to your playbook, add files, etc
- Add your changes
  - `git add .`
- Commit and sign off on your changes
  - `git commit -s -m "Contributing my first playbook"`
- Push your changes
  - `git push -u origin new-playbook`
- Submit your pull request
  - Now your are ready to submit the playbook project for approval. Go to the
    [Pull requests tab](https://github.com/IBM/z_ansible_collections_samples/pulls)
    and you should see an automatic suggestion from GitHub
    asking you to create a pull request from your new branch. Essentially this
    pull request will link your changes and this repository together so the
    changes can be accepted and pulled into the repository.
