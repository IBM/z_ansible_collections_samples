query_csi
=========

Query CSI data

Requirements
------------

- Ansible Collection `ibm.ibm_zos_core`

Role Variables
--------------

- ### **name**

  Specifies the user name to be associated with the new user ID.

  You can use a maximum of 20 alphanumeric or non-alphanumeric characters.

- ### **userid**

  Specifies the user to be defined to RACF.

- ### **default_group**

  Specifies the name of a RACF-defined group to be used as the default group for the user.

  If you do not specify a group, RACF uses your current connect group as the default.

- ### **default_group_authority**

  Specifies the level of group authority for the new user in the default group.

  The valid group authority values are USE, CREATE, CONNECT, and JOIN, as described in Group authorities.

  If you omit this operand or specify AUTHORITY without group-authority, the default value is USE.
  This operand is group-related. If a user is connected to other groups (with the CONNECT command),
  the user can have a different group authority in each group.

- ### **owner**

  Specifies a RACF-defined user or group to be assigned as the owner of the RACF profile for the user being added.

  If you omit this operand, you are defined as the owner.

- ### **user_catalog**

  Specifies the user catalog entryname for which an alias will be defined.


- ### **master_catalog**

  Identifies the catalog in which the alias is defined. If the catalog's volume is physically mounted, it is dynamically allocated. The volume must be mounted as permanently resident or reserved.

- ### **password**

  Specifies the user's initial logon password. This password is always set expired,
  thus requiring the user to change the password at initial logon.

  Note that the password syntax rules your installation defines using SETROPTS PASSWORD
  do not apply to this password.

  Whether password or passphrase arguments are used depends on system configuration. Passphrase takes priority.

- ### **generate_password**

  Specifies if a random password should be generated when no password is provided.

- ### **passphrase**

  Specifies the user's initial password phrase.
  The password phrase you define is a text string of up to 100 characters and must be enclosed in single
  quotation marks. The password phrase is always set expired, thus requiring the user to change it on initial use.

  Syntax rules for password phrases:
  - Maximum length: 100 characters
  - Minimum length:
    - 9 characters, when the encryption algorithm is KDFAES or ICHPWX11 is present and allows the new value
    - 14 characters, when ICHPWX11 is not present and the encryption algorithm is not KDFAES
  - Must not contain the user ID (as sequential uppercase or sequential lowercase characters)
  - Must contain at least 2 alphabetic characters (A - Z, a - z)
  - Must contain at least 2 non-alphabetic characters (numerics, punctuation, or special characters)
  - Must not contain more than 2 consecutive characters that are identical
  - If a single quotation mark is intended to be part of the password phrase,
    you must use two single quotation marks together for each single quotation mark.

  Whether password or passphrase arguments are used depends on system configuration. Passphrase takes priority.

- ### **generate_passphrase**

  Specifies if a random passphrase should be generated when no passphrase is provided.

- ### **print_pass**

  Specifies if a password and/or passphrase should be printed to the console upon completion of the playbook.

- ### **smtp_server**

  Specifies the SMTP server to use for sending email, if sending email is desired.

- ### **smtp_port**

  Specifies the SMTP port to use for sending email, if sending email is desired.

- ### **smtp_username**

  Specifies the username to use for sending email, if sending email is desired.

- ### **smtp_password**

  Specifies the password to use for sending email, if sending email is desired.

- ### **email_to**

  Specifies the email address which should receive the logon credentials for the new user.

- ### **send_email**

  Specifies if an email should be sent containing new user credentials.

- ### **security_label**

  Specifies the user's default security label, where security-label is an installation-defined
  security label name that represents an association between a particular security level and
  zero or more security categories.

  If the user does not enter a security label when entering the system, and none is assigned based
  on the user's port of entry, this value becomes the user's current security label.

  A security label corresponds to a particular security level (such as CONFIDENTIAL) with a set of
  zero or more security categories (such as PAYROLL or PERSONNEL).

- ### **security_level**

  Specifies the user's security level, where security_level is an installation-defined security
  level name that must be a member of the SECLEVEL profile in the SECDATA class.

  The security_level that you specify corresponds to the number of the minimum security level
  that a user must have to access the resource.

- ### **security_categories**

  Specifies one or more names of installation-defined security categories.

  The names you specify must be defined as members of the CATEGORY profile in a SECDATA class.
  security_categories must be provided as a list.

- ### **omvs_home_directory**

  Specifies the user's z/OS UNIX initial directory path name.

  This is the current working directory for the user's process when the user enters the TSO/E.
  When you define a HOME directory name to RACF, it can contain 1 - 1023 characters.

  The HOME path name can consist of any characters.
  If left blank, home directory is /u/userid.

- ### **omvs_uid**

  Specifies the user identifier. The UID is a numeric value from 0 - 2 147 483 647.

  When assigning a UID to a user, you should make sure that the user's default group has a GID.

  A user who has a UID and a current connect group that has a GID can use functions such as the TSO/E OMVS
  command and can access z/OS UNIX files based on the UID and GID values assigned.

  If left blank, AUTOUID is used.

- ### **omvs_gid**

  Specifies the group identifier. The GID is a numeric value from 0 - 2 147 483 647.

  If no GID specified, the default is used.

- ### **omvs_zfs_data_set_name**

  Specifies the name of the data set to be used when creating ZFS data set.

- ### **omvs_zfs_data_set_size**

  Specifies amount of space, in cylinders, to allocate for the ZFS data set.

  Provided value is used for primary and secondary space.

  If left blank, defaults to 50 CYL.

- ### **bpxprm_data_set**

  Specifies the data set which contains BPXPRMxx members to modify.

- ### **bpxprm_member**

  Specifies the last 2 characters (xx) of the BPXPRMxx member
  in SYS1.PARMLIB to which a mount command for new user's ZFS data set will be added.

  If not specified, no persistent mount will be made for new ZFS data set.

- ### **tso_account_number**

  Specifies the user's default TSO account number when logging on through the TSO/E logon panel.

  The account number you specify must be protected by a profile in the ACCTNUM general resource class.

  Account numbers can consist of any characters, and can be entered with or without single quotation marks.

- ### **tso_logon_procedure**

  Specifies the name of the user's default logon procedure when logging on through the TSO/E logon panel.

  The name you specify must be 1 - 8 alphanumeric characters and begin with an alphabetic character.

  The name must also be defined as a profile in the TSOPROC general resource class.

- ### **dfp_data_application**

  Specifies an 8-character DFP data application identifier.

- ### **data_class**

  Specifies the default data class. The maximum length of data-class-name is 8 characters.

  A data class can specify some or all of the physical data set attributes associated with a new data set.

  During new data set allocation, data management uses the value you specify as a default unless it is
  preempted by a higher priority default, or overridden in some other way, for example by JCL.

- ### **management_class**

  Specifies the default management class. The maximum length of management-class-name is 8 characters.

  A management class contains a collection of management policies that apply to data sets.

  Data management uses the value you specify as a default unless it is preempted by a higher priority default,
  or overridden in some other way, for example by JCL.

- ### **storage_class**

  Specifies the default storage class. The maximum length of storage-class-name is 8 characters.

  A storage class specifies the service level (performance and availability) for data sets managed by the
  storage management subsystem (SMS).

  During new data set allocation, data management uses the value you specify
  as a default unless it is preempted by a higher priority default,
  or overridden in some other way (for example, by JCL).

- ### **groups_to_connect**

  A list of groups to which the user should be added.

  Parameters:

  - **group**: Specifies a RACF-defined group. If you omit this operand, the user is connected to or modified in your current connect group.

  - **owner**: Specifies a RACF-defined user or group to be assigned as the owner of the connect profile. If you are creating a connection and you do not specify an owner, you are defined as the owner of the connect profile.

  - **authority**: Specifies the level of authority the user is to have in the group.
  The valid group authority values are USE, CREATE, CONNECT, and JOIN. If you are creating a connection and omit AUTHORITY or enter it without a value, the default is USE. You cannot give a user a higher level of authority in the group than you have.

  Example:

  ```yaml
    groups_to_connect:
      - group: "PAYROLL"
        owner: "PAYROLL"
        authority: "USE"
      - group: "RESEARCH"
        owner: ""
        authority: "CREATE"
  ```

- ### **resources_to_permit**

  A list of resources the user should be permitted to access.

  Parameters:

  - **profile**: Specifies the name of an existing discrete or generic profile whose access list you want to modify.

  - **authority**: Specifies the access authority you want to associate with the user being created. RACF sets the access authority in the standard access list. If you specify WHEN, RACF sets the access authority in the conditional access list. The valid access authorities are NONE, EXECUTE (for DATASET, PROGRAM, or APPCTP class only), READ, UPDATE, CONTROL, and ALTER.

  - **id**: Specifies the user IDs and group names of RACF-defined users or groups whose authority to access the resource you are giving, removing, or changing. If id is empty, default id is set to the userid of the user being created.

  Example:

  ```yaml
    resources_to_permit:
    - profile: "WJE10.DEPT2.DATA"
      authority: "UPDATE"
      id: "SYSPROG"
    - profile: "RESEARCH.PROJ01.DATA"
      authority: "READ"
      id: ""
  ```

- ### **templates_to_copy**

  A list of any templates on control node that should be evaluated and copied to the z/OS host.

  Parameters:

  - **src**: Specifies the absolute or relative path to the template on the Ansible control node. Relative paths are relative to playbook runners current working directory.

  - **dest**: Specifies the destination data set or unix path on the z/OS host. This is where the template contents are copied to after evaluation.

  Example:

  ```yaml
    templates_to_copy:
      - src: "../templates/profile.j2"
        dest: "/u/{{ userid }}/.profile"
      - src: "../templates/datatemplate.j2"
        dest: "{{ userid }}.private.somedata"
  ```

- ### **files_to_copy**

  A list of any files on control node that should be copied to the z/OS host.

  Parameters:

  - **src**: Specifies the absolute or relative path to the file on the Ansible control node. Relative paths are relative to playbook runners current working directory.

  - **dest**:      Specifies the destination data set or unix path on the z/OS host. This is where the file is copied.

  Example:

  ```yaml
    files_to_copy:
      - src: "../files/profile.txt"
        dest: "/u/{{ userid }}/.profile"
      - src: "../files/datafinal.txt"
        dest: "{{ userid }}.private.mydata"
  ```

- ### **target_charset**

  Specifies the character set any copied templates and files should be converted to.

Example Playbook
----------------

```yaml
- hosts: all
  collections:
    - ibm.ibm_zos_core
  gather_facts: no
  environment: "{{ environment_vars }}"

  tasks:
    - name: Add a new user to z/OS system
      include_role:
        name: add-zos-user
```

License
-------

Copyright (c) IBM Corporation 2020 Apache License, Version 2.0 (see https://opensource.org/licenses/Apache-2.0)

Author Information
------------------

- Blake Becker blake.becker@ibm.com, [@blakeinate](https://github.com/blakeinate)

Copyright
---------

© Copyright IBM Corporation 2021
