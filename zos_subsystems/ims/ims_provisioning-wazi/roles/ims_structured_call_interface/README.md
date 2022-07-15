provision-ims-dbdc
=========

Provision an IMS 15 DB/TM instance.

Requirements
------------

Any pre-requisites that may not be covered by Ansible itself or the role should be mentioned here. For instance, if the role uses the EC2 module, it may be a good idea to mention in this section that the boto package is required.

Role Variables
--------------

| Variable                           | Definition                                                                                                                                                          |
| ---------------------------------- | ------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| ADDTL_SAMP_PROC_LIB                | the library to send the output of DFSPROCB                                                                                                                          |
| ARC                                |                                                                                                                                                                     |
| CSQ_SSID                           | The ID of the IMS CSQ feature.                                                                                                                                      |
| DFS_AUTH_LIB_HLQ1                  | The first HLQ to be used to store middleware (IMS, zCEE, etc) related datasets. these include procs, OLDS, WADS, PROCLIB, JOBS, and application specific datasets.  |
| DFS_AUTH_LIB_HLQ2                  | The second HLQ to be used to store middleware (IMS, zCEE, etc) related datasets. these include procs, OLDS, WADS, PROCLIB, JOBS, and application specific datasets. |
| DFS_AUTH_LIB_SYSHLQ1               | The first HLQ for system datasets that will be used by IMS or other middleware during provisioning. These should exist prior to running playbooks.                  |
| DFS_AUTH_LIB_SYSHLQ2               | The second HLQ for system datasets that will be used by IMS or other middleware during provisioning. These should exist prior to running playbooks.                 |
| DFS_DS_VOLUME1                     | The first volume to use for storage when not using SMS.                                                                                                             |
| DFS_DS_VOLUME2                     | The second volume to use for storage when not using SMS.                                                                                                            |
| DFS_IMSPlex                        | The IMS plex name.                                                                                                                                                  |
| DFS_IMSXCFGroup                    | The IMS xcf group name to use for IMS and OTMA.                                                                                                                     |
| DFS_IMS_CRC                        | The command recognition character to use for the IMS instance.                                                                                                      |
| DFS_IMS_LINEGRP                    | The type of line group to use.                                                                                                                                      |
| DFS_IMS_SSID                       | The SSID for the IMS instance to be provisioned.                                                                                                                    |
| DFS_IMS_UNIT                       | The installation group name for the device on which the system is to place data sets.                                                                               |
| DFS_IMS_UNIT2                      | The installation unit name for the device on which the system is to place data sets.                                                                                |
| DFS_IMS_USERID                     | The user ID to use for IMS related provisioning steps.                                                                                                              |
| DFS_INIT_JAVA_CONF                 |                                                                                                                                                                     |
| DFS_IXUSTIM1                       | The EXEC statement TIME parameter for SMP/E, STAGE1, STAGE2.                                                                                                        |
| DFS_PORTID                         | The port ID used for TCPIP port, may be set dynamically.                                                                                                            |
| DFS_PORTIDSuf                      | The suffix to use for the ODBM name.                                                                                                                                |
| DFS_REGION_SSLTCPIPPORT            | The HTTPS port for IMS Connect, may be determined at runtime.                                                                                                       |
| DFS_REGION_TCPIPPORT               | The HTTP port for IMS Connect, may be determined at runtime                                                                                                         |
| DFS_SMS_CLASS                      | The SMS storage class to use when using SMS.                                                                                                                        |
| DFS_SSLPORTID                      | The port ID used for SSL TCPIP port, may be set dynamically.                                                                                                        |
| DFS_useIEFJOBS                     | Y or N should IEFJOBS be used                                                                                                                                       |
| DFS_useRRS                         | Y or N to determine if resource recovery services should be enabled                                                                                                 |
| DFS_workingdir                     | Working directory for the IMS Java region (JMP)                                                                                                                     |
| DXR_IRLM_Procedure                 | internal resource lock manager message procedure                                                                                                                    |
| DXR_IRLM_SSID                      | The ID of the internal resource lock manager                                                                                                                        |
| DYNAMICALLY_RESERVE_PORTS          | if DYNAMICALLY_RESERVE_PORTS=yes, playbooks will attempt to reserve ports                                                                                           |
| HAS_JMPS                           | boolean used to determine if the IMS instance has any MPP regions that need to be deprovisioned.                                                                    |
| HAS_MPPS                           | boolean used to determine if the IMS instance has any MPP regions that need to be deprovisioned.                                                                    |
| IST_VTAM_IMSAPPLID                 | The name for the VTAM application (probably IMS SSID)                                                                                                               |
| JAVA_CONF_PATH                     | The location to create and reference the profile that has environment settings and JVM options.                                                                     |
| JOB_CARD                           | The default job card inserted for dynamically generated JCL. ensure MSGCLASS and CLASS are correct for desired environment.                                         |
| RESERVE_SPECIFIC_PORTS              | If DYNAMICALLY_RESERVE_PORTS=yes and RESERVE_SPECIFIC_PORTS=no, playbook will reserve an open port within the default range of the Ansible zport module              |
| TARGET_USERNAME                    | The z/OS username to use for job submission.                                                                                                                        |
| TCPIP_PROFILE                      | the location of the TCPIP profile information, used to allocate ports                                                                                               |
| TCPIP_PROFILE_TMP                  | the location to save the temporary TCPIP profile information to use during TCPIP updates                                                                            |
| desired_install_job_save_locations | any jobs/procs created that should be saved for later use and/or reference                                                                                          |
| uss_file_path                      | the path where JCL and other scripts will be stored                                                                                                                 |
| uss_utilities_path                 | where MVSUTILS/MVSCMD and other needed tools/scripts are installed on z/OS target node                                                                              |
| zCloud_CSSLIB                      | Dataset containing zos CSS library                                                                                                                                  |
| zCloud_IEFJOBS                     | IEFJOBS dataset name                                                                                                                                                |
| zCloud_IMS_SVC_Type2               | type 2 supervisor call value (between 200-255, surround with quotes)                                                                                                |
| zCloud_IMS_SVC_Type4               | type 4 supervisor call value (between 200-255, surround with quotes)                                                                                                |
| zCloud_LERuntime                   | Data set name used as Language Environment runtime library                                                                                                          |
| zCloud_MACLIB                      | Dataset containing zos macro library                                                                                                                                |
| zCloud_MODGEN                      | the z/OS modgen library                                                                                                                                             |
| zCloud_PROCLIB                     | the PROCLIB to store procedures relating to the provisioned IMS instance                                                                                            |

Dependencies
------------

A list of other roles hosted on Galaxy should go here, plus any details in regards to parameters that may need to be set for other roles, or variables that are used from other roles.

Example Playbook
----------------

```yaml
- include_role:
    name: provision-ims-dbdc
```

License
-------

BSD

Author Information
------------------

Blake Becker (blake.becker@ibm.com)

Bryant Panyarachun (bpanyar@us.ibm.com)