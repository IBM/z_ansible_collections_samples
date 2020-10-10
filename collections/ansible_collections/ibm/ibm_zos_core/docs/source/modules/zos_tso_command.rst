
:github_url: https://github.com/ansible-collections/ibm_zos_core/blob/dev/plugins/modules/zos_tso_command.py

.. _zos_tso_command_module:


zos_tso_command -- Execute TSO commands
=======================================



.. contents::
   :local:
   :depth: 1


Synopsis
--------
- Execute TSO commands on the target z/OS system with the provided options and receive a structured response.





Parameters
----------


     
commands
  One or more TSO commands to execute on the target z/OS system.

  Accepts a single string or list of strings as input.


  | **required**: True
  | **type**: raw




Examples
--------

.. code-block:: yaml+jinja

   
   - name: Execute TSO commands to allocate a new dataset
     zos_tso_command:
         commands:
             - alloc da('TEST.HILL3.TEST') like('TEST.HILL3')
             - delete 'TEST.HILL3.TEST'

   - name: Execute TSO command list user TESTUSER to obtain TSO information
     zos_tso_command:
         commands:
              - LU TESTUSER










Return Values
-------------


   
                              
       output
        | List of each TSO command output.
      
        | **returned**: always
        | **type**: list
              
   
                              
        command
          | The executed TSO command.
      
          | **returned**: always
          | **type**: str
      
      
                              
        rc
          | The return code from the executed TSO command.
      
          | **returned**: always
          | **type**: int
      
      
                              
        content
          | The response resulting from the execution of the TSO command.
      
          | **returned**: always
          | **type**: list      
          | **sample**:

              .. code-block::

                       ["NO MODEL DATA SET                                                OMVSADM", "TERMUACC                                                                ", "SUBGROUP(S)= VSAMDSET SYSCTLG  BATCH    SASS     MASS     IMSGRP1       ", "             IMSGRP2  IMSGRP3  DSNCAT   DSN120   J42      M63           ", "             J91      J09      J97      J93      M82      D67           ", "             D52      M12      CCG      D17      M32      IMSVS         ", "             DSN210   DSN130   RAD      CATLG4   VCAT     CSP           "]
            
      
      
                              
        lines
          | The line number of the content.
      
          | **returned**: always
          | **type**: int
      
        
      
        
