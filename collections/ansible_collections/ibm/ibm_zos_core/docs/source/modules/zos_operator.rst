
:github_url: https://github.com/ansible-collections/ibm_zos_core/blob/dev/plugins/modules/zos_operator.py

.. _zos_operator_module:


zos_operator -- Execute operator command
========================================



.. contents::
   :local:
   :depth: 1


Synopsis
--------
- Execute an operator command and receive the output.





Parameters
----------


     
cmd
  The command to execute.


  | **required**: True
  | **type**: str


     
debug
  Return debugging information.


  | **required**: False
  | **type**: bool


     
verbose
  Return verbose information.


  | **required**: False
  | **type**: bool




Examples
--------

.. code-block:: yaml+jinja

   
   - name: Execute an operator command to show active jobs
     zos_operator:
       cmd: 'd u,all'

   - name: Execute an operator command to show active jobs with verbose information
     zos_operator:
       cmd: 'd u,all'
       verbose: true

   - name: Execute an operator command to show active jobs with verbose and debug information
     zos_operator:
       cmd: 'd u,all'
       verbose: true
       debug: true

   - name: Execute an operator command to purge all job logs (requires escaping)
     zos_operator:
       cmd: "\\$PJ(*)"










Return Values
-------------


   
                              
       rc
        | Return code of the operator command
      
        | **returned**: on success
        | **type**: int
      
      
                              
       content
        | The response resulting from the execution of the operator command
      
        | **returned**: on success
        | **type**: list      
        | **sample**:

              .. code-block::

                       ["MV2C      2020039  04:29:57.58             ISF031I CONSOLE XIAOPIN ACTIVATED ", "MV2C      2020039  04:29:57.58            -D U,ALL                           ", "MV2C      2020039  04:29:57.59             IEE457I 04.29.57 UNIT STATUS 948  ", "         UNIT TYPE STATUS        VOLSER     VOLSTATE      SS                 ", "          0100 3277 OFFLINE                                 0                ", "          0101 3277 OFFLINE                                 0                "]
            
      
      
                              
       changed
        | Indicates if any changes were made during module operation. Given operator commands may introduce changes that are unknown to the module. True is always returned unless either a module or command failure has occurred.
      
        | **returned**: always
        | **type**: bool
      
        
