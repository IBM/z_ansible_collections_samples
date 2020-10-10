
:github_url: https://github.com/ansible-collections/ibm_zos_core/blob/dev/plugins/modules/zos_operator_action_query.py

.. _zos_operator_action_query_module:


zos_operator_action_query -- Display messages requiring action
==============================================================



.. contents::
   :local:
   :depth: 1


Synopsis
--------
- Get a list of outstanding messages requiring operator action given one or more conditions.





Parameters
----------


     
job_name
  Return outstanding messages requiring operator action awaiting a reply for a particular job name.

  If the message job name is not specified, all outstanding messages for all job names are returned.

  A trailing asterisk, (*) wildcard is supported.


  | **required**: False
  | **type**: str


     
message_id
  Return outstanding messages requiring operator action awaiting a reply for a particular message identifier.

  If the message identifier is not specified, all outstanding messages for all message identifiers are returned.

  A trailing asterisk, (*) wildcard is supported.


  | **required**: False
  | **type**: str


     
system
  Return outstanding messages requiring operator action awaiting a reply for a particular system.

  If the system name is not specified, all outstanding messages for that system and for the local systems attached to it are returned.

  A trailing asterisk, (*) wildcard is supported.


  | **required**: False
  | **type**: str




Examples
--------

.. code-block:: yaml+jinja

   
   - name: Display all outstanding messages issued on system MV2H
     zos_operator_action_query:
         system: mv2h

   - name: Display all outstanding messages whose job name begin with im5
     zos_operator_action_query:
         job_name: im5*

   - name: Display all outstanding messages whose message id begin with dsi*
     zos_operator_action_query:
         message_id: dsi*

   - name: Display all outstanding messages given job_name, message_id, system
     zos_operator_action_query:
         job_name: mq*
         message_id: dsi*
         system: mv29






See Also
--------

.. seealso::

   - :ref:`zos_operator_module`



Return Values
-------------


   
                              
       changed
        | Indicates if any changes were made during module operation. Given operator action commands query for messages, True is always returned unless either a module or command failure has occurred.
      
        | **returned**: always
        | **type**: bool
      
      
                              
       count
        | The total number of outstanding messages.
      
        | **returned**: on success
        | **type**: int
        | **sample**: 12

            
      
      
                              
       actions
        | The list of the outstanding messages.
      
        | **returned**: success
        | **type**: list      
        | **sample**:

              .. code-block::

                       [{"job_id": "STC01537", "job_name": "IM5HCONN", "message_id": "HWSC0000I", "message_text": "*399 HWSC0000I *IMS CONNECT READY* IM5HCONN", "number": "001", "system": "MV27", "type": "R"}, {"job_id": "STC01533", "job_name": "IM5HCTRL", "message_id": "DFS3139I", "message_text": "*400 DFS3139I IMS INITIALIZED, AUTOMATIC RESTART PROCEEDING IM5H", "number": "002", "system": "MV27", "type": "R"}]
            
              
   
                              
        number
          | The message identification number.
      
          | **returned**: on success
          | **type**: int
          | **sample**: 1

            
      
      
                              
        type
          | The action type,'R' means request.
      
          | **returned**: on success
          | **type**: str
          | **sample**: R

            
      
      
                              
        system
          | System on which the outstanding message requiring operator action awaiting a reply.
      
          | **returned**: on success
          | **type**: str
          | **sample**: MV27

            
      
      
                              
        job_id
          | Job identifier for the outstanding message requiring operator action awaiting a reply.
      
          | **returned**: on success
          | **type**: str
          | **sample**: STC01537

            
      
      
                              
        message_text
          | Job identifier for outstanding message requiring operator action awaiting a reply.
      
          | **returned**: success
          | **type**: str
          | **sample**: *399 HWSC0000I *IMS CONNECT READY* IM5HCONN

            
      
      
                              
        job_name
          | Job name for outstanding message requiring operator action awaiting a reply.
      
          | **returned**: success
          | **type**: str
          | **sample**: IM5HCONN

            
      
      
                              
        message_id
          | Message identifier for outstanding message requiring operator action awaiting a reply.
      
          | **returned**: success
          | **type**: str
          | **sample**: HWSC0000I

            
      
        
      
        
