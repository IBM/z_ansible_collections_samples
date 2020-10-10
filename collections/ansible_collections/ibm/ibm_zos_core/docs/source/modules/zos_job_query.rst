
:github_url: https://github.com/ansible-collections/ibm_zos_core/blob/dev/plugins/modules/zos_job_query.py

.. _zos_job_query_module:


zos_job_query -- Query job status
=================================



.. contents::
   :local:
   :depth: 1


Synopsis
--------
- List z/OS job(s) and the current status of the job(s).
- Uses owner to filter the jobs by the job owner.
- Uses system to filter the jobs by system where the job is running (or ran) on.
- Uses job_id to filter the jobs by the job id.





Parameters
----------


     
job_id
  The job number that has been assigned to the job. These normally begin with STC, JOB, TSU and are followed by 5 digits.


  | **required**: False
  | **type**: str


     
job_name
  The job name to query.


  | **required**: False
  | **type**: str
  | **default**: *


     
owner
  Identifies the owner of the job.

  If no owner is set, the default set is 'none' and all jobs will be queried.


  | **required**: False
  | **type**: str




Examples
--------

.. code-block:: yaml+jinja

   
   - name: list zos jobs with a jobname 'IYK3ZNA1'
     zos_job_query:
       job_name: "IYK3ZNA1"

   - name: list the jobs matching jobname 'IYK3*'
     zos_job_query:
       job_name: "IYK3*"

   - name: list the job with a jobname 'IYK3ZNA*' and jobid as JOB01427
     zos_job_query:
       job_name: IYK3ZNA*
       job_id: JOB01427

   - name: list the job with a jobname 'IYK3ZNA*' and owner as BROWNAD
     zos_job_query:
       job_name: IYK3ZNA*
       owner: BROWNAD









Return Values
-------------


   
                              
       changed
        | True if the state was changed, otherwise False.
      
        | **returned**: always
        | **type**: bool
      
      
                              
       jobs
        | The list of z/OS job(s) and status.
      
        | **returned**: success
        | **type**: list      
        | **sample**:

              .. code-block::

                       [{"job_id": "JOB01427", "job_name": "IYK3ZNA1", "owner": "BROWNAD", "ret_code": "null"}, {"job_id": "JOB16577", "job_name": "IYK3ZNA2", "owner": "BROWNAD", "ret_code": {"code": "null", "msg": "CANCELED"}}]
            
              
   
                              
        job_name
          | The name of the batch job.
      
          | **type**: str
          | **sample**: IYK3ZNA2

            
      
      
                              
        owner
          | The owner who ran the job.
      
          | **type**: str
          | **sample**: BROWNAD

            
      
      
                              
        job_id
          | Unique job id assigned to the job by JES.
      
          | **type**: str
          | **sample**: JOB01427

            
      
      
                              
        ret_code
          | Return code output collected from job log.
      
          | **type**: dict      
          | **sample**:

              .. code-block::

                       [{"code": 0}, {"msg": "CC 0000"}]
            
              
   
                              
         msg
            | Return code or abend resulting from the job submission.
      
            | **type**: str
            | **sample**: CC 0000

            
      
      
                              
         code
            | Return code converted to integer value (when possible).
      
            | **type**: int
      
        
      
        
      
      
                              
       message
        | Message returned on failure.
      
        | **returned**: failure
        | **type**: str
        | **sample**: {'msg': 'List FAILED! no such job been found: IYK3Z0R9'}

            
      
        
