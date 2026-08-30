# ibm.ibm_zos_core.job_status

This role gathers job status information and checks if a job is active,
given a specific job ID.

## Role Variables

The only variable required is:

- `job_status_id`: The job ID assigned to the job. Format is `STC`, `JOB`,
  or `TSU` followed by up to 5 digits, or `S`, `J`, or `T` followed by 7
  digits for IDs over 99,999. Accepts the asterisk (`*`) as a wildcard, but
  only retrieves job information from the first match.
