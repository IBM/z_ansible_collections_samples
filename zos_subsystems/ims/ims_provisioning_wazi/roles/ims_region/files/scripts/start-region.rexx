/* REXX */
/**********************************************************************/
/* Submit a job to start a region or other long running proc.         */
/* Check successfully started and retry n times in case we hit        */
/* some common IO errors where start is needed a second time          */
/* Parms: 1   job to submit (needs job card)                          */
/*        2   name of proc that should be running after submit        */
/**********************************************************************/
/*####################################################################*/
/*# © Copyright IBM Corporation 2023                                  */
/*####################################################################*/
parse arg jobname regionname 
trace 'o'

catd=-1
term=0         /* used to save return values  */
highrc=0       /* return value variable       */

quiet = 0
jobOnly = 1

originalJobName = jobname
do i=0 to 3
    jobname=originalJobName /* submitpath procedure overrides default name */
    call submitpath
    if (highrc > 0) then
    do
        Say "Submission of job failed"
        exit highrc
    end
    address SYSCALL "SLEEP" 2 
    call checksuccess
    if (successstatus == 0) then
    do
        Say "Region start succeeded"
        exit 0
    end
end
say "Region start failed"
exit 1


/**********************************************************************/
/* All messages are issued by the write syscall.                      */
/* Parms: 1   message number                                          */
/*        2   message text                                            */
/*        3-n substitution text                                       */
/* Use %% as a place-holder for substitution text.                    */
/* NOTE: Has a line limit of 4096                                     */
/**********************************************************************/
say: procedure expose catd ESC_N
   mtext = getMsg(arg(1), arg(2)) || ESC_N

   do si = 3 to arg()
      parse var mtext mtpref '%%' mtsuf
      mtext = mtpref || arg(si) || mtsuf
   end

   address syscall "write 1 mtext"
return

/**********************************************************************/
/* All error messages are issued by the write syscall.                */
/* Parms:  1   message number                                         */
/*         2   message text (used only if catgets fails)              */
/*         3-n substitution text                                      */
/* Use %% as a place-holder for substitution text.                    */
/* NOTE: Has a line limit of 4096                                     */
/**********************************************************************/
sayerr: procedure expose catd ESC_N
   mtext = getMsg(arg(1), arg(2)) || ESC_N

   do si = 3 to arg()
      parse var mtext mtpref '%%' mtsuf
      mtext = mtpref || arg(si) || mtsuf
   end

   address syscall "write 2 mtext"
return


/**********************************************************************/
/* Retrieve message from the message catalog.                         */
/* Parms:  1   message number                                         */
/*         2   message text (used only if catgets fails)              */
/* The following global variables must be set                         */
/*    catd   catalog descriptor                                       */
/*    mset   message set number                                       */
/**********************************************************************/
getMsg: procedure expose catd
   parse arg msgNum, mtext
   mset=1                              /* use this message set number */

   if catd=-1 then
     do
       address syscall "catopen fsumrcat.cat" /* open this msg catalog*/
       catd=retval
     end

   if catd>=0 then
      address syscall "catgets (catd) (mset) (msgNum) mtext"

return mtext


/**********************************************************************/
/** Process path                                                      */
/**********************************************************************/
submitpath:
   /* read file into job. and submit                                  */
   address syscall 'readfile (jobname) job.'
   if retval<>-1 then
      do
        /* Pad jobname with single quotes... */
        jobname = "'" || jobname || "'"
        call sub getMsg(5405,"path")
      end
   else if (quiet) then
      highrc=1
   else
      do
        address syscall "strerror" errno errnojr "err."
        call sayerr 5407,"submit: '%%' Could not open file. errno=%%",,
             jobname, err.SE_ERRNO
        highrc=1
      end
return


/**********************************************************************/
/** Submit the job                                                    */
/** and output the correct message                                    */
/**********************************************************************/

sub:
   /* use rexx submit function to submit job, print jobid and source  */
   target=arg(1)
   if (jobname <> '') then
      target=target jobname

   /* Empty jobs break submit() */
   if (job.0 = 0) then
      do
         if (quiet <> 1) then
            call sayerr 5409,"Could not submit empty job."
         highrc=1
         return
      end

   rv=submit(job.)

   if substr(rv,1,1)<>'00'x then
      do
        if (jobOnly) then
          call say 5400, "%%",rv
        else
          call say 5401, "JOB %% submitted from %%",rv,target
      end
   else if (quiet) then
      highrc=1
   else
      do
        call sayerr 5408, "submit: Not accepted by JES."
        highrc=1
      end
return

checksuccess:
    rc=isfcalls('ON')
    isffindlim = 9999999
    isfscrolltype = 'FINDNEXT'
    isflinelim = 1
    isfcols = 'JNAME'
    successstatus=1
    do mainloop = 1 to 10

        Say "Looking for job"
        Address SDSF "ISFEXEC DA"                      
        if rc<>0 then   
            do                               
                Say "SDSF address space not loaded. RC=" rc
                successstatus=1
                leave
            end                         

        fixedField = word(isfcols,1)                      

        do ix=1 to isfrows                                
            if pos(regionname,JNAME.ix) = 1 then 
            do
                Say "Found job " JNAME.ix
                successstatus=0    
                leave                     
            end
        end
        if (successstatus==0) then
            leave
        address SYSCALL "SLEEP" 1     
    end
    rc=isfcalls('OFF')
    return successstatus
