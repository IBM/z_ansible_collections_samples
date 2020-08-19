//******************************************************************************
//* Copyright (c) IBM Corporation 2020
//******************************************************************************
//******************************************************************************
//* Configure the job card as needed, most common keyword parameters often
//* needing editing are:
//* CLASS: Used to achieve a balance between different types of jobs and avoid
//*        contention between jobs that use the same resources.
//* MSGLEVEL: controls hpw the allocation messages and termination messages are
//*           printed in the job's output listing (SYSOUT).
//* MSGCLASS: assign an output class for your output listing (SYSOUT)
//*
//* Notes:
//*   If you change the job name, you must update the playbook job name
//*   references.
//******************************************************************************
//HELLO    JOB (T043JM,JM00,1,0,0,0),'HELLO WORLD - JRM',
//             MSGCLASS=X,MSGLEVEL=1,NOTIFY=&SYSUID
//*
//* PRINT "HELLO WORLD" ON JOB OUTPUT
//*
//* NOTE THAT THE EXCLAMATION POINT IS INVALID EBCDIC FOR JCL
//*   AND WILL CAUSE A JCL ERROR
//*
//STEP0001 EXEC PGM=IEBGENER
//SYSIN    DD DUMMY
//SYSPRINT DD SYSOUT=*
//SYSUT1   DD *
HELLO, WORLD
/*
//SYSUT2   DD SYSOUT=*
//