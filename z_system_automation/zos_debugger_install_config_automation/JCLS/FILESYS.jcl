//FILESYS JOB CLASS=A,                                                  
//   MSGCLASS=A,REGION=0M,MSGLEVEL=(1,1),NOTIFY=&SYSUID
//* //***************************************************************
//* //* This job must be updated to reflect your environment.
//* //* This sample:
//* //*   . Allocates a new z/OS UNIX file system
//* //*   . Creates a mount point directory
//* //*   . Mounts the file system
//* //*
//* //* - Provide valid job card information
//* //* - Change:
//* //*   /u/ibmuser/dbg17/hadrh00
//* //*   ----+----1----+----2----+----3----+----4----+----5
//* //*              - To the absolute z/OS UNIX path for the download
//* //*                package (starting with /)
//* //*              - Maximum length is 50 characters
//* //*              - Do not include a trailing /
//* //*   IBMUSER.HADRH00.LIB
//* //*              - To your file system data set name
//* //*
//* //* Your userid MUST be defined as a SUPERUSER to successfully
//* //* run this job
//* //*
//* //***************************************************************
//* //*
//CREATE   EXEC PGM=IDCAMS,REGION=0M,COND=(0,LT)
//SYSPRINT DD SYSOUT=*
//SYSIN    DD *
   DEFINE CLUSTER ( -
          NAME(IBMUSER.HADRH00.LIB) -
          TRK(11807 1180) -
          LINEAR -
          SHAREOPTIONS(3) -
          )
//* //*
//         SET ZFSDSN='IBMUSER.HADRH00.LIB'
//FORMAT   EXEC PGM=IOEAGFMT,REGION=0M,COND=(0,LT),
//            PARM='-aggregate &ZFSDSN -compat'
//*STEPLIB  DD DISP=SHR,DSN=IOE.SIOELMOD        before z/OS 1.13
//*STEPLIB  DD DISP=SHR,DSN=SYS1.SIEALNKE       from z/OS 1.13
//SYSPRINT DD SYSOUT=*
//* //*
