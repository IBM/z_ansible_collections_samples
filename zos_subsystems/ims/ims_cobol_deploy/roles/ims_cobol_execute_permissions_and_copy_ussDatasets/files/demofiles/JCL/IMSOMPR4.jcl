//IMSOMPR4 JOB Y,
// CLASS=A,
// MSGCLASS=X,MSGLEVEL=(1,1),
// REGION=0M
//*
//  JCLLIB ORDER=IMS1510.PROCLIB
//*
//*
//*********************************************************************
//* IVP IMS 14.1
//*
//* SKELETON: DFSIXS19
//*
//* FUNCTION: IMBED - EXECUTION JOB FOR MPP #2 - IVP1
//*********************************************************************
//*
//************************************************************@SCPYRT**
//*                                                                   *
//*  LICENSED MATERIALS - PROPERTY OF IBM                             *
//*                                                                   *
//*  5635-A06                                                         *
//*                                                                   *
//*      COPYRIGHT IBM CORP. 1989,1998 ALL RIGHTS RESERVED            *
//*                                                                   *
//*  US GOVERNMENT USERS RESTRICTED RIGHTS - USE, DUPLICATION OR      *
//*  DISCLOSURE RESTRICTED BY GSA ADP SCHEDULE CONTRACT WITH          *
//*  IBM CORP.                                                        *
//*                                                                   *
//************************************************************@ECPYRT**
//*
//IMSOMPR4 EXEC PROC=DFSMPR4,TIME=(1440),
//         NODE1='IMS1510',
//         NODE2='DFS.V15R1M0',
//         NBA=6,
//         OBA=5,
//         SOUT='*',              SYSOUT CLASS
//         CL1=008,               TRANSACTION CLASS 1
//         CL2=000,               TRANSACTION CLASS 2
//         CL3=000,               TRANSACTION CLASS 3
//         CL4=000,               TRANSACTION CLASS 4
//         TLIM=10,               MPR TERMINATION LIMIT
//         SOD=,                  SPIN-OFF DUMP CLASS
//         IMSID=IMSO,       IMSID OF IMS CONTROL REGION
//         PREINIT=DC,            PROCLIB DFSINTXX MEMBER
//         PWFI=N                 PSEUDO=WFI
//*
//* OVERRIDE PROC DD STATEMENTS
//*
//* --NONE--
//*
//* ADDITIONAL DD STATEMENTS
//*
//DFSCTL   DD DISP=SHR,
//            DSN=DFS.V15R1M0.PROCLIB(DFSSBPRM)
//DFSSTAT  DD SYSOUT=*
//* REXX EXEC SOURCE LOCATION
//SYSEXEC  DD DISP=SHR,
//            DSN=DFS.V15R1M0.INSTALIB
//         DD DISP=SHR,
//            DSN=DFS.V15R1M0.SDFSEXEC
//* REXX INPUT LOCATION WHEN STACK IS EMPTY
//SYSTSIN  DD *
/*
//* REXX OUTPUT LOCATION
//SYSTSPRT DD SYSOUT=*
//* COBOL OUTPUT LOCATION
//SYSOUT   DD SYSOUT=*
//* COBOL ABEND OUTPUT LOCATION
//SYSABOUT DD SYSOUT=*
//*