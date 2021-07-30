//IMSOJMP1 JOB Y,
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
//* SKELETON: DFSIXS04
//*
//* FUNCTION: IMBED - EXECUTION JOB FOR JMP #1 - IVP1
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
//IMSOJMP1 EXEC PROC=DFSJMP,
//         NODE1='IMS1510',
//         NODE2='DFS.V15R1M0',
//         TIME=(1440),
//         RGN=0M,                       REGION SIZE FOR EXECUTION
//         OPT=W,                        WAIT FOR CNTRL PRGRM TO START
//         PCB=040,                      DEP RGN INTERREGN COM AREA SIZE
//         CL1=006,CL2=000,CL3=000,CL4=000,  TRANSACTION CLASS
//         NBA=5,                        FP DB BUFFER NUMBER
//         OBA=5,                        FP PAGE-FIXED BUFFER NUMBER
//         IMSID=IMSO,                   IMSID OF IMS CONTROL REGION
//         SSM=ESAF,                     DB2 SSM
//         ENVIRON=DFSJVMEV,             JVM ENVIRONMENT SETTINGS
//         JVMOPMAS=DFSJVMMS,            MASTER/STANDALONE JVM OPTIONS
//         SOUT=*
//*
//* OVERRIDE PROC DD STATEMENTS
//*
//* --NONE--
//*
//* ADDITIONAL DD STATEMENTS
//*
//*TDOUT   DD  SYSOUT=*
//*AVAOUT  DD  PATH='/tmp/andyn/JVM.out',
//*            PATHDISP=(KEEP,KEEP),
//*            PATHOPTS=(OWRONLY,OCREAT,OTRUNC),
//*            PATHMODE=SIRWXU
//*TDERR   DD  SYSOUT=*
//*YSPRINT DD  SYSOUT=*
//*AVAERR  DD  PATH='/tmp/andyn/JVM.err',
//*            PATHDISP=(KEEP,KEEP),
//*            PATHOPTS=(OWRONLY,OCREAT,OTRUNC),
//*            PATHMODE=SIRWXU
//*