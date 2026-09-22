//EQAWISVC JOB 5724-T07,MSGLEVEL=(1,1),MSGCLASS=A
//*********************************************************************
//*                                                                   *
//*  IBM z/OS Debugger                                                *
//*               17.0                                                *
//*                                                                   *
//*********************************************************************
//* Licensed Materials - Property of IBM                              *
//*                                                                   *
//* 5724-T07: IBM z/OS Debugger                                       *
//* Copyright IBM Corp. 1997, 2024 All Rights Reserved                *
//*                                                                   *
//* US Government Users Restricted Rights - Use, duplication or       *
//* disclosure restricted by GSA ADP Schedule Contract with IBM Corp. *
//*                                                                   *
//*********************************************************************
//*                                                                   *
//*  Install the z/OS Debugger SVCs in a running z/OS facility.       *
//*                                                                   *
//*  The z/OS Debugger SVCs support the Dynamic Debug Facility and    *
//*  other necessary z/OS Debugger functions.                         *
//*                                                                   *
//*  This program will load the z/OS Debugger                         *
//*     SVC 145 EQA00SVC(IGC0014E)                                    *
//*  replacing any existing                                           *
//*     SVC 145 entry in the SVCTABLE.                                *
//*                                                                   *
//*  This program will also load the z/OS Debugger                    *
//*     Extended SVC 109 function code 51 EQA01SVC(IGX00051)          *
//*  replacing any existing                                           *
//*     Extended SVC 109/51 entry in the SVCTABLE.                    *
//*                                                                   *
//*  This program can also remove the z/OS Debugger SVC 145 and       *
//*  replace it with the system default by coding                     *
//*     PARM='DELETE' on the EXEC PGM statement                       *
//*                                                                   *
//*                                                                   *
//* Note:                                                             *
//*  EQAINSVC, EQA00SVC, EQA01SVC must reside in an authorized        *
//*  library.                                                         *
//*  The STEPLIB and LOADLIB DD statements (identified by "<<<<<")    *
//*  must be changed to specify the dataset where EQAINSVC,           *
//*  EQA00SVC and EQA01SVC are installed.                             *
//*                                                                   *
//*  STEPLIB is for EQAINSVC                                          *
//*  LOADLIB is for EQA00SVC and EQA01SVC                             *
//*                                                                   *
//*  See the Customization Guide for further details on these SVCs.   *
//*                                                                   *
//*********************************************************************
//*
//*
//*-------------------------------------------------------------------*
//* Run the program
//*-------------------------------------------------------------------*
//*
//STEP10   EXEC PGM=EQAINSVC
//STEPLIB  DD   DISP=SHR,DSN={{ eqa_hlq }}.SEQAAUTH
//LOADLIB  DD   DISP=SHR,DSN={{ eqa_hlq }}.SEQAAUTH
//SYSUDUMP DD   SYSOUT=*
//*
//*  ========> END OF JOB EQAWISVC <========
//
