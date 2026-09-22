//EQAWAPLY JOB CLASS=A,
//     MSGCLASS=A,REGION=0M,MSGLEVEL=(1,1),NOTIFY=&SYSUID
//*********************************************************************
//* Licensed Materials - Property of IBM                              *
//* 5724-T07                                                          *
//* Copyright IBM Corp. 1997, 2024 All Rights Reserved                *
//*                                                                   *
//* US Government Users Restricted Rights - Use, duplication or       *
//* disclosure restricted by GSA ADP Schedule Contract with IBM Corp. *
//*                                                                   *
//*********************************************************************
//*                                                                   *
//*  This JCL will apply the base elements to the target libraries    *
//*  for product                                                      *
//*  IBM z/OS Debugger                                                *
//*  17.0                                                             *
//*                                                                   *
//*  CAUTION: This is neither a JCL procedure nor a complete job.     *
//*  Before using this job step, you will have to make the following  *
//*  modifications:                                                   *
//*                                                                   *
//*  1) Review the SMP/E control statements before submitting         *
//*     this job.                                                     *
//*  2) Add the job parameters to meet your system requirements.      *
//*                                                                   *
//*  You can either make the changes in steps 3 and 4 below           *
//*  by hand or by invoking the ISPF Editor macro EQAWEDIT,           *
//*  which you copied from the product data sets earlier.             *
//*  Consult the instructions in EQAWEDIT for more information.       *
//*                                                                   *
//*  3) Change #globalcsi to the data set name of your global         *
//*     CSI data set.                                                 *
//*  4) Change #tzone to the name of your target zone.                *
//*  5) The CHECK operand has been specified on the APPLY             *
//*     statement to identify possible conflicts in the               *
//*     target zone.                                                  *
//*                                                                   *
//*     After a successful APPLY CHECK run, remove the CHECK          *
//*     operand and run the job again to update the target            *
//*     libraries.                                                    *
//*                                                                   *
//*  Notes:                                                           *
//*                                                                   *
//*  1. This job will complete with a return code 0.                  *
//*                                                                   *
//*********************************************************************
//*        INVOKE SMP/E                                               *
//*********************************************************************
//APPLY   EXEC PGM=GIMSMP,PARM='DATE=U',REGION=0M
//SMPCSI   DD  DISP=SHR,
//             DSN={{ smphlq }}.CSI
//SMPHOLD  DD  DUMMY
//SMPCNTL  DD  *
    SET BDY(TARGET).                /* Set to TARGET zone        */
    APPLY  SELECT(
                  HADRH00           /* APPLY this base FMID      */
                  )
    GROUPEXTEND                     /* Also all requisite PTFs   */
    FORFMID(HADRH00)                /* For base FMID             */
    BYPASS(HOLDSYS,HOLDUSER,        /* Bypass options            */
    HOLDCLASS(UCLREL,ERREL)).
/*
//
