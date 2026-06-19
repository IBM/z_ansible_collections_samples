//EQAWRECV JOB CLASS=A,
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
//* This JCL will receive the base elements from the distribution     *
//* media for product                                                 *
//*   IBM z/OS Debugger                                               *
//*   17.0                                                            *
//*                                                                   *
//*  CAUTION: This is neither a JCL procedure nor a complete job.     *
//*  Before using this job step, you will have to make the following  *
//*  modifications:                                                   *
//*                                                                   *
//*  1) Review the SMP/E control statements before submitting         *
//*     this job.                                                     *
//*  2) Add the job parameters to meet your system requirements.      *
//*                                                                   *
//*  You can either make the changes in steps 3 and 4                 *
//*  by hand or by invoking the ISPF Editor macro EQAWEDIT,           *
//*  which you copied from the product data sets earlier.             *
//*  Consult the instructions in EQAWEDIT for more information.       *
//*                                                                   *
//*  3) Change #globalcsi to the data set name of your global         *
//*     CSI data set.                                                 *
//*                                                                   *
//*  4) Change #upldhlq to the high level qualifier(s) of the SMPMCS  *
//*     and REL files, as specified when uploading the files to the   *
//*     host (as described in the Program Directory).                 *
//*                                                                   *
//*  Notes:                                                           *
//*                                                                   *
//*  1. If #upldhlq is blank you must remove the RFPREFIX operand.    *
//*                                                                   *
//*  2. SMP/E makes copies of the relfiles and uses these as input.   *
//*     Uncomment and customize DD SMPTLIB if you want to place these *
//*     copies on a specific volume.                                  *
//*                                                                   *
//*  3. This job should complete with a return code 0.                *
//*                                                                   *
//*********************************************************************
//*       INVOKE SMP/E                                                *
//*********************************************************************
//*
//RECEIVE  EXEC PGM=GIMSMP,REGION=0M,PARM='DATE=U'
//SMPCSI   DD DISP=SHR,DSN={{ smphlq }}.CSI
//*SMPTLIB  DD  UNIT=SYSALLDA,SPACE=(TRK,(1,1)),VOL=SER=vvvvvv
//SMPHOLD  DD DUMMY
//*
//* input from data set (web download)
//*
//SMPPTFIN DD  DISP=SHR,DSN={{ zosmf_user }}.IBM.HADRH00.SMPMCS
//SMPCNTL  DD  *
    SET BDY(GLOBAL).
    RECEIVE SELECT(
                   HADRH00            /* Receive base FMID */
                  )
            RFPREFIX({{ zosmf_user }})
            SYSMODS LIST.
//*
