//EQAWMKD  JOB CLASS=A,
//     MSGCLASS=A,REGION=0M,MSGLEVEL=(1,1),NOTIFY=&SYSUID
//*
//*********************************************************************
//* Licensed Materials - Property of IBM                              *
//* 5724-T07                                                          *
//* Copyright IBM Corp. 2019, 2024 All Rights Reserved                *
//*                                                                   *
//* US Government Users Restricted Rights - Use, duplication or       *
//* disclosure restricted by GSA ADP Schedule Contract with IBM Corp. *
//*                                                                   *
//*********************************************************************
//*                                                                   *
//*  This JCL will create directories for product                     *
//*    IBM z/OS Debugger                                              *
//*    17.0                                                           *
//*                                                                   *
//* CAUTIONS:                                                         *
//* A) This job contains case sensitive path statements.              *
//* B) This is neither a JCL procedure nor a complete job.            *
//*    Before using this JCL, you will have to make the following     *
//*    modifications:                                                 *
//*                                                                   *
//* 1) Add the job parameters to meet your system requirements.       *
//*                                                                   *
//*  You can either make the changes in steps 2 through 3 below       *
//*  by hand or by invoking the ISPF Editor macro EQAWEDIT,           *
//*  which you copied from the product data sets earlier.             *
//*  Consult the instructions in EQAWEDIT for more information.       *
//*                                                                   *
//* 2a) If you are APPLYing this function for the first time, change  *
//*    #dsprefix to the value specified for DSPREFIX in the OPTIONS   *
//*    entry of the GLOBAL zone.                                      *
//*    If you used the optional EQAWSMPE job to define the CSI,       *
//*    the #dsprefix value will match the CSI high level qualifier.   *
//*                                                                   *
//* 2b) If you are running this job to install service after the      *
//*    product has been APPLYed, the EQAWMKDR EXEC will reside in     *
//*    a target library, so uncomment the second SYSEXEC statement    *
//*    and comment out the first one.                                 *
//*                                                                   *
//*    Change #hlq to the high level qualifier of the target library, *
//*    as used in the EQAWALOC job.                                   *
//*                                                                   *
//*    Change #tvol to the volser of the target library, as used in   *
//*    the EQAWALOC job.                                              *
//*                                                                   *
//* 3) Change the string "-PathPrefix-" to the appropriate            *
//*    high level directory name. For users installing in the root,   *
//*    this would be "/". For others, the high level directory may    *
//*    be something like "/service/", or a more meaningful name.      *
//*                                                                   *
//*    Please note that the replacement string is case sensitive.     *
//*                                                                   *
//* 4) If desired, change the SET DIR statement to use a different    *
//*    path to install this product.                                  *
//*                                                                   *
//*    If you change the statement to use the 'plugin' directory of   *
//*    (pre-requisite product) z/OS Explorer, then z/OS Explorer can  *
//*    automatically detect the presence of this product.             *
//*    A sample definition to use the 'plugin' directory is provided  *
//*    as comment.                                                    *
//*                                                                   *
//* Note(s):                                                          *
//*                                                                   *
//* 1. Ensure that -PathPrefix- is an absolute path name and begins   *
//*    and ends with a slash (/).                                     *
//*                                                                   *
//* 2. Ensure the directory specified by -PathPrefix- exists prior    *
//*    to running this job.                                           *
//*                                                                   *
//* 3. If you used the optional EQAWZFS job to mount a file system,   *
//*    keep -PathPrefix- and DIR the same as specified there.         *
//*                                                                   *
//* 3. Ensure you execute this job with a userid that is UID=0, or    *
//*    that is permitted to the 'BPX.SUPERUSER' profile in the        *
//*    FACILITY security class.                                       *
//*                                                                   *
//* 4. This job should complete with a return code 0.                 *
//*    If not, check the output, consult the z/OS UNIX System         *
//*    Services Messages and Codes manual to correct the problem,     *
//*    and resubmit this job.                                         *
//*                                                                   *
//*********************************************************************
//         EXPORT SYMLIST=DIR
//*
//         SET DIR='usr/lpp/IBM/debug'
//*
//MKDIR    EXEC PGM=IKJEFT1A,COND=(4,LT)
//SYSEXEC  DD DISP=SHR,DSN={{ smphlq }}.HADRH00.F1
//*
//*SYSEXEC  DD DISP=SHR,
//*            UNIT=SYSALLDA,
//*            VOL=SER=#tvol,
//*            DSN=#hlq.SEQASAMP
//*
//SYSTSPRT DD SYSOUT=*
//SYSTSIN  DD *,SYMBOLS=JCLONLY
PROFILE MSGID
EQAWMKDR {{ eqa_path_prefix }} &DIR
//*
