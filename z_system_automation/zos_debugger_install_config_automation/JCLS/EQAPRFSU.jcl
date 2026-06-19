//EQAPRFSU JOB 5724-T07,MSGLEVEL=(1,1),MSGCLASS=A
//*********************************************************************
//* Licensed Materials - Property of IBM                              *
//*                                                                   *
//* 5724-T07: IBM z/OS Debugger                                       *
//* Copyright IBM Corp. 2019, 2024 All Rights Reserved                *
//*                                                                   *
//* US Government Users Restricted Rights - Use, duplication or       *
//* disclosure restricted by GSA ADP Schedule Contract with IBM Corp. *
//*                                                                   *
//*                                                                   *
//* This JCL creates the directories and populates them with samples  *
//* needed for the configuration of the Debug Profile Service.        *
//*                                                                   *
//*                                                                   *
//* Refer to the bottom of this comment block for details on what     *
//* this JCL creates and copies.                                      *
//*                                                                   *
//*                                                                   *
//* CAUTIONS:                                                         *
//* A) This job contains case sensitive path statements.              *
//* B) This is neither a JCL procedure nor a complete job.            *
//*    Before using this JCL, you will have to make the following     *
//*    modifications:                                                 *
//*                                                                   *
//* 1) Add the job parameters to meet your system requirements.       *
//*                                                                   *
//* 2) Provide, in variable SVRPATH, the home directory of the        *
//*    product install (default is /usr/lpp/IBM/debug).               *
//*                                                                   *
//* 3) Provide, in variable CFGDIR, the home directory for the        *
//*    customized configuration files (default is /etc/debug).        *
//*                                                                   *
//* 4) Provide, in variable WRKDIR, the home directory for the        *
//*    logs and temporary work files (default is /var/debug).         *
//*                                                                   *
//* 5) Provide, in variable JAVADIR, the home directory of the        *
//*    Java SDK (default is /usr/lpp/java/J8.0_64).                   *
//*                                                                   *
//* 6) Provide, in variable STOREPW, the keystore password for        *
//*    generating the keystore sample (default is liberty).           *
//*                                                                   *
//* Note(s):                                                          *
//*                                                                   *
//* 1. THE USER ID THAT RUNS THIS JOB MUST HAVE SUFFICIENT z/OS UNIX  *
//*    AUTHORITY.                                                     *
//*                                                                   *
//* 2. When using existing directories, for example /etc, note that   *
//*    these directories are expected to have at least a permission   *
//*    bit mask that allows read/execute for everyone (eg. mask 755). *
//*    The user ID assigned to this job must have write permission.   *
//*                                                                   *
//* 3. If a target file already exists while copying the z/OS UNIX    *
//*    samples, the original file will be copied to                   *
//*    $FILE.backup.by.EQAPRFSU before replacing it by the sample.    *
//*                                                                   *
//* 4. The following directory, which is created by this job, must    *
//*    have a 777 permission bit mask (everyone has read, write and   *
//*    execute access).                                               *
//*    * $WRKDIR                                                      *
//*      default /var/debug                                           *
//*                                                                   *
//* 5. This job should complete with a return code 0.                 *
//*                                                                   *
//*********************************************************************
//*                                                                   *
//* JOB DETAILS                                                       *
//*                                                                   *
//*********************************************************************
//*                                                                   *
//* The JOB will execute a shell script that creates z/OS UNIX        *
//* directories (based upon the variables defined in the SET          *
//* statements), copies samples (from $SVRPATH/samples) and sets      *
//* permission bits for directories. All files are created with       *
//* permission bitmask 755 (read/write/execute for owner,             *
//* read/execute for group and other).                                *
//*                                                                   *
//*    * $CFGDIR                                                      *
//*      default: /etc/debug                                          *
//*      permission bitmask: 755                                      *
//*      files: eqaprof.env                            (from samples) *
//*             dtcn.ports                             (from samples) *
//*             keystore.p12                           (generated)    *
//*             eqadrest.env                           (from samples) *
//*                                                                   *
//*    * $WRKDIR                                                      *
//*      default: /var/debug                                          *
//*      permission bitmask: 777 (non-standard)                       *
//*                                                                   *
//*********************************************************************
//*                                                                   *
//* MIGRATION NOTES                                                   *
//*                                                                   *
//*********************************************************************
//*                                                                   *
//* The shell script will create a backup, <file>.backup.by.EQAPRFSU, *
//* if a target file already exists.                                  *
//*                                                                   *
//*********************************************************************
//*
//EXPORTS  EXPORT SYMLIST=*
//         SET SVRPATH='{{ eqa_path_prefix }}usr/lpp/IBM/debug'
//         SET CFGDIR='{{ CFGDIR }}'
//         SET WRKDIR='{{ WRKDIR }}'
//         SET JAVADIR='{{ JAVADIR }}'
//         SET STOREPW='liberty'
//*
//* Execute $SVRPATH/bin/eqaprfsu.sh
//*
//CONFIG   EXEC PGM=BPXBATSL,REGION=0M,TIME=NOLIMIT,
//  PARM='PGM /bin/sh &SVRPATH/bin/eqaprfsu.sh'
//STDOUT   DD SYSOUT=*
//STDERR   DD SYSOUT=*
//STDENV   DD *,SYMBOLS=JCLONLY
SVRPATH=&SVRPATH
CFGDIR=&CFGDIR
WRKDIR=&WRKDIR
JAVADIR=&JAVADIR
STOREPASS=&STOREPW
/*
//*
