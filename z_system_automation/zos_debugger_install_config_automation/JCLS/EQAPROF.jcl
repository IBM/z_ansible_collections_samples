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
//* This JCL procedure is used to start the Debug Profile Service.    *
//*                                                                   *
//* You will have to make the following modifications:                *
//*                                                                   *
//* 1) If you installed the product in a different directory than     *
//*    the default /usr/lpp/IBM/debug, change SVRPATH to refer to     *
//*    the correct directory.                                         *
//*                                                                   *
//* 2) If you customized the configuration files in a different       *
//*    directory than the default /etc/debug, change CFGDIR to        *
//*    refer to the correct directory.                                *
//*                                                                   *
//* 3) If you want the server to use a different work directory       *
//*    then the default /var/debug, change WRKDIR to refer to the     *
//*    desired directory.                                             *
//*                                                                   *
//* 4) If you customized the environment variables file in CFGDIR     *
//*    to a different file name than the default eqaprof.env, change  *
//*    ENVFILE to refer to the correct name.                          *
//*                                                                   *
//* 5) Change EQAHLQ to meet your installation requirements for       *
//*    SEQAMOD.                                                       *
//*                                                                   *
//* 6) Change DFHHLQ to meet your installation requirements for       *
//*    SDFHEXCI.                                                      *
//*                                                                   *
//* 7) If you customized the environment variables file in CFGDIR     *
//*    to a different file name than the default eqadrest.env, change *
//*    DATAFILE to refer to the correct name.                         *
//*                                                                   *
//* Note(s):                                                          *
//*                                                                   *
//* 1. This procedure contains case sensitive path statements.        *
//*                                                                   *
//* 2. Add a job card on line 1 and '//EQAPROF EXEC EQAPROF' at the   *
//*    bottom to submit this procedure as a user job.                 *
//*                                                                   *
//*********************************************************************
//*
//* Debug Profile Service
//*
//EQAPROF  PROC SVRPATH='{{ eqa_path_prefix }}usr/lpp/IBM/debug',
//         CFGDIR='{{ CFGDIR }}',
//         WRKDIR='{{ WRKDIR }}',
//         ENVFILE='eqaprof.env',
//         DATAFILE='eqadrest.env',
//         EQAHLQ='{{ eqa_hlq }}',
//         DFHHLQ='{{ DFHHLQ }}',
//         TZ='EST5EDT'
//         SET QUOTE=''''
//EXPORTS  EXPORT SYMLIST=*
//         SET SVRPATH=&QUOTE&SVRPATH&QUOTE
//         SET CFGDIR=&QUOTE&CFGDIR&QUOTE
//         SET WRKDIR=&QUOTE&WRKDIR&QUOTE
//         SET ENVFILE=&QUOTE&ENVFILE&QUOTE
//         SET DATAFILE=&QUOTE&DATAFILE&QUOTE
//         SET TZ=&QUOTE&TZ&QUOTE
//*---------------------------------------------------------
//* Start the Debug Profile Service
//*---------------------------------------------------------
//EQAPROF EXEC PGM=BPXBATCH,REGION=0M,TIME=NOLIMIT,MEMLIMIT=2G,
//  PARM='PGM /bin/sh &SVRPATH/servers/eqaProfile/bin/start.sh'
//STEPLIB  DD DSN=&EQAHLQ..SEQAMOD,DISP=SHR
//         DD DSN=&DFHHLQ..SDFHEXCI,DISP=SHR
//STDOUT   DD SYSOUT=*
//STDERR   DD SYSOUT=*
//STDENV   DD *,SYMBOLS=JCLONLY
TZ=&TZ
EQAPROF_CFG_DIR=&CFGDIR
EQAPROF_WRK_DIR=&WRKDIR
EQAPROF_ENVFILE=&CFGDIR/&ENVFILE
EQAPROF_DATAFILE=&CFGDIR/&DATAFILE
_BPXK_AUTOCVT=ON
/*
//        PEND
//*
