//*********************************************************************
//* Licensed Materials - Property of IBM                              *
//*                                                                   *
//* 5724-T07: IBM z/OS Debugger                                       *
//* Copyright IBM Corp. 2020, 2024 All Rights Reserved                *
//*                                                                   *
//* US Government Users Restricted Rights - Use, duplication or       *
//* disclosure restricted by GSA ADP Schedule Contract with IBM Corp. *
//*                                                                   *
//*                                                                   *
//* This JCL procedure is used to start the Remote Debug Service.     *
//*                                                                   *
//* You will have to make the following modifications:                *
//*                                                                   *
//* 1) If you installed the product in a different directory than     *
//*    the default /u/ibmuser, change SVRPATH to refer to     *
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
//*    to a different file name than the default eqarmtd.env, change  *
//*    ENVFILE to refer to the correct name.                          *
//*                                                                   *
//* Note(s):                                                          *
//*                                                                   *
//* 1. This procedure contains case sensitive path statements.        *
//*                                                                   *
//* 2. Add a job card on line 1 and '//EQARMTD EXEC EQARMTD' at the   *
//*    bottom to submit this procedure as a user job.                 *
//*                                                                   *
//*********************************************************************
//*
//* Remote Debug Service
//*
//EQARMTD  PROC SVRPATH='{{ eqa_path_prefix }}usr/lpp/IBM/debug',
//         CFGDIR='{{ CFGDIR }}',
//         WRKDIR='{{ WRKDIR }}',
//         ENVFILE='eqarmtd.env'
//         SET QUOTE=''''
//EXPORTS  EXPORT SYMLIST=*
//         SET SVRPATH=&QUOTE&SVRPATH&QUOTE
//         SET CFGDIR=&QUOTE&CFGDIR&QUOTE
//         SET WRKDIR=&QUOTE&WRKDIR&QUOTE
//         SET ENVFILE=&QUOTE&ENVFILE&QUOTE
//*---------------------------------------------------------
//* Start the Remote Debug Service
//*---------------------------------------------------------
//EQARMTD EXEC PGM=BPXBATSL,REGION=0M,TIME=NOLIMIT,
//  PARM='PGM /bin/sh &SVRPATH/remote-debug-service/bin/eqarmtd.sh'
//STDOUT   DD SYSOUT=*
//STDERR   DD SYSOUT=*
//STDENV   DD *,SYMBOLS=JCLONLY
EQARMTD_CFG_DIR=&CFGDIR
EQARMTD_WRK_DIR=&WRKDIR
EQARMTD_ENVFILE=&CFGDIR/&ENVFILE
EQARMTD_BASE=&SVRPATH/remote-debug-service
_BPXK_AUTOCVT=ON
/*
//        PEND
//*
