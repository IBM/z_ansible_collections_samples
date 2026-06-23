//EQAWZFS  JOB CLASS=A,
//    MSGCLASS=A,REGION=0M,MSGLEVEL=(1,1),NOTIFY=&SYSUID
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
//*  This JCL will create a z/OS UNIX file system, create a z/OS UNIX *
//*  mount point and mount the file system for product                *
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
//*  You can either make the changes in steps 2 through 6 below       *
//*  by hand or by invoking the ISPF Editor macro EQAWEDIT,           *
//*  which you copied from the product data sets earlier.             *
//*  Consult the instructions in EQAWEDIT for more information.       *
//*                                                                   *
//* 2) Choose the file system type, zFS or HFS (zFS is the default).  *
//*    IBM recommends to use zFS file systems.                        *
//*    When using HFS, comment out all zFS specific steps (ZFSTYPE,   *
//*    ZFSALLOC and ZFSFORMT) and uncomment the HFS specific steps    *
//*    (HFSTYPE and HFSALLOC).                                        *
//*                                                                   *
//* 3) Change the string "#fsdsn" to the appropriate data set name    *
//*    for the file system that will be created.                      *
//*    This must be done twice when creating a zFS file system. Once  *
//*    in the SET DSN statement and once in the SYSIN DD of the       *
//*    ZFSALLOC step.                                                 *
//*                                                                   *
//* 4) When creating a ZFS file system, change the string "#fsvol"    *
//*    in the SYSIN DD of the ALLOCZFS step to the appropriate volume *
//*    for the file system that will be created.                      *
//*                                                                   *
//* 5) Change the string "-PathPrefix-" to the appropriate            *
//*    high level directory name. For users installing in the root,   *
//*    this would be "/". For others, the high level directory may    *
//*    be something like "/service/", or a more meaningful name.      *
//*                                                                   *
//*    Please note that the replacement string is case sensitive.     *
//*                                                                   *
//* 6a) If you are APPLYing this function for the first time, change  *
//*    #dsprefix to the value specified for DSPREFIX in the OPTIONS   *
//*    entry of the GLOBAL zone.                                      *
//*    If you used the optional EQAWSMPE job to define the CSI,       *
//*    the #dsprefix value will match the CSI high level qualifier.   *
//*                                                                   *
//* 6b) If you are running this job to install service after the      *
//*    product has been APPLYed, the EQAWMNT EXEC will reside in      *
//*    a target library, so uncomment the second SYSEXEC statement    *
//*    and comment out the first one.                                 *
//*                                                                   *
//*    Change #hlq to the high level qualifier of the target library, *
//*    as used in the EQAWALOC job.                                   *
//*                                                                   *
//*    Change #tvol to the volser of the target library, as used in   *
//*    the EQAWALOC job.                                              *
//*                                                                   *
//* Note(s):                                                          *
//*                                                                   *
//* 1. Ensure that -PathPrefix- is an absolute path name and begins   *
//*    and ends with a slash (/).                                     *
//*                                                                   *
//* 2. The directory specified by -PathPrefix- will be created by the *
//*    job if it does not exist.                                      *
//*                                                                   *
//* 3. The combined length of the text in single quotes (') of the    *
//*    SET DSN and SET DIR statements may not exceed 84 characters.   *
//*                                                                   *
//* 4. Ensure you execute this job with a userid that is UID=0, or    *
//*    that is permitted to the 'BPX.SUPERUSER' profile in the        *
//*    FACILITY security class.                                       *
//*                                                                   *
//* 5. The ZFS started task needs READ access to the file system when *
//*    mounting a zFS file system. Lacking this permission will       *
//*    result in errno=79 errnojr=EF096055 for the mount command.     *
//*                                                                   *
//* 6. You should consider updating the BPXPRMxx PARMLIB member to    *
//*    mount the file system created with this job at IPL time.       *
//*                                                                   *
//*    MOUNT FILESYSTEM('#fsdsn')                                     *
//*       MOUNTPOINT('-PathPrefix-usr/lpp/IBM/debug')                 *
//*       MODE(RDWR)                 /* can be MODE(READ) */          *
//*       TYPE(ZFS) PARM('AGGRGROW') /* zFS, with extents */          *
//*    /* TYPE(HFS) */               /* HFS, auto. extent */          *
//*                                                                   *
//* 7. This job should complete with a return code 0.                 *
//*                                                                   *
//*********************************************************************
//*
//         SET DSN='{{ eqa_hlq }}.ZFS'
//*                 ----+----1----+----2----+----3----+----4----+----5
//         SET DIR='{{ eqa_path_prefix }}usr/lpp/IBM/debug'
//*
//* zFS
//*
//ZFSTYPE  SET TYPE=ZFS
//ZFSALLOC EXEC PGM=IDCAMS,REGION=0M,COND=(4,LT)
//SYSPRINT DD SYSOUT=*
//SYSIN    DD *
   DEFINE CLUSTER( -
            NAME({{ eqa_hlq }}.ZFS) -
            VOLUME({{ tvol }}) -
            LINEAR -
            TRACKS(7000,900) -
            SHAREOPTIONS(3) -
                 )
//*
//ZFSFORMT EXEC PGM=IOEAGFMT,REGION=0M,COND=(4,LT),
//            PARM='-aggregate &DSN -compat'
//*STEPLIB  DD DISP=SHR,DSN=IOE.SIOELMOD        before z/OS 1.13
//*STEPLIB  DD DISP=SHR,DSN=SYS1.SIEALNKE       from z/OS 1.13
//SYSPRINT DD SYSOUT=*
//*
//* HFS
//*
//*//HFSTYPE  SET TYPE=HFS
//*//HFSALLOC EXEC PGM=IEFBR14
//*//FILESYS  DD DISP=(NEW,CATLG,DELETE),
//*//            SPACE=(TRK,(7000,900)),
//*//            UNIT=SYSALLDA,
//*//*           VOL=SER=#fsvol,
//*//            BLKSIZE=0,LRECL=0,RECFM=U,DSORG=PO,
//*//            DSNTYPE=HFS,DSN=&DSN
//*
//* MOUNT
//*
//EXIST    EXEC PGM=IKJEFT01,REGION=0M,COND=(4,LT),
//            PARM='LISTDS ''&DSN'' HISTORY'
//SYSTSPRT DD SYSOUT=*
//SYSTSIN  DD DUMMY
//*
//MOUNT    EXEC PGM=IKJEFT1A,REGION=0M,COND=(4,LT),
//            PARM='%EQAWMNT &TYPE &DSN &DIR'
//SYSEXEC  DD DISP=SHR,DSN={{ zosmf_user }}.IBM.HADRH00.F1
//*
//*SYSEXEC  DD DISP=SHR,
//*            UNIT=SYSALLDA,
//*            VOL=SER=#tvol,
//*            DSN=#hlq.SEQASAMP
//*
//SYSTSPRT DD SYSOUT=*
//SYSTSIN  DD DUMMY
//*
