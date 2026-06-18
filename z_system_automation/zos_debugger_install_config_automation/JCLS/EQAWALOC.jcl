//EQAWALOC JOB CLASS=A,    
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
//*  This JCL will allocate target and distribution libraries for     *
//*  product                                                          *
//*    IBM z/OS Debugger                                              *
//*    17.0                                                           *
//*                                                                   *
//*  CAUTION: This is neither a JCL procedure nor a complete job.     *
//*  Before using this job step, you will have to make the following  *
//*  modifications:                                                   *
//*                                                                   *
//*  1) Add the job parameters to meet your system requirements.      *
//*                                                                   *
//*  You can either make the changes in steps 2 through 5 below       *
//*  by hand or by invoking the ISPF Editor macro EQAWEDIT,           *
//*  which you copied from the product data sets earlier.             *
//*  Consult the instructions in EQAWEDIT for more information.       *
//*                                                                   *
//*  2) Change #hlq to the appropriate high-level qualifier           *
//*     on the exec steps.                                            *
//*  3) Change DSP variable to the appropriate final disposition      *
//*     of the data set ("CATLG" is the default) on the exec          *
//*     steps if you choose not to use the default.                   *
//*  4) This job uses the recommended data set placement for          *
//*     the target libraries.                                         *
//*     Change #tvol to the volser for the target libraries.          *
//*  5) Change #dvol to the volser for the distribution libraries.    *
//*                                                                   *
//*    If you specify a volume for any data set in this job, you      *
//*    must also specify the same volume in the corresponding         *
//*    DDDEF entry in the DDDEF job, EQAWDDEF.                        *
//*                                                                   *
//*  Notes:                                                           *
//*                                                                   *
//*  1. This job will complete with a return code 0.  You must        *
//*     check allocation messages to verify the data sets are         *
//*     allocated and cataloged as expected.                          *
//*                                                                   *
//*  2. Run only the steps that are applicable to your installation:  *
//*                                                                   *
//*     Steps ALLOCT and ALLOCD contain all data sets as of           *
//*          Debug Tool V1.3.                                         *
//*     Steps ALCV3T and ALCV3D contain the new data sets as of       *
//*          Debug Tool V3.1.                                         *
//*     Step  ALCV4T contains the new data set as of                  *
//*          Debug Tool V4.1.                                         *
//*     Step  ALCV14D contains the new data set as of                 *
//*          z/OS Debugger V14.2.                                     *
//*                                                                   *
//*     If this is the first time you install z/OS Debugger, or you   *
//*     do not plan on reusing the data sets in prior releases,       *
//*     then you must run ALL of the allocation steps.                *
//*                                                                   *
//*  Deleted data set history:                                        *
//*       SEQACLIS, SEQADUM, SEQAIENU, SEQALPA, SEQAOS2,              *
//*       SEQAPROC, SEQA2ENU  obsolete as of Debug Tool V1.3          *
//*       SEQAPLIB, SEQAMLIB, SEQASLIB obsolete as of Debug Tool V3.1 *
//*       SEQABIN obsolete as of Debug Tool V13.1                     *
//*                                                                   *
//*       AEQACLIS, AEQAIENU, AEQAMOD2, AEQAOS2, AEQASRC2,            *
//*       AEQA2ENU  obsolete as of Debug Tool V1.3                    *
//*       AEQAPLIB, AEQAMLIB, AEQASLIB obsolete as of Debug Tool V3.1 *
//*       AEQABIN obsolete as of Debug Tool V13.1                     *
//*                                                                   *
//*  Notes:                                                           *
//*       LPALIB is no longer required as of Debug Tool V4.1.         *
//*       SEQALPA, which was obsolete as of Debug Tool V1.3, is now   *
//*       required as of Debug Tool V4.1.                             *
//*                                                                   *
//*       SEQAAUTH, SEQABMOD, SEQAMOD and AEQAMOD must be PDSEs.      *
//*       SEQALPA must NOT be a PDSE.                                 *
//*                                                                   *
//*********************************************************************
//*   Allocate Target Libraries                                       *
//*********************************************************************
//ALLOCTGT  PROC   HLQ=,
//            DSP=,
//            TVOL=
//ALLOC1    EXEC   PGM=IEFBR14
//*
//SEQAAUTH DD  DSN=&HLQ..SEQAAUTH,
//         DISP=(NEW,&DSP),
//         RECFM=U,
//         LRECL=0,
//         BLKSIZE=32760,
//         DSNTYPE=LIBRARY,
//         DSORG=PO,
//         SPACE=(TRK,(14,3)),
//         UNIT=SYSALLDA,
//         VOL=SER=&TVOL
//*
//SEQAMOD  DD  DSN=&HLQ..SEQAMOD,
//         DISP=(NEW,&DSP),
//         RECFM=U,
//         LRECL=0,
//         BLKSIZE=32760,
//         DSNTYPE=LIBRARY,
//         DSORG=PO,
//         SPACE=(TRK,(2400,240)),
//         UNIT=SYSALLDA,
//         VOL=SER=&TVOL
//*
//SEQASAMP DD  DSN=&HLQ..SEQASAMP,
//         DISP=(NEW,&DSP),
//         RECFM=FB,
//         LRECL=80,
//         BLKSIZE=0,
//         DSNTYPE=LIBRARY,
//         SPACE=(8800,(600,60,30)),
//         UNIT=SYSALLDA,
//         VOL=SER=&TVOL
//*
//SEQAEXEC DD  DSN=&HLQ..SEQAEXEC,
//         DISP=(NEW,&DSP),
//         RECFM=FB,
//         LRECL=80,
//         BLKSIZE=0,
//         DSNTYPE=LIBRARY,
//         SPACE=(8800,(800,80,15)),
//         UNIT=SYSALLDA,
//         VOL=SER=&TVOL
//*
//SEQATLIB DD  DSN=&HLQ..SEQATLIB,
//         DISP=(NEW,&DSP),
//         RECFM=FB,
//         LRECL=80,
//         BLKSIZE=0,
//         DSNTYPE=LIBRARY,
//         SPACE=(8800,(20,2,2)),
//         UNIT=SYSALLDA,
//         VOL=SER=&TVOL
//*
//EALLOC1 PEND
//*
//*********************************************************************
//*   Allocate Target Libraries new as of Debug Tool V3.1             *
//*********************************************************************
//*
//ALCV3TGT    PROC   HLQ=,
//            DSP=,
//            TVOL=
//ALLOC2    EXEC   PGM=IEFBR14
//*
//SEQABMOD DD  DSN=&HLQ..SEQABMOD,
//         DISP=(NEW,&DSP),
//         RECFM=U,
//         LRECL=0,
//         BLKSIZE=32760,
//         DSNTYPE=LIBRARY,
//         DSORG=PO,
//         SPACE=(TRK,(8,1)),
//         UNIT=SYSALLDA,
//         VOL=SER=&TVOL
//*
//SEQAMENU DD  DSN=&HLQ..SEQAMENU,
//         DISP=(NEW,&DSP),
//         RECFM=FB,
//         LRECL=80,
//         BLKSIZE=0,
//         DSNTYPE=LIBRARY,
//         SPACE=(8800,(5,1,2)),
//         UNIT=SYSALLDA,
//         VOL=SER=&TVOL
//*
//SEQAPENU DD  DSN=&HLQ..SEQAPENU,
//         DISP=(NEW,&DSP),
//         RECFM=FB,
//         LRECL=80,
//         BLKSIZE=0,
//         DSNTYPE=LIBRARY,
//         SPACE=(8800,(720,72,60)),
//         UNIT=SYSALLDA,
//         VOL=SER=&TVOL
//*
//SEQASENU DD  DSN=&HLQ..SEQASENU,
//         DISP=(NEW,&DSP),
//         RECFM=FB,
//         LRECL=80,
//         BLKSIZE=0,
//         DSNTYPE=LIBRARY,
//         SPACE=(8800,(44,4,5)),
//         UNIT=SYSALLDA,
//         VOL=SER=&TVOL
//*
//SEQAMENP DD  DSN=&HLQ..SEQAMENP,
//         DISP=(NEW,&DSP),
//         RECFM=FB,
//         LRECL=80,
//         BLKSIZE=0,
//         DSNTYPE=LIBRARY,
//         SPACE=(8800,(5,1,2)),
//         UNIT=SYSALLDA,
//         VOL=SER=&TVOL
//*
//SEQAPENP DD  DSN=&HLQ..SEQAPENP,
//         DISP=(NEW,&DSP),
//         RECFM=FB,
//         LRECL=80,
//         BLKSIZE=0,
//         DSNTYPE=LIBRARY,
//         SPACE=(8800,(720,72,60)),
//         UNIT=SYSALLDA,
//         VOL=SER=&TVOL
//*
//SEQASENP DD  DSN=&HLQ..SEQASENP,
//         DISP=(NEW,&DSP),
//         RECFM=FB,
//         LRECL=80,
//         BLKSIZE=0,
//         DSNTYPE=LIBRARY,
//         SPACE=(8800,(44,4,5)),
//         UNIT=SYSALLDA,
//         VOL=SER=&TVOL
//*
//EALLOC2 PEND
//*
//*********************************************************************
//*   Allocate Distribution Libraries                                 *
//*********************************************************************
//*
//ALLOCDLB  PROC   HLQ=,
//            DSP=,
//            DVOL=
//ALLOC3    EXEC   PGM=IEFBR14
//*
//AEQAMOD  DD  DSN=&HLQ..AEQAMOD,
//         DISP=(NEW,&DSP),
//         RECFM=U,
//         LRECL=0,
//         BLKSIZE=32760,
//         DSNTYPE=LIBRARY,
//         DSORG=PO,
//         SPACE=(TRK,(2400,240)),
//         UNIT=SYSALLDA,
//         VOL=SER=&DVOL
//*
//AEQASAMP DD  DSN=&HLQ..AEQASAMP,
//         DISP=(NEW,&DSP),
//         RECFM=FB,
//         LRECL=80,
//         BLKSIZE=0,
//         DSNTYPE=LIBRARY,
//         SPACE=(8800,(600,60,30)),
//         UNIT=SYSALLDA,
//         VOL=SER=&DVOL
//*
//AEQAEXEC DD  DSN=&HLQ..AEQAEXEC,
//         DISP=(NEW,&DSP),
//         RECFM=FB,
//         LRECL=80,
//         BLKSIZE=0,
//         DSNTYPE=LIBRARY,
//         SPACE=(8800,(800,80,15)),
//         UNIT=SYSALLDA,
//         VOL=SER=&DVOL
//*
//AEQATLIB DD  DSN=&HLQ..AEQATLIB,
//         DISP=(NEW,&DSP),
//         RECFM=FB,
//         LRECL=80,
//         BLKSIZE=0,
//         DSNTYPE=LIBRARY,
//         SPACE=(8800,(20,2,2)),
//         UNIT=SYSALLDA,
//         VOL=SER=&DVOL
//*
//EALLOC3 PEND
//*
//*********************************************************************
//*   Allocate Distribution Libraries new as of Debug Tool V3.1       *
//*********************************************************************
//*
//ALCV3DLB    PROC   HLQ=,
//            DSP=,
//            DVOL=
//ALLOC4    EXEC   PGM=IEFBR14
//*
//AEQAMENU DD  DSN=&HLQ..AEQAMENU,
//         DISP=(NEW,&DSP),
//         RECFM=FB,
//         LRECL=80,
//         BLKSIZE=0,
//         DSNTYPE=LIBRARY,
//         SPACE=(8800,(5,1,2)),
//         UNIT=SYSALLDA,
//         VOL=SER=&DVOL
//*
//AEQAPENU DD  DSN=&HLQ..AEQAPENU,
//         DISP=(NEW,&DSP),
//         RECFM=FB,
//         LRECL=80,
//         BLKSIZE=0,
//         DSNTYPE=LIBRARY,
//         SPACE=(8800,(720,72,60)),
//         UNIT=SYSALLDA,
//         VOL=SER=&DVOL
//*
//AEQASENU DD  DSN=&HLQ..AEQASENU,
//         DISP=(NEW,&DSP),
//         RECFM=FB,
//         LRECL=80,
//         BLKSIZE=0,
//         DSNTYPE=LIBRARY,
//         SPACE=(8800,(44,4,5)),
//         UNIT=SYSALLDA,
//         VOL=SER=&DVOL
//*
//AEQAMENP DD  DSN=&HLQ..AEQAMENP,
//         DISP=(NEW,&DSP),
//         RECFM=FB,
//         LRECL=80,
//         BLKSIZE=0,
//         DSNTYPE=LIBRARY,
//         SPACE=(8800,(5,1,2)),
//         UNIT=SYSALLDA,
//         VOL=SER=&DVOL
//*
//AEQAPENP DD  DSN=&HLQ..AEQAPENP,
//         DISP=(NEW,&DSP),
//         RECFM=FB,
//         LRECL=80,
//         BLKSIZE=0,
//         DSNTYPE=LIBRARY,
//         SPACE=(8800,(720,72,60)),
//         UNIT=SYSALLDA,
//         VOL=SER=&DVOL
//*
//AEQASENP DD  DSN=&HLQ..AEQASENP,
//         DISP=(NEW,&DSP),
//         RECFM=FB,
//         LRECL=80,
//         BLKSIZE=0,
//         DSNTYPE=LIBRARY,
//         SPACE=(8800,(44,4,5)),
//         UNIT=SYSALLDA,
//         VOL=SER=&DVOL
//*
//EALLOC4 PEND
//*
//*********************************************************************
//*   Allocate Target Library new as of Debug Tool V4.1               *
//*********************************************************************
//*
//ALCV4TGT    PROC   HLQ=,
//            DSP=,
//            TVOL=
//ALLOC5    EXEC   PGM=IEFBR14
//*
//SEQALPA  DD  DSN=&HLQ..SEQALPA,
//         DISP=(NEW,&DSP),
//         RECFM=U,
//         LRECL=0,
//         BLKSIZE=32760,
//         SPACE=(6144,(20,5,2)),
//         UNIT=SYSALLDA,
//         VOL=SER=&TVOL
//*
//EALLOC5 PEND
//*
//*********************************************************************
//*   Allocate Distribution Library new as of z/OS Debugger V14.2     *
//*********************************************************************
//*
//ALCVEDLB    PROC   HLQ=,
//            DSP=,
//            DVOL=
//ALLOC6    EXEC   PGM=IEFBR14
//*
//AEQAZFS  DD  DSN=&HLQ..AEQAZFS,
//         DISP=(NEW,&DSP),
//         RECFM=VB,
//         LRECL=6995,
//         BLKSIZE=0,
//         DSNTYPE=LIBRARY,
//         SPACE=(TRK,(2200,220,2)),
//         UNIT=SYSALLDA,
//         VOL=SER=&DVOL
//*
//EALLOC6 PEND
//*
//*********************************************************************
//*  The following steps execute the PROCs to allocate the            *
//*  data sets that were needed in prior releases, and are still      *
//*  required as of Debug Tool V4.1.                                  *
//*  Remove these steps if the data sets in prior releases already    *
//*  exist with proper space.                                         *
//*                                                                   *
//*  If this is the first time you install z/OS Debugger, or you do   *
//*  not plan on reusing the data sets in prior releases, then you    *
//*  must run ALL of the allocation steps below.                      *
//*********************************************************************
//*
//ALLOCT EXEC ALLOCTGT,
//            HLQ={{ eqa_hlq }},  * No default; high-level qualifier
//            DSP=CATLG,    * CATLG is the default
//            TVOL={{ tvol }}   * No default; volume for target libs
//*
//ALLOCD EXEC ALLOCDLB,
//            HLQ={{ eqa_hlq }},  * No default; high-level qualifier
//            DSP=CATLG,    * CATLG is the default
//            DVOL={{ dvol }}   * No default; volume for dist. libs
//*
//ALCV3T   EXEC ALCV3TGT,   * New target data sets for z/OS Debugger
//            HLQ={{ eqa_hlq }},  * No default; high-level qualifier
//            DSP=CATLG,    * CATLG is the default
//            TVOL={{ tvol }}   * No default; volume for target libs
//*
//ALCV3D   EXEC ALCV3DLB,   * New DLIB data sets for z/OS Debugger
//            HLQ={{ eqa_hlq }},  * No default; high-level qualifier
//            DSP=CATLG,    * CATLG is the default
//            DVOL={{ dvol }}   * No default; volume for dist. libs
//*
//*********************************************************************
//*   The following step executes the PROC to allocate the            *
//*   new data set required as of Debug Tool V4.1.                    *
//*********************************************************************
//*
//ALCV4T   EXEC ALCV4TGT,   * New target data set for z/OS Debugger
//            HLQ={{ eqa_hlq }},  * No default; high-level qualifier
//            DSP=CATLG,    * CATLG is the default
//            TVOL={{ tvol }}   * No default; volume for target libs
//*
//*********************************************************************
//*   The following step executes the PROC to allocate the            *
//*   new data set required as of  z/OS Debugger V14.2.               *
//*********************************************************************
//*
//ALCV14D   EXEC ALCVEDLB,   * New DLIB data set for z/OS Debugger
//            HLQ={{ eqa_hlq }},  * No default; high-level qualifier
//            DSP=CATLG,    * CATLG is the default
//            DVOL={{ dvol }}   * No default; volume for dist. libs
//*
