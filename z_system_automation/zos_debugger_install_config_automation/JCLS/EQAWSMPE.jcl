//EQAWSMPE JOB CLASS=A,
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
//*  This JCL will delete an old SMP/E environment, and then          *
//*  define and prime a new SMP/E environment for product             *
//*    IBM z/OS Debugger                                              *
//*    17.0                                                           *
//*                                                                   *
//*  CAUTION: This is neither a JCL procedure nor a complete job.     *
//*  Before using this job step, you will have to make the following  *
//*  modifications:                                                   *
//*                                                                   *
//*  1) This job is optional. Use it only if you want to install      *
//*     this product into its own SMP/E zones.                        *
//*  2) Add the job parameters to meet your system requirements.      *
//*                                                                   *
//*  You can either make the changes in steps 3 through 7 below       *
//*  by hand or by invoking the ISPF Editor macro EQAWEDIT,           *
//*  which you copied from the product data sets earlier.             *
//*  Consult the instructions in EQAWEDIT for more information.       *
//*                                                                   *
//*  3) Change #smphlq to the SMP/E high-level qualifier for your     *
//*     SMP/E zones and data sets.                                    *
//*  4) Change #volser to a valid volume serial on which your SMP/E   *
//*     zones and data sets will be allocated.                        *
//*  5) Change #tzone to the name of your target zone.                *
//*     (For example, 'TZONE')                                        *
//*  6) Change #dzone to the name of your distribution zone.          *
//*     (For example, 'DZONE')                                        *
//*  7) SYSALLDA is the default unit identification. Change           *
//*     this, if required, to the appropriate unit value.             *
//*                                                                   *
//*  Notes:                                                           *
//*                                                                   *
//*  1. This job contains 2 steps: IDCAMS and SMPE.                   *
//*     If you are running in a JES3 environment, split this job      *
//*     into two. That is, create a separate job for each step.       *
//*     Then submit the IDCAMS job first. When the IDCAMS job         *
//*     completes, submit the SMPE job.                               *
//*  2. Each step should complete with a return code 0.               *
//*                                                                   *
//*********************************************************************
//*
//* CAUTION: Check carefully the DELETE commands below to ensure that
//*          you do not delete the existing SMP/E CSI and data sets
//*          that might still be used by others.
//*
//IDCAMS  EXEC PGM=IDCAMS,REGION=1M
//SYSPRINT DD  SYSOUT=*
//ZPOOL    DD  DSN=SYS1.MACLIB(GIMZPOOL),DISP=SHR
//SYSIN    DD  *
   DELETE {{ smphlq }}.CSI           CLUSTER
   DELETE {{ smphlq }}.SMPLOG        NONVSAM
   DELETE {{ smphlq }}.SMPLOGA       NONVSAM
   DELETE {{ smphlq }}.SMPLTS        NONVSAM
   DELETE {{ smphlq }}.SMPMTS        NONVSAM
   DELETE {{ smphlq }}.SMPPTS        NONVSAM
   DELETE {{ smphlq }}.SMPSCDS       NONVSAM
   DELETE {{ smphlq }}.SMPSTS        NONVSAM
   SET MAXCC=0

   DEFINE CLUSTER(                                  -
                  NAME({{ smphlq }}.CSI)                 -
                  FREESPACE(10,5)                   -
                  KEYS(24 0)                        -
                  RECORDSIZE(24 143)                -
                  SHAREOPTIONS(2 3)                 -
                  UNIQUE                            -
                  VOLUMES({{ volser }})                  -
                 )                                  -
             DATA(                                  -
                  NAME({{ smphlq }}.CSI.DATA)            -
                  CONTROLINTERVALSIZE(4096)         -
                  CYLINDERS(10 5)                   -
                 )                                  -
            INDEX(                                  -
                  NAME({{ smphlq }}.CSI.INDEX)           -
                  CYLINDERS(1 1)                    -
                  IMBED                             -
                 )

   REPRO INFILE(ZPOOL) ODS({{ smphlq }}.CSI)
/*
//*
//SMPE    EXEC PGM=GIMSMP,REGION=0M
//SYSPRINT DD  SYSOUT=*
//SMPLIST  DD  SYSOUT=*
//SMPOUT   DD  SYSOUT=*
//SMPRPT   DD  SYSOUT=*
//SMPSNAP  DD  SYSOUT=*
//SMPCSI   DD  DISP=OLD,DSN={{ smphlq }}.CSI
//**************************************************************/
//*              SMP/E DATA SETS ALLOCATION                    */
//**************************************************************/
//*
//SMPLOG   DD DSN={{ smphlq }}.SMPLOG,DISP=(NEW,CATLG),
//            UNIT=SYSALLDA,VOL=SER={{ volser }},
//            DCB=(RECFM=VB,LRECL=260,BLKSIZE=0),
//            SPACE=(3200,(6000,3000),,,ROUND)
//SMPLOGA  DD DSN={{ smphlq }}.SMPLOGA,DISP=(NEW,CATLG),
//            UNIT=SYSALLDA,VOL=SER={{ volser }},
//            DCB=(RECFM=VB,LRECL=260,BLKSIZE=0),
//            SPACE=(3200,(6000,3000),,,ROUND)
//SMPLTS   DD DSN={{ smphlq }}.SMPLTS,DISP=(NEW,CATLG),
//            UNIT=SYSALLDA,VOL=SER={{ volser }},
//            DCB=(RECFM=U,LRECL=0,BLKSIZE=32760),
//            SPACE=(32760,(1700,1000),,,ROUND),
//            DSORG=PO,DSNTYPE=LIBRARY
//SMPMTS   DD DSN={{ smphlq }}.SMPMTS,DISP=(NEW,CATLG),
//            UNIT=SYSALLDA,VOL=SER={{ volser }},
//            DCB=(RECFM=FB,LRECL=80,BLKSIZE=0),
//            SPACE=(3120,(60,20,60),,,ROUND)
//SMPPTS   DD DSN={{ smphlq }}.SMPPTS,DISP=(NEW,CATLG),
//            UNIT=SYSALLDA,VOL=SER={{ volser }},
//            DCB=(RECFM=FB,LRECL=80,BLKSIZE=0),
//            SPACE=(3120,(40000,20000,300),,,ROUND)
//SMPSCDS  DD DSN={{ smphlq }}.SMPSCDS,DISP=(NEW,CATLG),
//            UNIT=SYSALLDA,VOL=SER={{ volser }},
//            DCB=(RECFM=FB,LRECL=80,BLKSIZE=0),
//            SPACE=(3120,(400,100,300),,,ROUND)
//SMPSTS   DD DSN={{ smphlq }}.SMPSTS,DISP=(NEW,CATLG),
//            UNIT=SYSALLDA,VOL=SER={{ volser }},
//            DCB=(RECFM=FB,LRECL=80,BLKSIZE=0),
//            SPACE=(3120,(400,100,150),,,ROUND)
//SMPWRK1  DD UNIT=SYSALLDA,SPACE=(CYL,(2,1,10)),DCB=BLKSIZE=6160
//SMPWRK2  DD UNIT=SYSALLDA,SPACE=(CYL,(2,1,10)),DCB=BLKSIZE=6160
//SMPWRK3  DD UNIT=SYSALLDA,SPACE=(CYL,(2,1,10)),DCB=BLKSIZE=3200
//SMPWRK4  DD UNIT=SYSALLDA,SPACE=(CYL,(2,1,10)),DCB=BLKSIZE=3200
//SMPWRK6  DD UNIT=SYSALLDA,SPACE=(CYL,(2,1,10)),DCB=BLKSIZE=3200
//SYSUT1   DD UNIT=SYSALLDA,SPACE=(CYL,(2,1))
//SYSUT2   DD UNIT=SYSALLDA,SPACE=(CYL,(2,1))
//SYSUT3   DD UNIT=SYSALLDA,SPACE=(CYL,(2,1))
//SYSUT4   DD UNIT=SYSALLDA,SPACE=(CYL,(2,5))
//*
//**************************************************************/
//*              SMP/E ZONES INITIALIZATION                    */
//**************************************************************/
//*
//SMPCNTL   DD *
    SET    BDY(GLOBAL).            /* SET TO GLOBAL ZONE         */
    UCLIN.
    ADD    GLOBALZONE              /* DEFINE GLOBAL ZONE NOW     */
           OPTIONS(GLBOPT)         /* DEFINE AN OPTIONS ENTRY    */
           SREL(Z038)              /* MVS                        */
           ZONEINDEX(              /* ZONES TO BE SET UP         */
     (TARGET,{{ smphlq }}.CSI,TARGET),  /* TARGET ZONE           */
     (DLIB,{{ smphlq }}.CSI,DLIB))    /* DISTRIBUTION ZONE       */
           .                       /*                            */
    ADD    OPTIONS(GLBOPT)         /* ADD AN OPTIONS ENTRY       */
           ASM(ASMUTIL)            /* SMP ASSEMBLER UTILITY NAME */
           LKED(LINKEDIT)          /* SMP LINK EDIT UTILITY NAME */
           COPY(IEBCOPY)           /* SMP COPY UTILITY NAME      */
           DSPREFIX({{ smphlq }})       /* TLIB DSN PREFIX */
           DSSPACE(300,150,250)    /* SPACE FOR TLIB DATA SETS   */
           MSGFILTER(YES)          /* DISPLAY ONLY IMPORTANT MSGS*/
           MSGWIDTH(80)            /* FORMAT MSGS TO 80-CHAR     */
           RETRYDDN(ALL)           /* COMPRESS AND RETRY         */
           .                       /*                            */
    ADD    UTILITY(ASMUTIL)        /* ASSEMBLER UTILITY ENTRY    */
           NAME(ASMA90)            /* ASMA90 IS HIGH-LEVEL ASM   */
           RC(4)                   /* RETURN CODE THRESHOLD      */
      PARM(DECK,NOOBJECT,USING(WARN(2))) /* PARAMETER OVERRIDE   */
           .                       /*                            */
    ADD    UTILITY(LINKEDIT)       /* LINK EDIT UTILITY ENTRY    */
           NAME(IEWL)              /* NAME OF LINKAGE EDITOR     */
           RC(4)                   /* RETURN CODE THRESHOLD      */
           PRINT(SYSPRINT)         /* DDNAME FOR SYSPRINT OUTPUT */
      PARM(SIZE=(1024K,32K),NCAL,LET,LIST,XREF) /* SIZE OVERRIDE */
           .                       /*                            */
    ADD    UTILITY(IEBCOPY)        /* COPY UTILITY ENTRY         */
           NAME(IEBCOPY)           /* NAME OF COPY PROGRAM       */
           RC(0)                   /* RETURN CODE THRESHOLD      */
           .                       /*                            */
    ADD DDDEF(SMPOUT)   SYSOUT(*).
    ADD DDDEF(SMPRPT)   SYSOUT(*).
    ADD DDDEF(SMPLIST)  SYSOUT(*).
    ADD DDDEF(SYSPRINT) SYSOUT(*).
    ADD DDDEF(SMPSNAP)  SYSOUT(*).
    ADD DDDEF(SMPLOG)
        DA({{ smphlq }}.SMPLOG)
        UNIT(SYSALLDA)
        WAITFORDSN
        MOD.
    ADD DDDEF(SMPLOGA)
        DA({{ smphlq }}.SMPLOGA)
        UNIT(SYSALLDA)
        WAITFORDSN
        MOD.
    ADD DDDEF(SMPPTS)
        DA({{ smphlq }}.SMPPTS)
        UNIT(SYSALLDA)
        WAITFORDSN
        SHR.
    ADD DDDEF(SMPTLIB)
        VOLUME({{ volser }})
        UNIT(SYSALLDA).
    ADD DDDEF(SYSUT1)   CYL SPACE(10,10) UNIT(SYSALLDA).
    ADD DDDEF(SYSUT2)   CYL SPACE(10,10) UNIT(SYSALLDA).
    ADD DDDEF(SYSUT3)   CYL SPACE(10,10) UNIT(SYSALLDA).
    ADD DDDEF(SYSUT4)   CYL SPACE(10,10) UNIT(SYSALLDA).
    ENDUCL .

    SET    BDY(TARGET).            /* SET TO TARGET ZONE         */
    UCLIN.
    ADD    TARGETZONE(TARGET)      /* DEFINE TARGET ZONE         */
           RELATED(DLIB)         /* DISTRIBUTION LIBRARY       */
           OPTIONS(GLBOPT)         /* DEFINE AN OPTIONS ENTRY    */
           SREL(Z038)              /* MVS                        */
           .                       /*                            */
    ADD DDDEF(SMPOUT)   SYSOUT(*).
    ADD DDDEF(SMPRPT)   SYSOUT(*).
    ADD DDDEF(SMPLIST)  SYSOUT(*).
    ADD DDDEF(SYSPRINT) SYSOUT(*).
    ADD DDDEF(SMPSNAP)  SYSOUT(*).
    ADD DDDEF(SMPLOG)
        DA({{ smphlq }}.SMPLOG)
        UNIT(SYSALLDA)
        WAITFORDSN
        MOD.
    ADD DDDEF(SMPLOGA)
        DA({{ smphlq }}.SMPLOGA)
        UNIT(SYSALLDA)
        WAITFORDSN
        MOD.
    ADD DDDEF(SMPLTS)
        DA({{ smphlq }}.SMPLTS)
        UNIT(SYSALLDA)
        WAITFORDSN
        OLD.
    ADD DDDEF(SMPMTS)
        DA({{ smphlq }}.SMPMTS)
        UNIT(SYSALLDA)
        WAITFORDSN
        OLD.
    ADD DDDEF(SMPPTS)
        DA({{ smphlq }}.SMPPTS)
        UNIT(SYSALLDA)
        WAITFORDSN
        SHR.
    ADD DDDEF(SMPSCDS)
        DA({{ smphlq }}.SMPSCDS)
        UNIT(SYSALLDA)
        WAITFORDSN
        OLD.
    ADD DDDEF(SMPSTS)
        DA({{ smphlq }}.SMPSTS)
        UNIT(SYSALLDA)
        WAITFORDSN
        OLD.
    ADD DDDEF(SYSLIB)     /* ASSIGN DDDEF TO SYSLIB CONCATENATION */
        CONCAT(SMPMTS     /* MUST BE FIRST */
              ).
    ADD DDDEF(SYSUT1)   CYL SPACE(100,200) UNIT(SYSALLDA).
    ADD DDDEF(SYSUT2)   CYL SPACE(10,10) UNIT(SYSALLDA).
    ADD DDDEF(SYSUT3)   CYL SPACE(10,10) UNIT(SYSALLDA).
    ADD DDDEF(SYSUT4)   CYL SPACE(10,10) UNIT(SYSALLDA).
    ADD DDDEF(SMPWRK1)  CYL SPACE(30,10) DIR(100) UNIT(SYSALLDA).
    ADD DDDEF(SMPWRK2)  CYL SPACE(30,10) DIR(100) UNIT(SYSALLDA).
    ADD DDDEF(SMPWRK3)  CYL SPACE(30,10) DIR(200) UNIT(SYSALLDA).
    ADD DDDEF(SMPWRK4)  CYL SPACE(30,10) DIR(100) UNIT(SYSALLDA).
    ADD DDDEF(SMPWRK6)  CYL SPACE(100,200) DIR(100) UNIT(SYSALLDA).
    ENDUCL .

    SET    BDY(DLIB).            /* SET TO DISTRIBUTION ZONE   */
    UCLIN.
    ADD    DLIBZONE(DLIB)        /* DEFINE DISTRIBUTION ZONE   */
           RELATED(TARGET)         /* TARGET LIBRARY             */
           OPTIONS(GLBOPT)         /* DEFINE AN OPTIONS ENTRY    */
           SREL(Z038)              /* MVS                        */
           .                       /*                            */
    ADD DDDEF(SMPOUT)   SYSOUT(*).
    ADD DDDEF(SMPRPT)   SYSOUT(*).
    ADD DDDEF(SMPLIST)  SYSOUT(*).
    ADD DDDEF(SYSPRINT) SYSOUT(*).
    ADD DDDEF(SMPSNAP)  SYSOUT(*).
    ADD DDDEF(SMPLOG)
        DA({{ smphlq }}.SMPLOG)
        UNIT(SYSALLDA)
        WAITFORDSN
        MOD.
    ADD DDDEF(SMPLOGA)
        DA({{ smphlq }}.SMPLOGA)
        UNIT(SYSALLDA)
        WAITFORDSN
        MOD.
    ADD DDDEF(SMPLTS)
        DA({{ smphlq }}.SMPLTS)
        UNIT(SYSALLDA)
        WAITFORDSN
        OLD.
    ADD DDDEF(SMPMTS)
        DA({{ smphlq }}.SMPMTS)
        UNIT(SYSALLDA)
        WAITFORDSN
        OLD.
    ADD DDDEF(SMPPTS)
        DA({{ smphlq }}.SMPPTS)
        UNIT(SYSALLDA)
        WAITFORDSN
        SHR.
    ADD DDDEF(SMPSCDS)
        DA({{ smphlq }}.SMPSCDS)
        UNIT(SYSALLDA)
        WAITFORDSN
        OLD.
    ADD DDDEF(SMPSTS)
        DA({{ smphlq }}.SMPSTS)
        UNIT(SYSALLDA)
        WAITFORDSN
        OLD.
    ADD DDDEF(SYSLIB)     /* ASSIGN DDDEF TO SYSLIB CONCATENATION */
        CONCAT(SMPMTS     /* MUST BE FIRST */
              ).
    ADD DDDEF(SYSUT1)   CYL SPACE(60,20) UNIT(SYSALLDA).
    ADD DDDEF(SYSUT2)   CYL SPACE(10,10) UNIT(SYSALLDA).
    ADD DDDEF(SYSUT3)   CYL SPACE(10,10) UNIT(SYSALLDA).
    ADD DDDEF(SYSUT4)   CYL SPACE(10,10) UNIT(SYSALLDA).
    ADD DDDEF(SMPWRK1)  CYL SPACE(30,10) DIR(100) UNIT(SYSALLDA).
    ADD DDDEF(SMPWRK2)  CYL SPACE(30,10) DIR(100) UNIT(SYSALLDA).
    ADD DDDEF(SMPWRK3)  CYL SPACE(30,10) DIR(200) UNIT(SYSALLDA).
    ADD DDDEF(SMPWRK4)  CYL SPACE(30,10) DIR(100) UNIT(SYSALLDA).
    ADD DDDEF(SMPWRK6)  CYL SPACE(60,20) DIR(100) UNIT(SYSALLDA).
    ENDUCL .
/*
//
