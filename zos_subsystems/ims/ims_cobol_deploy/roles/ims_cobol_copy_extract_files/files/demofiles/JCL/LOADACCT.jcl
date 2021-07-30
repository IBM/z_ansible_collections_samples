//LOADACCT JOB  REGION=0M,MSGLEVEL=(1,1),MSGCLASS=R,NOTIFY=&SYSUID
//*******************************************************************
//*  ALLOCATE ACCOUNT DB DATASET
//*******************************************************************
//ALLOC   EXEC PGM=IEFBR14
//SYSPRINT  DD SYSOUT=*
//A1        DD DSN=IMS1510.DEMO.ACCOUNT.DB,
//*            VOL=SER=LP0204,
//             STORCLAS=CLASS1,
//             SPACE=(CYL,(5,1)),UNIT=SYSDA,DISP=(,CATLG),
//             DCB=(RECFM=F,LRECL=8192,BLKSIZE=8192)
/*
//*******************************************************************
//*  PREREORG (ACCOUNT)
//*******************************************************************
//PREREORG EXEC PGM=DFSRRC00,
//        PARM='ULU,DFSURPR0,,,,,,,,,,,,N,N',REGION=1M
//STEPLIB  DD DSN=DFS.V15R1M0.SDFSRESL,DISP=SHR
//DFSRESLB DD DSN=DFS.V15R1M0.SDFSRESL,DISP=SHR
//IMS      DD DSN=IMS1510.DBDLIB,DISP=SHR
//DFSURCDS DD DSN=&&CDS,UNIT=SYSDA,DISP=(,PASS),
//            SPACE=(TRK,1),DCB=BLKSIZE=1600
//SYSPRINT DD SYSOUT=A
//SYSUDUMP DD SYSOUT=A
//SYSIN    DD *
DBIL=ACCOUNT
/*
//*******************************************************************
//*  LOAD DB (ACCOUNT)
//*******************************************************************
//LOADSTEP EXEC PGM=DFSRRC00,
//        PARM='DLI,LOADACCT,IBLOAD,,,,,,,,,,,N,N',REGION=0M
//STEPLIB  DD DSN=IMS1510.DEMO.PGMLIB,DISP=SHR
//         DD DSN=DFS.V15R1M0.SDFSRESL,DISP=SHR
//DFSRESLB DD DSN=DFS.V15R1M0.SDFSRESL,DISP=SHR
//IMS      DD DSN=IMS1510.PSBLIB,DISP=SHR
//         DD DSN=IMS1510.DBDLIB,DISP=SHR
//PRINTDD  DD SYSOUT=A,DCB=BLKSIZE=1200
//ACCTIN   DD DISP=SHR,DSN=IMS1510.DEMO.ACCOUNT.INPUT
//SYSABEND DD SYSOUT=A
//SYSPRINT DD SYSOUT=A
//DFSURCDS DD DSN=&&CDS,DISP=(OLD,PASS)
//DFSVSAMP DD *
IOBF=(2048,20000,Y,Y,OA2K)
IOBF=(2048,20000,Y,Y,OB2K)
IOBF=(2048,20000,Y,Y,OC2K)
IOBF=(2048,20000,Y,Y,OD2K)
IOBF=(4096,4000,Y,Y,O04K)
IOBF=(8192,20000,Y,Y,O08K)
IOBF=(12288,20000,Y,Y,O12K)
//IEFRDER  DD DUMMY
//ACCOUNT  DD DISP=SHR,DSN=IMS1510.DEMO.ACCOUNT.DB
//PUNCHDD  DD SYSOUT=B
//
//
//
//
//
//
//*******************************************************************
//*  SCRATCH AND UNCATALOG ACCOUNT DB DATASET
//*******************************************************************
//SCRTCH EXEC PGM=IEHPROGM
//SYSPRINT DD SYSOUT=*
//LP0204 DD VOL=SER=LP0204,UNIT=SYSDA,DISP=SHR
//SYSIN DD *
 SCRATCH DSNAME=IMS1510.DEMO.ACCOUNT.DB,                               -
               VOL=SYSDA=LP0204,                                       -
               PURGE
 UNCATLG DSNAME=IMS1510.DEMO.ACCOUNT.DB

/*