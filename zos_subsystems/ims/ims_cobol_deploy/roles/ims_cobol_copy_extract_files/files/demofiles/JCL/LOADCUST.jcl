//LOADCUST JOB  REGION=0M,MSGLEVEL=(1,1),MSGCLASS=R,NOTIFY=&SYSUID
//*******************************************************************
//*  ALLOCATE CUSTOMER DB DATASET
//*******************************************************************
//ALLOC   EXEC PGM=IEFBR14
//SYSPRINT  DD SYSOUT=*
//A1        DD DSN=IMS1510.DEMO.CUSTOMER.DB,
//*            VOL=SER=S32$7D,
//             STORCLAS=CLASS1,
//             SPACE=(CYL,(5,1)),UNIT=SYSDA,DISP=(,CATLG),
//             DCB=(RECFM=F,LRECL=8192,BLKSIZE=8192)
/*
//*******************************************************************
//*  PREREORG (CUSTOMER)
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
DBIL=CUSTOMER
/*
//*******************************************************************
//*  LOAD DB (CUSTOMER)
//*******************************************************************
//LOADSTEP EXEC PGM=DFSRRC00,
//        PARM='DLI,LOADCUST,IBLOAD,,,,,,,,,,,N,N',REGION=0M
//STEPLIB  DD DSN=IMS1510.DEMO.PGMLIB,DISP=SHR
//         DD DSN=DFS.V15R1M0.SDFSRESL,DISP=SHR
//DFSRESLB DD DSN=DFS.V15R1M0.SDFSRESL,DISP=SHR
//IMS      DD DSN=IMS1510.PSBLIB,DISP=SHR
//         DD DSN=IMS1510.DBDLIB,DISP=SHR
//PRINTDD  DD SYSOUT=A,DCB=BLKSIZE=1200
//CUSTIN   DD DISP=SHR,DSN=IMS1510.DEMO.CUSTOMER.INPUT
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
//CUSTOMER  DD DISP=SHR,DSN=IMS1510.DEMO.CUSTOMER.DB
//PUNCHDD  DD SYSOUT=B
//
//
//
//
//*******************************************************************
//*  SCRATCH AND UNCATALOG CUSTOMER DB DATASET
//*******************************************************************
//SCRTCH EXEC PGM=IEHPROGM
//SYSPRINT DD SYSOUT=*
//S32$7D DD VOL=SER=S32$7D,UNIT=SYSDA,DISP=SHR
//SYSIN DD *
 SCRATCH DSNAME=IMS1510.DEMO.CUSTOMER.DB,                              -
               VOL=SYSDA=S32$7D,                                       -
               PURGE
 UNCATLG DSNAME=IMS1510.DEMO.CUSTOMER.DB

/*