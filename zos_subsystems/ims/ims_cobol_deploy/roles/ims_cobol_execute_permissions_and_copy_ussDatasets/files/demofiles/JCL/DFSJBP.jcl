//******************************************************************    00010000
//*  DFSJBP Procedure                                                   00020000
//*                                                                     00030000
//*  This procedure starts a Java non-message driven dependent          00040000
//*  region that resembles a non-message-driven BMP region.             00050000
//*                                                                     00060000
//*  The high-level qualifier of the IMS data sets is IMS.              00070000
//*  If your installation does not use this default value,              00080000
//*  then set the NODEx parameters, which correspond                    00090000
//*  to the names specified in the IMSGEN macro.                        00100000
//*                                                                     00110000
//*  The default name of the callable services library data set         00120000
//*  is SYS1.CSSLIB.                                                    00130000
//*  If your installation overrides this default value                  00140000
//*  in the CSSLIB parameter of the sysgen IMSGEN macro,                00150000
//*  then you should set the CSSLIB parameter of this procedure.        00160000
//*                                                                     00170000
//*  The default name of the System C Runtime library data set          00180000
//*  is CEE.SCEERUN.                                                    00190000
//*  If your installation overrides this default value                  00200000
//*  in the SCEERUN parameter of the sysgen IMSGEN macro,               00210000
//*  then you should set the SCEERUN parameter of this procedure.       00220000
//*                                                                     00230000
//***********************************************************@SCPYRT**  00240000
//*                                                                     00250000
//*  Licensed Materials - Property of IBM                               00260000
//*                                                                     00270000
//*  5635-A06                                                           00280000
//*                                                                     00290000
//*      Copyright IBM Corp. 2016      All Rights Reserved              00300000
//*                                                                     00310000
//*  US Government Users Restricted Rights - Use, duplication or        00320000
//*  disclosure restricted by GSA ADP Schedule contract with            00330000
//*  IBM Corp.                                                          00340000
//*                                                                     00350000
//***********************************************************@ECPYRT**  00360000
//*                                                                     00370000
//       PROC MBR=TEMPNAME,PSB=,JVMOPMAS=,OUT=,                         00380000
//            OPT=N,SPIE=0,TEST=0,DIRCA=000,                            00390000
//            STIMER=,CKPTID=,PARDLI=,                                  00400000
//            CPUTIME=,NBA=,OBA=,IMSID=,AGN=,                           00410000
//            PREINIT=,RGN=52K,SOUT=A,                                  00420000
//            SYS2=,ALTID=,APARM=,ENVIRON=,                             00430000
//            LOCKMAX=,PRLD=,SSM=,                                      00440000
//            NODE1=IMS,                                                00450000
//            NODE2=IMS                                                 00460000
//*           CSSLIB='SYS1.CSSLIB',                                     00470000
//*           SCEERUN='CEE.SCEERUN'                                     00480000
//*                                                                     00490000
//*                                                                     00500000
//JBPRGN EXEC PGM=DFSRRC00,REGION=&RGN,                                 00510000
//            PARM=(JBP,&MBR,&PSB,&JVMOPMAS,&OUT,                       00520000
//            &OPT&SPIE&TEST&DIRCA,                                     00530000
//            &STIMER,&CKPTID,&PARDLI,&CPUTIME,                         00540000
//            &NBA,&OBA,&IMSID,&AGN,                                    00550000
//            &PREINIT,&ALTID,                                          00560000
//            '&APARM',&ENVIRON,                                        00570000
//            &LOCKMAX,&PRLD,&SSM)                                      00580000
//*                                                                     00590000
//STEPLIB  DD DSN=&NODE2..&SYS2.SDFSJLIB,DISP=SHR                       00600000
//         DD DSN=&NODE2..&SYS2.SDFSRESL,DISP=SHR                       00610000
//         DD DSN=&NODE1..&SYS2.PGMLIB,DISP=SHR                         00620000
//*        DD DSN=&SCEERUN.,DISP=SHR                                    00630001
//*        DD DSN=&CSSLIB.,DISP=SHR                                     00640001
//PROCLIB  DD DSN=&NODE1..&SYS2.PROCLIB,DISP=SHR                        00650000
//SYSUDUMP DD SYSOUT=&SOUT,                                             00660000
//         DCB=(LRECL=121,RECFM=VBA,BLKSIZE=3129),                      00670000
//         SPACE=(125,(2500,100),RLSE,,ROUND)                           00680000