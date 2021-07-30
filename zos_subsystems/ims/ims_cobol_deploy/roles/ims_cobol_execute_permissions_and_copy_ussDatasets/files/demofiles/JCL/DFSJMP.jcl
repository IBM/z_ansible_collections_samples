//******************************************************************    00010000
//*  DFSJMP Procedure                                                   00020000
//*                                                                     00030000
//*  This procedure starts a Java message-driven dependent              00040000
//*  region that resembles an MPP region.                               00050000
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
//       PROC SOUT=A,RGN=52K,SYS2=,                                     00380000
//            CL1=001,CL2=000,CL3=000,CL4=000,                          00390000
//            OPT=N,OVLA=0,SPIE=0,VALCK=0,TLIM=00,                      00400000
//            PCB=000,STIMER=,SOD=,                                     00410000
//            NBA=,OBA=,IMSID=,AGN=,                                    00420000
//            PREINIT=,ALTID=,PWFI=N,APARM=,                            00430000
//            LOCKMAX=,ENVIRON=,                                        00440000
//            JVMOPMAS=,PRLD=,SSM=,PARDLI=,                             00450000
//            MINTHRD=000,MAXTHRD=256,                                  00460000
//            NODE1=IMS,                                                00470000
//            NODE2=IMS                                                 00480000
//*           CSSLIB='SYS1.CSSLIB',                                     00490000
//*           SCEERUN='CEE.SCEERUN'                                     00500000
//*                                                                     00510000
//JMPRGN EXEC PGM=DFSRRC00,REGION=&RGN,                                 00520000
//            TIME=1440,DPRTY=(12,0),                                   00530000
//            PARM=(JMP,&CL1&CL2&CL3&CL4,                               00540000
//            &OPT&OVLA&SPIE&VALCK&TLIM&PCB,                            00550000
//            &STIMER,&SOD,&NBA,                                        00560000
//            &OBA,&IMSID,&AGN,&PREINIT,                                00570000
//            &ALTID,&PWFI,'&APARM',&LOCKMAX,                           00580000
//            &ENVIRON,,&JVMOPMAS,&PRLD,                                00590000
//            &SSM,&PARDLI,                                             00600000
//            &MINTHRD,&MAXTHRD)                                        00610000
//*                                                                     00620000
//*                                                                     00630000
//STEPLIB  DD DSN=&NODE1..&SYS2.DEMO.PGMLIB,DISP=SHR                    00640007
//         DD DSN=&NODE2..&SYS2.SDFSJLIB,DISP=SHR                       00650000
//         DD DSN=&NODE2..&SYS2.SDFSRESL,DISP=SHR                       00660000
//         DD DSN=DFS.V15R1M0.SDFSRESL,DISP=SHR                         00661003
//         DD DSN=DSN.V12R1M0.SDSNEXIT,DISP=SHR                         00662003
//         DD DSN=DSN.V12R1M0.SDSNLOD2,DISP=SHR                         00663003
//         DD DSN=DSN.V12R1M0.SDSNLOAD,DISP=SHR                         00670003
//*        DD DSN=&CSSLIB.,DISP=SHR                                     00680001
//PROCLIB  DD DSN=&NODE1..&SYS2.PROCLIB,DISP=SHR                        00690000
//******** EXTERNAL SUBSYSTEM STATEMENTS ************                   00691003
//*                                                                     00692003
//* USER MAY OPTIONALLY ADD THE DFSESL DD CARD                          00693003
//* FOR EXTERNAL SUBSYSTEM CONNECTION.                                  00694003
//*                                                                     00695003
//DFSESL   DD DSN=&NODE2..&SYS2.SDFSRESL,DISP=SHR                       00696006
//         DD DSN=DSN.V12R1M0.SDSNEXIT,DISP=SHR                         00696105
//         DD DSN=DSN.V12R1M0.SDSNLOD2,DISP=SHR                         00697003
//         DD DSN=DSN.V12R1M0.SDSNLOAD,DISP=SHR                         00698003
//*FSDB2AF DD DSN=DSN.V12R1M0.SDSNEXIT,DISP=SHR                         00699005
//*        DD DSN=DSN.V12R1M0.SDSNLOD2,DISP=SHR                         00699105
//*        DD DSN=DSN.V12R1M0.SDSNLOAD,DISP=SHR                         00699205
//SYSUDUMP DD SYSOUT=&SOUT,                                             00700000
//         DCB=(LRECL=121,BLKSIZE=3129,RECFM=VBA),                      00710000
//         SPACE=(125,(2500,100),RLSE,,ROUND)                           00720000
//STDERR   DD SYSOUT=&SOUT                                              00730002
//STDOUT   DD SYSOUT=&SOUT                                              00740002
//SYSPRINT DD SYSOUT=&SOUT                                              00750002