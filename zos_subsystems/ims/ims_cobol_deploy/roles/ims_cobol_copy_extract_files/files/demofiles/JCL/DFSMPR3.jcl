//******************************************************************    00010000
//*  DFSMPR Procedure                                                   00020000
//*                                                                     00030000
//*  This procedure is an online execution procedure that               00040000
//*  initiates an IMS message processing address space.                 00050000
//*                                                                     00060000
//*  The high-level qualifier of the IMS data sets is IMS.              00070000
//*  If your installation does not use this default value,              00080000
//*  then set the NODEx parameters, which correspond                    00090000
//*  to the names specified in the IMSGEN macro.                        00100000
//*                                                                     00110000
//***********************************************************@SCPYRT**  00120000
//*                                                                     00130000
//*  Licensed Materials - Property of IBM                               00140000
//*                                                                     00150000
//*  5635-A06                                                           00160000
//*                                                                     00170000
//*      Copyright IBM Corp. 2016      All Rights Reserved              00180000
//*                                                                     00190000
//*  US Government Users Restricted Rights - Use, duplication or        00200000
//*  disclosure restricted by GSA ADP Schedule contract with            00210000
//*  IBM Corp.                                                          00220000
//*                                                                     00230000
//***********************************************************@ECPYRT**  00240000
//*                                                                     00250000
//DFSMPR PROC SOUT=A,RGN=52K,SYS2=,                                     00260000
//            CL1=007,CL2=000,CL3=000,CL4=000,                          00270001
//            OPT=N,OVLA=0,SPIE=0,VALCK=0,TLIM=00,                      00280000
//            PCB=000,PRLD=,STIMER=,SOD=,DBLDL=,                        00290000
//            NBA=,OBA=,IMSID=,AGN=,VSFX=,VFREE=,                       00300000
//            PREINIT=,ALTID=,PWFI=N,                                   00310002
//            APARM=,LOCKMAX=,APPLFE=,PARDLI=,                          00320001
//            ENVIRON=,JVMOPMAS=,SSM=,                                  00330002
//            NODE1=IMS,                                                00340000
//            NODE2=IMS                                                 00350000
//*                                                                     00360000
//REGION EXEC PGM=DFSRRC00,REGION=&RGN,                                 00370000
//            TIME=1440,DPRTY=(12,0),                                   00380000
//            PARM=(MSG,&CL1&CL2&CL3&CL4,                               00390000
//            &OPT&OVLA&SPIE&VALCK&TLIM&PCB,                            00400000
//            &PRLD,&STIMER,&SOD,&DBLDL,&NBA,                           00410000
//            &OBA,&IMSID,&AGN,&VSFX,&VFREE,                            00420000
//            &SSM,&PREINIT,&ALTID,&PWFI,                               00430000
//            '&APARM',&LOCKMAX,&APPLFE,                                00440000
//            &ENVIRON,&JVMOPMAS,&PARDLI)                               00450000
//*                                                                     00460000
//STEPLIB  DD DSN=&NODE1..&SYS2.DEMO.PGMLIB,DISP=SHR                    00461103
//         DD DSN=&NODE2..&SYS2.SDFSJLIB,DISP=SHR                       00462003
//         DD DSN=&NODE2..&SYS2.SDFSRESL,DISP=SHR                       00463003
//         DD DSN=DFS.V15R1M0.SDFSRESL,DISP=SHR                         00464003
//         DD DSN=DSN.V12R1M0.SDSNEXIT,DISP=SHR                         00465003
//         DD DSN=DSN.V12R1M0.SDSNLOD2,DISP=SHR                         00466003
//         DD DSN=DSN.V12R1M0.SDSNLOAD,DISP=SHR                         00467003
//PROCLIB  DD DSN=&NODE1..&SYS2.PROCLIB,DISP=SHR                        00490004
//SYSUDUMP DD SYSOUT=&SOUT,                                             00500000
//         DCB=(LRECL=121,BLKSIZE=3129,RECFM=VBA),                      00510000
//         SPACE=(125,(2500,100),RLSE,,ROUND)                           00520000
//CEEOPTS  DD  *                                                        00530001
  ALL31(ON),                                                            00540001
  ANYHEAP(2097152,1048576,ANYWHERE,KEEP),                               00550001
  BELOWHEAP(8192,4096,FREE),                                            00560001
  CBLPSHPOP(ON),                                                        00570001
  CBLQDA(OFF),                                                          00580001
  CEEDUMP(60,SYSOUT=*,FREE=END,SPIN=UNALLOC),                           00590001
  CHECK(OFF),                                                           00600001
  COUNTRY(US),                                                          00610001
  NODEBUG,                                                              00620001
  DEPTHCONDLMT(4),                                                      00630001
  HEAP(83886080,4194304,ANYWHERE,KEEP,1048576,524288),                  00640001
  HEAPCHK(OFF,1,0,0,0,1024,0,1024,0),                                   00650001
  HEAPPOOLS(ON,8,10,32,10,128,10,256,10,1024,10,2048,10,0,10,0,10,0,    00660001
  10,0,10,0,10,0,10),                                                   00670001
  STACK(65536,65536,ANYWHERE,KEEP,524288,262144),                       00680001
  STORAGE(00,NONE,NONE,8192),                                           00690001
  THREADSTACK(OFF,65536,16384,ANYWHERE,KEEP,131072,131072),             00700001
  TRAP(ON,SPIE)                                                         00710001
/*                                                                      00710101
//DFSESL   DD DSN=&NODE2..&SYS2.SDFSRESL,DISP=SHR                       00711001
//         DD DSN=DSN.V12R1M0.SDSNEXIT,DISP=SHR                         00712001
//         DD DSN=DSN.V12R1M0.SDSNLOD2,DISP=SHR                         00713001
//         DD DSN=DSN.V12R1M0.SDSNLOAD,DISP=SHR                         00714001
//STDOUT  DD SYSOUT=*                                                   00780001
//STDERR  DD SYSOUT=*                                                   00790001
//SYSPRINT DD SYSOUT=&SOUT                                              00791001
//*                                                                     00800001