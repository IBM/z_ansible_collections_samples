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
//            CL1=005,CL2=000,CL3=000,CL4=000,                          00270000
//            OPT=N,OVLA=0,SPIE=0,VALCK=0,TLIM=00,                      00280000
//            PCB=000,PRLD=,STIMER=,SOD=,DBLDL=,                        00290000
//            NBA=,OBA=,IMSID=,AGN=,VSFX=,VFREE=,                       00300000
//            SSM=,PREINIT=,ALTID=,PWFI=N,                              00310000
//            APARM=,LOCKMAX=,APPLFE=,                                  00320000
//            ENVIRON=,JVMOPMAS=,PARDLI=,                               00330000
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
//STEPLIB  DD DSN=&NODE1..&SYS2.DEMO.PGMLIB,DISP=SHR                    00470000
//         DD DSN=&NODE2..&SYS2.SDFSRESL,DISP=SHR                       00481000
//PROCLIB  DD DSN=&NODE2..&SYS2.PROCLIB,DISP=SHR                        00490000
//SYSUDUMP DD SYSOUT=&SOUT,                                             00500000
//         DCB=(LRECL=121,BLKSIZE=3129,RECFM=VBA),                      00510000
//         SPACE=(125,(2500,100),RLSE,,ROUND)                           00520000
//CEEOPTS  DD *                                                         00521002
TEST(ERROR,*,PROMPT,TCPIP&localhost%8020:*)                             00522002
/*                                                                      00523002
//SYSPRINT DD SYSOUT=*                                                  00530000
//                                                                      00531001
//                                                                      00532001
//                                                                      00533001
//CEEOPTS  DD *                                                         00540001
TEST(ERROR,*,PROMPT,TCPIP&localhost%8020:*)                             00550001
/*                                                                      00560001