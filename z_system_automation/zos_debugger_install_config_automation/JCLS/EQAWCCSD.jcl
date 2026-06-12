//EQAWCCSD JOB 5724-T07,MSGLEVEL=(1,1),MSGCLASS=A
//*
//*********************************************************************
//*                                                                  */
//*  IBM z/OS Debugger                                               */
//*               17.0                                               */
//*                                                                  */
//*********************************************************************
//*                                                                   *
//* Licensed Materials - Property of IBM                              *
//*                                                                   *
//* 5724-T07: IBM z/OS Debugger                                       *
//* Copyright IBM Corp. 1997, 2024 All Rights Reserved                *
//*                                                                   *
//* US Government Users Restricted Rights - Use, duplication or       *
//* disclosure restricted by GSA ADP Schedule Contract with IBM Corp. *
//*                                                                   *
//********************************************************************
//*                                                                  *
//* EQAWCCSD JOB                                                     *
//*                                                                  *
//* This job will define all z/OS Debugger CICS programs,            *
//* transactions and transient data queues.                          *
//*                                                                  *
//* Caution: This is neither a JCL procedure nor a complete job.     *
//* Before using this job, you will have to make the following       *
//* modifications:                                                   *
//*                                                                  *
//* 1. Change the job card to meet your system requirements.         *
//* 2. Change the DFHPRFX DSN prefix value below to to meet your     *
//*    installation requirements for SDFHLOAD.                       *
//* 3. Change the CSDPRFX DSN prefix value below to meet your        *
//*    installation requirements for DFHCCSD.                        *
//* 4. Change the DTPRFX  DSN prefix value below to meet your        *
//*    installation requirements for SEQASAMP.                       *
//* 5. Change/verify the use of group name "EQA" in member EQACCSD.  *
//* 6. Change <list-name> to the appropriate CICS startup group      *
//*    list name in member EQACCSD.                                  *
//* 7. Review the TDQUEUE definitions if you use macro DCTs or       *
//*    wish to remove definitions for COBOL or PLI sidefile access.  *
//*                                                                  *
//* This job will complete with a return code of 4.                  *
//*                                                                  *
//********************************************************************
//        SET DFHPRFX={{ DFHHLQ }}
//        SET CSDPRFX={{ CSDPRFX }}
//        SET DTPRFX={{ eqa_hlq }}
//EQACICS EXEC PGM=DFHCSDUP,REGION=1024K,
//             PARM='CSD(READWRITE),PAGESIZE(60),NOCOMPAT'
//STEPLIB  DD DISP=SHR,DSN=&DFHPRFX..SDFHLOAD
//DFHCSD   DD DISP=SHR,DSN=&CSDPRFX..DFHCSD
//SYSPRINT DD SYSOUT=*
//*SYSIN    DD DISP=SHR,DSN=&DTPRFX..SEQASAMP(EQACCSD)
//SYSIN    DD *
**********************************************************************
*                                                                    *
* IBM z/OS Debugger - CICS CSD Resource Definitions                  *
*                                                                    *
*                                                                    *
* Note:    The group (EQA) that contains the z/OS Debugger run time  *
*          routines must be in the group list used during            *
*          CICS start-up.                                            *
*                                                                    *
* Note:    JCL to apply these definitions is in member EQAWCCSD      *
*          of the SEQASAMP library.                                  *
*                                                                    *
* Note:    Before applying these definitions,                        *
*          Change/verify the use of group name "EQA"                 *
*                                                                    *
* Caution: Before applying these definitions, change <list-name>     *
*          to the appropriate CICS startup group list name.          *
*                                                                    *
**********************************************************************
*                                                                    *
* Licensed Materials - Property of IBM                               *
*                                                                    *
* 5724-T07: IBM z/OS Debugger                                        *
* Copyright IBM Corp. 1992, 2024 All Rights Reserved                 *
*                                                                    *
* US Government Users Restricted Rights - Use, duplication or        *
* disclosure restricted by GSA ADP Schedule Contract with IBM Corp.  *
*                                                                    *
**********************************************************************
*                                                                    *
* XREF INFORMATION :                                                 *
*                                                                    *
* $MOD(EQACCSD) COMP(CICS)  PROD(DBGT): RDO definitions              *
*                                                                    *
* FLAG REASON   RLSE DATE   ORIGIN  : FLAG DESCRIPTION               *
* ---- -------- ---- ------ -------- ------------------------------- *
* $C3= r95754  V17R0 260318 Campbell: Add EQAVRM utility             *
* $YD= R94931  V17R0 250213 Young   : CICS Support                   *
* $A1= R90243  V16R0 241018 AGaray  : EXCI CONNECTION/SESSION        *
* $01= R94438  V17R0 240816 JG      : Remove EQA00OHM                *
* $YC= R92917  V16R0 230622 Young   : CICS Support                   *
* $YB= R88162  V15R0 220308 Young   : CICS Support                   *
* $YA= R88126  V15R0 210707 Young   : CICS Support                   *
* $K2= R85837  V14R2 200831 Kwok    : DTCN URIMAP                    *
* $K1= R84419  V14R2 200224 Kwok    : DTCN TCPIPSERVICE              *
* $Y9= R84109  V14R2 200121 Young   : CICS Support                   *
* $Y8= R79744  V14R1 180611 Young   : CICS Support                   *
* $TN= F10824  V13R1 150319 Piner   : CICS Support                   *
* $TM= W16959  V14R0 160516 Piner   : PICL Support                   *
* $TL= F10492  V13R1 150319 Piner   : CICS Support                   *
* $TK= F10055  V12R1 131029 Piner   : CICS Support                   *
* $TJ= F9938   V11R1 130814 Piner   : Add "CICS log"                 *
* $TI= F9696   V12R1 130415 Piner   : EQANCPLT depricated. Use 0C.   *
* $Y7= F9592   V12R1 121127 Young   : COBOL V5                       *
* $TH= C8935   V10R1 100331 Piner   : HANDLE ATNI abends in DTCN     *
* $Y6= D8845   V11R1 101220 Young   : Fix warning messages           *
* $TG= D8752    V9R1 101006 Piner   : Improve terminal validation    *
* $TF= F8666   V11R1 100908 Piner   : Create DTNP                    *
* $TE= F8431   V10R1 100304 Piner   : CICS Support                   *
* $TD= F8377    V9R1 100121 Piner   : DTCN profile delete service    *
* $L3= D8342   V10R1 091114 Lin     : add EXECKEY(CICS) in EQADCANO  *
* $Y5= F7922   V10R1 090622 Young   : FINDBP                         *
* $TC= D7912    V9R1 090616 Piner   : More DTCN profile manager      *
* $TB= D7912    V9R1 090603 Piner   : Define EQASCMU                 *
* $L2= F7803a   V9R1 090516 Lin     : DTCN profile manager           *
* $L1= F7803    V9R1 090425 Lin     : DTCN profile web programs      *
* $TA= F7847    V9R1 090402 Piner   : DTCN Profile Manager           *
* $T9= F7703    V9R1 090101 Piner   : Create DTSC                    *
* $Y4= F7684    V9R1 081217 Young   : C/C++ Expression               *
* $T8= C7572    V8R1 081006 Piner   : nonLE AMODE(24)                *
* $C2= F7355    V9R1 080619 Campbell: Store DTCN profiles in VSAM    *
* $T7= F7330    V9R1 080606 Piner   : CICS TS 4.1 support            *
* $T6= F7209    V9R1 080326 Piner   : CTA True and some removals     *
* $Y3= F7190a   V9R1 080318 Young   : DTU&AF -> DT                   *
* $Y2= F6942a   V8R1 071005 Young   : Add EQA00E4n modules           *
* $T5= F6613    V8R1 070606 Piner   : Split JCL into EQAWCCSD        *
* $C1= F6359b   V8R1 070410 Carter  : Add EQA00D3H                   *
* $T4= F6424    V8R1 070303 Piner   : Add DTST                       *
* $Y1= F5770D   V7R1 070302 Young   : Korean                         *
* $M1= F6154f   V7R1 070129 Manchala: Debug support for Q++ apps     *
* $T3= F5555    V7R1 060315 Piner   : Tidy up and improve            *
* $T2= F5546    V7R1 060313 Piner   : CICS TS 3.2 support            *
**********************************************************************
       DELETE GROUP(EQA)
***********************************************************************
* Program: CBCDEBUG (alias of EQARCDBG)                               *
***********************************************************************
DEFINE PROGRAM(CBCDEBUG)
       GROUP(EQA)
       DATALOC(ANY)
       CEDF(NO)
***********************************************************************
* Program: CEEEVDBG (alias of EQA00EVH)                               *
***********************************************************************
DEFINE PROGRAM(CEEEVDBG)
       GROUP(EQA)
       DATALOC(ANY)
       CEDF(NO)
***********************************************************************
* Program: CEEEV006 (alias of EQAEV006)                               *
***********************************************************************
DEFINE PROGRAM(CEEEV006)
       GROUP(EQA)
       DATALOC(ANY)
       CEDF(NO)
***********************************************************************
* Program: EQACIC32                                               @TMA*
***********************************************************************
DEFINE PROGRAM(EQACIC32)
       DESCRIPTION(Debug probe for CICS 3.2)
       GROUP(EQA)
       EXECKEY(CICS)
       DATALOC(ANY)
       CEDF(NO)
***********************************************************************
* Program: EQACIC41                                               @TMA*
***********************************************************************
DEFINE PROGRAM(EQACIC41)
       DESCRIPTION(Debug probe for CICS 4.1)
       GROUP(EQA)
       EXECKEY(CICS)
       DATALOC(ANY)
       CEDF(NO)
***********************************************************************
* Program: EQACIC51                                               @TMA*
***********************************************************************
DEFINE PROGRAM(EQACIC51)
       DESCRIPTION(Debug probe for CICS 5.1)
       GROUP(EQA)
       EXECKEY(CICS)
       DATALOC(ANY)
       CEDF(NO)
***********************************************************************
* Program: EQACIC61                                               @YBA*
***********************************************************************
DEFINE PROGRAM(EQACIC61)
       DESCRIPTION(Debug probe for CICS 6.1)
       GROUP(EQA)
       EXECKEY(CICS)
       DATALOC(ANY)
       CEDF(NO)
***********************************************************************
* Program: EQACTL                                                 @TMA*
***********************************************************************
DEFINE PROGRAM(EQACTL)
       DESCRIPTION(Debug CICS version check)
       GROUP(EQA)
       EXECKEY(CICS)
       DATALOC(ANY)
       CEDF(NO)
***********************************************************************
* Program: EQACSCTP                                                   *
***********************************************************************
DEFINE PROGRAM(EQACSCTP)
       GROUP(EQA)
       DATALOC(ANY)
       CEDF(NO)
***********************************************************************
* Program: EQACSMTP                                                   *
***********************************************************************
DEFINE PROGRAM(EQACSMTP)
       GROUP(EQA)
       DATALOC(ANY)
       CEDF(NO)
***********************************************************************
* Program: EQACSUTP                                                   *
***********************************************************************
DEFINE PROGRAM(EQACSUTP)
       GROUP(EQA)
       DATALOC(ANY)
       CEDF(NO)
***********************************************************************
* Program: EQACSXTP                                                   *
***********************************************************************
DEFINE PROGRAM(EQACSXTP)
       GROUP(EQA)
       DATALOC(ANY)
       CEDF(NO)
***********************************************************************
* Program: EQADAJPN                                                   *
***********************************************************************
DEFINE PROGRAM(EQADAJPN)
       GROUP(EQA)
       DATALOC(ANY)
       CEDF(NO)
***********************************************************************
* Program: EQADAKOR                                               @Y1A*
***********************************************************************
DEFINE PROGRAM(EQADAKOR)
       GROUP(EQA)
       DATALOC(ANY)
       CEDF(NO)
***********************************************************************
* Program: EQAAFSIN    Deprecated ... no longer used                  *
***********************************************************************
DEFINE PROGRAM(EQAAFSIN)
       GROUP(EQA)
       DATALOC(ANY)
       CEDF(NO)
***********************************************************************
* Program: EQADCCNM                                                   *
***********************************************************************
DEFINE PROGRAM(EQADCCNM)
       GROUP(EQA)
       DATALOC(ANY)
       CEDF(NO)
***********************************************************************
* Program: EQADCCXR                                                   *
***********************************************************************
DEFINE PROGRAM(EQADCCXR)
       GROUP(EQA)
       DATALOC(ANY)
       CEDF(NO)
***********************************************************************
* Program: EQADCCXT                                                   *
***********************************************************************
DEFINE PROGRAM(EQADCCXT)
       GROUP(EQA)
       DATALOC(ANY)
       CEDF(NO)
***********************************************************************
* Program: EQADCDEL                                               @TDA*
***********************************************************************
DEFINE PROGRAM(EQADCDEL)
       GROUP(EQA)
       EXECKEY(CICS)
       DATALOC(ANY)
       CEDF(NO)
***********************************************************************
* Program: EQADCER0                                                   *
***********************************************************************
DEFINE PROGRAM(EQADCER0)
       GROUP(EQA)
       DATALOC(ANY)
       CEDF(NO)
***********************************************************************
* Program: EQADCER1                                                   *
***********************************************************************
DEFINE PROGRAM(EQADCER1)
       GROUP(EQA)
       DATALOC(ANY)
       CEDF(NO)
***********************************************************************
* Program: EQADCER2                                                   *
***********************************************************************
DEFINE PROGRAM(EQADCER2)
       GROUP(EQA)
       DATALOC(ANY)
       CEDF(NO)
***********************************************************************
* Program: EQADCER3                                               @Y1A*
***********************************************************************
DEFINE PROGRAM(EQADCER3)
       GROUP(EQA)
       DATALOC(ANY)
       CEDF(NO)
***********************************************************************
* Program: EQADCHAB                                               @THA*
***********************************************************************
DEFINE PROGRAM(EQADCHAB)
       GROUP(EQA)
       DATALOC(ANY)
       CEDF(NO)
***********************************************************************
* Program: EQADCHU                                                    *
***********************************************************************
DEFINE PROGRAM(EQADCHU)
       GROUP(EQA)
       DATALOC(ANY)
       CEDF(NO)
***********************************************************************
* Program: EQADCMU                                                    *
***********************************************************************
DEFINE PROGRAM(EQADCMU)
       GROUP(EQA)
       DATALOC(ANY)
       CEDF(NO)
***********************************************************************
* Program: EQADCTSQ                                               @TDC*
***********************************************************************
DEFINE PROGRAM(EQADCTSQ)
       GROUP(EQA)
       EXECKEY(CICS)
       DATALOC(ANY)
       CEDF(NO)
***********************************************************************
* Program: EQADCVSA                                               @TDC*
***********************************************************************
DEFINE PROGRAM(EQADCVSA)
       GROUP(EQA)
       EXECKEY(CICS)
       DATALOC(ANY)
       CEDF(NO)
***********************************************************************
* Program: EQADCVSM                                               @TAM*
***********************************************************************
DEFINE PROGRAM(EQADCVSM)
       GROUP(EQA)
       DATALOC(ANY)
       CEDF(NO)
***********************************************************************
* Program: EQADCXEC                                                   *
***********************************************************************
DEFINE PROGRAM(EQADCXEC)
       GROUP(EQA)
       DATALOC(ANY)
       CEDF(NO)
***********************************************************************
* Program: EQADCXXT                                                   *
***********************************************************************
DEFINE PROGRAM(EQADCXXT)
       GROUP(EQA)
       DATALOC(ANY)
       CEDF(NO)
***********************************************************************
* Program: EQADCAN0 - analyzer program in DTCN profile API    @L3M@L1A*
*          must run EXECKEY(CICS)                                     *
***********************************************************************
DEFINE PROGRAM(EQADCAN0)
       GROUP(EQA)
       DATALOC(ANY)
       CEDF(NO)
       EXECKEY(CICS)
***********************************************************************
* Program: EQADCWB0                                               @L1A*
***********************************************************************
DEFINE PROGRAM(EQADCWB0)
       GROUP(EQA)
       DATALOC(ANY)
       CEDF(NO)
***********************************************************************
* Program: EQADCMGR                                               @L2A*
***********************************************************************
DEFINE PROGRAM(EQADCMGR)
       GROUP(EQA)
       DATALOC(ANY)
       CEDF(NO)
***********************************************************************
* Program: EQADFHNO                                                   *
***********************************************************************
DEFINE PROGRAM(EQADFHNO)
       GROUP(EQA)
       DATALOC(ANY)
       CEDF(NO)
       EXECKEY(CICS)
***********************************************************************
* Program: EQAEV006                                                   *
***********************************************************************
DEFINE PROGRAM(EQAEV006)
       GROUP(EQA)
       DATALOC(ANY)
       CEDF(NO)
***********************************************************************
* Program: EQAEVDBG                                               @TMA*
***********************************************************************
DEFINE PROGRAM(EQAEVDBG)
       GROUP(EQA)
       DATALOC(ANY)
       CEDF(NO)
***********************************************************************
* Program: EQALDR31                                               @TMA*
***********************************************************************
DEFINE PROGRAM(EQALDR31)
       GROUP(EQA)
       DATALOC(ANY)
       CEDF(NO)
***********************************************************************
* Program: EQANPCNM                                               @TFA*
***********************************************************************
DEFINE PROGRAM(EQANPCNM)
       GROUP(EQA)
       DATALOC(ANY)
       CEDF(NO)
***********************************************************************
* Program: EQANPMU                                                @TPA*
***********************************************************************
DEFINE PROGRAM(EQANPMU)
       GROUP(EQA)
       DATALOC(ANY)
       CEDF(NO)
***********************************************************************
* Program: EQAQPP                                               @M1A  *
***********************************************************************
DEFINE PROGRAM(EQAQPP)
       GROUP(EQA)
       DATALOC(ANY)
       CEDF(NO)
***********************************************************************
* Program: EQARCCDA                                                   *
***********************************************************************
DEFINE PROGRAM(EQARCCDA)
       GROUP(EQA)
       DATALOC(ANY)
       CEDF(NO)
***********************************************************************
* Program: EQARCDBG                                                   *
***********************************************************************
DEFINE PROGRAM(EQARCDBG)
       GROUP(EQA)
       DATALOC(ANY)
       CEDF(NO)
***********************************************************************
* Program: EQARCDB2                                                   *
***********************************************************************
DEFINE PROGRAM(EQARCDB2)
       GROUP(EQA)
       DATALOC(ANY)
       CEDF(NO)
***********************************************************************
* Program: EQARCEXP                                                   *
***********************************************************************
DEFINE PROGRAM(EQARCEXP)
       GROUP(EQA)
       DATALOC(ANY)
       CEDF(NO)
***********************************************************************
* Program: EQARCHEX                                               @Y4A*
***********************************************************************
DEFINE PROGRAM(EQARCHEX)
       GROUP(EQA)
       DATALOC(ANY)
       CEDF(NO)
***********************************************************************
* Program: EQARCIE3    Deprecated ... no longer used              @Y4C*
***********************************************************************
DEFINE PROGRAM(EQARCIE3)
       GROUP(EQA)
       DATALOC(ANY)
       CEDF(NO)
***********************************************************************
* Program: EQARCIF3    Deprecated ... no longer used              @Y4C*
***********************************************************************
DEFINE PROGRAM(EQARCIF3)
       GROUP(EQA)
       DATALOC(ANY)
       CEDF(NO)
***********************************************************************
* Program: EQARCI3E                                               @Y4A*
***********************************************************************
DEFINE PROGRAM(EQARCI3E)
       GROUP(EQA)
       DATALOC(ANY)
       CEDF(NO)
***********************************************************************
* Program: EQARCL4                                                    *
***********************************************************************
DEFINE PROGRAM(EQARCL4)
       GROUP(EQA)
       DATALOC(ANY)
       CEDF(NO)
***********************************************************************
* Program: EQARCME     Deprecated ... no longer used              @Y4C*
***********************************************************************
DEFINE PROGRAM(EQARCME)
       GROUP(EQA)
       DATALOC(ANY)
       CEDF(NO)
***********************************************************************
* Program: EQARCMG     Deprecated ... no longer used              @Y4C*
***********************************************************************
DEFINE PROGRAM(EQARCMG)
       GROUP(EQA)
       DATALOC(ANY)
       CEDF(NO)
***********************************************************************
* Program: EQARCMH     Deprecated ... no longer used              @Y4C*
***********************************************************************
DEFINE PROGRAM(EQARCMH)
       GROUP(EQA)
       DATALOC(ANY)
       CEDF(NO)
***********************************************************************
* Program: EQARCMK     Deprecated ... no longer used              @Y4C*
***********************************************************************
DEFINE PROGRAM(EQARCMK)
       GROUP(EQA)
       DATALOC(ANY)
       CEDF(NO)
***********************************************************************
* Program: EQARCOBD                                               @Y7A*
***********************************************************************
DEFINE PROGRAM(EQARCOBD)
       GROUP(EQA)
       DATALOC(ANY)
       CEDF(NO)
***********************************************************************
* Program: IGZDEBUG (alias of EQARCOBD)                           @Y7A*
***********************************************************************
DEFINE PROGRAM(IGZDEBUG)
       GROUP(EQA)
       DATALOC(ANY)
       CEDF(NO)
***********************************************************************
* Program: EQARCPDC                                               @Y4A*
***********************************************************************
DEFINE PROGRAM(EQARCPDC)
       GROUP(EQA)
       DATALOC(ANY)
       CEDF(NO)
***********************************************************************
* Program: EQARCPRS    Deprecated ... no longer used              @Y4C*
***********************************************************************
DEFINE PROGRAM(EQARCPRS)
       GROUP(EQA)
       DATALOC(ANY)
       CEDF(NO)
***********************************************************************
* Program: EQARCTRC                                                   *
***********************************************************************
DEFINE PROGRAM(EQARCTRC)
       GROUP(EQA)
       DATALOC(ANY)
       CEDF(NO)
***********************************************************************
* Program: EQARCTYP    Deprecated ... no longer used                  *
***********************************************************************
DEFINE PROGRAM(EQARCTYP)
       GROUP(EQA)
       DATALOC(ANY)
       CEDF(NO)
***********************************************************************
* Program: EQARCVND                                               @Y4A*
***********************************************************************
DEFINE PROGRAM(EQARCVND)
       GROUP(EQA)
       DATALOC(ANY)
       CEDF(NO)
***********************************************************************
* Program: EQASCMU                                                @TBA*
***********************************************************************
DEFINE PROGRAM(EQASCMU)
       GROUP(EQA)
       DATALOC(ANY)
       CEDF(NO)
***********************************************************************
* Program: EQASUER0                                                   *
***********************************************************************
DEFINE PROGRAM(EQASUER0)
       GROUP(EQA)
       DATALOC(ANY)
       CEDF(NO)
***********************************************************************
* Program: EQASUER1                                                   *
***********************************************************************
DEFINE PROGRAM(EQASUER1)
       GROUP(EQA)
       DATALOC(ANY)
       CEDF(NO)
***********************************************************************
* Program: EQASUER2                                                   *
***********************************************************************
DEFINE PROGRAM(EQASUER2)
       GROUP(EQA)
       DATALOC(ANY)
       CEDF(NO)
***********************************************************************
* Program: EQASUJCL                                                   *
***********************************************************************
DEFINE PROGRAM(EQASUJCL)
       GROUP(EQA)
       DATALOC(ANY)
       CEDF(NO)
***********************************************************************
* Program: EQAXSIO                                                    *
***********************************************************************
DEFINE PROGRAM(EQAXSIO)
       GROUP(EQA)
       DATALOC(ANY)
       CEDF(NO)
***********************************************************************
* Program: EQA0AIPI                                                   *
***********************************************************************
DEFINE PROGRAM(EQA0AIPI)
       GROUP(EQA)
       DATALOC(ANY)
       CEDF(NO)
***********************************************************************
* Program: EQA0CINF                                               @T5C*
***********************************************************************
DEFINE PROGRAM(EQA0CINF)
       GROUP(EQA)
       DATALOC(ANY)
       CEDF(NO)
***********************************************************************
* Program: EQA0VEXD                                               @00C*
***********************************************************************
DEFINE PROGRAM(EQA0VEXD)
       GROUP(EQA)
       DATALOC(ANY)
       CEDF(NO)
***********************************************************************
* Program: EQA0VEXE                                               @00C*
***********************************************************************
DEFINE PROGRAM(EQA0VEXE)
       GROUP(EQA)
       DATALOC(ANY)
       CEDF(NO)
***********************************************************************
* Program: EQA0CTRU                                               @T6A*
***********************************************************************
DEFINE PROGRAM(EQA0CTRU)
       GROUP(EQA)
       DATALOC(ANY)
       CEDF(NO)
       CONCURRENCY(THREADSAFE)
       EXECKEY(CICS)
***********************************************************************
* Program: EQA0S4CN                                                   *
***********************************************************************
DEFINE PROGRAM(EQA0S4CN)
       GROUP(EQA)
       DATALOC(ANY)
       CEDF(NO)
***********************************************************************
* Program: EQA0S4DS                                                   *
***********************************************************************
DEFINE PROGRAM(EQA0S4DS)
       GROUP(EQA)
       DATALOC(ANY)
       CEDF(NO)
***********************************************************************
* Program: EQA0S4LB                                                   *
***********************************************************************
DEFINE PROGRAM(EQA0S4LB)
       GROUP(EQA)
       DATALOC(ANY)
       CEDF(NO)
***********************************************************************
* Program: EQA0S4YA                                                   *
***********************************************************************
DEFINE PROGRAM(EQA0S4YA)
       GROUP(EQA)
       DATALOC(ANY)
       CEDF(NO)
***********************************************************************
* Program: EQA0S4YC                                                   *
***********************************************************************
DEFINE PROGRAM(EQA0S4YC)
       GROUP(EQA)
       DATALOC(ANY)
       CEDF(NO)
***********************************************************************
* Program: EQA0S4YE                                                   *
***********************************************************************
DEFINE PROGRAM(EQA0S4YE)
       GROUP(EQA)
       DATALOC(ANY)
       CEDF(NO)
***********************************************************************
* Program: EQA0S9LB                                                   *
***********************************************************************
DEFINE PROGRAM(EQA0S9LB)
       GROUP(EQA)
       DATALOC(ANY)
       CEDF(NO)
***********************************************************************
* Program: EQA0S9YA                                                   *
***********************************************************************
DEFINE PROGRAM(EQA0S9YA)
       GROUP(EQA)
       DATALOC(ANY)
       CEDF(NO)
***********************************************************************
* Program: EQA0S9YC                                                   *
***********************************************************************
DEFINE PROGRAM(EQA0S9YC)
       GROUP(EQA)
       DATALOC(ANY)
       CEDF(NO)
***********************************************************************
* Program: EQA0S9YE                                                   *
***********************************************************************
DEFINE PROGRAM(EQA0S9YE)
       GROUP(EQA)
       DATALOC(ANY)
       CEDF(NO)
***********************************************************************
* Program: EQA00A3H    Deprecated ... no longer used              @T6C*
***********************************************************************
DEFINE PROGRAM(EQA00A3H)
       GROUP(EQA)
       DATALOC(ANY)
       CEDF(NO)
***********************************************************************
* Program: EQA00AEH                                                   *
***********************************************************************
DEFINE PROGRAM(EQA00AEH)
       GROUP(EQA)
       DATALOC(ANY)
       CEDF(NO)
***********************************************************************
* Program: EQA00D3H    Deprecated ... no longer used              @T6C*
***********************************************************************
DEFINE PROGRAM(EQA00D3H)
       GROUP(EQA)
       DATALOC(ANY)
       CEDF(NO)
***********************************************************************
* Program: EQA00DEH                                                   *
***********************************************************************
DEFINE PROGRAM(EQA00DEH)
       GROUP(EQA)
       DATALOC(ANY)
       CEDF(NO)
***********************************************************************
* Program: EQA00DYN                                                   *
***********************************************************************
DEFINE PROGRAM(EQA00DYN)
       GROUP(EQA)
       DATALOC(ANY)
       CEDF(NO)
***********************************************************************
* Program: EQA00ER0                                                   *
***********************************************************************
DEFINE PROGRAM(EQA00ER0)
       GROUP(EQA)
       DATALOC(ANY)
       CEDF(NO)
***********************************************************************
* Program: EQA00ER1                                                   *
***********************************************************************
DEFINE PROGRAM(EQA00ER1)
       GROUP(EQA)
       DATALOC(ANY)
       CEDF(NO)
***********************************************************************
* Program: EQA00ER2                                                   *
***********************************************************************
DEFINE PROGRAM(EQA00ER2)
       GROUP(EQA)
       DATALOC(ANY)
       CEDF(NO)
***********************************************************************
* Program: EQA00ER3                                               @Y1A*
***********************************************************************
DEFINE PROGRAM(EQA00ER3)
       GROUP(EQA)
       DATALOC(ANY)
       CEDF(NO)
***********************************************************************
* Program: EQA00EVH                                                   *
***********************************************************************
DEFINE PROGRAM(EQA00EVH)
       GROUP(EQA)
       DATALOC(ANY)
       CEDF(NO)
***********************************************************************
* Program: EQA00E40                                               @Y2A*
***********************************************************************
DEFINE PROGRAM(EQA00E40)
       GROUP(EQA)
       DATALOC(ANY)
       CEDF(NO)
***********************************************************************
* Program: EQA00E41                                               @Y2A*
***********************************************************************
DEFINE PROGRAM(EQA00E41)
       GROUP(EQA)
       DATALOC(ANY)
       CEDF(NO)
***********************************************************************
* Program: EQA00E42                                               @Y2A*
***********************************************************************
DEFINE PROGRAM(EQA00E42)
       GROUP(EQA)
       DATALOC(ANY)
       CEDF(NO)
***********************************************************************
* Program: EQA00E43                                               @Y2A*
***********************************************************************
DEFINE PROGRAM(EQA00E43)
       GROUP(EQA)
       DATALOC(ANY)
       CEDF(NO)
***********************************************************************
* Program: EQA00E53 - CICS Global User Exit for CICS 5.3              *
*          MUST run EXECKEY(CICS)                                     *
***********************************************************************
DEFINE PROGRAM(EQA00E53)
       GROUP(EQA)
       DATALOC(ANY)
       CEDF(NO)
       CONCURRENCY(THREADSAFE)
       EXECKEY(CICS)
***********************************************************************
* Program: EQA00E62 - CICS Global User Exit for CICS TS 2.2           *
*          MUST run EXECKEY(CICS)                                     *
***********************************************************************
DEFINE PROGRAM(EQA00E62)
       GROUP(EQA)
       DATALOC(ANY)
       CEDF(NO)
       CONCURRENCY(THREADSAFE)
       EXECKEY(CICS)
***********************************************************************
* Program: EQA00E63 - CICS Global User Exit for CICS TS 2.3           *
*          MUST run EXECKEY(CICS)                                     *
***********************************************************************
DEFINE PROGRAM(EQA00E63)
       GROUP(EQA)
       DATALOC(ANY)
       CEDF(NO)
       CONCURRENCY(THREADSAFE)
       EXECKEY(CICS)
***********************************************************************
* Program: EQA00E64 - CICS Global User Exit                           *
*          MUST run EXECKEY(CICS)                                     *
***********************************************************************
DEFINE PROGRAM(EQA00E64)
       GROUP(EQA)
       DATALOC(ANY)
       CEDF(NO)
       CONCURRENCY(THREADSAFE)
       EXECKEY(CICS)
***********************************************************************
* Program: EQA00E65 - CICS Global User Exit                           *
*          MUST run EXECKEY(CICS)                                     *
***********************************************************************
DEFINE PROGRAM(EQA00E65)
       GROUP(EQA)
       DATALOC(ANY)
       CEDF(NO)
       CONCURRENCY(THREADSAFE)
       EXECKEY(CICS)
***********************************************************************
* Program: EQA00E66 - CICS Global User Exit                       @T7A*
*          MUST run EXECKEY(CICS)                                     *
***********************************************************************
DEFINE PROGRAM(EQA00E66)
       GROUP(EQA)
       DATALOC(ANY)
       CEDF(NO)
       CONCURRENCY(THREADSAFE)
       EXECKEY(CICS)
***********************************************************************
* Program: EQA00E67 - CICS Global User Exit                       @T7A*
*          MUST run EXECKEY(CICS)                                     *
***********************************************************************
DEFINE PROGRAM(EQA00E67)
       GROUP(EQA)
       DATALOC(ANY)
       CEDF(NO)
       CONCURRENCY(THREADSAFE)
       EXECKEY(CICS)
***********************************************************************
* Program: EQA00E68 - CICS Global User Exit                           *
*          MUST run EXECKEY(CICS)                                     *
***********************************************************************
DEFINE PROGRAM(EQA00E68)
       GROUP(EQA)
       DATALOC(ANY)
       CEDF(NO)
       CONCURRENCY(THREADSAFE)
       EXECKEY(CICS)
***********************************************************************
* Program: EQA00E69 - CICS Global User Exit                       @TKA*
*          MUST run EXECKEY(CICS)                                     *
***********************************************************************
DEFINE PROGRAM(EQA00E69)
       GROUP(EQA)
       DATALOC(ANY)
       CEDF(NO)
       CONCURRENCY(THREADSAFE)
       EXECKEY(CICS)
***********************************************************************
* Program: EQA00E70 - CICS Global User Exit                       @TLA*
*          MUST run EXECKEY(CICS)                                     *
***********************************************************************
DEFINE PROGRAM(EQA00E70)
       GROUP(EQA)
       DATALOC(ANY)
       CEDF(NO)
       CONCURRENCY(THREADSAFE)
       EXECKEY(CICS)
***********************************************************************
* Program: EQA00E71 - CICS Global User Exit                       @TNA*
*          MUST run EXECKEY(CICS)                                     *
***********************************************************************
DEFINE PROGRAM(EQA00E71)
       GROUP(EQA)
       DATALOC(ANY)
       CEDF(NO)
       CONCURRENCY(THREADSAFE)
       EXECKEY(CICS)
***********************************************************************
* Program: EQA00E72 - CICS Global User Exit                       @Y8A*
*          MUST run EXECKEY(CICS)                                     *
***********************************************************************
DEFINE PROGRAM(EQA00E72)
       GROUP(EQA)
       DATALOC(ANY)
       CEDF(NO)
       CONCURRENCY(THREADSAFE)
       EXECKEY(CICS)
***********************************************************************
* Program: EQA00E73 - CICS Global User Exit                       @Y9A*
*          MUST run EXECKEY(CICS)                                     *
***********************************************************************
DEFINE PROGRAM(EQA00E73)
       GROUP(EQA)
       DATALOC(ANY)
       CEDF(NO)
       CONCURRENCY(THREADSAFE)
       EXECKEY(CICS)
***********************************************************************
* Program: EQA00E74 - CICS Global User Exit                       @YAA*
*          MUST run EXECKEY(CICS)                                     *
***********************************************************************
DEFINE PROGRAM(EQA00E74)
       GROUP(EQA)
       DATALOC(ANY)
       CEDF(NO)
       CONCURRENCY(THREADSAFE)
       EXECKEY(CICS)
***********************************************************************
* Program: EQA00E75 - CICS Global User Exit                       @YCA*
*          MUST run EXECKEY(CICS)                                     *
***********************************************************************
DEFINE PROGRAM(EQA00E75)
       GROUP(EQA)
       DATALOC(ANY)
       CEDF(NO)
       CONCURRENCY(THREADSAFE)
       EXECKEY(CICS)
***********************************************************************
* Program: EQA00E76 - CICS Global User Exit                       @YDA*
*          MUST run EXECKEY(CICS)                                     *
***********************************************************************
DEFINE PROGRAM(EQA00E76)
       GROUP(EQA)
       DATALOC(ANY)
       CEDF(NO)
       CONCURRENCY(THREADSAFE)
       EXECKEY(CICS)
***********************************************************************
* Program: EQA00HFS                                                   *
***********************************************************************
DEFINE PROGRAM(EQA00HFS)
       GROUP(EQA)
       DATALOC(ANY)
       CEDF(NO)
***********************************************************************
* Program: EQA00MSG                                                   *
***********************************************************************
DEFINE PROGRAM(EQA00MSG)
       GROUP(EQA)
       DATALOC(ANY)
       CEDF(NO)
***********************************************************************
* Program: EQA00OHH                                                   *
***********************************************************************
DEFINE PROGRAM(EQA00OHH)
       GROUP(EQA)
       DATALOC(ANY)
       CEDF(NO)
***********************************************************************
* Program: EQA00OHT                                                   *
***********************************************************************
DEFINE PROGRAM(EQA00OHT)
       GROUP(EQA)
       CEDF(NO)
***********************************************************************
* Program: EQA00OSX                                                   *
***********************************************************************
DEFINE PROGRAM(EQA00OSX)
       GROUP(EQA)
       DATALOC(ANY)
       CEDF(NO)
***********************************************************************
* Program: EQA07BLK                                                   *
***********************************************************************
DEFINE PROGRAM(EQA07BLK)
       GROUP(EQA)
       DATALOC(ANY)
       CEDF(NO)
***********************************************************************
* Program: EQA07CUS                                                   *
***********************************************************************
DEFINE PROGRAM(EQA07CUS)
       GROUP(EQA)
       DATALOC(ANY)
       CEDF(NO)
***********************************************************************
* Program: EQA07STT                                                   *
***********************************************************************
DEFINE PROGRAM(EQA07STT)
       GROUP(EQA)
       DATALOC(ANY)
       CEDF(NO)
***********************************************************************
* Program: EQA10ANL                                                   *
***********************************************************************
DEFINE PROGRAM(EQA10ANL)
       GROUP(EQA)
       DATALOC(ANY)
       CEDF(NO)
***********************************************************************
* Program: EQA10ER0                                                   *
***********************************************************************
DEFINE PROGRAM(EQA10ER0)
       GROUP(EQA)
       DATALOC(ANY)
       CEDF(NO)
***********************************************************************
* Program: EQA10ER1                                                   *
***********************************************************************
DEFINE PROGRAM(EQA10ER1)
       GROUP(EQA)
       DATALOC(ANY)
       CEDF(NO)
***********************************************************************
* Program: EQA10ER2                                                   *
***********************************************************************
DEFINE PROGRAM(EQA10ER2)
       GROUP(EQA)
       DATALOC(ANY)
       CEDF(NO)
***********************************************************************
* Program: EQA10ER3                                               @Y1A*
***********************************************************************
DEFINE PROGRAM(EQA10ER3)
       GROUP(EQA)
       DATALOC(ANY)
       CEDF(NO)
***********************************************************************
* Program: EQA10FBP                                               @y6c*
***********************************************************************
DEFINE PROGRAM(EQA10FBP)
       GROUP(EQA)
       DATALOC(ANY)
       CEDF(NO)
***********************************************************************
* Program: EQA10FND                                                   *
***********************************************************************
DEFINE PROGRAM(EQA10FND)
       GROUP(EQA)
       DATALOC(ANY)
       CEDF(NO)
***********************************************************************
* Program: EQA10GL0                                                   *
***********************************************************************
DEFINE PROGRAM(EQA10GL0)
       GROUP(EQA)
       DATALOC(ANY)
       CEDF(NO)
***********************************************************************
* Program: EQA10GL1                                                   *
***********************************************************************
DEFINE PROGRAM(EQA10GL1)
       GROUP(EQA)
       DATALOC(ANY)
       CEDF(NO)
***********************************************************************
* Program: EQA10GL2                                                   *
***********************************************************************
DEFINE PROGRAM(EQA10GL2)
       GROUP(EQA)
       DATALOC(ANY)
       CEDF(NO)
***********************************************************************
* Program: EQA10GL3                                               @Y1A*
***********************************************************************
DEFINE PROGRAM(EQA10GL3)
       GROUP(EQA)
       DATALOC(ANY)
       CEDF(NO)
***********************************************************************
* Program: EQA10LN0                                                   *
***********************************************************************
DEFINE PROGRAM(EQA10LN0)
       GROUP(EQA)
       DATALOC(ANY)
       CEDF(NO)
***********************************************************************
* Program: EQA10LN1                                                   *
***********************************************************************
DEFINE PROGRAM(EQA10LN1)
       GROUP(EQA)
       DATALOC(ANY)
       CEDF(NO)
***********************************************************************
* Program: EQA10LN2                                                   *
***********************************************************************
DEFINE PROGRAM(EQA10LN2)
       GROUP(EQA)
       DATALOC(ANY)
       CEDF(NO)
***********************************************************************
* Program: EQA10LN3                                               @Y1A*
***********************************************************************
DEFINE PROGRAM(EQA10LN3)
       GROUP(EQA)
       DATALOC(ANY)
       CEDF(NO)
***********************************************************************
* Program: EQA10LTB                                                   *
***********************************************************************
DEFINE PROGRAM(EQA10LTB)
       GROUP(EQA)
       DATALOC(ANY)
       CEDF(NO)
***********************************************************************
* Program: EQA10LX0                                                   *
***********************************************************************
DEFINE PROGRAM(EQA10LX0)
       GROUP(EQA)
       DATALOC(ANY)
       CEDF(NO)
***********************************************************************
* Program: EQA10LX1                                                   *
***********************************************************************
DEFINE PROGRAM(EQA10LX1)
       GROUP(EQA)
       DATALOC(ANY)
       CEDF(NO)
***********************************************************************
* Program: EQA10LX2                                                   *
***********************************************************************
DEFINE PROGRAM(EQA10LX2)
       GROUP(EQA)
       DATALOC(ANY)
       CEDF(NO)
***********************************************************************
* Program: EQA10LX3                                               @Y1A*
***********************************************************************
DEFINE PROGRAM(EQA10LX3)
       GROUP(EQA)
       DATALOC(ANY)
       CEDF(NO)
***********************************************************************
* Program: EQA10MG0                                                   *
***********************************************************************
DEFINE PROGRAM(EQA10MG0)
       GROUP(EQA)
       DATALOC(ANY)
       CEDF(NO)
***********************************************************************
* Program: EQA10MG1                                                   *
***********************************************************************
DEFINE PROGRAM(EQA10MG1)
       GROUP(EQA)
       DATALOC(ANY)
       CEDF(NO)
***********************************************************************
* Program: EQA10MG2                                                   *
***********************************************************************
DEFINE PROGRAM(EQA10MG2)
       GROUP(EQA)
       DATALOC(ANY)
       CEDF(NO)
***********************************************************************
* Program: EQA10MG3                                               @Y1A*
***********************************************************************
DEFINE PROGRAM(EQA10MG3)
       GROUP(EQA)
       DATALOC(ANY)
       CEDF(NO)
***********************************************************************
* Program: EQA10OSM                                                   *
***********************************************************************
DEFINE PROGRAM(EQA10OSM)
       GROUP(EQA)
       DATALOC(ANY)
       CEDF(NO)
***********************************************************************
* Program: EQA10VC0                                                   *
***********************************************************************
DEFINE PROGRAM(EQA10VC0)
       GROUP(EQA)
       DATALOC(ANY)
       CEDF(NO)
***********************************************************************
* Program: EQA10VC1                                                   *
***********************************************************************
DEFINE PROGRAM(EQA10VC1)
       GROUP(EQA)
       DATALOC(ANY)
       CEDF(NO)
***********************************************************************
* Program: EQA10VC2                                                   *
***********************************************************************
DEFINE PROGRAM(EQA10VC2)
       GROUP(EQA)
       DATALOC(ANY)
       CEDF(NO)
***********************************************************************
* Program: EQA10VC3                                               @Y1A*
***********************************************************************
DEFINE PROGRAM(EQA10VC3)
       GROUP(EQA)
       DATALOC(ANY)
       CEDF(NO)
***********************************************************************
* Program: EQA10XSC                                                   *
***********************************************************************
DEFINE PROGRAM(EQA10XSC)
       GROUP(EQA)
       DATALOC(ANY)
       CEDF(NO)
***********************************************************************
* Program: EQA12CED                                                   *
***********************************************************************
DEFINE PROGRAM(EQA12CED)
       GROUP(EQA)
       DATALOC(ANY)
       CEDF(NO)
***********************************************************************
* Program: EQA12CEI                                                   *
***********************************************************************
DEFINE PROGRAM(EQA12CEI)
       GROUP(EQA)
       DATALOC(ANY)
       CEDF(NO)
***********************************************************************
* Program: EQA12CES                                                   *
***********************************************************************
DEFINE PROGRAM(EQA12CES)
       GROUP(EQA)
       DATALOC(ANY)
       CEDF(NO)
***********************************************************************
* Program: EQA12CEX                                                   *
***********************************************************************
DEFINE PROGRAM(EQA12CEX)
       GROUP(EQA)
       DATALOC(ANY)
       CEDF(NO)
***********************************************************************
* Program: EQA12CUR                                                   *
***********************************************************************
DEFINE PROGRAM(EQA12CUR)
       GROUP(EQA)
       DATALOC(ANY)
       CEDF(NO)
***********************************************************************
* Program: EQA12DCL                                                   *
***********************************************************************
DEFINE PROGRAM(EQA12DCL)
       GROUP(EQA)
       DATALOC(ANY)
       CEDF(NO)
***********************************************************************
* Program: EQA12DSP                                                   *
***********************************************************************
DEFINE PROGRAM(EQA12DSP)
       GROUP(EQA)
       DATALOC(ANY)
       CEDF(NO)
***********************************************************************
* Program: EQA12LXB                                                   *
***********************************************************************
DEFINE PROGRAM(EQA12LXB)
       GROUP(EQA)
       DATALOC(ANY)
       CEDF(NO)
***********************************************************************
* Program: EQA12QNS                                                   *
***********************************************************************
DEFINE PROGRAM(EQA12QNS)
       GROUP(EQA)
       DATALOC(ANY)
       CEDF(NO)
***********************************************************************
* Program: EQA12SCN                                                   *
***********************************************************************
DEFINE PROGRAM(EQA12SCN)
       GROUP(EQA)
       DATALOC(ANY)
       CEDF(NO)
***********************************************************************
* Program: EQA12SYA                                                   *
***********************************************************************
DEFINE PROGRAM(EQA12SYA)
       GROUP(EQA)
       DATALOC(ANY)
       CEDF(NO)
***********************************************************************
* Program: EQA12SYC                                                   *
***********************************************************************
DEFINE PROGRAM(EQA12SYC)
       GROUP(EQA)
       DATALOC(ANY)
       CEDF(NO)
***********************************************************************
* Program: EQA12SYE                                                   *
***********************************************************************
DEFINE PROGRAM(EQA12SYE)
       GROUP(EQA)
       DATALOC(ANY)
       CEDF(NO)
***********************************************************************
* Program: EQA13CEX                                                   *
***********************************************************************
DEFINE PROGRAM(EQA13CEX)
       GROUP(EQA)
       DATALOC(ANY)
       CEDF(NO)
***********************************************************************
* Program: EQA13CUR                                                   *
***********************************************************************
DEFINE PROGRAM(EQA13CUR)
       GROUP(EQA)
       DATALOC(ANY)
       CEDF(NO)
***********************************************************************
* Program: EQA13DCL                                                   *
***********************************************************************
DEFINE PROGRAM(EQA13DCL)
       GROUP(EQA)
       DATALOC(ANY)
       CEDF(NO)
***********************************************************************
* Program: EQA13DSP                                                   *
***********************************************************************
DEFINE PROGRAM(EQA13DSP)
       GROUP(EQA)
       DATALOC(ANY)
       CEDF(NO)
***********************************************************************
* Program: EQA13LXB                                                   *
***********************************************************************
DEFINE PROGRAM(EQA13LXB)
       GROUP(EQA)
       DATALOC(ANY)
       CEDF(NO)
***********************************************************************
* Program: EQA13MDV                                                   *
***********************************************************************
DEFINE PROGRAM(EQA13MDV)
       GROUP(EQA)
       DATALOC(ANY)
       CEDF(NO)
***********************************************************************
* Program: EQA13QNS                                                   *
***********************************************************************
DEFINE PROGRAM(EQA13QNS)
       GROUP(EQA)
       DATALOC(ANY)
       CEDF(NO)
***********************************************************************
* Program: EQA13SCN                                                   *
***********************************************************************
DEFINE PROGRAM(EQA13SCN)
       GROUP(EQA)
       DATALOC(ANY)
       CEDF(NO)
***********************************************************************
* Program: EQA13SWC                                                   *
***********************************************************************
DEFINE PROGRAM(EQA13SWC)
       GROUP(EQA)
       DATALOC(ANY)
       CEDF(NO)
***********************************************************************
* Program: EQA13SYA                                                   *
***********************************************************************
DEFINE PROGRAM(EQA13SYA)
       GROUP(EQA)
       DATALOC(ANY)
       CEDF(NO)
***********************************************************************
* Program: EQA13SYC                                                   *
***********************************************************************
DEFINE PROGRAM(EQA13SYC)
       GROUP(EQA)
       DATALOC(ANY)
       CEDF(NO)
***********************************************************************
* Program: EQA13SYE                                                   *
***********************************************************************
DEFINE PROGRAM(EQA13SYE)
       GROUP(EQA)
       DATALOC(ANY)
       CEDF(NO)
***********************************************************************
* Program: EQA14CAL                                                   *
***********************************************************************
DEFINE PROGRAM(EQA14CAL)
       GROUP(EQA)
       DATALOC(ANY)
       CEDF(NO)
***********************************************************************
* Program: EQA14CEX                                                   *
***********************************************************************
DEFINE PROGRAM(EQA14CEX)
       GROUP(EQA)
       DATALOC(ANY)
       CEDF(NO)
***********************************************************************
* Program: EQA14CMS                                                   *
***********************************************************************
DEFINE PROGRAM(EQA14CMS)
       GROUP(EQA)
       DATALOC(ANY)
       CEDF(NO)
***********************************************************************
* Program: EQA14CUR                                                   *
***********************************************************************
DEFINE PROGRAM(EQA14CUR)
       GROUP(EQA)
       DATALOC(ANY)
       CEDF(NO)
***********************************************************************
* Program: EQA14DCL                                                   *
***********************************************************************
DEFINE PROGRAM(EQA14DCL)
       GROUP(EQA)
       DATALOC(ANY)
       CEDF(NO)
***********************************************************************
* Program: EQA14DSP                                                   *
***********************************************************************
DEFINE PROGRAM(EQA14DSP)
       GROUP(EQA)
       DATALOC(ANY)
       CEDF(NO)
***********************************************************************
* Program: EQA14EVL                                                   *
***********************************************************************
DEFINE PROGRAM(EQA14EVL)
       GROUP(EQA)
       DATALOC(ANY)
       CEDF(NO)
***********************************************************************
* Program: EQA14LXB                                                   *
***********************************************************************
DEFINE PROGRAM(EQA14LXB)
       GROUP(EQA)
       DATALOC(ANY)
       CEDF(NO)
***********************************************************************
* Program: EQA14MDV                                               @HDA*
***********************************************************************
DEFINE PROGRAM(EQA14MDV)
       GROUP(EQA)
       DATALOC(ANY)
       CEDF(NO)
***********************************************************************
* Program: EQA14PRF                                                   *
***********************************************************************
DEFINE PROGRAM(EQA14PRF)
       GROUP(EQA)
       DATALOC(ANY)
       CEDF(NO)
***********************************************************************
* Program: EQA14QNS                                                   *
***********************************************************************
DEFINE PROGRAM(EQA14QNS)
       GROUP(EQA)
       DATALOC(ANY)
       CEDF(NO)
***********************************************************************
* Program: EQA14SCN                                                   *
***********************************************************************
DEFINE PROGRAM(EQA14SCN)
       GROUP(EQA)
       DATALOC(ANY)
       CEDF(NO)
***********************************************************************
* Program: EQA14SYA                                                   *
***********************************************************************
DEFINE PROGRAM(EQA14SYA)
       GROUP(EQA)
       DATALOC(ANY)
       CEDF(NO)
***********************************************************************
* Program: EQA14SYC                                                   *
***********************************************************************
DEFINE PROGRAM(EQA14SYC)
       GROUP(EQA)
       DATALOC(ANY)
       CEDF(NO)
***********************************************************************
* Program: EQA14SYE                                                   *
***********************************************************************
DEFINE PROGRAM(EQA14SYE)
       GROUP(EQA)
       DATALOC(ANY)
       CEDF(NO)
***********************************************************************
* Program: EQA17CED                                                   *
***********************************************************************
DEFINE PROGRAM(EQA17CED)
       GROUP(EQA)
       DATALOC(ANY)
       CEDF(NO)
***********************************************************************
* Program: EQA17CEI                                                   *
***********************************************************************
DEFINE PROGRAM(EQA17CEI)
       GROUP(EQA)
       DATALOC(ANY)
       CEDF(NO)
***********************************************************************
* Program: EQA17CEX                                                   *
***********************************************************************
DEFINE PROGRAM(EQA17CEX)
       GROUP(EQA)
       DATALOC(ANY)
       CEDF(NO)
***********************************************************************
* Program: EQA17CUR                                                   *
***********************************************************************
DEFINE PROGRAM(EQA17CUR)
       GROUP(EQA)
       DATALOC(ANY)
       CEDF(NO)
***********************************************************************
* Program: EQA17DCL                                                   *
***********************************************************************
DEFINE PROGRAM(EQA17DCL)
       GROUP(EQA)
       DATALOC(ANY)
       CEDF(NO)
***********************************************************************
* Program: EQA17DSP                                                   *
***********************************************************************
DEFINE PROGRAM(EQA17DSP)
       GROUP(EQA)
       DATALOC(ANY)
       CEDF(NO)
***********************************************************************
* Program: EQA17LXB                                                   *
***********************************************************************
DEFINE PROGRAM(EQA17LXB)
       GROUP(EQA)
       DATALOC(ANY)
       CEDF(NO)
***********************************************************************
* Program: EQA17QNS                                                   *
***********************************************************************
DEFINE PROGRAM(EQA17QNS)
       GROUP(EQA)
       DATALOC(ANY)
       CEDF(NO)
***********************************************************************
* Program: EQA17SCN                                                   *
***********************************************************************
DEFINE PROGRAM(EQA17SCN)
       GROUP(EQA)
       DATALOC(ANY)
       CEDF(NO)
***********************************************************************
* Program: EQA17SYA                                                   *
***********************************************************************
DEFINE PROGRAM(EQA17SYA)
       GROUP(EQA)
       DATALOC(ANY)
       CEDF(NO)
***********************************************************************
* Program: EQA17SYC                                                   *
***********************************************************************
DEFINE PROGRAM(EQA17SYC)
       GROUP(EQA)
       DATALOC(ANY)
       CEDF(NO)
***********************************************************************
* Program: EQA17SYE                                                   *
***********************************************************************
DEFINE PROGRAM(EQA17SYE)
       GROUP(EQA)
       DATALOC(ANY)
       CEDF(NO)
***********************************************************************
* Program: EQA18CED                                                   *
***********************************************************************
DEFINE PROGRAM(EQA18CED)
       GROUP(EQA)
       DATALOC(ANY)
       CEDF(NO)
***********************************************************************
* Program: EQA18CEI                                                   *
***********************************************************************
DEFINE PROGRAM(EQA18CEI)
       GROUP(EQA)
       DATALOC(ANY)
       CEDF(NO)
***********************************************************************
* Program: EQA18CEX                                                   *
***********************************************************************
DEFINE PROGRAM(EQA18CEX)
       GROUP(EQA)
       DATALOC(ANY)
       CEDF(NO)
***********************************************************************
* Program: EQA18CUR                                                   *
***********************************************************************
DEFINE PROGRAM(EQA18CUR)
       GROUP(EQA)
       DATALOC(ANY)
       CEDF(NO)
***********************************************************************
* Program: EQA18DCL                                                   *
***********************************************************************
DEFINE PROGRAM(EQA18DCL)
       GROUP(EQA)
       DATALOC(ANY)
       CEDF(NO)
***********************************************************************
* Program: EQA18DSP                                                   *
***********************************************************************
DEFINE PROGRAM(EQA18DSP)
       GROUP(EQA)
       DATALOC(ANY)
       CEDF(NO)
***********************************************************************
* Program: EQA18LXB                                                   *
***********************************************************************
DEFINE PROGRAM(EQA18LXB)
       GROUP(EQA)
       DATALOC(ANY)
       CEDF(NO)
***********************************************************************
* Program: EQA18QNS                                                   *
***********************************************************************
DEFINE PROGRAM(EQA18QNS)
       GROUP(EQA)
       DATALOC(ANY)
       CEDF(NO)
***********************************************************************
* Program: EQA18SCN                                                   *
***********************************************************************
DEFINE PROGRAM(EQA18SCN)
       GROUP(EQA)
       DATALOC(ANY)
       CEDF(NO)
***********************************************************************
* Program: EQA18SYA                                                   *
***********************************************************************
DEFINE PROGRAM(EQA18SYA)
       GROUP(EQA)
       DATALOC(ANY)
       CEDF(NO)
***********************************************************************
* Program: EQA18SYC                                                   *
***********************************************************************
DEFINE PROGRAM(EQA18SYC)
       GROUP(EQA)
       DATALOC(ANY)
       CEDF(NO)
***********************************************************************
* Program: EQA18SYE                                                   *
***********************************************************************
DEFINE PROGRAM(EQA18SYE)
       GROUP(EQA)
       DATALOC(ANY)
       CEDF(NO)
***********************************************************************
* Program: EQA30TKI                                                   *
***********************************************************************
DEFINE PROGRAM(EQA30TKI)
       GROUP(EQA)
       DATALOC(ANY)
       CEDF(NO)
***********************************************************************
* Program: EQA50CTL                                                   *
***********************************************************************
DEFINE PROGRAM(EQA50CTL)
       GROUP(EQA)
       DATALOC(ANY)
       CEDF(NO)
***********************************************************************
* Program: EQA50XIO                                                   *
***********************************************************************
DEFINE PROGRAM(EQA50XIO)
       GROUP(EQA)
       DATALOC(ANY)
       CEDF(NO)
***********************************************************************
* Program: EQAUEDAT                                                   *
***********************************************************************
DEFINE PROGRAM(EQAUEDAT)
       GROUP(EQA)
       DATALOC(ANY)
       CEDF(NO)
***********************************************************************
* Program: EQAVRM                                                 @C3A*
***********************************************************************
DEFINE PROGRAM(EQAVRM)   
       GROUP(EQA)
       DATALOC(ANY)
       CEDF(NO)
***********************************************************************
* Program: IEQAMSGT (alias of EQARCMG)    Deprecated ... not used @Y4C*
***********************************************************************
DEFINE PROGRAM(IEQAMSGT)
       GROUP(EQA)
       DATALOC(ANY)
       CEDF(NO)
***********************************************************************
* Program: UEQAMSGT (alias of EQA00MSG)                               *
***********************************************************************
DEFINE PROGRAM(UEQAMSGT)
       GROUP(EQA)
       DATALOC(ANY)
       CEDF(NO)
***********************************************************************
* Program: EQANCDBG                                                   *
***********************************************************************
DEFINE PROGRAM(EQANCDBG)
       GROUP(EQA)
       DATALOC(ANY)
       CEDF(NO)
***********************************************************************
* Program: EQANCTER                                                   *
***********************************************************************
DEFINE PROGRAM(EQANCTER)
       GROUP(EQA)
       DATALOC(ANY)
       CEDF(NO)
***********************************************************************
* Program: EQANCLDE                                                   *
***********************************************************************
DEFINE PROGRAM(EQANCLDE)
       GROUP(EQA)
       DATALOC(ANY)
       CEDF(NO)
***********************************************************************
* Program: EQANCREL                                                   *
***********************************************************************
DEFINE PROGRAM(EQANCREL)
       GROUP(EQA)
       DATALOC(ANY)
       CEDF(NO)
***********************************************************************
* Program: EQANCABE                                                   *
***********************************************************************
DEFINE PROGRAM(EQANCABE)
       GROUP(EQA)
       DATALOC(ANY)
       CEDF(NO)
***********************************************************************
* Program: EQANCDTR                                                   *
***********************************************************************
DEFINE PROGRAM(EQANCDTR)
       GROUP(EQA)
       DATALOC(ANY)
       CEDF(NO)
***********************************************************************
* Program: EQANCHAB                                                   *
***********************************************************************
DEFINE PROGRAM(EQANCHAB)
       GROUP(EQA)
       DATALOC(ANY)
       CEDF(NO)
***********************************************************************
* Program: EQANCEST                                                   *
***********************************************************************
DEFINE PROGRAM(EQANCEST)
       GROUP(EQA)
       DATALOC(ANY)
       CEDF(NO)
***********************************************************************
* Program: EQANCFRM                                                   *
***********************************************************************
DEFINE PROGRAM(EQANCFRM)
       GROUP(EQA)
       DATALOC(ANY)
       CEDF(NO)
***********************************************************************
* Program: EQANCMOD                                                   *
***********************************************************************
DEFINE PROGRAM(EQANCMOD)
       GROUP(EQA)
       DATALOC(ANY)
       CEDF(NO)
***********************************************************************
* Program: EQANCDTX - DTCX Activate Non-LE exits                      *
***********************************************************************
DEFINE PROGRAM(EQANCDTX)
       GROUP(EQA)
       DATALOC(ANY)
       EXECKEY(CICS)
       CEDF(NO)
***********************************************************************
* Program: EQANCFTC - XPCFTCH Global User Exit                        *
***********************************************************************
DEFINE PROGRAM(EQANCFTC)
       GROUP(EQA)
       DATALOC(ANY)
       CEDF(NO)
       CONCURRENCY(THREADSAFE)
***********************************************************************
* Program: EQANCX-- - XEIIN/XEIOUT Global User Exits                  *
***********************************************************************
DEFINE PROGRAM(EQANCXEI)
       GROUP(EQA)
       DATALOC(ANY)
       CEDF(NO)
       CONCURRENCY(THREADSAFE)
DEFINE PROGRAM(EQANCXIN)
       GROUP(EQA)
       DATALOC(ANY)
       CEDF(NO)
       CONCURRENCY(THREADSAFE)
DEFINE PROGRAM(EQANCXOU)
       GROUP(EQA)
       DATALOC(ANY)
       CEDF(NO)
       CONCURRENCY(THREADSAFE)
***********************************************************************
* Program: EQANCD-- - XDLIPRE/XDLIPOST Global User Exits          @00A*
***********************************************************************
DEFINE PROGRAM(EQANCDLI)
       GROUP(EQA)
       DATALOC(ANY)
       CEDF(NO)
       CONCURRENCY(THREADSAFE)
DEFINE PROGRAM(EQANCDPR)
       GROUP(EQA)
       DATALOC(ANY)
       CEDF(NO)
       CONCURRENCY(THREADSAFE)
DEFINE PROGRAM(EQANCDPO)
       GROUP(EQA)
       DATALOC(ANY)
       CEDF(NO)
       CONCURRENCY(THREADSAFE)
***********************************************************************
* Program: EQANCXAB - XPCTA Global User Exit                          *
***********************************************************************
DEFINE PROGRAM(EQANCXAB)
       GROUP(EQA)
       DATALOC(ANY)
       CEDF(NO)
       CONCURRENCY(THREADSAFE)
***********************************************************************
* Program: EQANCXHA - XPCHAIR Global User Exit                        *
***********************************************************************
DEFINE PROGRAM(EQANCXHA)
       GROUP(EQA)
       DATALOC(ANY)
       CEDF(NO)
       CONCURRENCY(THREADSAFE)
***********************************************************************
* Program: BZUXCLCT - Data collection in GLUEs                        *
***********************************************************************
DEFINE PROGRAM(BZUXCLCT)
       GROUP(EQA)
       DATALOC(ANY)
       CEDF(NO)
       CONCURRENCY(THREADSAFE)
***********************************************************************
*                                                                 @TID*
***********************************************************************
***********************************************************************
* Program: EQANCRET - nonLE RETURN router                         @T8A*
***********************************************************************
DEFINE PROGRAM(EQANCRET)
       GROUP(EQA)
       DATALOC(ANY)
       CEDF(NO)
***********************************************************************
* Program: EQAOPTS                                                    *
***********************************************************************
DEFINE PROGRAM(EQAOPTS)
       GROUP(EQA)
       DATALOC(ANY)
       CEDF(NO)
***********************************************************************
* Program: EQASTMEM                                               @T4A*
***********************************************************************
DEFINE PROGRAM(EQASTMEM)
       GROUP(EQA)
       EXECKEY(CICS)
       DATALOC(ANY)
       CEDF(NO)
***********************************************************************
* Program: EQASTMOD                                               @T4A*
***********************************************************************
DEFINE PROGRAM(EQASTMOD)
       GROUP(EQA)
       DATALOC(ANY)
       CEDF(NO)
***********************************************************************
* Program: EQASTMP                                                @T4A*
***********************************************************************
DEFINE PROGRAM(EQASTMP)
       GROUP(EQA)
       DATALOC(ANY)
       CEDF(NO)
***********************************************************************
* Program: EQASTMSG                                               @T4A*
***********************************************************************
DEFINE PROGRAM(EQASTMSG)
       GROUP(EQA)
       DATALOC(ANY)
       CEDF(NO)
***********************************************************************
* Program: EQA0CPLT - Misc PLT init program for z/OS Debugger         *
***********************************************************************
DEFINE PROGRAM(EQA0CPLT)
       GROUP(EQA)
       DATALOC(ANY)
       CEDF(NO)
***********************************************************************
* Program: EQA0CDTP - DTCP NEWPROG Activate/Deactivate trans          *
***********************************************************************
DEFINE PROGRAM(EQA0CDTP)
       GROUP(EQA)
       CEDF(NO)
**********************************************************************
* Program: SYS1.MIGLIB(IEWBIND)                                      *
**********************************************************************
DEFINE PROGRAM(IEWBIND)
       EXECKEY(CICS)             CONCURRENCY(THREADSAFE)
       DATALOC(ANY)              CEDF(NO)
       GROUP(EQA)
**********************************************************************
* Program: SYS1.MIGLIB(IEWBFDAT)                                     *
**********************************************************************
DEFINE PROGRAM(IEWBFDAT)
       EXECKEY(CICS)             CONCURRENCY(THREADSAFE)
       DATALOC(ANY)              CEDF(NO)
       GROUP(EQA)
**********************************************************************
* Program: EQAW3Z3                                               @TMA*
**********************************************************************
DEFINE PROGRAM(EQAW3Z3)
       DESCRIPTION(Windows-IA32 31 bit engine)
       EXECKEY(CICS)
       CONCURRENCY(THREADSAFE)
       DATALOC(ANY)
       CEDF(NO)
       GROUP(EQA)
**********************************************************************
* Program: EQAW3Z6                                               @TMA*
**********************************************************************
DEFINE PROGRAM(EQAW3Z6)
       DESCRIPTION(Windows-IA32 64 bit engine)
       EXECKEY(CICS)
       CONCURRENCY(THREADSAFE)
       DATALOC(ANY)
       CEDF(NO)
       GROUP(EQA)
**********************************************************************
* Program: EQAL6Z3                                               @TMA*
**********************************************************************
DEFINE PROGRAM(EQAL6Z3)
       DESCRIPTION(LINUX-X86-64 31 bit engine)
       EXECKEY(CICS)
       CONCURRENCY(THREADSAFE)
       DATALOC(ANY)
       CEDF(NO)
       GROUP(EQA)
**********************************************************************
* Program: EQAL6Z6                                               @TMA*
**********************************************************************
DEFINE PROGRAM(EQAL6Z6)
       DESCRIPTION(LINUX-X86-64 64 bit engine)
       EXECKEY(CICS)
       CONCURRENCY(THREADSAFE)
       DATALOC(ANY)
       CEDF(NO)
       GROUP(EQA)
***********************************************************************
* Transaction: CDT#                                               @TGC*
* z/OS Debugger / Screen display transaction.                         *
* Used in Separate Terminal Mode.                                     *
* Note: Users need authority to START this transaction.               *
***********************************************************************
DEFINE TRANSACTION(CDT#)
       PROGRAM(EQA10XSC)
       TASKDATALOC(ANY)
       DYNAMIC(YES)
       GROUP(EQA)
***********************************************************************
* Transaction: DTSC                                               @T9A*
* z/OS Debugger / Screen Control transaction.                         *
* Used in Screen Control Mode.                                        *
***********************************************************************
DEFINE TRANSACTION(DTSC)
       PROGRAM(EQA10XSC)
       TASKDATALOC(ANY)
       GROUP(EQA)
***********************************************************************
* Transaction: DTCN                                                   *
***********************************************************************
DEFINE TRANSACTION(DTCN)
       GROUP(EQA)
       PROGRAM(EQADCCNM)
       TASKDATALOC(ANY)
***********************************************************************
* Transaction: DTCD                                                   *
***********************************************************************
DEFINE TRANSACTION(DTCD)
       GROUP(EQA)
       PROGRAM(EQADCVSM)
       TASKDATALOC(ANY)
***********************************************************************
* Transaction: DTCI                                                   *
***********************************************************************
DEFINE TRANSACTION(DTCI)
       GROUP(EQA)
       PROGRAM(EQADCVSM)
       TASKDATALOC(ANY)
***********************************************************************
* Transaction: DTCX - Non-LE assembler support                        *
***********************************************************************
DEFINE TRANSACTION(DTCX)
       GROUP(EQA)
       PROGRAM(EQANCDTX)
       TASKDATALOC(ANY)
***********************************************************************
* Transaction: DTCP - Activate (DTCPO)/Deactivate (DTCPF) NEWPROG     *
***********************************************************************
DEFINE TRANSACTION(DTCP)
       GROUP(EQA)
       PROGRAM(EQA0CDTP)
       TASKDATALOC(ANY)
***********************************************************************
* Transaction: DTSI - Diagnostic information for DT               @T5A*
***********************************************************************
DEFINE TRANSACTION(DTSI)
       GROUP(EQA)
       PROGRAM(EQA0CINF)
       TASKDATALOC(ANY)
***********************************************************************
* Transaction: DTNP - Newcopy Program                             @TFA*
***********************************************************************
DEFINE TRANSACTION(DTNP)
       GROUP(EQA)
       PROGRAM(EQANPCNM)
       TASKDATALOC(ANY)
***********************************************************************
* Transaction: DTST - Storage utility                             @T4A*
***********************************************************************
DEFINE TRANSACTION(DTST)
       GROUP(EQA)
       PROGRAM(EQASTMOD)
       TASKDATALOC(ANY)
***********************************************************************
* RDO definitions for z/OS Debugger                                   *
* Extrapartition Transient Data Queues                                *
*                                                                     *
* Note that DD cards for these queues should NOT be added to the      *
* CICS start-up deck.                                             @TMA*
***********************************************************************
           DEFINE TDQUEUE(CINI)
                  TYPE(EXTRA)
                  DDNAME(CINSPIN)
                  TYPEFILE(INPUT)
                  OPENTIME(DEFERRED)
                  GROUP(EQA)
           DEFINE TDQUEUE(CINL)
                  TYPE(EXTRA)
                  DDNAME(CINSPLS)
                  TYPEFILE(INPUT)
                  OPENTIME(DEFERRED)
                  GROUP(EQA)
           DEFINE TDQUEUE(CINO)
                  TYPE(EXTRA)
                  DDNAME(CINSPOT)
                  TYPEFILE(OUTPUT)
                  OPENTIME(DEFERRED)
                  GROUP(EQA)
***********************************************************************
* RDO definitions for z/OS Debugger Intrapartition TD Queues      @TJA*
***********************************************************************
           DEFINE TDQUEUE(EQAL)
                  TYPE(INDIRECT)
                  INDIRECTNAME(CSSL)
                  GROUP(EQA)
***********************************************************************
* RDO definitions for COBOL sidefiles                                 *
*---------------------------------------------------------------------*
* In order to use Cobol sidefiles, the CIGZ TD queue MUST be defined. *
* Note that this queue may also be defined by Language Environment.   *
* If this definition conflicts with Language Environment's, you may   *
* omit this definition by commenting it out.                          *
* If you do not use COBOL sidefiles, you may omit this definition     *
* by commenting it out.                                               *
***********************************************************************
           DEFINE TDQUEUE(CIGZ)
                  TYPE(EXTRA)
                  DDNAME(IGZDBGIN)
                  TYPEFILE(INPUT)
                  OPENTIME(DEFERRED)
                  GROUP(EQA)
           DEFINE TDQUEUE(EQAM)
                  DESCRIPTION(Integrated debugger file/IO COBOL queue)
                  TYPE(EXTRA)
                  DDNAME(EQADBGM)
                  TYPEFILE(INPUT)
                  OPENTIME(DEFERRED)
                  GROUP(EQA)
***********************************************************************
* RDO definitions for PL/I sidefiles                                  *
*---------------------------------------------------------------------*
* In order to use PL/I sidefiles, the CIBM TD queue MUST be defined.  *
* Note that this queue may also be defined by Language Environment.   *
* If this definition conflicts with Language Environment's, you may   *
* omit this definition by commenting it out.                          *
* If you do not use PL/I sidefiles, you may omit this definition      *
* by commenting it out.                                               *
***********************************************************************
           DEFINE TDQUEUE(CIBM)
                  TYPE(EXTRA)
                  DDNAME(IBMDBGIN)
                  TYPEFILE(INPUT)
                  OPENTIME(DEFERRED)
                  GROUP(EQA)
           DEFINE TDQUEUE(EQAD)
                  DESCRIPTION(Integrated debugger file/IO PL/I queue)
                  TYPE(EXTRA)
                  DDNAME(EQADBGDS)
                  TYPEFILE(INPUT)
                  OPENTIME(DEFERRED)
                  GROUP(EQA)
***********************************************************************
* RDO definitions for DTCN VSAM profiles dataset                  @y6c*
*---------------------------------------------------------------------*
***********************************************************************
           DEFINE FILE(EQADPFMB)
                  DES(Debugging profiles base file - VSAM DTCN)
                  RL(NO)
                  READI(UNCOMMITTED)
                  STR(010)
                  STATUS(ENABLED)
                  OPENTIME(FIRSTREF)
                  DISP(SHARE)
                  RECORDF(V)
                  ADD(YES)
                  BROWSE(YES)
                  DELETE(YES)
                  READ(YES)
                  UPDATE(YES)
                  JNLSYNCWRITE(NO)
                  GROUP(EQA)
***********************************************************************
* Enable the two defines below if you need to use CADP                *
*                                                                     *
* Change #cicsvsam (x2) to the high level qualifier(s) of the CICS    *
* DFHDPFMB and DFHDPFMP data sets. The sample CICS region setup       *
* job, cicshlq.SDFHINST(DFHDEFDS), places these data sets in          *
* @dsindex@.CICS@regname@.DFHDPFM*.                                   *
*                                                                     *
* CADP - Define base file for Debugging Profiles (non-RLS)            *
***********************************************************************
*DEFINE FILE(DFHDPFMB)
*       DESCRIPTION(Debugging Profiles Base File)
*       RLSACCESS(NO)             LSRPOOLID(1)
*       READINTEG(UNCOMMITTED)    DSNSHARING(ALLREQS)
*       STRINGS(10)               STATUS(ENABLED)
*       OPENTIME(FIRSTREF)        DISPOSITION(SHARE)
*       DATABUFFERS(11)           INDEXBUFFERS(10)
*       TABLE(NO)                 RECORDFORMAT(V)
*       ADD(YES)                  BROWSE(YES)
*       DELETE(YES)               READ(YES)
*       UPDATE(YES)               JOURNAL(NO)
*       JNLREAD(NONE)             JNLSYNCREAD(NO)
*       JNLUPDATE(NO)             JNLADD(NONE)
*       JNLSYNCWRITE(NO)          RECOVERY(NONE)
*       FWDRECOVLOG(NO)           BACKUPTYPE(STATIC)
*       GROUP(EQA)
*       DSNAME(#cicsvsam.DFHDPFMB)
***********************************************************************
* CADP - Define path file for Debugging Profiles (non-RLS)            *
***********************************************************************
*DEFINE FILE(DFHDPFMP)
*       DESCRIPTION(Debugging Profiles Path File)
*       RLSACCESS(NO)             LSRPOOLID(1)
*       READINTEG(UNCOMMITTED)    DSNSHARING(ALLREQS)
*       STRINGS(10)               STATUS(ENABLED)
*       OPENTIME(FIRSTREF)        DISPOSITION(SHARE)
*       DATABUFFERS(11)           INDEXBUFFERS(10)
*       TABLE(NO)                 RECORDFORMAT(V)
*       ADD(YES)                  BROWSE(YES)
*       DELETE(YES)               READ(YES)
*       UPDATE(YES)               JOURNAL(NO)
*       JNLREAD(NONE)             JNLSYNCREAD(NO)
*       JNLUPDATE(NO)             JNLADD(NONE)
*       JNLSYNCWRITE(NO)          RECOVERY(NONE)
*       FWDRECOVLOG(NO)           BACKUPTYPE(STATIC)
*       GROUP(EQA)
*       DSNAME(#cicsvsam.DFHDPFMP)
***********************************************************************
* Definitions for Managing Debug Profiles                             *
***********************************************************************
* Only one set of resources must be defined at a time:                *
*  - TCPIPSERVICE and URIMAP resource definitions for DTCN API        *
*  - CONNECTION and SESSIONS resource definitions for CICS interface  *
*    EXCI                                                             *
*---------------------------------------------------------------------*
* TCPIPSERVICE and URIMAP                                         @K2C*
*---------------------------------------------------------------------*
* Enable the two defines below if you need to use any of the      @K2C*
* following:                                                      @K2C*
*   - z/OS Debugger Profiles view                                     *
*   - Debug Profile Service API                                       *
*   - DTCN Profiles view                                              *
*   - DTCN API                                                        *
*                                                                     *
* Note: Change #port to an unused TCP/IP port that the DTCN API       *
*       uses for HTTP communication.                                  *
* Note: To see the port numbers used by CICS regions,             @A1A*
*       run the following TSO command.                            @A1A*
*       Change #cicsjobname with desired naming pattern           @A1A*
*       TSO NETSTAT CO (CLI #cicsjobname*                         @A1A*
*---------------------------------------------------------------------*
 DEFINE TCPIPSERVICE(EQADTCN)
       DESCRIPTION(zDebug DTCN API)
       AUTHENTICATE(NO)                 BACKLOG(30)
       MAXDATALEN(032768)               PORTNUMBER({{ dtcn_port }})
       PROTOCOL(HTTP)                   SOCKETCLOSE(NO)
       SSL(NO)                          STATUS(OPEN)
       TRANSACTION(CWXN)                URM(EQADCAN0)
       GROUP(EQA)
 DEFINE URIMAP(EQAURIM)
       DESCRIPTION(zDebug DTCN URI MAP)
       USAGE(SERVER)                    SCHEME(HTTP)
       HOST(*)                          STATUS(ENABLED)
       ANALYZER(YES)                    PATH(/dtcn/*)
       GROUP(EQA)
*---------------------------------------------------------------------*
* CONNECTION and SESSIONS                                             *
*---------------------------------------------------------------------*
* Enable the two defines below if you need to use any of the          *
* following:                                                          *
*   - z/OS Debugger Profiles view                                     *
*   - Debug Profile Service API                                       *
*   - DTCN Profiles view                                              *
*---------------------------------------------------------------------*
*DEFINE CONNECTION(DPSC)
*       DESCRIPTION(DPS API EXCI Connection)
*       NETNAME(EQAPROF)                INSERVICE(YES)
*       ACCESSMETHOD(IRC)               ATTACHSEC(LOCAL)
*       PROTOCOL(EXCI)
*       CONNTYPE(SPECIFIC)
*       GROUP(EQA)
*DEFINE SESSIONS(DPSC)
*       DESCRIPTION(DPS API EXCI Sessions Definition)
*       CONNECTION(DPSC)                RECEIVEPFX(<)
*       PROTOCOL(EXCI)                  RECEIVECOUNT(200)
*       GROUP(EQA)
***********************************************************************
* Add IVT program CICIVCT and transaction IVCT                        *
***********************************************************************
 DEFINE PROGRAM(CICIVCT)
        GROUP(EQA)
 DEFINE TRANSACTION(IVCT)
        PROGRAM(CICIVCT)
        GROUP(EQA)
***********************************************************************
* Add the group to a GRPLIST list.                                    *
* Change the LIST operand to a LIST which is in your CICS GRPLIST.    *
***********************************************************************
           ADD    GROUP(EQA) LIST(EQALIST)
/*
