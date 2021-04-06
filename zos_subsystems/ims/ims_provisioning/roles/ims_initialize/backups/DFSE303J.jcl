{{ JOB_CARD }}
//* SKELETON: DFSIXSOZ
//*
//* FUNCTION: ADD CSL CONTROL STATEMENT MEMBERS TO IMS PROCLIB
//*********************************************************************
//*
//*
//********************************************************************
//PROCUPDT PROC MBR=TEMPNAME
//*
//P        EXEC PGM=IEBGENER
//SYSPRINT DD SYSOUT=*
//SYSUT2   DD DISP=SHR,
//            DSN={{ DFS_AUTH_LIB_HLQ1 }}.{{ DFS_AUTH_LIB_HLQ2 }}.PROCLIB(&MBR)
//SYSIN    DD DUMMY
//        PEND
//********************************************************************
//*
//* THE FOLLOWING STEP ADDS THE BPECONFG MEMBER TO IMS PROCLIB.
//* THIS MEMBER DEFINES THE CONFIGURATION FILE FOR BPE WITH
//* CQS, OM, RM, SCI.
//*
//BPECONFG EXEC PROC=PROCUPDT,MBR=BPECONFG
//P.SYSUT1 DD *
*--------------------------------------------------------------------*
* CONFIGURATION FILE FOR BPE WITH CQS, OM, RM, SCI - BPECONFG        *
*--------------------------------------------------------------------*

LANG=ENU                             /* LANGUAGE FOR MESSAGES      */
                                     /* (ENU = U.S. ENGLISH)       */

#
# DEFINITIONS FOR BPE SYSTEM TRACES
#

TRCLEV=(*,LOW,BPE)                   /* DEFAULT ALL TRACES TO LOW  */

# NOTE: KEEP THE FOLLOWING FOR COMPATIBILITY WITH 6.1 BPE

TRCLEV=(STG,LOW,BPE)                 /* STORAGE TRACE              */
TRCLEV=(CBS,LOW,BPE)                 /* CONTROL BLK SRVCS TRACE    */
TRCLEV=(DISP,LOW,BPE)                /* DISPATCHER TRACE           */
TRCLEV=(AWE,LOW,BPE)                 /* AWE SERVER TRACE           */
TRCLEV=(LATC,LOW,BPE)                /* LATCH TRACE                */
TRCLEV=(SSRV,LOW,BPE)                /* SYSTEM SERVICES TRACE      */
TRCLEV=(USRX,LOW,BPE)                /* USER EXIT SERVICES TRACE   */

#
# DEFINITIONS FOR CQS TRACES
#

TRCLEV=(*,LOW,CQS)                   /* DEFAULT ALL TRACES TO LOW  */

# NOTE: KEEP THE FOLLOWING FOR COMPATIBILITY WITH 6.1 BPE

TRCLEV=(CQS,LOW,CQS)                 /* CQS ADDRESS SPACE TRACE    */
TRCLEV=(STR,LOW,CQS)                 /* STRUCTURE TRACE            */
TRCLEV=(INTF,LOW,CQS)                /* INTERFACE TRACE            */

#
# DEFINITIONS FOR OM/RM/SCI TRACES
#

TRCLEV=(*,LOW,OM)                    /* DEFAULT OM TRACES TO LOW   */
TRCLEV=(*,LOW,RM)                    /* DEFAULT RM TRACES TO LOW   */
TRCLEV=(*,LOW,SCI)                   /* DEFAULT SCI TRACES TO LOW  */

*--------------------------------------------------------------------*
* END OF MEMBER BPECONFG                                             *
*--------------------------------------------------------------------*
/*
//*
//* THE FOLLOWING STEP ADDS THE CSLOI000 MEMBER TO IMS PROCLIB.
//* THIS MEMBER DEFINES THE OM INITIALIZATION PROCLIB MEMBER.
//*
//CSLOI000 EXEC PROC=PROCUPDT,MBR=CSLOI000
//P.SYSUT1 DD *
*--------------------------------------------------------------------*
* OM INITIALIZATION PROCLIB MEMBER - CSLOI000                        *
*--------------------------------------------------------------------*

ARMRST=N,                      /* SHOULD ARM RESTART OM ON FAILURE  */
CMDLANG=ENU,                   /* USE ENGLISH FOR COMMMAND DESC     */
CMDSEC=N,                      /* NO COMMAND SECURITY               */
OMNAME={{ DFS_IMS_SSID[-1] }}OM1,                    /* OM NAME (OMID = OM1OM)            */
IMSPLEX(
  NAME={{ DFS_IMSPlex }},                  /* IMSPLEX NAME (CSLPLEX1)           */
  AUDITLOG=SYSLOG.OM2Q01.LOG), /* MVS LOG STREAM                    */
CMDTEXTDSN={{ DFS_AUTH_LIB_SYSHLQ1}}.{{ DFS_AUTH_LIB_SYSHLQ2 }}.SDFSDATA /* CMD TEXT DATASET            */

*--------------------------------------------------------------------*
* END OF MEMBER CSLOI000                                             *
*--------------------------------------------------------------------*
/*
//*
//* THE FOLLOWING STEP ADDS THE CSLRI000 MEMBER TO IMS PROCLIB.
//* THIS MEMBER DEFINES THE RM INITIALIZATION PROCLIB MEMBER.
//*
//CSLRI000 EXEC PROC=PROCUPDT,MBR=CSLRI000
//P.SYSUT1 DD *
*--------------------------------------------------------------------*
* RM INITIALIZATION PROCLIB MEMBER.                                  *
*--------------------------------------------------------------------*

ARMRST=N,                      /* SHOULD ARM RESTART RM ON FAILURE  */
RMNAME={{ DFS_IMS_SSID[-1] }}RM1,                    /* RM NAME (RMID = RM1RM)            */
IMSPLEX(NAME={{ DFS_IMSPlex }})            /* IMSPLEX NAME (CSLPLEX1)           */

*--------------------------------------------------------------------*
* END OF MEMBER CSLRI000                                             *
*--------------------------------------------------------------------*
/*
//*
//* THE FOLLOWING STEP ADDS THE CSLSI000 MEMBER TO IMS PROCLIB.
//* THIS MEMBER DEFINES THE SCI INITIALIZATION PROCLIB MEMBER.
//*
//CSLSI000 EXEC PROC=PROCUPDT,MBR=CSLSI000
//P.SYSUT1 DD *
*--------------------------------------------------------------------*
* SCI INITIALIZATION PROCLIB MEMBER.                                 *
*--------------------------------------------------------------------*

ARMRST=N,                     /* SHOULD ARM RESTART SCI ON FAILURE  */
SCINAME={{ DFS_IMS_SSID[-1] }}SCI1,                 /* SCI NAME (SCIID = SCI1SCI)         */
IMSPLEX(NAME={{ DFS_IMSPlex }})           /* IMSPLEX NAME (CSLPLEX1)            */

*--------------------------------------------------------------------*
* END OF MEMBER CSLSI000                                             *
*--------------------------------------------------------------------*
/*
//*
//* THE STEP ADDING DFSCG000 MEMBER TO IMS PROCLIB IS OBSOLETE AND
//* IT HAS BEEN REMOVED. SEE DFSDFxxx FOR THE EQUIVALENT DEFINITIONS
//*
/*
//*
//* THE STEP THAT ADDS THE DFSCGECE MEMBER TO IMS PROCLIB
//* HAS BEEN REMOVED. IT IS OBSOLETE AND REPLACED BY DFSDFxxx
//*
//*
//* THE FOLLOWING STEP ADDS THE RM PROC MEMBER TO IMS PROCLIB
//* THIS MEMBER DEFINES THE IMS RESOURCE MANAGER LAYER PROCLIB
//* MEMBER
//* SKELETON: BASED ON DFSIXST9
//*
//*********************************************************************
//{{ DFS_IMS_SSID }}RM EXEC PROC=PROCUPDT,MBR={{ DFS_IMS_SSID }}RM
//P.SYSUT1 DD DATA,DLM=$$
//*
//*------------------------------------------------------------------*
//*   RM                                                             *
//*------------------------------------------------------------------*
//*     PARAMETERS:                                                  *
//*     BPECFG  - NAME OF BPE MEMBER                                 *
//*     RMINIT  - SUFFIX FOR YOUR CSLRIXXX MEMBER                    *
//*     ARMRST  - INDICATES IF ARM SHOULD BE USED                    *
//*     RMNAME  - NAME OF THE RM BEING STARTED                       *
//*------------------------------------------------------------------*
//*
//IEFPROC  EXEC PGM=BPEINI00,REGION=3000K,
//  PARM=('BPECFG=BPECONFG','BPEINIT=CSLRINI0','RMINIT=000',
//        'ARMRST=N','RMNAME={{ DFS_IMS_SSID[-1] }}RM1')
//*
//STEPLIB  DD  DSN={{ DFS_AUTH_LIB_HLQ1 }}.{{ DFS_AUTH_LIB_HLQ2 }}.SDFSRESL,DISP=SHR
//*
//PROCLIB  DD  DSN={{ DFS_AUTH_LIB_HLQ1 }}.{{ DFS_AUTH_LIB_HLQ2 }}.PROCLIB,DISP=SHR
//*
//SYSPRINT DD  SYSOUT=*
//SYSUDUMP DD  SYSOUT=*
//*
$$
//* THE FOLLOWING STEP ADDS THE SCI PROC MEMBER TO IMS PROCLIB
//* THIS MEMBER DEFINES THE IMS COMMON SERVICE LAYER PROCLIB
//* MEMBER
//*
//*
//{{ DFS_IMS_SSID }}SCI EXEC PROC=PROCUPDT,MBR={{ DFS_IMS_SSID }}SCI
//P.SYSUT1 DD DATA,DLM=$$
//*------------------------------------------------------------------*
//*   SCI                                                            *
//*------------------------------------------------------------------*
//*     PARAMETERS:                                                  *
//*     BPECFG  - NAME OF BPE MEMBER                                 *
//*     SCIINIT - SUFFIX FOR YOUR CSLSIXXX MEMBER                    *
//*     ARMRST  - INDICATES IF ARM SHOULD BE USED                    *
//*     SCINAME - NAME OF THE SCI BEING STARTED                      *
//*------------------------------------------------------------------*
//*
//IEFPROC  EXEC PGM=BPEINI00,REGION=3000K,
//  PARM=('BPECFG=BPECONFG','BPEINIT=CSLSINI0','SCIINIT=000',
//            'ARMRST=N','SCINAME={{ DFS_IMS_SSID[-1] }}SCI1')
//*
//STEPLIB  DD  DSN={{ DFS_AUTH_LIB_HLQ1 }}.{{ DFS_AUTH_LIB_HLQ2 }}.SDFSRESL,DISP=SHR
//*
//PROCLIB  DD  DSN={{ DFS_AUTH_LIB_HLQ1 }}.{{ DFS_AUTH_LIB_HLQ2 }}.PROCLIB,DISP=SHR
//*
//SYSPRINT DD  SYSOUT=*
//SYSUDUMP DD  SYSOUT=*
//*
$$
//*
//*
//* THE FOLLOWING STEP ADDS THE OM PROC MEMBER TO IMS PROCLIB
//* THIS MEMBER DEFINES THE IMS COMMON SERVICE LAYER PROCLIB
//* MEMBER
//*
//*
//{{ DFS_IMS_SSID }}OM EXEC PROC=PROCUPDT,MBR={{ DFS_IMS_SSID }}OM
//P.SYSUT1 DD DATA,DLM=$$
//*------------------------------------------------------------------*
//*   OM                                                             *
//*------------------------------------------------------------------*
//*     PARAMETERS:                                                  *
//*     BPECFG  - NAME OF BPE MEMBER                                 *
//*     OMINIT  - SUFFIX FOR YOUR CSLOIXXX MEMBER                    *
//*     ARMRST  - INDICATES IF ARM SHOULD BE USED                    *
//*     CMDLANG - LANGUAGE FOR COMMAND DESCRIPTION TEXT              *
//*     CMDSEC  - COMMAND SECURITY METHOD                            *
//*     OMNAME  - NAME OF THE OM BEING STARTED                       *
//*------------------------------------------------------------------*
//*
//IEFPROC  EXEC PGM=BPEINI00,REGION=3000K,
//  PARM=('BPECFG=BPECONFG','BPEINIT=CSLOINI0','OMINIT=000',
//        'ARMRST=N','CMDSEC=N','OMNAME={{ DFS_IMS_SSID[-1] }}OM1')
//*
//STEPLIB  DD  DSN={{ DFS_AUTH_LIB_HLQ1 }}.{{ DFS_AUTH_LIB_HLQ2 }}.SDFSRESL,DISP=SHR
//*
//PROCLIB  DD  DSN={{ DFS_AUTH_LIB_HLQ1 }}.{{ DFS_AUTH_LIB_HLQ2 }}.PROCLIB,DISP=SHR
//*
//SYSPRINT DD  SYSOUT=*
//SYSUDUMP DD  SYSOUT=*
//*
$$
//*
//*
//*
//* THE FOLLOWING STEP ADDS THE DFSDF000 MEMBER TO IMS PROCLIB.
//* THIS MEMBER CONTAINS PARAMETERS RELATED TO DRD.
//*
//DFSDF000 EXEC PROC=PROCUPDT,MBR=DFSDF000
//P.SYSUT1 DD *
*--------------------------------------------------------------------*
* COMMON SERVICE LAYER SECTION                                       *
*--------------------------------------------------------------------*
<SECTION=COMMON_SERVICE_LAYER>

CMDSEC=N                         /* NO CMD AUTHORIZATION CHECKING   */
IMSPLEX={{ DFS_IMSPlex }}                    /* IMSPLEX NAME                    */
MODBLKS=DYN                      /* DRD ENABLED;MODBLKS OLC DISABLE */
*
*--------------------------------------------------------------------*
* DYNAMIC RESOURCES SECTION                                          *
*--------------------------------------------------------------------*
<SECTION=DYNAMIC_RESOURCES>

AUTOEXPORT=AUTO,                 /* AUTO EXPORT RESOURCES TO RDDS   */
AUTOIMPORT=AUTO,                 /* AUTO IMPORT RESOURCES FR RDDS   */
RDDSDSN=(
  {{ DFS_AUTH_LIB_HLQ1 }}.{{ DFS_AUTH_LIB_HLQ2 }}.RDDS1,
  {{ DFS_AUTH_LIB_HLQ1 }}.{{ DFS_AUTH_LIB_HLQ2 }}.RDDS2,
  {{ DFS_AUTH_LIB_HLQ1 }}.{{ DFS_AUTH_LIB_HLQ2 }}.RDDS3)
*--------------------------------------------------------------------*
* LOGGER SECTION                                                 *
*--------------------------------------------------------------------*
<SECTION=LOGGER>
OLDSDEF=(OLDS=(00,01,02,03,04,05,99),BUFNO=005,MODE=DUAL)
WADSDEF=(WADS=(0,1,8,9))
ARC={{ARC}}
*
*--------------------------------------------------------------------*
* END OF MEMBER DFSDF000                                             *
*--------------------------------------------------------------------*
//*
//* THE FOLLOWING STEP ADDS THE CSLOIROM MEMBER USED BY OM IN
//* THE REPOSITORY INTO THE IMS PROCLIB
//*
//CSLOIROM  EXEC PROC=PROCUPDT,MBR=CSLOIROM
//P.SYSUT1 DD *
*--------------------------------------------------------------------*
* OM INITIALIZATION PROCLIB MEMBER.                                  *
*--------------------------------------------------------------------*
ARMRST=N,                      /* SHOULD ARM RESTART OM ON FAILURE  */
CMDLANG=ENU,                   /* USE ENGLISH FOR COMMMAND DESC     */
CMDSEC=N,                      /* NO COMMAND SECURITY               */
OMNAME={{ DFS_IMS_SSID[-1] }}OM1,                    /* OM NAME (OMID = OM1OM)            */
IMSPLEX(
  NAME={{ DFS_IMSPlex }},               /* IMSPLEX NAME (CSLPLEX1)           */
  AUDITLOG=SYSLOG.OM2Q01.LOG), /* MVS LOG STREAM                    */
CMDTEXTDSN={{ DFS_AUTH_LIB_SYSHLQ1}}.{{ DFS_AUTH_LIB_SYSHLQ2 }}.SDFSDATA /* CMD TEXT DATASET            */
/*
//*