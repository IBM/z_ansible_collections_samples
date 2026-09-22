//******************************************************************************
//* Copyright (c) IBM Corporation 2026
//******************************************************************************
//ACCEPT JOB CLASS=A,
//     MSGCLASS=A,REGION=0M,MSGLEVEL=(1,1),NOTIFY=&SYSUID
//ACCEPT  EXEC PGM=GIMSMP,PARM='DATE=U',REGION=0M
//SMPCSI   DD  DISP=SHR,
//             DSN={{ smphlq }}.CSI
//SMPHOLD  DD  DUMMY
//SMPCNTL  DD  *
   SET BDY(DLIB).
    ACCEPT PTFS
    GROUPEXTEND
    FORFMID(HADRH00)
    BYPASS(HOLDSYS,HOLDUSER,
    HOLDCLASS(UCLREL,ERREL)).
/*
