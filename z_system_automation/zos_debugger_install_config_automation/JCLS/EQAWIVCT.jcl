//EQAWIVCT JOB 5724-T07,MSGLEVEL=(1,1),MSGCLASS=A
//*********************************************************************
//*                                                                   *
//*  IBM z/OS Debugger                                                *
//*               17.0                                                *
//*                                                                   *
//*********************************************************************
//* Licensed Materials - Property of IBM                              *
//*                                                                   *
//* 5724-T07: IBM z/OS Debugger                                       *
//* Copyright IBM Corp. 2013, 2024 All Rights Reserved                *
//*                                                                   *
//* US Government Users Restricted Rights - Use, duplication or       *
//* disclosure restricted by GSA ADP Schedule Contract with IBM Corp. *
//*                                                                   *
//*********************************************************************
//*
//*  Dynamic Debug facility and COBOL TEST CICS IVP
//*  for Enterprise COBOL z/OS V5 and V6
//*
//*  This Install Verification Program (IVP) verifies two things:
//*    1. The COBOL TEST support for Enterprise COBOL for z/OS V5 and V6
//*       in z/OS Debugger for CICS is working.
//*    2. The z/OS Debugger SVCs are installed and working.
//*
//*  The requirements to run this IVP are:
//*     1. One of these compilers.
//*       -Enterprise COBOL for z/OS V5
//*       -Enterprise COBOL for z/OS V6
//*     2. The Language Environment and Binder service needed to
//*        support Enterprise COBOL for z/OS V5 or V6.
//*     3. A CICS region that has been customized to support
//*        z/OS Debugger.
//*
//*  The following sections in the Customization Guide must be
//*  implemented before this IVP can be run:
//*
//*    1. "Installing the z/OS Debugger SVCs" section in
//*       the "Customizing z/OS Debugger" chapter.
//*       Note: If your CICS region uses RENTPGM=PROTECT you will
//*             need to implement the optional section "Using the
//*             Authorized Debug Facility for protected programs".
//*    2. "Adding support for debugging under CICS" chapter.
//*       Note: For COBOL V5 and V6, you will need to add these 2 system data
//*       sets to the DFHRPL DD in your CICS REGION:
//*         SYS1.MIGLIB
//*         SYS1.SIEAMIGE
//*       along with these Language Environment data sets:
//*         CEE.SCEECICS
//*         CEE.SCEERUN
//*         CEE.SCEERUN2
//*
//*  IVP Instructions:
//*
//*    1. Determine if you can use the default program object name
//*       (CICIVCT) and transaction (IVCT) used in this IVP.
//*       You can use the following CICS transactions to determine
//*       if these items are already in use in your CICS region.
//*       CEDA DISPLAY GROUP(*) PROGRAM(CICIVCT)
//*       CEDA DISPLAY GROUP(*) TRANSACTION(IVCT)
//*       a. If the program name is in use, select a new program
//*          name and then change all instances of CICIVCT to that
//*          new name BEFORE you submit this JCL.
//*       b. If the transaction name is in use, select a new
//*          transaction name and then change all instances of IVCT to
//*          that new name.
//*
//*    2. Run this JCL (see instructions below) to create a program
//*       object.
//*
//*    3. Customize your CICS region startup JCL to support z/OS Debugger
//*       (per the Customization Guide, as described above), and
//*       add the program object data set created by this JCL
//*       (typically yourid.EQAIVP.LOADPDSE) to the beginning of the
//*       DFHRPL DD.
//*
//*    4. If a &sysuid..DBGTOOL.SAVESETS and/or
//*            &sysuid..DBGTOOL.SAVEBPS
//*       exist, rename them to another name before running the IVP
//*       (and then rename them back afterwards).
//*
//*    5. Start your CICS region and follow the instructions below
//*       to run the IVP under the control of z/OS Debugger.
//*
//*    6. These instructions are written assuming you are using
//*       the DTCN transaction to manage your z/OS Debugger profiles.
//*       If you are using CADP instead, then use the CADP
//*       transacton instead of the DTCN transaction in the
//*       instructions below (and make appropriate changes regarding
//*       the use of CADP versus DTCN).
//*
//*    Running this JCL:
//*
//*     This JCL does the following:
//*      1. Allocates cataloged data set.
//*      2. Compiles the COBOL test program (including running
/*          the CICS co-processor)
//*      3. Binds the COBOL test program
//*
//*    Customize this JCL:
//*
//*      1. Update the JOB card with the installation specific
//*         parameters.
//*
//*      2. If you choose to use a different prefix than the IBM
//*         supplied one for the COBOL compiler, please change the
//*         value of IGY to your chosen prefix on the
//*         // SET LNGPRFX=IGY statement.
//*
//*      3. If you choose to use a different prefix than the IBM
//*         supplied one for the Language Environment, please change
//*         the value of CEE to your chosen prefix on the
//*         // SET LIBPRFX=CEE statement.
//*
//*      4. If you choose to use a different prefix than the IBM
//*         supplied one for CICS, please change
//*         the value of CICS to your chosen prefix on the
//*         // SET CICSPRFX=CICS statement.
//*
//*      5. If you choose to use a different device than the IBM
//*         supplied one for data set allocation, please change the
//*         value of UNITDEV on the
//*         // SET UNITDEV=SYSALLDA statement.
//*
//*      7. This IVP creates and updates a data set with the name:
//*
//*         &SYSUID..EQAIVP.LOADPDSE
//*         If you choose to use a different data set prefix for this
//*         data set than the one shown, change the value of EQAIVP
//*         to your chosen prefix on the
//*         // SET EQAIVP=&SYSUID..EQAIVP statement.
//*
//*         NOTE: The CICS region will need READ access to this
//*               data set during the z/OS Debugger session:
//*               &SYSUID..EQAIVP.LOADPDSE
//*
//*    Run this JCL:
//*      Run this JCL and verify that the steps get the following
//*      return codes:
//*
//*        ALLOCC    00
//*        COMPILE   00
//*        BIND      00
//*
//*    Running the IVP under CICS:
//*
//*      1. Start the region that has been customized for z/OS Debugger
//*         and includes userid.EQAIVP.LOADPDSE in the DFHRPL DD.
//*
//*      2. Run these transactions in CICS in order to install the
//*         program and transaction:
//*          CEDA DEFINE GROUP(EQA) PROGRAM(CICIVCT)
//*          CEDA DEFINE GROUP(EQA) TRANSACTION(IVCT) PROGRAM(CICIVCT)
//*          CEDA INSTALL GROUP(EQA) PROGRAM(CICIVCT)
//*          CEDA INSTALL GROUP(EQA) TRANSACTION(IVCT)
//*
//*      3. Run the IVCT transaction to ensure that it works. When
//*         the transaction completes, you should see:
//*         IVCTCICIVCT ENDING
//*
//*      4. Run the DTCN transaction and press PF4 to save a profile
//*         and PF3 to exit DTCN.
//*
//*      5. Run the IVCT transaction. You should then see a z/OS Debugger
//*         screen appear.
//*         Notes: The date and time will differ.
//*                This output assumes that no
//*                 Global Preferences File was specified in
//*                 EQAW.SEQAMOD(EQAOPTS).
//*                The output shown in the source window will depend
//*                 on the level of the libraries you are using.
//*
//*         On a 24x80 terminal, it should be similar to this:
//*
//*         COBOL    LOCATION: CICIVCT ENTRY
//*         Command ===>
//*         MONITOR -+----1----+----2----+----3----+----4----+----5---
//*         ******************************* TOP OF MONITOR ***********
//*         ****************************** BOTTOM OF MONITOR *********
//*
//*
//*
//*
//*
//*         SOURCE: CICIVCT --1----+----2----+----3----+----4----+----
//*                3        PROGRAM-ID. CICIVCT.
//*                4
//*                5        INSTALLATION.  IBM - Silicon Valley Labora
//*                6        SECURITY.      Property of IBM Corporation
//*                7
//*                8       *******************************************
//*         LOG 0----+----1----+----2----+----3----+----4----+----5---
//*         ********************************* TOP OF LOG *************
//*         0001 IBM z/OS Debugger 17.0.n
//*         0002 11/19/2024 04:11:41 PM
//*         0003 5724-T07: Copyright IBM Corp. 1992, 2024
//*         PF  1:?          2:STEP       3:QUIT       4:LIST       5:
//*         PF  7:UP         8:DOWN       9:GO        10:ZOOM      11:
//*
//*      6. Enter this command (press Enter after the command):
//*         Notes: 1. The date and time will differ.
//*                2.  The CALL %VER command shows the build
//*                    date/time and service level (if any).
//*
//*          call %ver
//*
//*         The screen will update and you should now see a screen
//*         similar to this:
//*
//*         COBOL    LOCATION: CICIVCT ENTRY
//*         Command ===>
//*         MONITOR -+----1----+----2----+----3----+----4----+----5---
//*         ******************************* TOP OF MONITOR ***********
//*         ****************************** BOTTOM OF MONITOR *********
//*
//*
//*
//*
//*
//*         SOURCE: CICIVCT --1----+----2----+----3----+----4----+----
//*                3        PROGRAM-ID. CICIVCT.
//*                4
//*                5        INSTALLATION.  IBM - Silicon Valley Labora
//*                6        SECURITY.      Property of IBM Corporation
//*                7
//*                8       *******************************************
//*         LOG 0----+----1----+----2----+----3----+----4----+----5---
//*         0004  CALL %VER ;
//*         0005 IBM z/OS Debugger 17.0.n
//*         0006 08/19/2024 08:01:00 AM Level: 17.0.n PHnnnnn
//*         0007 5724-T07: Copyright IBM Corp. 1992, 2024
//*         PF  1:?          2:STEP       3:QUIT       4:LIST       5:
//*         PF  7:UP         8:DOWN       9:GO        10:ZOOM      11:
//*
//*      7. Enter these commands (press Enter after each command):
//*
//*          at label paraa
//*          go
//*          list prgstart
//*
//*         The screen will update and you should now see a screen
//*         similar to this:
//*
//*         COBOL    LOCATION: CICIVCT :> 47
//*         Command ===>
//*         MONITOR -+----1----+----2----+----3----+----4----+----5---
//*         ******************************* TOP OF MONITOR ***********
//*         ****************************** BOTTOM OF MONITOR *********
//*
//*
//*
//*
//*
//*         SOURCE: CICIVCT --1----+----2----+----3----+----4----+----
//*               48            EXEC CICS SEND FROM(PRGEND) LENGTH(L1)
//*               49
//*               50            EXEC CICS RETURN END-EXEC..
//*         ****************************** BOTTOM OF SOURCE **********
//*
//*
//*         LOG 0----+----1----+----2----+----3----+----4----+----5---
//*         0008  AT LABEL PARAA ;
//*         0009  GO ;
//*         0010  LIST PRGSTART ;
//*         0011 PRGSTART = 'CICIVCT STARTING    '
//*         PF  1:?          2:STEP       3:QUIT       4:LIST       5:
//*         PF  7:UP         8:DOWN       9:GO        10:ZOOM      11:
//*
//*      8. Enter this command (press Enter):
//*          QUIT DEBUG
//*         and then respond Y (and press Enter) to this prompt:
//*          Do you really want to terminate this session? N
//*         The screen will update and z/OS Debugger will end. You
//*         should now see:
//*         ivctCICIVCT ENDING
//*
//*      9. Run the DTCN transaction and press PF6 to delete the
//*         profile and PF3 to exit DTCN.
//*
//*     10. Run these transactions to remove the IVP program and
//*         transaction.
//*          CEDA DELETE GROUP(EQA) PROGRAM(CICIVCT)
//*          CEDA DELETE GROUP(EQA) TRANSACTION(IVCT)
//*          CEMT DISCARD PROGRAM(CICIVCT)
//*          CEMT DISCARD TRANSACTION(IVCT)
//*
//*     11. Take down your CICS region and remove the
//*         userid.EQAIVP.LOADPDSE data set from the DFHRPL DD.
//*
//*     12. This concludes this IVP.
//*
//*********************************************************************
//*                                                                   *
//* $MOD(EQAWIVCT) COMP(INST)  PROD(DBGT): z/OS Debugger CICS IVP for *
//*                               Enterprise COBOL for z/OS V5 and V6 *
//*                                                                   *
//* FLAG REASON   RLSE DATE   ORIGIN  : FLAG DESCRIPTION              *
//* ---- -------- ---- ------ -------- -----------------------------  *
//* $FC= r95644  V17R0 251111 Bershad : Fix initialization of ACCUMS  *
//* $y1= R10797  V13R1 160629 Young:    Enterprise COBOL V6           *
//* $Y0= F9641   V12R1 130116 Young:    Create                        *
//*                                                                   *
//*********************************************************************
//*
//*
//        SET LNGPRFX={{ ivp_LNGPRFX }}
//        SET LIBPRFX={{ ivp_LIBPRFX }}
//        SET CICSPRFX={{ DFHHLQ }}
//*
//        SET EQAIVP=&SYSUID..EQAIVP
//*
//        SET UNITDEV=SYSALLDA
//*
//*------------------------------------------------------------
//* 1. Allocate cataloged data set needed
//*------------------------------------------------------------
//*
//ALLOCC   EXEC PGM=IEFBR14
//LOADPDSE DD DSN=&EQAIVP..LOADPDSE,
//         DISP=(MOD,CATLG),
//         DCB=(DSORG=PO,RECFM=U,LRECL=0,BLKSIZE=32760),
//         SPACE=(CYL,(1,1)),UNIT=&UNITDEV.,DSNTYPE=LIBRARY
//*
//*------------------------------------------------------------
//* 3. Compile the test program
//*------------------------------------------------------------
//*
//COMPILE EXEC PGM=IGYCRCTL,
// PARM=('APOST,NONAME,TEST,CICS("SP COBOL3")'),
// REGION=0M
//STEPLIB  DD DISP=SHR,DSN=&LNGPRFX..SIGYCOMP
//         DD DISP=SHR,DSN=&CICSPRFX..SDFHLOAD
//         DD DISP=SHR,DSN=&LIBPRFX..SCEERUN
//         DD DISP=SHR,DSN=&LIBPRFX..SCEERUN2
//SYSPRINT DD SYSOUT=*
//SYSUT1   DD DSN=&&SYSUT1,UNIT=&UNITDEV.,DISP=(NEW,DELETE),
//            SPACE=(CYL,(1,1))
//SYSUT2   DD DSN=&&SYSUT2,UNIT=&UNITDEV.,DISP=(NEW,DELETE),
//            SPACE=(CYL,(1,1))
//SYSUT3   DD DSN=&&SYSUT3,UNIT=&UNITDEV.,DISP=(NEW,DELETE),
//            SPACE=(CYL,(1,1))
//SYSUT4   DD DSN=&&SYSUT4,UNIT=&UNITDEV.,DISP=(NEW,DELETE),
//            SPACE=(CYL,(1,1))
//SYSUT5   DD DSN=&&SYSUT5,UNIT=&UNITDEV.,DISP=(NEW,DELETE),
//            SPACE=(CYL,(1,1))
//SYSUT6   DD DSN=&&SYSUT6,UNIT=&UNITDEV.,DISP=(NEW,DELETE),
//            SPACE=(CYL,(1,1))
//SYSUT7   DD DSN=&&SYSUT7,UNIT=&UNITDEV.,DISP=(NEW,DELETE),
//            SPACE=(CYL,(1,1))
//SYSUT8   DD DSN=&&SYSUT8,UNIT=&UNITDEV.,DISP=(NEW,DELETE),
//            SPACE=(CYL,(1,1))
//SYSUT9   DD DSN=&&SYSUT9,UNIT=&UNITDEV.,DISP=(NEW,DELETE),
//            SPACE=(CYL,(1,1))
//SYSUT10  DD DSN=&&SYSUT10,UNIT=&UNITDEV.,DISP=(NEW,DELETE),
//            SPACE=(CYL,(1,1))
//SYSUT11  DD DSN=&&SYSUT11,UNIT=&UNITDEV.,DISP=(NEW,DELETE),
//            SPACE=(CYL,(1,1))
//SYSUT12  DD DSN=&&SYSUT12,UNIT=&UNITDEV.,DISP=(NEW,DELETE),
//            SPACE=(CYL,(1,1))
//SYSUT13  DD DSN=&&SYSUT13,UNIT=&UNITDEV.,DISP=(NEW,DELETE),
//            SPACE=(CYL,(1,1))
//SYSUT14  DD DSN=&&SYSUT14,UNIT=&UNITDEV.,DISP=(NEW,DELETE),
//            SPACE=(CYL,(1,1))
//SYSUT15  DD DSN=&&SYSUT15,UNIT=&UNITDEV.,DISP=(NEW,DELETE),
//            SPACE=(CYL,(1,1))
//SYSMDECK DD DSN=&&SYSMDECK,UNIT=&UNITDEV.,DISP=(NEW,DELETE),
//            SPACE=(CYL,(1,1))
//SYSLIN   DD DSN=&&OBJECT,
//            DISP=(NEW,PASS),UNIT=&UNITDEV.,
//            DCB=(DSORG=PS,RECFM=FB,LRECL=80,BLKSIZE=0),
//            SPACE=(CYL,(1,1))
//SYSIN    DD   *
       IDENTIFICATION DIVISION.

       PROGRAM-ID. CICIVCT.

       INSTALLATION.  IBM - Silicon Valley Laboratory.
       SECURITY.      Property of IBM Corporation.

      *****************************************************************
      ***                                                            **
      *** Function:                                                  **
      ***    Sample COBOL program used for z/OS Debugger IVP.        **
      ***    (Single compile unit).                                  **
      ***                                                            **
      *****************************************************************

       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.

       01  ACCUMS  COMP.
           04  ACCUM-A    PIC S9(4).
           04  ACCUM-B    PIC S9(4).

       77  PRGSTART     PIC X(20) VALUE 'DUMMY               '.
       77  PRGEND       PIC X(20) VALUE 'CICIVCT ENDING      '.
       77  L1           PIC S9(4) COMP.

       PROCEDURE DIVISION.

           MOVE 20 TO L1.
           MOVE  'CICIVCT STARTING' TO PRGSTART.
           EXEC CICS SEND FROM(PRGSTART) LENGTH(L1) END-EXEC.
           MOVE ZERO TO ACCUM-A
           MOVE ZERO TO ACCUM-B

           PERFORM  2  TIMES
               ADD  3  TO  ACCUM-A
           END-PERFORM

           PERFORM  ACCUM-A  TIMES
               ADD  4  TO  ACCUM-B
           END-PERFORM

           ADD  ACCUM-A  TO  ACCUM-B
           SUBTRACT 3 FROM  ACCUM-A
           DIVIDE ACCUM-A INTO  ACCUM-B.

       PARAA.
           EXEC CICS SEND FROM(PRGEND) LENGTH(L1) END-EXEC.

           EXEC CICS RETURN END-EXEC.
/*
//*
//*------------------------------------------------------------
//* 4. Bind step
//*------------------------------------------------------------
//*
//BIND    EXEC PGM=HEWL,PARM='CALL,LIST,XREF,LET,MAP,RENT',REGION=0M
//SYSLIB   DD DISP=SHR,DSN=&LIBPRFX..SCEELKED
//         DD DISP=SHR,DSN=&LIBPRFX..SCEELKEX
//         DD DISP=SHR,DSN=&CICSPRFX..SDFHLOAD
//OBJECT   DD DISP=(SHR,DELETE),DSN=&&OBJECT
//SYSPRINT DD SYSOUT=*
//SYSUT1   DD DISP=(NEW,DELETE),DSN=&&SYSUT1,UNIT=&UNITDEV.,
//            SPACE=(CYL,(1,1))
//SYSLMOD  DD DISP=(SHR,KEEP),DSN=&EQAIVP..LOADPDSE
//SYSLIN   DD *
     INCLUDE OBJECT
     INCLUDE SYSLIB(DFHELII)
     NAME CICIVCT(R)
/*
//*
//*                ========> END OF JOB EQAWIVCT <========
