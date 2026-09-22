//EQAWIVPT JOB 5724-T07,MSGLEVEL=(1,1),MSGCLASS=A
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
//*                                                                   *
//*  Dynamic Debug facility and COBOL TEST IVP for Ent COBOL z/OS V5  *
//*                                                               V6  *
//*                                                                   *
//*  This Install Verification Program (IVP) verifies two things:     *
//*    1. The COBOL TEST support for Enterprise COBOL for z/OS V5 and *
//*       V6 in z/OS Debugger is working.                             *
//*    2. The z/OS Debugger SVCs are installed and working.           *
//*                                                                   *
//*  A simple COBOL program is compiled using the compile option      *
//*  TEST.  The program is then executed with the TEST run time       *
//*  option so z/OS Debugger is invoked. A few z/OS Debugger commands *
//*  are then executed from a command file and the results are        *
//*  captured to a log file, which can then be compared with the      *
//*  expected results.                                                *
//*                                                                   *
//*  The requirements to run this IVP are:                            *
//*     1. One of these compilers:                                    *
//*       -Enterprise COBOL for z/OS V5                               *
//*       -Enterprise COBOL for z/OS V6                               *
//*     2. The Language Environment and Binder service needed to      *
//*        support Enterprise COBOL for z/OS V5 or V6.                *
//*     3. The installation steps described in "Installing the z/OS   *
//*        Debugger SVCs" in the z/OS Debugger Customization Guide    *
//*        have been completed.                                       *
//*                                                                   *
//*  This JCL will:                                                   *
//*     1. Allocate uncataloged temporary data sets.                  *
//*     2. Compile the simple COBOL test program                      *
//*     3. Bind the simple COBOL test program                         *
//*     4. Execute the simple COBOL test program                      *
//*                                                                   *
//*  IVP INSTRUCTIONS:                                                *
//*   Before submitting this job, the JCL must be customized          *
//*   for your installation.  The following changes need to be        *
//*   made:                                                           *
//*                                                                   *
//*    1. Update the JOB card with the installation specific          *
//*       parameters.                                                 *
//*    2. If you choose to use a different prefix than the IBM        *
//*       supplied one for the COBOL compiler, please change the      *
//*       value of IGY to your chosen prefix on the                   *
//*       // SET LNGPRFX=IGY statement.                               *
//*    3. If you choose to use a different prefix than the IBM        *
//*       supplied one for the z/OS Debugger, please change the value *
//*       of EQAW to your chosen prefix on the // SET DTPRFX=EQAW     *
//*       statement.                                                  *
//*    4. If you choose to use a different prefix than the IBM        *
//*       supplied one for the Language Environment, please change    *
//*       the value of CEE to your chosen prefix on the               *
//*       // SET LIBPRFX=CEE statement.                               *
//*       (in other words, comment the 1st, and un-comment the 2nd).  *
//*                                                                   *
//*    5. If your site has changed the Language Environment option    *
//*       CBLOPTS from its default of ON to OFF (in CEE*OPT), change  *
//*       the two lines below that look like this:                    *
//*                                                                   *
//*       //       PARM='/TEST(,CMDS,;,*) TRAP(ON)',                  *
//*       //*      PARM='TEST(,CMDS,;,*) TRAP(ON)/',                  *
//*                                                                   *
//*       to this:                                                    *
//*                                                                   *
//*       //*      PARM='/TEST(,CMDS,;,*) TRAP(ON)',                  *
//*       //       PARM='TEST(,CMDS,;,*) TRAP(ON)/',                  *
//*                                                                   *
//*       (in other words, comment the 1st, and un-comment the 2nd).  *
//*                                                                   *
//*    6. If you choose to use a different device than the IBM        *
//*       supplied one for data set allocation, please change the     *
//*       value of UNITDEV on the // SET UNITDEV=SYSALLDA statement.  *
//*    7. If a &sysuid..DBGTOOL.SAVESETS and/or                       *
//*            &sysuid..DBGTOOL.SAVEBPS                               *
//*       exist, rename them to another name before running the IVP   *
//*       (and then rename them back afterwards).                     *
//*                                                                   *
//*                                                                   *
//*  All steps in this job should complete with a return code 0       *
//*  and the following output to DDNAME INSPLOG:                      *
//*                                                                   *
//*  NOTE: The date and time will differ, but the rest of the         *
//*        output should match. This output assumes that no Global    *
//*        Preferences File was specified in EQAW.SEQAMOD(EQAOPTS).   *
//*                                                                   *
//*        The following message may appear in the output. This does  *
//*        not affect the execution of this IVP.                      *
//*                                                                   *
//*   The operating system has generated the following message:       *
//*     EQA2458I SVC Screening is disabled by EQAOPTS. Handling of    *
//*     non-LE events is not available.  Debugging of non-LE programs *
//*     will be restricted in this z/OS Debugger session.             *
//*                                                                   *
//*   * IBM z/OS Debugger 17.0.n                                      *
//*   * 11/19/2024 04:11:41 PM                                        *
//*   * 5724-T07: Copyright IBM Corp. 1992, 2024                      *
//*   * *** Commands file commands follow ***                         *
//*     COMMENT Simple test of PROGT ;                                *
//*     COMMENT show build date/time and service level (if any) ;     *
//*     CALL %VER ;                                                   *
//*   * IBM z/OS Debugger 17.0.n                                      *
//*   * 08/19/2024 08:01:00 AM Level: 17.0.n PHnnnnn                  *
//*   * 5724-T07: Copyright IBM Corp. 1992, 2024                      *
//*     QUERY DYNDEBUG ;                                              *
//*   * The setting of DYNDEBUG is ON                                 *
//*     STEP 3 ;                                                      *
//*     LIST STR1 ;                                                   *
//*   * STR1 = 'ONE  '                                                *
//*     01 TEMP PIC X(5) ;                                            *
//*     MOVE STR1 TO TEMP ;                                           *
//*     LIST TEMP ;                                                   *
//*   * TEMP = 'ONE  '                                                *
//*     AT 49                                                         *
//*       PERFORM                                                     *
//*         LIST ( "At the breakpoint for line", %LINE ) ;            *
//*         LIST ( R ) ;                                              *
//*         GO ;                                                      *
//*       END-PERFORM ;                                               *
//*     AT CHANGE STR1                                                *
//*       PERFORM                                                     *
//*         LIST STR1 ;                                               *
//*         GO ;                                                      *
//*       END-PERFORM ;                                               *
//*     AT EXIT *                                                     *
//*       LIST ( "Exiting ", %CU ) ;                                  *
//*     GO ;                                                          *
//*   * At the breakpoint for line                                    *
//*   * %LINE = '49.1'                                                *
//*   * R = 1                                                         *
//*   * STR1 = 'TOP  '                                                *
//*   * STR1 = 'BOT  '                                                *
//*   * At the breakpoint for line                                    *
//*   * %LINE = '49.1'                                                *
//*   * R = 2                                                         *
//*   * STR1 = 'TOP  '                                                *
//*   * STR1 = 'BOT  '                                                *
//*   * At the breakpoint for line                                    *
//*   * %LINE = '49.1'                                                *
//*   * R = 3                                                         *
//*   * STR1 = 'TOP  '                                                *
//*   * STR1 = 'BOT  '                                                *
//*   * At the breakpoint for line                                    *
//*   * %LINE = '49.1'                                                *
//*   * R = 4                                                         *
//*   * STR1 = 'TOP  '                                                *
//*   * STR1 = 'BOT  '                                                *
//*   * STR1 = 'DONE '                                                *
//*   * Exiting                                                       *
//*   * %CU = 'PROGT'                                                 *
//*   *  QUIT ;                                                       *
//*                                                                   *
//*********************************************************************
//*                                                                   *
//* $MOD(EQAWIVPT) COMP(INST)  PROD(DBGT): z/OS Debugger IVP for      *
//*                                Enterprise COBOL for z/OS V5 and V6*
//*                                                                   *
//* FLAG REASON   RLSE DATE   ORIGIN  : FLAG DESCRIPTION              *
//* ---- -------- ---- ------ -------- -----------------------------  *
//* $y1= R10797  V13R1 160629 Young:   Enterprise COBOL V6            *
//* $Y0= F9641   V12R1 130116 Young:   Create                         *
//*                                                                   *
//*********************************************************************
//*
//*
//        SET LNGPRFX={{ ivp_LNGPRFX }}
//        SET LIBPRFX={{ ivp_LIBPRFX }}
//        SET DTPRFX={{ eqa_hlq }}
//*
//        SET UNITDEV=SYSALLDA
//*
//*------------------------------------------------------------
//* 1. Allocate uncataloged temporary data sets needed
//*------------------------------------------------------------
//*
//ALLOC   EXEC PGM=IEFBR14
//OBJECT  DD  DSN=&&OBJECT,   -- holds object (text) from compiler
//            DISP=(NEW,PASS),UNIT=&UNITDEV.,
//            DCB=(DSORG=PO,RECFM=FB,LRECL=80,BLKSIZE=0),
//            SPACE=(CYL,(1,1,1))
//SYSLMOD DD  DSN=&&GOSET,    -- holds program object from binder
//            DISP=(NEW,PASS),UNIT=&UNITDEV.,
//            DCB=(DSORG=PO,RECFM=U,LRECL=0,BLKSIZE=32760),
//            SPACE=(CYL,(1,1)),DSNTYPE=LIBRARY
//*
//*
//*------------------------------------------------------------
//* 2. Compile the test program
//*------------------------------------------------------------
//*
//COMPILE EXEC PGM=IGYCRCTL,
// PARM=('NONAME,NOTERM,TEST'),
// REGION=0M
//STEPLIB  DD DISP=SHR,DSN=&LNGPRFX..SIGYCOMP
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
//SYSLIN   DD DISP=(SHR,PASS),DSN=&&OBJECT(PROGT)
//SYSIN    DD *
 CBL QUOTE
       Identification Division.
      *
       PROGRAM-ID.    PROGT.
      *
       Installation.  IBM - Silicon Valley Laboratory.
       Security.      Property of IBM Corporation.
      *LANGUAGE.      COBOL for OS/390
      *
      *****************************************************************
      ***                                                            **
      *** Function:                                                  **
      ***    Sample COBOL program used for z/OS Debugger testing     **
      ***    (Single compile unit).                                  **
      ***                                                            **
      *****************************************************************
      *
       ENVIRONMENT DIVISION.
         CONFIGURATION SECTION.
          SOURCE-COMPUTER.  IBM-370.
          OBJECT-COMPUTER.  IBM-370.
         INPUT-OUTPUT SECTION.
          FILE-CONTROL.
      *
      *****************************************************************
      *
       DATA DIVISION.
         FILE SECTION.
         WORKING-STORAGE SECTION.
         01     VARBL1          PIC 99    VALUE 10.
         01     VARBL2          PIC 99    VALUE 20.
         01     R               PIC 9     VALUE 1.
         01     STR1            PIC X(5).
         01     STR2            PIC X(5).
         01     STR3            PIC X(5).
         01     STR4            PIC X(5).
         01     STR5            PIC X(5).
         LINKAGE SECTION.
      *
      *****************************************************************
      *
       PROCEDURE DIVISION.
      *  MAIN.
           MOVE "ONE"   TO STR1.
           MOVE "TWO"   TO STR2.
           MOVE "THREE" TO STR3.
           MOVE "FOUR"  TO STR4.
           MOVE "FIVE"  TO STR5.
           PERFORM UNTIL R = 5
             ADD 1 TO VARBL1
             SUBTRACT 2 FROM VARBL2
             MOVE "TOP" TO STR1
             MOVE "BEG" TO STR2
             MOVE "UP"  TO STR3
             ADD 1 TO R
             MOVE "BOT" TO STR1
             MOVE "END" TO STR2
             MOVE "DOW" TO STR3
           END-PERFORM.
           MOVE "DONE" TO STR1.
           MOVE "END"  TO STR2.
           MOVE "FIN"  TO STR3.
           STOP RUN.
//*
//*------------------------------------------------------------
//* 3. Bind step
//*------------------------------------------------------------
//*
//BIND    EXEC PGM=IEWBLINK,PARM='MAP',REGION=0M
//SYSLIB   DD DISP=SHR,DSN=&LIBPRFX..SCEELKED
//         DD DISP=SHR,DSN=&LIBPRFX..SCEELKEX
//OBJECT   DD DISP=(SHR,DELETE),DSN=*.ALLOC.OBJECT
//SYSPRINT DD SYSOUT=*
//SYSUT1   DD DISP=(NEW,DELETE),DSN=&&SYSUT1,UNIT=&UNITDEV.,
//            SPACE=(CYL,(1,1))
//SYSLMOD  DD DISP=(SHR,PASS),DSN=*.ALLOC.SYSLMOD
//SYSLIN   DD *
     INCLUDE OBJECT(PROGT)
     NAME PROGT(R)
//*
//*------------------------------------------------------------
//* 4. Go go go
//*------------------------------------------------------------
//*
//INSPGO EXEC PGM=PROGT,
//       PARM='/TEST(,CMDS,;,*) TRAP(ON)',
//*      PARM='TEST(,CMDS,;,*) TRAP(ON)/',
//       REGION=0M,COND=(8,LE,BIND)
//STEPLIB  DD DISP=(SHR,PASS),DSN=*.ALLOC.SYSLMOD
//         DD DISP=SHR,DSN=&DTPRFX..SEQAMOD
//         DD DISP=SHR,DSN=&LIBPRFX..SCEERUN
//         DD DISP=SHR,DSN=&LIBPRFX..SCEERUN2
//INSPLOG  DD SYSOUT=*
//SYSPRINT DD SYSOUT=*
//SYSOUT   DD SYSOUT=*
//CMDS   DD  *
        Comment Simple test of PROGT ;
        comment show build date/time and service level (if any);
        call %ver;
        QUERY DYNDEBUG;
        STEP 3 ;
        LIST STR1 ;
        01 TEMP PIC X(5) ;
        MOVE STR1 TO TEMP ;
        LIST TEMP ;
        AT 49 PERFORM
          LIST ( "At the breakpoint for line", %LINE );
          LIST (R) ;
          GO ;
        END-PERFORM;
        AT CHANGE STR1 PERFORM
          LIST STR1 ;
          GO ;
        END-PERFORM;
        AT EXIT *
          LIST ( "Exiting ", %CU ) ;
        GO ;
        QUIT ;
//*
//*                ========> END OF JOB EQAWIVPT <========
