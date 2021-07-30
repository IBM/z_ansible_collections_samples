       CBL LIST,MAP,XREF,FLAG(I)
       IDENTIFICATION DIVISION.
       PROGRAM-ID. LOADACCT.

      ******************************************************************

      ******************************************************************

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT ACCTIN-FILE ASSIGN TO ACCTIN.
       DATA DIVISION.

       FILE SECTION.
         FD ACCTIN-FILE
            LABEL RECORDS ARE OMITTED
            RECORDING MODE IS F
            BLOCK CONTAINS 0 RECORDS
            DATA RECORD IS ACCTIN-IN.
       01  ACCTIN-IN          PIC X(200).

       WORKING-STORAGE SECTION.

      ******************************************************************
      *DATABASE CALL CODES
      ******************************************************************

       77  ISRT                PIC  X(04)        VALUE "ISRT".

      ******************************************************************
      *ERROR STATUS CODE AREA
      ******************************************************************

       01  BAD-STATUS.
           05  SC-MSG  PIC X(30) VALUE "BAD STATUS CODE WAS RECEIVED: ".
           05  SC             PIC X(2).

      ******************************************************************
      *SEGMENT SEARCH ARGUMENTS
      ******************************************************************
       01  ACCOUNT-SSA.
           05  FILLER          PIC  X(08)        VALUE "ACCOUNT ".
           05  FILLER          PIC  X(01)        VALUE ' '.

       01  ACCOUNT-SEG.
           05  ACCID           PIC  S9(18) COMP-5.
           05  ACCTYPE         PIC  X(1).
           05  BALANCE         PIC  S9(13)V9(2) COMP-3.
           05  LASTTXID        PIC  S9(18) COMP-5.

       01  TXT-ACCID           PIC  X(19).
       01  TXT-ACCTYPE         PIC  X(1).
       01  TXT-ACCTYPE-QUOTE   PIC  X(3).
       01  TXT-BALANCE         PIC  X(16).
       01  TXT-LASTTXID        PIC  X(19).

       01  INPUT-RECORDS     PIC  S9(9) COMP-5.
       77  TERM-IO             PIC 9 VALUE 0.

       LINKAGE SECTION.

       01  IOPCBA POINTER.
       01  DBPCB1 POINTER.
       01  DBPCB2 POINTER.
       01  DBPCB3 POINTER.
       01  DBPCB4 POINTER.
       01  DBPCB5 POINTER.
       01  DBPCB6 POINTER.
       01  DBPCB7 POINTER.
       01  DBPCB8 POINTER.
       01  DBPCB9 POINTER.

      ******************************************************************
      *I/O PCB
      ******************************************************************

       01  LTERMPCB.
           05  LOGTTERM        PIC  X(08).
           05  FILLER          PIC  X(02).
           05  TPSTAT          PIC  X(02).
           05  IODATE          PIC  X(04).
           05  IOTIME          PIC  X(04).
           05  FILLER          PIC  X(02).
           05  SEQNUM          PIC  X(02).
           05  MOD             PIC  X(08).

      ******************************************************************
      *DATABASE PCB
      ******************************************************************

       01  DBPCB.
           05  DBDNAME         PIC  X(08).
           05  SEGLEVEL        PIC  X(02).
           05  DBSTAT          PIC  X(02).
           05  PROCOPTS        PIC  X(04).
           05  FILLER          PIC  9(08) COMP.
           05  SEGNAMFB        PIC  X(08).
           05  LENKEY          PIC  9(08) COMP.
           05  SENSSSEGS       PIC  9(08) COMP.
           05  KEYFB           PIC  X(20).
           05  FILLER REDEFINES KEYFB.
               07  KEYFB1      PIC  X(9).
               07  FILLER      PIC  X(11).

       PROCEDURE DIVISION.
             ENTRY "DLITCBL"
             USING  DBPCB1, DBPCB2, DBPCB3, DBPCB4, DBPCB5,
                    DBPCB6, DBPCB7, DBPCB8, DBPCB9.

       BEGIN.
           SET ADDRESS OF DBPCB TO ADDRESS OF DBPCB1.
           DISPLAY 'DBPCB: ' DBPCB.

           OPEN INPUT  ACCTIN-FILE.
           MOVE ZERO TO INPUT-RECORDS.

           READ ACCTIN-FILE
               AT END
               DISPLAY 'END OF FILE'
               MOVE 1 TO TERM-IO
           END-READ.
           PERFORM PROCESS-INPUT UNTIL TERM-IO = 1.

           CLOSE ACCTIN-FILE.

           DISPLAY 'TOTAL INPUT RECORDS: ' INPUT-RECORDS.

           STOP RUN.

      * PROCEDURE: PROCESS-INPUT
       PROCESS-INPUT.

      *    DISPLAY 'INPUT: ' ACCTIN-IN

           UNSTRING ACCTIN-IN DELIMITED BY '","' OR '",' OR ',"' OR ',' OR '"'
             INTO TXT-ACCID
                  TXT-ACCTYPE
                  TXT-BALANCE
                  TXT-LASTTXID
           END-UNSTRING.

      *    DISPLAY 'ACCID: ' TXT-ACCID
      *    DISPLAY 'ACCTYPE: ' TXT-ACCTYPE
      *    DISPLAY 'BALANCE: ' TXT-BALANCE
      *    DISPLAY 'LASTTXID: ' TXT-LASTTXID

           COMPUTE ACCID = FUNCTION NUMVAL ( TXT-ACCID ).
           MOVE TXT-ACCTYPE TO ACCTYPE.
           COMPUTE BALANCE = FUNCTION NUMVAL ( TXT-BALANCE ).
           COMPUTE LASTTXID = FUNCTION NUMVAL ( TXT-LASTTXID ).

           CALL "CBLTDLI"
             USING ISRT, DBPCB, ACCOUNT-SEG, ACCOUNT-SSA.
           IF DBSTAT NOT = SPACES
             DISPLAY 'BAD STATUS CODE: ' DBSTAT
             MOVE 1 TO TERM-IO
           END-IF.

      *    DISPLAY 'ACCID: ' ACCID
      *    DISPLAY 'ACCTYPE: ' ACCTYPE
      *    DISPLAY 'BALANCE: ' BALANCE
      *    DISPLAY 'LASTTXID: ' LASTTXID

           ADD 1 TO INPUT-RECORDS.

           READ ACCTIN-FILE
               AT END
               DISPLAY 'END OF FILE'
               MOVE 1 TO TERM-IO
           END-READ.

           IF FUNCTION MOD (INPUT-RECORDS 1000) = 0
             DISPLAY 'INPUT-RECORDS: ' INPUT-RECORDS
           END-IF.
           IF INPUT-RECORDS = 1
             DISPLAY 'ACCOUNT-SEG >>' ACCOUNT-SEG '<<'
           END-IF.
       PROCESS-INPUT-END.