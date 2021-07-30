       CBL LIST,MAP,XREF,FLAG(I)
       IDENTIFICATION DIVISION.
       PROGRAM-ID. LOADTSTA.

      ******************************************************************

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT TSTAIN-FILE ASSIGN TO TSTAIN.
       DATA DIVISION.

       FILE SECTION.
         FD TSTAIN-FILE
            LABEL RECORDS ARE OMITTED
            RECORDING MODE IS F
            BLOCK CONTAINS 0 RECORDS
            DATA RECORD IS TSTAIN-IN.
       01  TSTAIN-IN          PIC X(200).

       WORKING-STORAGE SECTION.

      ******************************************************************
      *DATABASE CALL CODES
      ******************************************************************

       77  GU                  PIC  X(04)        VALUE "GU  ".
       77  GHU                 PIC  X(04)        VALUE "GHU ".
       77  GN                  PIC  X(04)        VALUE "GN  ".
       77  GHN                 PIC  X(04)        VALUE "GHN ".
       77  ISRT                PIC  X(04)        VALUE "ISRT".
       77  REPL                PIC  X(04)        VALUE "REPL".

      ******************************************************************
      *IMS STATUS CODES
      ******************************************************************

       77  GE                  PIC  X(02)        VALUE "GE".
       77  GB                  PIC  X(02)        VALUE "GB".

      ******************************************************************
      *ERROR STATUS CODE AREA
      ******************************************************************

       01  BAD-STATUS.
           05  SC-MSG  PIC X(30) VALUE "BAD STATUS CODE WAS RECEIVED: ".
           05  SC             PIC X(2).

      ******************************************************************
      *SEGMENT SEARCH ARGUMENTS
      ******************************************************************
       01  TSTAT-SSA.
           05  FILLER          PIC  X(08)        VALUE "TSTAT   ".
           05  FILLER          PIC  X(01)        VALUE ' '.

       01  TSTAT-SEG.
           05  TXID            PIC  S9(18) COMP-5.
           05  STATUS-SEG      PIC  X(1).
           05  STARTTIME       PIC  X(23).
           05  STOPTIME        PIC  X(23).
           05  CUSTID          PIC  S9(9) COMP-5.

       01  TXT-TXID            PIC  X(19).
       01  TXT-STATUS          PIC  X(1).
       01  TXT-STARTTIME       PIC  X(23).
       01  TXT-STOPTIME        PIC  X(23).
       01  TXT-CUSTID          PIC  X(9).

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
           SET ADDRESS OF DBPCB TO ADDRESS OF DBPCB7.
           DISPLAY 'DBPCB: ' DBPCB.

           OPEN INPUT  TSTAIN-FILE.
           MOVE ZERO TO INPUT-RECORDS.

           READ TSTAIN-FILE
               AT END
               DISPLAY 'END OF FILE'
               MOVE 1 TO TERM-IO
           END-READ.
           PERFORM PROCESS-INPUT UNTIL TERM-IO = 1.

           CLOSE TSTAIN-FILE.

           DISPLAY 'TOTAL INPUT RECORDS: ' INPUT-RECORDS.

           STOP RUN.

      * PROCEDURE: PROCESS-INPUT
       PROCESS-INPUT.


           UNSTRING TSTAIN-IN DELIMITED BY '","' OR '",' OR ',"' OR ',' OR '"'
             INTO TXT-TXID
                  TXT-STATUS
                  TXT-STARTTIME
                  TXT-STOPTIME
                  TXT-CUSTID
           END-UNSTRING.

           COMPUTE TXID = FUNCTION NUMVAL ( TXT-TXID ).
           MOVE TXT-STATUS TO STATUS-SEG.
           MOVE TXT-STARTTIME TO STARTTIME.
           MOVE TXT-STOPTIME TO STOPTIME.
           COMPUTE CUSTID = FUNCTION NUMVAL ( TXT-CUSTID ).

           CALL "CBLTDLI"
             USING ISRT, DBPCB, TSTAT-SEG, TSTAT-SSA.
           IF DBSTAT NOT = SPACES
             DISPLAY 'BAD STATUS CODE: ' DBSTAT
             MOVE 1 TO TERM-IO
           END-IF.

           ADD 1 TO INPUT-RECORDS.

           READ TSTAIN-FILE
               AT END
               DISPLAY 'END OF FILE'
               MOVE 1 TO TERM-IO
           END-READ.

           IF FUNCTION MOD (INPUT-RECORDS 1000) = 0
             DISPLAY 'INPUT-RECORDS: ' INPUT-RECORDS
           END-IF.
           IF INPUT-RECORDS = 1
             DISPLAY 'TSTAT-SEG >>' TSTAT-SEG '<<'
           END-IF.
       PROCESS-INPUT-END.