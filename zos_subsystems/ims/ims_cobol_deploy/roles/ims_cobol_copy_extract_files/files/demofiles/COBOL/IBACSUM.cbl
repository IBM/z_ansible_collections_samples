       CBL LIST,MAP,XREF,FLAG(I)
       IDENTIFICATION DIVISION.
       PROGRAM-ID. FBACSUM.
      *test1234
      ******************************************************************
      ******************************************************************

       ENVIRONMENT DIVISION.

       DATA DIVISION.
       WORKING-STORAGE SECTION.

      ******************************************************************
      *CONSTANTS
      ******************************************************************
      * RS.NEXT FAILED TO GET A ROW
       77  NOCUSTOMER        PIC  X(23) VALUE "CUSTOMER DOES NOT EXIST".
       77  NOACCOUNT         PIC  X(22) VALUE "ACCOUNT DOES NOT EXIST".

      * MESSAGE PROCESSING
       77  TERM-IO             PIC 9 VALUE 0.
       77  TERM-LOOP           PIC 9 VALUE 0.
       77  MESSAGE-EXIST       PIC X(2) VALUE 'CF'.
       77  NO-MORE-MESSAGE     PIC X(2) VALUE 'QC'.

      ******************************************************************
      *DATABASE CALL CODES
      ******************************************************************

       77  GU                  PIC  X(04)        VALUE "GU  ".
       77  GHU                 PIC  X(04)        VALUE "GHU ".
       77  GN                  PIC  X(04)        VALUE "GN  ".
       77  GHN                 PIC  X(04)        VALUE "GHN ".
       77  ISRT                PIC  X(04)        VALUE "ISRT".
       77  REPL                PIC  X(04)        VALUE "REPL".
       77  NEXT-CALL           PIC  X(04)        VALUE "    ".

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
      *SEGMENT AREAS
      ******************************************************************

       77  BALANCE-ZONED       PIC  Z(13).99.

       01  CUSTACCS-SEG.
           05  CUSTID-CA       PIC  S9(9) COMP-5.
           05  ACCID-CA        PIC  S9(18) COMP-5.
           05  ACCNUM-CA       PIC  S9(9) COMP-5.


       01  ACCOUNT-SEG.
           05  ACCID-ACC       PIC  S9(18) COMP-5.
           05  ACCTYPE-ACC     PIC  X(01).
           05  BALANCE-ACC     PIC  S9(13)V9(2) COMP-3.
           05  LASTTXID-ACC    PIC  S9(18) COMP-5.

       01  HISTORY-SEG.
           05  TXID-HIST       PIC  S9(18) COMP-5.
           05  TIMESTMP-HIST   PIC  X(23).
           05  TRANSTYP-HIST   PIC  X(1).
           05  AMOUNT-HIST     PIC  S9(13)V9(2) COMP-3.
           05  REFTXID-HIST    PIC  S9(18) COMP-5.
           05  ACCID-HIST      PIC  S9(18) COMP-5.

      ******************************************************************
      *INPUT/OUTPUT MESSAGE AREA
      ******************************************************************

       01  INPUT-AREA.
           05  LL-IN           PIC  9(04) COMP.
           05  ZZ-IN           PIC  9(04) COMP.
           05  TRAN-CODE       PIC  X(08).
           05  IN-CUSTID       PIC  X(09).

       01  OUTPUT-AREA.
           05  LL-OUT          PIC  9(04) COMP.
           05  ZZ-OUT          PIC  9(04) COMP.
           05  MSG-OUT         PIC  X(32).
           05  TOTAL-ACCS      PIC  9.
           05  ACCOUNT-SUMMARY OCCURS 1 TO 6 TIMES
                 DEPENDING ON TOTAL-ACCS.
               10  BALANCE-AS  PIC  S9(13)V9(2) COMP-3.
               10  ACCTYPE-AS  PIC  X(1).
               10  ACCID-AS    PIC  S9(18) COMP-5.

      ******************************************************************
      *SEGMENT SEARCH ARGUMENTS
      ******************************************************************

      *    ACCOUNT-SSA1 IS USED TO GET LAST TXID FOR HISTORY
      *    SELECT ... WHERE sa.account.accid = ?
      *    ALSO USED TO UPDATE AN ACCT RECORD WITH NEW INFO
      *    ALSO USED TO GET BALANCE AFTER TRAN
       01  ACCOUNT-SSA1.
           05  FILLER          PIC  X(08)        VALUE "ACCOUNT ".
           05  FILLER          PIC  X(01)        VALUE "(".
           05  FILLER          PIC  X(08)        VALUE "ACCID   ".
           05  FILLER          PIC  X(02)        VALUE "= ".
           05  ACCID           PIC  S9(18) COMP-5  VALUE +0.
           05  FILLER          PIC  X(01)        VALUE ")".
           05  FILLER          PIC  X(01)        VALUE ' '.

      *    CUSTACCS-SSA2 IS USED TO RETRIEVE ALL ACCOUNTS FOR A CUSTOMER
       01  CUSTACCS-SSA2.
           05  FILLER          PIC  X(08)        VALUE "CUSTACCS".
           05  FILLER          PIC  X(01)        VALUE "(".
           05  FILLER          PIC  X(08)        VALUE "CUSTID  ".
           05  FILLER          PIC  X(02)        VALUE "= ".
           05  CA-CUSTID       PIC  S9(9) COMP-5 VALUE +0.
           05  FILLER          PIC  X(01)        VALUE ")".
           05  FILLER          PIC  X(01)        VALUE ' '.

       LINKAGE SECTION.

       01  IOPCBA POINTER.
       01  DBPCB1 POINTER.
       01  DBPCB2 POINTER.

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
             USING  IOPCBA, DBPCB1, DBPCB2.

       BEGIN.

           DISPLAY 'IBACSUM ENTERS.... 7/30 3:02 AM '.
           MOVE 0 TO TERM-IO.
           SET ADDRESS OF LTERMPCB TO ADDRESS OF IOPCBA.
           PERFORM WITH TEST BEFORE UNTIL TERM-IO = 1
              CALL 'CBLTDLI' USING GU, LTERMPCB, INPUT-AREA
              IF TPSTAT  = '  ' OR TPSTAT = MESSAGE-EXIST
              THEN

      * RETRIEVE CUSTOMER ACCOUNT INFO
                PERFORM GET-ACCOUNT-SUMMARY thru GET-ACCOUNT-SUMMARY-END

                PERFORM INSERT-IO THRU INSERT-IO-END
              ELSE
                IF TPSTAT = NO-MORE-MESSAGE
                THEN
                  MOVE 1 TO TERM-IO
                ELSE
                  DISPLAY 'GU FROM IOPCB FAILED WITH STATUS CODE: '
                    TPSTAT
                END-IF
              END-IF
           END-PERFORM.
           STOP RUN.

      * PROCEDURE GET-ACCOUNT-SUMMARY
       GET-ACCOUNT-SUMMARY.
      *    RETRIEVE CUSTOMER'S ACCOUNT
           MOVE ZEROS TO OUTPUT-AREA.
           MOVE 0 TO TERM-LOOP.
           MOVE GHU TO NEXT-CALL.
           COMPUTE CA-CUSTID = FUNCTION NUMVAL ( IN-CUSTID ).
           COMPUTE TOTAL-ACCS = 0.
           PERFORM WITH TEST AFTER UNTIL TERM-LOOP = 1
             SET ADDRESS OF DBPCB TO ADDRESS OF DBPCB2
             CALL 'CBLTDLI'
               USING NEXT-CALL, DBPCB, CUSTACCS-SEG, CUSTACCS-SSA2
             IF DBSTAT = SPACES
      *        ISSUE CALL TO ACCOUNT-SEG
               MOVE ACCID-CA TO ACCID
               SET ADDRESS OF DBPCB TO ADDRESS OF DBPCB1
               CALL 'CBLTDLI'
                 USING GHU, DBPCB, ACCOUNT-SEG, ACCOUNT-SSA1
               IF DBSTAT = SPACES
                 COMPUTE TOTAL-ACCS = TOTAL-ACCS + 1
                 DISPLAY 'TOTAL-ACCS =' TOTAL-ACCS
      *          PERFORM RULES-CHECK THRU RULES-CHECK-END
                 MOVE BALANCE-ACC TO BALANCE-AS(TOTAL-ACCS)
                 DISPLAY 'BALANCE-ACC =  ' BALANCE-ACC



                 MOVE ZEROS TO BALANCE-AS(TOTAL-ACCS)



                 MOVE ACCTYPE-ACC TO ACCTYPE-AS(TOTAL-ACCS)
                 DISPLAY 'ACCTYPE-ACC = ' ACCTYPE-ACC
                 MOVE ACCID-ACC TO ACCID-AS(TOTAL-ACCS)
                 DISPLAY 'ACCID-ACC = ' ACCID-ACC
               ELSE
                 MOVE 1 TO TERM-LOOP
                 IF DBSTAT = GB OR DBSTAT = GE
                   IF TOTAL-ACCS < 1
                     MOVE NOACCOUNT TO MSG-OUT
                   END-IF
                 ELSE
                   MOVE DBSTAT TO SC
                   MOVE BAD-STATUS TO MSG-OUT
                 END-IF
               END-IF
             ELSE
               MOVE 1 TO TERM-LOOP
               IF DBSTAT = GB OR DBSTAT = GE
                 IF TOTAL-ACCS < 1
                   MOVE NOCUSTOMER TO MSG-OUT
                 END-IF
               ELSE
                 MOVE DBSTAT TO SC
                 MOVE BAD-STATUS TO MSG-OUT
               END-IF
             END-IF
             MOVE GHN TO NEXT-CALL
           END-PERFORM.

      *    CALCULATE LL VALUE FOR OUTPUT MESSAGE
           DISPLAY 'ACCOUNT-SUMMARY = ' ACCOUNT-SUMMARY(TOTAL-ACCS).
           COMPUTE LL-OUT = 28 + (TOTAL-ACCS
             * (LENGTH OF ACCOUNT-SUMMARY)).
           DISPLAY 'LL-OUT=' LL-OUT.
       GET-ACCOUNT-SUMMARY-END.

      * RULES CHECK FOR OLD ACCOUNTS

       RULES-CHECK.

      * THese are old customers - donot transact
           IF CA-CUSTID < 16
              MOVE 0      TO  BALANCE-ACC
           END-IF.

       RULES-CHECK-END.
      * PROCEDURE INSERT-IO : INSERT FOR IOPCB REQUEST HANDLER

       INSERT-IO.
           COMPUTE LL-OUT = LENGTH OF OUTPUT-AREA.
           MOVE 0 TO ZZ-OUT.
           CALL 'CBLTDLI' USING ISRT, LTERMPCB, OUTPUT-AREA.

           IF TPSTAT NOT = SPACES
             THEN
             DISPLAY 'INSERT TO IOPCB FAILED WITH STATUS CODE: '
                TPSTAT
           END-IF.
       INSERT-IO-END.
