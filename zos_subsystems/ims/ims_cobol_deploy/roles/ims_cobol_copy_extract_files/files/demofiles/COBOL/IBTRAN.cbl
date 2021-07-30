       CBL LIST,MAP,XREF,FLAG(I)
       IDENTIFICATION DIVISION.
       PROGRAM-ID. "IBTRAN" recursive.

      ******************************************************************

       ENVIRONMENT DIVISION.
      *
       CONFIGURATION SECTION.
      *
       REPOSITORY.
      * Java class
           Class DB2InsertHist is "nazare.jmp.controller.InsertHist"
           Class bytebuffer is "java.nio.ByteBuffer".

       DATA DIVISION.
       WORKING-STORAGE SECTION.

      ******************************************************************
      *CONSTANTS - Change to test
      ******************************************************************
      * ERROR MESSAGES
       77  NOCUSTOMER        PIC  X(23) VALUE "CUSTOMER DOES NOT EXIST".
       77  NOACCOUNT         PIC  X(22) VALUE "ACCOUNT DOES NOT EXIST".
       77  INVALIDTRXTYPE  PIC X(43)
             VALUE "INVALID ACCOUNT ACTION. MUST BE 'w' OR 'd'.".

      * MESSAGE PROCESSING
       77  TERM-IO             PIC 9 VALUE 0.
       77  TERM-LOOP           PIC 9 VALUE 0.
       77  MESSAGE-EXIST       PIC X(2) VALUE 'CF'.
       77  NO-MORE-MESSAGE     PIC X(2) VALUE 'QC'.

       77  MULT-FACTOR         PIC S9(18) COMP-5 VALUE 10000000000.

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
           05  IN-ACCID        PIC  X(18).
           05  IN-AMOUNT       PIC  X(16).
           05  IN-TRXTYPE      PIC  X(01).
           05  IN-CUSTID       PIC  X(09).

       01  OUTPUT-AREA.
           05  LL-OUT          PIC  9(04) COMP.
           05  ZZ-OUT          PIC  9(04) COMP.
           05  MSG-OUT         PIC  X(43).
           05  BAL   REDEFINES MSG-OUT.
               10 BALANCE-ZONED1      PIC  Z(13).99.
               10 FILLER              PIC  X(27).
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

      *    HISTORY-SSA1 IS USED TO INSERT TRANSACTION INTO HISTORY
       01  HISTORY-SSA1.
           05  FILLER          PIC  X(08)        VALUE "HISTORY ".
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

      ******************************************************************
      *CLOCK STRUCTURE
      ******************************************************************
       01  WS-CURRENT-DATE-DATA.
           05  WS-CURRENT-DATE.
             10  WS-CURRENT-YEAR         PIC 9(04).
             10  WS-CURRENT-MONTH        PIC 9(02).
	         10  WS-CURRENT-DAY          PIC 9(02).
           05  WS-CURRENT-TIME.
             10  WS-CURRENT-HOURS        PIC 9(02).
             10  WS-CURRENT-MINUTE       PIC 9(02).
             10  WS-CURRENT-SECOND       PIC 9(02).
             10  WS-CURRENT-MILLISECONDS PIC 9(02).
           05  WS-DIFF-FROM-GMT          PIC S9(04).

       01  TIMESTAMP.
           05  YEAR-TS         PIC X(4).
           05  FILLER          PIC X(1) VALUE '-'.
           05  MONTH-TS        PIC X(2).
           05  FILLER          PIC X(1) VALUE '-'.
           05  DAY-TS          PIC X(2).
           05  FILLER          PIC X(1) VALUE ' '.
           05  HOUR-TS         PIC X(2).
           05  FILLER          PIC X(1) VALUE ':'.
           05  MINUTE-TS       PIC X(2).
           05  FILLER          PIC X(1) VALUE ':'.
           05  SECOND-TS       PIC X(2).
           05  FILLER          PIC X(1) VALUE '.'.
           05  MILLISEC-TS     PIC X(2).
           05  FILLER          PIC X(1) VALUE '0'.

       LOCAL-STORAGE SECTION.
      * Local Java class reference
       01 DB2InsertTran-class-ref
           object reference DB2InsertHist value null.

       01 INSERT-METHOD-ID  PIC S9(9) BINARY.

       01 class-name          PICTURE X(50).
       01 METHOD-NAME       PIC X(30).
       01 METHOD-NAME-PTR   USAGE POINTER.
       01 SIGNATURE-NAME    PIC X(30).
       01 SIGNATURE-NAME-PTR USAGE POINTER.

       01 len               pic 9(9) binary.
       01 JAVA-PRIMED       PIC X(1) VALUE 'N'.
       01 HISTSEG-PTR       USAGE POINTER.
       01 HISTSEG-LEN       PIC  S9(18) COMP-5.
       01 HISTSEG-BUFF-PTR  USAGE POINTER.
       01 HISTSEG-BUFF      PIC X(56).

       LINKAGE SECTION.
       COPY JNI.

       01  IOPCBA POINTER.
       01  DBPCB1 POINTER.
       01  DBPCB2 POINTER.
       01  DBPCB3 POINTER.

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

       PROCEDURE DIVISION
      *      ENTRY "DLITCBL"
             USING  IOPCBA, DBPCB1, DBPCB2, DBPCB3.

       BEGIN.

           MOVE 'N' to JAVA-PRIMED.
           MOVE 0 TO TERM-IO.
           SET ADDRESS OF LTERMPCB TO ADDRESS OF IOPCBA.
      *     DISPLAY 'IOPCBA   = ' IOPCBA.
      *     DISPLAY 'LTERMPCB = ' LTERMPCB.
      *     DISPLAY 'JAVA     = ' JAVA-PRIMED.
           PERFORM WITH TEST BEFORE UNTIL TERM-IO = 1
            DISPLAY 'READY TO CALL DLI'
              CALL 'CBLTDLI' USING GU, LTERMPCB, INPUT-AREA
      *     DISPLAY 'AFTER CALLING DLI'
              IF TPSTAT  = '  ' OR TPSTAT = MESSAGE-EXIST
              THEN

      * DOING ACCOUNT DEPOSIT/WITHDRAWAL
                PERFORM ACCOUNT-ACTIVITY thru ACCOUNT-ACTIVITY-END

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

           IF JAVA-PRIMED = 'Y'
           THEN
      *      DISPLAY 'DeleteLocalRef HISTSEG-BUFF-PTR'
             Call DeleteLocalRef USING BY VALUE JNIEnvPtr,
                                     BY VALUE HISTSEG-BUFF-PTR

      *      DISPLAY 'DeleteLocalRef DB2InsertTran-class-ref'
             Call DeleteLocalRef USING BY VALUE JNIEnvPtr,
                                     BY VALUE DB2InsertTran-class-ref
           END-IF.

           DISPLAY 'Goback'.

           GOBACK.

      * PROCEDURE ACCOUNT-ACTIVITY
       ACCOUNT-ACTIVITY.
           MOVE ZEROS TO OUTPUT-AREA.

           DISPLAY 'IN-ACCID: ' IN-ACCID
           DISPLAY 'IN-AMOUNT: ' IN-AMOUNT
           DISPLAY 'IN-TRXTYPE: ' IN-TRXTYPE
           DISPLAY 'IN-CUSTID: ' IN-CUSTID

      * CHECK FOR VALID TRANSACTION TYPE
           IF IN-TRXTYPE NOT = 'd' AND IN-TRXTYPE NOT = 'w' AND
              IN-TRXTYPE NOT = 'D' AND IN-TRXTYPE NOT = 'W'
             MOVE INVALIDTRXTYPE TO MSG-OUT
           ELSE
      * RETRIEVE NEXT TRANSACTION ID
             COMPUTE ACCID = FUNCTION NUMVAL ( IN-ACCID )
             SET ADDRESS OF DBPCB TO ADDRESS OF DBPCB1
             CALL 'CBLTDLI'
               USING GHU, DBPCB, ACCOUNT-SEG, ACCOUNT-SSA1
             IF DBSTAT NOT = SPACES
               IF DBSTAT = GB OR DBSTAT = GE
                 MOVE NOACCOUNT TO MSG-OUT
               ELSE
                 MOVE DBSTAT TO SC
                 MOVE BAD-STATUS TO MSG-OUT
               END-IF
             ELSE
      * UPDATE THE HISTORY SEG
               COMPUTE ACCID-HIST = ACCID
               COMPUTE TXID-HIST = ACCID-HIST * MULT-FACTOR
                 + LASTTXID-ACC + 1
               MOVE IN-TRXTYPE TO TRANSTYP-HIST
               COMPUTE AMOUNT-HIST = FUNCTION NUMVAL( IN-AMOUNT )
               COMPUTE REFTXID-HIST = 0

               MOVE FUNCTION CURRENT-DATE TO WS-CURRENT-DATE-DATA
               MOVE WS-CURRENT-YEAR TO YEAR-TS
               MOVE WS-CURRENT-MONTH TO MONTH-TS
               MOVE WS-CURRENT-DAY TO DAY-TS
               MOVE WS-CURRENT-HOURS TO HOUR-TS
               MOVE WS-CURRENT-MINUTE TO MINUTE-TS
               MOVE WS-CURRENT-SECOND TO SECOND-TS
               MOVE WS-CURRENT-MILLISECONDS TO MILLISEC-TS
               MOVE TIMESTAMP TO TIMESTMP-HIST

               IF JAVA-PRIMED = 'N'
               THEN
                 PERFORM PRIME-JAVA thru PRIME-JAVA-END
                 MOVE 'Y' TO JAVA-PRIMED
               END-IF

               IF JAVA-PRIMED = 'Y'
               THEN
                 DISPLAY 'SAVE HISTORY TO DB2'
                 PERFORM JAVA-SAVEHIST THRU JAVA-SAVEHIST-END
               END-IF

               DISPLAY 'SAVE HISTORY TO IMS'
               SET ADDRESS OF DBPCB TO ADDRESS OF DBPCB3
               CALL "CBLTDLI"
                 USING ISRT, DBPCB, HISTORY-SEG, HISTORY-SSA1
               IF DBSTAT NOT = SPACES
                 DISPLAY 'BAD STATUS CODE: ' DBSTAT
                 MOVE 1 TO TERM-IO
               END-IF
      * UPDATE THE BALANCE AND LASTTXID IN THE ACCOUNT SEGMENT
               COMPUTE LASTTXID-ACC = LASTTXID-ACC + 1
               IF IN-TRXTYPE = 'w'
                 COMPUTE BALANCE-ACC = BALANCE-ACC - AMOUNT-HIST
               ELSE
                 COMPUTE BALANCE-ACC = BALANCE-ACC + AMOUNT-HIST
               END-IF

               SET ADDRESS OF DBPCB TO ADDRESS OF DBPCB1
               CALL "CBLTDLI"
                 USING REPL, DBPCB, ACCOUNT-SEG

               IF DBSTAT NOT = SPACES
                 MOVE DBSTAT TO SC
                 MOVE BAD-STATUS TO MSG-OUT
                 DISPLAY "Bad status code: " SC
               END-IF
      * RETRIEVE BALANCE TO RETURN TO CLIENT
               COMPUTE BALANCE-ZONED = BALANCE-ACC + 0
               MOVE BALANCE-ZONED TO MSG-OUT

               PERFORM GET-ACCOUNT-SUMMARY THRU
                 GET-ACCOUNT-SUMMARY-END
             END-IF
           END-IF.

       ACCOUNT-ACTIVITY-END.

      * PROCEDURE GET-ACCOUNT-SUMMARY
       GET-ACCOUNT-SUMMARY.
      *    RETRIEVE CUSTOMER'S ACCOUNT
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
                 MOVE BALANCE-ACC TO BALANCE-AS(TOTAL-ACCS)
                 MOVE ACCTYPE-ACC TO ACCTYPE-AS(TOTAL-ACCS)
                 MOVE ACCID-ACC TO ACCID-AS(TOTAL-ACCS)
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
           COMPUTE LL-OUT = 48 + (TOTAL-ACCS
             * (LENGTH OF ACCOUNT-SUMMARY(1))).
       GET-ACCOUNT-SUMMARY-END.

      * PROCEDURE INSERT-IO : INSERT FOR IOPCB REQUEST HANDLER

       INSERT-IO.
           COMPUTE LL-OUT = LENGTH OF OUTPUT-AREA.
      *    DISPLAY 'INSERT-IO'.
      *    DISPLAY 'LL-OUT : ' LL-OUT.
           MOVE 0 TO ZZ-OUT.
            CALL 'CBLTDLI' USING ISRT, LTERMPCB, OUTPUT-AREA.

           IF TPSTAT NOT = SPACES
             THEN
             DISPLAY 'INSERT TO IOPCB FAILED WITH STATUS CODE: '
                TPSTAT
           END-IF.
       INSERT-IO-END.

      * PROCEDURE JAVA-SAVEHIST
      * This procedure does
      *    jobject NewDirectByteBuffer(JNIEnv* env, void*
      *     address, jlong capacity);
      *    insertHist(HISTSEG-BUFF-PTR)
       JAVA-SAVEHIST.

      *    DISPLAY 'NewDirectByteBuffer'.
      *    DISPLAY 'txid =             ' TXID-HIST.
      *    DISPLAY 'HIST-SEG =         ' HISTORY-SEG.
           COMPUTE HISTSEG-LEN = LENGTH OF HISTORY-SEG.
           SET HISTSEG-PTR TO ADDRESS OF HISTORY-SEG
           Call NewDirectByteBuffer USING BY VALUE JNIEnvPtr,
                                             HISTSEG-PTR,
                                                  HISTSEG-LEN
                                       returning  HISTSEG-BUFF-PTR.

      *    DISPLAY 'HISTSEG-LEN  = ' HISTSEG-LEN.
      *    DISPLAY 'HIST BUFFER  = ' HISTSEG-BUFF-PTR.
           DISPLAY 'calling Java Method   '.
           CALL CallStaticVoidMethod using by value JNIEnvPtr
                                      by value DB2InsertTran-class-ref
                                      by value INSERT-METHOD-ID
                                      by value HISTSEG-BUFF-PTR.

       JAVA-SAVEHIST-END.

      * PROCEDURE PRIME-JAVA
       PRIME-JAVA.
      *    DISPLAY 'Setting address of JNIEnvPtr'.
           Set address of JNIEnv to JNIEnvPtr.

      *    DISPLAY 'Setting address of JavaNativeInterface (JNI)'.
           Set address of JNINativeInterface to JNIEnv.

      *    DISPLAY 'Convert class name from EBCDIC to ASCII'.
           Move z"nazare/jmp/controller/InsertHist" to class-name.
           Call "__etoa" using by value address of class-name
               returning len.

      *    DISPLAY 'FindClass - attempt to load Java class'.
           Call FindClass using by value JNIEnvPtr
              address of class-name returning DB2InsertTran-class-ref.

           If DB2InsertTran-class-ref = null
             DISPLAY "ERROR LOADING CLASS: " class-name
             Goback
           End-if.

      *    DISPLAY 'FindClass worked'.

      *    Look up doTest method DB2InsertHist
      *    ([B)V indicates no parms and a void return
           Move z"insertHist" to METHOD-NAME.
           Call "__etoa" using by value address of
                                           METHOD-NAME
                         returning len.

           Move z"(Ljava/nio/ByteBuffer;)V" to SIGNATURE-NAME.
           Call "__etoa" using by value address of
                                           SIGNATURE-NAME
                         returning len.

           SET METHOD-NAME-PTR TO ADDRESS OF METHOD-NAME.
           SET SIGNATURE-NAME-PTR TO ADDRESS OF SIGNATURE-NAME.

      *    DISPLAY 'Call GetStaticMethodId insertHist(ByteBuffer)'.
           CALL GetStaticMethodId USING BY VALUE JNIEnvPtr
                                           DB2InsertTran-class-ref
                                           METHOD-NAME-PTR
                                           SIGNATURE-NAME-PTR
                            RETURNING INSERT-METHOD-ID.

           If INSERT-METHOD-ID = 0
              Display "Error occurred while getting INSERT-METHOD-ID"
              Stop run
           End-if.

       PRIME-JAVA-END.