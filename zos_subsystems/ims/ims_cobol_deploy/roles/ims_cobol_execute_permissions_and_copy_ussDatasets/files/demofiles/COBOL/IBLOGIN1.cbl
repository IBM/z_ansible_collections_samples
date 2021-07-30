       CBL LIST,MAP,XREF,FLAG(I)
       IDENTIFICATION DIVISION.
       PROGRAM-ID. FBLOGIN.

      ******************************************************************

       ENVIRONMENT DIVISION.

       DATA DIVISION.
       WORKING-STORAGE SECTION.

      ******************************************************************
      *CONSTANTS
      ******************************************************************
      * ERROR MESSAGES
       77  LOGINSUCCESSFULL    PIC  X(16)      VALUE "LOGIN SUCCESSFUL".
       77  CUSTLOGGEDIN   PIC  X(26) VALUE "CUSTOMER ALREADY LOGGED IN".
       77  PASSWORDINVALID     PIC  X(16)      VALUE "PASSWORD INVALID".
       77  NOCUSTOMER        PIC  X(23) VALUE "CUSTOMER DOES NOT EXIST".

       77  LOGGEDIN         PIC  X(1)  VALUE "1".
       77  LOGGEDOUT        PIC  X(1)  VALUE "0".

      * MESSAGE PROCESSING
       77  TERM-IO             PIC 9 VALUE 0.
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
      *CUSTOMER SEGMENT AREA
      ******************************************************************

       01  CUST-SEG.
           05  CUSTID-SEG      PIC  S9(9) COMP-5.
           05  LASTNAME-SEG    PIC  X(50).
           05  FIRSTNAME-SEG   PIC  X(50).
           05  ADDRESS-SEG     PIC  X(80).
           05  CITY-SEG        PIC  X(25).
           05  STATE-SEG       PIC  X(2).
           05  ZIPCODE-SEG     PIC  X(15).
           05  PHONE-SEG       PIC  X(12).
           05  STATUS-SEG      PIC  X(1).
           05  PASSWD-SEG      PIC  X(16).
           05  CUSTTYPE-SEG    PIC  X(1).
           05  LASTLOGIN-SEG   PIC  X(23).

      ******************************************************************
      *INPUT/OUTPUT MESSAGE AREA
      *FBLOGIN 16918    password
      ******************************************************************

       01  INPUT-AREA.
           05  LL-IN           PIC  9(04) COMP.
           05  ZZ-IN           PIC  9(04) COMP.
           05  TRAN-CODE       PIC  X(08).
           05  IN-CUSTID       PIC  X(09).
           05  IN-PASSWD       PIC  X(16).

       01  OUTPUT-AREA.
           05  LL-OUT          PIC  9(04) COMP VALUE 36.
           05  ZZ-OUT          PIC  9(04) COMP.
           05  MSG-OUT         PIC  X(32).

      ******************************************************************
      *SEGMENT SEARCH ARGUMENTS
      ******************************************************************

      *    CUSTOMER-SSA1 IS USED TO FIND INFO FROM THE CUSTOMER RECORD
      *    SELECT ... WHERE sa.customer.custid = ?
      *    ALSO USED TO MARK CUSTOMER TABLE THAT USER IS LOGGED IN
       01  CUSTOMER-SSA1.
           05  FILLER          PIC  X(08)        VALUE "CUSTOMER".
           05  FILLER          PIC  X(01)        VALUE "(".
           05  FILLER          PIC  X(08)        VALUE "CUSTID  ".
           05  FILLER          PIC  X(02)        VALUE "EQ".
           05  CUSTID          PIC  S9(9) COMP-5 VALUE +0.
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

       LINKAGE SECTION.

       01  IOPCBA POINTER.
       01  DBPCB1 POINTER.

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
             USING  IOPCBA, DBPCB1.

       BEGIN.

           MOVE 0 TO TERM-IO.
           SET ADDRESS OF LTERMPCB TO ADDRESS OF IOPCBA.
           PERFORM WITH TEST BEFORE UNTIL TERM-IO = 1
              CALL 'CBLTDLI' USING GU, LTERMPCB, INPUT-AREA
              IF TPSTAT  = '  ' OR TPSTAT = MESSAGE-EXIST
              THEN

      * DOING LOGIN
                PERFORM LOGIN thru LOGIN-END

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

      * PROCEDURE LOGIN
       LOGIN.
      * PULL INPUT VARIABLES FROM INPUT MESSAGE
           MOVE ZEROS TO OUTPUT-AREA.
           COMPUTE CUSTID = FUNCTION NUMVAL ( IN-CUSTID ).
           SET ADDRESS OF DBPCB TO ADDRESS OF DBPCB1.
      *    DISPLAY "DBPCB: " DBPCB.

           CALL "CBLTDLI"
             USING GHU, DBPCB, CUST-SEG, CUSTOMER-SSA1.

      *    DISPLAY "CUST-SEG: " CUST-SEG.

           IF DBSTAT NOT = SPACES
             IF DBSTAT = GB OR DBSTAT = GE
               MOVE NOCUSTOMER TO MSG-OUT
               DISPLAY "NO CUSTOMER"
             ELSE
               MOVE DBSTAT TO SC
               MOVE BAD-STATUS TO MSG-OUT
               DISPLAY "Bad status code: " SC
             END-IF
           ELSE
      * CHECK FOR PASSWORD MATCH
             IF PASSWD-SEG NOT = IN-PASSWD
               MOVE PASSWORDINVALID TO MSG-OUT
               DISPLAY "Bad password"
             ELSE
      * CHECK IF ALREADY LOGGED IN
               IF STATUS-SEG = LOGGEDIN
                 MOVE CUSTLOGGEDIN TO MSG-OUT
                 DISPLAY "Customer already logged in"
               ELSE
      * UPDATE LASTLOGIN VALUE WITH STCK
                 MOVE FUNCTION CURRENT-DATE TO WS-CURRENT-DATE-DATA
                 MOVE WS-CURRENT-YEAR TO YEAR-TS
                 MOVE WS-CURRENT-MONTH TO MONTH-TS
                 MOVE WS-CURRENT-DAY TO DAY-TS
                 MOVE WS-CURRENT-HOURS TO HOUR-TS
                 MOVE WS-CURRENT-MINUTE TO MINUTE-TS
                 MOVE WS-CURRENT-SECOND TO SECOND-TS
                 MOVE WS-CURRENT-MILLISECONDS TO MILLISEC-TS
                 MOVE TIMESTAMP TO LASTLOGIN-SEG
      *        DISPLAY "TIMESTAMP: " TIMESTAMP
      * UPDATE STATUS TO LOGGED IN
                 MOVE LOGGEDIN TO STATUS-SEG
                 CALL "CBLTDLI"
                   USING REPL, DBPCB, CUST-SEG

                 IF DBSTAT NOT = SPACES
                   MOVE DBSTAT TO SC
                   MOVE BAD-STATUS TO MSG-OUT
                   DISPLAY "Bad status code: " SC
                 END-IF

      * RETURN LOGIN SUCCESSFUL
                 MOVE LOGINSUCCESSFULL TO MSG-OUT
               END-IF
             END-IF
           END-IF.

       LOGIN-END.

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