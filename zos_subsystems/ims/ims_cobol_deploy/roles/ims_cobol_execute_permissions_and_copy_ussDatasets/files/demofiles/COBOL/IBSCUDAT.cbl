       CBL LIST,MAP,XREF,FLAG(I)
       IDENTIFICATION DIVISION.
       PROGRAM-ID. FBSCUDAT.

      ******************************************************************

       ENVIRONMENT DIVISION.

       DATA DIVISION.
       WORKING-STORAGE SECTION.

      ******************************************************************
      * CONSTANTS
      ******************************************************************
      * RS.NEXT FAILED TO GET A ROW
       77  NOCUSTOMER      PIC  X(23) VALUE "CUSTOMER DOES NOT EXIST".

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

       01  REPLFAILED.
           05  RF-MSG    PIC  X(27) VALUE "UNABLE TO UPDATE CUSTOMER: ".
           05  RF-SC       PIC X(2).

      ******************************************************************
      *SEGMENT AREAS
      ******************************************************************

       01 CUSTOMER-SEG.
           05  CUSTID-CD       PIC  S9(9) COMP-5.
           05  LASTNAME-CD     PIC  X(50).
           05  FIRSTNAME-CD    PIC  X(50).
           05  ADDRESS-CD      PIC  X(80).
           05  CITY-CD         PIC  X(25).
           05  STATE-CD        PIC  X(2).
           05  ZIPCODE-CD      PIC  X(15).
           05  PHONE-CD        PIC  X(12).
           05  STATUS-CD       PIC  X(1).
           05  PASSWORD-CD     PIC  X(16).
           05  CUSTOMERTYPE-CD PIC  X(1).
           05  LASTLOGIN-CD    PIC  X(23).

      ******************************************************************
      *INPUT/OUTPUT MESSAGE AREA
      ******************************************************************

       01  INPUT-AREA.
           05  LL-IN           PIC  9(04) COMP.
           05  ZZ-IN           PIC  9(04) COMP.
           05  TRAN-CODE       PIC  X(08).
           05  CUSTOMER-IN.
             10  CUSTID-IN       PIC  S9(9) COMP-5.
             10  LASTNAME-IN     PIC  X(50).
             10  FIRSTNAME-IN    PIC  X(50).
             10  ADDRESS-IN      PIC  X(80).
             10  CITY-IN         PIC  X(25).
             10  STATE-IN        PIC  X(2).
             10  ZIPCODE-IN      PIC  X(15).
             10  PHONE-IN        PIC  X(12).

       01  OUTPUT-AREA.
           05  LL-OUT          PIC  9(04) COMP.
           05  ZZ-OUT          PIC  9(04) COMP.
           05  MSG-OUT         PIC  X(32).
           05  CUSTOMER-OUT.
             10  CUSTID-OUT      PIC  S9(9) COMP-5.
             10  LASTNAME-OUT    PIC  X(50).
             10  FIRSTNAME-OUT   PIC  X(50).
             10  ADDRESS-OUT     PIC  X(80).
             10  CITY-OUT        PIC  X(25).
             10  STATE-OUT       PIC  X(2).
             10  ZIPCODE-OUT     PIC  X(15).
             10  PHONE-OUT       PIC  X(12).

      ******************************************************************
      *SEGMENT SEARCH ARGUMENTS
      ******************************************************************

      *    CUSTOMER-SSA1 IS USED TO FIND INFO FROM THE CUSTOMER RECORD
      *    SELECT ... WHERE sa.customer.custid = ?
      *    ALSO USED TO MARK CUSTOMER TABLE THAT USER IS LOGGED IN
      *    UPDATE ... WHERE sa.customer.custid = ?
      *    ALSO USED TO LOGOUT CUSTOMER
      *    ALSO USED TO RETRIEVE CUSTOMER INFO
      *    ALSO USED TO UPDATE CUSTOMER INFO
       01  CUSTOMER-SSA1.
           05  FILLER          PIC  X(08)        VALUE "CUSTOMER".
           05  FILLER          PIC  X(01)        VALUE "(".
           05  FILLER          PIC  X(08)        VALUE "CUSTID  ".
           05  FILLER          PIC  X(02)        VALUE "EQ".
           05  CUSTID          PIC  S9(9) COMP-5 VALUE +0.
           05  FILLER          PIC  X(01)        VALUE ")".
           05  FILLER          PIC  X(01)        VALUE ' '.

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
      * RETRIEVE CUSTOMER ACCOUNT INFO
                PERFORM SET-CUSTOMER-DATA thru SET-CUSTOMER-DATA-END

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

      * PROCEDURE SET-CUSTOMER-DATA
       SET-CUSTOMER-DATA.
      *    SET A CUSTOMER'S DATA
           MOVE ZEROS TO OUTPUT-AREA.
           MOVE CUSTID-IN TO CUSTID.
           SET ADDRESS OF DBPCB TO ADDRESS OF DBPCB1.
           CALL 'CBLTDLI'
             USING GHU, DBPCB, CUSTOMER-SEG, CUSTOMER-SSA1.
           IF DBSTAT = SPACES
      *      UPDATE THE CUSTOMER'S DATA
             MOVE FIRSTNAME-IN TO FIRSTNAME-CD
             MOVE LASTNAME-IN TO LASTNAME-CD
             MOVE ADDRESS-IN TO ADDRESS-CD
             MOVE CITY-IN TO CITY-CD
             MOVE STATE-IN TO STATE-CD
             MOVE ZIPCODE-IN TO ZIPCODE-CD
             MOVE PHONE-IN TO PHONE-CD
             CALL 'CBLTDLI'
               USING REPL, DBPCB, CUSTOMER-SEG
             IF DBSTAT = SPACES
               MOVE CUSTOMER-IN TO CUSTOMER-OUT
             ELSE
               MOVE DBSTAT TO RF-SC
               MOVE REPLFAILED TO MSG-OUT
             END-IF
           ELSE
             IF DBSTAT = GB OR DBSTAT = GE
               MOVE NOCUSTOMER TO MSG-OUT
             ELSE
               MOVE DBSTAT TO SC
               MOVE BAD-STATUS TO MSG-OUT
             END-IF
           END-IF.
       SET-CUSTOMER-DATA-END.

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