       CBL LIST,MAP,XREF,FLAG(I)
       IDENTIFICATION DIVISION.
       PROGRAM-ID. FBGCUDAT.

      ******************************************************************

       ENVIRONMENT DIVISION.

       DATA DIVISION.
       WORKING-STORAGE SECTION.

      ******************************************************************
      *CONSTANTS
      ******************************************************************
      * RS.NEXT FAILED TO GET A ROW
       77  NOCUSTOMER        PIC  X(23) VALUE "CUSTOMER DOES NOT EXIST".

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
       77  INQY                PIC  X(4)         VALUE 'INQY'.

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
           05  IN-CUSTID       PIC  X(09).

       01  OUTPUT-AREA.
           05  LL-OUT          PIC  9(04) COMP.
           05  ZZ-OUT          PIC  9(04) COMP.
           05  MSG-OUT         PIC  X(32).
           05  FIRSTNAME-OUT   PIC  X(50).
           05  LASTNAME-OUT    PIC  X(50).
           05  ADDRESS-OUT     PIC  X(80).
           05  CITY-OUT        PIC  X(25).
           05  STATE-OUT       PIC  X(2).
           05  ZIPCODE-OUT     PIC  X(15).
           05  PHONE-OUT       PIC  X(12).
           05  CUSTID-OUT      PIC  X(09).

      ******************************************************************
      *SEGMENT SEARCH ARGUMENTS
      ******************************************************************

      *    CUSTOMER-SSA1 IS USED TO FIND INFO FROM THE CUSTOMER RECORD
      *    SELECT ... WHERE sa.customer.custid = ?
       01  CUSTOMER-SSA1.
           05  FILLER          PIC  X(08)        VALUE "CUSTOMER".
           05  FILLER          PIC  X(01)        VALUE "(".
           05  FILLER          PIC  X(08)        VALUE "CUSTID  ".
           05  FILLER          PIC  X(02)        VALUE "EQ".
           05  CUSTID          PIC  S9(9) COMP-5 VALUE +0.
           05  FILLER          PIC  X(01)        VALUE ")".
           05  FILLER          PIC  X(01)        VALUE ' '.

       01 INQY-IO-AREA.
           05 INQY-ENVIRON-DATA  PIC X(100) VALUE SPACES.
           05 INQY-LEN-RECOVERY-SEC PIC S9(04) VALUE 16 COMP.
           05 INQY-RECOVERY-SECTION PIC X(016) VALUE SPACES.
           05 INQY-LEN-APARM     PIC S9(04) VALUE 32 COMP.
           05 INQY-APARM         PIC X(032) VALUE SPACES.
       01 INQY-ENVIRON.
           05 INQY-IMS-ID        PIC X(08) VALUE SPACES.
           05 INQY-IMS-RELEASE   PIC X(04) VALUE SPACES.
           05 INQY-IMS-CNTRL-TYPE PIC X(08) VALUE SPACES.
           05 INQY-IMS-APP-REG-TYPE PIC X(08) VALUE SPACES.
           05 INQY-IMS-REGION-ID PIC X(04) VALUE SPACES.
           05 INQY-APPLIC-PRGM-NAME PIC X(08) VALUE SPACES.
           05 INQY-PSB-NAME      PIC X(08) VALUE SPACES.
           05 INQY-TRAN-NAME     PIC X(08) VALUE SPACES.
           05 INQY-GROUP-NAME    PIC X(08) VALUE SPACES.
           05 INQY-STATUS-GRP-IND PIC X(04) VALUE SPACES.
           05 INQY-ADDR-REC-TOKEN PIC X(04) VALUE SPACES.
           05 INQY-ADDR-APPLIC-PARM PIC X(04) VALUE SPACES.
           05 INQY-SHR-QUEUE-IND PIC X(04) VALUE SPACES.
           05 INQY-USR-ID-ADDR-SP PIC X(08) VALUE SPACES.
           05 INQY-USR-ID-IND    PIC X(01) VALUE SPACES.
           05 INQY-RRS-IND       PIC X(03) VALUE SPACES.
       01 AIB.
           02 AIBRID       PIC X(8) VALUE 'DFSAIB  '.
           02 AIBRLEN      PIC 9(9) USAGE BINARY VALUE 264.
           02 AIBRSFUNC    PIC X(8) VALUE 'ENVIRON '.
           02 AIBRSNM1     PIC X(8) VALUE 'IOPCB   '.
           02 AIBRSNM2     PIC X(8) VALUE SPACES.
           02 AIBRESV1     PIC X(8).
           02 AIBOALEN     PIC 9(9) USAGE BINARY VALUE 152.
           02 AIBOAUSE     PIC 9(9) USAGE BINARY.
           02 AIBRESV2     PIC X(12).
           02 AIBRETRN     PIC 9(9) USAGE BINARY.
           02 AIBREASN     PIC 9(9) USAGE BINARY.
           02 AIBERRXT     PIC 9(9) USAGE BINARY.
           02 AIBRESA1     USAGE POINTER.
           02 AIBRESA2     USAGE POINTER.
           02 AIBRESA3     USAGE POINTER.
           02 AIBRESV4     PIC X(40).
           02 AIBRSAVE OCCURS 18 TIMES USAGE POINTER.
           02 AIBRTOKN OCCURS 6 TIMES USAGE POINTER.
           02 AIBRTOKC     PIC X(16).
           02 AIBRTOKV     PIC X(16).
           02 AIBRTOKA OCCURS 2 TIMES PIC 9(9) USAGE BINARY.

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
       01  PCB-MAPTR.
          05 MAPTR-DBDNAME           PIC X(08).
          05 MAPTR-LEVEL-NUM         PIC X(02).
          05 MAPTR-STATUS-CODE       PIC X(02).
          05 MAPTR-PROC-OPTIONS      PIC X(04).
          05 MAPTR-FILLER            PIC S9(5) COMP.
          05 MAPTR-SEG-NAME          PIC X(08).
          05 MAPTR-KEY-LNGTH         PIC S9(5) COMP.
          05 MAPTR-NUM-SEGS          PIC S9(5) COMP.

       PROCEDURE DIVISION.
             ENTRY "DLITCBL"
             USING  IOPCBA, DBPCB1, PCB-MAPTR.

       BEGIN.
           MOVE 0 TO INQY-IO-AREA.
           SET ADDRESS OF LTERMPCB TO ADDRESS OF IOPCBA.
           CALL 'AIBTDLI' USING INQY AIB INQY-IO-AREA.
           EVALUATE TPSTAT
           WHEN SPACES
               DISPLAY 'PARM-LEN  =' INQY-LEN-APARM '='
               DISPLAY ' PARM =' INQY-APARM   '='
               MOVE INQY-ENVIRON-DATA TO INQY-ENVIRON
               DISPLAY 'INQY-IO-AR=' INQY-IO-AREA '='
               DISPLAY 'INQY-ENV  =' INQY-ENVIRON '='
               DISPLAY 'AIB  =' AIB '='
               DISPLAY 'PCB  =' LTERMPCB '='
           WHEN OTHER
               DISPLAY 'ERROR ON GET IMS PARM INQY '
                       ' STATUS CODE=' TPSTAT
               ' PCB=' LTERMPCB
           END-EVALUATE.


           MOVE 0 TO TERM-IO.
           SET ADDRESS OF LTERMPCB TO ADDRESS OF IOPCBA.
           PERFORM WITH TEST BEFORE UNTIL TERM-IO = 1
              CALL 'CBLTDLI' USING GU, LTERMPCB, INPUT-AREA
              IF TPSTAT  = '  ' OR TPSTAT = MESSAGE-EXIST
              THEN
      * RETRIEVE CUSTOMER ACCOUNT INFO
                PERFORM GET-CUSTOMER-DATA thru GET-CUSTOMER-DATA-END

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

      * PROCEDURE GET-CUSTOMER-DATA
       GET-CUSTOMER-DATA.
      *  RETRIEVE CUSTOMER'S DATA
           MOVE ZEROS TO OUTPUT-AREA.
           COMPUTE CUSTID = FUNCTION NUMVAL ( IN-CUSTID ).
           SET ADDRESS OF DBPCB TO ADDRESS OF DBPCB1.
           CALL 'CBLTDLI'
             USING GU, DBPCB, CUSTOMER-SEG, CUSTOMER-SSA1.

           IF DBSTAT = SPACES
             MOVE FIRSTNAME-CD TO FIRSTNAME-OUT
             MOVE LASTNAME-CD TO LASTNAME-OUT
             MOVE ADDRESS-CD TO ADDRESS-OUT
             MOVE CITY-CD TO CITY-OUT
             MOVE STATE-CD TO STATE-OUT
             MOVE ZIPCODE-CD TO ZIPCODE-OUT
             MOVE PHONE-CD TO PHONE-OUT
             MOVE CUSTID-CD TO CUSTID-OUT
           ELSE
             IF DBSTAT = GB OR DBSTAT = GE
               MOVE NOCUSTOMER TO MSG-OUT
             ELSE
               MOVE DBSTAT TO SC
               MOVE BAD-STATUS TO MSG-OUT
             END-IF
           END-IF.
       GET-CUSTOMER-DATA-END.


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