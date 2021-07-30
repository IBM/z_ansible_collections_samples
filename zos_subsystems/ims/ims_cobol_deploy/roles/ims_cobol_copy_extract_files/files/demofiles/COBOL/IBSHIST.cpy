      ******************************************************************
      *INPUT/OUTPUT MESSAGE AREA
      ******************************************************************

       01  INPUT-AREA.
           05  LL-IN           PIC  9(04) COMP.
           05  ZZ-IN           PIC  9(04) COMP.
           05  IN-ACCID        PIC  X(18).

       01  OUTPUT-AREA.
           05  LL-OUT          PIC  9(04) COMP.
           05  ZZ-OUT          PIC  9(04) COMP.
           05  MSG-OUT         PIC  X(43).
           05  TXID            PIC  S9(18) COMP-5.
           05  TIMESTMP        PIC  X(23).
           05  TRANSTYP        PIC  X(1).
           05  AMOUNT          PIC  S9(13)V9(2) COMP-3.
           05  REFTXID         PIC  S9(18) COMP-5.
           05  ACCID           PIC  S9(18) COMP-5.