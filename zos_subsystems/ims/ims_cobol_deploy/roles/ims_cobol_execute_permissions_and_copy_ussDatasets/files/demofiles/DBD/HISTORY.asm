***********************************************************************
* Friendly Bank - HISTORY DBD
*               RMNAME=(DFSHDC40,15,3500),
***********************************************************************

      DBD   NAME=HISTORY,                                              C
               ENCODING=Cp1047,                                        C
               ACCESS=(HDAM,OSAM),                                     C
               RMNAME=(DFSHDC40,50,10),                                C
               PASSWD=NO

      DATASET  DD1=HISTORY,                                            C
               DEVICE=3390,                                            C
               SIZE=(2048),                                            C
               SCAN=0


***********************************************************************
*        SEGMENT HISTORY
***********************************************************************
      SEGM  NAME=HISTORY,                                              C
               PARENT=0,                                               C
               BYTES=(56),                                             C
               RULES=(LLL,HERE)

      FIELD NAME=(TXID,SEQ,U),                                         C
               BYTES=8,                                                C
               START=1,                                                C
               TYPE=C,                                                 C
               DATATYPE=LONG

      FIELD NAME=TIME,                                                 C
               BYTES=23,                                               C
               START=9,                                                C
               TYPE=C,                                                 C
               DATATYPE=TIMESTAMP

      DFSMARSH INTERNALTYPECONVERTER=CHAR,                             C
               PATTERN='yyyy-MM-dd HH:mm:ss.SSS'

      FIELD NAME=TRANSTYP,                                             C
               EXTERNALNAME=TRANSTYPE,                                 C
               BYTES=1,                                                C
               START=32,                                               C
               TYPE=C,                                                 C
               DATATYPE=CHAR

      FIELD NAME=AMOUNT,                                               C
               BYTES=8,                                                C
               START=33,                                               C
               TYPE=C,                                                 C
               DATATYPE=DECIMAL(15,2)

      FIELD NAME=REFTXID,                                              C
               BYTES=8,                                                C
               START=41,                                               C
               TYPE=C,                                                 C
               DATATYPE=LONG
***********************************************************************
*        MAY NEED TO MAKE ACCID A SECONDARY INDEX!!!!
***********************************************************************
      FIELD NAME=ACCID,                                                C
               BYTES=8,                                                C
               START=49,                                               C
               TYPE=C,                                                 C
               DATATYPE=LONG

      DBDGEN
      FINISH
      END