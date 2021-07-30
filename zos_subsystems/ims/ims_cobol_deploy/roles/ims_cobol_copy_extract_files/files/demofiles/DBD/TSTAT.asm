***********************************************************************
* Friendly Bank - TSTAT DBD
***********************************************************************
      DBD   NAME=TSTAT,                                                C
               ENCODING=Cp1047,                                        C
               ACCESS=(HDAM,OSAM),                                     C
               RMNAME=(DFSHDC40,15,35),                                C
               PASSWD=NO

      DATASET  DD1=TSTAT,                                              C
               DEVICE=3390,                                            C
               SIZE=(2048),                                            C
               SCAN=0
***********************************************************************
*        SEGMENT TRANSSTATUS
***********************************************************************
      SEGM  NAME=TSTAT,                                                C
               EXTERNALNAME=TRANSSTATUS,                               C
               PARENT=0,                                               C
               BYTES=(59),                                             C
               RULES=(LLL,HERE)

      FIELD NAME=(TXID,SEQ,U),                                         C
               BYTES=8,                                                C
               START=1,                                                C
               TYPE=C,                                                 C
               DATATYPE=LONG

      FIELD NAME=STATUS,                                               C
               BYTES=1,                                                C
               START=9,                                                C
               TYPE=C,                                                 C
               DATATYPE=CHAR

      FIELD NAME=STARTTIM,                                             C
               EXTERNALNAME=STARTTIME,                                 C
               BYTES=23,                                               C
               START=10,                                               C
               TYPE=C,                                                 C
               DATATYPE=TIMESTAMP

      DFSMARSH INTERNALTYPECONVERTER=CHAR,                             C
               PATTERN='yyyy-MM-dd HH:mm:ss.SSS'

      FIELD NAME=STOPTIME,                                             C
               BYTES=23,                                               C
               START=33,                                               C
               TYPE=C,                                                 C
               DATATYPE=TIMESTAMP

      DFSMARSH INTERNALTYPECONVERTER=CHAR,                             C
               PATTERN='yyyy-MM-dd HH:mm:ss.SSS'
***********************************************************************
*        MAY NEED TO MAKE CUSTID A SECONDARY INDEX!!!!
***********************************************************************
      FIELD NAME=CUSTID,                                               C
               BYTES=4,                                                C
               START=56,                                               C
               TYPE=C,                                                 C
               DATATYPE=INT

      DBDGEN
      FINISH
      END