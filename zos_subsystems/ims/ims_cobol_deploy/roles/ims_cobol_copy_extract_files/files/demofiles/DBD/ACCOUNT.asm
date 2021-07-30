***********************************************************************
* Friendly Bank - ACCOUNT DBD
***********************************************************************
      DBD   NAME=ACCOUNT,                                              C
               ENCODING=Cp1047,                                        C
               ACCESS=(HDAM,OSAM),                                     C
               RMNAME=(DFSHDC40,50,10),                                C
               PASSWD=NO

      DATASET  DD1=ACCOUNT,                                            C
               DEVICE=3390,                                            C
               SIZE=(2048),                                            C
               SCAN=0
***********************************************************************
*        SEGMENT ACCOUNT
***********************************************************************
      SEGM  NAME=ACCOUNT,                                              C
               PARENT=0,                                               C
               BYTES=(25),                                             C
               RULES=(LLL,HERE)

      FIELD NAME=(ACCID,SEQ,U),                                        C
               BYTES=8,                                                C
               START=1,                                                C
               TYPE=C,                                                 C
               DATATYPE=LONG

      FIELD NAME=ACCTYPE,                                              C
               BYTES=1,                                                C
               START=9,                                                C
               TYPE=C,                                                 C
               DATATYPE=CHAR

      FIELD NAME=BALANCE,                                              C
               BYTES=8,                                                C
               START=10,                                               C
               TYPE=C,                                                 C
               DATATYPE=DECIMAL(15,2)

      FIELD NAME=LASTTXID,                                             C
               BYTES=8,                                                C
               START=18,                                               C
               TYPE=C,                                                 C
               DATATYPE=LONG

      DBDGEN
      FINISH
      END