***********************************************************************
* Friendly Bank - CUSTACCS DBD
***********************************************************************
      DBD   NAME=CUSTACCS,                                             C
               ENCODING=Cp1047,                                        C
               ACCESS=(HDAM,OSAM),                                     C
               RMNAME=(DFSHDC40,100,5),                                C
               PASSWD=NO

      DATASET  DD1=CUSTACCS,                                           C
               DEVICE=3390,                                            C
               SIZE=(2048),                                            C
               SCAN=0
***********************************************************************
*        SEGMENT CUSTACCS
***********************************************************************
      SEGM  NAME=CUSTACCS,                                             C
               EXTERNALNAME=CUSTOMERACCS,                              C
               PARENT=0,                                               C
               BYTES=(16),                                             C
               RULES=(LLL,HERE)

      FIELD NAME=(CUSTID,SEQ,M),                                       C
               BYTES=4,                                                C
               START=1,                                                C
               TYPE=C,                                                 C
               DATATYPE=INT

      FIELD NAME=CUSACCID,                                             C
               BYTES=12,                                               C
               START=1,                                                C
               TYPE=C,                                                 C
               DATATYPE=BINARY

      FIELD NAME=ACCID,                                                C
               BYTES=8,                                                C
               START=5,                                                C
               TYPE=C,                                                 C
               DATATYPE=LONG

      FIELD NAME=ACCNUM,                                               C
               BYTES=4,                                                C
               START=13,                                               C
               TYPE=C,                                                 C
               DATATYPE=INT

      DBDGEN
      FINISH
      END