***********************************************************************
* Friendly Bank - CUSTTYPE DBD
***********************************************************************
      DBD   NAME=CUSTTYPE,                                             C
               ENCODING=Cp1047,                                        C
               ACCESS=(HDAM,OSAM),                                     C
               RMNAME=(DFSHDC40,50,10),                                C
               PASSWD=NO

      DATASET  DD1=CUSTTYPE,                                           C
               DEVICE=3390,                                            C
               SIZE=(2048),                                            C
               SCAN=0
***********************************************************************
*        SEGMENT CUSTTYPE
***********************************************************************
      SEGM  NAME=CUSTTYPE,                                             C
               EXTERNALNAME=CUSTOMERTYPE,                              C
               PARENT=0,                                               C
               BYTES=(21),                                             C
               RULES=(LLL,HERE)

      FIELD NAME=(CODE,SEQ,U),                                         C
               BYTES=1,                                                C
               START=1,                                                C
               TYPE=C,                                                 C
               DATATYPE=CHAR

      FIELD NAME=DESCRIPT,                                             C
               EXTERNALNAME=DESCRIPTION,                               C
               BYTES=20,                                               C
               START=2,                                                C
               TYPE=C,                                                 C
               DATATYPE=CHAR

      DBDGEN
      FINISH
      END