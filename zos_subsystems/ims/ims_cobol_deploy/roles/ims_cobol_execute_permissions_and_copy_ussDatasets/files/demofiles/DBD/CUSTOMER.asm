***********************************************************************
* Friendly Bank - CUSTOMER DBD
***********************************************************************
      DBD   NAME=CUSTOMER,                                             C
               ENCODING=Cp1047,                                        C
               ACCESS=(HDAM,OSAM),                                     C
               RMNAME=(DFSHDC40,5,10),                                 C
               PASSWD=NO

      DATASET  DD1=CUSTOMER,                                           C
               DEVICE=3390,                                            C
               SIZE=(2048),                                            C
               SCAN=0
***********************************************************************
*        SEGMENT CUSTOMER
***********************************************************************
      SEGM  NAME=CUSTOMER,                                             C
               PARENT=0,                                               C
               BYTES=(279),                                            C
               RULES=(LLL,HERE)

      FIELD NAME=(CUSTID,SEQ,U),                                       C
               BYTES=4,                                                C
               START=1,                                                C
               TYPE=C,                                                 C
               DATATYPE=INT

      FIELD NAME=LASTNAME,                                             C
               BYTES=50,                                               C
               START=5,                                                C
               TYPE=C,                                                 C
               DATATYPE=CHAR

      FIELD NAME=FIRSTNM,                                              C
               EXTERNALNAME=FIRSTNAME,                                 C
               BYTES=50,                                               C
               START=55,                                               C
               TYPE=C,                                                 C
               DATATYPE=CHAR

      FIELD NAME=ADDRESS,                                              C
               BYTES=80,                                               C
               START=105,                                              C
               TYPE=C,                                                 C
               DATATYPE=CHAR

      FIELD NAME=CITY,                                                 C
               BYTES=25,                                               C
               START=185,                                              C
               TYPE=C,                                                 C
               DATATYPE=CHAR

      FIELD NAME=STATE,                                                C
               BYTES=2,                                                C
               START=210,                                              C
               TYPE=C,                                                 C
               DATATYPE=CHAR

      FIELD NAME=ZIPCODE,                                              C
               BYTES=15,                                               C
               START=212,                                              C
               TYPE=C,                                                 C
               DATATYPE=CHAR

      FIELD NAME=PHONE,                                                C
               BYTES=12,                                               C
               START=227,                                              C
               TYPE=C,                                                 C
               DATATYPE=CHAR

      FIELD NAME=STATUS,                                               C
               BYTES=1,                                                C
               START=239,                                              C
               TYPE=C,                                                 C
               DATATYPE=CHAR

      FIELD NAME=PASSWORD,                                             C
               BYTES=16,                                               C
               START=240,                                              C
               TYPE=C,                                                 C
               DATATYPE=CHAR

      FIELD NAME=CUSTTYPE,                                             C
               EXTERNALNAME=CUSTOMERTYPE,                              C
               BYTES=1,                                                C
               START=256,                                              C
               TYPE=C,                                                 C
               DATATYPE=CHAR

      FIELD NAME=LASTLOGI,                                             C
               EXTERNALNAME=LASTLOGIN,                                 C
               BYTES=23,                                               C
               START=257,                                              C
               TYPE=C,                                                 C
               DATATYPE=TIMESTAMP

      DFSMARSH INTERNALTYPECONVERTER=CHAR,                             C
               PATTERN='yyyy-MM-dd HH:mm:ss.SSS'

      DBDGEN
      FINISH
      END