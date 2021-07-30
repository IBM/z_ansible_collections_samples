**********************************************************************
* This is a mapping of PSB names to Java samples.
* The Java samples are delivered in the OpenDBIVP.jar file,
* which can be found in /usr/lpp/ims/ims15/imsjava/samples.
* The location of this file must be specified separately
* by the DFSJVMMS member in the shareable application classpath.
* PSB          Regions   Java programs
* --------     -------   -------------
* DFSIVP37     JMP       IMSIVP.class
* DFSIVP67     JBP       IMSIVPJBP.class
**********************************************************************
DFSIVP37=samples/ivp/ims/IMSIVP
IBGHIST=nazare/jmp/controller/QueryTransaction
*
**********************************************************************
* The exec parms in the JBP region proc are set as:
* MBR=DFSJBP and PSB=DFSIVP67
**********************************************************************
DFSJBP=samples/ivp/ims/IMSIVPJBP
*
DFSCATS2=samples/ivp/opendb/OpenDBCatalogSQLType2
DFSCATD2=samples/ivp/opendb/OpenDBCatalogDLIType2
*