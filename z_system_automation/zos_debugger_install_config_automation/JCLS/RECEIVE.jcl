//RECEIVE JOB CLASS=A,
//     MSGCLASS=A,REGION=0M,MSGLEVEL=(1,1),NOTIFY=&SYSUID
//RECEIVE  EXEC PGM=GIMSMP,REGION=0M,PARM='DATE=U'
//SMPCSI   DD DISP=SHR,
//            DSN={{ smphlq }}.CSI
//SMPHOLD  DD DUMMY                                                 
//SMPPTFIN DD DISP=SHR,DSN={{ zosmf_user }}.{{ ptf_file }}
//*                                                                
//SMPCNTL  DD *                                                     
    SET BDY(GLOBAL).
    RECEIVE                                                            
    SYSMODS LIST .
/*                                                                 
