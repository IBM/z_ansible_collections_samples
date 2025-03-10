{{ JOB_CARD }}                                                                   
//*********************************************************************
//* Licensed Materials - Property of IBM                            ***
//* 5698-AA4                                                    8.2 ***
//* © Copyright IBM Corp. 2011, 2020.  All rights reserved.         ***
//* © Copyright HCL Technologies Ltd. 2020, 2022.                   ***
//*                                                                 ***
//*Change Activity:                                                 ***
//*Flg=Reason   Vers Date    Name Description                       ***
//*-----------  ---- ------- ---- ----------------------------------***
//*$P3=OA64003  V8R2 16Nov22 ADL                                    ***
//*GA           V8R2 11Dec20 HCL                                    ***
//*$P2=OA45327  V8R1 29Jul14 ADL                                    ***
//*$P1=OA42802  V8R1 02Aug13 ADL                                    ***
//*********************************************************************
//* New in V8.2:                                                     **
//*  SET OUTFMT=XML   <= was previously "XLS"                       ** 
//*  @headers         <= new optional parameter (see example below) ** 
//*   @headers = full  <= displays report title, timestamp, list    ** 
//*                         of parameters, and column headings       **
//*   @headers = title <= displays report title, timestamp and      ** 
//*                         column headings                          **
//*   @headers = none  <= displays column headings only             ** 
//*                                                                  **
//* Analyzer batch mode job:                                         **
//* a. Parameters are case sensitive                                 **
//* b. All parameters must start in column 1.                        **
//* c. In OUTDSN, increase the size according to report output.      **
//* d. In WORK0, increase the size according to report output.       **
//*    WORK0 can be changed to a permanent dataset, if required.     **
//* e. In //TPARAM, setting TRACE = Y will force an Analyzer trace.  **
//*                                                                  **
//* Records are fetched and stored in the WORK0 file.                **
//* Once the fetch is complete, records are then physically written  **
//* to the OUTDSN dataset, in 8K blocks.                             **
//*                                                                  **
//*********************************************************************
//   JCLLIB  ORDER=(IZSAM.V820.INST.REPDB1.JCLLIB)                     
//*                                                                    
//*  SET OUTFMT=TXT                                                    
//*  SET OUTFMT=XLS                                                    
//*  SET OUTFMT=XML                                                    
//   SET OUTFMT=CSV                                                    
//*  SET OUTFMT=HTM                                                    
//*                                                                    
//   SET OUTDSN=&SYSUID..IZSAMALZ.&OUTFMT  Output dsn                  
//*                                                                    
//**DELOLD   EXEC PGM=IEFBR14                                          
//**OUTDSN   DD DSN=&OUTDSN,                                           
//**         DISP=(MOD,DELETE),UNIT=SYSALLDA,SPACE=(TRK,(0,0))         
//*                                                                    
//ALLOC    EXEC PGM=IEFBR14                                            
//OUTDSN   DD DISP=(MOD,CATLG),DSN=&OUTDSN,                            
//            UNIT=SYSALLDA,SPACE=(CYL,(200,100),RLSE)                 
/*                                                                     
//*********************************************************************
//ANALYZER EXEC PGM=HSICANLZ,REGION=0M,TIME=NOLIMIT                    
//STEPLIB  DD DISP=SHR,DSN=DSN.V13R1M0.SDSNEXIT                        
//         DD DISP=SHR,DSN=DSN.V13R1M0.SDSNLOAD                        
//         DD DISP=SHR,DSN=HSI.V8R2M0.SHSIMOD1                         
//         DD DISP=SHR,DSN=CEE.SCEERUN                                 
//         DD DISP=SHR,DSN=CBC.SCLBDLL                                 
//SYSPRINT DD SYSOUT=*,LRECL=500                                       
//HSIANL1  DD DISP=SHR,DSN=HSI.V8R2M0.SHSIANL1                         
//HSIANL2  DD DISP=SHR,DSN=HSI.V8R2M0.SHSIANL2                         
//HSICUST  DD DISP=SHR,                                                
//       DSN=IZSAM.V820.INST.REPDB1.PARMLIB(HSISANCQ)                  
//*HSINLS  DD DISP=SHR,DSN=HSI.V8R2M0.SHSIANL1(HSINLSJP)
//DSNAOINI DD DISP=SHR,                                 
//       DSN=IZSAM.V820.INST.REPDB1.PARMLIB(HSISCLI)    
//WORK0    DD DSN=&WORK0,DISP=(NEW,DELETE),             
//            UNIT=SYSALLDA,SPACE=(CYL,(200,100),RLSE)  
//TPARAM   DD DUMMY                                     
//OUTPUT1  DD  DISP=OLD,DSN=&OUTDSN,LRECL=2000          
//APPSTATS DD SYSOUT=*,LRECL=1000                       
//APPTRACE DD SYSOUT=*                                  
//SYSIN    DD  *                                        
/disc/product_libraries                                 
class       = All Classes                               
repository  = REPHLQ1                                   
exclunknown = off                                       
showclass   = off                                       
outlimit    = 1000                                      
/*                                                      