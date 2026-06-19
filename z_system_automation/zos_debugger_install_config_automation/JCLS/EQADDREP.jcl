//EQADDREP JOB CLASS=A,                                                 
//         MSGCLASS=A,REGION=0M,MSGLEVEL=(1,1),NOTIFY=&SYSUID
//*********************************************************************
//* Licensed Materials - Property of IBM                              *
//* 5724-T07                                                          *
//* Copyright IBM Corp. 1997, 2024 All Rights Reserved                *
//*                                                                   *
//* US Government Users Restricted Rights - Use, duplication or       *
//* disclosure restricted by GSA ADP Schedule Contract with IBM Corp. *
//*                                                                   *
//*********************************************************************
//*                                                                   *
//*  This JCL will create DDDEF entries for product                   *
//*    IBM z/OS Debugger                                              *
//*    17.0                                                           *
//*                                                                   *
//* CAUTIONS:                                                         *
//* A) This job contains case sensitive path statements.              *
//* B) This is neither a JCL procedure nor a complete job.            *
//*    Before using this JCL, you will have to make the following     *
//*    modifications:                                                 *
//*                                                                   *
//*  1) Add the job parameters to meet your system requirements.      *
//*                                                                   *
//*  You can either make the changes in steps 2 through 9 below       *
//*  by hand or by invoking the ISPF Editor macro EQAWEDIT,           *
//*  which you copied from the product data sets earlier.             *
//*  Consult the instructions in EQAWEDIT for more information.       *
//*                                                                   *
//*  2) Change IBMUSER.HADRH00.CSI to the data set name of your global *
//*     CSI data set.                                                 *
//*  3) Change TARGET to the name of your target zone.                *
//*  4) Change DLIB to the name of your distribution zone.            *
//*  5) Change IBMUSER to the appropriate high-level qualifier.       *
//*  6) This job uses the recommended data set placement for the      *
//*     target libraries.                                             *
//*     Change T50735 to the volser of the target volume.             *
//*  7) Change T50731 to the volser of the distribution volume.       *
//*  8) Change the string "-PathPrefix-" in step DEFPATH to the       *
//*     high-level directory name, as used in job EQAWMKD.            *
//*     Please note that the replacement string is case sensitive.    *
//*  9) Examine the CALLLIBS library names (for example,              *
//*     CEE.SCEELKED) in step DDDEFSHT, and make changes to the       *
//*     high-level qualifiers as appropriate.                         *
//*                                                                   *
//*  If you specify a volume for any data set in this job, you        *
//*  must also specify the same volume in the corresponding           *
//*  data set allocation job, EQAWALOC.                               *
//*                                                                   *
//*  These changes must be done before running this job!              *
//*                                                                   *
//*  Notes:                                                           *
//*                                                                   *
//*  1. This job should complete with a return code 0.  However,      *
//*     if some or all of these DDDEF entries already exist, then     *
//*     the job will complete with a return code 8. You will have     *
//*     to examine the output and determine whether or not the        *
//*     existing entries should be replaced. You can change           *
//*     the 'ADD' to 'REP' in this job to replace existing            *
//*     entries.                                                      *
//*                                                                   *
//*  2. See the program directory for any data sets that must         *
//*     be in the SYSLIB concatenation.                               *
//*                                                                   *
//*  3. The unit and volume parameters may be deleted for             *
//*     data sets that are cataloged.                                 *
//*                                                                   *
//*  4. Ensure that -PathPrefix- is an absolute path name and begins  *
//*     and ends with a slash (/).                                    *
//*                                                                   *
//*  5. Run only the steps that are applicable to your installation:  *
//*                                                                   *
//*     Steps DDDEFT and DDDEFD contain all DDDEFs as of              *
//*        Debug Tool V1.3.                                           *
//*                                                                   *
//*     Steps DDDEFV3T and DDDEFV3D contain the DDDEFs new as of      *
//*        Debug Tool V3.1.                                           *
//*                                                                   *
//*     Step DDDEFV4T contains the DDDEF new as of                    *
//*        Debug Tool V4.1.                                           *
//*                                                                   *
//*     Step DDDEFSHT contains the DDDEFs for CALLLIBS libraries.     *
//*                                                                   *
//*     If this is the first time you install z/OS Debugger, or you   *
//*     do not plan on reusing the DDDEFs in prior releases,          *
//*     then you must run ALL of the DDDEF steps.                     *
//*                                                                   *
//*  5. In step DDDEFSHT, DDDEF entries are created for               *
//*     data sets that may already exist.  If any of these            *
//*     entries already exist in the zone, remove or comment out      *
//*     these ADD DDDEF commands.                                     *
//*                                                                   *
//*********************************************************************
//*  Allocate DDDEF entries in target zone                            *
//*********************************************************************
//DDDEFT  EXEC PGM=GIMSMP,REGION=0M
//SMPCSI  DD   DSN=IBMUSER.HADRH00.CSI,
//             DISP=SHR
//SMPCNTL DD *
    SET   BDY(TARGET).
     UCLIN.
      REP DDDEF (SEQAZFS)
       /* do NOT alter PATH, correction is done in step DEFPATH */
          PATH('/u/ibmuser/dbg17/') .
     ENDUCL.
/*
//*
 