//EQAWDDEF JOB CLASS=A,
//     MSGCLASS=A,REGION=0M,MSGLEVEL=(1,1),NOTIFY=&SYSUID
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
//*  2) Change #globalcsi to the data set name of your global         *
//*     CSI data set.                                                 *
//*  3) Change #tzone to the name of your target zone.                *
//*  4) Change #dzone to the name of your distribution zone.          *
//*  5) Change #hlq to the appropriate high-level qualifier.          *
//*  6) This job uses the recommended data set placement for the      *
//*     target libraries.                                             *
//*     Change #tvol to the volser of the target volume.              *
//*  7) Change #dvol to the volser of the distribution volume.        *
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
//SMPCSI  DD   DSN={{ smphlq }}.CSI,
//             DISP=SHR
//SMPCNTL DD *
    SET   BDY(TARGET).
     UCLIN.
      ADD DDDEF (SEQAAUTH)
          DATASET({{ eqa_hlq }}.SEQAAUTH)
          UNIT(SYSALLDA)
          VOLUME({{ tvol }})
          WAITFORDSN
          SHR.
      ADD DDDEF (SEQAMOD)
          DATASET({{ eqa_hlq }}.SEQAMOD)
          UNIT(SYSALLDA)
          VOLUME({{ tvol }})
          WAITFORDSN
          SHR.
      ADD DDDEF (SEQASAMP)
          DATASET({{ eqa_hlq }}.SEQASAMP)
          UNIT(SYSALLDA)
          VOLUME({{ tvol }})
          WAITFORDSN
          SHR.
      ADD DDDEF (SEQAEXEC)
          DATASET({{ eqa_hlq }}.SEQAEXEC)
          UNIT(SYSALLDA)
          VOLUME({{ tvol }})
          WAITFORDSN
          SHR.
      ADD DDDEF (SEQATLIB)
          DATASET({{ eqa_hlq }}.SEQATLIB)
          UNIT(SYSALLDA)
          VOLUME({{ tvol }})
          WAITFORDSN
          SHR.
      ADD DDDEF (SEQAZFS)
       /* do NOT alter PATH, correction is done in step DEFPATH */
          PATH('/usr/lpp/IBM/debug/IBM/') .
      ADD DDDEF (AEQAMOD)
          DATASET({{ eqa_hlq }}.AEQAMOD)
          UNIT(SYSALLDA)
          VOLUME({{ dvol }})
          WAITFORDSN
          SHR.
      ADD DDDEF (AEQASAMP)
          DATASET({{ eqa_hlq }}.AEQASAMP)
          UNIT(SYSALLDA)
          VOLUME({{ dvol }})
          WAITFORDSN
          SHR.
      ADD DDDEF (AEQAEXEC)
          DATASET({{ eqa_hlq }}.AEQAEXEC)
          UNIT(SYSALLDA)
          VOLUME({{ dvol }})
          WAITFORDSN
          SHR.
      ADD DDDEF (AEQATLIB)
          DATASET({{ eqa_hlq }}.AEQATLIB)
          UNIT(SYSALLDA)
          VOLUME({{ dvol }})
          WAITFORDSN
          SHR.
     ENDUCL.
/*
//*
//*********************************************************************
//*  Allocate DDDEF entries in distribution zone                      *
//*********************************************************************
//DDDEFD  EXEC PGM=GIMSMP,REGION=0M
//SMPCSI  DD   DSN={{ smphlq }}.CSI,
//             DISP=SHR
//SMPCNTL DD *
    SET   BDY(DLIB).
     UCLIN.
      ADD DDDEF (AEQAMOD)
          DATASET({{ eqa_hlq }}.AEQAMOD)
          UNIT(SYSALLDA)
          VOLUME({{ dvol }})
          WAITFORDSN
          SHR.
      ADD DDDEF (AEQASAMP)
          DATASET({{ eqa_hlq }}.AEQASAMP)
          UNIT(SYSALLDA)
          VOLUME({{ dvol }})
          WAITFORDSN
          SHR.
      ADD DDDEF (AEQAEXEC)
          DATASET({{ eqa_hlq }}.AEQAEXEC)
          UNIT(SYSALLDA)
          VOLUME({{ dvol }})
          WAITFORDSN
          SHR.
      ADD DDDEF (AEQATLIB)
          DATASET({{ eqa_hlq }}.AEQATLIB)
          UNIT(SYSALLDA)
          VOLUME({{ dvol }})
          WAITFORDSN
          SHR.
      ADD DDDEF (AEQAZFS)
          DATASET({{ eqa_hlq }}.AEQAZFS)
          UNIT(SYSALLDA)
          VOLUME({{ dvol }})
          WAITFORDSN
          SHR.
     ENDUCL.
/*
//*
//*********************************************************************
//*  Allocate DDDEF entries new as of Debug Tool V3.1                 *
//*********************************************************************
//DDDEFV3T EXEC PGM=GIMSMP,REGION=0M
//SMPCSI  DD   DSN={{ smphlq }}.CSI,
//             DISP=SHR
//SMPCNTL DD *
    SET   BDY(TARGET).
     UCLIN.
      ADD DDDEF (SEQABMOD)
          DATASET({{ eqa_hlq }}.SEQABMOD)
          UNIT(SYSALLDA)
          VOLUME({{ tvol }})
          WAITFORDSN
          SHR.
      ADD DDDEF (SEQAMENU)
          DATASET({{ eqa_hlq }}.SEQAMENU)
          UNIT(SYSALLDA)
          VOLUME({{ tvol }})
          WAITFORDSN
          SHR.
      ADD DDDEF (SEQAPENU)
          DATASET({{ eqa_hlq }}.SEQAPENU)
          UNIT(SYSALLDA)
          VOLUME({{ tvol }})
          WAITFORDSN
          SHR.
      ADD DDDEF (SEQASENU)
          DATASET({{ eqa_hlq }}.SEQASENU)
          UNIT(SYSALLDA)
          VOLUME({{ tvol }})
          WAITFORDSN
          SHR.
      ADD DDDEF (SEQAMENP)
          DATASET({{ eqa_hlq }}.SEQAMENP)
          UNIT(SYSALLDA)
          VOLUME({{ tvol }})
          WAITFORDSN
          SHR.
      ADD DDDEF (SEQAPENP)
          DATASET({{ eqa_hlq }}.SEQAPENP)
          UNIT(SYSALLDA)
          VOLUME({{ tvol }})
          WAITFORDSN
          SHR.
      ADD DDDEF (SEQASENP)
          DATASET({{ eqa_hlq }}.SEQASENP)
          UNIT(SYSALLDA)
          VOLUME({{ tvol }})
          WAITFORDSN
          SHR.
      ADD DDDEF (AEQAMENU)
          DATASET({{ eqa_hlq }}.AEQAMENU)
          UNIT(SYSALLDA)
          VOLUME({{ dvol }})
          WAITFORDSN
          SHR.
      ADD DDDEF (AEQAPENU)
          DATASET({{ eqa_hlq }}.AEQAPENU)
          UNIT(SYSALLDA)
          VOLUME({{ dvol }})
          WAITFORDSN
          SHR.
      ADD DDDEF (AEQASENU)
          DATASET({{ eqa_hlq }}.AEQASENU)
          UNIT(SYSALLDA)
          VOLUME({{ dvol }})
          WAITFORDSN
          SHR.
      ADD DDDEF (AEQAMENP)
          DATASET({{ eqa_hlq }}.AEQAMENP)
          UNIT(SYSALLDA)
          VOLUME({{ dvol }})
          WAITFORDSN
          SHR.
      ADD DDDEF (AEQAPENP)
          DATASET({{ eqa_hlq }}.AEQAPENP)
          UNIT(SYSALLDA)
          VOLUME({{ dvol }})
          WAITFORDSN
          SHR.
      ADD DDDEF (AEQASENP)
          DATASET({{ eqa_hlq }}.AEQASENP)
          UNIT(SYSALLDA)
          VOLUME({{ dvol }})
          WAITFORDSN
          SHR.
     ENDUCL.
/*
//*
//DDDEFV3D EXEC PGM=GIMSMP,REGION=0M
//SMPCSI  DD   DSN={{ smphlq }}.CSI,
//             DISP=SHR
//SMPCNTL DD *
    SET   BDY(DLIB).
     UCLIN.
      ADD DDDEF (AEQAMENU)
          DATASET({{ eqa_hlq }}.AEQAMENU)
          UNIT(SYSALLDA)
          VOLUME({{ dvol }})
          WAITFORDSN
          SHR.
      ADD DDDEF (AEQAPENU)
          DATASET({{ eqa_hlq }}.AEQAPENU)
          UNIT(SYSALLDA)
          VOLUME({{ dvol }})
          WAITFORDSN
          SHR.
      ADD DDDEF (AEQASENU)
          DATASET({{ eqa_hlq }}.AEQASENU)
          UNIT(SYSALLDA)
          VOLUME({{ dvol }})
          WAITFORDSN
          SHR.
      ADD DDDEF (AEQAMENP)
          DATASET({{ eqa_hlq }}.AEQAMENP)
          UNIT(SYSALLDA)
          VOLUME({{ dvol }})
          WAITFORDSN
          SHR.
      ADD DDDEF (AEQAPENP)
          DATASET({{ eqa_hlq }}.AEQAPENP)
          UNIT(SYSALLDA)
          VOLUME({{ dvol }})
          WAITFORDSN
          SHR.
      ADD DDDEF (AEQASENP)
          DATASET({{ eqa_hlq }}.AEQASENP)
          UNIT(SYSALLDA)
          VOLUME({{ dvol }})
          WAITFORDSN
          SHR.
     ENDUCL.
/*
//*
//*********************************************************************
//*  Allocate DDDEF entry new as of Debug Tool V4.1                   *
//*********************************************************************
//DDDEFV4T EXEC PGM=GIMSMP,REGION=0M
//SMPCSI  DD   DSN={{ smphlq }}.CSI,
//             DISP=SHR
//SMPCNTL DD *
    SET   BDY(TARGET).
     UCLIN.
      ADD DDDEF (SEQALPA)
          DATASET({{ eqa_hlq }}.SEQALPA)
          UNIT(SYSALLDA)
          VOLUME({{ tvol }})
          WAITFORDSN
          SHR.
     ENDUCL.
/*
//*
//*********************************************************************
//*  Allocate DDDEF entries for CALLLIBS libraries                    *
//*********************************************************************
//DDDEFSHT EXEC PGM=GIMSMP,REGION=0M
//SMPCSI  DD   DSN={{ smphlq }}.CSI,
//             DISP=SHR
//SMPCNTL DD *
    SET   BDY(TARGET).
     UCLIN.
      ADD DDDEF (SCEELKED)
          DATASET(CEE.SCEELKED)
          UNIT(SYSALLDA)
          WAITFORDSN
          SHR.
      ADD DDDEF (CSSLIB)
          DATASET(SYS1.CSSLIB)
          UNIT(SYSALLDA)
          WAITFORDSN
          SHR.
      ADD DDDEF (SEZATCP)
          DATASET(EZA.SEZATCP)
          UNIT(SYSALLDA)
          WAITFORDSN
          SHR.
     ENDUCL.
//*
//*  Change the string "-PathPrefix-" to the appropriate
//*  high level directory name. For users installing in the root,
//*  this would be "/". For others, the high level directory may
//*  be something like "/service/", or a more meaningful name.
//*
//*  "-PathPrefix-" must match the value used in job EQAWMKD.
//*
//*  Verify that the changed path statement does not contain
//*  double slashes (such as //usr/lpp) prior to running this step.
//*
//DEFPATH  EXEC PGM=GIMSMP,REGION=0M
//SMPCSI  DD   DSN={{ smphlq }}.CSI,
//             DISP=SHR
//SMPCNTL DD *
    SET   BDY(TARGET).
     ZONEEDIT DDDEF.
      CHANGE PATH('/usr/lpp/IBM/debug'*,
       '{{ eqa_path_prefix }}usr/lpp/IBM/debug'*) .
     ENDZONEEDIT .
//*

