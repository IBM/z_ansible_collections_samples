//UNZIP JOB CLASS=A,                                                   
//   MSGCLASS=A,REGION=0M,MSGLEVEL=(1,1),NOTIFY=&SYSUID
//GIMUNZIP EXEC PGM=GIMUNZIP,REGION=0M,COND=(0,LT)
//*STEPLIB  DD DISP=SHR,DSN=SYS1.MIGLIB
//SYSUT3   DD UNIT=SYSALLDA,SPACE=(CYL,(50,10))
//SYSUT4   DD UNIT=SYSALLDA,SPACE=(CYL,(25,5))
//SMPOUT   DD SYSOUT=*
//SYSPRINT DD SYSOUT=*
//SMPDIR   DD PATHDISP=KEEP,
//   PATH='{{ remote_path_relfiles }}'
//SYSIN    DD *
<GIMUNZIP>
<ARCHDEF archid="IBM.HADRH00.SMPMCS"
         newname="{{ zosmf_user }}.IBM.HADRH00.SMPMCS"/>
<ARCHDEF archid="IBM.HADRH00.F1"
         newname="{{ zosmf_user }}.IBM.HADRH00.F1"/>
<ARCHDEF archid="IBM.HADRH00.F2"
         newname="{{ zosmf_user }}.IBM.HADRH00.F2"/>
<ARCHDEF archid="IBM.HADRH00.F3"
         newname="{{ zosmf_user }}.IBM.HADRH00.F3"/>
<ARCHDEF archid="IBM.HADRH00.F4"
         newname="{{ zosmf_user }}.IBM.HADRH00.F4"/>
<ARCHDEF archid="IBM.HADRH00.F5"
         newname="{{ zosmf_user }}.IBM.HADRH00.F5"/>
<ARCHDEF archid="IBM.HADRH00.F6"
         newname="{{ zosmf_user }}.IBM.HADRH00.F6"/>
<ARCHDEF archid="IBM.HADRH00.F7"
         newname="{{ zosmf_user }}.IBM.HADRH00.F7"/>
<ARCHDEF archid="IBM.HADRH00.F8"
         newname="{{ zosmf_user }}.IBM.HADRH00.F8"/>
<ARCHDEF archid="IBM.HADRH00.F9"
         newname="{{ zosmf_user }}.IBM.HADRH00.F9"/>
<ARCHDEF archid="IBM.HADRH00.F10"
         newname="{{ zosmf_user }}.IBM.HADRH00.F10"/>
<ARCHDEF archid="IBM.HADRH00.F11"
         newname="{{ zosmf_user }}.IBM.HADRH00.F11"/>
</GIMUNZIP>
//*
 
