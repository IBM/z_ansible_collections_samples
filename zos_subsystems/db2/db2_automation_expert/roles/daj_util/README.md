# IBM Db2 Automation Expert Role: daj_util

The `daj_util` role will generate and run Db2 utilities for the specified Db2 subsystem, LPAR, utility,
and objects.

This allows you to provide Db2 utilities as a self-service to developers and others.
Instead of opening a ticket or emailing the DBAs, the developer can run utilities on their own.

For more details see the API documented in UMS Swagger for the API `/plugin/automation-expert/utility/generate-utility`.

To retrieve the list of possible utilities (`daj_util_utilityname`) use the
[daj_list_utilities](../daj_list_utilities) role. The role take optional
`daj_util_profile_creator` and `daj_util_profile_name` parameters. 
If the parameters are not provided, a default
profile configured by the installer will be used. To retrive the list of
available profiles use the [daj_list_profiles](../daj_list_profiles) role.

`daj_util_objects` is a list of objects. See the UMS Swagger documentation for more details on
the allowed values.


The sample input below shows Ansible code to generate and run a REORG on Db2 subsystem DSN1. The default
profile for REORG is used.
```
    - name: Run Db2 utility
      ansible.builtin.include_role:
        name: daj_util
      vars:
      vars:
        daj_util_ssid: DSN1
        daj_util_sysname: SYS1
        daj_util_utilityname: REORG
        daj_util_objects: [
          {
            "objectType": "TS",
            "databaseName": "DSN8D13A",
            "objectName": "DSN8S13E"
          }
        ]
        daj_util_verbose: true
```

Sample output is shown below. As `daj_util_verbose: true` the playbook will output the generated utility statementsand also the output from running the REORG.
```
TASK [daj_util : Generate utility] ********************************************************************************
ok: [localhost]

TASK [daj_util : Register utility id] *****************************************************************************
ok: [localhost]

TASK [daj_util : Show utility id] *********************************************************************************
ok: [localhost] => {
    "msg": "Db2 Automation Expert evaluation id: 57096"
}

TASK [daj_util : Get generated utility] ***************************************************************************
ok: [localhost]

TASK [daj_util : Show utility] ************************************************************************************
ok: [localhost] => {
    "msg": [
        "TEMPLATE SREC                                                                ",
        "           UNIT     SYSDA                                                      ",
        "           DSN      'DSN1.REC.&DB..&SN..P&PART..&UNIQ.'                        ",
        "           MAXPRIME 00066666                                                   ",
        "           UNCNT    5                                                          ",
        "           MGMTCLAS DEL7DAY                                                    ",
        "           DISP     (MOD,CATLG,CATLG)                                          ",
        "                                                                               ",
        "  TEMPLATE R1LP0001                                                            ",
        "           UNIT     SYSDA                                                      ",
        "           DSN      'DSN1.IC.&DB..&SN..P&PART..&UNIQ.'                         ",
        "           MAXPRIME 00066666                                                   ",
        "           UNCNT    5                                                          ",
        "           MGMTCLAS DEL7DAY                                                    ",
        "           DISP     (MOD,CATLG,DELETE)                                         ",
        "                                                                               ",
        "  LISTDEF REO11001                                                             ",
        "         INCLUDE TABLESPACE DSN8D13A.DSN8S13E                                  ",
        "                                                                               ",
        "  REORG TABLESPACE LIST REO11001                                               ",
        "        SCOPE        ALL                                                       ",
        "        LOG          NO                                                        ",
        "        SORTDATA     YES                                                       ",
        "        INITCDDS     NO                                                        ",
        "        UNLDDN       SREC                                                      ",
        "        COPYDDN     (R1LP0001)                                                 ",
        "        SHRLEVEL     CHANGE                                                    ",
        "        TIMEOUT      TERM                                                      ",
        "        FORCE        READERS                                                   ",
        "        DRAIN_WAIT   120                                                       ",
        "        RETRY        10                                                        ",
        "        RETRY_DELAY  5                                                         ",
        "        DRAIN        WRITERS                                                   ",
        "        LONGLOG      CONTINUE                                                  ",
        "        DELAY        1200                                                      ",
        "        LOGRANGES    YES                                                       ",
        "        DRAIN_ALLPARTS NO                                                      ",
        "        FASTSWITCH   NO                                                        ",
        "        UNLOAD       CONTINUE                                                  ",
        "        STATISTICS                                                             ",
        "          TABLE(ALL)                                                           ",
        "          INDEX(ALL)                                                           ",
        "          REPORT       NO                                                      ",
        "          UPDATE       ALL                                                     ",
        "          INVALIDATECACHE NO                                                   ",
        "          HISTORY      NONE                                                    ",
        "          FORCEROLLUP  NO                                                      ",
        "        SORTDEVT     SYSDA"
    ]
}

TASK [daj_util : Run generated utility] ***************************************************************************
ok: [localhost]

TASK [daj_util : Get executed utility] ****************************************************************************
FAILED - RETRYING: [localhost]: Get executed utility (5 retries left).
ok: [localhost]

TASK [daj_util : Show utility] ************************************************************************************
ok: [localhost] => {
    "msg": [
        "1DSNU000I    244 10:47:11.30 DSNUGUTC - OUTPUT START FOR UTILITY, UTILID = A1063",
        " DSNU1045I   244 10:47:11.31 DSNUGTIS - PROCESSING SYSIN AS UNICODE UTF-8 ",
        "0DSNU050I    244 10:47:11.31 DSNUGUTC -  TEMPLATE SREC UNIT SYSDA DSN 'DSN1.REC.&DB..&SN..P&PART..&UNIQ.' MAXPRIME ",
        " 66666 UNCNT 5 MGMTCLAS DEL7DAY DISP(MOD, CATLG, CATLG)",
        " DSNU1035I   244 10:47:11.31 DSNUJTDR - TEMPLATE STATEMENT PROCESSED SUCCESSFULLY ",
        "0DSNU050I    244 10:47:11.31 DSNUGUTC -  TEMPLATE R1LP0001 UNIT SYSDA DSN 'DSN1.IC.&DB..&SN..P&PART..&UNIQ.' ",
        " MAXPRIME 66666 UNCNT 5 MGMTCLAS DEL7DAY DISP(MOD, CATLG, DELETE)",
        " DSNU1035I   244 10:47:11.31 DSNUJTDR - TEMPLATE STATEMENT PROCESSED SUCCESSFULLY ",
        "0DSNU050I    244 10:47:11.31 DSNUGUTC -  LISTDEF REO11001 INCLUDE TABLESPACE DSN8D13A.DSN8S13E",
        " DSNU1035I   244 10:47:11.32 DSNUILDR - LISTDEF STATEMENT PROCESSED SUCCESSFULLY",
        "0DSNU050I    244 10:47:11.32 DSNUGUTC -  REORG TABLESPACE LIST REO11001 SCOPE ALL LOG NO SORTDATA YES INITCDDS NO ",
        " UNLDDN SREC COPYDDN(R1LP0001) SHRLEVEL CHANGE TIMEOUT TERM FORCE READERS DRAIN_WAIT 120 RETRY 10 RETRY_DELAY 5 ",
        " DRAIN WRITERS LONGLOG CONTINUE DELAY 1200 LOGRANGES YES DRAIN_ALLPARTS NO FASTSWITCH NO UNLOAD CONTINUE STATISTICS ",
        " TABLE(ALL) INDEX(ALL) REPORT NO UPDATE ALL INVALIDATECACHE NO HISTORY NONE FORCEROLLUP NO SORTDEVT SYSDA",
        " DSNU1033I   244 10:47:11.36 DSNUGULM - PROCESSING LIST ITEM: TABLESPACE DSN8D13A.DSN8S13E",
        " DSNU2901I !DSN1 244 10:47:11.59 DSNURMAP - MAPPING TABLE DB2USER.RTE6AE7C_MAPTABLE_A1063_0000",
        "             AND MAPPING INDEX DB2USER.IXE6AE7C_MAPINDEX_A1063_0000 ",
        "             CREATED IN DSN01630.RTE6AE7C ",
        " DSNU2903I   244 10:47:11.66 DSNURORG - PARTITION LEVEL INLINE COPY DATASETS WILL BE ALLOCATED",
        " DSNU1038I   244 10:47:11.69 DSNUGDYN - DATASET ALLOCATED.  TEMPLATE=R1LP0001 ",
        "                        DDNAME=SYS00005   ",
        "                        DSN=DSN1.IC.DSN8D13A.DSN8S13E.P00001.I676OR16 ",
        " DSNU2904I   244 10:47:11.70 DSNURPCT - DATA RECORDS WILL BE UNLOADED VIA TABLE SPACE SCAN FROM TABLESPACE",
        " DSN8D13A.DSN8S13E",
        " DSNU1038I   244 10:47:11.73 DSNUGDYN - DATASET ALLOCATED.  TEMPLATE=SREC ",
        "                        DDNAME=SYS00006   ",
        "                        DSN=DSN1.REC.DSN8D13A.DSN8S13E.P00000.I676OR3F",
        " DSNU251I    244 10:47:11.74 DSNURULD - UNLOAD PHASE STATISTICS - NUMBER OF RECORDS UNLOADED=0 FOR TABLESPACE ",
        " DSN8D13A.DSN8S13E PART 1 ",
        " DSNU252I    244 10:47:11.74 DSNURULD - UNLOAD PHASE STATISTICS - NUMBER OF RECORDS UNLOADED=0 FOR TABLESPACE ",
        " DSN8D13A.DSN8S13E",
        " DSNU250I    244 10:47:11.74 DSNURULD - UNLOAD PHASE COMPLETE, ELAPSED TIME=00:00:00",
        " DSNU3345I   244 10:47:11.79 DSNURPIB - MAXIMUM UTILITY PARALLELISM IS 5 BASED ON NUMBER OF PARTITIONS, INDEXES AND ",
        " STATISTICS ",
        " DSNU397I    244 10:47:11.79 DSNURPIB - NUMBER OF TASKS CONSTRAINED BY CPUS TO 5  ",
        " DSNU303I  !DSN1 244 10:47:11.80 DSNURWT - (RE)LOAD PHASE STATISTICS - NUMBER OF RECORDS=0 FOR TABLE DSN8D13A.THAATEST",
        " PART=1 ",
        " DSNU304I  !DSN1 244 10:47:11.80 DSNURWT - (RE)LOAD PHASE STATISTICS - NUMBER OF RECORDS=0 FOR TABLE DSN8D13A.THAATEST",
        " DSNU302I    244 10:47:11.80 DSNURILD - (RE)LOAD PHASE STATISTICS - NUMBER OF INPUT RECORDS PROCESSED=0 ",
        " DSNU300I    244 10:47:11.80 DSNURILD - (RE)LOAD PHASE COMPLETE, ELAPSED TIME=00:00:00",
        " DSNU1138I !DSN1 244 10:47:11.81 DSNURLOG - DRAIN WRITERS WITH START TIME 2023-09-01-10.47.11.816246 HAS COMPLETED",
        " SUCCESSFULLY ",
        " DSNU1138I !DSN1 244 10:47:11.81 DSNURLOG - DRAIN ALL WITH START TIME 2023-09-01-10.47.11.817521 HAS COMPLETED",
        " SUCCESSFULLY ",
        " DSNU1139I   244 10:47:11.82 DSNURLGD - FINAL LOG ITERATION STATISTICS. NUMBER OF LOG RECORDS = 0 ",
        " DSNU386I    244 10:47:11.82 DSNURLGD - LOG PHASE STATISTICS. NUMBER OF ITERATIONS = 2, NUMBER OF LOG RECORDS = 0 ",
        " DSNU385I    244 10:47:11.82 DSNURLGD - LOG PHASE COMPLETE, ELAPSED TIME = 00:00:00 ",
        " DSNU400I    244 10:47:11.83 DSNURBID - COPY PROCESSED FOR TABLESPACE DSN8D13A.DSN8S13E PART 1",
        "                       NUMBER OF PAGES=4",
        "                       AVERAGE PERCENT FREE SPACE PER PAGE =  0.00",
        "                       PERCENT OF CHANGED PAGES =100.00 ",
        "                       ELAPSED TIME=00:00:00",
        " DSNU387I    244 10:47:11.92 DSNURSWT - SWITCH PHASE COMPLETE, ELAPSED TIME = 00:00:00",
        " DSNU428I    244 10:47:11.93 DSNURSWT - DB2 IMAGE COPY SUCCESSFUL FOR TABLESPACE DSN8D13A.DSN8S13E PARTITION 1",
        " DSNU610I  !DSN1 244 10:47:11.94 DSNUSUTP - SYSTABLEPART CATALOG UPDATE FOR DSN8D13A.DSN8S13E SUCCESSFUL",
        " DSNU610I  !DSN1 244 10:47:11.94 DSNUSUPT - SYSTABSTATS CATALOG UPDATE FOR DSN8D13A.THAATEST SUCCESSFUL ",
        " DSNU610I  !DSN1 244 10:47:11.94 DSNUSUPC - SYSCOLSTATS CATALOG UPDATE FOR DSN8D13A.THAATEST SUCCESSFUL ",
        " DSNU610I  !DSN1 244 10:47:11.94 DSNUSUTB - SYSTABLES CATALOG UPDATE FOR DSN8D13A.THAATEST SUCCESSFUL ",
        " DSNU610I  !DSN1 244 10:47:11.94 DSNUSUCO - SYSCOLUMNS CATALOG UPDATE FOR DSN8D13A.THAATEST SUCCESSFUL",
        " DSNU610I  !DSN1 244 10:47:11.94 DSNUSUTS - SYSTABLESPACE CATALOG UPDATE FOR DSN8D13A.DSN8S13E SUCCESSFUL ",
        " DSNU620I  !DSN1 244 10:47:11.94 DSNUSEF2 - RUNSTATS CATALOG TIMESTAMP = 2023-09-01-10.47.11.700970 ",
        " DSNU010I    244 10:47:12.17 DSNUGBAC - UTILITY EXECUTION COMPLETE, HIGHEST RETURN CODE=0 ",
        " DSNU010I    244 10:47:12.17 DSNUGBAC - UTILITY EXECUTION COMPLETE, HIGHEST RETURN CODE=0"
    ]
}
```


# Copyright

© Copyright Rocket Software 2023

# License

Licensed under [Apache License,
Version 2.0](https://opensource.org/licenses/Apache-2.0)
