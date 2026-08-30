REXX_TEMPLATE = """/* rexx */
Address LINK 'CSLULXSB'
 /********************************************************************/
 /* any parms are treated as an IMS operator command to issue.       */
 /********************************************************************/
name="%s"
Parse upper var name theIMScmd '~' route

 /********************************************************************/
 /* PROGRAM NAME: REXXGTP                                            */
 /*                                                                  */
 /* PROGRAM TYPE: REXX EXEC                                          */
 /*                                                                  */
 /* PROGRAMMER(S):   Pedro Vera                                      */
 /*                  Jerry Li                                        */
 /*                                                                  */
 /* DATE:         May 08, 2020                                       */
 /*                                                                  */
 /* PURPOSE:      This REXX EXEC will display a TSO SPOC-like        */
 /*               listing of IMSplex tabular data.                   */
 /*               Special processing: the rows are grouped so        */
 /*               will data that fits in one line is grouped         */
 /*               together.                                          */
 /*                                                                  */
 /*               For example:                                       */
 /*                    row1  col01 col02 col03 ... col09             */
 /*                    row2  col01 col02 col03                       */
 /*                    row3  col01 col02 col03                       */
 /*                                                                  */
 /*                    row1  col11 col12 col13 ... col19             */
 /*                    row2  col11 col12 col13                       */
 /*                    row3  col11 col12 col13                       */
 /*                                                                  */
 /*               Rexx Environment:    CSLULXSB                      */
 /*               IMSplex Environment: IMSSPOC                       */
 /*                                                                  */
 /* LIBRARY:      IMSTESTG.SPOC.CLIST(RXSPWIDE)                      */
 /*                                                                  */
 /*  INPUT:       Variables passed from PL/X programs which          */
 /*               obtained the command responses returned by OM.     */
 /*                                                                  */
 /*  OUTPUT:      On screen display of stem variables from XML tags. */
 /*                                                                  */
 /* MAINTENANCE:                                                     */
 /* NAME DATE      CHANGE DESCRIPTION                                */
 /* ---- --------  ------------------------------------------------- */
 /* PSV  08-28-06  Program Created                                   */
 /* JL   05-08-20  Program Updated for Ansible module use.           */
 /*                                                                  */
 /********************************************************************/
wideload. = ''
If rc = 0 Then
 Do
 /*  signal on error */
   Address IMSSPOC
   "ims %s"
   "wait 5:00"
    cartid = 'TEST13'
   "CART" cartid
   if route ^= '' then
     "ROUTE" route

  /* issue the command */
    theIMScmd

  /********************************************************/
  /* valid name and matches stemname, RC = (00000000x)    */
  /********************************************************/
   results = cslulgtp('wideload.', cartid,"1:30")

   "End"   /* clean up SPOC environment */


Say '{'

Say '  "spoc_api_return": '
Say '   {'
Say '     "imsrc": "'imsrc'",'
Say '     "reason": "'imsreason'"'
Say '   }'
Say '    , '

 /*********************************************************************/
 /* Within the CTL group a set of supgroup information may be present,*/
 /* if so, display the information.                                   */
 /*********************************************************************/
Say '  "command_return":'
Say '   {'
/* Say '     "ctl.omname": "'wideload.ctl.omname'",' */
/* Say '     "ctl.omvsn": "'wideload.ctl.omvsn'",'      */
/* Say '     "ctl.xmlvsn": "'wideload.ctl.xmlvsn'",'       */
/* Say '     "ctl.statime": "'wideload.ctl.statime'",'     */
/* Say '     "ctl.stotime": "'wideload.ctl.stotime'",'    */
/* Say '     "ctl.rqsttkn1": "'wideload.ctl.rqsttkn1'",'  */

 /*********************************************************************/
 /* Multiple groupings of the CMDERR may be present.                  */
 /* Within each of these CMDERR groups there may be subgroups of      */
 /* information. Display all of the information for each CMDERR group.*/
 /* Repeat until all of the CMDERR groups have processed.             */
 /*********************************************************************/
If wideload.cmderr.0 /= '' Then
  Do
    Say '    "ims_member_messages": ['
    Do ix = 1 To wideload.cmderr.0
      Say '     {'
      Say '        "Mbr name' ix '": ' '"'wideload.cmderr.ix.name'",'
      Say '        "type": "'wideload.cmderr.ix.typ'",'
      Say '        "styp": "'wideload.cmderr.ix.styp'",'
      Say '        "cmderr rc": "'wideload.cmderr.ix.rc'",'
      Say '        "rsn": "'wideload.cmderr.ix.rsn'"'
      Say '     }'
      If ix < wideload.cmderr.0 Then
        Say '     ,'
     End
    Say '    ],'
   End

 /********************************************************************/
 /* Within the cmdsecrr group there are subgroups of information that*/
 /* may or may not be present. If the information is present,        */
 /* display it. The Saf subgroup, may contain another set of         */
 /* information within this subgroup. Display all of the information */
 /* contained within the Saf subgroup.                               */
 /********************************************************************/
If '' ^= wideload.cmdsecerr.Exit.rc Then
  Do
Say '     "cmdsecerr rc": "'wideload.cmdsecerr.Exit.rc'",'
Say '     "cmdsecerr userdata": "'wideload.cmdsecerr.Exit.userdata'",'
Say '     "cmdsecerr saf rc": "'wideload.cmdsecerr.saf.rc'",'
Say '     "cmdsecerr saf racfrc": "'wideload.cmdsecerr.saf.racfrc'",'
Say '     "cmdsecerr saf racfrsn": "'wideload.cmdsecerr.saf.racfrsn'",'
  End

Say '     "ctl.rc": "'wideload.ctl.rc'",'
Say '     "ctl.rsn": "'wideload.ctl.rsn'"'

/* Say '     "cmd_master": "'wideload.cmd.master'"'  */
/* Say '     "cmd_userid": "'wideload.cmd.userid'"' */
/* Say '     "cmd_verb": "'wideload.cmd.verb'",' */
/* Say '     "cmd_kwd": "'wideload.cmd.kwd'",'  */
/* Say '     "cmd_input": "'wideload.cmd.input'"'   */
Say '   }'

 /*******************************************************************/
 /* There may be multiple RSP groups. Within each of these groups   */
 /* there is a row (x) which contains information from the HDR      */
 /* HDR (y) slbl processing. The slbl is the header tag for the RSP */
 /* information that will be displayed.                             */
 /* Repeat processing until all RSP groups have been processed.     */
 /*******************************************************************/
If wideload.rsp.0 /= '' Then
  Do
    say '    , '
    recl = 700
    Do y = 1 To wideload.hdr.0
      wideload.hdr.y.printed = 0
    End

    printed_all = 0
    pass = 0
    Say '  "type_2_response": ['
    Do While (^ printed_all)
      print_cnt  = 0
      pass = pass + 1
      Do y = 1 To wideload.hdr.0
        print_cnt = print_cnt + wideload.hdr.y.printed
      End
      If print_cnt = wideload.hdr.0 Then
        Printed_all = 1
      Else
        Do x = 1 To wideload.rsp.0
          If wideload.rsp.x.0 /= '' Then
            Do
              row = ''
              If pass > 1 Then
               row = '  '
               Say '   {'
               rowcnt = 0
               Do y = 1 To wideload.rsp.x.0
                  Do
                    rowcnt = rowcnt + 1
                    wideload.hdr.y.printed = 1
                    data = '     "'||wideload.hdr.y.llbl||'": "'||wideload.rsp.x.y||'"'
                    If rowcnt < wideload.hdr.0 Then
                      data = data||','
                    Say data
                   End
                End
               Say '   }'
               If x < wideload.rsp.0 Then
                 Say '   ,'
             End
         End
     End
    Say "  ]"
   End




 /*******************************************************************/
 /* Within the MSGDATA group there are MBR subgroups. Within the MBR*/
 /* subgroups there may be one or more MSGs. Porcess each MBR       */
 /* subgroup and the MSGs within these subgroups. If information is */
 /* present, display that information.                              */
 /* Repeat processing until all MSGDATA and subgroups have processed*/
 /*******************************************************************/
If wideload.msgdata.name.0 /= '' Then
  Do
    Say '    , '
    Say '  "type_1_response": ['
    Do y = 1 To wideload.msgdata.name.0
      Say '   {'
      If wideload.msgdata.name.0 /= '' Then
        Do
          If wideload.msgdata.msg.y.0 /= '' Then
            Do
              Say '     "Mbr name' y'":' '"'wideload.msgdata.name.y'",'
              Say '     "msg data":  ['
              Do x = 1 To wideload.msgdata.msg.y.0
                wideload.msgdata.msg.y.x = TRANSLATE(wideload.msgdata.msg.y.x, "'", '"') /* replace double quote with single */
                data = wideload.msgdata.msg.y.x
                msgline = '           "'wideload.msgdata.msg.y.x'"'
                If x < wideload.msgdata.msg.y.0 Then
                  msgline=msgline||','
                Say msgline
              End
              Say '      ]'
            End
          Else
            Do
              Say '     "Mbr name' y'":' '"'wideload.msgdata.name.y'"'
            End
        End
        Say '   }'
        If y < wideload.msgdata.name.0 Then
          Say '   ,'
    End
    Say '  ]'
  End
Say "}"
Drop wideload.
return 0
"""
