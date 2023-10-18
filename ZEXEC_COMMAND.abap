REPORT ZEXEC_COMMAND LINE-SIZE 255 NO STANDARD PAGE HEADING.

TABLES: SXPGCOTABE, SXPGCOSTAB.

data: TCODE LIKE TSTC-TCODE,
      USER(6),
      CMD(254), "Max length = 254 Bytes
      CMD_NR TYPE I,
      CMD_LINE TYPE I,
      RESULT(255) OCCURS 100 WITH HEADER LINE, "No max length
      BEGIN OF LIST OCCURS 100,
        NR TYPE I,
        LINE(255),
      END OF LIST,
      rdisp_call_system,
      HOME(60),
      opsysgroup like opsystem-filesys,
      CD_PATH(60),
      lCD_PATH(60).

data: begin of par_sub occurs 0,
       STATUS TYPE I,
       PNAME(60),
       USER_WERT(60),
       DEFAULT_WERT(60),
      end of par_sub.

*---------------------------------------------------------------------*
* Start of selection
*---------------------------------------------------------------------*
START-OF-SELECTION.
* user check
*  IF  sy-uname <> 'KIMEYER'.
*  AND sy-uname <> 'JHEIDER'.
*    MESSAGE ID 'LXE_TRANS' TYPE 'A' NUMBER '007'.
*  ENDIF.

* Authority check
  DATA AUTHRC TYPE I.

* Are OS Commands allowed?
  CALL 'C_SAPGPARAM' ID 'NAME'  FIELD 'rdisp/call_system' "#ec notext
                     ID 'VALUE' FIELD rdisp_call_system.

* Host Userid
  USER = SY-SYSID.
  USER+3 = 'adm'. "#ec notext
  TRANSLATE USER TO LOWER CASE.

* Read profile parameters
  CALL 'C_SAPGALLPARAM' ID 'PAR_SUB'  FIELD PAR_SUB-*SYS*.
  SORT PAR_SUB BY PNAME.
  READ TABLE PAR_SUB WITH KEY PNAME = 'DIR_HOME' BINARY SEARCH.
  IF SY-SUBRC = 0.
    IF PAR_SUB-USER_WERT IS INITIAL.
      HOME = PAR_SUB-DEFAULT_WERT.
    ELSE.
      HOME = PAR_SUB-USER_WERT.
    ENDIF.
  ENDIF.

* Operating system group
  clear opsysgroup.
  select single filesys from opsystem into opsysgroup
    where opsys = sy-opsys.

* Write list
  PERFORM WRITE_LIST.

*---------------------------------------------------------------------*
* Heading lines
*---------------------------------------------------------------------*
TOP-OF-PAGE.
  PERFORM TOP_OF_PAGE.
TOP-OF-PAGE DURING LINE-SELECTION.
  PERFORM TOP_OF_PAGE.
*
FORM TOP_OF_PAGE.
  FORMAT RESET.
  WRITE: / 'R/3 ',          SY-SYSID(3) INTENSIFIED,
                            SY-MANDT INTENSIFIED,
        (5) space,
           'Benutzer'(005), SY-UNAME INTENSIFIED,
           'Datum'(002),    SY-DATUM DD/MM/YYYY INTENSIFIED,
           'Zeit'(003),
                        (8) SY-UZEIT USING EDIT MASK '__:__:__'
                                     INTENSIFIED.
  WRITE: / 'Host'(004),     SY-HOST INTENSIFIED,
           'Benutzer'(005), USER INTENSIFIED.
  if cd_path is initial.
    WRITE: / 'Pfad'(006),     HOME INTENSIFIED.
  else.
    WRITE: / 'Pfad'(006),     cd_path INTENSIFIED.
  endif.
  if rdisp_call_system = '0'.
    write: / 'Keine OS-Kommandos möglich (rdisp/call_system=0)'(001)
             color col_negative.
  endif.
  ULINE.
  WRITE: /
      '!nr..   Execute command nr from history with trailing ..'(007).
  WRITE: /
      '!!..    Execute last command from history with trailing ..'(008).
  WRITE: /  '$(name) Wird aufgelöst gegen logische '
           &'OS-Kommandos und Profileparameter'(009).

  ULINE.
ENDFORM.

*---------------------------------------------------------------------*
* Write list of commands and results
*---------------------------------------------------------------------*
FORM WRITE_LIST.
* Change working directory supported?
  case opsysgroup.
    when 'AS/400'.
      SET PF-STATUS 'MAIN' excluding 'CD'.
    when 'UNIX' or 'WINDOWS NT'.
      SET PF-STATUS 'MAIN'.
    when others.
      SET PF-STATUS 'MAIN' excluding 'CD'.
  endcase.
  FORMAT RESET.
  SET BLANK LINES ON.
* History of commands and result
  LOOP AT LIST.
    IF LIST-NR = 0.
      WRITE / LIST-LINE. HIDE LIST-NR.
    ELSE.
      CLEAR CMD.
      CMD(6) = ABS( LIST-NR ).
      CONDENSE CMD NO-GAPS.
      CONCATENATE '[' CMD(6) ']' LIST-LINE INTO CMD.
      WRITE / CMD INTENSIFIED ON. HIDE LIST-NR.
    ENDIF.
  ENDLOOP.

* Input field for new command
  CLEAR CMD.
  WRITE / CMD INPUT ON.
  CMD_LINE = SY-LINNO.

* Scroll down to command field
  DATA LINE TYPE I.
  LINE = SY-LINNO - SY-SROWS + 3.
  SCROLL LIST INDEX 1 TO LAST PAGE LINE LINE.
  SET CURSOR FIELD 'CMD' LINE SY-LINNO.
  CLEAR SY-LSIND.
ENDFORM.

*---------------------------------------------------------------------*
* AT USER-COMMAND
*---------------------------------------------------------------------*
at user-command.
  case sy-ucomm.
    when 'EXEC'. perform exec.
    when 'DELE'. perform dele.
    when 'CD'.   perform cd.
    when 'AL11' "SAP-Directories
      or 'SM51' "SAP-Server
      or 'SM36' "Job-Definition
      or 'SM37' "Job-Übersicht
      or 'SM49' "Ext. OS-Kommandos Ausführen
      or 'SM69'. "Ext. OS-Kommandos Verwalten
      TCODE = sy-ucomm.
      call transaction TCODE.
    when 'RSPFPAR'. "Profileparameter
      SUBMIT RSPFPAR via selection-screen and return.
  endcase.

at pf7.
  perform cd.

at pf8.
  perform exec.

at pf9.
  perform dele.

*---------------------------------------------------------------------*
* Execute command
*---------------------------------------------------------------------*
form exec.
  DATA: OFFSET TYPE I,
        LENGTH TYPE I.
  FIELD-SYMBOLS: <F>.

* Read input field
  CLEAR CMD.
  READ LINE CMD_LINE FIELD VALUE CMD.
  CHECK SY-SUBRC = 0 AND CMD NE SPACE.

  DATA ORIG_CMD LIKE CMD.
  ORIG_CMD = CMD.

* Get command from history
  IF CMD(1) = '!'.
    IF CMD(2) = '!!'.
      LIST-NR = CMD_NR.
      OFFSET = 2.
    ELSE.
      IF CMD+1 CO '0123456789'. ENDIF.
      LENGTH = SY-FDPOS.
      OFFSET = LENGTH + 1.
      CHECK LENGTH > 0.
      LIST-NR = CMD+1(LENGTH).
    ENDIF.
    CHECK LIST-NR > 0.
    READ TABLE LIST WITH KEY NR = LIST-NR.
    CHECK SY-SUBRC = 0.
    ASSIGN CMD+OFFSET(*) TO <F>.
    CONCATENATE LIST-LINE <F> INTO CMD.
  ENDIF.

* Resolve against logical commands and profile parameters
  DATA: VARNAME(60),
        VARVALUE(60).
  CLEAR OFFSET.
  DO.
    ASSIGN CMD+OFFSET(*) TO <F>.
    IF NOT <F> CS '$('. EXIT. ENDIF.
    OFFSET = OFFSET + SY-FDPOS + 2.
    ASSIGN CMD+OFFSET(*) TO <F>.
    IF NOT <F> CS ')'. EXIT. ENDIF.
    LENGTH = SY-FDPOS.
    IF LENGTH = 0. EXIT. ENDIF.
    ASSIGN CMD+OFFSET(LENGTH) TO <F>.

*  Try specific locical OS command (Customer)
    SELECT SINGLE * FROM SXPGCOSTAB
      WHERE NAME = <F>
        AND OPSYSTEM = SY-OPSYS.
    IF SY-SUBRC NE 0.
*     Try general locical OS command (Customer)
      SELECT SINGLE * FROM SXPGCOSTAB
        WHERE NAME = <F>
          AND OPSYSTEM = 'ANYOS'.
    ENDIF.
    IF SY-SUBRC = 0.
      CONCATENATE '$(' <F> ')' INTO VARNAME.
      CONCATENATE SXPGCOSTAB-OPCOMMAND
                  SXPGCOSTAB-PARAMETERS
        INTO VARVALUE SEPARATED BY SPACE.
    ELSE.
*     Try specific locical OS command (SAP)
      SELECT SINGLE * FROM SXPGCOTABE
        WHERE NAME = <F>
          AND OPSYSTEM = SY-OPSYS.
      IF SY-SUBRC NE 0.
*       Try general locical OS command (SAP)
        SELECT SINGLE * FROM SXPGCOTABE
          WHERE NAME = <F>
            AND OPSYSTEM = 'ANYOS'.
      ENDIF.
      IF SY-SUBRC = 0.
        CONCATENATE '$(' <F> ')' INTO VARNAME.
        CONCATENATE SXPGCOTABE-OPCOMMAND
                    SXPGCOTABE-PARAMETERS
          INTO VARVALUE SEPARATED BY SPACE.
      ELSE.
*       Try profile parameter
        PAR_SUB-PNAME = <F>.
        READ TABLE PAR_SUB WITH KEY PNAME = PAR_SUB-PNAME BINARY SEARCH.
        IF SY-SUBRC = 0.
          CONCATENATE '$(' <F> ')' INTO VARNAME.
          IF PAR_SUB-USER_WERT IS INITIAL.
            VARVALUE = PAR_SUB-DEFAULT_WERT.
          ELSE.
            VARVALUE = PAR_SUB-USER_WERT.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.
*   Resolve variable
    IF VARNAME NE SPACE.
      LENGTH = STRLEN( VARVALUE ).
      IF LENGTH = 0. LENGTH = 1. ENDIF.
      ASSIGN VARVALUE(LENGTH) TO <F>.
      LENGTH = STRLEN( VARNAME ).
      REPLACE VARNAME LENGTH LENGTH
              WITH <F> INTO CMD.
    ENDIF.
  ENDDO.

* Store substituted command
  ADD 1 TO CMD_NR.
  IF ORIG_CMD NE CMD.
    LIST-NR = 0 - CMD_NR.
    LIST-LINE = ORIG_CMD.
    APPEND LIST.
  ENDIF.
  LIST-NR = CMD_NR.
  LIST-LINE = CMD.
  APPEND LIST.

* Change working directory
  if not cd_path is initial.
    case opsysgroup.
      when 'AS/400'.
      when 'UNIX'.
        concatenate 'cd' cd_path ';' cmd into cmd
          separated by space.
      when 'WINDOWS NT'.
        concatenate 'cd' cd_path '&' cmd into cmd
          separated by space.
      when others.
    endcase.
  endif.

* Execute command
  REFRESH RESULT.
  CALL 'SYSTEM' ID 'COMMAND' FIELD CMD
                ID 'TAB'     FIELD RESULT-*SYS*.

* Store result
  if sy-subrc ne 0.
    write  sy-subrc to LIST-LINE(4).
    concatenate 'Returncode'(010) '=' LIST-LINE into LIST-LINE.
    append list.
  endif.
  CLEAR LIST.
  LOOP AT RESULT.
    if opsysgroup = 'WINDOWS NT'.
      data: x0d type x value '0D'.
      offset = strlen( result ) - 1.
      if offset >= 0.
        assign result+offset(1) to <f> type 'X'.
        if <f> = x0d.
          clear result+offset(1).
        endif.
      endif.
    endif.
    LIST-LINE = RESULT.
    APPEND LIST.
  ENDLOOP.

* Write list
  PERFORM WRITE_LIST.
endform.

*---------------------------------------------------------------------*
* Clear result
*---------------------------------------------------------------------*
form dele.
  DELETE LIST WHERE NR = 0.
  PERFORM WRITE_LIST.
endform.

*---------------------------------------------------------------------*
* Change working directory
*---------------------------------------------------------------------*
form cd.
  data: old_cd_path like cd_path.
  old_cd_path = cd_path.
  call screen 1 starting at 10 10.
  if cd_path ne old_cd_path.
    PERFORM WRITE_LIST.
  endif.
endform.

*&---------------------------------------------------------------------*
*&      Module  STATUS_0001  OUTPUT
*&---------------------------------------------------------------------*
MODULE STATUS_0001 OUTPUT.
  SET PF-STATUS 'CD'.
  SET TITLEBAR 'CD'.
  lcd_path = cd_path.
ENDMODULE.                 " STATUS_0001  OUTPUT

*&---------------------------------------------------------------------*
*&      Module  EXIT_0001  INPUT
*&---------------------------------------------------------------------*
MODULE EXIT_0001 INPUT.
  leave to screen 0.
ENDMODULE.                 " EXIT_0001  INPUT

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0001  INPUT
*&---------------------------------------------------------------------*
data: text like cmd.
*
MODULE USER_COMMAND_0001 INPUT.
* Try cd command
  clear cmd.
  case opsysgroup.
    when 'AS/400'.
    when 'UNIX'.
      concatenate 'cd' lcd_path into cmd separated by space.
    when 'WINDOWS NT'.
      concatenate 'cd' lcd_path into cmd separated by space.
    when others.
  endcase.
  if cmd is initial.
    leave to screen 0.
    exit.
  endif.
  REFRESH RESULT.
  CALL 'SYSTEM' ID 'COMMAND' FIELD CMD
                ID 'TAB'     FIELD RESULT-*SYS*.
  if sy-subrc ne 0.
    message e368(00) with
      'Verzeichnis existiert nicht'(011) lcd_path.
  endif.
* Ok
  cd_path = lcd_path.

  leave to screen 0.
ENDMODULE.                 " USER_COMMAND_0001  INPUT
