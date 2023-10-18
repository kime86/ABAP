*&---------------------------------------------------------------------*
*& Report  ZRSPC_MON
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT ZRSPC_MON.
*"=============================================================================
*" TYPE-POOLS
*"=============================================================================
TYPE-POOLS: list.
*"=============================================================================
*" TYPES
*"=============================================================================
TYPES: BEGIN OF ty_chain_TEXT,
          CHAIN_ID TYPE RSPC_CHAIN,
          TXTLG    TYPE RSTXTLG,
       END OF ty_chain_TEXT,
       BEGIN OF ty_chain_log,
          CHAIN_ID TYPE RSPC_CHAIN,
          LOG_ID TYPE RSPC_LOGID,
          DATUM TYPE SYDATUM,
          ZEIT TYPE SYUZEIT,
          ANALYZED_STATUS TYPE RSPC_STATE,
          REPORTED_STATUS TYPE RSPC_STATE,
          SYNCHRONOUS TYPE RSPC_SYNCHRONOUS,
          SYNC_DEBUG TYPE RSPC_SYNCHRONOUS,
          WARN TYPE BOOLEAN,
       END OF ty_chain_log,
       BEGIN OF ty_CHAIN_SEL,
          CHAIN_ID TYPE RSPC_CHAIN,
          RAN TYPE BOOLEAN,
       END OF ty_chain_sel,
       BEGIN OF ty_stat_sel,
         RSPC_STATE TYPE RSPC_STATE,
       END OF ty_stat_sel,
       BEGIN OF ty_report,
         CHAIN_ID TYPE RSPC_CHAIN,
         LOG_ID TYPE RSPC_LOGID,
         ICON TYPE ICON_D,
         STATE TYPE RSPC_STATE,
         NORUN TYPE BOOLEAN,
       END OF ty_report.
*"=============================================================================
*" CONSTANTS
*"=============================================================================
CONSTANTS: lc_replyto TYPE AD_SMTPADR VALUE 'BatchMan@sportscheck.com'.
*"=============================================================================
*" DATA
*"=============================================================================
DATA: ls_log TYPE RSPCLOGCHAIN,
      lv_html TYPE STRING,
      lt_rspcinfo TYPE HASHED TABLE OF TY_CHAIN_TEXT WITH UNIQUE KEY CHAIN_ID,
      ls_rspcinfo TYPE ty_chain_text,
      lt_selrspc TYPE HASHED TABLE OF TY_CHAIN_SEL WITH UNIQUE KEY CHAIN_ID,
      lt_logchain TYPE HASHED TABLE OF ty_chain_log WITH UNIQUE KEY CHAIN_ID LOG_ID DATUM ZEIT,
      lt_warnstat TYPE HASHED TABLE OF TY_STAT_SEL WITH UNIQUE KEY RSPC_STATE,
      lt_report TYPE HASHED TABLE OF TY_report WITH UNIQUE KEY CHAIN_ID LOG_ID,
      ls_report TYPE ty_report,
      lt_statetxt TYPE HASHED TABLE OF DD07T WITH UNIQUE KEY DOMVALUE_L,
      lv_htmlreport TYPE STRING,
      lv_udatum LIKE sy-datum,
      lv_uzeit LIKE sy-UZEIT,
      lr_mime          TYPE REF TO cl_gbt_multirelated_service,
      lr_document TYPE REF TO CL_DOCUMENT_BCS,
      lr_request       TYPE REF TO cl_bcs,
      lr_exception TYPE REF TO cx_bcs,
      lv_style TYPE STRING,
      lv_jobnm TYPE TBTCM-JOBNAME,
      lv_statustxt TYPE STRING,
      lv_subject TYPE STRING,
      lv_sub TYPE SO_OBJ_DES,
      lv_strtmp2 TYPE STRING,
      lv_chaintext TYPE RSTXTLG,
      lv_statetext TYPE STRING,
      ls_statetxt TYPE DD07T,
      ls_chaintext TYPE TY_CHAIN_TEXT,
      lv_iconhtml TYPE STRING,
      lv_class TYPE STRING,
      lt_sortrep TYPE STANDARD TABLE OF TY_REPORT,
      lt_reportlog TYPE HASHED TABLE OF ZRSPC_MON WITH UNIQUE KEY CHAIN_ID LOG_ID,
      ls_reportlog TYPE ZRSPC_MON,
      lr_rspc_log TYPE REF TO CL_RSPC_LOG,
      lt_t246 TYPE HASHED TABLE OF T246 WITH UNIQUE KEY SPRSL WOTNR,
      lv_dayno TYPE scal-INDICATOR,
      lv_strtmp TYPE STRING.
*      lv_jobnm TYPE BTCJOB,
*      lv_style TYPE STRING
      .
*"=============================================================================
*" Field Symbols
*"=============================================================================
FIELD-SYMBOLS: <lfs_rspcinfo> TYPE TY_CHAIN_TEXT,
               <lfs_selrspc> TYPE Ty_chain_sel,
               <lfs_logchain> TYPE ty_chain_log,
               <lfs_report> TYPE ty_report,
               <lfs_reportlog> TYPE ZRSPC_MON,
               <lfs_scday> TYPE BOOLEAN,
               <lfs_t246> TYPE T246
               .
*"=============================================================================
*" SELECT-OPTIONS:
*"    so_proc   = Process Name
*"=============================================================================
SELECTION-SCREEN BEGIN OF BLOCK blk1 WITH FRAME TITLE t1.

  SELECTION-SCREEN BEGIN OF LINE.
    SELECTION-SCREEN COMMENT (17) l_chain FOR FIELD so_chain.
    SELECT-OPTIONS  so_chain   FOR ls_log-CHAIN_ID NO INTERVALS.
  SELECTION-SCREEN END OF LINE.
  SELECTION-SCREEN BEGIN OF LINE.
    SELECTION-SCREEN COMMENT (17) l_estat FOR FIELD so_stat.
    SELECT-OPTIONS so_stat for ls_log-ANALYZED_STATUS NO INTERVALS DEFAULT 'R'.
  SELECTION-SCREEN END OF LINE.
  SELECTION-SCREEN BEGIN OF LINE.
    PARAMETER p_norun TYPE BOOLEAN AS CHECKBOX DEFAULT ABAP_FALSE.
    SELECTION-SCREEN COMMENT 3(20) l_norun FOR FIELD p_norun.
  SELECTION-SCREEN END OF LINE.

  SELECTION-SCREEN skip.
  SELECTION-SCREEN BEGIN OF LINE.
    PARAMETER p_wrep TYPE BOOLEAN AS CHECKBOX DEFAULT ABAP_FALSE.
    SELECTION-SCREEN COMMENT 3(20) l_wrep FOR FIELD p_wrep.
  SELECTION-SCREEN END OF LINE.
  SELECTION-SCREEN BEGIN OF LINE.
    PARAMETER p_chkml TYPE BOOLEAN AS CHECKBOX.
    SELECTION-SCREEN COMMENT 3(20) l_chkml FOR FIELD p_chkml.
  SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN END OF BLOCK blk1.

SELECTION-SCREEN BEGIN OF BLOCK blk2 WITH FRAME TITLE t2.
  SELECTION-SCREEN BEGIN OF LINE.
    SELECTION-SCREEN COMMENT (20) l_subj FOR FIELD p_subj.
    PARAMETER p_subj TYPE RSTXTLG DEFAULT '<SY-SYSID> : Processchain Monitor'.
  SELECTION-SCREEN END OF LINE.

  SELECTION-SCREEN skip.
  SELECTION-SCREEN BEGIN OF LINE.
   SELECTION-SCREEN COMMENT (20) l_disl FOR FIELD p_disl.
   PARAMETER p_disl TYPE SOOD-OBJNAM.
  SELECTION-SCREEN END OF LINE.
  SELECTION-SCREEN BEGIN OF LINE.
    SELECTION-SCREEN COMMENT (20) l_mail FOR FIELD p_mail.
    PARAMETER p_mail TYPE AD_SMTPADR.
  SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN END OF BLOCK blk2.

SELECTION-SCREEN BEGIN OF BLOCK blk3 WITH FRAME TITLE t3.
  SELECTION-SCREEN BEGIN OF LINE.
    PARAMETER p_wd1 TYPE BOOLEAN AS CHECKBOX DEFAULT ABAP_TRUE.
    SELECTION-SCREEN COMMENT 3(10) l_wd1 FOR FIELD p_wd1.

    SELECTION-SCREEN POSITION 15.
    PARAMETER p_wd2 TYPE BOOLEAN AS CHECKBOX DEFAULT ABAP_TRUE.
    SELECTION-SCREEN COMMENT 18(10) l_wd2 FOR FIELD p_wd2.

    SELECTION-SCREEN POSITION 30.
    PARAMETER p_wd3 TYPE BOOLEAN AS CHECKBOX DEFAULT ABAP_TRUE.
    SELECTION-SCREEN COMMENT 33(10) l_wd3 FOR FIELD p_wd3.

    SELECTION-SCREEN POSITION 45.
    PARAMETER p_wd4 TYPE BOOLEAN AS CHECKBOX DEFAULT ABAP_TRUE.
    SELECTION-SCREEN COMMENT 48(10) l_wd4 FOR FIELD p_wd4.

    SELECTION-SCREEN POSITION 60.
    PARAMETER p_wd5 TYPE BOOLEAN AS CHECKBOX DEFAULT ABAP_TRUE.
    SELECTION-SCREEN COMMENT 63(10) l_wd5 FOR FIELD p_wd5.
  SELECTION-SCREEN END OF LINE.

  SELECTION-SCREEN BEGIN OF LINE.
    PARAMETER p_wd6 TYPE BOOLEAN AS CHECKBOX DEFAULT ABAP_TRUE.
    SELECTION-SCREEN COMMENT 3(10) l_wd6 FOR FIELD p_wd6.

    SELECTION-SCREEN POSITION 15.
    PARAMETER p_wd7 TYPE BOOLEAN AS CHECKBOX DEFAULT ABAP_TRUE.
    SELECTION-SCREEN COMMENT 18(10) l_wd7 FOR FIELD p_wd7.
  SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN END OF BLOCK blk3.
*"=============================================================================
*" INITIALIZATION
*"=============================================================================
INITIALIZATION.
  l_chain    = 'Processchain'.
  l_chkml    = 'Send Mailreport'.
  l_estat    = 'Warn on Status'.
  l_disl     = 'Distribution List'.
  l_norun    = 'Warn if not ran'.
  l_mail     = 'E-Mail address'.
  l_wrep     = 'Report repeatedly'.
  l_subj     = 'Subject'.
  l_wd1      = 'Monday'.
  l_wd2      = 'Tuesday'.
  l_wd3      = 'Wednesday'.
  l_wd4      = 'Thursday'.
  l_wd5      = 'Friday'.
  l_wd6      = 'Saturday'.
  l_wd7      = 'Sunday'.
  t1         = 'General parameter'.
  t2         = 'Mail report parameter'.
  t3         = 'Start conditions'.
*"=============================================================================
*" AT SELECTION-SCREEN
*"=============================================================================
AT SELECTION-SCREEN.

*"=============================================================================
*" Value Help
*"=============================================================================
AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_disl.
  DATA: begin of lt_objnam occurs 0,
          objnam like sood-objnam,
          objdes LIKE sood-objdes,
        end of lt_objnam.
  DATA: lt_ret like ddshretval occurs 0 with HEADER LINE,
        lt_dyn like dynpread occurs 0 with HEADER LINE.

  SELECT objnam objdes FROM sood
    into table lt_objnam WHERE objtp = 'DLI'.

  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
        RETFIELD = 'OBJNAM'
        DYNPNR   = sy-DYNNR
        DYNPPROG = sy-repid
        DYNPROFIELD = 'p_disl'
        VALUE_ORG = 'S'
    TABLES
      VALUE_TAB = LT_OBJNAM
      RETURN_TAB = LT_RET
    EXCEPTIONS
      parameter_error = 1
      no_values_found = 2
      others = 3.

*"=============================================================================
*" START-OF-SELECTION
*"=============================================================================
START-OF-SELECTION.

*" ============================================================================
*"  Check if start-parameters are met
*" ============================================================================
SELECT * FROM T246
  INTO CORRESPONDING FIELDS OF TABLE LT_T246
  WHERE sprsl = 'EN'.

CALL FUNCTION 'DATE_COMPUTE_DAY'
  EXPORTING
    DATE          = sy-datum
 IMPORTING
   DAY           = lv_dayno.
          .

CONCATENATE 'P_WD' lv_dayno INTO LV_STRTMP.
ASSIGN (lv_strtmp) TO <lfs_scday>.
IF <lfs_scday> IS NOT ASSIGNED OR <LFS_SCDAY> EQ ABAP_FALSE.
  CLEAR lv_strtmp.

  READ TABLE lt_t246 ASSIGNING <lfs_t246> WITH TABLE KEY SPRSL = 'EN' WOTNR = lv_dayno.
  IF sy-subrc EQ 0.
    CONCATENATE ' on ' <lfs_t246>-LANGT INTO lv_strtmp RESPECTING BLANKS.
  ENDIF.

  CONCATENATE 'Program ' sy-repid INTO LV_STRTMP2 RESPECTING BLANKS.
  CONDENSE LV_STRTMP2.
  IF sy-slset IS NOT INITIAL.
    CONCATENATE lv_strtmp2 ' variant ' sy-slset INTO lv_strtmp2 RESPECTING BLANKS.
    CONDENSE lv_strtmp2.
  ENDIF.

  CONCATENATE lv_strtmp2 ' is not intended to run' lv_strtmp INTO lv_strtmp.
  WRITE: lv_strtmp.
  EXIT.
ENDIF.

CLEAR: LV_STRTMP, LV_STRTMP2.
*" ============================================================================
*"  Start programm
*" ============================================================================
*" 0) Delte old entries from persistance
DELETE FROM ZRSPC_MON WHERE DATUM < sy-datum.

*" 0.1) Read persisted warnings
SELECT *
  FROM ZRSPC_MON
  INTO CORRESPONDING FIELDS OF TABLE LT_REPORTLOG
  WHERE DATUM = sy-datum.

*" 1) Read Tables
*" Process chain text
SELECT DISTINCT CHAIN_ID TXTLG
  FROM RSPCCHAINT
  INTO CORRESPONDING FIELDS OF TABLE LT_RSPCINFO
  WHERE langu = sy-langu
  AND OBJVERS = 'A'.

*" Chains to chek (to fetch not run chains)
SELECT DISTINCT CHAIN_ID
  FROM RSPCCHAINATTR
  INTO CORRESPONDING FIELDS OF TABLE LT_SELRSPC
  WHERE CHAIN_ID IN SO_CHAIN.


*" Chain logs from today
SELECT *
  FROM RSPCLOGCHAIN
  INTO CORRESPONDING FIELDS OF TABLE LT_LOGCHAIN
  WHERE CHAIN_ID IN SO_CHAIN
  AND DATUM EQ SY-DATUM.
CALL FUNCTION 'RSSM_GET_TIME'
  EXPORTING
    i_datum_loc = sy-datum
    i_uzeit_loc = '000000'
  IMPORTING
    e_datum_utc = LV_UDATUM
    e_uzeit_utc = LV_UZEIT.
IF LV_UDATUM < sy-datum.
* .... Append for Eurasia ....
  SELECT * FROM rspclogchain
    APPENDING CORRESPONDING FIELDS OF TABLE LT_LOGCHAIN
    WHERE chain_id IN SO_CHAIN
      AND ( ( datum  = LV_UDATUM ) AND ( zeit >= LV_UZEIT ) ).
ELSE.
  CALL FUNCTION 'RSSM_GET_TIME'
    EXPORTING
      i_datum_loc = sy-datum
* .... Note:1685016 changing the value frm 232359 to 235959
      i_uzeit_loc = '235959'
    IMPORTING
      e_datum_utc = LV_UDATUM
      e_uzeit_utc = LV_UZEIT.
  IF LV_UDATUM > sy-datum.
* .... Append for Americas ....
    SELECT * FROM rspclogchain
      APPENDING CORRESPONDING FIELDS OF TABLE LT_LOGCHAIN
        WHERE chain_id IN SO_CHAIN
        AND ( ( datum  = LV_UDATUM ) AND ( zeit <= LV_UZEIT ) ).
  ENDIF.
ENDIF.

*" States to warn on
SELECT DISTINCT DOMVALUE_L
  FROM DD07L
  INTO TABLE LT_WARNSTAT
  WHERE DOMNAME = 'RSPC_STATE'
  AND AS4LOCAL = 'A'
  AND DOMVALUE_L IN SO_STAT.

SELECT *
  FROM DD07T
  INTO TABLE LT_STATETXT
  WHERE DOMNAME = 'RSPC_STATE'
  AND DDLANGUAGE = sy-langu.

*" 2) Run Checks
LOOP AT LT_LOGCHAIN ASSIGNING <LFS_LOGCHAIN>.

*" Force status update.
TRY.

  IF NOT ( <LFS_LOGCHAIN>-SYNCHRONOUS EQ 'X' AND <LFS_LOGCHAIN>-SYNC_DEBUG IS INITIAL ). " Prevent exclusive lock on synchronous chain-runs
    CREATE OBJECT LR_RSPC_LOG
      EXPORTING
        I_LOGID = <lfs_logchain>-LOG_ID
        I_CHAIN = <LFS_LOGCHAIN>-CHAIN_ID
*        I_TYPE  =
*        I_VARIANT =
        I_UPDATE_CCMS_TIMESTAMP = abap_true
*        I_DONT_POLL =
        I_DONT_UPDATE = abap_false
*        I_MIX_ALWAYS =
*        I_S_LOGATTR =
*        I_T_LOG =
      EXCEPTIONS
        NO_LOG  = 1
        ABORTED = 2
        others  = 3
        .

*    lr_rspc_log->REFRESH( I_UPDATE_CCMS_TIMESTAMP = 'X' ).
    lr_rspc_log->get_status( IMPORTING E_STATUS = <LFS_LOGCHAIN>-ANALYZED_STATUS ).
  ENDIF.
ENDTRY.

*" Update Run-Status
  READ TABLE LT_SELRSPC ASSIGNING <LFS_SELRSPC>
    WITH KEY CHAIN_ID = <LFS_LOGCHAIN>-CHAIN_ID.
  IF sy-subrc EQ 0.
    <lfs_selrspc>-ran = ABAP_TRUE.
  ENDIF.

  UNASSIGN <LFS_REPORTLOG>.
  READ TABLE LT_REPORTLOG ASSIGNING <LFS_REPORTLOG>
    WITH KEY CHAIN_ID = <LFS_LOGCHAIN>-CHAIN_ID
             LOG_ID = <lfs_logchain>-LOG_ID.

  READ TABLE LT_WARNSTAT TRANSPORTING NO FIELDS WITH KEY RSPC_STATE = <LFS_LOGCHAIN>-ANALYZED_STATUS.
  IF sy-subrc EQ 0.
    IF P_WREP = ABAP_TRUE.
      <lfs_logchain>-WARN = ABAP_TRUE.
    ELSE.
      IF <LFS_REPORTLOG> IS ASSIGNED AND <lfs_logchain>-ANALYZED_STATUS = <LFS_REPORTLOG>-STATE.
        <lfs_logchain>-WARN = ABAP_FALSE.
      ELSE.
        <lfs_logchain>-WARN = ABAP_TRUE.
      ENDIF.
    ENDIF.
  ENDIF.

  IF <LFS_REPORTLOG> IS ASSIGNED.
    <LFS_REPORTLOG>-STATE = <LFS_LOGCHAIN>-ANALYZED_STATUS.
  ENDIF.
ENDLOOP.

*" 3) Reporte Ketten die nicht gelaufen sind
IF P_NORUN EQ ABAP_TRUE.
  LOOP AT lt_selrspc ASSIGNING <LFS_SELRSPC> WHERE RAN = ABAP_FALSE.
    ls_report-CHAIN_ID = <LFS_SELRSPC>-chain_id.
    CLEAR ls_report-LOG_ID.
    ls_report-ICON = '@BZ@'.
    CLEAR ls_report-STATE.
    ls_report-NORUN = ABAP_TRUE.
    INSERT ls_report INTO TABLE LT_REPORT.
  ENDLOOP.
ENDIF.

*" 4) Report based on status
LOOP AT lt_logchain ASSIGNING <LFS_LOGCHAIN> WHERE WARN = ABAP_TRUE.
  ls_report-CHAIN_ID = <LFS_LOGCHAIN>-CHAIN_ID.
  ls_report-LOG_ID = <LFS_LOGCHAIN>-LOG_ID.
  ls_report-STATE = <lfs_logchain>-ANALYZED_STATUS.
  ls_report-NORUN = ABAP_FALSE.
  CASE <LFS_LOGCHAIN>-ANALYZED_STATUS.
    WHEN 'R' OR 'X' OR 'S' OR 'J' OR 'F'.
      ls_report-ICON = '@5C@'. "ICON_LED_RED
    WHEN 'G'.
      ls_report-ICON = '@5B@'. "ICON_LED_GREEN
    WHEN 'A'.
      ls_report-ICON = '@5D@'. "ICON_LED_YELLOW
    WHEN OTHERS.
      ls_report-ICON = '@BZ@'. "ICON_LED_INACTIVE
  ENDCASE.

  INSERT LS_REPORT INTO TABLE LT_REPORT.
ENDLOOP.

*"=============================================================================
*" CREATE REPORT
*"=============================================================================
LV_HTMLREPORT = '<div style="overflow-x:auto;"><table>'.

CALL FUNCTION 'GET_JOB_RUNTIME_INFO'
 IMPORTING
   JOBNAME                       = lv_jobnm
 EXCEPTIONS
   NO_RUNTIME_INFO               = 1
   OTHERS                        = 2
          .
IF SY-SUBRC EQ 0.
  CONCATENATE 'Job-Name: <code>'
              lv_jobnm
              '</code><br /><hr /><br />'
              LV_HTMLREPORT
  INTO LV_HTMLREPORT.
ENDIF.

lt_sortrep = lt_report.
SORT LT_SORTREP BY CHAIN_ID.

LOOP AT LT_SORTREP ASSIGNING <LFS_REPORT>.
  CLEAR: LS_STATETXT, lv_chaintext, lv_statetext.

  IF <LFS_REPORT>-NORUN EQ ABAP_FALSE.
    READ TABLE LT_STATETXT INTO LS_STATETXT  WITH KEY DOMVALUE_L = <lfs_report>-STATE.
    IF sy-subrc EQ 0.
      LV_STATETEXT = LS_STATETXT-DDTEXT.
    ENDIF.
  ELSE.
    LV_STATETEXT = 'No run'.
  ENDIF.

  READ TABLE LT_RSPCINFO INTO LS_CHAINTEXT WITH KEY CHAIN_ID = <LFS_REPORT>-CHAIN_ID.
  IF sy-subrc EQ 0.
    LV_CHAINTEXT = LS_CHAINTEXT-TXTLG.
  ENDIF.

  CASE <LFS_REPORT>-ICON.
    WHEN '@5C@'. LV_ICONHTML = 'cid:LED_RED'. LV_CLASS = 'red'.
    WHEN '@5B@'. LV_ICONHTML = 'cid:LED_GREEN'. LV_CLASS = 'gre'.
    WHEN '@5D@'. LV_ICONHTML = 'cid:LED_YELLOW'. LV_CLASS = 'yel'.
    WHEN '@BZ@'. LV_ICONHTML = 'cid:LED_UNKNOWN'. LV_CLASS = 'unk'.
  ENDCASE.

  WRITE: / <lfs_report>-ICON,
           <LFS_REPORT>-CHAIN_ID,
           <lfs_report>-LOG_ID,
           LV_STATETEXT
          .

  CONCATENATE
    LV_HTMLREPORT
    '<tr class="'
    lv_class
    '"><td><img src="'
    LV_ICONHTML
    '" /></td><td><code>'
    <lfs_report>-CHAIN_ID
    '</code></td><td><code>'
    <lfs_report>-LOG_ID
    '</code></td><td>'
    LV_CHAINTEXT
    '</td><td>'
    LV_STATETEXT
    '</td></tr>'
  INTO LV_HTMLREPORT.
ENDLOOP.

CONCATENATE
  LV_HTMLREPORT
  '<table></div>'
INTO
  LV_HTMLREPORT.

LOOP AT LT_REPORT INTO <LFS_REPORT>.
  LS_REPORTLOG-CHAIN_ID = <lfs_report>-CHAIN_ID.
  LS_REPORTLOG-LOG_ID = <lfs_report>-LOG_ID.
  LS_REPORTLOG-STATE = <lfs_report>-STATE.
  LS_REPORTLOG-DATUM = sy-datum.

  INSERT ls_reportlog INTO TABLE LT_REPORTLOG.
ENDLOOP.

MODIFY ZRSPC_MON FROM TABLE LT_REPORTLOG.
COMMIT WORK.

*"=============================================================================
*" CREATE MAIL
*"=============================================================================
CHECK LT_REPORT IS NOT INITIAL.
CHECK p_chkml = 'X'.

CONCATENATE '<style>'
'table { border-spacing: 0; } '
'th, td { padding: 3px; } '
'tr:hover td { border-bottom: 1px dashed #444444; '
'border-top: 1px dashed #444444; '
'background-color: #b3e6ff; } '
'tr.red { background-color: #ffcccc; } '
'tr.gre { background-color: #b3ffb3; } '
'tr.yel { background-color: #ffffcc; } '
'tr.unk { background-color: #f2f2f2; } '
'</style>'
INTO LV_STYLE.

IF P_DISL IS INITIAL AND P_MAIL IS INITIAL.
  WRITE: / 'No recipient has been selected - cancel'.
  EXIT.
ENDIF.

TRY.
  " 1) Create Request
  lr_request = cl_bcs=>CREATE_PERSISTENT( ).

  " 2) Set Sender
  LR_REQUEST->set_sender(
    i_sender = CL_sapuser_bcs=>CREATE( sy-uname )
*    I_sender = CL_cam_address_bcs=>CREATE_INTERNET_ADDRESS( 'dontreply@somewhere.com' )
  ).

  LR_REQUEST->SET_REPLY_TO(
    I_REPLY_TO = cl_cam_address_bcs=>CREATE_INTERNET_ADDRESS( LC_REPLYTO )
  ).

  " 3) Add recipient
  IF p_disl IS NOT INITIAL.
    lr_request->ADD_RECIPIENT(
      I_RECIPIENT = CL_DISTRIBUTIONLIST_BCS=>GETU_PERSISTENT(
        i_dliname = p_disl
        I_PRIVATE = space
      )
      i_express = 'X'
    ).
  ENDIF.

  IF p_mail IS NOT INITIAL.

    lr_request->ADD_RECIPIENT(
      I_RECIPIENT = CL_cam_address_bcs=>CREATE_INTERNET_ADDRESS( P_MAIL )
      i_express = 'X'
    ).
  ENDIF.

  " 5) Create MIME-Multipart Object
  CREATE OBJECT LR_MIME.

  " 5.1) Set HTML-Text
  CONCATENATE lv_style LV_HTMLREPORT INTO lv_html.
  lr_mime->SET_MAIN_HTML(
    CONTENT = CL_DOCUMENT_BCS=>STRING_TO_SOLI( ip_string = lv_html )
  ).

  " 5.2) Add Images
  PERFORM ADD_IMAGE USING '/SAP/PUBLIC/BC/Icons/s_s_ledg.gif' 'LED_GREEN' CHANGING lr_mime.
  PERFORM ADD_IMAGE USING '/SAP/PUBLIC/BC/Icons/s_s_ledr.gif' 'LED_RED' CHANGING lr_mime.
  PERFORM ADD_IMAGE USING '/SAP/PUBLIC/BC/Icons/s_s_ledy.gif' 'LED_YELLOW' CHANGING lr_mime.
  PERFORM ADD_IMAGE USING '/SAP/PUBLIC/BC/Icons/s_s_ledi.gif' 'LED_UNKNOWN' CHANGING lr_mime.

  " 6) Create Document
  lv_subject = P_SUBJ.
  REPLACE '<SY-SYSID>' IN LV_SUBJECT WITH sy-SYSID.
  LV_SUB = LV_SUBJECT.

  lr_document = cl_document_bcs=>CREATE_FROM_MULTIRELATED(
      I_SUBJECT = lv_sub
      I_MULTIREL_SERVICE = LR_MIME
  ).

  lr_request->SET_DOCUMENT(
    LR_DOCUMENT
  ).

  " 4) Set send immediately
  lr_request->SET_SEND_IMMEDIATELY( I_SEND_IMMEDIATELY = 'X' ).


  " 7) Send Mail
  LR_REQUEST->send( ).

  COMMIT WORK.
CATCH cx_bcs INTO lr_exception.
  LV_HTML = lr_exception->get_text( ).
  WRITE: / 'Senden fehlgeschlagen: ', LV_HTML.
  EXIT.
ENDTRY.

WRITE: / 'Mail-Report erfolgreich versendet'.

*" ============================================================================
*"  Adds Image from MIME-Repository-URL as Binary to MIME-Multipart
*" ============================================================================
FORM ADD_IMAGE  USING     I_URL TYPE STRING
                          I_CID TYPE STRING
                CHANGING  cr_mime TYPE REF TO cl_gbt_multirelated_service.

DATA: lr_mrep TYPE REF TO if_mr_api,
      lv_content TYPE XSTRING,
      lv_mimetype TYPE W3CONTTYPE,
      lv_filename TYPE STRING,
      lv_objlen TYPE SO_OBJ_LEN,
      lv_fnd TYPE i.

LR_MREP = cl_mime_repository_api=>if_mr_api~get_api( ).

CALL METHOD lr_mrep->get
  EXPORTING
    i_url = I_URL
  IMPORTING
    E_MIME_TYPE = lv_mimetype
    e_content = lv_content
  EXCEPTIONS
    parameter_missing = 1
    error_occured = 2
    not_found = 3
    permission_failure = 4
    others = 5.
    .

IF sy-subrc NE 0.
  WRITE: / 'MIME-Repository ''', I_URL, ''' not found...'.
  EXIT.
ENDIF.

LV_FILENAME = match( val = I_URL regex = '[^/|\\]*$' ).

LV_OBJLEN = xstrlen( lv_content ).
cr_mime->ADD_BINARY_PART(
  CONTENT = CL_DOCUMENT_BCS=>XSTRING_TO_SOLIX( IP_XSTRING = lv_content )
  filename = lv_filename
  CONTENT_TYPE = LV_MIMETYPE
  CONTENT_ID = i_CID
  LENGTH = LV_OBJLEN
).

ENDFORM.
