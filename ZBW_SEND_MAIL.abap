*&---------------------------------------------------------------------*
*& Report ZBW_SEND_MAIL
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT ZBW_SEND_MAIL.

TYPE-POOLS: swww, icon.
TABLES sscrfields.

*&---------------------------------------------------------------------*
*" SELECTION SCREEN
*&---------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK blk1 WITH FRAME TITLE t1.
  SELECTION-SCREEN BEGIN OF LINE.
    SELECTION-SCREEN COMMENT (20) l_subj FOR FIELD p_subj.
    PARAMETER p_subj TYPE RSTXTLG OBLIGATORY.
  SELECTION-SCREEN END OF LINE.

*  SELECTION-SCREEN BEGIN OF LINE.
*    SELECTION-SCREEN COMMENT (20) l_templ FOR FIELD s_templ.
*    PARAMETER s_templ TYPE swww_t_template_name OBLIGATORY.
*  SELECTION-SCREEN END OF LINE.

  SELECTION-SCREEN skip.

  SELECTION-SCREEN BEGIN OF LINE.
    PARAMETER p_stmt TYPE RSDU_T_abapsource NO-DISPLAY.
    SELECTION-SCREEN PUSHBUTTON 1(30) l_but1 USER-COMMAND cli1 VISIBLE LENGTH 30.
  SELECTION-SCREEN END OF LINE.

  SELECTION-SCREEN skip.

  SELECTION-SCREEN BEGIN OF LINE.
    SELECTION-SCREEN COMMENT (20) l_disl FOR FIELD p_disl.
    PARAMETER p_disl TYPE SOOD-OBJNAM OBLIGATORY.
  SELECTION-SCREEN END OF LINE.

  SELECTION-SCREEN BEGIN OF LINE.
    SELECTION-SCREEN COMMENT (20) l_sendr FOR FIELD p_sendr.
    PARAMETER p_sendr TYPE AD_SMTPADR OBLIGATORY DEFAULT 'foo@bar.com'.
  SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN END OF BLOCK blk1.
  SELECTION-SCREEN skip.

  SELECTION-SCREEN BEGIN OF LINE.
    PARAMETER p_dryrun TYPE c  AS CHECKBOX.
    SELECTION-SCREEN COMMENT (20) l_dryrun FOR FIELD p_dryrun.
  SELECTION-SCREEN END OF LINE.

*&---------------------------------------------------------------------*
*" INITIALIZATION
*&---------------------------------------------------------------------*
INITIALIZATION.
  t1    = 'E-Mail'.
  l_disl = 'Empfänger (SO23)'.
*  l_templ = 'Textvorlage (SMW0)'.
  l_subj = 'Betreff'.
  l_dryrun = 'Testmodus'.
  l_sendr = 'Absender'.
  l_but1 = ICON_CHANGE_TEXT && | Edit E-Mail content|.
*&---------------------------------------------------------------------*
*" VALUE HELP
*&---------------------------------------------------------------------*
*AT SELECTION-SCREEN ON VALUE-REQUEST FOR s_templ.
*TYPES: BEGIN OF tsy_wwwdata_f4,
*          OBJID TYPE W3OBJID,
*          TEXT TYPE W3_TEXT,
*       END OF tsy_wwwdata_f4,
*       tty_wwwdata_f4 TYPE TABLE OF tsy_wwwdata_f4.
*
*DATA: lt_values TYPE TABLE OF tsy_wwwdata_f4,
*      lt_return   TYPE STANDARD TABLE OF ddshretval.
*
*  SELECT *
*  INTO CORRESPONDING FIELDS OF TABLE lt_values
*  FROM WWWDATA
*  WHERE RELID = 'HT'.
*
*  IF sy-subrc NE 0.
*    RETURN.
*  ENDIF.
*
*  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
*    exporting
*      RETFIELD         = 'OBJID'    " Name of field in VALUE_TAB
*      VALUE_ORG        = 'S'        " Value return: C: cell by cell, S: structured
*    tables
*      VALUE_TAB        = lt_values  " Table of values: entries cell by cell
*      RETURN_TAB       = lt_return  " Return the selected value
*    exceptions
*      PARAMETER_ERROR  = 1          " Incorrect parameter
*      NO_VALUES_FOUND  = 2          " No values found
*      OTHERS           = 3
*    .
*
*  IF sy-subrc NE 0.
*    RETURN.
*  ENDIF.
*
*  read table lt_return INTO DATA(ls_return) INDEX 1.
*  S_TEMPL = ls_return-fieldval.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_disl.
  TYPES: BEGIN OF tsy_sood_f4,
            OBJNAM TYPE SO_OBJ_NAM,
            OBJDES TYPE SO_OBJ_DES,
          END OF tsy_sood_f4,
          tty_sood_f4 TYPE TABLE OF tsy_sood_f4.

  DATA: lt_f42 TYPE  tty_sood_f4,
        lt_return2 TYPE STANDARD TABLE OF ddshretval.

  SELECT *
  FROM SOOD
  INTO CORRESPONDING FIELDS OF TABLE lt_f42
  WHERE OBJTP = 'DLI'.

  IF sy-subrc NE 0.
    RETURN.
  ENDIF.

  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    exporting
      RETFIELD         = 'OBJNAM'    " Name of field in VALUE_TAB
      VALUE_ORG        = 'S'        " Value return: C: cell by cell, S: structured
    tables
      VALUE_TAB        = lt_f42  " Table of values: entries cell by cell
      RETURN_TAB       = lt_return2  " Return the selected value
    exceptions
      PARAMETER_ERROR  = 1          " Incorrect parameter
      NO_VALUES_FOUND  = 2          " No values found
      OTHERS           = 3
    .

  IF sy-subrc NE 0.
    RETURN.
  ENDIF.

  read table lt_return2 INTO DATA(ls_return) INDEX 1.
  P_DISL = ls_return-fieldval.
*&---------------------------------------------------------------------*
*" Command
*&---------------------------------------------------------------------*
AT SELECTION-SCREEN.
  CASE sscrfields.
    WHEN 'CLI1'.
      CL_RSDU_EDITOR=>edit(
        CHANGING C_T_CODE = P_STMT ).
  ENDCASE.
*&---------------------------------------------------------------------*
*" START-OF-SELECTION
*&---------------------------------------------------------------------*
START-OF-SELECTION.

DATA: lv_templ TYPE STRING,
      ls_stmt TYPE rsdu_abapsource.
DATA: lv_html TYPE STRING.

LOOP AT P_STMT INTO LS_STMT.
  lv_templ = lv_templ && || && ls_stmt-LINE.
ENDLOOP.

LV_HTML = ZCL_HTML_REPORT=>PARSE_TEMPLATE( LV_TEMPL ).

*CALL METHOD ZCL_HTML_REPORT=>GET_HTML_TEMPLATE
*  EXPORTING
*    IV_TEMPLATE = lv_templ
*  IMPORTING
*    EV_CONTENT  = lv_html
*    .

DATA: lv_subj TYPE STRING.
CALL METHOD ZCL_HTML_REPORT=>PARSE_TEMPLATE
  EXPORTING
    IV_INPUT = || && P_SUBJ
  RECEIVING
    EV_OUTPUT = lv_subj
    .


IF p_dryrun = 'X'.
*&---------------------------------------------------------------------*
*" HTML Preview
*&---------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF SCREEN 100.
SELECTION-SCREEN END OF SCREEN 100.

cl_abap_browser=>show_html( html_string = lv_html
                            title = || && lv_subj
                            container   = cl_gui_container=>default_screen ).

cl_abap_list_layout=>suppress_toolbar( ).

CALL SELECTION-SCREEN 100.

ELSE.
*&---------------------------------------------------------------------*
*" Send Mail
*&---------------------------------------------------------------------*
  DATA: lr_send_request TYPE REF TO cl_bcs,
        lr_document TYPE REF TO CL_DOCUMENT_BCS,
        lr_mrep TYPE REF TO if_mr_api ,
        lr_mime TYPE REF TO cl_gbt_multirelated_service,
        lv_line TYPE STRING,
        lv_subject TYPE so_obj_des,
        lv_text TYPE SOLI_TAB,
        lv_mail TYPE AD_SMTPADR,
        lv_att type string,
        gv_content TYPE xstring.

  lv_subject = lv_subj.

  TRY.
    lr_send_request = cl_bcs=>create_persistent( ).

*" Absender
    lr_send_request->set_sender(
      i_sender = cl_cam_address_bcs=>create_internet_address(
        p_sendr
        )
    ).

*" Empfänger
    lr_send_request->ADD_RECIPIENT(
      I_RECIPIENT = CL_DISTRIBUTIONLIST_BCS=>GETU_PERSISTENT(
        I_DLINAME = || && p_disl
        i_private = space
      )
      I_EXPRESS = 'X'
    ).

*" Content festlegen
    CREATE OBJECT LR_MIME.
    lr_mime->SET_MAIN_HTML(
      CONTENT = CL_DOCUMENT_BCS=>STRING_TO_SOLI(
                    ip_string = lv_html
      )
    ).
    lr_document = CL_DOCUMENT_BCS=>CREATE_FROM_MULTIRELATED(
      i_subject = LV_SUBJECT
      I_MULTIREL_SERVICE = LR_MIME ).
    lr_send_request->set_document(
      lr_document
    ).

*" Sofort senden
    lr_send_request->SET_SEND_IMMEDIATELY(
      i_send_immediately = 'X'
    ).

*" Absenden
    lr_send_request->send( ).

    COMMIT WORK.
  ENDTRY.

ENDIF.
