*&---------------------------------------------------------------------*
*& Report  ZFIND_IN_APD
*&
*&---------------------------------------------------------------------*
*& Suche nach Strings in Analyseprozessen
*&
*&---------------------------------------------------------------------*
REPORT ZFIND_IN_APD.
*"=============================================================================
*" DATA
*"=============================================================================
DATA: lt_rsant_process  TYPE TABLE OF rsant_process,
      lt_string         TYPE TABLE OF STRING,
      ls_rsant          TYPE rsant_process,
      lt_fields         TYPE SLIS_T_FIELDCAT_ALV.
*"=============================================================================
*" SELECT-OPTIONS:
*"    so_proc   = Process Name
*"=============================================================================
SELECTION-SCREEN BEGIN OF LINE.
  SELECTION-SCREEN COMMENT (12) l_proc FOR FIELD so_proc.
  SELECT-OPTIONS  so_proc   FOR ls_rsant-process.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN BEGIN OF LINE.
  SELECTION-SCREEN COMMENT (15) l_act FOR FIELD p_act.
  PARAMETER p_act TYPE RSOBJVERS  DEFAULT 'A'.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN SKIP.
*"=============================================================================
*" PARAMETER:
*"    p_string  = Suchstring in Analysprozessen
*"    p_act     = Objektversion 'A' = Active
*"    p_icase   = Ignore case 'X' = True
*"=============================================================================
SELECTION-SCREEN BEGIN OF LINE.
  SELECTION-SCREEN COMMENT (15) l_string for field p_string.
  PARAMETER p_string  TYPE string     OBLIGATORY LOWER CASE.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN BEGIN OF LINE.
  SELECTION-SCREEN COMMENT (15) l_case for field p_case.
  PARAMETER p_case TYPE abap_bool  DEFAULT abap_true AS CHECKBOX.
SELECTION-SCREEN END OF LINE.
*"=============================================================================
*" INITIALIZATION
*"=============================================================================
INITIALIZATION.
  l_proc    = 'Prozessname'.
  l_act     = 'Objektversion'.
  l_string  = 'Such-Pattern'.
  l_case    = 'Ignore Case'.
*"=============================================================================
*" FIELD SYMBOLS
*"=============================================================================
FIELD-SYMBOLS: <lfs_line> TYPE RSANT_PROCESS.

START-OF-SELECTION.
*" 1) Selektion der Analyseprozesse
SELECT * FROM RSANT_PROCESS
  INTO CORRESPONDING FIELDS OF TABLE lt_rsant_process
  WHERE OBJVERS EQ p_act
    AND process IN so_proc
  .

*" 2) Suche nach string / regex
LOOP AT lt_rsant_process ASSIGNING <lfs_line>.
  SPLIT <lfs_line>-xml AT cl_abap_char_utilities=>cr_lf INTO TABLE lt_string.
  IF p_case EQ abap_true.
    FIND REGEX p_string IN TABLE lt_string IGNORING CASE.
  ELSE.
    FIND REGEX p_string IN TABLE lt_string RESPECTING CASE.
  ENDIF.

  IF sy-subrc NE 0.
    DELETE lt_rsant_process
      WHERE PROCESS = <lfs_line>-PROCESS
        AND OBJVERS = <lfs_line>-OBJVERS.
  ENDIF.
ENDLOOP.

*" 3) Ausgabe der Treffer in ALV-Grid
CALL FUNCTION 'REUSE_ALV_FIELDCATALOG_MERGE'
 EXPORTING
   I_PROGRAM_NAME               = sy-repid
   I_STRUCTURE_NAME             = 'RSANT_PROCESS'
   I_INCLNAME                   = sy-repid
  CHANGING
    CT_FIELDCAT                  = lt_fields
          .

CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
 EXPORTING
   I_CALLBACK_PROGRAM                = sy-repid
   I_CALLBACK_USER_COMMAND           = 'USER_COMMAND' " Seperate FORM
   IT_FIELDCAT                       = lt_fields
  TABLES
    T_OUTTAB                          = lt_rsant_process
          .

*"=============================================================================
*" FORM USER_COMMAND
*"=============================================================================
FORM USER_COMMAND USING r_ucomm LIKE sy-ucomm
                  rs_selfield TYPE slis_selfield.

  CASE r_ucomm.
    WHEN '&IC1'.
*" TODO: Handle Doubleclick

    WHEN OTHERS.

  ENDCASE.
ENDFORM.
