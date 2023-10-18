*&---------------------------------------------------------------------*
*& Report ZBW_OPENHUB_FILEMGMT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT ZBW_OPENHUB_FILEMGMT.

DATA: ls_ohdst TYPE RSBOHDEST-OHDEST.
*" =====================================================================
*" F4-Help for OpenHub-Destinations
*" =====================================================================
FORM F4_OHDEST.
  SELECT rsbohdest~OHDEST, rsbohdestt~TXTLG
    FROM RSBOHDEST
    LEFT OUTER JOIN RSBOHDESTT
      ON RSBOHDESTT~OBJVERS = RSBOHDEST~OBJVERS
    AND RSBOHDESTT~OHDEST = RSBOHDEST~OHDEST
    AND RSBOHDESTT~LANGU = @sy-langu
    INTO TABLE @DATA(it_f4ohdest)
    WHERE RSBOHDEST~OBJVERS = 'A'
    AND DESTTYPE = 'FILO'
    ORDER BY rsbohdest~OHDEST ASCENDING
    .

  IF sy-subrc = 0.
    PERFORM SHOW_F4_HELP USING 'OHDEST' 'SO_OHDEST' it_f4ohdest.
  ENDIF.
ENDFORM.
*" =====================================================================
*" Show F4-Help Dialog
*" =====================================================================
FORM SHOW_F4_HELP USING iv_field TYPE DFIES-FIELDNAME
                        iv_dpf TYPE HELP_INFO-DYNPROFLD
                        it_data TYPE STANDARD TABLE.

    CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
      EXPORTING
        RETFIELD               = iv_field
       DYNPPROG               = sy-repid
       DYNPNR                 = sy-dynnr
       DYNPROFIELD            = iv_dpf
       VALUE_ORG              = 'S'
      TABLES
        VALUE_TAB              = it_data
              .
    IF SY-SUBRC <> 0.
      RETURN.
    ENDIF.
ENDFORM.
*" =====================================================================
*" SELECTION-SCREEN
*" =====================================================================
SELECTION-SCREEN BEGIN OF BLOCK B1.
  SELECTION-SCREEN BEGIN OF LINE.
    SELECTION-SCREEN COMMENT 1(30) c_ohdst.
    SELECT-OPTIONS so_ohdst FOR ls_ohdst NO INTERVALS OBLIGATORY.
  SELECTION-SCREEN END OF LINE.

  SELECTION-SCREEN SKIP.

  SELECTION-SCREEN BEGIN OF LINE.
    PARAMETERS: p_dels TYPE C AS CHECKBOX DEFAULT ''.
    SELECTION-SCREEN COMMENT 3(30) c_dels FOR FIELD p_dels.
  SELECTION-SCREEN END OF LINE.
  SELECTION-SCREEN BEGIN OF LINE.
    PARAMETERS: p_genh TYPE C AS CHECKBOX DEFAULT ''.
    SELECTION-SCREEN COMMENT 3(30) c_genh FOR FIELD p_genh.
  SELECTION-SCREEN END OF LINE.
  SELECTION-SCREEN BEGIN OF LINE.
    PARAMETERS: p_wtrig TYPE C AS CHECKBOX DEFAULT ''.
    SELECTION-SCREEN COMMENT 3(30) c_wtrig FOR FIELD p_wtrig.
  SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN END OF BLOCK B1.
*" =====================================================================
*" TEXTE
*" =====================================================================
INITIALIZATION.
  c_ohdst = 'OpenHubs'.
  c_dels = 'Lösche Struktur-Dateien'.
  c_genh = 'Generiere HEADER-Zeile'.
  c_wtrig = 'Trigger.dat erzeugen'.
*" =====================================================================
*" VALUE HELP
*" =====================================================================
AT SELECTION-SCREEN ON VALUE-REQUEST FOR so_ohdst-LOW.
  PERFORM F4_OHDEST.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR so_ohdst-HIGH.
  PERFORM F4_OHDEST.
*" =====================================================================
*" START-OF-SELECTION
*" =====================================================================
START-OF-SELECTION.

TYPES: BEGIN OF ty_filenm,
         FILEINTERN TYPE FILEINTERN,
         PATH TYPE STRING,
         FILE TYPE STRING,
       END OF ty_filenm,
       BEGIN OF ty_fields,
         OHDEST TYPE RSOHDEST,
         POSIT TYPE RSPOSIT,
         FIELDNM TYPE RSFIELDNM,
         TXTSH TYPE RSTXTSH,
         TXTLG TYPE RSTXTLG,
       END OF ty_fields.

DATA: lt_ohdest TYPE TABLE OF RSBOHDEST,
      lt_ohfile TYPE TABLE OF RSBFILE,
      ls_files TYPE ty_filenm,
      lt_files TYPE TABLE OF ty_filenm,
      lv_tmpstr TYPE string,
      lt_tmpstr TYPE TABLE OF string,
      lt_ohflds TYPE SORTED TABLE OF ty_fields
        WITH UNIQUE KEY OHDEST POSIT.

FIELD-SYMBOLS: <lfs_ohdest> TYPE RSBOHDEST,
               <lfs_ohfile> TYPE RSBFILE,
               <lfs_ohfields> TYPE TY_FIELDS
               .

*" Get relevant OpenHubs
SELECT * FROM RSBOHDEST INTO CORRESPONDING FIELDS OF TABLE lt_ohdest
  WHERE OHDEST IN SO_OHDST
  AND DESTTYPE = 'FILO'
  AND OBJVERS = 'A'.

*" Get the logical names of OpenHub-Files
SELECT *
  FROM RSBFILE
  INTO CORRESPONDING FIELDS OF TABLE @lt_ohfile
  FOR ALL ENTRIES IN @lt_ohdest WHERE OHDEST = @lt_ohdest-OHDEST
  AND OBJVERS = 'A'.

*" OpenHub Field informations
SELECT RSBOHFIELDS~OHDEST, RSBOHFIELDS~POSIT, RSBOHFIELDS~FIELDNM, TXTSH, TXTLG
  FROM RSBOHFIELDS
  LEFT OUTER JOIN RSBOHFIELDST
    ON RSBOHFIELDS~OHDEST = RSBOHFIELDST~OHDEST
    AND RSBOHFIELDS~OBJVERS = RSBOHFIELDST~OBJVERS
    AND RSBOHFIELDS~FIELDNM = RSBOHFIELDST~FIELDNM
  INTO CORRESPONDING FIELDS OF TABLE @LT_OHFLDS
  FOR ALL ENTRIES IN @lt_ohdest WHERE RSBOHFIELDS~OHDEST = @lt_ohdest-OHDEST
    AND RSBOHFIELDS~OBJVERS = 'A'
    AND RSBOHFIELDST~LANGU = @sy-langu
  .

*" Get external filenames
CLEAR lt_files.
LOOP AT lt_ohfile ASSIGNING <lfs_ohfile>.
  CLEAR ls_files.
  ls_files-FILEINTERN = <lfs_ohfile>-FILEINTERN.

  CALL FUNCTION 'FILE_GET_NAME'
    EXPORTING
      LOGICAL_FILENAME              = <lfs_ohfile>-FILEINTERN
      ELEMINATE_BLANKS              = ' '
    IMPORTING
      FILE_NAME                     = ls_files-FILE
            .

  CALL FUNCTION 'TRINT_SPLIT_FILE_AND_PATH'
    EXPORTING
      FULL_NAME           = ls_files-file
   IMPORTING
     STRIPPED_NAME       = ls_files-file
     FILE_PATH           = ls_files-path
            .

  APPEND ls_files TO lt_files.
ENDLOOP.

"BREAK-POINT.

*" Delete Structure-Files
IF p_dels = 'X'.
  WRITE: / 'Deletion of structure information files: '.
  WRITE: / ''.

  LOOP AT lt_files INTO ls_files.
    lv_tmpstr = ls_files-PATH && 'S_' && ls_files-FILE.
    DELETE DATASET lv_tmpstr.
    IF sy-subrc EQ 0.
      WRITE: / 'File "' && lv_tmpstr && '" deleted successfully.'.
    ELSE.
      WRITE: / 'Could not delete file "' && lv_tmpstr && '"'.
    ENDIF.
  ENDLOOP.

  ULINE.
ENDIF.

IF p_genh = 'X'.
  WRITE: / 'Generation of header lines for files'.
  WRITE: / ''.

  LOOP AT LT_OHFILE ASSIGNING <lfs_ohfile>.
    " Kopfzeile aus OpenHub-Feldern lesen
    CLEAR: lv_tmpstr, lt_tmpstr, ls_files.
    LOOP AT LT_OHFLDS ASSIGNING <lfs_ohfields> WHERE OHDEST = <lfs_ohfile>-OHDEST.
      IF <lfs_ohfields>-TXTLG IS NOT INITIAL.
        lv_tmpstr = <lfs_ohfields>-TXTLG.
      ELSEIF <lfs_ohfields>-TXTSH IS NOT INITIAL.
        lv_tmpstr = <lfs_ohfields>-TXTSH.
      ELSE.
        lv_tmpstr = <lfs_ohfields>-FIELDNM.
      ENDIF.

      APPEND lv_tmpstr TO LT_TMPSTR.
    ENDLOOP.

    " Kopfzeile generieren
    CONCATENATE LINES OF lt_tmpstr INTO DATA(lv_hdrline) SEPARATED BY <lfs_ohfile>-SEPARATOR.
    lv_hdrline = lv_hdrline && CL_ABAP_CHAR_UTILITIES=>NEWLINE.

    CLEAR lv_tmpstr.
    READ TABLE LT_FILES INTO LS_FILES WITH KEY FILEINTERN = <lfs_ohfile>-FILEINTERN.

    lv_tmpstr = ls_files-PATH && ls_files-FILE.
    DATA(lr_conv) = CL_ABAP_CONV_OUT_CE=>CREATE( ).
    IF <lfs_ohfile>-CODEPAGE IS NOT INITIAL.
      lr_conv = CL_ABAP_CONV_OUT_CE=>CREATE( encoding = || && <lfs_ohfile>-CODEPAGE ).
    ENDIF.


    lr_conv->RESET( ).
    lr_conv->WRITE( data = lv_hdrline ).
    DATA(lv_xhdr) = lr_conv->GET_BUFFER( ).

    OPEN DATASET lv_tmpstr FOR UPDATE IN BINARY MODE.
    IF sy-subrc EQ 8.
      WRITE: / |For OpenHub | && <lfs_ohfile>-OHDEST && | file does not exist: | && lv_tmpstr.
      CONTINUE.
    ENDIF.

    DATA: lv_pos TYPE int8,
          lv_buffer(20000),
          lv_buflen TYPE int8,
          lv_whdr TYPE c.
    DATA(lv_addlen) = xstrlen( lv_xhdr ).

    CLEAR: lv_pos, lv_buffer, lv_buflen, lv_whdr.

    SET DATASET lv_tmpstr POSITION -1.
    GET DATASET lv_tmpstr POSITION lv_pos.
    lv_buflen = lv_addlen.

    WHILE lv_pos > 0.
      lv_pos = lv_pos - lv_buflen.
      IF lv_pos LE 0.
        lv_pos = 0.
        lv_whdr = 'X'.
      ENDIF.

      SET DATASET lv_tmpstr POSITION lv_pos.
      READ DATASET lv_tmpstr INTO lv_buffer MAXIMUM LENGTH lv_addlen ACTUAL LENGTH lv_buflen.
      DATA(lv_npos) = lv_pos + lv_addlen.

      SET DATASET lv_tmpstr POSITION lv_npos.
      TRANSFER lv_buffer TO lv_tmpstr LENGTH lv_buflen.

      IF lv_whdr EQ 'X'.
        SET DATASET lv_tmpstr POSITION 0.
        TRANSFER lv_xhdr TO lv_tmpstr.
        EXIT.
      ENDIF.
    ENDWHILE.
    CLOSE DATASET lv_tmpstr.
    IF sy-subrc EQ 0.
      WRITE: / |Headerline for OpenHub | && <lfs_ohfile>-OHDEST && | written to file: | && lv_tmpstr.
    ENDIF.
  ENDLOOP.

  ULINE.
ENDIF.

IF p_wtrig EQ 'X'.
  WRITE: / 'Creating Trigger-Files'.
  WRITE: / '' .
  LOOP AT LT_FILES ASSIGNING FIELD-SYMBOL(<lfs_files>) GROUP BY ( key1 = <lfs_files>-PATH ).
      CLEAR lv_tmpstr.
      lv_tmpstr = <lfs_files>-PATH && 'Trigger.dat'.
      OPEN DATASET lv_tmpstr FOR OUTPUT IN TEXT MODE ENCODING DEFAULT.
      IF sy-subrc NE 0.
        WRITE: / 'Could not create Trigger-File at: ' && lv_tmpstr.
        CONTINUE.
      ENDIF.
      TRANSFER 'Finish' TO lv_tmpstr.
      CLOSE DATASET lv_tmpstr.
      IF sy-subrc EQ 0.
        WRITE: / 'Created Trigger-File at: ' && lv_tmpstr.
      ENDIF.
  ENDLOOP.

  ULINE.
ENDIF.
