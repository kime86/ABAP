*&---------------------------------------------------------------------*
*& Report ZBW_MEMORY_PER_OBJECT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT ZBW_MEMORY_PER_OBJECT.

TYPES: BEGIN OF ty_data,
        OBJNM TYPE String,
        OBJTP TYPE String,
        OBJTXT TYPE String,
        DATUM TYPE DATS,
        OBJSIZE TYPE INT8,
        TABLES TYPE String,
      END OF ty_data,
      BEGIN OF ty_hdbout,
        TABLE_NAME TYPE STRING,
        SIZE TYPE INT8,
      END OF ty_hdbout,
      tt_tables TYPE TABLE OF string.

DATA: lt_data TYPE TABLE OF ty_data,
      lt_tmp TYPE TABLE OF ty_data,
      lt_hdbout TYPE TABLE OF ty_hdbout,
      ls_hdbout TYPE ty_hdbout,
      ls_data TYPE ty_data,
      lv_pattern TYPE string,
      lv_max TYPE i,
      lv_percent TYPE i,
      lv_msg TYPE string.

FIELD-SYMBOLS: <lfs_data> TYPE ty_data.


*" 1) Read InfoObjects
PERFORM WRITE_PROGRESS USING 0 'Read InfoObjects...'.
SELECT t1~IOBJNM AS OBJNM,
       'IOBJ' AS OBJTP,
       t2~TXTLG AS OBJTXT
  INTO CORRESPONDING FIELDS OF TABLE @lt_tmp
  FROM RSDIOBJ AS t1
  LEFT OUTER JOIN RSDIOBJT AS t2
    ON t1~IOBJNM = t2~IOBJNM
    AND t2~LANGU = @sy-langu
    AND t1~objvers = t2~objvers
  WHERE t1~OBJVERS = 'A'.

APPEND LINES OF lt_tmp TO lt_data.
FREE lt_tmp.

*" 2) Read DSOs (classic)
PERFORM WRITE_PROGRESS USING 10 'Read DSOs...'.
SELECT t1~ODSOBJECT AS OBJNM,
       'ODSO' AS OBJTP,
       t2~TXTLG AS OBJTXT
  INTO CORRESPONDING FIELDS OF TABLE @lt_tmp
  FROM RSDODSO AS t1
  LEFT OUTER JOIN RSDODSOT AS t2
    ON t1~ODSOBJECT = t2~ODSOBJECT
    AND t2~LANGU = @sy-langu
    AND t1~OBJVERS = t2~OBJVERS
  WHERE t1~OBJVERS = 'A'.

APPEND LINES OF lt_tmp TO lt_data.
FREE lt_tmp.

*" 3) Read ADSOs
PERFORM WRITE_PROGRESS USING 20 'Read ADSOs...'.
SELECT t1~ADSONM AS OBJNM,
       'ADSO' AS OBJTP,
       t2~DESCRIPTION AS OBJTXT
  INTO CORRESPONDING FIELDS OF TABLE @lt_tmp
  FROM RSOADSO AS t1
  LEFT OUTER JOIN RSOADSOT AS t2
    ON t1~ADSONM = t2~ADSONM
    AND t2~LANGU = @sy-langu
    AND t2~TTYP = 'EUSR'
    AND t2~OBJVERS = t1~OBJVERS
  WHERE t1~OBJVERS = 'A'.

APPEND LINES OF lt_tmp TO lt_data.
FREE lt_tmp.

*" 4) Read Cubes
PERFORM WRITE_PROGRESS USING 30 'Read Cubes...'.
SELECT t1~INFOCUBE AS OBJNM,
       'CUBE' AS OBJTP,
       t2~TXTLG AS OBJTXT
  INTO CORRESPONDING FIELDS OF TABLE @lt_tmp
  FROM RSDCUBE AS t1
  LEFT OUTER JOIN RSDCUBET AS t2
    ON t1~INFOCUBE = t2~INFOCUBE
    AND t2~LANGU = @sy-langu
    AND t2~OBJVERS = t1~OBJVERS
  WHERE t1~OBJVERS = 'A'.

APPEND LINES OF lt_tmp TO lt_data.
FREE lt_tmp.

*" 5) Read last loading request date
PERFORM WRITE_PROGRESS USING 40 'Read last loading requests...'.
SELECT  t1~TGT AS OBJNM,
        MAX( t2~DATUM ) AS DATUM
  INTO CORRESPONDING FIELDS OF TABLE @lt_tmp
  FROM RSBKDTP AS t1
  LEFT OUTER JOIN RSREQDONE AS t2
    ON t1~DTP = t2~LOGDPID
  GROUP BY t1~TGT.

*" 6) Put last loading date to table...
PERFORM WRITE_PROGRESS USING 50 'Read last loading requests...'.
SORT lt_tmp BY OBJNM.
LOOP AT lt_data ASSIGNING <LFS_DATA>.
  CLEAR ls_data.
  READ TABLE lt_tmp
    INTO ls_data
    WITH KEY OBJNM = <lfs_data>-OBJNM
    BINARY SEARCH.

  <lfs_data>-DATUM = ls_data-DATUM.
ENDLOOP.
FREE lt_tmp.

*" 7) Read Table-Sizes from HANA-View
PERFORM WRITE_PROGRESS USING 60 'Read HANA Table Sizes...'.
EXEC SQL.
  OPEN dbcursor FOR
  SELECT  TABLE_NAME,
          SUM( MEMORY_SIZE_IN_TOTAL ) AS SIZE
    FROM M_CS_TABLES
    WHERE TABLE_NAME LIKE '/BI%'
    GROUP BY TABLE_NAME
ENDEXEC.

*" Fetch results...
DO.
  EXEC SQL.
    FETCH NEXT dbcursor
      INTO :ls_hdbout-TABLE_NAME,
           :ls_hdbout-SIZE

  ENDEXEC.
  IF sy-subrc EQ 0.
    APPEND ls_hdbout TO lt_hdbout.
    CLEAR ls_hdbout.
  ELSE.
    EXIT.
  ENDIF.
ENDDO.

DATA: lt_tables TYPE TABLE OF string,
      ls_table TYPE string.

" 8) SUM up Object sizes over tables
PERFORM WRITE_PROGRESS USING 80 'Read HANA Table Sizes...'.
SORT lt_hdbout BY TABLE_NAME.
LOOP AT lt_data ASSIGNING <lfs_data>.
  CLEAR lt_tables.
  PERFORM GET_OBJECT_TABLES USING <lfs_data> CHANGING lt_tables.

  LOOP AT lt_tables INTO ls_table.
    CLEAR ls_hdbout.
    READ TABLE lt_hdbout INTO ls_hdbout WITH KEY TABLE_NAME = ls_table BINARY SEARCH.
*    LOOP AT lt_hdbout INTO ls_hdbout WHERE TABLE_NAME CP ls_table.
      IF ls_hdbout IS NOT INITIAL.
        ADD ls_hdbout-SIZE TO <lfs_data>-OBJSIZE.
        IF <lfs_data>-TABLES IS INITIAL.
          <lfs_data>-TABLES = ls_table.
        ELSE.
          <lfs_data>-TABLES = <lfs_data>-TABLES && |, | && ls_table.
        ENDIF.
      ENDIF.
*    ENDLOOP.
  ENDLOOP.

*  LOOP AT lt_hdbout INTO ls_hdbout WHERE TABLE_NAME CS <lfs_data>-OBJNM.
*    ADD ls_hdbout-SIZE TO <lfs_data>-OBJSIZE.
*    IF <lfs_data>-TABLES IS INITIAL.
*      <lfs_data>-TABLES = ls_hdbout-TABLE_NAME.
*    ELSE.
*      <lfs_data>-TABLES = <lfs_data>-TABLES && |, | && ls_hdbout-TABLE_NAME.
*    ENDIF.
*  ENDLOOP.
ENDLOOP.

SORT lt_data BY OBJSIZE DESCENDING.

PERFORM WRITE_PROGRESS USING 100 'Displaying data...'.

DATA: lt_fieldcat TYPE SLIS_T_FIELDCAT_ALV,
      ls_fieldcat TYPE slis_fieldcat_alv.

ls_fieldcat-FIELDNAME = 'OBJNM'.
ls_fieldcat-SELTEXT_L = 'Objekt Name'.
ls_fieldcat-DATATYPE = 'SSTR'.
APPEND ls_fieldcat TO lt_fieldcat.

ls_fieldcat-FIELDNAME = 'OBJTP'.
ls_fieldcat-SELTEXT_L = 'Objekt Typ'.
ls_fieldcat-DATATYPE = 'SSTR'.
APPEND ls_fieldcat TO lt_fieldcat.

ls_fieldcat-FIELDNAME = 'OBJTXT'.
ls_fieldcat-SELTEXT_L = 'Bezeichnung'.
ls_fieldcat-DATATYPE = 'SSTR'.
APPEND ls_fieldcat TO lt_fieldcat.

ls_fieldcat-FIELDNAME = 'DATUM'.
ls_fieldcat-SELTEXT_L = 'Zuletzt beladen am'.
ls_fieldcat-DATATYPE = 'DATS'.
APPEND ls_fieldcat TO lt_fieldcat.

ls_fieldcat-FIELDNAME = 'OBJSIZE'.
ls_fieldcat-SELTEXT_L = 'Gesamtgröße (Byte)'.
ls_fieldcat-DATATYPE = 'INT8'.
APPEND ls_fieldcat TO lt_fieldcat.

ls_fieldcat-FIELDNAME = 'TABLES'.
ls_fieldcat-SELTEXT_L = 'Datenbank Tabellen'.
ls_fieldcat-DATATYPE = 'SSTR'.
APPEND ls_fieldcat TO lt_fieldcat.

CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
 EXPORTING
*   I_INTERFACE_CHECK                 = ' '
*   I_BYPASSING_BUFFER                = ' '
*   I_BUFFER_ACTIVE                   = ' '
*   I_CALLBACK_PROGRAM                = ' '
*   I_CALLBACK_PF_STATUS_SET          = ' '
*   I_CALLBACK_USER_COMMAND           = ' '
*   I_CALLBACK_TOP_OF_PAGE            = ' '
*   I_CALLBACK_HTML_TOP_OF_PAGE       = ' '
*   I_CALLBACK_HTML_END_OF_LIST       = ' '
*   I_STRUCTURE_NAME                  =
*   I_BACKGROUND_ID                   = ' '
*   I_GRID_TITLE                      =
*   I_GRID_SETTINGS                   =
*   IS_LAYOUT                         =
   IT_FIELDCAT                       = lt_fieldcat
*   IT_EXCLUDING                      =
*   IT_SPECIAL_GROUPS                 =
*   IT_SORT                           =
*   IT_FILTER                         =
*   IS_SEL_HIDE                       =
*   I_DEFAULT                         = 'X'
*   I_SAVE                            = ' '
*   IS_VARIANT                        =
*   IT_EVENTS                         =
*   IT_EVENT_EXIT                     =
*   IS_PRINT                          =
*   IS_REPREP_ID                      =
*   I_SCREEN_START_COLUMN             = 0
*   I_SCREEN_START_LINE               = 0
*   I_SCREEN_END_COLUMN               = 0
*   I_SCREEN_END_LINE                 = 0
*   I_HTML_HEIGHT_TOP                 = 0
*   I_HTML_HEIGHT_END                 = 0
*   IT_ALV_GRAPHICS                   =
*   IT_HYPERLINK                      =
*   IT_ADD_FIELDCAT                   =
*   IT_EXCEPT_QINFO                   =
*   IR_SALV_FULLSCREEN_ADAPTER        =
* IMPORTING
*   E_EXIT_CAUSED_BY_CALLER           =
*   ES_EXIT_CAUSED_BY_USER            =
  TABLES
    T_OUTTAB                          = lt_data
* EXCEPTIONS
*   PROGRAM_ERROR                     = 1
*   OTHERS                            = 2
          .
IF SY-SUBRC <> 0.
* Implement suitable error handling here
ENDIF.


FORM WRITE_PROGRESS  USING iv_percent TYPE i iv_msg TYPE string.

 CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
   EXPORTING
     percentage = iv_percent
     text       = iv_msg.

ENDFORM.

FORM GET_OBJECT_TABLES USING iv_object TYPE ty_data
                       CHANGING lt_tables TYPE tt_tables.

  DATA: lv_string TYPE string,
        lv_objnam TYPE string,
        lv_prefix TYPE string.

  IF iv_object-OBJNM+0(1) = '0'.
    lv_prefix = '/BI0/'.
    lv_objnam = iv_object-OBJNM.
    SHIFT lv_objnam LEFT BY 1 PLACES.
  ELSE.
    lv_prefix = '/BIC/'.
    lv_objnam = iv_object-OBJNM.
  ENDIF.

  CASE iv_object-OBJTP.
    WHEN 'IOBJ'.
      APPEND lv_prefix && 'P' && lv_objnam TO lt_tables. " Attributes
      APPEND lv_prefix && 'Q' && lv_objnam TO lt_tables. " Time Dependant
      APPEND lv_prefix && 'T' && lv_objnam TO lt_tables. " Texts
      APPEND lv_prefix && 'X' && lv_objnam TO lt_tables. "
      APPEND lv_prefix && 'Y' && lv_objnam TO lt_tables.
      APPEND lv_prefix && 'S' && lv_objnam TO lt_tables.
      APPEND lv_prefix && 'H' && lv_objnam TO lt_tables.
      APPEND lv_prefix && 'K' && lv_objnam TO lt_tables.
      APPEND lv_prefix && 'I' && lv_objnam TO lt_tables.
      APPEND lv_prefix && 'J' && lv_objnam TO lt_tables.

    WHEN 'ODSO'.
      APPEND lv_prefix && 'A' && lv_objnam && '00' TO lt_tables. " Aktive Daten
      APPEND lv_prefix && 'A' && lv_objnam && '40' TO lt_tables. "Aktivierungs-Queue

    WHEN 'ADSO'.
      APPEND lv_prefix && 'A' && lv_objnam && '2' TO lt_tables. " Aktive Daten
      APPEND lv_prefix && 'A' && lv_objnam && '1' TO lt_tables. " Aktivierungs-Queue
      APPEND lv_prefix && 'A' && lv_objnam && '3' TO lt_tables. " Aktivierungs-Queue

    WHEN 'CUBE'.
      APPEND lv_prefix && 'E' && lv_objnam TO lt_tables. " E-Fact table
      APPEND lv_prefix && 'F' && lv_objnam TO lt_tables. " Fact table
      APPEND lv_prefix && 'D' && lv_objnam && 'P' TO lt_tables.
      APPEND lv_prefix && 'D' && lv_objnam && 'T' TO lt_tables.
      APPEND lv_prefix && 'D' && lv_objnam && 'U' TO lt_tables.

  ENDCASE.

ENDFORM.
