*&---------------------------------------------------------------------*
*& Report ZBW_FIND_IOBJ_VALUE
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT ZBW_FIND_IOBJ_VALUE.

PARAMETERS: p_iobj TYPE RSD_IOBJNM OBLIGATORY.
PARAMETERS: p_value TYPE STRING.

TYPES: BEGIN OF ty_s_lookup,
         TLOGO TYPE STRING,
         IPRO TYPE RSINFOCUBE,
         IOBJ TYPE RSD_IOBJNM,
         ENTRIES TYPE i,
       END OF ty_s_lookup,
       ty_t_lookup TYPE TABLE OF ty_s_lookup.

FIELD-SYMBOLS: <lfs_chb> TYPE RS_S_USED_BY,
               <lfs_lou> TYPE ty_s_lookup.

DATA: lt_lookup TYPE TY_T_LOOKUP,
      ls_lookup TYPE ty_s_lookup,
      lt_chabas TYPE RS_T_USED_BY,
      lt_chabas_tmp TYPE RS_T_USED_BY.

ls_lookup-TLOGO = 'IOBJ'.
ls_lookup-IOBJ = p_iobj.
ls_lookup-IPRO = p_iobj.
APPEND ls_lookup TO lt_lookup.

PERFORM GET_IOBJ_USAGE USING p_IOBJ
                       CHANGING lt_lookup
                                lt_chabas.

LOOP AT lt_chabas ASSIGNING <LFS_CHB> WHERE TOBJVERS = 'A'.
  PERFORM GET_IOBJ_USAGE USING <lfs_chb>-TOBJNM
                         CHANGING lt_lookup
                                  LT_CHABAS_TMP.
ENDLOOP.

SORT lt_lookup.
DELETE ADJACENT DUPLICATES FROM lt_lookup.

DATA: lt_data TYPE TABLE OF string,
      lv_entries TYPE i.
DAta: lv_msg TYPE STRING.

LOOP AT LT_LOOKUP ASSIGNING <lfs_lou>. " WHERE TLOGO EQ 'CUBE' OR TLOGO EQ 'ODSO' OR TLOGO EQ 'ADSO'.
  CLEAR lt_data.

  lv_msg = |Scanning | && <lfs_lou>-TLOGO && | | && <lfs_lou>-IPRO.
  PERFORM PROGRESS_INDICATOR USING lv_msg.
    PERFORM READ_INFO_PROV USING <lfs_lou>-IPRO
                                 <lfs_lou>-IOBJ
                                 p_value
                           CHANGING
                                 lt_data.

  DESCRIBE TABLE lt_data LINES lv_entries.
  <lfs_lou>-ENTRIES = lv_entries.
  WRITE: / <lfs_lou>-TLOGO, <lfs_lou>-IPRO, <lfs_lou>-IOBJ , |Found: | && lv_entries.
ENDLOOP.

DATA: lt_fields         TYPE SLIS_T_FIELDCAT_ALV,
      ls_fieldcat       TYPE slis_fieldcat_alv.

LS_FIELDCAT-FIELDNAME = 'TLOGO'.
ls_fieldcat-SELTEXT_L = 'Objekt Typ'.
ls_fieldcat-DATATYPE = 'SSTR'.
APPEND ls_fieldcat TO lt_fields.

LS_FIELDCAT-FIELDNAME = 'IPRO'.
ls_fieldcat-SELTEXT_L = 'Objekt'.
ls_fieldcat-DATATYPE = 'SSTR'.
APPEND ls_fieldcat TO lt_fields.

LS_FIELDCAT-FIELDNAME = 'IOBJ'.
ls_fieldcat-SELTEXT_L = 'Merkmal'.
ls_fieldcat-DATATYPE = 'SSTR'.
APPEND ls_fieldcat TO lt_fields.

LS_FIELDCAT-FIELDNAME = 'ENTRIES'.
ls_fieldcat-SELTEXT_L = 'Treffer'.
ls_fieldcat-DATATYPE = 'INT8'.
APPEND ls_fieldcat TO lt_fields.

*" 3) Ausgabe der Treffer in ALV-Grid
*CALL FUNCTION 'REUSE_ALV_FIELDCATALOG_MERGE'
* EXPORTING
*   I_PROGRAM_NAME               = sy-repid
*   I_STRUCTURE_NAME             = 'TY_S_LOOKUP'
*   I_INCLNAME                   = sy-repid
*  CHANGING
*    CT_FIELDCAT                  = lt_fields
*          .

CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
 EXPORTING
   I_CALLBACK_PROGRAM                = sy-repid
   I_CALLBACK_USER_COMMAND           = 'USER_COMMAND' " Seperate FORM
   IT_FIELDCAT                       = lt_fields
  TABLES
    T_OUTTAB                          = lt_lookup
          .


FORM READ_INFO_PROV USING I_INFOPROVNM TYPE RSINFOCUBE
                          I_IOBJNM TYPE RSD_IOBJNM
                          I_VALUE TYPE string
                    CHANGING E_DATA TYPE ANY TABLE
.
  DATA: lt_sfc TYPE RSDRI_TH_SFC,
        ls_sfc TYPE RSDRI_S_SFC,
        lt_sfk TYPE RSDRI_TH_SFK,
        lv_firstcall TYPE RS_BOOL,
        lv_eod TYPE rs_bool,
        ls_cobpro TYPE RSD_S_COB_PRO,
        ls_structtp TYPE REF TO CL_ABAP_STRUCTDESCR,
        lr_tabdesc TYPE REF TO CL_ABAP_TABLEDESCR,
        lt_comptab TYPE CL_ABAP_STRUCTDESCR=>COMPONENT_TABLE,
        ls_compline LIKE LINE OF LT_COMPTAB,
        lr_dref TYPE REF TO data,
        lr_dref3 TYPE REF TO data,
        lr_element   TYPE REF TO cl_abap_elemdescr,
        lt_range TYPE RSDRI_T_RANGE,
        ls_range TYPE RSDRI_S_RANGE.

  FIELD-SYMBOLS: <lfs_struc> TYPE ANY,
                 <lft_tmp> TYPE ANY TABLE.

  " Get InfoObject description
  CALL FUNCTION 'RSD_IOBJ_GET'
    EXPORTING
      I_IOBJNM                    = i_iobjnm
   IMPORTING
     E_S_COB_PRO                 = ls_cobpro
            .

  " Add InfoObject to Cube-Selection
  ls_sfc-CHANM = ls_cobpro-IOBJNM.
  ls_sfc-CHAALIAS = ls_cobpro-FIELDNM.
  INSERT ls_sfc INTO TABLE lt_sfc.

  ls_range-CHANM = ls_cobpro-IOBJNM.
  IF i_value CS '*'.
    ls_range-COMPOP = 'CP'.
  ELSE.
    ls_range-COMPOP = 'EQ'.
  ENDIF.
  ls_range-SIGN = 'I'.
  ls_range-LOW = I_VALUE.
  APPEND ls_range TO LT_RANGE.

  " Create Structure for output table
  LR_ELEMENT ?= CL_ABAP_ELEMDESCR=>DESCRIBE_BY_NAME( ls_cobpro-DTELNM ).
  ls_compline-NAME = ls_cobpro-FIELDNM.
  ls_compline-type = LR_ELEMENT.
  APPEND ls_compline TO lt_comptab.
  LS_STRUCTTP = CL_ABAP_STRUCTDESCR=>CREATE( lt_comptab ).
  CREATE DATA lr_dref TYPE HANDLE LS_STRUCTTP.
  ASSIGN lr_dref->* TO <lfs_struc>.

  " Create buffer table
  LR_TABDESC = CL_ABAP_TABLEDESCR=>CREATE( ls_structtp ).
  CREATE DATA lr_dref3 TYPE HANDLE lr_tabdesc.
  ASSIGN lr_dref3->* TO <lft_tmp>.

  " Read InfoProvider
*  WHILE lv_eod = ''.
    CALL FUNCTION 'RSDRI_INFOPROV_READ'
      EXPORTING
        I_INFOPROV                   = I_INFOPROVNM
        I_TH_SFC                     = lt_sfc
        I_TH_SFK                     = lt_sfk
        I_T_RANGE                    = lt_range
*       I_TH_TABLESEL                =
*       I_T_RTIME                    =
*       I_REFERENCE_DATE             = SY-DATUM
*       I_ROLLUP_ONLY                = RS_C_TRUE
*       I_T_REQUID                   =
*       I_T_REQTSN                   =
*       I_SAVE_IN_TABLE              = ' '
*       I_TABLENAME                  =
*       I_SAVE_IN_FILE               = ' '
*       I_FILENAME                   =
*       I_SAVE_IN_INDEX              = ' '
*       I_INDEX_ID                   =
*       I_PACKAGESIZE                = 2000
*       I_MAXROWS                    = 0
*       I_AUTHORITY_CHECK            = RSDRC_C_AUTHCHK-READ
*       I_CURRENCY_CONVERSION        = 'X'
*       I_USE_DB_AGGREGATION         = RS_C_TRUE
*       I_USE_AGGREGATES             = RS_C_TRUE
*       I_READ_ODS_DELTA             = RS_C_FALSE
*       I_CALLER                     = RSDRS_C_CALLER-RSDRI
*       I_DEBUG                      = RS_C_FALSE
*       I_CLEAR                      = RS_C_FALSE
*       I_COMMIT_ALLOWED             = RS_C_TRUE
*       I_WITH_NLS                   = RS_C_FALSE
*       I_CHECK_RESULTS              = RS_C_FALSE
*       I_PROCESS_MPRO_IN_TREX       =
     IMPORTING
       E_T_DATA                     = <lft_tmp>
       E_END_OF_DATA                = lv_eod
*       E_AGGREGATE                  =
*       E_SPLIT_OCCURRED             =
*       E_T_MSG                      =
*       E_STEPUID                    =
      CHANGING
        C_FIRST_CALL                 = LV_FIRSTCALL
  EXCEPTIONS
      X_MESSAGE = 1
      OTHERS = 2
    .

    INSERT LINES OF <lft_tmp> INTO TABLE e_data.
    FREE <lft_tmp>.
*  ENDWHILE.
ENDFORM.

FORM PROGRESS_INDICATOR USING I_MSG TYPE STRING.

 CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
    EXPORTING
      text   = i_MSG
    EXCEPTIONS
      OTHERS = 1.

ENDFORM.

FORM GET_IOBJ_USAGE USING i_iobj TYPE RSD_IOBJNM
                    CHANGING ct_lookup TYPE ty_t_lookup
                             ct_chabas TYPE RS_T_USED_BY.

DATA: lt_cubes TYPE RS_T_USED_BY,
      lt_odsos TYPE RSO_T_TLOGO_ASC,
      lt_adsos TYPE RSO_T_TLOGO_ASC,
      lt_attri TYPE RS_T_USED_BY,
      ls_lookup TYPE ty_s_lookup.

FIELD-SYMBOLS: <lfs_cube> TYPE rs_s_used_by,
               <lfs_dso> TYPE RSO_S_TLOGO_ASC,
               <lfs_chb> TYPE RS_S_USED_BY.

CALL FUNCTION 'RSD_IOBJ_USAGE'
  EXPORTING
    I_IOBJNM                     = i_iobj
 IMPORTING
   E_T_CUBE                     = lt_cubes
   E_T_ATR_IOBJ                 = lt_attri
   E_T_CHABAS_IOBJ              = CT_CHABAS
   E_T_ODSO                     = lt_odsos
   E_T_ADSO                     = lt_adsos
 EXCEPTIONS
   ILLEGAL_INPUT                = 1
   OTHERS                       = 2
   .

ls_lookup-IOBJ = i_iobj.
LS_LOOKUP-TLOGO = 'CUBE'.
LOOP AT lt_cubes ASSIGNING <LFS_CUBE>.
  ls_lookup-IPRO = <lfs_cube>-TOBJNM.
  APPEND ls_lookup TO CT_LOOKUP.
ENDLOOP.

ls_lookup-TLOGO = 'ODSO'.
LOOP AT lt_odsos ASSIGNING <lfs_dso>.
  ls_lookup-IPRO = <lfs_dso>-OBJNM.
  APPEND ls_lookup TO ct_lookup.
ENDLOOP.

ls_lookup-TLOGO = 'ADSO'.
LOOP AT lt_adsos ASSIGNING <lfs_dso>.
  ls_lookup-IPRO = <lfs_dso>-OBJNM.
  APPEND ls_lookup TO ct_lookup.
ENDLOOP.

ls_lookup-TLOGO = 'IOBJ'.
LOOP AT lt_attri ASSIGNING <lfs_cube>.
  ls_lookup-IPRO = <LFS_CUBE>-TOBJNM.
  APPEND ls_lookup TO ct_lookup.
ENDLOOP.

ENDFORM.
