*&---------------------------------------------------------------------*
*& Report ZDELETE_OLD_OUTPUT_FILES
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT ZDELETE_OLD_OUTPUT_FILES.

PARAMETERS: p_older TYPE I DEFAULT '30' OBLIGATORY,
            p_simul TYPE c DEFAULT 'X' AS CHECKBOX.

DATA: lt_files TYPE TABLE OF STRING,
      lt_fileinfo TYPE ZCL_FILE_UTILS=>YTT_FILE_INFO,
      ls_fileinfo TYPE ZCL_FILE_UTILS=>YT_FILE_INFO,
      lt_path TYPE TABLE OF PATH,
      ls_path TYPE  PATH,
      ls_checkdat TYPE sy-datum,
      lv_fsize TYPE p,
      lv_size TYPE p,
      lv_cnt TYPE i.

*" Force Simulation mode on PB1
IF p_SIMUL NE 'X' AND sy-SYSID EQ 'PB1'.
  WRITE /: |Running in productive environment - forcing simulation!|.
  P_SIMUL = 'X'.
ENDIF.

IF p_SIMUL EQ 'X'.
  WRITE /: |---- Simulation ----|.
ENDIF.

*" Calculate the compare Date (today - p_older days)
ls_checkdat = sy-datum.
ls_checkdat = ls_checkdat - p_older.
WRITE /: |Delete files older than | && ls_checkdat.
ULINE.

*" Get a list of all Z*OUT* Folders from Transaction FILE
*" And replace <SYSID> parameter to get the physical folder name
SELECT PATHINTERN,
       FILESYS,
       RTRIM( REPLACE( REPLACE( PATHEXTERN, '<SYSID>', @sy-sysid ), '<FILENAME>', ' ' ), ' ' ) AS PATHEXTERN
  FROM PATH
  WHERE PATHINTERN LIKE 'Z%OUT%'
  INTO CORRESPONDING FIELDS OF TABLE @lt_path
  .

*" Loop at the output-folders
LOOP AT lt_path INTO ls_path.
  CLEAR LT_FILEINFO.

*" Get files of physical folder - with file information (e.g. last modification date)
  ZCL_FILE_UTILS=>LIST_DIRECTORY( EXPORTING IV_DIRECTORY =  ls_path-pathextern && '' IMPORTING ET_FILE_INFO = LT_FILEINFO ).

*" Loop over the files in folder
  LOOP AT LT_FILEINFO INTO ls_fileinfo.

*" Check file is older than comparison date
    if LS_FILEINFO-FILE_MODIFICATION_DATE LT ls_checkdat.
      lv_fsize = LS_FILEINFO-FILE_SIZE.
      WRITE /: |Deleting | && ls_path-PATHEXTERN && ls_fileinfo-FILE_NAME && | | && ZCL_FILE_UTILS=>FILESIZE_TO_HUMAN_READABLE( IV_SIZE =  lv_fsize ).

*" IF not simulated - delete the file
        IF p_simul NE 'X'.
          ZCL_FILE_UTILS=>DELETE_FILE( EXPORTING IV_FILENAME = ls_path-pathextern && ls_fileinfo-FILE_NAME
                                    EXCEPTIONS ERROR_DELETE_FILE = 1 OTHERS = 2 ).
        ENDIF.

*" IF successfully deleted sum up the file size for stats OTHERWISE give a warning
        IF sy-subrc EQ 0.
          lv_size = lv_size + LS_FILEINFO-FILE_SIZE.
          ADD 1 TO lv_cnt.
        ELSE.
          WRITE /: |    | && ls_path-PATHEXTERN && ls_fileinfo-FILE_NAME && | could not be deleted.|.
        ENDIF.
    ENDIF.
  ENDLOOP.
ENDLOOP.

*" Write statistics
ULINE.
WRITE /: lv_cnt && | Files deleted    | && ZCL_FILE_UTILS=>FILESIZE_TO_HUMAN_READABLE( IV_SIZE = lv_size ) && | saved.|.
