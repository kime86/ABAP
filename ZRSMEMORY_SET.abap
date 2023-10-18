*&---------------------------------------------------------------------*
*& Report ZRSMEMORY_SET
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT ZRSMEMORY_SET.

  DATA: v_initialized TYPE c VALUE ''.

  DATA: a TYPE i, b TYPE i, c TYPE i, d TYPE i, e TYPE i, f TYPE i,
      g TYPE i, h TYPE i.
  DATA: aa TYPE i, bb TYPE i, cc TYPE i, dd TYPE i, ee TYPE i, ff TYPE i,
        gg TYPE i, hh TYPE i.

  DATA: f0 TYPE p, f1 TYPE p, f2 TYPE p, f3 TYPE p, f4 TYPE p, f5 TYPE p,
        f6 TYPE p, f7 TYPE p.

  DATA: bf0 TYPE p, bf1 TYPE p, bf2 TYPE p, bf3 TYPE p,
        bf4 TYPE p, bf5 TYPE p, bf6 TYPE p, bf7 TYPE p.

  DATA: fcode   LIKE sy-ucomm,
        init,
        timeout TYPE i, logsize TYPE i,
        emtotal TYPE p, heapdia TYPE p, heapbtc TYPE p.

  SELECTION-SCREEN BEGIN OF BLOCK blk1 WITH FRAME TITLE t_blk1 NO INTERVALS.
    SELECTION-SCREEN COMMENT /1(10) c_hd1.
    SELECTION-SCREEN COMMENT 12(15) c_hd2.
    SELECTION-SCREEN COMMENT 40(15) c_hd3.
    SELECTION-SCREEN ULINE.

    SELECTION-SCREEN BEGIN OF LINE.
      SELECTION-SCREEN COMMENT 3(15) c_a FOR FIELD p_a.
      PARAMETERS: p_a(1) TYPE n.
      SELECTION-SCREEN POSITION 40.
      PARAMETERS: p_0(15) TYPE n.
    SELECTION-SCREEN END OF LINE.
    SELECTION-SCREEN BEGIN OF LINE.
      SELECTION-SCREEN COMMENT 3(15) c_b FOR FIELD p_b.
      PARAMETERS: p_b(1) TYPE n.
      SELECTION-SCREEN POSITION 40.
      PARAMETERS: p_1(15) TYPE n.
    SELECTION-SCREEN END OF LINE.
    SELECTION-SCREEN BEGIN OF LINE.
      SELECTION-SCREEN COMMENT 3(15) c_c FOR FIELD p_c.
      PARAMETERS: p_c(1) TYPE n.
      SELECTION-SCREEN POSITION 40.
      PARAMETERS: p_2(15) TYPE n.
    SELECTION-SCREEN END OF LINE.
    SELECTION-SCREEN BEGIN OF LINE.
      SELECTION-SCREEN COMMENT 3(15) c_d FOR FIELD p_d.
      PARAMETERS: p_d(1) TYPE n.
      SELECTION-SCREEN POSITION 40.
      PARAMETERS: p_3(15) TYPE n.
    SELECTION-SCREEN END OF LINE.
    SELECTION-SCREEN BEGIN OF LINE.
      SELECTION-SCREEN COMMENT 3(15) c_e FOR FIELD p_e.
      PARAMETERS: p_e(1) TYPE n.
      SELECTION-SCREEN POSITION 40.
      PARAMETERS: p_4(15) TYPE n.
    SELECTION-SCREEN END OF LINE.
  SELECTION-SCREEN END OF BLOCK blk1.

  SELECTION-SCREEN COMMENT /1(30) c_hd4.
  SELECTION-SCREEN BEGIN OF BLOCK blk2 WITH FRAME TITLE t_blk2 NO INTERVALS.
    SELECTION-SCREEN BEGIN OF LINE.
      SELECTION-SCREEN COMMENT 3(15) c_aa FOR FIELD p_aa.
      PARAMETERS: p_aa(1) TYPE n.
      SELECTION-SCREEN POSITION 40.
      PARAMETERS: p_00(15) TYPE n.
    SELECTION-SCREEN END OF LINE.
    SELECTION-SCREEN BEGIN OF LINE.
      SELECTION-SCREEN COMMENT 3(15) c_bb FOR FIELD p_bb.
      PARAMETERS: p_bb(1) TYPE n.
      SELECTION-SCREEN POSITION 40.
      PARAMETERS: p_11(15) TYPE n.
    SELECTION-SCREEN END OF LINE.
    SELECTION-SCREEN BEGIN OF LINE.
      SELECTION-SCREEN COMMENT 3(15) c_cc FOR FIELD p_cc.
      PARAMETERS: p_cc(1) TYPE n.
      SELECTION-SCREEN POSITION 40.
      PARAMETERS: p_22(15) TYPE n.
    SELECTION-SCREEN END OF LINE.
    SELECTION-SCREEN BEGIN OF LINE.
      SELECTION-SCREEN COMMENT 3(15) c_dd FOR FIELD p_dd.
      PARAMETERS: p_dd(1) TYPE n.
      SELECTION-SCREEN POSITION 40.
      PARAMETERS: p_33(15) TYPE n.
    SELECTION-SCREEN END OF LINE.
    SELECTION-SCREEN BEGIN OF LINE.
      SELECTION-SCREEN COMMENT 3(15) c_ee FOR FIELD p_ee.
      PARAMETERS: p_ee(1) TYPE n.
      SELECTION-SCREEN POSITION 40.
      PARAMETERS: p_44(15) TYPE n.
    SELECTION-SCREEN END OF LINE.
  SELECTION-SCREEN END OF BLOCK blk2.

  SELECTION-SCREEN COMMENT /1(30) c_hd5.
  SELECTION-SCREEN BEGIN OF BLOCK blk3 WITH FRAME TITLE t_blk3 NO INTERVALS.
    SELECTION-SCREEN BEGIN OF LINE.
      SELECTION-SCREEN COMMENT (25) c_hdia FOR FIELD p_hdia.
      PARAMETERS: p_hdia(15) TYPE n.
    SELECTION-SCREEN END OF LINE.
    SELECTION-SCREEN BEGIN OF LINE.
      SELECTION-SCREEN COMMENT (25) c_ndia FOR FIELD p_ndia.
      PARAMETERS: p_ndia(15) TYPE n.
    SELECTION-SCREEN END OF LINE.
    SELECTION-SCREEN BEGIN OF LINE.
      SELECTION-SCREEN COMMENT (25) c_htot FOR FIELD p_htot.
      PARAMETERS: p_htot(15) TYPE n.
    SELECTION-SCREEN END OF LINE.
    SELECTION-SCREEN BEGIN OF LINE.
      SELECTION-SCREEN COMMENT (25) c_logt FOR FIELD p_logt.
      PARAMETERS: p_logt(8) TYPE n.
    SELECTION-SCREEN END OF LINE.
    SELECTION-SCREEN BEGIN OF LINE.
      SELECTION-SCREEN COMMENT (25) c_logs FOR FIELD p_logs.
      PARAMETERS: p_logs(8) TYPE n.
    SELECTION-SCREEN END OF LINE.
  SELECTION-SCREEN END OF BLOCK blk3.

FORM set_get USING mode.
  IF mode = ' ' OR mode = 'X'.
                                                          "#EC CI_CCALL
    CALL 'STORAGE_SET'                                    "#EC CI_CCALL
       ID 'MODE' FIELD mode                               "#EC CI_CCALL
       ID 'A' FIELD a                                     "#EC CI_CCALL
       ID 'B' FIELD b                                     "#EC CI_CCALL
       ID 'C' FIELD c                                     "#EC CI_CCALL
       ID 'D' FIELD d                                     "#EC CI_CCALL
       ID 'E' FIELD e                                     "#EC CI_CCALL
       ID 'F' FIELD f                                     "#EC CI_CCALL
       ID 'G' FIELD g                                     "#EC CI_CCALL
       ID 'H' FIELD h                                     "#EC CI_CCALL
       ID '0' FIELD f0                                    "#EC CI_CCALL
       ID '1' FIELD f1                                    "#EC CI_CCALL
       ID '2' FIELD f2                                    "#EC CI_CCALL
       ID '3' FIELD f3                                    "#EC CI_CCALL
       ID '4' FIELD f4                                    "#EC CI_CCALL
       ID '5' FIELD f5                                    "#EC CI_CCALL
       ID '6' FIELD f6                                    "#EC CI_CCALL
       ID '7' FIELD f7                                    "#EC CI_CCALL
       ID 'HEAPTOTAL' FIELD emtotal                       "#EC CI_CCALL
       ID 'HEAPDIA' FIELD heapdia                         "#EC CI_CCALL
       ID 'HEAPBTC' FIELD heapbtc                         "#EC CI_CCALL
       ID 'LOGSIZE' FIELD logsize                         "#EC CI_CCALL
       ID 'LOGTIME' FIELD timeout.                        "#EC CI_CCALL
  ENDIF.
  IF mode = 'B' OR mode = 'Y'.
                                                          "#EC CI_CCALL
    CALL 'STORAGE_SET'                                    "#EC CI_CCALL
       ID 'MODE' FIELD mode                               "#EC CI_CCALL
       ID 'A' FIELD aa                                    "#EC CI_CCALL
       ID 'B' FIELD bb                                    "#EC CI_CCALL
       ID 'C' FIELD cc                                    "#EC CI_CCALL
       ID 'D' FIELD dd                                    "#EC CI_CCALL
       ID 'E' FIELD ee                                    "#EC CI_CCALL
       ID 'F' FIELD ff                                    "#EC CI_CCALL
       ID 'G' FIELD gg                                    "#EC CI_CCALL
       ID 'H' FIELD hh                                    "#EC CI_CCALL
       ID '0' FIELD bf0                                   "#EC CI_CCALL
       ID '1' FIELD bf1                                   "#EC CI_CCALL
       ID '2' FIELD bf2                                   "#EC CI_CCALL
       ID '3' FIELD bf3                                   "#EC CI_CCALL
       ID '4' FIELD bf4                                   "#EC CI_CCALL
       ID '5' FIELD bf5                                   "#EC CI_CCALL
       ID '6' FIELD bf6                                   "#EC CI_CCALL
       ID '7' FIELD bf7                                   "#EC CI_CCALL
       ID 'HEAPTOTAL' FIELD emtotal                       "#EC CI_CCALL
       ID 'HEAPDIA' FIELD heapdia                         "#EC CI_CCALL
       ID 'HEAPBTC' FIELD heapbtc                         "#EC CI_CCALL
       ID 'LOGSIZE' FIELD logsize                         "#EC CI_CCALL
       ID 'LOGTIME' FIELD timeout.                        "#EC CI_CCALL
  ENDIF.
ENDFORM.

AT SELECTION-SCREEN OUTPUT.
  IF V_INITIALIZED IS INITIAL.
    PERFORM set_get USING space.
    PERFORM set_get USING 'B'.

    P_A = a.
    P_B = b.
    P_C = c.
    P_D = d.
    P_E = e.
    P_0 = f0.
    P_1 = f1.
    P_2 = f2.
    P_3 = f3.
    P_4 = f4.
    P_AA = aa.
    P_BB = bb.
    P_CC = cc.
    P_DD = dd.
    P_EE = ee.
    P_00 = bf0.
    P_11 = bf1.
    P_22 = bf2.
    P_33 = bf3.
    P_44 = bf4.
    P_HDIA = heapdia.
    P_NDIA = heapbtc.
    P_HTOT = emtotal.
    P_LOGT = timeout.
    P_LOGS = logsize.

    v_initialized = 'X'.
  ENDIF.
INITIALIZATION.
  t_blk1 = 'Quota Dialog'.
  t_blk2 = 'Quota Batch/VB/Spool'.
  t_blk3 = 'Sonstige Parameter'.
  c_hd1 = 'Schritt'.
  c_hd2 = 'Speicherklasse'.
  c_hd3 = 'Göße [Bytes]'.
  c_a = '1'.
  c_b = '2'.
  c_c = '3'.
  c_d = '4'.
  c_e = '5'.
  c_hd4 = 'Speicherklassen: EM(1) HEAP(2)'.
  c_aa = '1'.
  c_bb = '2'.
  c_cc = '3'.
  c_dd = '4'.
  c_ee = '5'.
  c_hd5 = 'Speicherklassen: EM(1) HEAP(2)'.
  c_hdia = 'abap/heap area dia:'.
  c_ndia = 'abap/heap area nondia:'.
  c_htot = 'abap/heap area total:'.
  c_logt = 'em/stat log timeout:'.
  c_logs = 'em/stat log size MB:'.


START-OF-SELECTION.
    a = P_A.
    b = P_B.
    c = P_C.
    d = P_D.
    e = P_E.
    f0 = P_0.
    f1 = P_1.
    f2 = P_2.
    f3 = P_3.
    f4 = P_4.
    aa = P_AA.
    bb = P_BB.
    cc = P_CC.
    dd = P_DD.
    ee = P_EE.
    bf0 = P_00.
    bf1 = P_11.
    bf2 = P_22.
    bf3 = P_33.
    bf4 = P_44.
    heapdia = P_HDIA.
    heapbtc = P_NDIA.
    emtotal = p_htot.
    timeout = p_logt.
    logsize = p_logs.

    PERFORM set_get USING 'X'.
    PERFORM set_get USING 'Y'.
