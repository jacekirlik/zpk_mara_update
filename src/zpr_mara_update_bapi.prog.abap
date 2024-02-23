*&---------------------------------------------------------------------*
*& Report ZPR_MARA_UPDATE_BAPI
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zpr_mara_update_bapi.

TYPES:
  BEGIN OF ts_importfile,
    matnr TYPE mara-matnr,
    matkl TYPE mara-matkl,
  END OF ts_importfile.

DATA: gt_importfile TYPE TABLE OF ts_importfile WITH DEFAULT KEY,
      gt_dbmara     TYPE TABLE OF ts_importfile WITH DEFAULT KEY,
      gt_matgrp     TYPE TABLE OF ts_importfile WITH DEFAULT KEY.

DATA: gt_files  TYPE filetable,
      gv_rc     TYPE i,
      gv_action TYPE i.

* Structures for Bapi
DATA: gs_headdata    LIKE bapimathead,
      gs_clientdata  LIKE bapi_mara,
      gs_clientdatax LIKE bapi_marax,
      gs_return      LIKE bapiret2.

DATA: gv_countok  TYPE i,
      gv_countnok TYPE i,
      gv_countch  TYPE i.

SELECTION-SCREEN BEGIN OF BLOCK import_options WITH FRAME TITLE TEXT-001.
  SELECTION-SCREEN SKIP.
  SELECTION-SCREEN PUSHBUTTON /1(20) TEXT-002 USER-COMMAND but1.
  PARAMETERS: p_header AS CHECKBOX DEFAULT 'X'.
SELECTION-SCREEN END OF BLOCK import_options.

PARAMETERS: p_file   TYPE string.
*&---------------------------------------------------------------------*

AT SELECTION-SCREEN OUTPUT.
  LOOP AT SCREEN.
    IF screen-name = 'P_FILE'.
      screen-input = 0.
      MODIFY SCREEN.
    ENDIF.
  ENDLOOP.
*&---------------------------------------------------------------------*

AT SELECTION-SCREEN.
  IF sy-ucomm = 'BUT1'.
    CALL METHOD cl_gui_frontend_services=>file_open_dialog
      EXPORTING
        file_filter             = |csv (*.csv)\|*.csv|       " File Extension Filter String
        multiselection          = abap_false                 " Multiple selections poss.
      CHANGING
        file_table              = gt_files                   " Table Holding Selected Files
        rc                      = gv_rc                      " Return Code, Number of Files or -1 If Error Occurred
        user_action             = gv_action                  " User Action (See Class Constants ACTION_OK, ACTION_CANCEL)
      EXCEPTIONS
        file_open_dialog_failed = 1                " "Open File" dialog failed
        cntl_error              = 2                " Control error
        error_no_gui            = 3                " No GUI available
        not_supported_by_gui    = 4                " GUI does not support this
        OTHERS                  = 5.
    IF sy-subrc <> 0.
      MESSAGE 'Incorrect parameter: File Name' TYPE 'I'.
    ENDIF.

    IF gv_action = cl_gui_frontend_services=>action_ok.
      IF lines( gt_files ) = 1.
        p_file = gt_files[ 1 ]-filename.
      ENDIF.
    ENDIF.


    CALL METHOD cl_gui_frontend_services=>gui_upload
      EXPORTING
        filename                = p_file            " Name of file
        filetype                = 'ASC'             " File Type (ASCII, Binary)
      CHANGING
        data_tab                = gt_importfile     " Transfer table for file contents
      EXCEPTIONS
        file_open_error         = 1                 " File does not exist and cannot be opened
        file_read_error         = 2                 " Error when reading file
        no_batch                = 3                 " Front-End Function Cannot Be Executed in Backgrnd
        gui_refuse_filetransfer = 4                 " Incorrect front end or error on front end
        invalid_type            = 5                 " Incorrect parameter FILETYPE
        no_authority            = 6                 " No Upload Authorization
        unknown_error           = 7                 " Unknown error
        bad_data_format         = 8                 " Cannot Interpret Data in File
        header_not_allowed      = 9                 " Invalid header
        separator_not_allowed   = 10                " Invalid separator
        header_too_long         = 11                " Header information currently restricted to 1023 bytes
        unknown_dp_error        = 12                " Error when calling data provider
        access_denied           = 13                " Access to File Denied
        dp_out_of_memory        = 14                " Not Enough Memory in DataProvider
        disk_full               = 15                " Storage Medium full
        dp_timeout              = 16                " Timeout of DataProvider
        not_supported_by_gui    = 17                " GUI does not support this
        error_no_gui            = 18                " GUI not available
        OTHERS                  = 19.
    IF sy-subrc <> 0.
      MESSAGE 'Incorrect parameter: File Name' TYPE 'I'.
    ENDIF.
  ENDIF.
*&---------------------------------------------------------------------*



START-OF-SELECTION.
  IF p_file IS INITIAL.
    MESSAGE 'Incorrect parameter: File Name' TYPE 'I'.
  ELSE.

    DATA(lv_start_line) = COND #( WHEN p_header = abap_true THEN 2 ELSE 1 ).

    LOOP AT gt_importfile ASSIGNING FIELD-SYMBOL(<fs_importfile>) FROM lv_start_line.
      SPLIT <fs_importfile> AT ';' INTO TABLE DATA(lt_columns).
      <fs_importfile>-matnr = lt_columns[ 1 ].
      <fs_importfile>-matkl = lt_columns[ 2 ].
      APPEND <fs_importfile> TO gt_matgrp.
    ENDLOOP.

    SELECT mara~matnr, mara~matkl
      FROM mara
      FOR ALL ENTRIES IN @gt_matgrp
      WHERE mara~matnr = @gt_matgrp-matnr
      INTO TABLE @gt_dbmara.

    LOOP AT gt_matgrp ASSIGNING FIELD-SYMBOL(<fs_matgrp>).
      READ TABLE gt_dbmara WITH
        TABLE KEY matnr = <fs_matgrp>-matnr
                  matkl = <fs_matgrp>-matkl
      ASSIGNING FIELD-SYMBOL(<fs_dbmara>).

      IF sy-subrc = 0.
        IF <fs_matgrp>-matnr = <fs_dbmara>-matnr AND <fs_matgrp>-matkl = <fs_dbmara>-matkl.
          gv_countok = gv_countok + 1.
        ENDIF.
      ELSE.

* Update fields
        gs_headdata-material = <fs_matgrp>-matnr.
        gs_headdata-basic_view  = 'X'.
        gs_clientdata-matl_group = <fs_matgrp>-matkl.
        gs_clientdatax-matl_group = 'X'.

* Call the BAPI
        CALL FUNCTION 'BAPI_MATERIAL_SAVEDATA'
          EXPORTING
            headdata    = gs_headdata
            clientdata  = gs_clientdata
            clientdatax = gs_clientdatax
          IMPORTING
            return      = gs_return.

* Commit
        CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'.

* RETURN-TYPE is 'E' in case of error, else 'S'.
        IF gs_return-type = 'E'.
          WRITE: / 'Processing error for MATRN:', gs_headdata-material COLOR = 6.
          gv_countnok = gv_countnok + 1.
        ELSEIF gs_return-type = 'S'.
          WRITE: / 'MAKTL:', gs_clientdata-matl_group, 'has been updated for MATNR:', gs_headdata-material COLOR = 5.
          gv_countch = gv_countch + 1.
        ENDIF.
      ENDIF.
    ENDLOOP.

    WRITE:  / ,
            / 'Count of materials with changed MAKTL:', gv_countch,
            / 'Count of materials with correct MAKTL:', gv_countok,
            / 'Count of materials with error process:', gv_countnok.

    CLEAR: p_file, gt_importfile, gt_matgrp, gt_dbmara.
  ENDIF.
*&---------------------------------------------------------------------*
