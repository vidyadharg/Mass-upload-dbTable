*&---------------------------------------------------------------------*
*& Report ZDBTABLE_UPDN
*& Program to mass upload/download data in the database table.
*&---------------------------------------------------------------------*
REPORT zdbtable_updn.
TABLES: sscrfields.


CONSTANTS :
  BEGIN OF gc,
    sflight TYPE rsrd1-tbma_val VALUE 'SFLIGHT',
    sbook   TYPE rsrd1-tbma_val VALUE 'SBOOK',
  END OF gc.

DATA :
  doc_container TYPE REF TO cl_gui_docking_container.

FIELD-SYMBOLS:
  <ft_data> TYPE STANDARD TABLE.

*— Declaration for factory ALV
DATA: salv TYPE REF TO cl_salv_table.

*— Selection Screen
PARAMETERS: ptable TYPE rsrd1-tbma_val AS LISTBOX VISIBLE LENGTH 20
                 OBLIGATORY DEFAULT gc-sflight USER-COMMAND ptab.
SELECTION-SCREEN FUNCTION KEY 4.
SELECTION-SCREEN FUNCTION KEY 5.

INITIALIZATION.
  PERFORM f_init.

*— Validate table name
AT SELECTION-SCREEN ON ptable.
  PERFORM f_validate_table.

AT SELECTION-SCREEN.

  CASE sscrfields-ucomm.
    WHEN 'FC05'.
      PERFORM f_read_excel_update_tab.
    WHEN 'FC04'.
      PERFORM f_export_to_excel.
    WHEN 'PTAB'.
      PERFORM f_build_container.
  ENDCASE.

FORM f_init.
  DATA : li_list    TYPE vrm_values,
         lt_exclude TYPE TABLE OF rsexfcode.

  lt_exclude = VALUE #(
    ( fcode = 'PRIN' )  "Execute and Print.
    ( fcode = 'ONLI' )  "Execute.
    ( fcode = 'SJOB' )  "Execute in Background
    ( fcode = 'VDEL' )  "Variant Delete
    ( fcode = 'SPOS' )  "Variant Save
    ( fcode = 'GET'  )   "Get...
    ( fcode = 'VSHO' )  "Display...
    ( fcode = 'VDEL' )  "Delete...
    ( fcode = 'SPOS' )  "Save as Variant...
    ( fcode = 'LVUV' ) ). "User Variables...

  CALL FUNCTION 'RS_SET_SELSCREEN_STATUS'
    EXPORTING
      p_status  = sy-pfkey
    TABLES
      p_exclude = lt_exclude.

  DATA functxt TYPE smp_dyntxt.

  functxt-icon_id = icon_import.
  functxt-quickinfo = 'Import from file'.
  functxt-icon_text = 'Import from file'.
  sscrfields-functxt_05 = functxt.

  functxt-icon_id = icon_export.
  functxt-quickinfo = 'Export to file'.
  functxt-icon_text = 'Export to file'.
  sscrfields-functxt_04 = functxt.

*DB table list
  li_list = VALUE #(
       ( key = gc-sflight )
       ( key = gc-sbook ) ).

  CALL FUNCTION 'VRM_SET_VALUES'
    EXPORTING
      id              = 'PTABLE'
      values          = li_list
    EXCEPTIONS
      id_illegal_name = 1
      OTHERS          = 2.

  ptable = gc-sflight.


  PERFORM f_build_container.

ENDFORM.

FORM f_read_excel_update_tab .
  DATA :
    lt_raw_data TYPE truxs_t_text_data,
    rc          TYPE i,
    lv_filename TYPE  rlgrap-filename,
    lt_files    TYPE filetable,
    answer      TYPE c.

  DATA lt_upload TYPE REF TO data.

  FIELD-SYMBOLS:
    <ft_upload> TYPE STANDARD TABLE.


  cl_gui_frontend_services=>file_open_dialog(
     EXPORTING
       window_title = 'Select File'
       file_filter  = 'XLS'
     CHANGING
       file_table   = lt_files
       rc           = rc
     EXCEPTIONS
       OTHERS       = 4 ).

  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
               WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ELSE.
    PERFORM f_confirm CHANGING answer.
    IF answer EQ '1'.

      DATA(ls_path) = VALUE #( lt_files[ 1 ] DEFAULT space ).
      IF ls_path IS NOT INITIAL.
        lv_filename =  ls_path-filename.

*— Create dynamic internal table
        CREATE DATA lt_upload TYPE TABLE OF (ptable).
        ASSIGN lt_upload->* TO <ft_upload>.

        CALL FUNCTION 'TEXT_CONVERT_XLS_TO_SAP'
          EXPORTING
*           I_FIELD_SEPERATOR    =
            i_line_header        = 'X'
            i_tab_raw_data       = lt_raw_data
            i_filename           = lv_filename
          TABLES
            i_tab_converted_data = <ft_upload>
          EXCEPTIONS
            conversion_failed    = 1
            OTHERS               = 2.

        IF sy-subrc <> 0.
          MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
          WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
        ELSE.
          PERFORM f_upload_table TABLES <ft_upload>.
        ENDIF.
      ENDIF.
    ELSE.
      MESSAGE s056(fc). STOP.
    ENDIF.
  ENDIF.
ENDFORM.

FORM f_upload_table
  TABLES pt_upload.
  FIELD-SYMBOLS <fs_field> TYPE any.

  DATA tabname TYPE  rstable-tabname .
  tabname = ptable.

  DEFINE set_field .
    ASSIGN COMPONENT &1 OF STRUCTURE <fs_upload> TO <fs_field>.
    IF sy-subrc EQ 0.
      <fs_field> = &2.
    ENDIF.
  END-OF-DEFINITION.

  IF pt_upload[] IS NOT INITIAL.
    LOOP AT pt_upload ASSIGNING FIELD-SYMBOL(<fs_upload>).
*validate each record
      ASSIGN COMPONENT 'MANDT' OF STRUCTURE <fs_upload> TO <fs_field>.
      IF sy-subrc EQ 0.
        <fs_field> = sy-mandt.
      ENDIF.
      CASE ptable.
        WHEN gc-sflight. "validate, set defaults
*        WHEN gc-ZTABLE.
*          set_field 'CREABY' sy-uname.
*          set_field 'CRDATE' sy-datum.
*          set_field 'CRTIME' sy-uzeit.
*          set_field 'CRTCODE' sy-tcode.
*          set_field 'CHNGBY' sy-uname.
*          set_field 'CHNGDATE' sy-datum.
*          set_field 'CHNGTIME' sy-uzeit.
*          set_field 'CHNGTCODE' sy-tcode.
*        	WHEN OTHERS.
      ENDCASE.

    ENDLOOP.

    SET UPDATE TASK LOCAL.   " switch to local update
*      Lock the table
    CALL FUNCTION 'ENQUEUE_E_TABLE'
      EXPORTING
        mode_rstable   = 'E'
        tabname        = tabname
      EXCEPTIONS
        foreign_lock   = 1
        system_failure = 2
        OTHERS         = 3.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                          WITH sy-msgv1 sy-msgv2 sy-msgv3.
    ELSE.
      MODIFY (ptable) FROM TABLE pt_upload.
    ENDIF.

*  Unlock the table
    CALL FUNCTION 'DEQUEUE_E_TABLE'
      EXPORTING
        mode_rstable = 'E'
        tabname      = tabname.

    MESSAGE |Table | & |{ ptable }| & | Updated Succesfully| TYPE 'S'.

    PERFORM f_build_container.
  ENDIF.
ENDFORM.

FORM f_confirm CHANGING antwort TYPE c.
  DATA :
    titlebar TYPE string,
    textline TYPE string.

* Confirm the intent
  textline = |You are importing records in table | && |{ ptable }| && |, Please Confirm.|.
  titlebar = 'Import records'.

  CALL FUNCTION 'POPUP_TO_CONFIRM'
    EXPORTING
      titlebar              = titlebar
*     DIAGNOSE_OBJECT       = ' '
      text_question         = textline
      text_button_1         = 'Continue'(016)
      icon_button_1         = 'ICON_OKAY'
      text_button_2         = 'Stop'(017)
      icon_button_2         = 'ICON_MESSAGE_CRITICAL'
      default_button        = '2'
      display_cancel_button = ''
*     USERDEFINED_F1_HELP   = ' '
*     START_COLUMN          = 25
*     START_ROW             = 6
*     POPUP_TYPE            =
*     IV_QUICKINFO_BUTTON_1 = ' '
*     IV_QUICKINFO_BUTTON_2 = ' '
    IMPORTING
      answer                = antwort
* TABLES
*     PARAMETER             =
    EXCEPTIONS
      text_not_found        = 1
      OTHERS                = 2.
*'1' left pushbutton
*'2' next pushbutton
*'A' 'Cancel' pushbutton
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH sy-msgv1.
  ELSEIF antwort EQ '2'.
  ENDIF.
ENDFORM.

FORM f_build_container.

*— Declarations for dynamic data
  DATA gt_data TYPE REF TO data.

  CLEAR: salv.

*— Create dynamic internal table
  CREATE DATA gt_data TYPE TABLE OF (ptable).
  ASSIGN gt_data->* TO <ft_data>.

  SELECT *
    FROM (ptable) INTO TABLE <ft_data>.

  IF doc_container IS NOT INITIAL.
    doc_container->free(
      EXCEPTIONS
        cntl_error        = 1                " CNTL_ERROR
        cntl_system_error = 2                " CNTL_SYSTEM_ERROR
        OTHERS            = 3 ).
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
        WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.
  ENDIF.

  CREATE OBJECT doc_container
    EXPORTING
      repid = sy-repid
      dynnr = sy-dynnr
      side  = doc_container->dock_at_bottom
*     extension = 325
      ratio = 93.

  IF salv IS INITIAL.
    TRY.
*— Create Instance
        CALL METHOD cl_salv_table=>factory
          EXPORTING
            r_container    = doc_container
            container_name = 'CONTAINER'
          IMPORTING
            r_salv_table   = salv
          CHANGING
            t_table        = <ft_data>.
      CATCH cx_salv_msg.                                "#EC NO_HANDLER
    ENDTRY.

    salv->get_display_settings( )->set_list_header( |{ lines( <ft_data> ) }| & | Records from table | & |{ ptable }| ).
    salv->get_columns( )->set_optimize( abap_true ).
    salv->get_selections( )->set_selection_mode( if_salv_c_selection_mode=>row_column ). "Allow single row Selection"

*— Display ALV \Output
    salv->display( ).
  ENDIF.
ENDFORM.

FORM f_export_to_excel.
  DATA:
    filename          TYPE string,
    lt_binary_content TYPE solix_tab,
    lv_attachment_x   TYPE xstring.

  IF <ft_data> IS NOT INITIAL.
    PERFORM pickup_path_file CHANGING filename.
    IF filename IS NOT INITIAL.
      filename = filename && '\' && ptable && sy-sysid && sy-datum && '.xls'.

      lv_attachment_x =   zcl_file_utility=>itab_2_xlsx( <ft_data> ).

      cl_bcs_convert=>xstring_to_solix(
        EXPORTING iv_xstring = lv_attachment_x
        RECEIVING et_solix   = lt_binary_content ).


* Get trailing blank
      cl_gui_frontend_services=>gui_download( EXPORTING bin_filesize = xstrlen( lv_attachment_x )
                                                        filename     = filename
                                                        filetype     = 'BIN'
                                               CHANGING data_tab     = lt_binary_content ).
    ENDIF.
  ENDIF.

ENDFORM.


FORM pickup_path_file CHANGING filepath.
  cl_gui_frontend_services=>directory_browse(
     EXPORTING window_title    = 'Browse Path to download'
               initial_folder = 'C:\temp1'
     CHANGING selected_folder = filepath ).

ENDFORM.

FORM f_validate_table.
*— Upload only Tables in customer namespace
*  IF ptable+0(1) NE 'Z' AND ptable+0(1) NE 'Y'.
*    MESSAGE 'Only tables in customer namespace can be uploaded.' TYPE 'E'.
*  ENDIF.

*— Only transparent tables can be uploaded
  SELECT SINGLE tabname
  FROM dd02l INTO @DATA(lv_tabname)
  WHERE tabname = @ptable AND
        tabclass = 'TRANSP'.
  IF sy-subrc NE 0.
    MESSAGE 'Only transparent tables can be uploaded.' TYPE 'E'.
  ENDIF.
ENDFORM.
