*&---------------------------------------------------------------------*
*&  Include           ZDBTABLE_UPDN_1_CLS
*&---------------------------------------------------------------------*

CLASS lcl_file_utility DEFINITION.

  PUBLIC SECTION.
    CLASS-METHODS itab_2_xlsx
      IMPORTING
        it_fcat         TYPE lvc_t_fcat OPTIONAL
        it_sort         TYPE lvc_t_sort OPTIONAL
        it_filt         TYPE lvc_t_filt OPTIONAL
        is_layout       TYPE lvc_s_layo OPTIONAL
        it_hlink        TYPE lvc_t_hype OPTIONAL
        VALUE(it_data)   TYPE STANDARD TABLE
      RETURNING
        VALUE(r_xstring) TYPE xstring .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS lcl_file_utility IMPLEMENTATION.
  METHOD itab_2_xlsx.
    DATA :
      l_enable_lean_export TYPE abap_bool,
      mt_fcat              TYPE lvc_t_fcat,
      mt_data              TYPE REF TO data,
      m_flavour            TYPE string,
      m_version            TYPE string,
      mo_result_data       TYPE REF TO cl_salv_ex_result_data_table,
      mo_columns           TYPE REF TO cl_salv_columns_table,
      mo_aggreg            TYPE REF TO cl_salv_aggregations,
      mo_salv_table        TYPE REF TO cl_salv_table,
      m_file_type          TYPE salv_bs_constant.

    FIELD-SYMBOLS:
      <tab> TYPE STANDARD TABLE.

    GET REFERENCE OF it_data INTO mt_data.

*if we didn't pass fieldcatalog we need to create it
    IF it_fcat[] IS INITIAL.
      ASSIGN mt_data->* TO <tab>.
      TRY .
          cl_salv_table=>factory(
          EXPORTING
            list_display = abap_false
          IMPORTING
            r_salv_table = mo_salv_table
          CHANGING
            t_table      = <tab> ).
        CATCH cx_salv_msg.

      ENDTRY.
      "get colums & aggregation infor to create fieldcat
      mo_columns  = mo_salv_table->get_columns( ).
      mo_aggreg   = mo_salv_table->get_aggregations( ).
      mt_fcat     =  cl_salv_controller_metadata=>get_lvc_fieldcatalog(
                                    r_columns      = mo_columns
                                    r_aggregations = mo_aggreg ).
    ELSE.
*else we take the one we passed
      mt_fcat[] = it_fcat[].
    ENDIF.

    l_enable_lean_export = cl_alv_z_params=>get_parameter(
                               cl_alv_z_params=>c_flag-use_lean_export ).
    " ALV Lean Export
    IF l_enable_lean_export EQ abap_true.
      cl_salv_bs_lex=>export_from_result_data_table(
        EXPORTING
          is_format            = if_salv_bs_lex_format=>mc_format_xlsx
          ir_result_data_table =  cl_salv_ex_util=>factory_result_data_table(
                                                  r_data                      = mt_data
                                                  s_layout                    = is_layout
                                                  t_fieldcatalog              = mt_fcat
                                                  t_sort                      = it_sort
                                                  t_filter                    = it_filt
                                                  t_hyperlinks                = it_hlink )
        IMPORTING
          er_result_file       = r_xstring ).
    ELSE.

      IF cl_salv_bs_a_xml_base=>get_version( ) EQ if_salv_bs_xml=>version_25 OR
         cl_salv_bs_a_xml_base=>get_version( ) EQ if_salv_bs_xml=>version_26.

        mo_result_data = cl_salv_ex_util=>factory_result_data_table(
            r_data                      = mt_data
            s_layout                    = is_layout
            t_fieldcatalog              = mt_fcat
            t_sort                      = it_sort
            t_filter                    = it_filt
        ).

        CASE cl_salv_bs_a_xml_base=>get_version( ).
          WHEN if_salv_bs_xml=>version_25.
            m_version = if_salv_bs_xml=>version_25.
          WHEN if_salv_bs_xml=>version_26.
            m_version = if_salv_bs_xml=>version_26.
        ENDCASE.

        "if we flag i_XLSX then we'll create XLSX if not then MHTML excel file
*    if i_xlsx is not initial.
        m_file_type = if_salv_bs_xml=>c_type_xlsx.
*    else.
*      m_file_type = if_salv_bs_xml=>c_type_mhtml.
*    endif.


        m_flavour = if_salv_bs_c_tt=>c_tt_xml_flavour_export.
        "transformation of data to excel
        CALL METHOD cl_salv_bs_tt_util=>if_salv_bs_tt_util~transform
          EXPORTING
            xml_type      = m_file_type
            xml_version   = m_version
            r_result_data = mo_result_data
            xml_flavour   = m_flavour
            gui_type      = if_salv_bs_xml=>c_gui_type_gui
          IMPORTING
            xml           = r_xstring.
      ENDIF.
    ENDIF.
  ENDMETHOD.
ENDCLASS.
