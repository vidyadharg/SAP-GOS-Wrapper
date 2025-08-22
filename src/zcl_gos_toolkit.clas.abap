CLASS zcl_gos_toolkit DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    CLASS-METHODS:
      class_constructor,
      remove_text_in_string
        IMPORTING iv_string     TYPE string
                  iv_remove     TYPE csequence
        RETURNING VALUE(result) TYPE string,

      convert_xtab_to_xtab
        IMPORTING
          im_xtab        TYPE STANDARD TABLE
        EXPORTING
          VALUE(et_xtab) TYPE STANDARD TABLE,

      convert_ctab_to_ctab
        IMPORTING
          im_ctab TYPE STANDARD TABLE
        EXPORTING
          et_ctab TYPE STANDARD TABLE,

      split_filename
        IMPORTING
          iv_filename  TYPE clike
        EXPORTING
          ev_name      TYPE clike
          ev_extension TYPE clike,

      get_mimetype
        IMPORTING
          !i_extension      TYPE clike
        RETURNING
          VALUE(r_mimetype) TYPE mimetypes-type,
      get_gos_relations
        IMPORTING content_key TYPE SIBFLPORB
        RETURNING VALUE(rt_content_key) TYPE sibflporbt.

  PRIVATE SECTION.
    CLASS-DATA:
      gt_mimetype TYPE STANDARD TABLE OF mimetypes.

ENDCLASS.


CLASS zcl_gos_toolkit IMPLEMENTATION.
  METHOD get_gos_relations.
    DATA:
      lt_lporb TYPE sibflporbt.

    data(lo_badi) = NEW cl_ex_gos_mult_publish( ).

    APPEND content_key TO lt_lporb.

    lo_badi->if_ex_gos_mult_publish~add_objects(
      EXPORTING
        flt_val  = 'VIEW_ATTA'
      CHANGING
        ct_lporb = lt_lporb ).

    DELETE ADJACENT DUPLICATES FROM lt_lporb COMPARING ALL FIELDS.

    rt_content_key = lt_lporb.

  ENDMETHOD.

  METHOD remove_text_in_string.
    result = replace( val  = iv_string
                      sub  = iv_remove
                      with = `` ).
  ENDMETHOD.

  METHOD convert_ctab_to_ctab.
    DATA:
      lv_line_content TYPE string.

    LOOP AT im_ctab ASSIGNING FIELD-SYMBOL(<im_ctab>).
      DO.
        ASSIGN COMPONENT sy-index OF STRUCTURE <im_ctab> TO FIELD-SYMBOL(<fs>).
        IF sy-subrc <> 0.
          EXIT.
        ENDIF.
        lv_line_content = |{ lv_line_content }{ <fs> }|.
      ENDDO.
    ENDLOOP.

    et_ctab = cl_bcs_convert=>string_to_soli( lv_line_content ).

  ENDMETHOD.

  METHOD convert_xtab_to_xtab.
    cl_bcs_convert=>xstring_to_xtab(
      EXPORTING iv_xstring = cl_bcs_convert=>xtab_to_xstring( im_xtab )
      IMPORTING et_xtab = et_xtab ).

  ENDMETHOD.

  METHOD get_mimetype.

    ASSIGN gt_mimetype[ extension = i_extension ] TO FIELD-SYMBOL(<fs_mimetype>).
    IF sy-subrc <> 0.

      APPEND INITIAL LINE TO gt_mimetype ASSIGNING <fs_mimetype>.
      <fs_mimetype>-extension = i_extension.

      CALL FUNCTION 'SDOK_MIMETYPE_GET'
        EXPORTING
          extension = i_extension
        IMPORTING
          mimetype  = <fs_mimetype>-type.
    ENDIF.

    r_mimetype = <fs_mimetype>-type.

  ENDMETHOD.

  METHOD split_filename.
    TYPES:
      tt_string
          TYPE STANDARD TABLE OF string
          WITH DEFAULT KEY .

    DATA:
      lt_split TYPE tt_string.

    CLEAR:
      ev_name,
      ev_extension.

    SPLIT iv_filename AT '.' INTO TABLE lt_split.

    LOOP AT lt_split ASSIGNING FIELD-SYMBOL(<lv_split>).

      IF sy-tabix EQ lines( lt_split ).
        ev_extension = <lv_split>.
      ELSE.
        ev_name =
          |{ ev_name }| &&
          |{ COND #( WHEN ev_name IS NOT INITIAL THEN '.' ) }| &&
          |{ <lv_split> }|
        .
      ENDIF.

    ENDLOOP.

    IF ev_name IS INITIAL.
      ev_name = iv_filename.
      ev_extension = space.
    ENDIF.

    TRANSLATE ev_extension TO UPPER CASE.

  ENDMETHOD.

  METHOD class_constructor.

  ENDMETHOD.

ENDCLASS.
