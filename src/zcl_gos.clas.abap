CLASS zcl_gos DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES zif_gos.

    TYPES:
      BEGIN OF t_url,
        address     TYPE string,
        description TYPE string,
      END OF t_url .
    TYPES:
      tt_url
          TYPE STANDARD TABLE OF t_url
          WITH DEFAULT KEY .

    ALIASES:
      t_doc_content_key FOR zif_gos~t_doc_content_key,
      tt_doc_content FOR zif_gos~tt_doc_content,
      tt_bdn_con FOR zif_gos~tt_bdn_con,
      t_doc_content FOR zif_gos~t_doc_content,
      t_doc_content_cache FOR zif_gos~t_doc_content_cache,
      gt_doc_content_cache FOR zif_gos~gt_doc_content_cache,
      attach_doc FOR zif_gos~attach_doc,
      get_doc_content FOR zif_gos~get_doc_content,
      delete_doc FOR zif_gos~delete_doc.

    METHODS:
      attach_url
        IMPORTING
          is_key TYPE t_doc_content_key
          is_url TYPE t_url
        RAISING
          zcx_gos_url_attach,

      get_url_list
        IMPORTING
          is_key        TYPE t_doc_content_key
        RETURNING
          VALUE(rt_url) TYPE tt_url
        RAISING
          zcx_gos_doc_content,

      constructor
        IMPORTING docs_frm_cont_repo TYPE char1 DEFAULT 'X'.

  PROTECTED SECTION.
  PRIVATE SECTION.

    CONSTANTS c_object_type_url TYPE soodk-objtp VALUE 'URL' ##NO_TEXT.
    CONSTANTS c_url_prefix TYPE char5 VALUE '&KEY&' ##NO_TEXT.


    "! <p class="shorttext synchronized" lang="en">flag to get docs from Content Repository</p>
    DATA gv_docs_frm_cont_repo TYPE char1.


    METHODS extract_file_from_obj_header
      IMPORTING
        it_head      TYPE ccrctt_text_tab
      EXPORTING
        ev_filename  TYPE string
        ev_extension TYPE clike
        ev_mimetype  TYPE mimetypes-type .
ENDCLASS.

CLASS zcl_gos IMPLEMENTATION.


  METHOD attach_doc.

    DATA:
      ls_folder_id        TYPE sofdk,
      ls_object_hd_change TYPE sood1,
      ls_object_id        TYPE soodk,
      lt_data             TYPE soli_tab,
      lt_xdata            TYPE solix_tab.

    TRY.

        " ______________________________
        " Attach et

        CALL FUNCTION 'SO_FOLDER_ROOT_ID_GET'
          EXPORTING
            region                = 'B'
          IMPORTING
            folder_id             = ls_folder_id
          EXCEPTIONS
            communication_failure = 1
            owner_not_exist       = 2
            system_failure        = 3
            x_error               = 4
            OTHERS                = 5
            ##FM_SUBRC_OK.

        zcx_function_subrc=>raise_if_sysubrc_not_initial( 'SO_FOLDER_ROOT_ID_GET' ).

        cl_bcs_convert=>xstring_to_xtab(
          EXPORTING iv_xstring = iv_hex_string
          IMPORTING et_xtab    = lt_xdata
        ).

        CALL FUNCTION 'SO_SOLIXTAB_TO_SOLITAB'
          EXPORTING
            ip_solixtab = lt_xdata
          IMPORTING
            ep_solitab  = lt_data.

        zcl_gos_toolkit=>split_filename(
          EXPORTING
            iv_filename  = iv_filename
          IMPORTING
            ev_extension = ls_object_hd_change-file_ext
        ).

        ls_object_hd_change-objdes = iv_description.
        ls_object_hd_change-objla  = sy-langu.
        ls_object_hd_change-objlen = xstrlen( iv_hex_string ).
        ls_object_hd_change-objpri = 5.

        DATA(lt_obj_header) = VALUE ccrctt_text_tab(
          ( line = |&SO_FILENAME={ iv_filename }| )
          ( line = '&SO_FORMAT=BIN' )
        ).

        CALL FUNCTION 'SO_OBJECT_INSERT'
          EXPORTING
            folder_id                  = ls_folder_id
            object_hd_change           = ls_object_hd_change
            object_type                = 'EXT'
            owner                      = sy-uname
          IMPORTING
            object_id                  = ls_object_id
          TABLES
            objcont                    = lt_data
            objhead                    = lt_obj_header
          EXCEPTIONS
            active_user_not_exist      = 1
            communication_failure      = 2
            component_not_available    = 3
            dl_name_exist              = 4
            folder_not_exist           = 5
            folder_no_authorization    = 6
            object_type_not_exist      = 7
            operation_no_authorization = 8
            owner_not_exist            = 9
            parameter_error            = 10
            substitute_not_active      = 11
            substitute_not_defined     = 12
            system_failure             = 13
            x_error                    = 14
            OTHERS                     = 15
            ##FM_SUBRC_OK. "#EC NUMBER_OK

        zcx_function_subrc=>raise_if_sysubrc_not_initial( 'SO_OBJECT_INSERT' ).

        " ______________________________
        " Nesne ile bağlantıyı sağla

        DATA(ls_obj_rolea) = VALUE borident(
          objkey  = is_key-objkey
          objtype = is_key-classname
        ).

        DATA(ls_obj_roleb) = VALUE borident(
          objkey  = |{ ls_folder_id-foltp }{ ls_folder_id-folyr  }{ ls_folder_id-folno }{ ls_object_id-objtp }{ ls_object_id-objyr }{ ls_object_id-objno }|
          objtype = 'MESSAGE'
        ).

        CALL FUNCTION 'BINARY_RELATION_CREATE'
          EXPORTING
            obj_rolea      = ls_obj_rolea
            obj_roleb      = ls_obj_roleb
            relationtype   = 'ATTA'
          EXCEPTIONS
            no_model       = 1
            internal_error = 2
            unknown        = 3
            OTHERS         = 4
            ##FM_SUBRC_OK.

        zcx_function_subrc=>raise_if_sysubrc_not_initial( 'BINARY_RELATION_CREATE' ).

        COMMIT WORK AND WAIT.

      CATCH zcx_gos_doc_attach INTO DATA(lo_attach_error).
        ROLLBACK WORK.                                 "#EC CI_ROLLBACK
        RAISE EXCEPTION lo_attach_error.
      CATCH cx_root INTO DATA(lo_diaper).
        ROLLBACK WORK.                                 "#EC CI_ROLLBACK
        RAISE EXCEPTION TYPE zcx_gos_doc_attach
          EXPORTING
            previous = lo_diaper
            objectid = |{ is_key-classname } { is_key-objkey }|.
    ENDTRY.


    " https://blogs.sap.com/2013/05/23/the-gos-generic-object-services-class-that-does-all-the-work/

  ENDMETHOD.


  METHOD attach_url.

    DATA:
      ls_folder_id        TYPE sofdk,
      ls_object_hd_change TYPE sood1,
      ls_object_id        TYPE soodk.

    TRY.

        " ______________________________
        " Folder ID'yi belirle

        CALL FUNCTION 'SO_FOLDER_ROOT_ID_GET'
          EXPORTING
            region                = 'B'
          IMPORTING
            folder_id             = ls_folder_id
          EXCEPTIONS
            communication_failure = 1
            owner_not_exist       = 2
            system_failure        = 3
            x_error               = 4
            OTHERS                = 5
            ##FM_SUBRC_OK.

        zcx_function_subrc=>raise_if_sysubrc_not_initial( 'SO_FOLDER_ROOT_ID_GET' ).

        " ______________________________
        " URL'yi kaydet

        ls_object_hd_change-objdes = is_url-description.
        ls_object_hd_change-objla  = sy-langu.
        ls_object_hd_change-objsns = 'O'.

        DATA(lt_obj_header) = VALUE ccrctt_text_tab( ).
        DATA(lt_data)       = VALUE soli_tab( ( line = |{ c_url_prefix }{ is_url-address }| ) ).

        CALL FUNCTION 'SO_OBJECT_INSERT'
          EXPORTING
            folder_id                  = ls_folder_id
            object_hd_change           = ls_object_hd_change
            object_type                = c_object_type_url
            owner                      = sy-uname
          IMPORTING
            object_id                  = ls_object_id
          TABLES
            objcont                    = lt_data
            objhead                    = lt_obj_header
          EXCEPTIONS
            active_user_not_exist      = 1
            communication_failure      = 2
            component_not_available    = 3
            dl_name_exist              = 4
            folder_not_exist           = 5
            folder_no_authorization    = 6
            object_type_not_exist      = 7
            operation_no_authorization = 8
            owner_not_exist            = 9
            parameter_error            = 10
            substitute_not_active      = 11
            substitute_not_defined     = 12
            system_failure             = 13
            x_error                    = 14
            OTHERS                     = 15
            ##FM_SUBRC_OK. "#EC NUMBER_OK

        zcx_function_subrc=>raise_if_sysubrc_not_initial( 'SO_OBJECT_INSERT' ).

        " ______________________________
        " Bağlantıyı kaydet

        DATA(ls_obj_rolea) = VALUE borident(
          objkey  = is_key-objkey
          objtype = is_key-classname
        ).

        DATA(ls_obj_roleb) = VALUE borident(
          objkey  = |{ ls_folder_id-foltp }{ ls_folder_id-folyr  }{ ls_folder_id-folno }{ ls_object_id-objtp }{ ls_object_id-objyr }{ ls_object_id-objno }|
          objtype = 'MESSAGE'
        ).

        DATA(lv_reltype) = CONV breltyp-reltype( c_object_type_url ).

        CALL FUNCTION 'BINARY_RELATION_CREATE'
          EXPORTING
            obj_rolea      = ls_obj_rolea
            obj_roleb      = ls_obj_roleb
            relationtype   = lv_reltype
          EXCEPTIONS
            no_model       = 1
            internal_error = 2
            unknown        = 3
            OTHERS         = 4
            ##FM_SUBRC_OK.

        zcx_function_subrc=>raise_if_sysubrc_not_initial( 'BINARY_RELATION_CREATE' ).

        COMMIT WORK AND WAIT.

      CATCH zcx_gos_url_attach INTO DATA(lo_attach_error).
        ROLLBACK WORK.                                 "#EC CI_ROLLBACK
        RAISE EXCEPTION lo_attach_error.
      CATCH cx_root INTO DATA(lo_diaper).
        RAISE EXCEPTION TYPE zcx_gos_url_attach
          EXPORTING
            previous = lo_diaper
            objectid = |{ is_key-classname } { is_key-objkey }|
            url      = is_url-address.
    ENDTRY.

    " https://subrc0.wordpress.com/2012/11/13/gos-adding-an-external-link-to-an-object/

  ENDMETHOD.


  METHOD constructor.
    gv_docs_frm_cont_repo = docs_frm_cont_repo.
  ENDMETHOD.

  METHOD delete_doc.

    TRY.

        DATA(ls_obj_rolea) = VALUE borident(
          objkey  = is_key-objkey
          objtype = is_key-classname
        ).

        DATA(ls_obj_roleb) = VALUE borident(
          objkey  = |{ is_folder_id-objtp }{ is_folder_id-objyr  }{ is_folder_id-objno }{ is_object_id-objtp }{ is_object_id-objyr }{ is_object_id-objno }|
          objtype = 'MESSAGE'
        ).

        CALL FUNCTION 'BINARY_RELATION_DELETE'
          EXPORTING
            obj_rolea          = ls_obj_rolea
            obj_roleb          = ls_obj_roleb
            relationtype       = 'ATTA'
          EXCEPTIONS
            entry_not_existing = 1
            internal_error     = 2
            no_relation        = 3
            no_role            = 4
            OTHERS             = 5
            ##FM_SUBRC_OK.

        zcx_function_subrc=>raise_if_sysubrc_not_initial( 'BINARY_RELATION_DELETE' ).

        CALL FUNCTION 'SO_OBJECT_DELETE'
          EXPORTING
            folder_id                  = is_folder_id
            object_id                  = is_object_id
          EXCEPTIONS
            communication_failure      = 1
            folder_not_empty           = 2
            folder_not_exist           = 3
            folder_no_authorization    = 4
            forwarder_not_exist        = 5
            object_not_exist           = 6
            object_no_authorization    = 7
            operation_no_authorization = 8
            owner_not_exist            = 9
            substitute_not_active      = 10
            substitute_not_defined     = 11
            system_failure             = 12
            x_error                    = 13
            OTHERS                     = 14
            ##FM_SUBRC_OK. "#EC NUMBER_OK

        zcx_function_subrc=>raise_if_sysubrc_not_initial( 'SO_OBJECT_DELETE' ).

        COMMIT WORK AND WAIT.

      CATCH zcx_gos_doc_delete INTO DATA(lo_del_error).
        RAISE EXCEPTION lo_del_error.

      CATCH cx_root INTO DATA(lo_diaper).

        RAISE EXCEPTION TYPE zcx_gos_doc_delete
          EXPORTING
            previous  = lo_diaper
            folder_id = is_folder_id
            object_id = is_object_id.

    ENDTRY.

  ENDMETHOD.


  METHOD extract_file_from_obj_header.

    " ______________________________
    " Hazırlık

    CLEAR:
      ev_filename,
      ev_extension,
      ev_mimetype.

    " ______________________________
    " Dosya adı

    LOOP AT it_head
      ASSIGNING FIELD-SYMBOL(<ls_head>)
      WHERE line+0(13) EQ '&SO_FILENAME='.

      ev_filename = <ls_head>-line.
      SHIFT ev_filename LEFT BY 13 PLACES.               "#EC NUMBER_OK
      EXIT.

    ENDLOOP.

    IF ev_filename IS INITIAL.
      RETURN.
    ENDIF.

    " ______________________________
    " Uzantı

    IF ev_extension IS REQUESTED OR
       ev_mimetype  IS REQUESTED.

      zcl_gos_toolkit=>split_filename(
        EXPORTING iv_filename  = ev_filename
        IMPORTING ev_extension = ev_extension
      ).

    ENDIF.

    " ______________________________
    " Mimetype

    IF ev_mimetype IS REQUESTED AND
       ev_extension IS NOT INITIAL.

      CALL FUNCTION 'SDOK_MIMETYPE_GET'
        EXPORTING
          extension = ev_extension
        IMPORTING
          mimetype  = ev_mimetype.

    ENDIF.

  ENDMETHOD.


  METHOD get_doc_content.

    DATA:
      lt_bdn_con TYPE tt_bdn_con.

    ASSIGN gt_doc_content_cache[
        KEY primary_key COMPONENTS
        content_key = is_key
      ] TO FIELD-SYMBOL(<ls_cache>).

    IF sy-subrc NE 0.

      DATA(ls_cache) = VALUE t_doc_content_cache( content_key = is_key ).

      TRY.

          CALL FUNCTION 'BDS_GOS_CONNECTIONS_GET'
            EXPORTING
              classname          = ls_cache-content_key-classname " LIKP
              objkey             = ls_cache-content_key-objkey " 8800000077
            TABLES
              gos_connections    = lt_bdn_con
            EXCEPTIONS
              no_objects_found   = 1
              internal_error     = 2
              internal_gos_error = 3
              OTHERS             = 4
              ##FM_SUBRC_OK.

          zcx_function_subrc=>raise_if_sysubrc_not_initial( 'BDS_GOS_CONNECTIONS_GET' ).

          ls_cache-content = VALUE #(
            FOR ls_bdn_con IN lt_bdn_con (
              gos_conn = ls_bdn_con
            )
          ).

          LOOP AT ls_cache-content ASSIGNING FIELD-SYMBOL(<ls_content>).

            DATA(lv_docid) = CONV sofolenti1-doc_id( <ls_content>-gos_conn-loio_id ).

            CALL FUNCTION 'SO_DOCUMENT_READ_API1'
              EXPORTING
                document_id                = lv_docid
              IMPORTING
                document_data              = <ls_content>-doc_data
              TABLES
                object_header              = <ls_content>-obj_header
                object_content             = <ls_content>-txt_content
                contents_hex               = <ls_content>-hex_content
              EXCEPTIONS
                document_id_not_exist      = 1
                operation_no_authorization = 2
                x_error                    = 3
                OTHERS                     = 4.

            IF sy-subrc NE 0.
              DELETE ls_cache-content.
              CONTINUE. " Ilımlı yaklaşıyoruz
            ENDIF.

            extract_file_from_obj_header(
              EXPORTING
                it_head      = <ls_content>-obj_header
              IMPORTING
                ev_filename  = <ls_content>-filename
                ev_extension = <ls_content>-extension
                ev_mimetype  = <ls_content>-mime_type
            ).

            <ls_content>-hex_string = cl_bcs_convert=>solix_to_xstring(
                                            it_solix = <ls_content>-hex_content
                                            iv_size  = CONV #( <ls_content>-doc_data-doc_size )
            ).


          ENDLOOP.

        CATCH zcx_gos_doc_content INTO ls_cache-cx ##no_handler .
        CATCH cx_root INTO DATA(lo_diaper).
          ls_cache-cx = NEW #(
            objectid = |{ is_key-classname } { is_key-objkey }|
            previous = lo_diaper
          ).
      ENDTRY.

      "get docs from Content repository
      IF gv_docs_frm_cont_repo = 'X'.
        TRY.
            APPEND LINES OF NEW zcl_gos_arl( )->get_doc_content( is_key  ) TO ls_cache-content.
          CATCH zcx_gos_doc_content INTO ls_cache-cx ##no_handler .
          CATCH cx_root INTO lo_diaper.
            ls_cache-cx = NEW #(
              objectid = |{ is_key-classname } { is_key-objkey }|
              previous = lo_diaper  ).

        ENDTRY.
      ENDIF.
      "end Content repository docs

      INSERT ls_cache
        INTO TABLE gt_doc_content_cache
        ASSIGNING <ls_cache>.

    ENDIF.

    IF <ls_cache>-cx IS NOT INITIAL.
      RAISE EXCEPTION <ls_cache>-cx.
    ENDIF.

    rt_content = <ls_cache>-content.

  ENDMETHOD.

  METHOD get_url_list.

    rt_url = VALUE #(
      FOR _ls_content IN get_doc_content( is_key )
      WHERE ( doc_data-obj_type EQ c_object_type_url )
      (
        description = _ls_content-doc_data-obj_descr
        address     = zcl_gos_toolkit=>remove_text_in_string(
          iv_string = VALUE #( _ls_content-txt_content[ 1 ]-line DEFAULT space )
          iv_remove = c_url_prefix
        )
      )
    ).

  ENDMETHOD.

ENDCLASS.
