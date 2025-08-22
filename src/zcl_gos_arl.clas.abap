"! <p class="shorttext synchronized" lang="en">Docs from Content Repository</p>
CLASS zcl_gos_arl DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES zif_gos.

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

  PROTECTED SECTION.
  PRIVATE SECTION.
    TYPES:
      BEGIN OF ty_toasp,
        ar_object  TYPE toasp-ar_object,
        objecttext TYPE toasp-objecttext,
      END OF ty_toasp,
      tt_toasp TYPE SORTED TABLE OF ty_toasp WITH UNIQUE KEY primary_key COMPONENTS ar_object.

    DATA:
      gt_toasp TYPE tt_toasp.
    METHODS:
      get_mimetype_doc_info
        IMPORTING
          i_conn            TYPE toav0
          i_extension       TYPE clike
        RETURNING
          VALUE(r_mimetype) TYPE mimetypes-type.
ENDCLASS.

CLASS zcl_gos_arl IMPLEMENTATION.
  METHOD get_doc_content.
    DATA:
      lt_arc_meta        TYPE TABLE OF toaom,
      lt_connections     TYPE STANDARD TABLE OF toav0,
      lt_file_attributes TYPE STANDARD TABLE OF toaat,
      lt_archivobject    TYPE STANDARD TABLE OF docs,
      lt_binarchivobject TYPE STANDARD TABLE OF tbl1024,
      ls_content         TYPE t_doc_content,
      lv_length          TYPE sapb-length,
      lv_binlength       TYPE sapb-length.

    ASSIGN gt_doc_content_cache[
        KEY primary_key COMPONENTS
        content_key = is_key
      ] TO FIELD-SYMBOL(<ls_cache>).

    IF sy-subrc NE 0.

      DATA(ls_cache) = VALUE t_doc_content_cache( content_key = is_key ).

      TRY.

          "Find the archive files available for the given document
          CALL FUNCTION 'ARCHIV_GET_CONNECTIONS'
            EXPORTING
              objecttype      = CONV saeanwdid( is_key-classname )  "'VBRK'
              object_id       = CONV saeobjid( is_key-objkey )      "Business document number
              client          = sy-mandt
              "archiv_id       = ls_toa01-archiv_id
              until_ar_date   = sy-datum
            TABLES
              connections     = lt_connections
              file_attributes = lt_file_attributes
            EXCEPTIONS
              nothing_found   = 1
              OTHERS          = 2 ##FM_SUBRC_OK.
          IF sy-subrc > 1.
            zcx_function_subrc=>raise_if_sysubrc_not_initial( 'ARCHIV_GET_CONNECTIONS' ).
          ENDIF.
          LOOP AT lt_connections INTO DATA(ls_connections).

            CLEAR ls_content.
            ls_content-repo-conn = ls_connections.
            ls_content-repo-file_attr = VALUE #( lt_file_attributes[ arc_doc_id = ls_connections-archiv_id ] OPTIONAL ).

            "filename     "extension
            zcl_gos_toolkit=>split_filename(
              EXPORTING iv_filename  = ls_content-repo-file_attr-filename
              IMPORTING ev_name      = ls_content-filename
                        ev_extension = ls_content-extension ).


            ls_connections-reserve = replace( val = ls_connections-reserve
                                              sub = '*'
                                              with = '' ).
            ls_content-extension = COND #( WHEN ls_content-extension IS INITIAL
                                             THEN ls_connections-reserve
                                             ELSE ls_content-extension ).

            DATA(lv_objecttext) =  VALUE #( gt_toasp[ ar_object = ls_connections-ar_object ]-objecttext OPTIONAL ).
            lv_objecttext = replace( val = lv_objecttext
                                     sub = '/'
                                     with = '' ).
            DATA(lv_filename) = COND #( WHEN lv_objecttext IS INITIAL
                                          THEN ls_connections-arc_doc_id
                                          ELSE lv_objecttext ).
            ls_content-filename = COND #( WHEN ls_content-filename IS INITIAL
                                            THEN |{ lv_filename }.{ ls_content-extension }|
                                            ELSE ls_content-filename ).


            "mime_type
            ls_content-mime_type = get_mimetype_doc_info(
                                      EXPORTING i_conn = ls_connections
                                                i_extension = ls_content-extension ).

            "Read a file from archiveLink
            CALL FUNCTION 'ARCHIVOBJECT_GET_TABLE'
              EXPORTING
                archiv_id                = ls_connections-archiv_id  "C9
                document_type            = CONV saedoktyp( ls_connections-ar_object )  "SDOINVOICE
                archiv_doc_id            = ls_connections-arc_doc_id "AID A2DFC5C1EEA85AB067D04AD7515
                signature                = 'X'
              IMPORTING
                length                   = lv_length
                binlength                = lv_binlength
              TABLES
                archivobject             = lt_archivobject       "ls_content-repo-txt_content "
                binarchivobject          = lt_binarchivobject    "ls_content-repo-hex_content
              EXCEPTIONS
                error_archiv             = 1
                error_communicationtable = 2
                error_kernel             = 3
                OTHERS                   = 4.

            IF sy-subrc NE 0.
              CONTINUE.
            ENDIF.

            " CONVERT tab xstring 1024 to 255
            zcl_gos_toolkit=>convert_xtab_to_xtab(
                      EXPORTING im_xtab = lt_binarchivobject
                      IMPORTING et_xtab = ls_content-hex_content  ).

            " CONVERT tab char 1024 to 255
            zcl_gos_toolkit=>convert_ctab_to_ctab(
                EXPORTING im_ctab  = lt_archivobject
                IMPORTING et_ctab =  ls_content-txt_content ).

            ls_content-hex_string = cl_bcs_convert=>xtab_to_xstring( ls_content-hex_content ).
            ls_content-gos_conn-objkey = ls_connections-object_id.
            ls_content-gos_conn-classname = ls_connections-sap_object. "bkpf
            ls_content-doc_data-doc_size = lv_binlength.
            APPEND ls_content TO ls_cache-content.
          ENDLOOP.

        CATCH zcx_gos_doc_content INTO ls_cache-cx ##no_handler .
        CATCH cx_root INTO DATA(lo_diaper).
          ls_cache-cx = NEW #(
            objectid = |{ is_key-classname } { is_key-objkey }|
            previous = lo_diaper
          ).
      ENDTRY.

      INSERT ls_cache
        INTO TABLE gt_doc_content_cache
        ASSIGNING <ls_cache>.

    ENDIF.

    IF <ls_cache>-cx IS NOT INITIAL.
      RAISE EXCEPTION <ls_cache>-cx.
    ENDIF.

    rt_content = <ls_cache>-content.

  ENDMETHOD.

  METHOD attach_doc.
*        is_key         TYPE t_doc_content_key
*        iv_filename    TYPE clike
*        iv_description TYPE clike
*        iv_hex_string  TYPE xstring
*        is_toav0       TYPE toav0 OPTIONAL
    DATA:
      lv_archiv_doc_id   TYPE sapb-sapadokid,
      lv_length          TYPE sapb-length,
      lv_name            TYPE string,
      lv_ext             TYPE toadv-doc_type,
      lv_desc            TYPE toaat-descr,
      lt_binarchivobject TYPE tabl1024_t.

    TRY.
        "filename     "extension
        zcl_gos_toolkit=>split_filename(
          EXPORTING iv_filename  = iv_filename
          IMPORTING ev_name      = DATA(lv_filename)
                    ev_extension = lv_ext ).

        IF is_toav0-archiv_id IS INITIAL.
          SELECT SINGLE archiv_id
            FROM toaom  INTO @DATA(lv_archiv_id)
            WHERE sap_object = @is_key-classname AND "BKPF
                  ar_object  = @is_toav0-ar_object  AND "SDOINVOICE
                  ar_status    = 'X'.

        ELSE.
          lv_archiv_id = is_toav0-archiv_id.
        ENDIF.

        cl_bcs_convert=>xstring_to_xtab( EXPORTING iv_xstring = iv_hex_string
                                         IMPORTING et_xtab    = lt_binarchivobject ).
        "length of the file
        lv_length  = xstrlen( iv_hex_string ).

        " archieving of the uploaded file
        CALL FUNCTION 'ARCHIVOBJECT_CREATE_TABLE'
          EXPORTING
            archiv_id                = lv_archiv_id                        "C9
            arc_doc_id               = is_key-objkey                       "is_TOAV0-object_id 90890193890000808828
            document_type            = lv_ext
            compid                   = iv_filename
            length                   = lv_length
          IMPORTING
            archiv_doc_id            = lv_archiv_doc_id
          TABLES
*           archivobject             = archivobject
            binarchivobject          = lt_binarchivobject
          EXCEPTIONS
            error_archiv             = 1
            error_communicationtable = 2
            error_kernel             = 3
            OTHERS                   = 4
            ##FM_SUBRC_OK.
        zcx_function_subrc=>raise_if_sysubrc_not_initial( 'ARCHIVOBJECT_CREATE_TABLE' ).

*    lv_object_id = object_key.
*    lv_desc = lv_file_a = lv_name_with_ext.

        CALL FUNCTION 'ARCHIV_CONNECTION_INSERT'
          EXPORTING
            archiv_id             = lv_archiv_id "|G3|
            arc_doc_id            = lv_archiv_doc_id
            ar_object             = is_toav0-ar_object  "'ZFIJEPSSL'
            object_id             = CONV saeobjid( is_key-objkey ) "90890193890000808828
            sap_object            = CONV saeanwdid( is_key-classname ) "'BKPF'
            doc_type              = lv_ext
            filename              = CONV char255( iv_filename )
            descr                 = CONV char060( iv_filename )
            creator               = sy-uname
          EXCEPTIONS
            error_connectiontable = 1
            OTHERS                = 2
            ##FM_SUBRC_OK.
        zcx_function_subrc=>raise_if_sysubrc_not_initial( 'ARCHIV_CONNECTION_INSERT' ).

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

  ENDMETHOD.

  METHOD delete_doc.
    IF is_toav0 IS NOT INITIAL.
      CALL FUNCTION 'ARCHIVOBJECT_DELETE'
        EXPORTING
          archiv_doc_id            = is_toav0-arc_doc_id                 "AID A2DFC5C1EEA85AB067D04AD7515
          archiv_id                = is_toav0-archiv_id                  "C9
*         doc_type                 = conv char20( is_TOAV0-reserve )     "PDF
          sap_object               = is_key-classname                    "VBRK
          ar_object                = is_toav0-ar_object                  "SDOINVOICE
*         ar_date                  = conv char8( is_TOAV0-ar_date )
*         del_date                 = conv char8( is_TOAV0-del_date )
          object_id                = is_key-objkey                       "is_TOAV0-object_id 90890193890000808828
        EXCEPTIONS
          error_archiv             = 1
          error_communicationtable = 2
          error_kernel             = 3
          OTHERS                   = 4
          ##FM_SUBRC_OK.

      zcx_function_subrc=>raise_if_sysubrc_not_initial( 'SCMS_DOC_INFO' ).
    ENDIF.
  ENDMETHOD.

  METHOD get_mimetype_doc_info.
    DATA lt_comps TYPE STANDARD TABLE OF scms_stinf.

    CALL FUNCTION 'SCMS_DOC_INFO'
      EXPORTING
        stor_cat              = space             " Category
        crep_id               = i_conn-archiv_id  " Repository (Only Allowed if Category = SPACE)
        doc_id                = i_conn-arc_doc_id " Document ID
      TABLES
        comps                 = lt_comps
      EXCEPTIONS
        bad_storage_type      = 1                " Storage Category Not Supported
        bad_request           = 2                " Unknown Functions or Parameters
        unauthorized          = 3                " Security Breach
        not_found             = 4                " Document/ Component/ Content Repository Not Found
        conflict              = 5                " Document/ Component/ Administration Data is Inaccessible
        internal_server_error = 6                " Internal Error in Content Server
        error_http            = 7                " Error in HTTP Access
        error_signature       = 8                " Error when Calculating Signature
        error_config          = 9                " Configuration error
        error_hierarchy       = 10               " Error When Accessing Structures
        error_parameter       = 11               " Parameter error
        error                 = 12               " Unspecified error
        OTHERS                = 13 ##FM_SUBRC_OK.

    zcx_function_subrc=>raise_if_sysubrc_not_initial( 'SCMS_DOC_INFO' ).

    DATA(ls_comps) = VALUE #( lt_comps[ 1 ] OPTIONAL ).
    r_mimetype = ls_comps-mimetype.
    "ev_content_length = ls_comps-comp_size.

    r_mimetype = COND #( WHEN r_mimetype IS INITIAL AND
                              i_extension IS NOT INITIAL
                           THEN zcl_gos_toolkit=>get_mimetype( i_extension )
                           ELSE r_mimetype ).

  ENDMETHOD.
ENDCLASS.
