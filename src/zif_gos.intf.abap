INTERFACE zif_gos
  PUBLIC.

  TYPES:
    tt_bdn_con
      TYPE STANDARD TABLE OF bdn_con
      WITH DEFAULT KEY,

    tt_c1024   TYPE STANDARD TABLE OF docs WITH EMPTY KEY,

    BEGIN OF t_repo,
      conn      TYPE toav0,
      file_attr TYPE toaat,
      "txt_content TYPE tt_c1024,
      "hex_content TYPE tabl1024_t,
    END OF t_repo,

    BEGIN OF t_doc_content,
      gos_conn    TYPE bdn_con,
      doc_data    TYPE sofolenti1,
      obj_header  TYPE ccrctt_text_tab,
      filename    TYPE string,
      extension   TYPE char10,
      txt_content TYPE ccrctt_text_tab,
      hex_content TYPE solix_tab,
      hex_string  TYPE xstring,
      mime_type   TYPE mimetypes-type,
      repo        TYPE t_repo,
    END OF t_doc_content,

    tt_doc_content
      TYPE STANDARD TABLE OF t_doc_content
      WITH DEFAULT KEY,

    BEGIN OF t_doc_content_key,
      classname TYPE bapibds01-classname,
      objkey    TYPE swotobjid-objkey,
    END OF t_doc_content_key,

    BEGIN OF t_doc_content_cache,
      content_key TYPE t_doc_content_key,
      content     TYPE tt_doc_content,
      cx          TYPE REF TO zcx_gos_doc_content,
    END OF t_doc_content_cache,

    tt_doc_content_cache
      TYPE HASHED TABLE OF t_doc_content_cache
      WITH UNIQUE KEY primary_key COMPONENTS content_key.

  DATA:
    gt_doc_content_cache TYPE tt_doc_content_cache.

  METHODS:
    attach_doc
      IMPORTING
        is_key         TYPE t_doc_content_key
        iv_filename    TYPE clike
        iv_description TYPE clike
        iv_hex_string  TYPE xstring
        is_toav0       TYPE toav0 OPTIONAL
      RAISING
        zcx_gos_doc_attach,

    delete_doc
      IMPORTING
        is_key       TYPE t_doc_content_key
        is_folder_id TYPE soodk
        is_object_id TYPE soodk
        is_toav0     TYPE toav0 OPTIONAL
      RAISING
        zcx_gos_doc_delete,
    get_doc_content
      IMPORTING
        is_key            TYPE t_doc_content_key
      RETURNING
        VALUE(rt_content) TYPE tt_doc_content
      RAISING
        zcx_gos_doc_content.

ENDINTERFACE.
