CLASS zcx_gos_doc_delete DEFINITION
  PUBLIC
  INHERITING FROM cx_static_check
  CREATE PUBLIC.

  PUBLIC SECTION.
    METHODS constructor
      IMPORTING textid    LIKE textid   OPTIONAL
                previous LIKE previous OPTIONAL
                folder_id TYPE soodk
                object_id TYPE soodk.

    DATA folder_id TYPE soodk READ-ONLY.
    DATA object_id TYPE soodk READ-ONLY.
ENDCLASS.

CLASS zcx_gos_doc_delete IMPLEMENTATION.
  METHOD constructor ##ADT_SUPPRESS_GENERATION.
    super->constructor( textid   = textid
                        previous = previous ).
    me->folder_id = folder_id.
    me->object_id = object_id.
  ENDMETHOD.
ENDCLASS.
