CLASS zcx_gos_doc_attach DEFINITION
  PUBLIC
  INHERITING FROM cx_static_check
  create public.

  PUBLIC SECTION.
    METHODS constructor
      IMPORTING textid    LIKE textid   OPTIONAL
                previous LIKE previous OPTIONAL
                objectid  TYPE csequence.

    DATA objectid TYPE string READ-ONLY.
ENDCLASS.


CLASS zcx_gos_doc_attach IMPLEMENTATION.
  METHOD constructor ##ADT_SUPPRESS_GENERATION.
    super->constructor( textid   = textid
                        previous = previous ).
    me->objectid = objectid.
  ENDMETHOD.
ENDCLASS.
