CLASS zcx_gos_url_attach DEFINITION
  PUBLIC
  INHERITING FROM cx_static_check
  CREATE PUBLIC .

  PUBLIC SECTION.
    METHODS constructor
      IMPORTING textid    LIKE textid   OPTIONAL
                previous LIKE previous OPTIONAL
                objectid  TYPE csequence
                url       TYPE csequence.

    DATA objectid TYPE string READ-ONLY.
    DATA url      TYPE string.
ENDCLASS.


CLASS zcx_gos_url_attach IMPLEMENTATION.
  METHOD constructor ##ADT_SUPPRESS_GENERATION.
    super->constructor( textid   = textid
                        previous = previous ).
    me->objectid = objectid.
    me->url      = url.
  ENDMETHOD.
ENDCLASS.
