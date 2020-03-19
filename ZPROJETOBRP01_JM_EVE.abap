*&---------------------------------------------------------------------*
*&  Include           ZPROJETOBRP01_JM_EVE
*&---------------------------------------------------------------------*

* Declara uma variável do tipo da classe
  DATA:
        go_reembolso TYPE REF TO lcl_reembolso. "Classe local

  START-OF-SELECTION.

    CREATE OBJECT go_reembolso.

  GET peras.

    rp_provide_from_last p0001 space pn-begda pn-endda.

    go_reembolso->processa( ).

  END-OF-SELECTION.

    go_reembolso->exibe( ).