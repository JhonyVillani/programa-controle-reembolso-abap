*&---------------------------------------------------------------------*
*&  Include           ZPROJETOBRP01_JM_EVE
*&---------------------------------------------------------------------*

* Declara uma variável do tipo da classe
  DATA: go_reembolso TYPE REF TO zprojetobcl01_jm. "Classe global

  START-OF-SELECTION.

    CREATE OBJECT go_reembolso.

  GET peras.

    rp_provide_from_last p0001 space pn-begda pn-endda.
    rp_provide_from_last p0002 space pn-begda pn-endda.

    go_reembolso->processa(
      EXPORTING
        iv_p0001 = p0001
        iv_p0002 = p0002 ).

  END-OF-SELECTION.

    go_reembolso->exibe( ).