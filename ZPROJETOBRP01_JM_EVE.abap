*&---------------------------------------------------------------------*
*&  Include           ZPROJETOBRP01_JM_EVE
*&---------------------------------------------------------------------*

* Declara uma variável do tipo da classe
  DATA: go_reembolso TYPE REF TO zprojetobcl01_jm. "Classe global

  START-OF-SELECTION.

*     Validando campos de mês e ano
*---------------------------------------------------------------------------
    IF ( p_mes < '01' OR p_mes > '12' ) OR ( NOT p_mes CO '0123456789' ).
      MESSAGE s001(00) WITH text-m01 DISPLAY LIKE 'E'.

      "Retorna para a tela de seleção
      LEAVE LIST-PROCESSING.
    ENDIF.

    IF ( p_ano < '2000' OR p_ano > '9999' ) OR ( NOT p_ano CO '0123456789' ).
      MESSAGE s001(00) WITH text-m02 DISPLAY LIKE 'E'.

      "Retorna para a tela de seleção
      LEAVE LIST-PROCESSING.
    ENDIF.

*     Criação de objeto e chamada de métodos
*------------------------------------------------------

    CREATE OBJECT go_reembolso.

  GET peras.

    rp_provide_from_last p0001 space pn-begda pn-endda.
    rp_provide_from_last p0002 space pn-begda pn-endda.

    go_reembolso->processa(
      EXPORTING
        iv_p0001 = p0001
        iv_p0002 = p0002
        iv_mes   = p_mes
        iv_ano   = p_ano ).

  END-OF-SELECTION.

    go_reembolso->exibe( ).