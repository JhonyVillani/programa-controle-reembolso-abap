*&---------------------------------------------------------------------*
*&  Include           ZPROJETOBRP01_JM_EVE
*&---------------------------------------------------------------------*

AT SELECTION-SCREEN OUTPUT.

  "Chamada de método estático da classe Local
  lcl_eventos=>modifica_tela( ).

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_file. "No momento que for requisitado um valor, preencherá a variável p_file

  "Chamada de método estático da classe Global
  zprojetobcl01_jm=>browse_popup(
  IMPORTING
          ev_directory = p_file ). "Importa o caminho definido pelo usuário via matchcode (apenas se clicado)

START-OF-SELECTION.

*     Validando campos de mês e ano
*---------------------------------------------------------------------------
  lcl_eventos=>valida_mes_ano( ).

*     Verifica se o RADIO button de exportar arquivo está marcado
*----------------------------------------------------------------
  lcl_eventos=>verifica_radiobtn( ).

*     Criação de objeto e chamada de métodos
*------------------------------------------------------

  CREATE OBJECT go_reembolso.

GET peras.

  rp_provide_from_last p0001 space pn-begda pn-endda.
  rp_provide_from_last p0002 space pn-begda pn-endda.

  go_reembolso->processa(
    EXPORTING
      is_p0001 = p0001
      is_p0002 = p0002
      iv_mes   = p_mes
      iv_ano   = p_ano
      iv_flag  = lcl_eventos=>lv_flag ).

END-OF-SELECTION.

*     Prepara a exibição dos dados caso exista(m) resultado(s)
*-------------------------------------------------------------
  "Verifica se a tabela está vazia
  go_reembolso->verifica( ).

  CASE abap_true.
    WHEN p_alv.
      go_reembolso->alv( ).
    WHEN p_smart.
      go_reembolso->smart( ).
    WHEN p_export.
      go_reembolso->converte( ).
  ENDCASE.