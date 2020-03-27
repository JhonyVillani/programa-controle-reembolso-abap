*&---------------------------------------------------------------------*
*&  Include           ZPROJETOBRP01_JM_C01
*&---------------------------------------------------------------------*

*----------------------------------------------------------------------*
*       CLASS lcl_eventos DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_eventos DEFINITION.
  PUBLIC SECTION.

    CLASS-DATA: lv_flag TYPE flag.

    CLASS-METHODS:
      modifica_tela,
      valida_mes_ano,
      verifica_radiobtn.

ENDCLASS.                    "lcl_eventos DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_eventos IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_eventos IMPLEMENTATION.
  METHOD modifica_tela.

    LOOP AT SCREEN.

      "Se não estiver marcado para exportar, oculta campo
      IF p_alv = 'X' OR p_smart = 'X'.

        IF screen-group1 = 'T1'.
          screen-invisible = 1.
          screen-input     = 0.
          screen-active    = 0.
          MODIFY SCREEN.
          CONTINUE.
        ENDIF.

        "Reapresenta o campo
      ELSE.
        IF screen-group1 = 'T1'.
          screen-invisible = 0.
          screen-input     = 1.
          screen-active    = 1.
          MODIFY SCREEN.
          CONTINUE.
        ENDIF.
      ENDIF.

    ENDLOOP. "Encerra loop at screen

  ENDMETHOD.                    "modifica_tela

  METHOD valida_mes_ano.

    IF ( p_mes < '01' OR p_mes > '12' ) OR ( NOT p_mes CO '0123456789' ).
      MESSAGE s208(00) WITH text-m01 DISPLAY LIKE 'E'.

      "Retorna para a tela de seleção
      LEAVE LIST-PROCESSING.
    ENDIF.

    IF ( p_ano < '2000' OR p_ano > '9999' ) OR ( NOT p_ano CO '0123456789' ).
      MESSAGE s208(00) WITH text-m02 DISPLAY LIKE 'E'.

      "Retorna para a tela de seleção
      LEAVE LIST-PROCESSING.
    ENDIF.

  ENDMETHOD.                    "valida_mes_ano

  METHOD verifica_radiobtn.

    IF p_export IS NOT INITIAL.

      "Passa um flag coo parâmetro pro método processa, que fará um tratamento exclusivo
      lv_flag = 'X'.

      IF p_file IS INITIAL.

        "Informa o usuário que o campo está vazio
        MESSAGE s208(00) WITH text-m03 DISPLAY LIKE 'E'.

        "Retorna à tela de seleção
        LEAVE LIST-PROCESSING.

        "Chama o método que verifica se o diretório existe
      ELSE.

        "Caso o caminho tenha sido preenchido, entra no método que verifica se existe
        zprojetobcl01_jm=>verifica_diretorio(
        IMPORTING
          ev_directory = p_file ). "Se existir, o parameter recebe o caminho

      ENDIF. "Verifica se o diretório está vazio

    ENDIF. "Verifica RADIO de Exportar

  ENDMETHOD.                    "exibe

ENDCLASS.                    "lcl_eventos IMPLEMENTATION