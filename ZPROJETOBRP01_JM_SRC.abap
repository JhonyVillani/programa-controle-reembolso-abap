*&---------------------------------------------------------------------*
*&  Include           ZPROJETOBRP01_JM_SRC
*&---------------------------------------------------------------------*

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-001.
PARAMETER p_mes TYPE char2 DEFAULT sy-datum+4(2).
PARAMETER p_ano TYPE char4 DEFAULT sy-datum(4).
SELECTION-SCREEN END OF BLOCK b1.

*     Bloco de saída
*------------------------------------------------------------------------------------
SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE text-002.
PARAMETER p_alv    RADIOBUTTON GROUP view USER-COMMAND muda_tela DEFAULT 'X'.
PARAMETER p_smart  RADIOBUTTON GROUP view.
PARAMETER p_export RADIOBUTTON GROUP view.

"Parâmetro para leitura de dados
PARAMETERS: p_file TYPE rlgrap-filename MODIF ID t1.

SELECTION-SCREEN END OF BLOCK b2.

AT SELECTION-SCREEN OUTPUT.

  PERFORM modifica_tela.

  "No momento que for requisitado um valor, preencherá a variável p_file

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_file.

  "Chamada de método estático devido não existir instâncias de objetos
  zprojetobcl01_jm=>browse_popup(
  IMPORTING
          ev_directory = p_file ). "Importa o caminho definido pelo usuário via matchcode (apenas se clicado)

*&---------------------------------------------------------------*
*&      Form  MODIFICA_TELA
*&---------------------------------------------------------------*
FORM modifica_tela .

*     Loop at screen para a ação do usuário no radio button
*----------------------------------------------------------
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

ENDFORM.                    " MODIFICA_TELA