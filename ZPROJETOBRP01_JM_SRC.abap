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
SELECTION-SCREEN END OF BLOCK b2.