*&---------------------------------------------------------------------*
*&  Include           ZPROJETOBRP01_JM_C01
*&---------------------------------------------------------------------*

*----------------------------------------------------------------------*
*       CLASS lcl_reembolso DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_reembolso DEFINITION.
  PUBLIC SECTION.

    TYPES: BEGIN OF ty_s_saida,
            pernr    TYPE p0001-pernr,
           END OF ty_s_saida.

    DATA: gt_saida TYPE TABLE OF ty_s_saida,
          gs_saida TYPE ty_s_saida,
          gs_p0001 TYPE p0001.

    METHODS:
      constructor,
      processa,
      exibe.

ENDCLASS.                    "lcl_reembolso DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_reembolso IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_reembolso IMPLEMENTATION.
  METHOD constructor.

  ENDMETHOD.                    "constructor

  METHOD processa.

  ENDMETHOD.                    "processamento

  METHOD exibe.

  ENDMETHOD.                    "exibe

ENDCLASS.                    "lcl_reembolso IMPLEMENTATION